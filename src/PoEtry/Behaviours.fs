namespace PoEtry

/// Per-skill behaviour that stats alone don't describe. Each entry says where its
/// rules come from; numbers are read from the gem's stats.
module Behaviours =
    /// Behaviour observed in game that the data doesn't carry, per skill (granted effect id or
    /// name). Each entry says who observed it. Applied to the skill and its parts.
    let observed : (string * (Skill -> Skill)) list =
        let projectiles f (s: Skill) =
            { s with Delivery = (match s.Delivery with Projectiles p -> Projectiles(f p) | d -> d) }
        [ // user: Falling Thunder's projectiles appear about half a metre out from the impact
          "Falling Thunder", projectiles (fun p -> { p with StartOffset = 5.0 }) ]

    let applyObserved (skill: Skill) =
        match observed |> List.tryFind (fun (n, _) -> n = skill.Name) with
        | Some (_, f) -> { f skill with Parts = skill.Parts |> List.map f }
        | None -> skill

    let private stat (stats: (string * float) list) id =
        stats |> List.filter (fst >> (=) id) |> List.sumBy snd

    /// Elemental Conflux: "greatly more damage of a randomly chosen Element. The Element
    /// affected changes frequently, though the same Element can be affected multiple
    /// times in succession." Period = skill effect duration (base + alt quality) times
    /// duration modifiers from supports. Each period picks fire, cold or lightning uniformly.
    let elementalConflux (stats: (string * float) list) : Cycle option =
        let more = stat stats "skill_elemental_conflux_active_element_damage_+%_final"
        if more = 0.0 then None
        else
            let baseMs = stat stats "base_skill_effect_duration"
            let durationMult =
                stats
                |> List.filter (fun (id, _) -> id.Contains "skill_effect_duration_+%")
                |> List.fold (fun k (id, v) -> if id.EndsWith "_final" then k * (1.0 + v / 100.0) else k) 1.0
                |> fun more ->
                    more * (1.0 + (stats |> List.filter (fun (id, _) -> id = "skill_effect_duration_+%") |> List.sumBy snd) / 100.0)
            Some
                { Name = "Elemental Conflux"
                  Period = max 0.5 (baseMs / 1000.0 * durationMult)
                  Variants = DamageType.elemental |> List.map (fun t -> [ Mod.more (Damage(OfType t)) more |> Mod.from "Elemental Conflux" ]) }

    /// All cycles granted by the build's persistent buffs.
    let cycles (db: GameDb) (characterMods: ModDb) (setups: GemSetup list) =
        setups |> List.choose (fun s -> elementalConflux (Gems.rawStats db characterMods s))

    /// Timed resource effects from the character's stats, and resource maxima.
    /// - `consume_frenzy_power_and_endurance_charge_every_x_ms` (Charge Regulation): one of each charge.
    /// - Thaumaturgical Dynamism: a random charge every `chargeGenSeconds` (the game derives the
    ///   interval from socketed gem attribute requirements; that formula isn't in the data).
    /// - `base_active_skill_buff_stack_gain_frequency_ms` (Virtuous Barrier): a random mote. Maximum
    ///   motes per attribute = the description's base (`skill_specific_stat_description_mode`) +
    ///   active skills requiring that attribute, single-attribute skills counting twice.
    let charges = [ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ]

    let periodics (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (chargeGenSeconds: float) =
        let mods = ModDb.ofList sheet.Mods
        let sum stat = Mods.sumBase Ctx.empty [ Named stat ] mods
        [ let consume = sum "ConsumeEachChargeEveryMs"
          if consume > 0.0 then
              yield { Name = "consume charges"; Every = consume / 1000.0; Action = ConsumeEach charges }
          if Mods.flag Ctx.empty (Named "GeneratesRandomCharges") mods then
              // weighted by the total attribute requirements of socketed skill gems:
              // Strength -> endurance, Dexterity -> frenzy, Intelligence -> power
              let gems =
                  build.Groups
                  |> List.filter (fun g -> g.Enabled)
                  |> List.collect (fun g -> g.Gems |> List.filter (fun gem -> gem.Enabled))
                  |> List.choose (fun gem -> Map.tryFind gem.Name db.Gems |> Option.map (fun d -> gem, d))
              let total pick =
                  gems |> List.sumBy (fun (gem, d) -> Gems.attributeRequirement gem.Level (pick d.Requirements) d.Support)
              let weights =
                  [ "EnduranceCharge", total (fun (s, _, _) -> s)
                    "FrenzyCharge", total (fun (_, d, _) -> d)
                    "PowerCharge", total (fun (_, _, i) -> i) ]
              yield { Name = "generate charges"; Every = chargeGenSeconds; Action = GainWeighted weights }
          let motes = sum "MoteGainEveryMs"
          if motes > 0.0 then
              yield { Name = "motes"; Every = motes / 1000.0; Action = GainRandom [ "Mote:red"; "Mote:green"; "Mote:blue" ] } ]

    /// Maximum motes per colour from the active skill gems' attribute requirements.
    let moteMaxima (db: GameDb) (build: PobBuild) (baseMax: int) =
        let actives =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.collect (fun g -> g.Gems |> List.filter (fun gem -> gem.Enabled && not gem.IsSupport))
            |> List.choose (fun gem -> Map.tryFind gem.Name db.Gems)
        let count pick =
            actives
            |> List.sumBy (fun g ->
                let s, d, i = g.Requirements
                let n = [ s; d; i ] |> List.filter (fun x -> x > 0.0) |> List.length
                if pick (s, d, i) > 0.0 then (if n = 1 then 2 else 1) else 0)
        [ "Mote:red", baseMax + count (fun (s, _, _) -> s)
          "Mote:green", baseMax + count (fun (_, d, _) -> d)
          "Mote:blue", baseMax + count (fun (_, _, i) -> i) ]

    /// Charge durations: 15 s base (PoB CalcSetup) x increased charge duration.
    let chargeDurations (sheet: CharacterSheet) =
        let mods = ModDb.ofList sheet.Mods
        charges |> List.map (fun c -> c, floor (15.0 * Mods.inc Ctx.empty [ Named(c + "Duration") ] mods)) |> Map.ofList

    /// The infusion a gem's skill consumes ("active_skill_consumes_a_X_infusion") and its
    /// "-Infused" stat set as a skill.
    let infusedCopy (db: GameDb) (charMods: ModDb) mainHand (setup: GemSetup) =
        let consumes =
            Gems.rawStats db charMods setup
            |> List.tryPick (fun (id, v) ->
                if v > 0.0 && id.StartsWith "active_skill_consumes_a_" && id.EndsWith "_infusion" then
                    let el = id.Substring(24, id.Length - 24 - 9)
                    Some(string (System.Char.ToUpperInvariant el.[0]) + el.Substring 1 + "Infusion")
                else None)
        match consumes, Gems.infusedSet db setup.Gem with
        | Some element, Some set ->
            match Gems.skill db charMods mainHand { setup with StatSet = set } with
            | Ok (skill, _) -> Some(element, skill)
            | Error _ -> None
        | _ -> None

    /// Meta gems of a build and the spells they trigger. Every enabled group whose active gem is
    /// a Meta gem: energy sources, energy multiplier (gem + the group's supports), refund chance,
    /// release (Invocation or at maximum) and maximum energy from its stats; the group's other
    /// skill gems become triggered copies with the group's supports. Energy per trigger = 100 per
    /// second of base cast time of the socketed spells ("10 per 0.1 s", community source).
    let metas (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (infused: bool) =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.indexed
        |> List.choose (fun (gi, g) ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            match enabled |> List.tryFind (fun x -> not x.IsSupport && Gems.isMeta db x.Name) with
            | None -> None
            // Blasphemy is an aura, not a trigger (see curseAuras)
            | Some metaGem when Gems.data db metaGem.Name |> Option.exists (fun d -> d.Types.Contains "IsBlasphemy") -> None
            | Some metaGem ->
                let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
                let spells = enabled |> List.filter (fun x -> not x.IsSupport && x.Name <> metaGem.Name)
                let raw = Gems.rawStats db charMods { Gem = metaGem.Name; Level = metaGem.Level; Quality = metaGem.Quality; StatSet = 1; Supports = supports }
                let sum pred = raw |> List.filter (fun (id, _) -> pred id) |> List.sumBy snd
                let name = sprintf "%s #%d" metaGem.Name (gi + 1)
                let sources =
                    raw
                    |> List.choose (fun (id, v) ->
                        if id.EndsWith "_per_monster_power_on_crit" then Some(CritHits(v / 100.0))
                        elif id.EndsWith "_per_monster_power_on_freeze" then Some(InflictsAilment(Freeze, v / 100.0))
                        elif id.EndsWith "_per_monster_power_on_shock" then Some(InflictsAilment(Shock, v / 100.0))
                        elif id.EndsWith "_per_monster_power_on_ignite" then Some(InflictsAilment(Ignite, v / 100.0))
                        elif id.EndsWith "_per_monster_power_on_hit" then Some(MeleeHits(v / 100.0))
                        elif id.EndsWith "_per_monster_power_on_melee_kill" then Some(MeleeKills(v / 100.0))
                        elif id.EndsWith "_centienergy_per_unit_travelled_while_dodge_rolling" then Some(DodgeRolls(v / 100.0))
                        elif id.EndsWith "_centienergy_on_block" then Some(Blocks(v / 100.0))
                        elif id.EndsWith "_centienergy_per_10ms_base_cast_time" then Some(CastsSpells(v / 100.0))
                        else None)
                let rename copyName (s: Skill) =
                    { s with Name = copyName; Tags = s.Tags.Add Triggered; Parts = s.Parts |> List.map (fun p -> { p with Name = copyName + ": " + p.Name; Tags = p.Tags.Add Triggered }) }
                let triggered =
                    spells
                    |> List.choose (fun sp ->
                        let set = if infused then Gems.infusedSet db sp.Name |> Option.defaultValue 1 else 1
                        match Gems.skill db charMods (Gems.chooseWeapon db sp.Name sheet.Weapons) { Gem = sp.Name; Level = sp.Level; Quality = sp.Quality; StatSet = set; Supports = supports } with
                        | Ok (skill, _) ->
                            let copyName = sprintf "%s (%s)" skill.Name name
                            let castTime =
                                (Gems.data db sp.Name |> Option.map (fun d -> d.CastTime) |> Option.defaultValue 1.0)
                                + (Gems.rawStats db charMods { Gem = sp.Name; Level = sp.Level; Quality = sp.Quality; StatSet = 1; Supports = [] }
                                   |> List.filter (fun (id, _) -> id = "total_cast_time_+_ms") |> List.sumBy snd) / 1000.0
                                  * Constants.addedCastTimeEnergyWeight
                            Some(rename copyName skill, castTime)
                        | Error _ -> None)
                // the infused copies, used when an infusion is consumed
                let consumers =
                    if infused then []
                    else
                        spells
                        |> List.choose (fun sp ->
                            let setup = { Gem = sp.Name; Level = sp.Level; Quality = sp.Quality; StatSet = 1; Supports = supports }
                            infusedCopy db charMods (Gems.chooseWeapon db setup.Gem sheet.Weapons) setup
                            |> Option.map (fun (element, copy) ->
                                let copyName = sprintf "%s (%s)" copy.Name name
                                copyName, element, rename (copyName + " infused") copy))
                let cost = triggered |> List.sumBy (fun (_, ct) -> 100.0 * ct)
                let invocation = (Gems.data db metaGem.Name |> Option.exists (fun d -> d.Types.Contains "Invocation"))
                let cooldown =
                    Gems.data db metaGem.Name
                    |> Option.bind (fun d -> Map.tryFind metaGem.Level d.Levels |> Option.orElse (Map.tryFind 1 d.Levels))
                    |> Option.bind (fun l -> l.Cooldown)
                    |> Option.defaultValue 0.2
                let maxEnergy =
                    if invocation then sum ((=) "generic_ongoing_trigger_maximum_energy") * (1.0 + sum ((=) "skill_maximum_energy_+%") / 100.0)
                    else cost
                let meta =
                    { Name = name
                      Release = (if invocation then Invocation cooldown else AtMaximum)
                      MaxEnergy = maxEnergy
                      Cost = cost
                      Triggers = triggered |> List.map (fun (s, _) -> s.Name)
                      Sources = sources
                      EnergyMult = 1.0 + sum ((=) "energy_generated_+%") / 100.0
                      RefundChance = sum ((=) "trigger_skills_refund_half_energy_spent_chance_%") }
                Some(meta, triggered |> List.map fst, consumers))

    /// Runic Ward Remnants from a persistent gem with `ward_remnants_gain_per_globe`, with its
    /// group's supports (Remnant Potency, Harmonic Remnants). Collecting takes `pickupSeconds`
    /// (guess); "remnant_delay_X_ms" adds to it. Ward overflows to 1.5x (user).
    let wardRemnants (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (pickupSeconds: float) =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.tryPick (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            enabled
            |> List.tryPick (fun gem ->
                if gem.IsSupport then None
                else
                    let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
                    let raw = Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports }
                    let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                    if sum "ward_remnants_gain_per_globe" <= 0.0 then None
                    else
                        Some
                            { OnKillChance = sum "ward_remnants_chance_to_spawn_orb_on_killing_enemy_%"
                              OnStunChance = sum "ward_remnants_chance_to_spawn_remnant_on_stun_%"
                              StunCooldown = sum "ward_remnants_spawn_remnant_on_stun_cooldown_ms" / 1000.0
                              WardPerRemnant = sum "ward_remnants_gain_per_globe" * (1.0 + sum "remnant_effect_+%" / 100.0)
                              ExtraChance = sum "chance_%_to_create_additional_remnant"
                              Delay = pickupSeconds + sum "remnant_delay_X_ms" / 1000.0
                              Overflow = 1.5 }))

    /// The elemental (and Verisium) infusions: maximum 3 each (BaseMaximumInfusionCount, plus
    /// "maximum X infusion stacks"), 20 s (BaseInfusionDurationMs, "infusion duration").
    let infusionResources = [ "FireInfusion"; "ColdInfusion"; "LightningInfusion"; "VerisiumInfusion" ]

    let infusionMaxima (sheet: CharacterSheet) =
        let mods = ModDb.ofList sheet.Mods
        let baseMax = Map.find "BaseMaximumInfusionCount" Tables.gameConstants
        infusionResources
        |> List.map (fun r -> r, int (baseMax + Mods.sumBase Ctx.empty [ Named("Max" + r); Named "MaxInfusions" ] mods))
        |> Map.ofList

    /// Infusion durations; Verisium ones also with their gem's "verisium infusion duration".
    let infusionDurations (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) =
        let mods = ModDb.ofList sheet.Mods
        let baseD = Map.find "BaseInfusionDurationMs" Tables.gameConstants / 1000.0 * Mods.inc Ctx.empty [ Named "InfusionDuration" ] mods
        let verisium =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport))
            |> List.sumBy (fun gem ->
                Gems.rawStats db mods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                |> List.filter (fun (id, _) -> id = "verisium_infusion_duration_+%") |> List.sumBy snd)
        infusionResources
        |> List.map (fun r -> r, (if r = "VerisiumInfusion" then baseD * (1.0 + verisium / 100.0) else baseD))
        |> Map.ofList

    /// Infusion remnants from a persistent gem (Siphon Elements) with its group's supports
    /// (Harmonic Remnants: a second remnant).
    let infusions (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (pickupSeconds: float) (consumers: Map<string, string * string>) =
        let charMods = ModDb.ofList sheet.Mods
        let remnants, extra, delay =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.tryPick (fun g ->
                let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
                enabled
                |> List.tryPick (fun gem ->
                    if gem.IsSupport then None
                    else
                        let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
                        let raw = Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports }
                        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                        let r =
                            [ Freeze, "ColdInfusion", sum "infusion_remnants_%_chance_to_spawn_cold_infusion_on_freezing_an_enemy"
                              Ignite, "FireInfusion", sum "infusion_remnants_%_chance_to_spawn_fire_infusion_on_igniting_an_enemy"
                              Shock, "LightningInfusion", sum "infusion_remnants_%_chance_to_spawn_lightning_infusion_on_shocking_an_enemy" ]
                            |> List.filter (fun (_, _, c) -> c > 0.0)
                        if r.IsEmpty then None
                        else Some(r, sum "chance_%_to_create_additional_remnant", pickupSeconds + sum "remnant_delay_X_ms" / 1000.0)))
            |> Option.defaultValue ([], 0.0, pickupSeconds)
        if remnants.IsEmpty && consumers.IsEmpty then None
        else
            Some
                { Remnants = remnants
                  ExtraRemnantChance = extra
                  Delay = delay
                  Consumers = consumers
                  Wildcard = Some "VerisiumInfusion" }

    /// A skill that grants Verisium infusions when used (Powered by Verisium): (count) from
    /// "verisium_infusion_skill_number_of_verisium_infusion_to_grant".
    let verisiumGrant (db: GameDb) (charMods: ModDb) (setup: GemSetup) =
        Gems.rawStats db charMods setup
        |> List.filter (fun (id, _) -> id = "verisium_infusion_skill_number_of_verisium_infusion_to_grant")
        |> List.sumBy snd
        |> int

    /// Buffs a skill grants when used, from its gem data:
    /// - a skill that consumes charges and has "..._buff_duration_per_stack_ms" /
    ///   "..._per_<charge>_charge_ms" (Charged Staff, Pinnacle of Power): one stack per charge
    ///   held, each lasting that long and carrying the gem's "..._per_stack" /
    ///   "..._per_<charge>_charge" stats; "chance to double effect of removing charges"
    ///   doubles the stacks;
    /// - any other non-persistent Buff skill with a duration: its stats for that duration.
    /// The buff is called "<skill> buff". Only the gem's own stats make the buff; supports
    /// scale its duration.
    let buffRules (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list =
        let own = Gems.rawStats db charMods { setup with Supports = [] }
        let withSupports = Gems.rawStats db charMods setup
        let sum id = withSupports |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
        let durationMult = Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
        // stats carry the gem's name ("charged_staff_..."): drop exactly that, so its words
        // aren't read as scopes ("staff")
        let namePrefix = (setup.Gem.ToLowerInvariant() |> String.map (fun ch -> if System.Char.IsLetterOrDigit ch then ch else '_')) + "_"
        let modsOf (id: string) v =
            let id = if id.StartsWith namePrefix then id.Substring namePrefix.Length else id
            match Gems.gemMeaning id v with
            | Mods ms -> ms |> List.map (Mod.from ("buff:" + skill.Name))
            | _ -> []
        let types = Gems.data db setup.Gem |> Option.map (fun d -> d.Types) |> Option.defaultValue Set.empty
        match skill.Consumes with
        | Some charge ->
            let word = charge.Replace("Charge", "").ToLowerInvariant()
            let stackSuffixes = [ "_per_stack"; "_per_" + word + "_charge" ]
            let seconds =
                own |> List.tryPick (fun (k, v) ->
                    if k.EndsWith "_buff_duration_per_stack_ms" || k.EndsWith("_buff_duration_per_" + word + "_charge_ms") then Some(v / 1000.0) else None)
            let perStack =
                own
                |> List.collect (fun (id, v) ->
                    match stackSuffixes |> List.tryFind (fun (sfx: string) -> id.EndsWith sfx) with
                    | Some sfx -> modsOf (id.Substring(0, id.Length - sfx.Length)) v
                    | None -> [])
                // Charged Staff's buff is for quarterstaff attacks
                |> List.map (fun m -> if setup.Gem = "Charged Staff" then Mod.whenever (SkillIs Quarterstaff) m else m)
            match seconds with
            | Some sec when not perStack.IsEmpty ->
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0
                    Reaction = ChargeBuff(skill.Name + " buff", charge, perStack, sec * durationMult, sum "chance_%_to_double_effect_of_removing_charges") } ]
            | _ -> []
        | None when types.Contains "Buff" && not (types.Contains "Persistent") && not (types.Contains "Mark") ->
            let seconds = own |> List.tryPick (fun (k, v) -> if k = "base_skill_effect_duration" then Some(v / 1000.0) else None)
            let noDamage = own |> List.exists (fun (k, v) -> k = "base_deal_no_damage" && v <> 0.0)
            let mods =
                own
                |> List.filter (fun (id, _) -> not (id.Contains "duration") && (noDamage || Gems.grantsToCharacter db id))
                |> List.collect (fun (id, v) -> modsOf id v)
            match seconds with
            | Some sec when not mods.IsEmpty ->
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(skill.Name + " buff", sec * durationMult, 1, mods) } ]
            | _ -> []
        | None -> []

    /// A curse or mark from its gem data: the debuff it puts on monsters, what it gives
    /// you against them, and its duration. Curses ("AppliesCurse") and Marks:
    /// - "base_skill_buff_<stat>_to_apply / _to_grant": resistances, armour, damage dealt;
    /// - "base_skill_debuff_action_speed_+%_final_to_inflict": action speed;
    /// - marks: extra crit damage against the marked enemy, more freeze / electrocute buildup.
    /// Returns (debuff name, duration, monster-side mods, player-side mods).
    let curseOf (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : (string * float * Mod list * Mod list) option =
        let types = Gems.data db setup.Gem |> Option.map (fun d -> d.Types) |> Option.defaultValue Set.empty
        let isCurse, isMark = types.Contains "AppliesCurse", types.Contains "Mark"
        if not (isCurse || isMark) then None
        else
            let raw = Gems.rawStats db charMods { setup with Supports = [] }
            let name = (if isCurse then "Curse:" else "Mark:") + skill.Name
            let against = EnemyHas name
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let all = ModDb.ofList (ModDb.toList charMods @ skill.Mods)
            let duration =
                (raw |> List.tryPick (fun (k, v) -> if k = "base_skill_effect_duration" then Some(v / 1000.0) else None) |> Option.defaultValue 8.0)
                * Mods.mult ctx [ SkillDuration; Named(if isCurse then "CurseDuration" else "MarkDuration") ] all
            let monster, player =
                raw
                |> List.fold
                    (fun (ms, ps) (id, v) ->
                        match id with
                        | "base_skill_buff_chaos_damage_resistance_%_to_apply" -> Mod.base' (Resist Chaos) v :: ms, ps
                        | "base_skill_buff_all_elements_resistance_%_to_apply" -> (DamageType.elemental |> List.map (fun t -> Mod.base' (Resist t) v)) @ ms, ps
                        | "base_skill_buff_total_armour_-_to_grant" -> Mod.base' Armour (-v) :: ms, ps
                        | "base_skill_buff_damage_+%_final_to_apply" -> Mod.more (Named "DamageDealt") v :: ms, ps
                        | "base_skill_debuff_action_speed_+%_final_to_inflict" -> Mod.more (Named "ActionSpeed") v :: ms, ps
                        | "enemy_additional_critical_strike_multiplier_against_self" -> ms, (Mod.base' CritBonus v |> Mod.whenever against) :: ps
                        | id when id.EndsWith "_hit_damage_freeze_multiplier_+%_final" -> ms, (Mod.more (Buildup Freeze) v |> Mod.whenever against) :: ps
                        | id when id.EndsWith "_hit_damage_electrocute_multiplier_+%" -> ms, (Mod.inc (Buildup Electrocute) v |> Mod.whenever against) :: ps
                        | _ -> ms, ps)
                    ([], [])
            // your curse / mark effect scales what it does
            let effect = Mods.mult ctx [ Named(if isCurse then "CurseEffect" else "MarkEffect") ] all
            Some(name, duration, monster |> List.map (Mod.scaled effect), player |> List.map (Mod.scaled effect >> Mod.from skill.Name))

    /// Impending Doom: "trigger an explosion when the curse ends". Recasting a curse on an enemy
    /// that has it ends the old one, so every cast onto a cursed enemy triggers Doom Blast (the
    /// support's granted skill, at the curse gem's level, with the group's supports). A curse
    /// running out on its own is not modelled. Returns the skill and the rule.
    let impendingDoom (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) (curseName: string) : (Skill * Rule) option =
        setup.Supports
        |> List.tryPick (fun (name, _) ->
            match Gems.data db name, Map.tryFind name db.Gems with
            | Some sd, Some gd when sd.StatSets |> List.exists (fun st -> st.Stats |> List.contains "trigger_vicious_hex_explosion_when_curse_ends") ->
                gd.AdditionalEffects |> List.tryPick (fun id -> if Map.containsKey id db.Skills then Some id else None)
            | _ -> None)
        |> Option.bind (fun blastId ->
            match Gems.skill db charMods None { setup with Gem = blastId } with
            | Ok (s, _) ->
                let name = skill.Name + ": " + s.Name
                Some(
                    { s with Name = name; Tags = s.Tags.Add Triggered; Cost = 0.0; Cooldown = None },
                    { Hook = OnHit; Filter = Called skill.Name; Chance = 100.0; Reaction = IfEnemyHas(curseName, TriggerSkill name) })
            | Error _ -> None)

    /// Warcries: using one empowers the next attacks, one per `warcry_empowers_per_X_monster_power`
    /// of enemy power, counting up to `..._mp_cap`. What empowered attacks get is on the gem:
    /// Infernal Cry "Empowered Attacks Gain X% of damage as extra Fire damage", Seismic Cry an
    /// extra aftershock for slams. Returns the use rule and the mods for empowered attacks.
    /// Other warcry effects (Guard, rage per power, knockback, the cry's explosion): not modelled.
    let warcry (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list * Mod list =
        match Gems.data db setup.Gem with
        | Some d when d.Types.Contains "Warcry" ->
            let raw = Gems.rawStats db charMods { setup with Supports = [] }
            let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
            let per, cap = sum "warcry_empowers_per_X_monster_power", sum "warcry_empowers_per_X_monster_power_mp_cap"
            if per <= 0.0 then [], []
            else
                let empowered (m: Mod) = m |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "EmpoweredAttacks") |> Mod.from skill.Name
                let fire = sum "infernal_cry_exerted_attack_all_damage_%_to_gain_as_fire_%"
                let aftershocks = sum "seismic_cry_slam_skill_additional_aftershocks"
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = Empower("EmpoweredAttacks", per, (if cap > 0.0 then cap else infinity)) } ],
                [ if fire > 0.0 then
                      for t in DamageType.all -> empowered (Mod.base' (GainAs(t, Fire)) fire)
                  if aftershocks > 0.0 then yield empowered (Mod.base' (Named "AftershockChance") (100.0 * aftershocks) |> Mod.whenever (SkillIs Slam)) ]
        | _ -> [], []

    /// What heralds do themselves (their buff's mods are on the character already). The rule is
    /// the gem's description, the damage its second stat set:
    /// - Herald of Ice: an attack that kills a frozen enemy makes it explode ("Explosion").
    /// - Herald of Thunder: after an attack kills a shocked enemy, lightning bolts strike the
    ///   targets of the next N attack hits ("Bolt", N = `herald_of_thunder_storm_max_hits`).
    /// The explosion is centred on the enemy aimed at, not on the corpse. A herald's own hits
    /// don't set heralds off. Herald of Ash, Blood, Plague: not modelled.
    let heralds (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Skill list * Rule list =
        let charMods = ModDb.ofList sheet.Mods
        let attack = WithTagNot(Attack, Herald)
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
            enabled
            |> List.filter (fun x -> not x.IsSupport && (x.Name = "Herald of Ice" || x.Name = "Herald of Thunder"))
            |> List.choose (fun gem ->
                let label = if gem.Name = "Herald of Ice" then "Explosion" else "Bolt"
                Gems.data db gem.Name
                |> Option.bind (fun d -> d.StatSets |> List.tryFindIndex (fun s -> s.Label = label))
                |> Option.bind (fun i ->
                    let setup = { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = i + 1; Supports = supports }
                    match Gems.skill db charMods (Gems.chooseWeapon db gem.Name sheet.Weapons) setup with
                    | Ok (s, _) ->
                        let name = gem.Name + ": " + label
                        let skill = { s with Name = name; Tags = s.Tags.Add(Triggered).Add(Herald); Parts = []; Cost = 0.0; Cooldown = None }
                        let rules =
                            if gem.Name = "Herald of Ice" then
                                [ { Hook = OnKillOf "Frozen"; Filter = attack; Chance = 100.0; Reaction = TriggerSkill name } ]
                            else
                                let bolts =
                                    Gems.rawStats db charMods { setup with StatSet = 1 }
                                    |> List.filter (fun (k, _) -> k = "herald_of_thunder_storm_max_hits") |> List.sumBy snd |> int |> max 1
                                [ { Hook = OnKillOf "Shocked"; Filter = attack; Chance = 100.0; Reaction = SetStacks("Herald of Thunder bolts", bolts) }
                                  { Hook = OnHit; Filter = attack; Chance = 100.0; Reaction = SpendStack("Herald of Thunder bolts", TriggerSkill name) } ]
                        Some(skill, rules)
                    | Error _ -> None)))
        |> List.unzip
        |> fun (skills, rules) -> skills, List.concat rules

    /// The game's standard named effects, with the numbers of its keyword text:
    /// Onslaught 20% increased skill speed; Arcane Surge 15% increased cast speed (its mana
    /// regeneration is not applied); Unholy Might 30% of damage gained as extra chaos; Tailwind
    /// 2% increased skill speed and 10% increased evasion per stack, up to 10; Withered 5%
    /// increased chaos damage taken per stack, up to 10. Each x its effect / magnitude modifiers.
    let private keywordEffect (sheetDb: ModDb) (name: string) : int * Mod list =
        let c = Ctx.empty
        let eff stat = Mods.inc c [ Named stat ] sheetDb
        match name with
        | "Onslaught" -> 1, [ Mod.inc Speed (20.0 * eff "OnslaughtEffect") ]
        | "ArcaneSurge" -> 1, [ Mod.inc Speed (15.0 * eff "ArcaneSurgeEffect") |> Mod.whenever (SkillIs Spell) ]
        | "UnholyMight" -> 1, [ for t in DamageType.all -> Mod.base' (GainAs(t, Chaos)) (30.0 * eff "UnholyMightMagnitude") ]
        | "Tailwind" -> 10, [ Mod.inc Speed (2.0 * eff "TailwindEffect") ]
        | "Withered" -> 10, [ Mod.inc (DamageTaken(OfType Chaos)) (5.0 * eff "WitheredMagnitude") ]
        | _ -> 1, []
        |> fun (n, ms) -> n, ms |> List.map (Mod.from name)

    /// Sheet mods for named effects the character simply has ("You have Unholy Might", also
    /// conditionally), and what they add for effects whose stacks sit on the player (Tailwind's
    /// evasion) or on the enemy (Critical Weakness: +0.5% crit chance per stack).
    let keywordMods (sheet: CharacterSheet) : Mod list =
        let db = ModDb.ofList sheet.Mods
        let has =
            sheet.Mods
            |> List.filter (fun m -> m.Stat = Named "Has:UnholyMight")
            |> List.collect (fun m -> snd (keywordEffect db "UnholyMight") |> List.map (fun k -> { k with Conds = m.Conds @ k.Conds }))
        has
        @ [ Mod.inc Evasion (10.0 * Mods.inc Ctx.empty [ Named "TailwindEffect" ] db) |> Mod.whenever (PerStack("Tailwind", 1.0)) |> Mod.from "Tailwind"
            Mod.base' CritChance 0.5 |> Mod.whenever (PerEnemyStack("CriticalWeakness", 1.0)) |> Mod.from "Critical Weakness" ]

    /// Rules from "Rule:<hook>:<effect>:<seconds>" stats on the character (any skill, or the
    /// kind its condition names) and on a skill's own gems (that skill only).
    let keywordRules (sheet: CharacterSheet) (skills: Skill list) : Rule list =
        let db = ModDb.ofList sheet.Mods
        let ruleOf (filter: SkillFilter) (m: Mod) =
            match m.Stat with
            | Named n when n.StartsWith "Rule:" && m.Value > 0.0 ->
                match n.Split(':') with
                | [| _; hook; effect; seconds |] ->
                    let hook = match hook with "OnCrit" -> Some OnCrit | "OnHit" -> Some OnHit | "OnKill" -> Some OnKill | "OnUse" -> Some OnUse | "WhenHit" -> Some WhenHit | _ -> None
                    let secs = float seconds
                    let filter = m.Conds |> List.tryPick (function SkillIs t -> Some(WithTag t) | _ -> None) |> Option.defaultValue filter
                    hook |> Option.map (fun h ->
                        let reaction =
                            if effect.StartsWith "-" then RemoveBuff(effect.Substring 1)
                            else
                                let maxStacks, mods = keywordEffect db effect
                                if effect = "Withered" then ApplyDebuff(effect, secs, maxStacks, mods) else GrantBuff(effect, secs, maxStacks, mods)
                        { Hook = h; Filter = filter; Chance = min 100.0 m.Value; Reaction = reaction })
                | _ -> None
            | _ -> None
        (sheet.Mods |> List.choose (ruleOf AnySkill))
        @ (skills |> List.collect (fun s -> s.Mods |> List.choose (ruleOf (Called s.Name))))

    /// Critical Weakness inflicted on enemies in your presence every second for N seconds: a
    /// steady N stacks (at most 20) on every enemy.
    let criticalWeaknessAura (sheet: CharacterSheet) : (string * int * Mod list) list =
        let n = Mods.sumBase Ctx.empty [ Named "CriticalWeaknessAura" ] (ModDb.ofList sheet.Mods)
        if n > 0.0 then [ "CriticalWeakness", min 20 (int n), [] ] else []

    /// Withering Presence: "Withers enemies in your Presence every N seconds" for its duration:
    /// a steady number of Withered stacks (duration / interval, at most 10) on every enemy.
    let witheringPresence (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : (string * int * Mod list) list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport))
        |> List.tryPick (fun gem ->
            let raw = Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
            let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
            let every, lasts = sum "skill_withering_presence_frequency_ms", sum "base_skill_effect_duration"
            if every > 0.0 && lasts > 0.0 then
                let maxStacks, mods = keywordEffect charMods "Withered"
                Some [ "Withered", min maxStacks (int (ceil (lasts / every))), mods ]
            else None)
        |> Option.defaultValue []

    /// Charms from the equipped items (API layout: "Flask: Lasts N Seconds", "Flask: Consumes N
    /// of M Charges on use", "Utility: <effect>", "Used when you ..."). Returns the charms and
    /// the sheet mods their effects are (conditional on the charm's buff).
    let charms (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Charm list * Mod list =
        let sheetDb = ModDb.ofList sheet.Mods
        let durationInc = Mods.inc Ctx.empty [ Named "CharmDuration" ] sheetDb
        let gainInc = Mods.sumInc Ctx.empty [ Named "CharmChargesGained" ] sheetDb
        let numbers (l: string) =
            l.Split(' ') |> Array.choose (fun w -> match System.Double.TryParse(w.Trim(',').TrimEnd('%')) with | true, v -> Some v | _ -> None) |> List.ofArray
        build.Items
        |> List.filter (fun (_, text) -> text.Contains "Flask: Lasts")
        |> List.mapi (fun i (_, text) ->
            let lines = text.Split('\n') |> Array.map (fun l -> l.Trim()) |> List.ofArray
            let find (start: string) = lines |> List.tryFind (fun l -> l.StartsWith start)
            let first (start: string) = find start |> Option.bind (numbers >> List.tryHead)
            let ownLine (part: string) = lines |> List.tryFind (fun l -> l.Contains part && not (l.StartsWith "Flask:") && not (l.StartsWith "Utility:")) |> Option.bind (numbers >> List.tryHead)
            let trigger =
                match find "Used when you " |> Option.map (fun l -> l.Substring 14) with
                | Some "become Frozen" -> "Frozen"
                | Some "become Stunned" -> "Stunned"
                | Some "become Ignited" -> "Ignited"
                | Some "become Shocked" -> "Shocked"
                | Some "start Bleeding" -> "Bleeding"
                | Some "become Poisoned" -> "Poisoned"
                | Some "are affected by a Slow" -> "Slowed"
                | Some "kill a Rare or Unique enemy" -> "KillRare"
                | Some t when t.StartsWith "take " && t.EndsWith " damage from a Hit" -> "Hit:" + t.Split(' ').[1]
                | _ -> ""
            let name = lines |> List.tryFind (fun l -> l.Contains "Charm") |> Option.defaultValue "Charm"
            let buff = sprintf "Charm%d" (i + 1)
            let utility = lines |> List.filter (fun l -> l.StartsWith "Utility: ") |> List.map (fun l -> l.Substring 9)
            let prevents = utility |> List.exists (fun l -> l.StartsWith "Grants Immunity" || l = "Cannot be Stunned" || l.Contains "unaffected by Slows")
            let stats, _ = StatText.parseLines db.Text utility
            let mods, _, _ = Items.interpret stats
            let consumes = find "Flask: Consumes" |> Option.map numbers |> Option.defaultValue []
            let perUse, maxC = (match consumes with [ a; b ] -> a, b | _ -> 0.0, 0.0)
            { Name = name
              Trigger = trigger
              Buff = buff
              Prevents = prevents
              Duration = (first "Flask: Lasts" |> Option.defaultValue 0.0) * durationInc
              ChargesPerUse = perUse
              MaxCharges = maxC
              ChargesPerSecond = ownLine "Charges per Second" |> Option.defaultValue 0.0
              ChargeOnKillChance = ownLine "Chance to gain a Charge when you kill an enemy" |> Option.defaultValue 0.0
              ChargeGainMult = 1.0 + ((ownLine "increased Charges gained" |> Option.defaultValue 0.0) + gainInc) / 100.0
              LifeOnUse = ownLine "Life when Used" |> Option.defaultValue 0.0
              ManaOnUse = ownLine "Mana when Used" |> Option.defaultValue 0.0
              StartsRecharge = lines |> List.exists (fun l -> l.Contains "Energy Shield Recharge starts on use") },
            mods |> List.map (Mod.whenever (WhileBuff buff) >> Mod.from name))
        |> List.filter (fun (c, _) -> c.Trigger <> "" && c.Duration > 0.0)
        |> List.unzip
        |> fun (cs, ms) -> cs, List.concat ms

    /// Blasphemy: the curses socketed with it are an aura. Every enemy has them all the time
    /// (the aura's area is not simulated), with the gem's less curse effect
    /// ("support_blasphemy_curse_effect_+%_final" of the support Blasphemy grants) and the
    /// group's supports. Returns the debuffs and the mods the curses give the player.
    let curseAuras (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : (string * int * Mod list) list * Mod list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            match enabled |> List.tryFind (fun x -> not x.IsSupport && (Gems.data db x.Name |> Option.exists (fun d -> d.Types.Contains "IsBlasphemy"))) with
            | None -> []
            | Some aura ->
                let less =
                    Map.tryFind aura.Name db.Gems
                    |> Option.map (fun gd -> gd.AdditionalEffects)
                    |> Option.defaultValue []
                    |> List.choose (fun id -> Map.tryFind id db.Skills)
                    |> List.collect (fun sd -> sd.StatSets |> List.tryHead |> Option.map (fun set -> Gems.stats sd set aura.Level 0) |> Option.defaultValue [])
                    |> List.filter (fun (id, _) -> id = "support_blasphemy_curse_effect_+%_final")
                    |> List.sumBy snd
                let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
                enabled
                |> List.filter (fun x -> not x.IsSupport && x.Name <> aura.Name)
                |> List.choose (fun gem ->
                    let setup = { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports }
                    match Gems.skill db charMods None setup with
                    | Ok (s, _) ->
                        curseOf db charMods setup s
                        |> Option.map (fun (name, _, monster, player) ->
                            let k = 1.0 + less / 100.0
                            (name, 1, monster |> List.map (Mod.scaled k)), player |> List.map (Mod.scaled k))
                    | Error _ -> None))
        |> List.unzip
        |> fun (auras, grants) -> auras, List.concat grants

    /// What a used skill does besides hitting, from its gem data:
    /// - "all elemental exposure magnitude" (Frost Bomb): exposure to fire, cold and lightning
    ///   for its secondary duration, x (1 + exposure effect);
    /// - GeneratesInfusion + GeneratesRemnants with one element (Frost Bomb: "leaving behind a
    ///   Cold Infusion"): a remnant at detonation, picked up `pickupSeconds` later.
    let useRules (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skillName: string) (pickupSeconds: float) : Rule list =
        let raw = Gems.rawStats db charMods setup
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let exposure =
            let m = sum "active_skill_all_elemental_exposure_magnitude"
            if m <= 0.0 then []
            else
                let amount = m * (1.0 + sum "exposure_effect_+%" / 100.0)
                let duration = sum "base_secondary_skill_effect_duration" / 1000.0
                [ for dt in [ Fire; Cold; Lightning ] ->
                    { Hook = OnUse; Filter = Called skillName; Chance = 100.0; Reaction = ApplyExposure(dt, amount, duration) } ]
        let remnant =
            match Gems.data db setup.Gem with
            | Some d when d.Types.Contains "GeneratesInfusion" && d.Types.Contains "GeneratesRemnants" && not (d.Types.Contains "Persistent") ->
                let elements = [ "Fire"; "Cold"; "Lightning" ] |> List.filter d.Types.Contains
                match elements with
                | [ el ] ->
                    let detonation = sum "base_skill_detonation_time" / 1000.0 * (1.0 + sum "support_fast_forward_detonation_time_+%_final" / 100.0)
                    [ { Hook = OnUse; Filter = Called skillName; Chance = 100.0
                        Reaction = DropRemnant(el + "Infusion", detonation + pickupSeconds, sum "chance_%_to_create_additional_remnant") } ]
                | _ -> []
            | _ -> []
        exposure @ remnant

    /// The character's Spirit and what its persistent skills reserve.
    /// Spirit: flat amounts (quests, mods, sceptres' own value) and "per X on body armour",
    /// x increases. Reserved: each persistent gem's `spiritReservationFlat` at its level plus
    /// its supports', Blasphemy's per socketed curse, / reservation efficiency (the character's
    /// and the gem's own, e.g. from its quality). A minion gem's reservation is per minion
    /// (the first few can be free). A skill granted by an item reserves nothing.
    type SpiritUse =
        { Gem: string
          /// Spirit for the gem itself (0 for minion gems), per minion, and minions that cost nothing.
          Flat: float
          PerMinion: float
          FreeMinions: int }

    /// Equipped corrupted items ("for each Corrupted Item Equipped"): not flasks, charms,
    /// jewels or the weapon set not in use.
    let corruptedItems (build: PobBuild) =
        build.Items
        |> List.filter (fun (slot, text) -> not (slot.StartsWith "Flask" || slot.StartsWith "Charm" || slot.StartsWith "Jewel" || slot.Contains "Swap") && text.Split('\n') |> Array.exists (fun l -> l.Trim() = "Corrupted"))
        |> List.length

    let spiritTotal (sheet: CharacterSheet) (build: PobBuild) =
        let db = ModDb.ofList sheet.Mods
        let c = { Ctx.empty with Buffs = Map.ofList [ "CorruptedItems", corruptedItems build ] }
        let item stat slot = sheet.ItemDefences |> List.sumBy (fun (s, st, v) -> if st = stat && (slot = "" || s = slot) then v else 0.0)
        let per name stat = match Mods.sumBase c [ Named name ] db with | n when n > 0.0 -> floor (item stat "Body Armour" / n) | _ -> 0.0
        // a sceptre's Spirit counts for the weapon set it is in: the set that gives more is taken
        let setOne = sheet.ItemDefences |> List.sumBy (fun (s, st, v) -> if st = Named "Spirit" && s.StartsWith "Weapon" then v else 0.0)
        let setTwo = Mods.sumBase c [ Named "SpiritOnSetTwo" ] db
        let fromItems = if Mods.flag c (Named "NoSpiritFromEquipment") db then 0.0 else item (Named "Spirit") "" - setOne + max setOne setTwo
        if Mods.flag c (Named "NoSpirit") db then 0.0 else
        (Mods.sumBase c [ Named "Spirit" ] db + fromItems + per "SpiritPerBodyES" MaxEnergyShield + per "SpiritPerBodyEvasion" Evasion)
        * Mods.mult c [ Named "Spirit" ] db
        |> floor

    let spiritUses (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : SpiritUse list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled && g.Slot <> "granted")
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let reservation (name: string) (level: int) =
                Gems.data db name
                |> Option.bind (fun d -> Map.tryFind level d.Levels |> Option.orElse (Map.tryFind 1 d.Levels))
                |> Option.bind (fun l -> l.SpiritReservation)
                |> Option.defaultValue 0.0
            let level (x: PobGem) = Gems.data db x.Name |> Option.bind (fun d -> Map.tryFind x.Level d.Levels |> Option.orElse (Map.tryFind 1 d.Levels))
            enabled
            |> List.filter (fun x -> not x.IsSupport)
            |> List.choose (fun gem ->
                match Gems.data db gem.Name with
                | Some d when d.Types.Contains "Persistent" || d.Types.Contains "HasReservation" ->
                    let tags = d.Types |> Set.toList |> List.map (fun t -> Tag("Type:" + t)) |> Set.ofList
                    let own = reservation gem.Name gem.Level
                    // supports' own Spirit, and their "+N% reservation", which multiply with each other
                    // In a meta gem a support belongs to the meta gem or to a skill socketed in it:
                    // one that fits a socketed skill and not the meta gem costs the meta gem nothing.
                    let others = enabled |> List.filter (fun x -> not x.IsSupport && x.Name <> gem.Name) |> List.choose (fun x -> Gems.data db x.Name)
                    let forOther (x: PobGem) =
                        match Gems.data db x.Name with
                        | Some s -> not (Gems.canSupport s d.Types) && others |> List.exists (fun o -> Gems.canSupport s o.Types)
                        | None -> false
                    let supportGems = enabled |> List.filter (fun x -> x.IsSupport && not (forOther x))
                    let supports = supportGems |> List.sumBy (fun x -> reservation x.Name x.Level)
                    let supportMult = supportGems |> List.fold (fun acc x -> acc * (1.0 + (level x |> Option.map (fun l -> l.ReservationMultiplier) |> Option.defaultValue 0.0) / 100.0)) 1.0
                    let raw = Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                    let stat id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                    // a gem's own reservation efficiency (level, quality) is for that gem only: read
                    // from its stats here, never from the buff it gives the character
                    let forGem = sheet.Mods |> List.filter (fun m -> not (m.Source.StartsWith "buff:")) |> ModDb.ofList
                    let ownEff = stat "base_reservation_efficiency_+%" + stat "base_spirit_reservation_efficiency_+%"
                    let ctx = { Ctx.empty with SkillTags = tags; SkillName = d.Name }
                    let eff =
                        (Mods.inc ctx [ Named "ReservationEfficiency" ] forGem + ownEff / 100.0) * Mods.more ctx [ Named "ReservationEfficiency" ] forGem
                        / (Mods.more ctx [ Named "Reservation" ] forGem * supportMult)
                        |> max 0.01
                    // Blasphemy: per curse socketed with it
                    let curses = enabled |> List.filter (fun x -> not x.IsSupport && x.Name <> gem.Name) |> List.length
                    let perCurse = stat "blasphemy_base_spirit_reservation_per_socketed_curse" * float curses
                    let isMinion = d.Types.Contains "Minion" || not d.MinionList.IsEmpty
                    if isMinion then Some { Gem = gem.Name; Flat = 0.0; PerMinion = (own + supports) / eff; FreeMinions = int (stat "first_X_minions_have_0_base_spirit_reservation") }
                    elif own + supports + perCurse > 0.0 then Some { Gem = gem.Name; Flat = (own + supports + perCurse) / eff; PerMinion = 0.0; FreeMinions = 0 }
                    else None
                | _ -> None))

    /// How many minions each persistent minion gem has: the free ones, plus what the Spirit
    /// left after everything else pays for. With several minion gems the spare Spirit is
    /// shared out evenly (the game lets the player choose; the API doesn't say).
    let minionCounts (spirit: float) (uses: SpiritUse list) : Map<string, int> =
        let fixedCost = uses |> List.sumBy (fun u -> u.Flat)
        let minionGems = uses |> List.filter (fun u -> u.PerMinion > 0.0)
        let spare = max 0.0 (spirit - fixedCost)
        let share = if minionGems.IsEmpty then 0.0 else spare / float minionGems.Length
        minionGems
        |> List.map (fun u -> u.Gem, max 1 (u.FreeMinions + int (floor (share / u.PerMinion))))
        |> Map.ofList

    /// Persistent minions of a build as actors. Each minion is the monster its gem names
    /// (skills' minion list; "Spectre: X" / "Companion: X" by monster name) at the gem's level:
    /// its attacks use "weapon" damage = monster damage of that level x the monster's damage
    /// multiplier (+- spread) at its attack time; its skills scale with its level; the gem's
    /// supports apply to them; "Minions deal / have ..." mods of the owner apply.
    /// Returns the actors and notes about what was left out.
    let minions (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Actor list * string list =
        let charMods = ModDb.ofList sheet.Mods
        // as many of each as the character's Spirit pays for
        let counts = minionCounts (spiritTotal sheet build) (spiritUses db sheet build)
        let minionMods = Mods.forMinions sheet.Mods
        let levelOf (gemName: string) (gemLevel: int) =
            let req name = Gems.data db name |> Option.bind (fun d -> Map.tryFind gemLevel d.Levels) |> Option.map (fun l -> int l.LevelRequirement)
            // spectre / companion gems aren't in the gem data: take a minion gem's level curve
            req gemName |> Option.filter (fun l -> l > 0) |> Option.orElse (req "Skeletal Warrior") |> Option.defaultValue sheet.Level |> max 1
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.map (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
            enabled
            |> List.filter (fun x -> not x.IsSupport)
            |> List.map (fun gem ->
                let after (prefix: string) = if gem.Name.StartsWith prefix then Some(gem.Name.Substring prefix.Length) else None
                let byName = after "Spectre: " |> Option.orElse (after "Companion: ")
                let data = Gems.data db gem.Name
                let persistent = byName.IsSome || (data |> Option.exists (fun d -> d.Types.Contains "Persistent"))
                let monsters =
                    match byName with
                    | Some n -> db.Minions |> Map.toList |> List.map snd |> List.filter (fun m -> m.Name = n) |> List.truncate 1
                    | None -> data |> Option.map (fun d -> d.MinionList) |> Option.defaultValue [] |> List.truncate 1 |> List.choose (fun id -> Map.tryFind id db.Minions)
                match monsters with
                | [] ->
                    [], (if byName.IsSome || (data |> Option.exists (fun d -> not d.MinionList.IsEmpty)) then [ sprintf "%s: no monster data" gem.Name ] else [])
                | _ when not persistent -> [], [ sprintf "%s: temporary minions are not simulated" gem.Name ]
                | m :: _ ->
                    let level = levelOf gem.Name gem.Level
                    let avg = m.Damage * Constants.monsterDamage level
                    let weapon =
                        { Damage = Map.ofList [ Physical, { Min = avg * (1.0 - m.DamageSpread); Max = avg * (1.0 + m.DamageSpread) } ]
                          CritChance = m.CritChance
                          AttacksPerSecond = 1.0 / max 0.05 m.AttackTime
                          ReloadTime = 0.0
                          Tags = [] }
                    let skills =
                        m.SkillList
                        |> List.choose (fun sid ->
                            match Gems.minionSkill db charMods (Some weapon) sid (float level) supports with
                            | Ok (s, _) when not s.BaseDamage.IsEmpty -> Some { s with Name = m.Name + ": " + s.Name; Cost = 0.0 }
                            | _ -> None)
                    let estimate (s: Skill) =
                        (s.BaseDamage |> Map.toList |> List.sumBy (fun (_, r) -> (r.Min + r.Max) / 2.0)) * s.BaseMultiplier / max 0.05 s.BaseTime
                    // cooldown skills when ready, otherwise the strongest of the rest
                    let ordered =
                        (skills |> List.filter (fun s -> s.Cooldown.IsSome))
                        @ (skills |> List.filter (fun s -> s.Cooldown.IsNone) |> List.sortByDescending estimate |> List.truncate 1)
                    // a pack counts as several ("skill_wolf_pack_size")
                    let count =
                        Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                        |> List.tryPick (fun (k, v) -> if k.EndsWith "_pack_size" then Some(int v) else None)
                        |> Option.defaultValue 1
                        |> fun pack -> pack * (Map.tryFind gem.Name counts |> Option.defaultValue 1)
                    // a minion without any damaging skill (a healer) simply doesn't fight
                    if ordered.IsEmpty then [], []
                    else
                        [ for i in 1 .. count ->
                            let name = if count > 1 then sprintf "%s %d" m.Name i else m.Name
                            Actor.make name minionMods ordered ],
                        []))
        |> List.concat
        |> List.unzip
        |> fun (actors, notes) -> List.concat actors, List.concat notes

    /// Totems of a build as actors. A totem gem ("SummonsTotem") grants a second skill, the
    /// totem's own attack or spell (gems.json additionalGrantedEffectId); the totem uses it
    /// with the owner's weapon and mods (it is the owner's skill, used by proxy) and the gem's
    /// supports. It acts only while the buff "<gem> buff" is on the player: that is the totem
    /// standing, placed by using the gem (see `totemPlacement`). Up to the gem's totem limit
    /// stand together; placing each one separately is not simulated.
    /// Meta totems (Spell Totem, Ancestral Warrior Totem, Mortar Cannon) use the group's
    /// socketed skill instead.
    let totems (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Actor list * string list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
            enabled
            |> List.filter (fun x -> not x.IsSupport && (Gems.data db x.Name |> Option.exists (fun d -> d.Types.Contains "SummonsTotem")))
            |> List.map (fun gem ->
                let effect =
                    Map.tryFind gem.Name db.Gems
                    |> Option.bind (fun gd -> gd.AdditionalEffects |> List.tryFind (fun id -> Map.containsKey id db.Skills && not (id.StartsWith "Support")))
                // meta totems use the group's socketed skill instead
                let skillGem =
                    match effect with
                    | Some id -> Some id
                    | None -> enabled |> List.tryFind (fun x -> not x.IsSupport && x.Name <> gem.Name) |> Option.map (fun x -> x.Name)
                match skillGem with
                | None -> [], [ sprintf "%s: no skill for the totem to use" gem.Name ]
                | Some id ->
                    let weapon = Gems.chooseWeapon db (if effect.IsSome then gem.Name else id) sheet.Weapons
                    match Gems.skill db charMods weapon { Gem = id; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports } with
                    | Ok (s, _) ->
                        let rec asTotem (sk: Skill) =
                            { sk with Name = gem.Name + ": " + sk.Name; Tags = sk.Tags.Add Totem; Cost = 0.0; Parts = sk.Parts |> List.map asTotem }
                        let s = asTotem s
                        let limit =
                            Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports }
                            |> List.filter (fun (k, _) -> k = "base_number_of_totems_allowed") |> List.sumBy snd |> int |> max 1
                        [ for i in 1 .. limit ->
                            { Actor.make (if limit > 1 then sprintf "%s %d" gem.Name i else gem.Name + " (totem)") sheet.Mods [ s ] with ActiveWhile = Some(gem.Name + " buff") } ],
                        []
                    | Error e -> [], [ sprintf "%s: %s" gem.Name e ]))
        |> List.unzip
        |> fun (actors, notes) -> List.concat actors, List.concat notes

    /// Using a totem gem places the totem: the buff its actor waits for, lasting the totem's
    /// duration ("base_totem_duration" x skill duration mods).
    let totemPlacement (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list =
        match Gems.data db setup.Gem with
        | Some d when d.Types.Contains "SummonsTotem" ->
            let raw = Gems.rawStats db charMods setup
            let seconds = (raw |> List.filter (fun (k, _) -> k = "base_totem_duration") |> List.sumBy snd) / 1000.0
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let duration = (if seconds > 0.0 then seconds else 8.0) * Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
            [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(skill.Name + " buff", duration, 1, []) } ]
        | _ -> []

    /// Skills that keep hitting after one use ("Sustained": Orb of Storms, Solar Orb,
    /// Thunderstorm, Firestorm...): the hits of one use, timed from its end, and the buff that
    /// marks the effect as standing (so it isn't recast while it lasts).
    /// The interval is the gem's "..._frequency_ms" / "..._pulse_delay_ms" /
    /// "..._every_x_ms" stat; the number of hits its "..._maximum_number_of_hits" /
    /// "..._number_of_pulses", else duration / interval.
    let sustained (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : ((HitTiming * float) list * Rule list) option =
        match Gems.data db setup.Gem with
        | Some d when d.Types.Contains "Sustained" || (Gems.rawStats db charMods { setup with Supports = [] } |> List.exists (fun (k, v) -> k.EndsWith "_hit_interval_ms" && v > 0.0)) ->
            // a projectile that keeps hitting what it stays on (Twister: "..._hit_interval_ms" for
            // its duration) is used again and again; a Sustained effect stands and isn't recast
            let stands = d.Types.Contains "Sustained"
            let own = Gems.rawStats db charMods { setup with Supports = [] }
            let pick (endings: string list) = own |> List.tryPick (fun (k, v) -> if v > 0.0 && endings |> List.exists (fun (e: string) -> k.EndsWith e) then Some v else None)
            let interval = pick [ "_frequency_ms"; "_pulse_delay_ms"; "_pulse_interval_ms"; "_every_x_ms"; "_fireball_delay_ms"; "_hit_interval_ms" ] |> Option.map (fun v -> v / 1000.0)
            let fixedCount = pick [ "_maximum_number_of_hits"; "_number_of_pulses" ]
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let duration = skill.Duration * Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
            match interval with
            | Some dt when dt > 0.0 ->
                let n =
                    match fixedCount with
                    | Some c -> int c
                    | None -> if duration > 0.0 then int (duration / dt) else 1
                let n = max 1 (min 200 n)
                let lasts = if duration > 0.0 then duration else float n * dt
                Some(
                    [ for k in 1 .. n -> After(float k * dt), 1.0 ],
                    (if stands then [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(skill.Name + " buff", lasts, 1, []) } ] else []))
            | _ -> None
        | _ -> None

    /// Elemental Archon (BuffTemplates ArchonElemental: 25% more elemental damage with spells,
    /// 100% more ignite chance / freeze buildup / shock chance with spells), plus "Archon Buffs
    /// also grant ..." mods, all x increased Archon effect. Base duration 10 s, recovery 20 s
    /// (poe2db), scaled by Archon duration / "recovery period expires faster"; none with
    /// "no recovery period".
    let elementalArchon (sheet: CharacterSheet) =
        let mods = ModDb.ofList sheet.Mods
        let c = Ctx.empty
        let effect = Mods.inc c [ Named "ArchonEffect" ] mods
        let grant id = Mods.sumBase c [ Named("ArchonGrants:" + id) ] mods * effect
        let spell m = m |> Mod.whenever (SkillIs Spell)
        { Name = "Elemental Archon"
          OnIgniteStat = "ElementalArchonOnIgnite"
          Duration = 10.0 * Mods.inc c [ Named "ArchonDuration" ] mods
          Recovery = if Mods.flag c (Named "ArchonNoRecovery") mods then 0.0 else 20.0 / Mods.inc c [ Named "ArchonRecoveryFaster" ] mods
          Mods =
            [ spell (Mod.more (Damage Elemental) (25.0 * effect))
              spell (Mod.more (AilmentChance Ignite) (100.0 * effect))
              spell (Mod.more (Buildup Freeze) (100.0 * effect))
              spell (Mod.more (AilmentChance Shock) (100.0 * effect))
              Mod.inc CritChance (grant "archon_grants_critical_strike_chance_+%")
              Mod.base' CritBonus (grant "archon_grants_base_critical_strike_multiplier_+") ]
            @ Mod.resistAllElemental (grant "archon_grants_all_elemental_resistance_+%")
            |> List.map (Mod.from "Elemental Archon") }
