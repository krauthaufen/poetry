namespace PoEsie

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

    /// Persistent minions of a build as actors. Each minion is the monster its gem names
    /// (skills' minion list; "Spectre: X" / "Companion: X" by monster name) at the gem's level:
    /// its attacks use "weapon" damage = monster damage of that level x the monster's damage
    /// multiplier (+- spread) at its attack time; its skills scale with its level; the gem's
    /// supports apply to them; "Minions deal / have ..." mods of the owner apply.
    /// Returns the actors and notes about what was left out.
    let minions (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Actor list * string list =
        let charMods = ModDb.ofList sheet.Mods
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
                    if ordered.IsEmpty then [], [ sprintf "%s: none of the minion's skills could be built" gem.Name ]
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
