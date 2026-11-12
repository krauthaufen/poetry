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

    /// Which charges the build can generate at all, and whether without killing anything
    /// (true) or only from kills, culls and corpses (false). A fight starts with the charges
    /// that have a source; against a lone boss only with those that need no kill.
    let chargeSources (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Map<string, bool> =
        let mods = ModDb.ofList sheet.Mods
        let has name = Mods.flag Ctx.empty (Named name) mods || Mods.sumBase Ctx.empty [ Named name ] mods > 0.0
        let word c = match c with "PowerCharge" -> "Power Charge" | "FrenzyCharge" -> "Frenzy Charge" | _ -> "Endurance Charge"
        let fromSheet =
            (if has "GeneratesRandomCharges" then charges |> List.map (fun c -> c, true) else [])
            @ (if has "PowerChargeOnInfusionConsumed" then [ "PowerCharge", true ] else [])
        let fromGems =
            build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled))
            |> List.choose (fun x -> Gems.data db x.Name)
            |> List.filter (fun d -> d.Types.Contains "GeneratesCharges")
            |> List.collect (fun d ->
                let needsKill = [ "kill"; "Cull"; "Corpse" ] |> List.exists (fun w -> d.Description.Contains w)
                charges |> List.filter (fun c -> d.Description.Contains(word c)) |> List.map (fun c -> c, not needsKill))
        // a source that needs no kill wins over one that does
        fromSheet @ fromGems |> List.groupBy fst |> List.map (fun (c, l) -> c, l |> List.exists snd) |> Map.ofList

    /// Ghost Dance: "When Hit, lose a Ghost Shroud. Regenerate Energy Shield equal to N% of
    /// your Evasion Rating per second if you have lost a Ghost Shroud Recently". Returns the
    /// most shrouds and the percent of evasion per second.
    let ghostDance (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : (int * float) option =
        let mods = ModDb.ofList sheet.Mods
        build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems)
        |> List.filter (fun x -> x.Enabled && not x.IsSupport && (Gems.data db x.Name).IsSome)
        |> List.tryPick (fun gem ->
            let raw = Gems.rawStats db mods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
            let stat id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
            let shrouds = stat "ghost_dance_max_stacks"
            if shrouds > 0.0 then
                Some(int shrouds, stat "skill_base_ghost_dance_grants_%_evasion_as_es_regeneration_per_minute_if_have_lost_ghost_dance_shroud_recently" / 60.0)
            else None)

    let periodics (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (chargeGenSeconds: float) =
        let mods = ModDb.ofList sheet.Mods
        let sum stat = Mods.sumBase Ctx.empty [ Named stat ] mods
        [ let consume = sum "ConsumeEachChargeEveryMs"
          if consume > 0.0 then
              yield { Name = "consume charges"; Every = consume / 1000.0; Action = ConsumeEach charges }
          let fragment = sum "DivineFragmentEvery"
          if fragment > 0.0 then
              yield { Name = "fragment of divinity"; Every = fragment; Action = GainRandom [ "DivineFragment" ] }
          // Into the Breach: a Flame of Chayula appears every so often; a purple one, picked up,
          // gives "N% of damage as extra Chaos" for 8 seconds, each on its own, ten at most
          for g in build.Groups |> List.filter (fun g -> g.Enabled) do
              for gem in g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport) do
                  let setup = { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                  let raw = if (Gems.data db gem.Name).IsSome then Gems.rawStats db mods setup else []
                  let stat id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                  // Time of Need: "periodically invokes divine blessings to heal you"
                  let heal, healEvery = stat "skill_time_of_need_gain_x_life", stat "base_cooldown_modifiable_repeat_interval_ms" / 1000.0
                  if heal > 0.0 && healEvery > 0.0 then
                      yield { Name = gem.Name; Every = healEvery / Mods.mult Ctx.empty [ CooldownRecovery ] mods; Action = HealLife heal }
                  // Ghost Dance: "Gain a Ghost Shroud every N seconds"
                  if stat "ghost_dance_max_stacks" > 0.0 && healEvery > 0.0 then
                      yield { Name = gem.Name; Every = healEvery / Mods.mult Ctx.empty [ CooldownRecovery ] mods; Action = GainRandom [ "GhostShroud" ] }
                  let every = stat "breach_flame_spawn_rate_ms" / 1000.0
                  let perFlame = stat "breach_flame_chaos_addition_%"
                  if every > 0.0 && perFlame > 0.0 then
                      let flag name = Mods.flag Ctx.empty (Named name) mods
                      let doubled = if flag "BreachFlameDoubled" then 2.0 else 1.0
                      let effect = 1.0 + (stat "remnant_effect_+%" + Mods.sumInc Ctx.empty [ Named "RemnantEffect" ] mods) / 100.0
                      let chance = if flag "BreachFlameOnly:purple" then 1.0 elif flag "BreachFlameOnly:red" || flag "BreachFlameOnly:blue" then 0.0 else 1.0 / 3.0
                      let value = floor (perFlame * effect * doubled)
                      if chance > 0.0 then
                          yield { Name = "flames of chayula"; Every = every
                                  Action = GainTimedBuff("Purple Flame", chance, 8.0 * doubled, 10, [ for t in DamageType.all -> Mod.base' (GainAs(t, Chaos)) value |> Mod.from gem.Name ]) }
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
        // no stat set of its own for the infused form, but stats that say what consuming does
        // (Arc: "200% more damage when Lightning Infused", "Chains 6 additional times")
        | Some element, None ->
            let raw = Gems.rawStats db charMods { setup with Supports = [] }
            let ending (e: string) = raw |> List.filter (fun (k, _) -> k.EndsWith e) |> List.sumBy snd
            let more, chains = ending "_damage_+%_final_from_infusion_consumption", ending "_chains_gained_from_infusion_consumption"
            if more = 0.0 && chains = 0.0 then None
            else
                match Gems.skill db charMods mainHand setup with
                | Ok (skill, _) ->
                    Some(
                        element,
                        { skill with
                            Mods = (if more <> 0.0 then [ Mod.more (Damage AllDamage) more |> Mod.from "infusion consumed" ] else []) @ skill.Mods
                            Delivery =
                                match skill.Delivery with
                                | Projectiles p -> Projectiles { p with Chains = p.Chains + int chains }
                                | other -> other })
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

    /// Mana Remnants (the persistent gem with `mana_remnants_mana_gain_per_globe`), with its
    /// group's supports. Collecting takes `pickupSeconds` (guess). Mana overflows to twice the
    /// maximum (guess: the game only says it can overflow).
    let manaRemnants (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) (pickupSeconds: float) =
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
                    if sum "mana_remnants_mana_gain_per_globe" <= 0.0 then None
                    else
                        Some
                            { ManaRemnants.OnKillChance = sum "mana_remnants_chance_to_spawn_orb_on_killing_enemy_with_elemental_ailment_%"
                              CritCooldown = sum "mana_remnants_spawn_remnant_on_crit_vs_elemental_ailment_enemy_every_X_ms" / 1000.0
                              ManaPerRemnant = sum "mana_remnants_mana_gain_per_globe" * (1.0 + (sum "remnant_effect_+%" + Mods.sumInc Ctx.empty [ Named "RemnantEffect" ] charMods) / 100.0)
                              PercentOfMaxMana = Mods.sumBase Ctx.empty [ Named "RemnantRecoverManaPercent" ] charMods
                              ExtraChance = sum "chance_%_to_create_additional_remnant"
                              Delay = pickupSeconds + sum "remnant_delay_X_ms" / 1000.0
                              Overflow = 2.0 }))

    /// Banners (War, Defiance, Dread): the gem reserves Spirit and grants the banner skill, which
    /// needs Glory from attack hits and then gives its aura for a duration. Only what the engine
    /// uses of the aura is taken: attack damage, attack speed, accuracy.
    let banners (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Banner list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let supportStats =
                enabled |> List.filter (fun x -> x.IsSupport) |> List.collect (fun x ->
                    match Gems.data db x.Name with
                    | Some s when s.Support -> s.StatSets |> List.tryHead |> Option.map (fun set -> Gems.stats s set x.Level 0) |> Option.defaultValue []
                    | _ -> [])
            enabled |> List.filter (fun x -> not x.IsSupport) |> List.collect (fun gem ->
                Map.tryFind gem.Name db.Gems |> Option.map (fun g -> g.AdditionalEffects) |> Option.defaultValue []
                |> List.choose (fun id -> Map.tryFind id db.Skills)
                |> List.choose (fun d ->
                    match d.StatSets |> List.tryHead with
                    | Some set when d.Types.Contains "Banner" ->
                        let stats = Gems.stats d set gem.Level gem.Quality @ supportStats
                        let sum id = stats |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                        let effect = 1.0 + (Mods.sumInc Ctx.empty [ Named "BannerEffect" ] charMods) / 100.0
                        let glory = sum "active_skill_requires_X_glory" * (1.0 + sum "support_glory_required_+%_final" / 100.0)
                        if glory <= 0.0 then None
                        else
                            Some
                                { Banner.Name = gem.Name
                                  Glory = glory
                                  PerAttackHit = sum "active_skill_generates_mp_%_glory_per_attack_hit"
                                  Duration = sum "base_skill_effect_duration" / 1000.0 * Mods.mult Ctx.empty [ SkillDuration ] charMods
                                  Mods =
                                    [ if sum "base_skill_buff_attack_damage_+%_final_to_apply" <> 0.0 then
                                          yield Mod.more (Damage AllDamage) (sum "base_skill_buff_attack_damage_+%_final_to_apply" * effect) |> Mod.whenever (SkillIs Attack)
                                      if sum "base_skill_buff_banner_attack_speed_+%_to_apply" <> 0.0 then
                                          yield Mod.inc Speed (sum "base_skill_buff_banner_attack_speed_+%_to_apply" * effect) |> Mod.whenever (SkillIs Attack)
                                      if sum "base_skill_buff_banner_accuracy_+%_to_apply" <> 0.0 then
                                          yield Mod.inc (Named "Accuracy") (sum "base_skill_buff_banner_accuracy_+%_to_apply" * effect) ]
                                    |> List.map (Mod.from ("banner:" + gem.Name)) }
                    | _ -> None)))

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

    /// "Curse zones erupt after a delay" (`curse_delay_duration_ms`), slower or faster with
    /// "Curse Activation"; none with "Curses have no Activation Delay". Marks have none.
    let curseDelay (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : float =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
        let all = ModDb.ofList (ModDb.toList charMods @ skill.Mods)
        if Mods.flag ctx (Named "NoCurseDelay") all then 0.0
        else
            (raw |> List.sumBy (fun (k, v) -> if k = "curse_delay_duration_ms" then v / 1000.0 else 0.0))
            * max 0.0 (Mods.mult ctx [ Named "CurseDelay" ] all)

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

    /// Pain Offering: while its spike stands, minions deal increased damage and attack and cast
    /// faster. The skeleton it takes is not removed. Returns the rules (a marker buff on the
    /// player keeps it from being recast while it lasts).
    let offering (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let damage, speed = sum "pain_offering_damage_+%", sum "pain_offering_attack_and_cast_speed_+%"
        let spell = sum "power_offering_buff_spell_damage_+%"
        if spell > 0.0 then
            // Soul Offering: increased spell damage for the player while it stands
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let duration = sum "base_skill_effect_duration" / 1000.0 * Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
            [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0
                Reaction = GrantBuff(skill.Name + " buff", duration, 1, [ Mod.inc (Damage AllDamage) spell |> Mod.whenever (SkillIs Spell) |> Mod.from skill.Name ]) } ]
        elif damage = 0.0 && speed = 0.0 then []
        else
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let duration = sum "base_skill_effect_duration" / 1000.0 * Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
            let mods = [ Mod.inc (Damage AllDamage) damage; Mod.inc Speed speed ] |> List.map (Mod.from skill.Name)
            [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantMinionBuff(skill.Name, duration, mods) }
              { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(skill.Name + " buff", duration, 1, []) } ]

    /// Freezing Mark, Voltaic Mark: "When the Marked target is Frozen, the Mark Activates,
    /// granting you a Buff which gives extra Cold damage". The buff is given by a hit on a marked
    /// enemy that is frozen (electrocuted) and lasts the gem's secondary duration. The mark is
    /// not taken off: while the enemy stays frozen the buff is refreshed (a little generous).
    let markBuff (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let pick (ending: string) = raw |> List.tryPick (fun (k, v) -> if k.EndsWith ending && v > 0.0 then Some v else None)
        let duration = (raw |> List.filter (fun (k, _) -> k = "base_secondary_skill_effect_duration") |> List.sumBy snd) / 1000.0
        // Sniper's Mark: "The next Critical Hit the target receives Activates the Mark, Consuming
        // it to deal extra damage and grant you a Frenzy Charge"
        let sniper =
            let consumedByCrit = Gems.data db setup.Gem |> Option.exists (fun d -> d.Types.Contains "Mark" && d.Description.Contains "next Critical Hit")
            if not consumedByCrit then []
            else
                let mark = "Mark:" + skill.Name
                [ if (Gems.data db setup.Gem).Value.Description.Contains "Frenzy Charge" then
                      yield { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = IfEnemyHas(mark, GainResource("FrenzyCharge", 1)) }
                  yield { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = IfEnemyHas(mark, RemoveDebuff mark) } ]
        let activated =
            [ "_damage_buff_damage_%_to_gain_as_cold", Cold, "Frozen"
              "_damage_buff_damage_%_to_gain_as_lightning", Lightning, "Electrocuted" ]
        sniper
        @ (activated
        |> List.collect (fun (ending, element, state) ->
            match pick ending with
            | Some v when duration > 0.0 ->
                let mods = [ for t in DamageType.all -> Mod.base' (GainAs(t, element)) v |> Mod.from skill.Name ]
                [ { Hook = OnHit; Filter = AnySkill; Chance = 100.0
                    Reaction = IfEnemyHas("Mark:" + skill.Name, IfEnemyHas(state, GrantBuff(skill.Name + " activated", duration, 1, mods))) } ]
            | _ -> []))

    /// Skills that empower the attacks that follow:
    /// - Mantra of Destruction: the next attack gains a share of all damage as extra chaos;
    /// - Temper Weapon: every strike of the hammer empowers 3 attacks (12 at most) to Combust
    ///   when they hit: the gem's second effect, an area attack triggered by the hit.
    /// An empowerment is used up by an attack when its last hit has landed.
    /// Returns extra skills, rules and mods for the empowered attacks.
    let empowerNext (db: GameDb) (charMods: ModDb) (weapon: Weapon option) (setup: GemSetup) (skill: Skill) : Skill list * Rule list * Mod list =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let buff = "EmpoweredAttacks:" + skill.Name
        let chaos = sum "mantra_of_destruction_grant_all_damage_%_to_gain_as_chaos_with_attacks"
        let perStrike, cap = sum "temper_weapon_empowers_per_strike", sum "imbue_weapon_max_exerts"
        if chaos > 0.0 then
            [],
            [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = SetStacks(buff, max 1 (int (sum "skill_empowers_next_x_melee_attacks"))) } ],
            [ for t in DamageType.all do
                yield Mod.base' (GainAs(t, Chaos)) chaos |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff buff) |> Mod.from skill.Name ]
        elif perStrike > 0.0 then
            let combust =
                Map.tryFind setup.Gem db.Gems |> Option.map (fun g -> g.AdditionalEffects) |> Option.defaultValue []
                |> List.tryPick (fun id ->
                    match Gems.skill db charMods weapon { setup with Gem = id } with
                    | Ok (s, _) -> Some { s with Name = skill.Name + ": " + s.Name; Tags = s.Tags.Add Triggered; Cost = 0.0; Cooldown = None }
                    | Error _ -> None)
            match combust with
            | Some c ->
                [ c ],
                [ for _ in 1 .. int perStrike -> { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(buff, 1e9, int cap, []) } ]
                @ [ { Hook = OnHit; Filter = WithTagNot(Attack, Triggered); Chance = 100.0; Reaction = IfBuff(buff, TriggerSkill c.Name) } ],
                []
            | None -> [], [], []
        else [], [], []

    /// Sigil of Power: a sigil for its duration whose buff grants more spell damage per Stage;
    /// "Gains a Stage when you Spend a total of X% of your Maximum Mana while in Area",
    /// "Minimum 1 second between gaining Stages". The player is taken to stand in it; it starts
    /// with one Stage (guess). The engine counts the mana spent (Sim.sigilSpend).
    let sigilOfPower (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : Rule list =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let perStage = sum "circle_of_power_spell_damage_+%_final_per_stage"
        if perStage = 0.0 then []
        else
            let ctx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
            let duration = sum "base_skill_effect_duration" / 1000.0 * Mods.mult ctx [ SkillDuration ] (ModDb.ofList (ModDb.toList charMods @ skill.Mods))
            let buff = skill.Name + " buff"
            let mods =
                [ Mod.more (Damage AllDamage) perStage |> Mod.whenever (SkillIs Spell)
                  Mod.base' (Named "SigilMaxStages") (sum "circle_of_power_max_stages")
                  Mod.base' (Named "SigilManaPercentPerStage") (sum "base_circle_of_power_mana_%_spend_per_upgrade") ]
                |> List.map (Mod.from skill.Name)
            [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = RemoveBuff buff }
              { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff(buff, duration, 1, mods) } ]

    /// Demon Form: "You gain Demonflame every second you remain in demon form, causing your
    /// Life to be lost at an ever-increasing rate. Maximum 10 Demonflame. Revert to human form
    /// if you reach 1 Life". Cast speed while in the form, more spell damage per Demonflame.
    /// The engine counts the Demonflame and takes the life (Sim.step); the buff carries the
    /// numbers. Returns the skill (not pressed again while in the form), the rule and the mods.
    let demonForm (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : (Skill * Rule list * Mod list) option =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let perStack = sum "demon_form_spell_damage_+%_final_per_stack"
        if perStack <= 0.0 then None
        else
            let carried =
                [ Mod.base' (Named "DemonLifeLossPerStack") (sum "demon_form_life_loss_per_minute_per_stack" / 60.0)
                  Mod.base' (Named "DemonCooldown") (skill.Cooldown |> Option.defaultValue 0.0) ]
                |> List.map (Mod.from skill.Name)
            Some(
                { skill with Mods = Mod.flag (Named "ToggleBuff:Demon Form") :: skill.Mods },
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = GrantBuff("Demon Form", 1e9, 1, carried) } ],
                [ Mod.more (Damage AllDamage) perStack |> Mod.whenever (SkillIs Spell) |> Mod.whenever (PerStack("Demonflame", 1.0))
                  Mod.inc Speed (sum "demon_form_grants_cast_speed_+%") |> Mod.whenever (SkillIs Spell) |> Mod.whenever (WhileBuff "Demon Form") ]
                |> List.map (Mod.from skill.Name))

    /// Tempest Bell: "The Bell damages enemies on impact and can be Hit by your skills, creating a
    /// damaging shockwave". The bell stands for `bell_hit_limit` hits: each attack hit with another
    /// skill sets off the Shockwave part (at most one per `bell_shockwave_cooldown_ms`), the
    /// first with the impact. The bell is taken to stand where the attacks land (guess).
    /// Returns the skill with its shockwave waiting to be set off, and the rules.
    let tempestBell (db: GameDb) (charMods: ModDb) (setup: GemSetup) (skill: Skill) : (Skill * Rule list) option =
        let stats = Gems.data db setup.Gem |> Option.map (fun d -> d.StatSets |> List.collect (fun st -> st.ConstantStats)) |> Option.defaultValue []
        let value id = stats |> List.tryPick (fun (k, v) -> if k = id then Some v else None) |> Option.defaultValue 0.0
        let limit = value "bell_hit_limit"
        match skill.Parts |> List.tryFind (fun p -> p.Name.EndsWith ": Shockwave") with
        | Some wave when limit > 0.0 ->
            let wave' =
                { wave with
                    Cooldown = (match value "bell_shockwave_cooldown_ms" with 0.0 -> None | ms -> Some(ms / 1000.0))
                    Cost = 0.0
                    Tags = wave.Tags.Add Triggered
                    Mods = (Mod.flag (Named "OnlyTriggered") |> Mod.whenever (SkillNamed wave.Name)) :: wave.Mods }
            Some(
                { skill with Parts = skill.Parts |> List.map (fun p -> if p.Name = wave.Name then wave' else p) },
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = SetStacks("TempestBell", int limit) }
                  { Hook = OnHit; Filter = WithTagNot(Attack, Tag("Gem:" + setup.Gem)); Chance = 100.0; Reaction = SpendStack("TempestBell", TriggerSkill wave.Name) } ])
        | _ -> None

    /// Ice-Tipped Arrows: "Empowering your next Repeatable Bow or Projectile Spear Attacks to
    /// Convert Physical Damage to Cold Damage and Trigger Ice Fragments on Hit". The gem's second
    /// effect is the fragments: they land around the target and explode after their detonation
    /// time; four of the five are taken to reach a boss-sized target (guess).
    /// Returns the fragment skill, the rules and the mods for the empowered attacks.
    let iceTipped (db: GameDb) (charMods: ModDb) (weapon: Weapon option) (setup: GemSetup) (skill: Skill) : (Skill * Rule list * Mod list) option =
        let raw = Gems.rawStats db charMods { setup with Supports = [] }
        let sum id = raw |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
        let n = sum "shearing_bolts_number_of_empowered_attacks"
        if n <= 0.0 then None
        else
            let buff = "EmpoweredRepeatable:" + skill.Name
            let repeatable = Tag "Type:Barrageable"
            Map.tryFind setup.Gem db.Gems |> Option.map (fun g -> g.AdditionalEffects) |> Option.defaultValue []
            |> List.tryPick (fun id ->
                match Gems.skill db charMods weapon { setup with Gem = id } with
                | Ok (s, _) -> Some(id, s)
                | Error _ -> None)
            |> Option.map (fun (id, s) ->
                let fragRaw = Gems.rawStats db charMods { setup with Gem = id; Supports = [] }
                let fsum k = fragRaw |> List.filter (fun (x, _) -> x = k) |> List.sumBy snd
                let name = skill.Name + ": " + s.Name
                { s with
                    Name = name; Tags = s.Tags.Add Triggered; Cost = 0.0
                    HitDelay = fsum "base_skill_detonation_time" / 1000.0
                    // every fragment is thrown on from the enemy hit, into a cone away from the
                    // player, and explodes where it lands
                    Delivery =
                        Projectiles
                            { ProjectileShape.make (max 1 (int (fsum "base_number_of_projectiles"))) with
                                FromImpact = true
                                Scatter = Some(fsum "ice_fragment_min_chunk_distance", fsum "ice_fragment_max_chunk_distance", fsum "ballistic_ice_chunk_cone_angle", fsum "active_skill_base_area_of_effect_radius") }
                    Hits = [ 1.0, 1.0 ] },
                [ { Hook = OnUse; Filter = Called skill.Name; Chance = 100.0; Reaction = SetStacks(buff, int n) }
                  { Hook = OnHit; Filter = WithTag repeatable; Chance = 100.0; Reaction = IfBuff(buff, TriggerSkill name) } ],
                [ Mod.base' (Convert(Physical, Cold)) (sum "shearing_bolts_non_skill_base_physical_damage_%_to_convert_to_cold")
                  |> Mod.whenever (SkillIs repeatable) |> Mod.whenever (WhileBuff buff) |> Mod.from skill.Name ])

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
    /// don't set heralds off. Herald of Ash: see `heraldOfAsh`. Blood, Plague: not modelled.
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

    /// Herald of Ash: a non-herald attack that kills with overkill of at least
    /// `herald_of_ash_overkill_threshold_%` of the enemy's life makes it explode: enemies in the
    /// explosion's area are ignited for `herald_of_ash_burning_%_overkill_damage_per_minute` of
    /// the overkill damage, for the explosion's duration. The explosion (stat set "Explosion")
    /// is returned as a skill that is never used: it carries the area and duration.
    let heraldOfAsh (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) : Skill list * OverkillBurn list =
        let charMods = ModDb.ofList sheet.Mods
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.collect (fun g ->
            let enabled = g.Gems |> List.filter (fun x -> x.Enabled)
            let supports = enabled |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
            enabled
            |> List.filter (fun x -> not x.IsSupport)
            |> List.choose (fun gem ->
                let setup n = { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = n; Supports = supports }
                let sum n id = Gems.rawStats db charMods (setup n) |> List.filter (fun (k, _) -> k = id) |> List.sumBy snd
                let threshold = sum 1 "herald_of_ash_overkill_threshold_%"
                Gems.data db gem.Name
                |> Option.filter (fun _ -> threshold > 0.0)
                |> Option.bind (fun d -> d.StatSets |> List.tryFindIndex (fun s -> s.Stats |> List.contains "herald_of_ash_burning_%_overkill_damage_per_minute"))
                |> Option.bind (fun i ->
                    match Gems.skill db charMods None (setup (i + 1)) with
                    | Ok (s, _) ->
                        let name = gem.Name + ": Explosion"
                        Some({ s with Name = name; Tags = s.Tags.Add(Triggered).Add(Herald); Parts = []; Cost = 0.0; Cooldown = None },
                             { Skill = name; Filter = WithTagNot(Attack, Herald); ThresholdPercent = threshold
                               PercentPerSecond = sum (i + 1) "herald_of_ash_burning_%_overkill_damage_per_minute" / 60.0 })
                    | Error _ -> None)))
        |> List.unzip

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

    /// Skills an item triggers: "Hits with this Weapon have N% chance to Trigger X per M
    /// Strength" is a "TriggerOnHit:X" stat; every hit of an attack the player uses rolls it.
    /// Returns (skill name, rule).
    let itemTriggers (sheet: CharacterSheet) (str: float, dex: float, int': float) : (string * Rule) list =
        let db = ModDb.ofList sheet.Mods
        let c = { Ctx.empty with Buffs = Map.ofList [ "Str", int str; "Dex", int dex; "Int", int int' ] }
        sheet.Mods
        |> List.choose (fun m -> match m.Stat with | Named n when n.StartsWith "TriggerOnHit:" -> Some n | _ -> None)
        |> List.distinct
        |> List.map (fun n ->
            let skill = n.Substring 13
            skill, { Hook = OnHit; Filter = WithTagNot(Attack, Triggered); Chance = min 100.0 (Mods.sumBase c [ Named n ] db); Reaction = TriggerSkill skill })

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
                                // "up to a maximum of 30 on each target" (stat text)
                                if effect = "BloodstoneLance" then ApplyDebuff(effect, secs, 30, [])
                                elif effect = "Withered" then ApplyDebuff(effect, secs, maxStacks, mods) else GrantBuff(effect, secs, maxStacks, mods)
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
                // "Initially applies X% Elemental Exposure and Y% Exposure per pulse, up to a maximum of Z%"
                let scale = 1.0 + sum "exposure_effect_+%" / 100.0
                let perPulse, cap = sum "active_skill_all_elemental_exposure_compounding_magnitude" * scale, sum "active_skill_all_elemental_exposure_compounding_magnitude_cap" * scale
                let detonation = sum "base_skill_detonation_time" / 1000.0 * (1.0 + sum "support_fast_forward_detonation_time_+%_final" / 100.0)
                let pulses = int (detonation / Constants.orbPulseInterval + 1e-9)
                [ for dt in [ Fire; Cold; Lightning ] do
                    yield { Hook = OnUse; Filter = Called skillName; Chance = 100.0; Reaction = ApplyExposure(dt, amount, duration) }
                    if perPulse > 0.0 && cap > amount && pulses > 0 then
                        yield { Hook = OnUse; Filter = Called skillName; Chance = 100.0
                                Reaction = GrowExposure(dt, amount, perPulse, cap, Constants.orbPulseInterval, pulses, duration) } ]
        let remnant =
            match Gems.data db setup.Gem with
            | Some d when d.Types.Contains "GeneratesInfusion" && d.Types.Contains "GeneratesRemnants" && not (d.Types.Contains "Persistent") ->
                let elements = [ "Fire"; "Cold"; "Lightning" ] |> List.filter d.Types.Contains
                match elements with
                | [ el ] ->
                    let detonation = sum "base_skill_detonation_time" / 1000.0 * (1.0 + sum "support_fast_forward_detonation_time_+%_final" / 100.0)
                    // a seed (Living Bomb) leaves its remnant when it explodes, not when it is planted
                    // and a skill that only succeeds on an enemy that can be culled (Lightning Warp:
                    // "Creates a Lightning Infusion on successful use") when it does
                    let seed = sum "living_bomb_damage_threshold_%_of_ailment_threshold" > 0.0 || raw |> List.exists (fun (k, _) -> k = "active_skill_show_cull_range")
                    [ { Hook = (if seed then OnHit else OnUse); Filter = Called skillName; Chance = 100.0
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
          FreeMinions: int
          /// Percent of maximum life reserved instead ("Reserve Life instead of Spirit").
          LifePercent: float }

    /// Equipped corrupted items ("for each Corrupted Item Equipped"): not flasks, charms,
    /// jewels or the weapon set not in use.
    let corruptedItems (build: PobBuild) =
        build.Items
        |> List.filter (fun (slot, text) -> not (slot.StartsWith "Flask" || slot.StartsWith "Charm" || slot.StartsWith "Jewel" || slot.Contains "Swap") && text.Split('\n') |> Array.exists (fun l -> l.Trim() = "Corrupted"))
        |> List.length

    let spiritTotal (sheet: CharacterSheet) (build: PobBuild) =
        let db = ModDb.ofList sheet.Mods
        let spectrums = build.Items |> List.filter (fun (slot, text) -> slot.StartsWith "Jewel" && text.Contains "Grand Spectrum") |> List.length
        let c = { Ctx.empty with Buffs = Map.ofList [ "CorruptedItems", corruptedItems build; "GrandSpectrum", spectrums ] }
        let sheet = { sheet with ItemDefences = sheet.ItemDefences |> List.filter (fun (s, _, _) -> not (s.Contains "Swap")) }
        let item stat slot = sheet.ItemDefences |> List.sumBy (fun (s, st, v) -> if st = stat && (slot = "" || s = slot) then v else 0.0)
        let per name stat = match Mods.sumBase c [ Named name ] db with | n when n > 0.0 -> floor (item stat "Body Armour" / n) | _ -> 0.0
        // a sceptre's Spirit counts for the weapon set it is in: the set that gives more is taken
        let setOne = sheet.ItemDefences |> List.sumBy (fun (s, st, v) -> if st = Named "Spirit" && s.StartsWith "Weapon" then v else 0.0)
        let setTwo = Mods.sumBase c [ Named "SpiritOnSetTwo" ] db
        let fromItems = if Mods.flag c (Named "NoSpiritFromEquipment") db then 0.0 else item (Named "Spirit") "" - setOne + max setOne setTwo
        // "+40 to Spirit for each of your empty Charm slots": slots from the belt ("Has 2 Charm
        // Slots") and "+1 Charm Slot" lines, three at most, less the charms worn
        let emptyCharmSlots =
            let count (text: string) =
                text.Split('\n') |> Array.sumBy (fun l ->
                    let w = l.Trim().Split(' ')
                    if (l.Contains "Charm Slot") && w.Length >= 3 then
                        (if w.[0] = "Has" then (match System.Double.TryParse w.[1] with | true, v -> v | _ -> 0.0)
                         elif w.[0].StartsWith "+" then (match System.Double.TryParse(w.[0].Substring 1) with | true, v -> v | _ -> 0.0)
                         else 0.0)
                    else 0.0)
            let slots = (build.Items |> List.sumBy (fun (slot, text) -> if slot.Contains "Swap" then 0.0 else count text)) + (build.ExtraLines |> List.sumBy count)
            let worn = build.Items |> List.filter (fun (_, text) -> text.Contains "Flask: Lasts") |> List.length
            max 0.0 (min 3.0 slots - float worn)
        // from the sheet's own numbers (Setup puts them on the sheet as "Sheet:Life", "Sheet:Dex", ...):
        // "+25 to Spirit while you have at least 200 Dexterity", "+1 to Maximum Spirit per 25 Maximum Life"
        let own name = Mods.sumBase c [ Named("Sheet:" + name) ] db
        let fromSheet =
            ([ "Str"; "Dex"; "Int" ] |> List.sumBy (fun a -> if own a >= 200.0 then Mods.sumBase c [ Named(sprintf "SpiritIfAtLeast|%s|200" a) ] db else 0.0))
            + (match Mods.sumBase c [ Named "SpiritPerLife" ] db with
               | per when per > 0.0 -> floor (own "Life" / per)
               | _ -> 0.0)
        if Mods.flag c (Named "NoSpirit") db then 0.0 else
        (Mods.sumBase c [ Named "Spirit" ] db + Mods.sumBase c [ Named "SpiritPerEmptyCharmSlot" ] db * emptyCharmSlots + fromSheet + fromItems + per "SpiritPerBodyES" MaxEnergyShield + per "SpiritPerBodyEvasion" Evasion)
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
                    // Atziri's Communion: "Reserve Life instead of Spirit", so many hundredths of a
                    // percent of life per point of Spirit
                    let lifePerSpirit =
                        Gems.rawStats db charMods { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supportGems |> List.map (fun x -> x.Name, x.Level) }
                        |> List.filter (fun (k, _) -> k = "skill_reserves_X_life_permyriad_per_spirit_instead_of_spirit") |> List.sumBy snd
                    if isMinion then Some { Gem = gem.Name; Flat = 0.0; PerMinion = (own + supports) / eff; FreeMinions = int (stat "first_X_minions_have_0_base_spirit_reservation"); LifePercent = 0.0 }
                    elif own + supports + perCurse > 0.0 && lifePerSpirit > 0.0 then
                        Some { Gem = gem.Name; Flat = 0.0; PerMinion = 0.0; FreeMinions = 0; LifePercent = (own + supports + perCurse) / eff * lifePerSpirit / 100.0 }
                    elif own + supports + perCurse > 0.0 then Some { Gem = gem.Name; Flat = (own + supports + perCurse) / eff; PerMinion = 0.0; FreeMinions = 0; LifePercent = 0.0 }
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
            // (a ball in flight does not stand: the next one is cast while it travels)
            let stands = d.Types.Contains "Sustained" && not (Map.containsKey setup.Gem Constants.passingProjectileSpeed)
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
                    | None when duration > 0.0 -> int (duration / dt)
                    | None ->
                        // a slow projectile that strikes what is near as it passes (Ball Lightning):
                        // one discharge per interval for its whole flight; each hits what is
                        // within its radius of where the ball is by then
                        match Map.tryFind setup.Gem Constants.passingProjectileSpeed, pick [ "active_skill_base_area_of_effect_radius" ] with
                        | Some speed, Some _ -> int ((ProjectileShape.make 1).Range / speed / dt + 1e-9)
                        | _ -> 1
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
