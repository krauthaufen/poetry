namespace PoEsie

/// Per-skill behaviour that stats alone don't describe. Each entry says where its
/// rules come from; numbers are read from the gem's stats.
module Behaviours =
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
              yield { Name = "generate charges"; Every = chargeGenSeconds; Action = GainAnyRandom charges }
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
