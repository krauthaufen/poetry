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
