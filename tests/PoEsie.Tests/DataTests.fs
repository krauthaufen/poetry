/// Tests against the exported game data (data/game). Expected values are read off
/// the raw data or worked out by hand, never taken from PoB's output.
module PoEsie.DataTests

open PoEsie

#if !FABLE_COMPILER
let private db =
    lazy
        (let dir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data", "game")
         GameDb.load (fun f -> System.IO.File.ReadAllText(System.IO.Path.Combine(dir, f))))

let run (test: string -> (unit -> unit) -> unit) (near: float -> float -> unit) (equal: obj -> obj -> unit) =
    let parse line = StatText.parse db.Value.Text line

    test "text: increased / reduced resolve to signed stat values" (fun () ->
        equal (box (Some [ "attack_damage_+%", 12.0 ])) (box (parse "12% increased Attack Damage"))
        equal (box (Some [ "attack_damage_+%", -10.0 ])) (box (parse "10% reduced Attack Damage")))

    test "text: roll ranges read as their average, sign kept" (fun () ->
        equal (box (Some [ "base_fire_damage_resistance_%", 7.5 ])) (box (parse "+(5-10)% to Fire Resistance")))

    test "text: two-value lines" (fun () ->
        equal
            (box (Some [ "attack_minimum_added_physical_damage", 5.0; "attack_maximum_added_physical_damage", 10.0 ]))
            (box (parse "Adds 5 to 10 Physical Damage to Attacks")))

    test "meaning: scoped damage, more, conditions, minions" (fun () ->
        let mods id v = match StatMap.meaning id v with Mods ms -> ms | m -> failwithf "%s -> %A" id m
        equal (box [ Mod.inc (Damage AllDamage) 20.0 |> Mod.whenever (SkillIs Spell) ]) (box (mods "spell_damage_+%" 20.0))
        equal (box [ Mod.inc (Damage(OfType Fire)) 20.0 ]) (box (mods "fire_damage_+%" 20.0))
        equal (box [ Mod.more (Damage AllDamage) 25.0 |> Mod.whenever (SkillIs Spell) ]) (box (mods "support_controlled_destruction_spell_damage_+%_final" 25.0))
        equal (box [ Mod.inc (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "Blinded") ]) (box (mods "damage_+%_vs_blinded_enemies" 30.0))
        equal (box [ { Mod.inc (Damage AllDamage) 40.0 with Stat = ForMinions(Damage AllDamage) } ]) (box (mods "minion_damage_+%" 40.0))
        equal (box [ Mod.base' (Penetration Cold) 15.0 ]) (box (mods "base_reduce_enemy_cold_resistance_%" 15.0)))

    test "meaning: unknown words are never guessed" (fun () ->
        equal (box Unmapped) (box (StatMap.meaning "damage_+%_per_frobnicated_widget" 10.0)))

    test "gem: Spark level 20 base damage, crit and cast time come from the gem data" (fun () ->
        match Gems.skill db.Value ModDb.empty None { Gem = "Spark"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            near 11.0 s.BaseDamage.[Lightning].Min
            near 216.0 s.BaseDamage.[Lightning].Max
            near 9.0 s.BaseCrit
            near 0.7 s.BaseTime
        | Error e -> failwith e)

    test "gem: +levels from the character raise the gem level" (fun () ->
        let mods = ModDb.ofList [ Mod.base' (Named "GemLevel") 1.0 |> Mod.whenever (SkillIs Spell) ]
        match Gems.skill db.Value mods None { Gem = "Spark"; Level = 19; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) -> near 216.0 s.BaseDamage.[Lightning].Max
        | Error e -> failwith e)

    test "item: local damage, quality and attack speed apply to the weapon" (fun () ->
        let item =
            "Item Class: One Hand Maces\nRarity: Rare\nDoom Knuckle\nPlated Mace\n--------\nQuality: +20% (augmented)\n--------\n"
            + "Adds 10 to 20 Physical Damage\n40% increased Physical Damage\n15% increased Attack Speed\n30% increased Fire Damage\n"
        let p = Items.parse db.Value item
        let w = p.Weapon.Value
        // Plated Mace: 18-38 physical, 1.4 APS, 5% crit
        near ((18.0 + 10.0) * 1.6) w.Damage.[Physical].Min
        near ((38.0 + 20.0) * 1.6) w.Damage.[Physical].Max
        near (1.4 * 1.15) w.AttacksPerSecond
        equal (box [ Mod.inc (Damage(OfType Fire)) 30.0 ]) (box p.Mods))
#else
let run (_: string -> (unit -> unit) -> unit) (_: float -> float -> unit) (_: obj -> obj -> unit) = ()
#endif
