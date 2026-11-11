/// Tests against the exported game data (data/game). Expected values are read off
/// the raw data or worked out by hand, never taken from PoB's output.
module PoEtry.DataTests

open PoEtry

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
        // Plated Mace: 18-38 physical, 1.4 APS, 5% crit; quality multiplies the increased damage
        near ((18.0 + 10.0) * 1.4 * 1.2) w.Damage.[Physical].Min
        near ((38.0 + 20.0) * 1.4 * 1.2) w.Damage.[Physical].Max
        near (1.4 * 1.15) w.AttacksPerSecond
        equal (box [ Mod.inc (Damage(OfType Fire)) 30.0 ]) (box p.Mods))

    // ---- the character sheet ----
    let dir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data", "game")
    let treeJson = System.IO.File.ReadAllText(System.IO.Path.Combine(dir, "tree.json"))
    let tree = Tree.parseNodes treeJson
    let bases = Defence.classBases treeJson
    let blank : PobBuild =
        { Level = 1; ClassName = "Witch"; Ascendancy = ""; Items = []; TreeNodes = []; TreeNodesSet1 = []; TreeNodesSet2 = []
          Groups = []; MainGroup = 1; Config = Map.empty; TreeOverrides = Map.empty; ExtraLines = [] }
    let sheetOf (b: PobBuild) = Pob.character db.Value tree (Tree.jewelSlots treeJson) b
    let stats (b: PobBuild) = Defence.compute bases (sheetOf b)
    let item slot (lines: string list) = slot, String.concat "\n" ([ "Rarity: Rare"; "Test Item"; "--------" ] @ lines)
    let resist t (v: PlayerStats) = Map.find t v.Resist

    test "sheet: class bases, also under the ascendancy's name" (fun () ->
        // Witch: 7 / 7 / 15; life 16 + 12 per level + 2 per Strength; mana 30 + 4 per level + 2 per Intelligence
        let v = stats blank
        near 7.0 v.Str
        near 15.0 v.Int
        near (16.0 + 12.0 + 14.0) v.Life
        near (30.0 + 4.0 + 30.0) v.Mana
        near 15.0 (stats { blank with ClassName = "Infernalist" }).Int
        near 15.0 (stats { blank with ClassName = "Titan" }).Str)

    test "sheet: elemental resistances start at -60, chaos at 0; evasion at 7" (fun () ->
        let v = stats blank
        near -60.0 (resist Fire v)
        near 0.0 (resist Chaos v)
        near 7.0 v.Evasion)

    test "sheet: an armour piece's own armour lines are in the armour it shows" (fun () ->
        let body = item "Body Armour" [ "Armour: 500"; "--------"; "+100 to Armour"; "50% increased Armour"; "+30 to maximum Life" ]
        let v = stats { blank with Items = [ body ] }
        near 500.0 v.Armour
        near (42.0 + 30.0) v.Life)

    test "sheet: 'Armour: 300' is the item's armour, 'Armour: +18% ...' a rune line for armour pieces" (fun () ->
        let helmet = item "Helmet" [ "Armour: 300"; "--------"; "Armour: +18% to Fire Resistance"; "Martial Weapon: 20% increased Attack Speed" ]
        let v = stats { blank with Items = [ helmet ] }
        near 300.0 v.Armour
        near -42.0 (resist Fire v)
        equal (box 0) (box ((sheetOf { blank with Items = [ helmet ] }).Mods |> List.filter (fun m -> m.Stat = Speed) |> List.length)))

    test "sheet: 'per Socket filled' counts the item's runes" (fun () ->
        let body = item "Body Armour" [ "Energy Shield: 100"; "Sockets filled: 3"; "--------"; "+10% to all Elemental Resistances per Socket filled"; "+7 to all Attributes per Socket filled" ]
        let v = stats { blank with Items = [ body ] }
        near -30.0 (resist Cold v)
        near (7.0 + 21.0) v.Str)

    test "sheet: a ring that reflects the other ring is a copy of it" (fun () ->
        let ring = item "Ring 1" [ "+20 to Strength" ]
        let mirror = item "Ring 2" [ "Reflects opposite Ring" ]
        near (7.0 + 40.0) (stats { blank with Items = [ ring; mirror ] }).Str)

    test "sheet: increases for one slot add to the global ones, for that item only" (fun () ->
        let body = item "Body Armour" [ "Evasion Rating: 1000"; "--------" ]
        let boots = item "Boots" [ "Evasion Rating: 100"; "--------" ]
        let lines = [ "100% increased Evasion Rating"; "50% increased Evasion Rating from Equipped Body Armour" ]
        let v = stats { blank with Items = [ body; boots ]; ExtraLines = lines }
        near (1000.0 * 2.5 + 100.0 * 2.0 + 7.0 * 2.0) v.Evasion)

    test "sheet: Eldritch Battery turns item energy shield into base mana" (fun () ->
        let body = item "Body Armour" [ "Energy Shield: 200"; "--------" ]
        let eb = tree |> Map.pick (fun id n -> if n.IsKeystone && n.Name = "Eldritch Battery" then Some id else None)
        let lines = [ "50% increased maximum Energy Shield"; "10% increased maximum Mana" ]
        let without = stats { blank with Items = [ body ]; ExtraLines = lines }
        let withEb = stats { blank with Items = [ body ]; ExtraLines = lines; TreeNodes = [ eb ] }
        near 300.0 without.EnergyShield
        near 0.0 withEb.EnergyShield
        // 200 item energy shield becomes base mana: the mana increase applies, the energy shield increase doesn't
        near (without.Mana + 200.0 * 1.1) withEb.Mana
        // granted by a line that is just its name
        let jewel = item "Jewel 1" [ "Eldritch Battery" ]
        near withEb.Mana (stats { blank with Items = [ body; jewel ]; ExtraLines = lines }).Mana)

    test "sheet: weapon set passives only count for their set" (fun () ->
        let node = tree |> Map.pick (fun id n -> if n.Stats = [ "+8 to Strength" ] then Some id else None)
        near 15.0 (stats { blank with TreeNodesSet1 = [ node ] }).Str
        near 7.0 (stats { blank with TreeNodesSet2 = [ node ] }).Str
        // and a passive allocated on the tree and by an item counts once
        near 15.0 (stats { blank with TreeNodes = [ node; node ] }).Str)

    test "sheet: a weapon's own leech and crit bonus are for attacks with it" (fun () ->
        let mace = "Weapon 1", "Rarity: Rare\nDoom Knuckle\nPlated Mace\n--------\nLeeches 8% of Physical Damage as Life\n+25% to Critical Damage Bonus\n"
        let mods = (sheetOf { blank with Items = [ mace ] }).Mods
        let attack = { Ctx.empty with SkillTags = Set.ofList [ Attack ] }
        let spell = { Ctx.empty with SkillTags = Set.ofList [ Spell ] }
        let db' = ModDb.ofList mods
        near 25.0 (Mods.sumBase attack [ CritBonus ] db')
        near 0.0 (Mods.sumBase spell [ CritBonus ] db')
        near 8.0 (Mods.sumBase attack [ Named "LifeLeech" ] db'))

    test "attributes scale stats; fire resistance mirrored to cold and lightning" (fun () ->
        let helmet lines = item "Helmet" ([ "Energy Shield: 100"; "--------" ] @ lines)
        let stats lines = stats { blank with Items = [ helmet lines ] }
        let plain = stats []
        // "1% increased Energy Shield per 2 Strength": Strength 40 more than without -> at least 20% more of the 100
        let str = stats [ "+40 to Strength"; "1% increased Energy Shield per 2 Strength" ]
        near (floor ((plain.Str + 40.0) / 2.0)) ((str.EnergyShield / plain.EnergyShield - 1.0) * 100.0)
        // +50% fire resistance at 50% of its value: +25% cold and lightning; the cap moves with fire's
        let res t (v: PlayerStats) = v.Resist.[t]
        let mirrored = stats [ "+50% to Fire Resistance"; "+3% to Maximum Fire Resistance"
                               "Modifiers to Fire Resistance also grant Cold and Lightning Resistance at 50% of their value"
                               "Modifiers to Maximum Fire Resistance also grant Maximum Cold and Lightning Resistance"; "+150% to Cold Resistance" ]
        near (res Lightning plain + 25.0) (res Lightning mirrored)
        near 78.0 (res Cold mirrored))

    test "effect of small passives, of an item's bonuses; per armour on an item; a character's \"more Life\"" (fun () ->
        // Hulking Form (30115): 50% increased effect of Small Passive Skills. "+8 to Strength" (34202)
        // becomes +12; the "+5 to any Attribute" node (10100) stays +5
        let withNodes nodes = stats { blank with TreeNodes = nodes; TreeOverrides = Map.ofList [ 10100, [ "+5 to Strength" ] ] }
        let plain = stats blank
        near (plain.Str + 13.0) (withNodes [ 34202; 10100 ]).Str
        near (plain.Str + 17.0) (withNodes [ 34202; 10100; 30115 ]).Str
        // "+5 to Strength" at 50% more effect is +7: the game's stats are whole numbers
        let ring slot = item slot [ "+5 to Strength"; "+20 to Intelligence" ]
        let left = stats { blank with Items = [ ring "Ring 1"; ring "Ring 2" ]; ExtraLines = [ "50% increased bonuses gained from left Equipped Ring" ] }
        near (plain.Str + 7.0 + 5.0) left.Str
        near (plain.Int + 30.0 + 20.0) left.Int
        // a quiver is the item in the off hand with Quiver in its text
        let quiver = "Weapon 2", "Rarity: Rare\nTest Quiver\nBroadhead Quiver\n--------\n+10 to Dexterity"
        near (plain.Dex + 13.0) (stats { blank with Items = [ quiver ]; ExtraLines = [ "30% increased bonuses gained from Equipped Quiver" ] }).Dex
        // "+1 to maximum Life per 8 Item Armour on Equipped Helmet": 100 armour -> +12
        let helmet lines = item "Helmet" ([ "Armour: 100"; "--------" ] @ lines)
        let bare = stats { blank with Items = [ helmet [] ] }
        near (bare.Life + 12.0) (stats { blank with Items = [ helmet [ "+1 to maximum Life per 8 Armour on Equipped Helmet" ] ] }).Life
        // "15% more Maximum Life" is the Titan's stat here, not a monster rarity's
        near (plain.Life * 1.15) (stats { blank with ExtraLines = [ "15% more Maximum Life" ] }).Life)

    test "The Adorned, Grand Spectrum resistances, doubled evasion from helmet, gloves and boots" (fun () ->
        // a corrupted magic jewel counts for 100% more beside The Adorned; a rare or uncorrupted one does not
        let jewel n rarity corrupted = sprintf "Jewel %d" n, String.concat "\n" ([ "Rarity: " + rarity; "Test Ruby"; "--------" ] @ (if corrupted then [ "Corrupted" ] else []) @ [ "--------"; "+10 to Strength" ])
        let adorned = "Jewel 3", "Rarity: Unique\nThe Adorned\nDiamond\n--------\n100% increased Effect of Jewel Socket Passive Skills\ncontaining Corrupted Magic Jewels"
        let plain = stats blank
        near (plain.Str + 30.0) (stats { blank with Items = [ jewel 0 "Magic" true; jewel 1 "Rare" true; jewel 2 "Magic" false ] }).Str
        near (plain.Str + 40.0) (stats { blank with Items = [ jewel 0 "Magic" true; jewel 1 "Rare" true; jewel 2 "Magic" false; adorned ] }).Str
        // "+7% to all Elemental Resistances per socketed Grand Spectrum": two of them give 2 x 2 x 7
        let spectrum n = sprintf "Jewel %d" n, "Rarity: Unique\nGrand Spectrum\nSapphire\n--------\n+7% to all Elemental Resistances per socketed Grand Spectrum"
        let b = { blank with Items = [ spectrum 0; spectrum 1 ] }
        let c = Setup.loadWith db.Value treeJson b Options.defaults
        near (resist Cold plain + 28.0) (resist Cold c.Vitals)
        // evasion from helmet doubled, from body armour halved
        let helmet = item "Helmet" [ "Evasion Rating: 100" ]
        let body = item "Body Armour" [ "Evasion Rating: 100" ]
        let lines = [ "Evasion Rating from Equipped Helmet, Gloves and Boots is doubled"; "Evasion Rating from Equipped Body Armour is halved" ]
        let before = stats { blank with Items = [ helmet; body ] }
        let after = stats { blank with Items = [ helmet; body ]; ExtraLines = lines }
        near 50.0 (after.Evasion - before.Evasion))

    test "spirit: less Spirit, per empty charm slot, per life, at 200 of an attribute, per Spirit" (fun () ->
        let quest = [ "+100 to Spirit" ]
        let total b = (Setup.loadWith db.Value treeJson b Options.defaults).Spirit
        near 100.0 (total { blank with ExtraLines = quest })
        // Lord of the Wilds (61942): 50% less Spirit
        near 50.0 (total { blank with ExtraLines = quest; TreeNodes = [ 61942 ] })
        // a belt with 2 charm slots and the quest's third, one charm worn: 2 empty x 40
        let belt = item "Belt" [ "Has 2 Charm Slots" ]
        let charm = "Charm 1", "Rarity: Magic\nTest Charm\n--------\nFlask: Lasts 3 Seconds\nUsed when you become Frozen"
        let slots = quest @ [ "+1 Charm Slot"; "+40 to Spirit for each of your empty Charm slots" ]
        near 180.0 (total { blank with ExtraLines = slots; Items = [ belt; charm ] })
        // "+25 to Spirit while you have at least 200 Dexterity"
        let dex n = quest @ [ sprintf "+%d to Dexterity" n; "+25 to Spirit while you have at least 200 Dexterity" ]
        near 100.0 (total { blank with ExtraLines = dex 100 })
        near 125.0 (total { blank with ExtraLines = dex 200 })
        // "20% increased Spirit" on a body armour is global; "+2 to Armour per 1 Spirit"
        let body = item "Body Armour" [ "Armour: 100"; "--------"; "20% increased Spirit" ]
        near 120.0 (total { blank with ExtraLines = quest; Items = [ body ] })
        let armour b = (Setup.loadWith db.Value treeJson b Options.defaults).Vitals.Armour
        near 200.0 (armour { blank with ExtraLines = quest @ [ "+2 to Armour per 1 Spirit" ] } - armour { blank with ExtraLines = quest }))

    test "defences by slot: the shield's and the body armour's own; evasion gained as armour at base" (fun () ->
        let shield = "Weapon 2", "Rarity: Rare\nTest Shield\n--------\nArmour: 200"
        let body = item "Body Armour" [ "Armour: 300" ]
        let b = { blank with Items = [ shield; body ] }
        let plain = stats b
        near 500.0 plain.Armour
        // "100% increased Armour, Evasion and Energy Shield from Equipped Shield": the shield's 200 only
        near 700.0 (stats { b with ExtraLines = [ "100% increased Armour, Evasion and Energy Shield from Equipped Shield" ] }).Armour
        // "50% more Armour from Equipped Body Armour": the body armour's 300 only
        near 650.0 (stats { b with ExtraLines = [ "50% more Armour from Equipped Body Armour" ] }).Armour
        // "Gain 30% of Evasion Rating as extra Armour": 30% of the helmet's 1000 and the 7 everyone has,
        // increased by armour's 100%, not by evasion's 50%
        let helmet = item "Helmet" [ "Evasion Rating: 1000" ]
        let gain = stats { blank with Items = [ helmet ]; ExtraLines = [ "Gain 30% of Evasion Rating as extra Armour"; "100% increased Armour"; "50% increased Evasion Rating" ] }
        near (1007.0 * 0.3 * 2.0) gain.Armour
        near (1007.0 * 1.5) gain.Evasion)

    test "Smith of Kitava: what the body armour grants, armour per connected notable" (fun () ->
        // Smith's Masterwork (9988) with Kitavan Engraving (60913: "Body Armour grants 15% increased
        // maximum Life") and Tantalum Alloy (61039: "Body Armour grants +75% to Fire Resistance")
        let plain = stats blank
        let smith = stats { blank with TreeNodes = [ 9988; 60913; 61039 ] }
        near (plain.Life * 1.15) smith.Life
        // +200 armour for each of the two connected notables
        near (plain.Armour + 400.0) smith.Armour
        let fire (b: PobBuild) = Defence.uncappedResist Ctx.empty (ModDb.ofList (sheetOf b).Mods) Fire
        near (fire blank + 75.0) (fire { blank with TreeNodes = [ 9988; 60913; 61039 ] }))

    test "Poisonburst Arrow's burst only poisons; Flames of Chayula come as a periodic buff" (fun () ->
        let bow = (Items.parse db.Value "Rarity: Rare\nTest Bow\nWarmonger Bow\n--------\n").Weapon
        match Gems.skill db.Value ModDb.empty bow { Gem = "Poisonburst Arrow"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            let burst = s.Parts |> List.find (fun p -> p.Name.EndsWith "Poison Burst")
            let pdb = ModDb.ofList burst.Mods
            equal (box true) (box (Mods.flag Ctx.empty (Named "NoHitDamage") pdb))
            near 100.0 (Mods.sumBase Ctx.empty [ AilmentChance Poison ] pdb)
            // "Poison duration is 3 seconds"
            near 3.0 (Ailments.duration Ctx.empty pdb Poison)
            // the arrow itself is an ordinary hit
            equal (box false) (box (Mods.flag Ctx.empty (Named "NoHitDamage") (ModDb.ofList s.Mods)))
        | Error e -> failwith e
        let gem name = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = false; StatSet = 1 }
        let b = { blank with Groups = [ { Label = ""; Slot = ""; Enabled = true; Gems = [ gem "Into the Breach" ] } ] }
        let flames only = Behaviours.periodics db.Value (sheetOf { b with ExtraLines = only }) b 5.0 |> List.tryPick (fun p -> match p.Action with GainTimedBuff (n, chance, d, cap, mods) -> Some(p.Every, chance, d, cap, mods) | _ -> None)
        match flames [] with
        | Some (every, chance, d, cap, mods) ->
            // level 20: a flame every 1.21 s, one in three purple, 8 s each, ten at most
            near 1.21 every
            near (1.0 / 3.0) chance
            near 8.0 d
            equal (box 10) (box cap)
            // 7% of damage as extra chaos, more with the gem's own "increased effect" at level 20
            equal (box true) (box (mods |> List.exists (fun m -> m.Stat = GainAs(Physical, Chaos) && m.Value >= 7.0)))
        | None -> failwith "no flames")

    test "channelled skills cost mana per second; a barrage fires at one target" (fun () ->
        match Gems.skill db.Value ModDb.empty None { Gem = "Flameblast"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            // level 20: 3450 mana per minute, nothing up front
            near (3450.0 / 60.0) s.CostPerSecond
            near 0.0 s.Cost
        | Error e -> failwith e
        match Gems.skill db.Value ModDb.empty None { Gem = "Spark"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) -> near 0.0 s.CostPerSecond
        | Error e -> failwith e)

    test "low life only where the build keeps itself there; fewer enemies to be surrounded" (fun () ->
        let bonus = [ "30% increased Damage when on Low Life" ]
        let start b = (Setup.loadWith db.Value treeJson b Options.defaults).Vitals.StartLife
        let shield = item "Helmet" [ "Energy Shield: 100" ]
        // nothing recovers life and the build has a low-life bonus: starts there
        near 0.35 (start { blank with ExtraLines = bonus })
        // no bonus, or some recovery: full life
        near 1.0 (start blank)
        near 1.0 (start { blank with ExtraLines = bonus @ [ "5 Life Regeneration per second" ] })
        // "Your Life cannot change while you have Energy Shield" keeps it there despite regeneration
        near 0.35 (start { blank with Items = [ shield ]; ExtraLines = bonus @ [ "5 Life Regeneration per second"; "Your Life cannot change while you have Energy Shield" ] })
        near 0.35 (Setup.loadWith db.Value treeJson blank { Options.defaults with LowLife = true }).Vitals.StartLife
        equal (box 1) (box (Setup.loadWith db.Value treeJson { blank with ExtraLines = [ "Require 4 fewer enemies to be Surrounded" ] } Options.defaults).Vitals.SurroundedAt))

    test "the sheet of the second weapon set counts its weapons, not the first set's" (fun () ->
        let b = { blank with Items = [ item "Weapon 1" [ "+30 to Strength" ]; item "Weapon 1 Swap" [ "+10 to Strength" ]
                                       "Weapon 2", "Rarity: Rare\nTest Shield\n--------\nArmour: 200"
                                       "Weapon 2 Swap", "Rarity: Rare\nOther Shield\n--------\nArmour: 50" ] }
        let one = stats b
        let two = Defence.computeWith (Map.ofList [ "WeaponSetTwo", 1 ]) bases (sheetOf b)
        near 20.0 (one.Str - two.Str)
        near 4.0 (one.Armour / two.Armour))

    test "spell damage per 100 maximum mana counts the hundreds of mana" (fun () ->
        let ms = match StatMap.meaning "spell_damage_+%_per_100_maximum_mana" 3.0 with Mods ms -> ms | m -> failwithf "%A" m
        let db = ModDb.ofList ms
        let spell = { Ctx.empty with SkillTags = Set.ofList [ Spell ]; Buffs = Map.ofList [ "MaxMana100", 12 ] }
        // 1234 mana: 12 hundreds x 3%
        near 1.36 (Mods.inc spell [ Damage AllDamage ] db)
        near 1.0 (Mods.inc { spell with SkillTags = Set.ofList [ Attack ] } [ Damage AllDamage ] db))

    test "spirit: flat amounts, a sceptre's own value, the better weapon set; minions from what is left" (fun () ->
        let sceptre slot = slot, "Rarity: Rare\nTest Sceptre\n--------\nSpirit: 120\n--------\n30% increased Spirit\n"
        let helmet = item "Helmet" [ "Armour: 100"; "--------"; "+50 to Spirit" ]
        let quest = [ "+100 to Spirit" ]
        // the sceptre's own "30% increased Spirit" is already in the 120 it shows
        near 270.0 (let b = { blank with Items = [ sceptre "Weapon 2"; helmet ]; ExtraLines = quest } in Behaviours.spiritTotal (sheetOf b) b)
        // a sceptre in weapon set two counts when it gives more than set one's
        near 270.0 (let b = { blank with Items = [ sceptre "Weapon 2 Swap"; helmet ]; ExtraLines = quest } in Behaviours.spiritTotal (sheetOf b) b)
        near 150.0 (let b = { blank with Items = [ helmet ]; ExtraLines = quest } in Behaviours.spiritTotal (sheetOf b) b)
        // Skeletal Warriors: 20 Spirit each at level 20, the first two free; Herald of Thunder 30
        let gem name = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = false; StatSet = 1 }
        let group name = { Label = ""; Slot = ""; Enabled = true; Gems = [ gem name ] }
        let b = { blank with Items = [ helmet ]; ExtraLines = quest; Groups = [ group "Skeletal Warrior"; group "Herald of Thunder" ] }
        let uses = Behaviours.spiritUses db.Value (sheetOf b) b
        let counts = Behaviours.minionCounts 150.0 uses
        // 150 - 30 = 120 spare: 6 paid minions and 2 free
        equal (box 8) (box counts.["Skeletal Warrior"]))

    test "spirit reservation: granted skills are free, quality and tree efficiency, corrupted items" (fun () ->
        let gem name q = { Name = name; Level = 20; Quality = q; Enabled = true; IsSupport = false; StatSet = 1 }
        let group slot name q = { Label = ""; Slot = slot; Enabled = true; Gems = [ gem name q ] }
        let cost (b: PobBuild) name = Behaviours.spiritUses db.Value (sheetOf b) b |> List.filter (fun u -> u.Gem = name) |> List.sumBy (fun u -> u.Flat)
        // Cast on Critical: 100 Spirit; 0.5% reservation efficiency per quality
        near 100.0 (cost { blank with Groups = [ group "" "Cast on Critical" 0 ] } "Cast on Critical")
        near (100.0 / 1.1) (cost { blank with Groups = [ group "" "Cast on Critical" 20 ] } "Cast on Critical")
        // a skill an item grants reserves nothing
        near 0.0 (cost { blank with Groups = [ group "granted" "Cast on Critical" 0 ] } "Cast on Critical")
        // "Meta Skills have 20% increased Reservation Efficiency" adds to the quality's 10%; a herald is not meta
        let tree = { blank with ExtraLines = [ "Meta Skills have 20% increased Reservation Efficiency"; "8% increased Reservation Efficiency of Herald Skills" ]
                                Groups = [ group "" "Cast on Critical" 20; group "" "Herald of Thunder" 0 ] }
        near (100.0 / 1.3) (cost tree "Cast on Critical")
        near (30.0 / 1.08) (cost tree "Herald of Thunder")
        // "Persistent Buffs have 50% less Reservation"
        near 15.0 (cost { blank with ExtraLines = [ "Persistent Buffs have 50% less Reservation" ]; Groups = [ group "" "Herald of Thunder" 0 ] } "Herald of Thunder")
        // supports: Clarity II adds 20 Spirit to a persistent buff; Heightened Curse's 140% is for
        // the curse socketed in a meta gem, not for the meta gem
        let sup name = { Name = name; Level = 1; Quality = 0; Enabled = true; IsSupport = true; StatSet = 1 }
        let withGems gems = { blank with Groups = [ { Label = ""; Slot = ""; Enabled = true; Gems = gems } ] }
        near 50.0 (cost (withGems [ gem "Herald of Thunder" 0; sup "Clarity II" ]) "Herald of Thunder")
        near 100.0 (cost (withGems [ gem "Cast on Critical" 0; gem "Enfeeble" 0; sup "Heightened Curse" ]) "Cast on Critical")
        // Dialla's Desire: 90% reservation
        near 27.0 (cost (withGems [ gem "Herald of Thunder" 0; sup "Dialla's Desire" ]) "Herald of Thunder")
        // "1% increased Spirit for each Corrupted Item Equipped"
        let corrupt slot = slot, "Rarity: Rare\nTest Helm\n--------\nArmour: 100\n--------\n+100 to Spirit\n1% increased Spirit for each Corrupted Item Equipped\n--------\nCorrupted\n"
        let b = { blank with Items = [ corrupt "Helmet" ] }
        near 101.0 (Behaviours.spiritTotal (sheetOf b) b))

    test "api: gem quality only when shown, weapon set passives" (fun () ->
        let json =
            """{"character":{"level":5,"class":"Witch","equipment":[],"jewels":[],
               "skills":[{"typeLine":"Spark","properties":[{"name":"Level","values":[["5",0]]}]},
                         {"typeLine":"Comet","properties":[{"name":"Level","values":[["7",0]]},{"name":"[Quality]","values":[["+12%",1]]}]}],
               "passives":{"hashes":[1,2],"specialisations":{"set1":[3],"set2":[4,5]}}}}"""
        let b, _ = Api.read json
        equal (box [ 0; 12 ]) (box (b.Groups |> List.map (fun g -> g.Gems.Head.Quality)))
        equal (box ([ 1; 2 ], [ 3 ], [ 4; 5 ])) (box (b.TreeNodes, b.TreeNodesSet1, b.TreeNodesSet2)))

    test "gem: a crossbow gem is its shot, loaded from its ammunition" (fun () ->
        let crossbow = (Items.parse db.Value "Rarity: Normal\nAlloy Crossbow\n--------\n").Weapon
        match Gems.skill db.Value ModDb.empty crossbow { Gem = "Glacial Bolt"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            equal (box "Glacial Bolt") (box s.Name)
            equal (box 1) (box s.Clip)
            // Alloy Crossbow: reload 0.7 s; the load costs 49 mana at level 20; the shot deals 145% of weapon damage
            near 0.7 s.ReloadTime
            near 49.0 s.Cost
            near 1.45 s.BaseMultiplier
            equal (box true) (box (s.Tags.Contains Projectile))
        | Error e -> failwith e
        match Gems.skill db.Value ModDb.empty crossbow { Gem = "Rapid Shot"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) -> equal (box 100) (box s.Clip)
        | Error e -> failwith e)

    test "gem: a staged channel is one use to full stages" (fun () ->
        match Gems.skill db.Value ModDb.empty None { Gem = "Flameblast"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            // 10 stages of 0.25 s and a 0.6 s release; 75% more damage and 6 units of radius per stage
            near (10.0 * 0.25 + 0.6) s.BaseTime
            near 8.5 (Mods.mult { Ctx.empty with SkillTags = s.Tags } [ Damage AllDamage ] (ModDb.ofList s.Mods))
            equal (box (AreaAroundSelf 60.0)) (box s.Delivery)
        | Error e -> failwith e)

    test "gem: shield attacks hit with the shield, scaled by its armour" (fun () ->
        let mods armour = ModDb.ofList [ Mod.base' (Named "ShieldArmour") armour ]
        let build armour =
            match Gems.skill db.Value (mods armour) None { Gem = "Resonating Shield"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
            | Ok (s, _) -> s
            | Error e -> failwith e
        // level 20: 103-155 physical, +5 to 7 per 15 armour on the shield; 0.8 s attack time
        let bare, armoured = build 0.0, build 300.0
        near 103.0 bare.BaseDamage.[Physical].Min
        near 155.0 bare.BaseDamage.[Physical].Max
        near (103.0 + 5.0 * 20.0) armoured.BaseDamage.[Physical].Min
        near (155.0 + 7.0 * 20.0) armoured.BaseDamage.[Physical].Max
        near 0.8 bare.BaseTime)

    test "gem: a channelled skill with a cooldown is held for its minimum channel time" (fun () ->
        let mace = (Items.parse db.Value "Rarity: Normal\nPlated Mace\n--------\n").Weapon
        match Gems.skill db.Value ModDb.empty mace { Gem = "Flame Breath"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            // 3 s minimum channel, hitting once per attack time throughout
            near 3.0 s.BaseTime
            if s.Hits.Length < 5 then failwithf "expected a hit per attack time, got %d" s.Hits.Length
            near 1.0 (fst (List.last s.Hits))
        | Error e -> failwith e)

    test "Blasphemy: socketed curses become auras at the gem's reduced effect" (fun () ->
        let gem name support = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = support; StatSet = 1 }
        let group = { Label = ""; Slot = ""; Enabled = true; Gems = [ gem "Blasphemy" false; gem "Despair" false ] }
        let b = { blank with Groups = [ group ] }
        let sheet = sheetOf b
        match Behaviours.curseAuras db.Value sheet b with
        | [ name, _, [ m ] ], _ ->
            equal (box "Curse:Despair") (box name)
            equal (box (Resist Chaos)) (box m.Stat)
            // Despair alone, for comparison: the aura has 41% less effect at level 20
            match Gems.skill db.Value ModDb.empty None { Gem = "Despair"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
            | Ok (s, _) ->
                match Behaviours.curseOf db.Value ModDb.empty { Gem = "Despair"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } s with
                | Some (_, _, [ full ], _) -> near (full.Value * 0.59) m.Value
                | other -> failwithf "unexpected curse %A" other
            | Error e -> failwith e
        | other -> failwithf "unexpected auras %A" other
        // and Blasphemy is not a trigger gem
        equal (box 0) (box (Behaviours.metas db.Value sheet b false).Length))
#else
let run (_: string -> (unit -> unit) -> unit) (_: float -> float -> unit) (_: obj -> obj -> unit) = ()
#endif
