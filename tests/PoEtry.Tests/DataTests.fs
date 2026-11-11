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
        // Plated Mace: 18-38 physical, 1.4 APS, 5% crit
        near ((18.0 + 10.0) * 1.6) w.Damage.[Physical].Min
        near ((38.0 + 20.0) * 1.6) w.Damage.[Physical].Max
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

    test "Blasphemy: socketed curses become auras at the gem's reduced effect" (fun () ->
        let gem name support = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = support; StatSet = 1 }
        let group = { Label = ""; Slot = ""; Enabled = true; Gems = [ gem "Blasphemy" false; gem "Despair" false ] }
        let b = { blank with Groups = [ group ] }
        let sheet = sheetOf b
        match Behaviours.curseAuras db.Value sheet b with
        | [ name, [ m ] ], _ ->
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
