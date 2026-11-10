/// Dependency-free test runner, so the suite also runs under Fable.
module PoEsie.Tests

open PoEsie

let mutable private failures = 0
let mutable private passed = 0

let private test name (f: unit -> unit) =
    try
        f ()
        passed <- passed + 1
    with e ->
        failures <- failures + 1
        printfn "FAIL %s\n     %s" name e.Message

let private near (expected: float) (actual: float) =
    if abs (expected - actual) > 1e-6 * max 1.0 (abs expected) then
        failwithf "expected %g, got %g" expected actual

let private equal expected actual =
    if expected <> actual then failwithf "expected %A, got %A" expected actual

let ctx = Ctx.empty
let fireSpell dmg =
    { Skill.make "Bolt" [ Spell ] with
        BaseDamage = Map.ofList [ Fire, Range.fixedAt dmg ]
        BaseCrit = 0.0 }
let enemy mods = { Level = 83; Mods = ModDb.ofList mods }
let noIgnite = Mod.more (AilmentChance Ignite) -100.0

[<EntryPoint>]
let main _ =
    test "increased adds, more multiplies" (fun () ->
        let mods = [ Mod.inc (Damage AllDamage) 50.0; Mod.inc (Damage AllDamage) 50.0; Mod.more (Damage AllDamage) 20.0; Mod.more (Damage AllDamage) -50.0 ]
        near (2.0 * 1.2 * 0.5) (Mods.mult ctx [ Damage AllDamage ] (ModDb.ofList mods)))

    test "per-stack mods scale with stacks" (fun () ->
        let m = Mod.inc (Damage AllDamage) 4.0 |> Mod.whenever (PerStack("Frenzy", 1.0))
        near 1.12 (Mods.mult { ctx with Buffs = Map.ofList [ "Frenzy", 3 ] } [ Damage AllDamage ] (ModDb.ofList [ m ])))

    test "skill conversion first, global conversion on the rest" (fun () ->
        let mods = [ Mod.base' (SkillConvert(Physical, Fire)) 60.0; Mod.base' (Convert(Physical, Cold)) 50.0 ]
        let r = Hit.convert ctx (ModDb.ofList mods) (Map.ofList [ Physical, Range.fixedAt 100.0 ])
        near 20.0 r.[Physical].Max
        near 20.0 r.[Cold].Max
        near 60.0 r.[Fire].Max)

    test "conversion over 100% is scaled down" (fun () ->
        let mods = [ Mod.base' (Convert(Physical, Fire)) 80.0; Mod.base' (Convert(Physical, Cold)) 40.0 ]
        let r = Hit.convert ctx (ModDb.ofList mods) (Map.ofList [ Physical, Range.fixedAt 120.0 ])
        equal false (r.ContainsKey Physical)
        near 80.0 r.[Fire].Max
        near 40.0 r.[Cold].Max)

    test "conversion never goes backwards" (fun () ->
        let r = Hit.convert ctx (ModDb.ofList [ Mod.base' (Convert(Fire, Physical)) 100.0 ]) (Map.ofList [ Fire, Range.fixedAt 10.0 ])
        near 10.0 r.[Fire].Max)

    test "converted damage only uses the final type's modifiers" (fun () ->
        let skill = { Skill.make "Strike" [ Attack ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let mods = [ Mod.base' (Convert(Physical, Fire)) 100.0; Mod.inc (Damage(OfType Physical)) 100.0; Mod.inc (Damage(OfType Fire)) 50.0 ]
        let d = Hit.rollUnmitigated (Rng 1u) ctx (ModDb.ofList mods) skill false
        near 150.0 (Dmg.get Fire d)
        near 0.0 (Dmg.get Physical d))

    test "gain as extra uses post-conversion damage" (fun () ->
        let mods = [ Mod.base' (Convert(Physical, Fire)) 50.0; Mod.base' (GainAs(Fire, Chaos)) 20.0 ]
        let r = Hit.ranges ctx (ModDb.ofList mods) { Skill.make "x" [] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ] }
        near 50.0 r.[Physical].Max
        near 50.0 r.[Fire].Max
        near 10.0 r.[Chaos].Max)

    test "added damage effectiveness and base multiplier" (fun () ->
        let skill = { fireSpell 10.0 with AddedDamageEffectiveness = 0.5; BaseMultiplier = 2.0 }
        let r = Hit.ranges ctx (ModDb.ofList (Mod.added Fire 20.0 40.0)) skill
        near 40.0 r.[Fire].Min
        near 60.0 r.[Fire].Max)

    test "crit multiplier: base +100% bonus plus added bonus, then increased" (fun () ->
        near 2.5 (Hit.critMultiplier ctx (ModDb.ofList [ Mod.base' CritBonus 50.0 ]))
        near 4.0 (Hit.critMultiplier ctx (ModDb.ofList [ Mod.base' CritBonus 50.0; Mod.inc CritBonus 100.0 ])))

    test "penetration cannot push resistance below zero" (fun () ->
        near 0.0 (Hit.effectiveResist ctx (ModDb.ofList [ Mod.base' (Penetration Fire) 40.0 ]) (enemy [ Mod.base' (Resist Fire) 30.0 ]) Fire))

    test "exposure lowers resistance before penetration" (fun () ->
        let def = enemy [ Mod.base' (Resist Fire) 50.0; Mod.base' (Resist Fire) -20.0 ]
        near 20.0 (Hit.effectiveResist ctx (ModDb.ofList [ Mod.base' (Penetration Fire) 10.0 ]) def Fire))

    test "resistance is capped at the enemy's maximum" (fun () ->
        near 75.0 (Hit.effectiveResist ctx ModDb.empty (enemy [ Mod.base' (Resist Cold) 120.0 ]) Cold))

    test "negative resistance increases damage and ignores penetration" (fun () ->
        let dealt = Hit.mitigate ctx (ModDb.ofList [ Mod.base' (Penetration Fire) 50.0 ]) (enemy [ Mod.base' (Resist Fire) -25.0 ]) (Map.ofList [ Fire, 100.0 ])
        near 125.0 dealt.[Fire])

    test "armour reduces physical by A / (A + 10 D)" (fun () ->
        near 50.0 (Hit.physicalReduction (enemy [ Mod.base' Armour 1000.0 ]) 100.0)
        near 75.0 (Hit.physicalReduction (enemy [ Mod.base' Armour 1e9 ]) 100.0))

    test "shock-style damage taken multiplies the hit" (fun () ->
        let dealt = Hit.mitigate ctx ModDb.empty (enemy [ Mod.inc (DamageTaken AllDamage) 20.0 ]) (Map.ofList [ Cold, 100.0 ])
        near 120.0 dealt.[Cold])

    test "ignite: 20% of the fire hit per second for 4 seconds" (fun () ->
        let hit = { Crit = false; Unmitigated = Map.ofList [ Fire, 100.0 ]; Dealt = Map.empty }
        let inflicts = Ailments.fromHit (Rng 1u) ctx (ModDb.ofList [ Mod.base' (AilmentChance Ignite) 100.0 ]) (enemy []) hit
        let ignite = inflicts |> List.pick (function Dot (Ignite, t, dps, dur, n) -> Some(t, dps, dur, n) | _ -> None)
        equal (Fire, 20.0, 4.0, 1) ignite)

    test "ignite chance scales with damage over ailment threshold" (fun () ->
        let def = enemy []
        let threshold = Ailments.threshold def
        let hit = { Crit = false; Unmitigated = Map.ofList [ Fire, threshold ]; Dealt = Map.empty }
        near Constants.igniteChanceMultiplier (Ailments.chance ctx ModDb.empty def hit Ignite))

    let boss life = { EnemyDef.atLevel 83 life with Mods = [] }
    let fight skill life =
        Fight.single (boss life) (Actor.make "Player" [ noIgnite ] [ skill ]) 60.0

    test "sim: fixed hits on a cadence kill at the exact hit" (fun () ->
        let o = Sim.run (fight (fireSpell 100.0) 1000.0) 1u
        equal (Some 10.0) o.Ttk
        equal 10 o.Counts.["hits:Bolt"])

    test "sim: speed shortens the cadence" (fun () ->
        let f = fight (fireSpell 100.0) 1000.0
        let f = { f with Player = { f.Player with Mods = Mod.inc Speed 100.0 :: f.Player.Mods } }
        equal (Some 5.0) (Sim.run f 1u).Ttk)

    test "sim: ignite kills between events at the exact time" (fun () ->
        let skill = { fireSpell 100.0 with BaseTime = 0.5; Cooldown = Some 100.0 }
        let f = { fight skill 150.0 with Player = Actor.make "Player" [ Mod.base' (AilmentChance Ignite) 100.0 ] [ skill ] }
        let o = Sim.run f 1u
        near 3.0 o.Ttk.Value
        near 50.0 o.DamageBy.["Ignite"])

    test "sim: ignites don't stack, the strongest applies" (fun () ->
        let skill = { fireSpell 100.0 with BaseTime = 1.0 }
        let f = { fight skill 1e9 with Player = Actor.make "Player" [ Mod.base' (AilmentChance Ignite) 100.0 ] [ skill ]; MaxTime = 10.0 }
        let o = Sim.run f 1u
        // ignite from t=1 to 10 at 20 dps
        near 180.0 o.DamageBy.["Ignite"])

    test "sim: meta gem triggers when energy is full" (fun () ->
        let bolt = fireSpell 100.0
        let nova = { fireSpell 1000.0 with Name = "Nova"; Tags = Set.ofList [ Spell; Triggered ] }
        let player =
            { Actor.make "Player" [ noIgnite ] [ bolt; nova ] with
                Rotation = [ Actor.always "Bolt" ]
                Rules = [ { Hook = OnHit; Filter = Called "Bolt"; Chance = 100.0; Reaction = GainEnergy("Cast on Hit", 50.0) } ]
                Metas = [ { Name = "Cast on Hit"; MaxEnergy = 100.0; Triggers = "Nova" } ] }
        let o = Sim.run { fight bolt 1e9 with Player = player; MaxTime = 10.5 } 1u
        equal 10 o.Counts.["hits:Bolt"]
        equal 5 o.Counts.["trigger:Nova"])

    test "sim: buffs from rules feed back into damage" (fun () ->
        let bolt = fireSpell 100.0
        let player =
            { Actor.make "Player" [ noIgnite ] [ bolt ] with
                Rules = [ { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("Rage", 10.0, 2, [ Mod.inc (Damage AllDamage) 50.0 ]) } ] }
        let o = Sim.run { fight bolt 1e9 with Player = player; MaxTime = 3.5 } 1u
        // hits: 100, 150, 200
        near 450.0 o.DamageBy.["Bolt (hit)"])

    test "sim: same seed, same fight" (fun () ->
        let skill = { fireSpell 100.0 with BaseDamage = Map.ofList [ Fire, { Min = 50.0; Max = 150.0 } ]; BaseCrit = 30.0 }
        let f = { fight skill 5000.0 with Player = Actor.make "Player" [] [ skill ] }
        equal (Sim.run f 7u) (Sim.run f 7u))

    let dummy name x y = { EnemyDef.atLevel 83 1e9 with Name = name; Position = Vec.make x y; Size = 4.0; Mods = [] }

    test "geometry: area at target hits everything within radius + body size" (fun () ->
        let a, b, c = dummy "a" 30.0 0.0, dummy "b" 40.0 0.0, dummy "c" 60.0 0.0
        equal [ "a"; "b" ] (Geometry.targets (AreaAtTarget 10.0) 1.0 a [ a; b; c ]))

    test "geometry: increased area grows the radius by the square root" (fun () ->
        // 10 * sqrt(4) = 20 radius reaches c at distance 30 - 4 size = 26? no: 20 + 4 = 24 < 30
        let a, c = dummy "a" 30.0 0.0, dummy "c" 55.0 0.0
        equal [ "a" ] (Geometry.targets (AreaAtTarget 10.0) 4.0 a [ a; c ])
        equal [ "a"; "c" ] (Geometry.targets (AreaAtTarget 10.0) 9.0 a [ a; c ]))

    test "geometry: projectiles stop at the first enemy unless they pierce" (fun () ->
        let a, b = dummy "a" 30.0 0.0, dummy "b" 60.0 0.0
        let p = ProjectileShape.make 1
        equal [ "a" ] (Geometry.targets (Projectiles p) 1.0 a [ a; b ])
        equal [ "a"; "b" ] (Geometry.targets (Projectiles { p with Pierce = 1 }) 1.0 a [ a; b ]))

    test "geometry: without shotgun, a use hits each enemy once" (fun () ->
        let a = dummy "a" 30.0 0.0
        let p = { ProjectileShape.make 3 with Spread = 5.0; CanShotgun = false }
        equal [ "a" ] (Geometry.targets (Projectiles p) 1.0 a [ a ])
        equal [ "a"; "a"; "a" ] (Geometry.targets (Projectiles { p with CanShotgun = true }) 1.0 a [ a ]))

    test "geometry: nova from an impact point outside the boss hits with the share of the circle it covers" (fun () ->
        // boss radius 10 at distance 40; impact 20 in front of the player -> 20 from the centre
        // covered angle = 2 asin(10/20) = 60 degrees -> 6 of 36 projectiles (edges counted)
        let boss = { dummy "b" 40.0 0.0 with Size = 10.0 }
        let p = { ProjectileShape.make 36 with Spread = 360.0; FromImpact = true; Range = 200.0 }
        let hits = Geometry.targetsFrom 20.0 (Projectiles p) 1.0 boss [ boss ] |> List.length
        if hits < 5 || hits > 7 then failwithf "expected about 6 hits, got %d" hits
        // impact inside the boss: every projectile hits
        equal 36 (Geometry.targetsFrom 35.0 (Projectiles p) 1.0 boss [ boss ] |> List.length))

    test "geometry: a projectile bounces off a wall once with terrain chain" (fun () ->
        // flying +x from the origin, wall at x = 20, enemy behind the player at x = -20
        let behind = dummy "behind" -20.0 0.0
        let wall = Vec.make 20.0 -50.0, Vec.make 20.0 50.0
        let p chance = { ProjectileShape.make 1 with Range = 100.0; TerrainChain = chance }
        let fly chance = Geometry.trace (Rng 1u) [ wall ] (p chance) Vec.zero (Vec.make 1.0 0.0) [ behind ]
        equal [ "behind" ] (fly 100.0).Hit
        equal ([]: string list) (fly 0.0).Hit
        // a second wall behind: still only one bounce
        let back = Vec.make -40.0 -50.0, Vec.make -40.0 50.0
        let path = Geometry.trace (Rng 1u) [ wall; back ] (p 100.0) Vec.zero (Vec.make 1.0 0.0) []
        equal 3 path.Points.Length)

    test "geometry: chains jump to the nearest unhit enemy in range" (fun () ->
        let a, b, c = dummy "a" 30.0 0.0, dummy "b" 30.0 20.0, dummy "c" 30.0 -60.0
        let p = { ProjectileShape.make 1 with Chains = 2; ChainRange = 30.0 }
        equal [ "a"; "b" ] (Geometry.targets (Projectiles p) 1.0 a [ a; b; c ]))

    test "sim: a pack is cleared, kills are recorded per enemy" (fun () ->
        let nova = { fireSpell 100.0 with Name = "Nova"; Delivery = AreaAroundSelf 50.0 }
        let enemies = [ for i in 1..5 -> { dummy (sprintf "m%d" i) (float (10 * i)) 0.0 with Life = 250.0 } ]
        let f = { Fight.single enemies.Head (Actor.make "Player" [ noIgnite ] [ nova ]) 60.0 with Enemies = enemies }
        let o = Sim.run f 1u
        equal (Some 3.0) o.Ttk
        equal 5 o.Kills.Length)

    test "sim: bosses take half curse effect" (fun () ->
        let bolt = fireSpell 100.0
        let boss = { EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 30.0 0.0) with Life = 1e9; Mods = [ Mod.more (Named "CurseEffectOnSelf") -50.0 ] }
        let player =
            { Actor.make "Player" [ noIgnite ] [ bolt ] with
                Rules = [ { Hook = OnUse; Filter = AnySkill; Chance = 100.0; Reaction = ApplyCurse("Flammability", 10.0, [ Mod.base' (Resist Fire) -40.0 ]) } ] }
        let o = Sim.run (Fight.single boss player 1.5) 1u
        near 120.0 o.DamageBy.["Bolt (hit)"])

    test "sim: a cycle switches its buff every period" (fun () ->
        let bolt = fireSpell 100.0
        let cycle = { Name = "Flux"; Period = 2.0; Variants = [ [ Mod.more (Damage(OfType Fire)) 100.0 ]; [ Mod.more (Damage(OfType Cold)) 100.0 ] ] }
        let player = { Actor.make "Player" [ noIgnite ] [ bolt ] with Cycles = [ cycle ] }
        let o = Sim.run (Fight.single (boss 1e9) player 9.5) 3u
        // picks at t = 0, 2, 4, 6, 8; hits at t = 1..9, each doubled while the fire variant is active
        let picks = o.Counts |> Map.filter (fun k _ -> k.StartsWith "cycle:") |> Map.toList |> List.sumBy snd
        equal 5 picks
        let dmg = o.DamageBy.["Bolt (hit)"]
        if dmg < 900.0 || dmg > 1800.0 || dmg % 100.0 <> 0.0 then failwithf "unexpected damage %g" dmg)

    DataTests.run test near (fun a b -> equal a b)

    printfn "%d passed, %d failed" passed failures
    if failures = 0 then 0 else 1
