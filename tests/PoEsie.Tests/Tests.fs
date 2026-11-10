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
        { Enemy = boss life
          Player = Actor.make "Player" [ noIgnite ] [ skill ]
          Minions = []
          MaxTime = 60.0 }

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

    DataTests.run test near (fun a b -> equal a b)

    printfn "%d passed, %d failed" passed failures
    if failures = 0 then 0 else 1
