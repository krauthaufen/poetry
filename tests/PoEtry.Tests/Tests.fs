/// Dependency-free test runner, so the suite also runs under Fable.
module PoEtry.Tests

open PoEtry

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

    test "the random numbers are the same stream on every platform, and uniform" (fun () ->
        // xoshiro128** from seed 1: the same four numbers on .NET and in JavaScript
        let r = Rng 1u
        equal [ 3028752090u; 1821319296u; 1022962477u; 864284253u ] [ for _ in 1 .. 4 -> r.NextUInt() ]
        let r = Rng 7u
        let xs = [ for _ in 1 .. 20000 -> r.Float() ]
        equal true (xs |> List.forall (fun x -> x >= 0.0 && x < 1.0))
        near 0.5 (List.average xs |> fun m -> System.Math.Round(m, 1)))

    test "inevitable critical hits: every hit crits, with 30% less bonus per re-roll" (fun () ->
        // 50% crit chance, +100% bonus on top of the base: a first-roll crit deals x3
        let skill = { fireSpell 100.0 with BaseCrit = 50.0 }
        let mods = ModDb.ofList [ Mod.flag (Named "InevitableCrits"); Mod.base' CritBonus 100.0 ]
        let rng = Rng 7u
        let hits = [ for _ in 1 .. 4000 -> Hit.roll rng ctx mods skill (enemy []) ]
        equal true (hits |> List.forall (fun h -> h.Crit))
        // expected bonus share: 0.5 x 1 + 0.25 x 0.7 + 0.125 x 0.4 + 0.0625 x 0.1 (then nothing) = 0.73125
        let mean = hits |> List.averageBy (fun h -> h.Unmitigated |> Map.toList |> List.sumBy snd)
        let expected = 100.0 * (1.0 + 2.0 * 0.73125)
        if abs (mean - expected) > 0.03 * expected then failwithf "mean %.1f, expected %.1f" mean expected)

    test "hits ignore the elemental resistances of frozen enemies, where they are positive" (fun () ->
        let mods = ModDb.ofList [ Mod.flag (Named "IgnoreElementalResist") |> Mod.whenever (EnemyHas "Frozen") ]
        let dmg = Map.ofList [ Cold, 100.0 ]
        let frozen = { ctx with EnemyStatus = Map.ofList [ "Frozen", 1 ] }
        near 60.0 (Hit.mitigate ctx mods (enemy [ Mod.base' (Resist Cold) 40.0 ]) dmg).[Cold]
        near 100.0 (Hit.mitigate frozen mods (enemy [ Mod.base' (Resist Cold) 40.0 ]) dmg).[Cold]
        // a negative resistance stays
        near 120.0 (Hit.mitigate frozen mods (enemy [ Mod.base' (Resist Cold) -20.0 ]) dmg).[Cold])

    test "bifurcating critical hits: two rolls, the bonus twice when both succeed" (fun () ->
        // 50% chance, bonus 200%: 25% no crit (100), 50% one crit (300), 25% both (500)
        let skill = { fireSpell 100.0 with BaseCrit = 50.0 }
        let mods = ModDb.ofList [ Mod.flag (Named "BifurcateCrits"); Mod.base' CritBonus 100.0 ]
        let rng = Rng 3u
        let hits = [ for _ in 1 .. 6000 -> Hit.roll rng ctx mods skill (enemy []) ]
        let share = float (hits |> List.filter (fun h -> h.Crit) |> List.length) / 6000.0
        if abs (share - 0.75) > 0.02 then failwithf "crit share %.3f" share
        let mean = hits |> List.averageBy (fun h -> h.Unmitigated |> Map.toList |> List.sumBy snd)
        if abs (mean - 300.0) > 9.0 then failwithf "mean %.1f" mean)

    test "conversion over 100% is scaled down" (fun () ->
        let mods = [ Mod.base' (Convert(Physical, Fire)) 80.0; Mod.base' (Convert(Physical, Cold)) 40.0 ]
        let r = Hit.convert ctx (ModDb.ofList mods) (Map.ofList [ Physical, Range.fixedAt 120.0 ])
        equal false (r.ContainsKey Physical)
        near 80.0 r.[Fire].Max
        near 40.0 r.[Cold].Max)

    test "an evaded critical hit is an ordinary hit; cold builds no stun" (fun () ->
        // a roll of 1 or more stands for the second evasion test having been passed
        let sk = { fireSpell 100.0 with BaseCrit = 100.0 }
        let plain = Hit.rollWith (Some 1.0) (Rng 1u) ctx ModDb.empty sk (enemy [])
        let crit = Hit.rollWith (Some 0.0) (Rng 1u) ctx ModDb.empty sk (enemy [])
        equal (false, true) (plain.Crit, crit.Crit)
        equal false (List.contains Cold (Ailments.scalesFrom HeavyStun)))

    test "conversion goes in any direction" (fun () ->
        // Blueflame Bracers: fire to cold
        let r = Hit.convert ctx (ModDb.ofList [ Mod.base' (Convert(Fire, Cold)) 100.0 ]) (Map.ofList [ Fire, Range.fixedAt 10.0 ])
        near 10.0 r.[Cold].Max
        equal false (r.ContainsKey Fire))

    test "conversion is not recursive within a step, but the second step converts what the first made" (fun () ->
        // physical to lightning and lightning to cold from items: the lightning stays lightning
        let items = [ Mod.base' (Convert(Physical, Lightning)) 100.0; Mod.base' (Convert(Lightning, Cold)) 100.0 ]
        let r = Hit.convert ctx (ModDb.ofList items) (Map.ofList [ Physical, Range.fixedAt 100.0 ])
        near 100.0 r.[Lightning].Max
        // wiki: the skill converts 60% to lightning, Call of the Brotherhood lightning to cold: 40 physical, 60 cold
        let mods = [ Mod.base' (SkillConvert(Physical, Lightning)) 60.0; Mod.base' (Convert(Lightning, Cold)) 100.0 ]
        let r = Hit.convert ctx (ModDb.ofList mods) (Map.ofList [ Physical, Range.fixedAt 100.0 ])
        near 40.0 r.[Physical].Max
        near 60.0 r.[Cold].Max
        // wiki: the skill converts 80% to fire, Avatar of Fire 75% of the rest: 95 fire
        let mods = [ Mod.base' (SkillConvert(Physical, Fire)) 80.0; Mod.base' (Convert(Physical, Fire)) 75.0 ]
        let r = Hit.convert ctx (ModDb.ofList mods) (Map.ofList [ Physical, Range.fixedAt 100.0 ])
        near 95.0 r.[Fire].Max)

    test "converted damage only uses the final type's modifiers" (fun () ->
        let skill = { Skill.make "Strike" [ Attack ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let mods = [ Mod.base' (Convert(Physical, Fire)) 100.0; Mod.inc (Damage(OfType Physical)) 100.0; Mod.inc (Damage(OfType Fire)) 50.0 ]
        let d = Hit.rollUnmitigated (Rng 1u) ctx (ModDb.ofList mods) skill false
        near 150.0 (Dmg.get Fire d)
        near 0.0 (Dmg.get Physical d))

    test "gain as extra comes from base damage, then conversion applies to it too" (fun () ->
        // 100 physical; gain 20% of physical as extra fire -> 100 phys + 20 fire;
        // then 50% physical -> cold: 50 phys, 50 cold, 20 fire
        let mods = [ Mod.base' (GainAs(Physical, Fire)) 20.0; Mod.base' (Convert(Physical, Cold)) 50.0 ]
        let r = Hit.ranges ctx (ModDb.ofList mods) { Skill.make "x" [] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ] }
        near 50.0 r.[Physical].Max
        near 50.0 r.[Cold].Max
        near 20.0 r.[Fire].Max)

    test "gained physical damage is converted like any other physical" (fun () ->
        let mods = [ Mod.base' (GainAs(Fire, Physical)) 50.0; Mod.base' (SkillConvert(Physical, Lightning)) 100.0 ]
        let r = Hit.ranges ctx (ModDb.ofList mods) { Skill.make "x" [] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ] }
        equal false (r.ContainsKey Physical)
        near 50.0 r.[Lightning].Max
        near 100.0 r.[Fire].Max)

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

    test "Flammability left by earlier fire hits adds to the chance to ignite" (fun () ->
        let def = enemy []
        let threshold = Ailments.threshold def
        // a fire hit of the whole ailment threshold: 20% Flammability (1% per 5% of the threshold)
        let hit = { Crit = false; Unmitigated = Map.ofList [ Fire, threshold ]; Dealt = Map.empty }
        near 20.0 (Ailments.flammability ctx ModDb.empty def hit)
        near 0.0 (Ailments.flammability ctx ModDb.empty def { hit with Unmitigated = Map.ofList [ Cold, threshold ] })
        // with 35% already on the enemy the same hit ignites with 55%
        let primed = { def with Mods = ModDb.add def.Mods (Mod.base' (Named "Flammability") 35.0) }
        near 55.0 (Ailments.chance ctx ModDb.empty primed hit Ignite))

    test "one critical hit roll per use: every hit of the use shares it" (fun () ->
        let skill = { fireSpell 100.0 with BaseCrit = 50.0 }
        let rng = Rng 3u
        // a roll below the chance: all crit; above: none, whatever the hit's own dice would say
        equal true ([ for _ in 1 .. 200 -> Hit.rollWith (Some 0.2) rng ctx ModDb.empty skill (enemy []) ] |> List.forall (fun h -> h.Crit))
        equal true ([ for _ in 1 .. 200 -> Hit.rollWith (Some 0.8) rng ctx ModDb.empty skill (enemy []) ] |> List.forall (fun h -> not h.Crit)))

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

    test "sim: a sealed skill starts with its seals, breaks them on use and regains them over time" (fun () ->
        // two seals at most, one every 2.5 s; each broken seal repeats the hit at half damage.
        // 1 s per use, at 0 .. 6 s: the first breaks two seals, those at 3 s and 6 s one each
        let seal name v = Mod.base' (Named name) v
        let skill = { fireSpell 100.0 with Mods = [ seal "MaxSeals" 2.0; seal "SealIntervalMs" 2500.0; seal "SealRepeats" 1.0; seal "SealRepeatDamage" -50.0 ] }
        let f = { fight skill 1e9 with MaxTime = 6.5 }
        let o = Sim.run f 1u
        equal 7 o.Counts.["uses:Bolt"]
        equal 3 o.Counts.["seals:Bolt"]
        // hits land at the end of a use: six by 6.5 s, and the three repeats of the first two breaks
        near (6.0 * 100.0 + 3.0 * 50.0) o.DamageBy.["Bolt (hit)"])

    // an attack that always hits, 1 s per use, and the mods a skill gets from its Glory / Combo stats
    let strike name dmg = { Skill.make name [ Attack; Tag "Type:Melee" ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt dmg ]; BaseCrit = 0.0 }
    let sure = Mod.base' (Named "Accuracy") 1e12
    let needs name stat v = Mod.base' (Named stat) v |> Mod.whenever (SkillNamed name)
    let duel skills time = { Fight.single (boss 1e12) (Actor.make "Player" [ noIgnite; sure ] skills) time with MaxTime = time }

    test "glory: a skill that needs it waits until attack hits have built it, then spends it" (fun () ->
        // 100 Glory, 50% of the monster's power (20 for a boss) per attack hit: ten hits of the
        // other skill. Its own hits build nothing.
        let slam = { strike "Slam" 1000.0 with Mods = [ needs "Slam" "Requires:Glory" 100.0; needs "Slam" "Builds:Glory:AttackHit" 50.0 ] }
        let o = Sim.run (duel [ slam; strike "Jab" 10.0 ] 22.5) 1u
        // Jab at 0 .. 9 s, Slam at 10 s, Jab at 11 .. 20 s, Slam at 21 s, Jab at 22 s
        equal 21 o.Counts.["uses:Jab"]
        equal 2 o.Counts.["uses:Slam"])

    test "glory: less Glory required shortens the wait" (fun () ->
        let slam = { strike "Slam" 1000.0 with Mods = [ needs "Slam" "Requires:Glory" 100.0; needs "Slam" "Builds:Glory:AttackHit" 50.0; needs "Slam" "GloryRequiredMore" -50.0 ] }
        let o = Sim.run (duel [ slam; strike "Jab" 10.0 ] 12.5) 1u
        equal 2 o.Counts.["uses:Slam"]
        equal 11 o.Counts.["uses:Jab"])

    test "combo: one per melee attack hit with another skill, spent on use" (fun () ->
        let bell = { strike "Bell" 1000.0 with Mods = [ needs "Bell" "Requires:Combo" 4.0 ] }
        let o = Sim.run (duel [ bell; strike "Jab" 10.0 ] 10.5) 1u
        // Jab at 0 .. 3 s, Bell at 4 s, Jab at 5 .. 8 s, Bell at 9 s, Jab at 10 s
        equal 9 o.Counts.["uses:Jab"]
        equal 2 o.Counts.["uses:Bell"]
        // a spell builds no Combo
        let o = Sim.run (duel [ bell; fireSpell 10.0 ] 10.5) 1u
        equal false (o.Counts.ContainsKey "uses:Bell"))

    test "chained attack: every third use is the slower final strike, twice over" (fun () ->
        // uses at 0 and 1 s are plain; the third (2 s, half speed: until 4 s) hits twice for 300;
        // then again from 4 s
        let final =
            { strike "Jab: Final" 300.0 with
                Mods = [ Mod.flag (Named "SeqSteps:2"); Mod.base' (Named "SeqHits") 2.0; Mod.more Speed -50.0 |> Mod.whenever (SkillNamed "Jab: Final") ] }
        let jab = { strike "Jab" 100.0 with Mods = [ Mod.flag (Named "SeqSteps:0,1"); Mod.base' (Named "SeqLength") 3.0 ]; Parts = [ final ] }
        let o = Sim.run (duel [ jab ] 6.5) 1u
        equal 6 o.Counts.["uses:Jab"]
        near 400.0 o.DamageBy.["Jab (hit)"]
        near 600.0 o.DamageBy.["Jab: Final (hit)"])

    test "seed: planted once, it explodes when the enemy has taken enough damage since" (fun () ->
        // the seed needs 100% of the ailment threshold in damage; Bolt deals a quarter of it per hit
        let b = boss 1e12
        let threshold = Ailments.threshold { Level = b.Level; Mods = ModDb.ofList b.Mods }
        let bomb = { fireSpell 5000.0 with Name = "Bomb"; Mods = [ Mod.base' (Named "DetonatedByDamage") 100.0 ] }
        let bolt = fireSpell (threshold / 4.0)
        let f = { Fight.single b (Actor.make "Player" [ noIgnite ] [ bomb; bolt ]) 9.5 with MaxTime = 9.5 }
        let o = Sim.run f 1u
        // Bomb at 0 s, Bolts land at 2, 3, 4, 5 s: it explodes at 5 s and is planted again
        equal 2 o.Counts.["planted:Bomb"]
        equal 1 o.Counts.["hits:Bomb"]
        near 5000.0 o.DamageBy.["Bomb (hit)"])

    test "barrage: the next repeatable attack fires again per repeat in one longer use; the cooldown grows with it" (fun () ->
        let barrage =
            { Skill.make "Barrage" [ Spell ] with
                Hits = []; Cooldown = Some 2.0; Duration = 5.0
                Mods = [ Mod.base' (Named "BarrageRepeats") 2.0; Mod.base' (Named "BarrageRepeatLess") 50.0; Mod.base' (Named "BarrageCooldownPct") 400.0 ] }
        let shot = { strike "Shot" 100.0 with Tags = Set.ofList [ Attack; Tag "Type:Barrageable" ] }
        let o = Sim.run (duel [ barrage; shot ] 9.5) 1u
        // Barrage at 0 s; the shot at 1 s takes 1.1 s and hits for 100, 50, 50; Barrage is back
        // at 2 + 4 x 1.1 = 6.4 s; plain shots from 2.1 s land at 3.1 .. 7.1 s; Barrage again at
        // 7.1 s, the barraged shot from 8.1 s
        equal 2 o.Counts.["uses:Barrage"]
        equal 2 o.Counts.["barraged:Shot"]
        near (2.0 * 200.0 + 5.0 * 100.0) o.DamageBy.["Shot (hit)"])

    test "empowered repeatable attacks: the next two trigger a skill on hit, the third does not" (fun () ->
        let buff = "EmpoweredRepeatable:Tip"
        let tip = { Skill.make "Tip" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let shot = { strike "Shot" 100.0 with Tags = Set.ofList [ Attack; Tag "Type:Barrageable" ] }
        let frag = { fireSpell 30.0 with Name = "Frag"; Tags = Set.ofList [ Spell; Triggered ] }
        let f = duel [ tip; shot; frag ] 5.5
        let player =
            { f.Player with
                Rotation = f.Player.Rotation |> List.filter (fun st -> st.Use <> "Frag")
                Rules =
                    [ { Hook = OnUse; Filter = Called "Tip"; Chance = 100.0; Reaction = SetStacks(buff, 2) }
                      { Hook = OnHit; Filter = WithTag(Tag "Type:Barrageable"); Chance = 100.0; Reaction = IfBuff(buff, TriggerSkill "Frag") } ] }
        let o = Sim.run { f with Player = player } 1u
        equal 4 o.Counts.["hits:Shot"]
        equal 2 o.Counts.["hits:Frag"])

    test "a part that waits to be set off: the next three attack hits each cause one, the use itself none" (fun () ->
        let gem = Tag "Gem:Bell"
        let wave = { strike "Bell: Wave" 30.0 with Tags = Set.ofList [ Attack; gem; Triggered ]; Mods = [ Mod.flag (Named "OnlyTriggered") ] }
        let bell = { strike "Bell" 50.0 with Tags = Set.ofList [ Attack; gem ]; Cooldown = Some 100.0; Parts = [ wave ] }
        let f = duel [ bell; strike "Jab" 10.0 ] 5.5
        let rules =
            [ { Hook = OnUse; Filter = Called "Bell"; Chance = 100.0; Reaction = SetStacks("Stands", 3) }
              { Hook = OnHit; Filter = WithTagNot(Attack, gem); Chance = 100.0; Reaction = SpendStack("Stands", TriggerSkill "Bell: Wave") } ]
        let o = Sim.run { f with Player = { f.Player with Rules = rules } } 1u
        equal 1 o.Counts.["hits:Bell"]
        equal 4 o.Counts.["hits:Jab"]
        equal 3 o.Counts.["hits:Bell: Wave"])

    test "warp into one's own projectile: only with one on its way; it ends the projectile and explodes" (fun () ->
        // Ball: 1 s cast, then a bolt every second for four seconds. Warp: only against a
        // cullable enemy or into a Ball, 0.5 s
        let ball = { fireSpell 10.0 with Name = "Ball" }
        let warp = { fireSpell 1000.0 with Name = "Warp"; BaseTime = 0.5; Mods = [ Mod.flag (Named "OnlyVs:Cullable"); Mod.flag (Named "WarpsInto:Ball") ] }
        let f = { Fight.single (boss 1e12) (Actor.make "Player" [ noIgnite ] [ warp; ball ]) 6.2 with MaxTime = 6.2 }
        let f = { f with Player = { f.Player with HitPattern = Map.ofList [ "Ball", [ for k in 1 .. 4 -> After(float k), 1.0 ] ] } }
        let o = Sim.run f 1u
        // Ball at 0 s, its first bolt at 1 s; the warp then (the other three bolts never come)
        // explodes at 1.5 s; and so on every 1.5 s: four balls, four bolts, four warps
        equal 4 o.Counts.["warped into:Ball"]
        near 40.0 o.DamageBy.["Ball (hit)"]
        near 4000.0 o.DamageBy.["Warp (hit)"])

    test "demon form: a Demonflame per second, life lost for each, the form ends at 1 life" (fun () ->
        // 1000 life; 60 life per second per Demonflame: 60 + 120 + .. + 360 = 1260 by 7 s
        let form = { Skill.make "Form" [ Spell ] with Hits = []; Cooldown = Some 5.0; Mods = [ Mod.flag (Named "ToggleBuff:Demon Form") ] }
        let carried = [ Mod.base' (Named "DemonLifeLossPerStack") 60.0; Mod.base' (Named "DemonCooldown") 5.0 ] |> List.map (Mod.from "Form")
        let more = Mod.more (Damage AllDamage) 10.0 |> Mod.whenever (PerStack("Demonflame", 1.0))
        let player =
            { Actor.make "Player" [ noIgnite; more ] [ form; fireSpell 100.0 ] with
                Vitals = Some(PlayerStats.basic 1000.0 0.0)
                Rules = [ { Hook = OnUse; Filter = Called "Form"; Chance = 100.0; Reaction = GrantBuff("Demon Form", 1e9, 1, carried) } ] }
        let o = Sim.run { Fight.single (boss 1e12) player 8.5 with MaxTime = 8.5 } 1u
        equal 1 o.Counts.["demon form ended"]
        equal 1 o.Counts.["uses:Form"]
        // bolts land at 2 .. 8 s with 2, 3, 4, 5, 6 Demonflame, then none
        near (100.0 * (1.2 + 1.3 + 1.4 + 1.5 + 1.6 + 1.0 + 1.0)) o.DamageBy.["Bolt (hit)"])

    test "rain: projectiles land at random over the area; the share that hits is the area ratio" (fun () ->
        // area 36, impact 7, a target of radius 10 in the middle: (17 / 36)^2 = 22.3%
        let target = { EnemyDef.atLevel 83 1e6 with Size = 10.0 }
        let rain = Projectiles { ProjectileShape.make 10000 with Rain = Some(36.0, 7.0) }
        let hits = (Geometry.targetsFrom (Rng 7u) [] 100.0 rain 1.0 target [ target ]).Length
        equal true (abs (float hits / 10000.0 - 0.223) < 0.015))

    test "a part that happens once per use while the rest keeps hitting" (fun () ->
        let spray = { fireSpell 10.0 with Name = "Vent: Spray" }
        let vent = { fireSpell 100.0 with Name = "Vent"; Cooldown = Some 100.0; Mods = [ Mod.flag (Named "OncePerUse") ]; Parts = [ spray ] }
        let f = { Fight.single (boss 1e12) (Actor.make "Player" [ noIgnite ] [ vent ]) 5.5 with MaxTime = 5.5 }
        let f = { f with Player = { f.Player with HitPattern = Map.ofList [ "Vent", [ for k in 1 .. 4 -> After(float k), 1.0 ] ] } }
        let o = Sim.run f 1u
        near 100.0 o.DamageBy.["Vent (hit)"]
        near 40.0 o.DamageBy.["Vent: Spray (hit)"])

    test "sigil: a stage for every half of maximum mana spent while it stands, up to its maximum" (fun () ->
        // 100 mana (plenty of regeneration), a Bolt costs 30: stages at the 2nd and 4th Bolt
        let sigil = { Skill.make "Sigil" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let mods = [ Mod.more (Damage AllDamage) 10.0; Mod.base' (Named "SigilMaxStages") 3.0; Mod.base' (Named "SigilManaPercentPerStage") 50.0 ]
        let player =
            { Actor.make "Player" [ noIgnite ] [ sigil; { fireSpell 100.0 with Cost = 30.0 } ] with
                Vitals = Some { PlayerStats.basic 1000.0 100.0 with ManaRegen = 100.0 }
                Rules = [ { Hook = OnUse; Filter = Called "Sigil"; Chance = 100.0; Reaction = GrantBuff("Sigil buff", 60.0, 1, mods) } ] }
        let o = Sim.run { Fight.single (boss 1e12) player 7.5 with MaxTime = 7.5 } 1u
        equal 2 o.Counts.["sigil stage"]
        // hits at 2 s with one stage, at 3 and 4 s with two, at 5, 6 and 7 s with three
        near (110.0 + 2.0 * 120.0 + 3.0 * 130.0) o.DamageBy.["Bolt (hit)"])

    test "a skill that empowers the next attack: that one only" (fun () ->
        let buff = "EmpoweredAttacks:Mantra"
        let mantra = { Skill.make "Mantra" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let more = Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff buff)
        let f = duel [ mantra; strike "Jab" 10.0 ] 5.5
        let player =
            { f.Player with
                Mods = more :: f.Player.Mods
                Rules = [ { Hook = OnUse; Filter = Called "Mantra"; Chance = 100.0; Reaction = SetStacks(buff, 1) } ] }
        let o = Sim.run { f with Player = player } 1u
        // Jab lands at 2, 3, 4, 5 s: the first doubled
        near 50.0 o.DamageBy.["Jab (hit)"])

    test "recorded damage: a quarter of what the enemy lost is taken again when the debuff ends" (fun () ->
        let agony = { Skill.make "Agony" [ Spell ] with Hits = []; Duration = 4.0; Mods = [ Mod.base' (Named "RecordsDamage") 25.0 ] }
        let f = { Fight.single (boss 1e12) (Actor.make "Player" [ noIgnite ] [ agony; fireSpell 100.0 ]) 6.5 with MaxTime = 6.5 }
        let o = Sim.run f 1u
        // Agony at 0 s lasts until 5 s; the Bolts at 2, 3 and 4 s are recorded (the one at 5 s
        // lands as it ends): a quarter of 300
        near 75.0 o.DamageBy.["Agony"]
        equal 2 o.Counts.["recording:Agony"])

    test "banner: attack hits build its Glory, then its aura lasts for the duration" (fun () ->
        // 200 Glory at 100% of 20 per hit: placed with the tenth hit (10 s) and the twentieth.
        // 4 s of 100% more damage: the hits at 11, 12 and 13 s are doubled, the one at 14 s is not
        let banner = { Banner.Name = "War Banner"; Glory = 200.0; PerAttackHit = 100.0; Duration = 4.0; Mods = [ Mod.more (Damage AllDamage) 100.0 ] }
        let f = duel [ strike "Jab" 100.0 ] 20.5
        let f = { f with Player = { f.Player with Banners = [ banner ] } }
        let o = Sim.run f 1u
        equal 2 o.Counts.["banner:War Banner"]
        near 2300.0 o.DamageBy.["Jab (hit)"])

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
                Metas = [ Meta.simple "Cast on Hit" 100.0 "Nova" ] }
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

    let dummy name x y = { EnemyDef.atLevel 83 1e9 with Name = name; Position = Vec.make x y; Size = 4.0; Mods = []; Attack = None }

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
        let hits = Geometry.targetsFrom (Rng 1u) [] 20.0 (Projectiles p) 1.0 boss [ boss ] |> List.length
        if hits < 5 || hits > 7 then failwithf "expected about 6 hits, got %d" hits
        // impact inside the boss: every projectile hits
        equal 36 (Geometry.targetsFrom (Rng 1u) [] 35.0 (Projectiles p) 1.0 boss [ boss ] |> List.length))

    test "geometry: erratic projectiles zigzag within their band and hit what a leg crosses" (fun () ->
        let boss = { dummy "b" 30.0 0.0 with Size = 10.0 }
        let p = { ProjectileShape.make 1 with Wander = 20.0; Range = 200.0 }
        for seed in 1u .. 50u do
            // heading straight at the enemy: at most 10 off its centre line, so it hits, and stops there
            let at = Geometry.trace (Rng seed) [] p Vec.zero (Vec.make 1.0 0.0) [ boss ]
            equal [ "b" ] at.Hit
            // heading away at a right angle: never nearer than 20, and it flies its whole reach
            let off = Geometry.trace (Rng seed) [] p Vec.zero (Vec.make 0.0 1.0) [ boss ]
            equal [] off.Hit
            near 200.0 (List.last off.Points).Y
            // every corner of the zigzag lies within half the band of the heading
            for pt in off.Points do
                if abs pt.X > 10.0 + 1e-9 then failwithf "strayed %f" pt.X
            if off.Points |> List.forall (fun pt -> abs pt.X < 1e-9) then failwith "flew straight"
        // a full circle of 36 from the player, at a random angle to the enemy: the share that
        // heads into it hits (2 asin(10/30) = 39 of 360 degrees, a little more for the zigzag)
        let ring = { p with Count = 36; Spread = 360.0 }
        let hits = [ 1u .. 200u ] |> List.sumBy (fun seed -> Geometry.targetsFrom (Rng seed) [] infinity (Projectiles ring) 1.0 boss [ boss ] |> List.length)
        let share = float hits / (200.0 * 36.0)
        if share < 0.10 || share > 0.20 then failwithf "share %.3f" share
        // "fired at the same time can Hit the same target no more than once every 0.66 seconds":
        // nine sparks sent the same way all reach the enemy within a moment, and one hits
        let volley = { p with Count = 9; Spread = 0.0; HitInterval = 0.66; Speed = 100.0 }
        equal [ "b" ] (Geometry.targetsFrom (Rng 3u) [] infinity (Projectiles volley) 1.0 boss [ boss ])
        // a second enemy behind is hit by one of the sparks that passed through the first
        let behind = { dummy "c" 70.0 0.0 with Size = 10.0 }
        equal [ "b"; "c" ] (Geometry.targetsFrom (Rng 3u) [] infinity (Projectiles volley) 1.0 boss [ boss; behind ])
        // the fan widens with every spark: 15 degrees apart, three span 30, twenty-five the full circle
        let fan n = Geometry.paths (Rng 1u) [] { p with Count = n; SpreadPerProjectile = 15.0; Wander = 0.0; Range = 10.0 } infinity boss [] |> List.map (fun path -> Vec.angle (List.last path.Points) * 180.0 / System.Math.PI)
        near -15.0 (List.head (fan 3))
        near 15.0 (List.last (fan 3))
        near 60.0 (List.last (fan 9)))

    test "geometry: scattered chunks and spears in wait hit by where they land" (fun () ->
        let boss = { dummy "b" 30.0 0.0 with Size = 10.0 }
        // chunks thrown 1 to 40 units on from the enemy, exploding in 18: those within 28 of
        // its centre hit, 27 of the 39 units of distance
        let chunks = Projectiles { ProjectileShape.make 10000 with FromImpact = true; Scatter = Some(1.0, 40.0, 100.0, 18.0) }
        let share = float (Geometry.targetsFrom (Rng 1u) [] infinity chunks 1.0 boss [ boss ]).Length / 10000.0
        if abs (share - 27.0 / 39.0) > 0.02 then failwithf "chunks: %.3f" share
        // spears at random over a radius of 69: only one the enemy stands on (within its own 10) goes off
        let spears = Projectiles { ProjectileShape.make 10000 with Rain = Some(69.0, 6.0); TouchTriggered = true }
        let touched = float (Geometry.targetsFrom (Rng 1u) [] infinity spears 1.0 boss [ boss ]).Length / 10000.0
        if abs (touched - (10.0 / 69.0) ** 2.0) > 0.005 then failwithf "spears: %.4f" touched
        // "Can Hit each enemy only once every 0.12 seconds", an arrow every 0.08: of arrows that
        // all land on the enemy, every second one hits
        let dense = Projectiles { ProjectileShape.make 100 with Rain = Some(1.0, 7.0); Every = 0.08; HitInterval = 0.12 }
        equal 50 (Geometry.targetsFrom (Rng 1u) [] infinity dense 1.0 boss [ boss ]).Length
        // without the wait, the blast radius counts as well
        let rain = Projectiles { ProjectileShape.make 10000 with Rain = Some(69.0, 6.0) }
        let rained = float (Geometry.targetsFrom (Rng 1u) [] infinity rain 1.0 boss [ boss ]).Length / 10000.0
        if abs (rained - (16.0 / 69.0) ** 2.0) > 0.008 then failwithf "rain: %.4f" rained)

    test "geometry: a projectile bounces off a wall once with terrain chain" (fun () ->
        // flying +x from the origin, wall at x = 20; the enemy sits off-axis, within chain
        // range of the wall, so the chain has to target it (a mirror bounce would miss)
        let behind = dummy "behind" 0.0 15.0
        let wall = Vec.make 20.0 -50.0, Vec.make 20.0 50.0
        let p chance = { ProjectileShape.make 1 with Range = 100.0; TerrainChain = chance; ChainRange = 30.0 }
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

    let vitals = { PlayerStats.basic 100.0 100.0 with ManaRegen = 10.0 }

    test "vitals: a skill that costs more than regen waits for mana" (fun () ->
        // 50 mana per 1 s cast, 100 mana pool, 10 mana/s regen: 2 free casts, then one cast per 5 s
        let bolt = { fireSpell 100.0 with Cost = 50.0 }
        let player = { Actor.make "Player" [ noIgnite ] [ bolt ] with Vitals = Some vitals }
        let o = Sim.run (Fight.single (boss 1e9) player 20.0) 1u
        let uses = o.Counts.["uses:Bolt"]
        if uses < 5 || uses > 7 then failwithf "expected about 6 uses, got %d" uses
        if o.ManaStarved < 8.0 then failwithf "expected long mana waits, got %g" o.ManaStarved)

    test "vitals: monsters walk up, hit, and can kill the player" (fun () ->
        let attack = { Interval = 1.0; Damage = Map.ofList [ Physical, 30.0 ]; Accuracy = 1e9; Range = 5.0; CritChance = 0.0; Melee = true }
        let brute = { dummy "brute" 50.0 0.0 with Attack = Some attack; Speed = 10.0 }
        let player = { Actor.make "Player" [ noIgnite ] [ { fireSpell 0.0 with BaseDamage = Map.empty } ] with Vitals = Some vitals }
        let o = Sim.run (Fight.single brute player 30.0) 1u
        // walks 50 - 4 size - 5 range = 41 units at 10/s, then 30 per second against 100 life
        match o.Died with
        | Some t when t > 4.0 && t < 9.0 -> ()
        | d -> failwithf "expected death between 4 and 9 s, got %A" d)

    test "meta: triggered skills give no energy (no trigger loops)" (fun () ->
        // a crit-only bolt feeds Cast on Crit; the triggered nova also always crits but must not feed it
        let bolt = { fireSpell 100.0 with BaseCrit = 100.0 }
        let nova = { fireSpell 100.0 with Name = "Nova"; BaseCrit = 100.0; Tags = Set.ofList [ Spell; Triggered ] }
        let meta = { Meta.simple "CoC" 1.0 "Nova" with Sources = [ CritHits 1.0 ]; EnergyMult = 1e6 }
        let player = { Actor.make "Player" [ noIgnite ] [ bolt; nova ] with Rotation = [ Actor.always "Bolt" ]; Metas = [ meta ] }
        let o = Sim.run (Fight.single (boss 1e12) player 5.5) 1u
        // 5 bolts -> 5 crits -> 5 triggers; the novas' own crits add nothing
        equal 5 o.Counts.["trigger:Nova"])

    // ---- defence: what monster hits do to the player ----
    let idle = { fireSpell 0.0 with BaseDamage = Map.empty }
    let attacker t dmg interval =
        { dummy "brute" 8.0 0.0 with
            Attack = Some { Interval = interval; Damage = Map.ofList [ t, dmg ]; Accuracy = 1e9; Range = 5.0; CritChance = 0.0; Melee = false } }
    let against stats enemy time = Sim.run (Fight.single enemy { Actor.make "Player" [ noIgnite ] [ idle ] with Vitals = Some stats } time) 1u

    test "defence: chaos damage removes twice as much energy shield" (fun () ->
        let stats = { PlayerStats.basic 100.0 0.0 with EnergyShield = 100.0 }
        let died t = (against stats (attacker t 30.0 1.0) 30.0).Died |> Option.defaultValue 99.0
        // physical: 100 ES + 100 life last 7 hits; chaos: the ES only absorbs 50
        if died Chaos > died Physical - 1.5 then failwithf "chaos %g, physical %g" (died Chaos) (died Physical))

    test "defence: recoup gives back its share of damage taken over 4 seconds" (fun () ->
        let plain = PlayerStats.basic 100.0 0.0
        let recoup = { plain with Recoup = [ "Life", DamageType.all, 90.0 ] }
        // 20 per second in, 18 per second back once the streams overlap
        let died s = (against s (attacker Physical 20.0 1.0) 60.0).Died |> Option.defaultValue 99.0
        if died recoup < died plain + 5.0 then failwithf "recoup %g, plain %g" (died recoup) (died plain))

    test "defence: energy shield recharges after the delay" (fun () ->
        // one hit of 80 into 100 ES, then nothing: back to full, so a second hit 10 s later is absorbed too
        let stats = { PlayerStats.basic 100.0 0.0 with EnergyShield = 100.0; EnergyShieldRecharge = 50.0; ESRechargeDelay = 4.0 }
        let o = against stats (attacker Physical 80.0 10.0) 25.0
        equal None o.Died
        near 1.0 o.MinLife)

    test "defence: cold hits build up freeze on the player" (fun () ->
        // threshold 500: each 60 cold hit builds 24%
        let o = against (PlayerStats.basic 1000.0 0.0) (attacker Cold 60.0 1.0) 8.0
        if not (o.Counts.ContainsKey "player:Frozen") then failwithf "never frozen: %A" o.Counts)

    test "defence: a big fire hit ignites the player and burns" (fun () ->
        let o = against { PlayerStats.basic 1e7 0.0 with AilmentThreshold = 1000.0 } (attacker Fire 2e5 8.0) 16.0
        equal true (o.Counts.ContainsKey "player:Ignited")
        // more than whole hits: the burn
        let burn = o.DamageTaken - 2e5 * floor (o.DamageTaken / 2e5)
        if burn < 1e4 then failwithf "no burn: took %g" o.DamageTaken)

    test "defence: armour applying to elemental damage lowers it" (fun () ->
        let plain = PlayerStats.basic 1e6 0.0
        let armoured = { plain with Armour = 5000.0; ArmourAppliesTo = Map.ofList [ Fire, 100.0 ] }
        let taken s = (against { s with Avoid = Map.ofList [ "Ignite", 100.0 ] } (attacker Fire 500.0 1.0) 5.5).DamageTaken
        // 5000 / (5000 + 10 * 500) = 50% less
        near 0.5 (taken armoured / taken plain))

    test "accuracy: attacks can miss, spells can't" (fun () ->
        near 100.0 (Hit.accuracyHitChance 1000.0 2000.0)
        near 63.0 (Hit.accuracyHitChance 1000.0 300.0)
        // at 9 m accuracy is 90% lower
        near 30.0 (Hit.accuracyAtDistance 90.0 300.0)
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 10.0 ] }
        let run mods skill = Sim.run (Fight.single (boss 1e9) (Actor.make "Player" (noIgnite :: mods) [ skill ]) 200.5) 1u
        let poor = run [ Mod.base' (Named "Accuracy") 50.0 ] swing
        if not (poor.Counts.ContainsKey "miss:Swing") then failwith "an inaccurate attack never missed"
        let spell = run [ Mod.base' (Named "Accuracy") 50.0 ] (fireSpell 10.0)
        equal false (spell.Counts.ContainsKey "miss:Bolt"))

    test "skill damage over time ticks for its duration and doesn't stack" (fun () ->
        // no hit damage; 100 chaos per second for 2 s, recast every second: always exactly one instance
        let drain = { Skill.make "Drain" [ Spell ] with Dots = Map.ofList [ Chaos, 100.0 ]; Duration = 2.0; BaseTime = 1.0 }
        let o = Sim.run (Fight.single (boss 1e9) (Actor.make "Player" [ noIgnite ] [ drain ]) 10.5) 1u
        // first hit lands at 1 s; 9.5 s of ticking
        near 950.0 o.DamageBy.["Drain (chaos over time)"])

    test "defence: ordinary hits never heavy stun the player" (fun () ->
        // big enough to light stun every time; players are only heavily stunned while blocking with a raised shield or parrying
        let stats = { PlayerStats.basic 1e6 0.0 with StunThreshold = 1000.0 }
        let o = against stats (attacker Chaos 2000.0 1.0) 30.0
        equal false (o.Counts.ContainsKey "player:HeavilyStunned")
        if not (o.Counts.ContainsKey "player:Stunned") then failwith "light stuns should still happen")

    test "charms: used when their condition occurs, preventing it while they have charges" (fun () ->
        let thawing =
            { Name = "Thawing Charm"; Trigger = "Frozen"; Buff = "Charm1"; Prevents = true; Duration = 3.0; ChargesPerUse = 40.0; MaxCharges = 80.0
              ChargesPerSecond = 0.0; ChargeOnKillChance = 0.0; ChargeGainMult = 1.0; LifeOnUse = 0.0; ManaOnUse = 0.0; StartsRecharge = false }
        let fight charms time =
            let player = { Actor.make "Player" [ noIgnite ] [ idle ] with Vitals = Some(PlayerStats.basic 1e6 0.0); Charms = charms }
            Sim.run (Fight.single (attacker Cold 60000.0 1.0) player time) 1u
        // threshold 500000: each hit builds 24%, so a freeze would come with every 5th hit
        let bare = fight [] 30.5
        if not (bare.Counts.ContainsKey "player:Frozen") then failwith "no freeze without the charm"
        // two uses' worth of charges: the first two freezes are stopped, the third gets through
        let o = fight [ thawing ] 30.5
        equal 2 o.Counts.["charm:Thawing Charm"]
        if o.Counts.["charm prevented:Frozen"] < 2 then failwith "the charm should have prevented the freezes it was used on"
        if not (o.Counts.ContainsKey "player:Frozen") then failwith "out of charges, the next freeze should land"
        if o.Counts.["player:Frozen"] >= bare.Counts.["player:Frozen"] then failwith "the charm made no difference")

    test "kills of enemies with a status set things off; stacks are spent one per hit" (fun () ->
        // an attack that kills a marked enemy grants 2 stacks; the next 2 attack hits each trigger a bolt
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let bolt = { Skill.make "Bolt" [ Attack; Herald; Triggered ] with BaseDamage = Map.ofList [ Lightning, Range.fixedAt 5.0 ]; BaseCrit = 0.0 }
        let attack = WithTagNot(Attack, Herald)
        let rules =
            [ { Hook = OnUse; Filter = Called "Swing"; Chance = 100.0; Reaction = ApplyDebuff("Doomed", 100.0, 1, []) }
              { Hook = OnKillOf "Doomed"; Filter = attack; Chance = 100.0; Reaction = SetStacks("Bolts", 2) }
              { Hook = OnHit; Filter = attack; Chance = 100.0; Reaction = SpendStack("Bolts", TriggerSkill "Bolt") } ]
        let player = { Actor.make "Player" [] [ swing; bolt ] with Rotation = [ Actor.always "Swing" ]; Rules = rules }
        let weak = { dummy "weak" 10.0 0.0 with Life = 150.0 }
        let tough = { dummy "tough" 12.0 0.0 with Life = 1e9 }
        let o = Sim.run { Fight.single weak player 8.5 with Enemies = [ weak; tough ]; Targeting = Nearest } 1u
        // the second swing kills the weak one; the bolts ride on the next two swings, and their own hits spend nothing
        equal 2 o.Counts.["trigger:Bolt"]
        equal 2 o.Counts.["hits:Bolt"])

    test "charges: chance to gain one more, and 'gained a Power Charge recently'" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let perCharge = Mod.inc (Damage AllDamage) 100.0 |> Mod.whenever (PerStack("PowerCharge", 1.0))
        let run perUse mods =
            let rule = { Hook = OnUse; Filter = Called "Swing"; Chance = 100.0; Reaction = GainResource("PowerCharge", perUse) }
            let player = { Actor.make "Player" (perCharge :: mods) [ swing ] with Rotation = [ Actor.always "Swing" ]; Rules = [ rule ]; MaxResources = Map.ofList [ "PowerCharge", 100 ] }
            (Sim.run (Fight.single (boss 1e9) player 4.5) 1u).DamageBy.["Swing (hit)"]
        // one charge per swing with a 100% chance for one more is two charges per swing
        let one, two = run 1 [], run 2 []
        if two <= one then failwith "charges should add damage"
        near two (run 1 [ Mod.base' (Named "ExtraChargeChance:PowerCharge") 100.0 ])
        near one (run 1 [ Mod.base' (Named "ExtraChargeChance:FrenzyCharge") 100.0 ])
        // "if you've gained a Power Charge Recently" holds once a charge was gained
        let recent = run 1 [ Mod.more (Damage AllDamage) 50.0 |> Mod.whenever (WhileBuff "GainedPowerChargeRecently") ]
        if recent < one * 1.2 then failwith "gained recently should apply")

    test "overkill explosion: nearby enemies are ignited for a share of the overkill damage" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let blast = { Skill.make "Blast" [ Herald; Triggered ] with Delivery = AreaAtTarget 30.0; Duration = 3.0 }
        let burn = { Skill = "Blast"; Filter = WithTagNot(Attack, Herald); ThresholdPercent = 20.0; PercentPerSecond = 23.0 }
        let player = { Actor.make "Player" [] [ swing; blast ] with Rotation = [ Actor.always "Swing" ]; OverkillBurns = [ burn ] }
        let tough = { dummy "tough" 12.0 0.0 with Life = 1e9 }
        let run life = Sim.run { Fight.single tough player 6.5 with Enemies = [ { dummy "weak" 10.0 0.0 with Life = life }; tough ]; Targeting = Nearest } 1u
        // 100 damage into 50 life: 50 overkill (100% of its life): 23% of 50 per second for 3 s
        let o = run 50.0
        equal 1 o.Counts.["overkill ignite:Blast"]
        near (50.0 * 0.23 * 3.0) o.DamageBy.["Ignite"]
        // 100 into 90 life: 10 overkill is under 20% of 90: no explosion
        if (run 90.0).Counts.ContainsKey "overkill ignite:Blast" then failwith "overkill under the threshold should not explode")

    test "armour break: hits strip armour; fully broken armour takes 20% increased physical damage" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 1000.0 ]; BaseCrit = 0.0 }
        // 10000 armour against 1000 damage: 50% less. Breaking 400% of the damage dealt: 2000 after the first hit
        let armoured = { boss 1e9 with Mods = [ Mod.base' Armour 10000.0 ] }
        let run mods = Sim.run (Fight.single armoured (Actor.make "Player" mods [ swing ]) 3.5) 1u
        near 1500.0 (run []).DamageBy.["Swing (hit)"]
        let o = run [ Mod.base' (Named "ArmourBreakOfPhysical") 400.0 ]
        // hit 1: 500 dealt, breaks 2000 -> armour 8000: 1000 x (1 - 8000/18000) = 555.6, breaks 2222 -> 5778:
        // 1000 x (1 - 5778/15778) = 633.8
        near (500.0 + 1000.0 * (1.0 - 8000.0 / 18000.0) + 1000.0 * (1.0 - (8000.0 - 4000.0 / 1.8) / (18000.0 - 4000.0 / 1.8))) o.DamageBy.["Swing (hit)"]
        // an enemy without armour to speak of is fully broken at once: +20% from the second hit on
        let bare = { boss 1e9 with Mods = [ Mod.base' Armour 1.0 ] }
        let broken = Sim.run (Fight.single bare (Actor.make "Player" [ Mod.base' (Named "ArmourBreakOfPhysical") 100.0 ] [ swing ]) 3.5) 1u
        let first = 1000.0 * (1.0 - 1.0 / 10001.0)
        near (first + 2.0 * 1200.0) broken.DamageBy.["Swing (hit)"])

    test "rage: gained on hits, 1% more attack damage each, lost after 4 s without gaining" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let mods = [ Mod.base' (Named "RageOnHit") 5.0 |> Mod.whenever (SkillIs Melee) ]
        let player = { Actor.make "Player" mods [ swing ] with MaxResources = Map.ofList [ "Rage", 30 ] }
        let o = Sim.run (Fight.single (boss 1e9) player 8.5) 1u
        // hits at 1..8 s with 0, 5, 10 ... 30, 30 rage
        near (100.0 * (1.0 + 1.05 + 1.10 + 1.15 + 1.20 + 1.25 + 1.30 + 1.30)) o.DamageBy.["Swing (hit)"]
        // 30 rage and nothing to keep it: after 4 s it drains at 5 per second, gone at 10 s
        let slow = { swing with BaseTime = 12.0 }
        let idle = { Actor.make "Player" [] [ slow ] with MaxResources = Map.ofList [ "Rage", 30 ]; Resources = [ "Rage", 30 ] }
        near 100.0 (Sim.run (Fight.single (boss 1e9) idle 12.5) 1u).DamageBy.["Swing (hit)"]
        // regeneration keeps it up
        let fed = { idle with Mods = [ Mod.base' (Named "RageRegen") 6.0 ] }
        near 130.0 (Sim.run (Fight.single (boss 1e9) fed 12.5) 1u).DamageBy.["Swing (hit)"])

    test "a periodic heal: life back every so often" (fun () ->
        let player =
            { Actor.make "Player" [ noIgnite ] [ fireSpell 1.0 ] with
                Vitals = Some { PlayerStats.basic 1000.0 0.0 with StartLife = 0.5 }
                Periodics = [ { Name = "Time of Need"; Every = 2.0; Action = HealLife 100.0 } ] }
        let o = Sim.run { Fight.single (boss 1e9) player 5.0 with MaxTime = 5.0 } 1u
        equal 2 o.Counts.["healed"])

    test "a part that spends Rage takes the place of the plain hit while there is enough" (fun () ->
        // 25 Rage, 10 per use: two enraged uses (300 each), then plain ones (100)
        let big = { strike "Slam: Enraged" 300.0 with Mods = [ Mod.base' (Named "SpendsRage") 10.0 ] }
        let slam = { strike "Slam" 100.0 with Mods = [ Mod.flag (Named "NotEnraged") ]; Parts = [ big ] }
        let f = duel [ slam ] 4.5
        let o = Sim.run { f with Player = { f.Player with MaxResources = Map.ofList [ "Rage", 30 ]; Resources = [ "Rage", 25 ] } } 1u
        equal 2 o.Counts.["enraged:Slam"]
        // the Rage that is left gives 1% more attack damage each: 300 x 1.15 and 300 x 1.05
        near 660.0 o.DamageBy.["Slam: Enraged (hit)"]
        equal 2 o.Counts.["hits:Slam"])

    test "exposure that grows with every pulse, up to its cap" (fun () ->
        // an orb: 20% exposure at once, 10% more every second for 4 pulses, at most 50%.
        // The boss has no resistance: a hit of 100 deals 100 x (1 + exposure)
        let orb = { Skill.make "Orb" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let player =
            { Actor.make "Player" [ noIgnite ] [ orb; fireSpell 100.0 ] with
                Rules =
                    [ { Hook = OnUse; Filter = Called "Orb"; Chance = 100.0; Reaction = ApplyExposure(Fire, 20.0, 60.0) }
                      { Hook = OnUse; Filter = Called "Orb"; Chance = 100.0; Reaction = GrowExposure(Fire, 20.0, 10.0, 50.0, 1.0, 4, 60.0) } ] }
        let o = Sim.run { Fight.single (boss 1e12) player 6.5 with MaxTime = 6.5 } 1u
        // Bolts land at 2 .. 6 s: 40% exposure at 2 s, 50% from 3 s
        near (140.0 + 4.0 * 150.0) o.DamageBy.["Bolt (hit)"])

    test "an offering: the player's skill buffs the minions for its duration" (fun () ->
        // the minion hits for 100 every second; the offering at 0 s gives 50% increased damage for 3 s
        let off = { Skill.make "Offering" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let player =
            { Actor.make "Player" [ noIgnite ] [ off ] with
                Rules = [ { Hook = OnUse; Filter = Called "Offering"; Chance = 100.0; Reaction = GrantMinionBuff("Offering", 3.0, [ Mod.inc (Damage AllDamage) 50.0 ]) } ] }
        let minion = Actor.make "Skeleton" [ noIgnite; sure ] [ strike "Bite" 100.0 ]
        let f = { Fight.single (boss 1e12) player 5.5 with MaxTime = 5.5; Minions = [ minion ] }
        let o = Sim.run f 1u
        // bites land at 1 .. 5 s: those at 1 and 2 s are within the three seconds
        near (2.0 * 150.0 + 3.0 * 100.0) o.DamageBy.["Bite (hit)"])

    // ---- minions that can be killed ----
    let body life revive =
        { Life = life; EnergyShield = 0.0; Armour = 0.0; Evasion = 0.0; Resist = Map.empty; Melee = true; Revive = revive; Power = 0.55; LifeRegen = 0.0 }

    test "minions: a monster kills a minion, it is revived after its revive time and killed again" (fun () ->
        // the player can't be hurt, so every attack (60 a second) goes at the skeleton with 100
        // life: it dies with the second hit, is back 5 s later, takes the hit of that moment...
        let skeleton = { Actor.make "Skeleton" [ noIgnite; sure ] [ strike "Bite" 100.0 ] with Body = Some(body 100.0 (Some 5.0)) }
        let f = { Fight.single (attacker Physical 60.0 1.0) (Actor.make "Player" [ noIgnite ] [ idle ]) 20.0 with MaxTime = 20.0; Minions = [ skeleton ] }
        let o = Sim.run f 1u
        // deaths at t0 + 1, + 7, + 13, + 19 (t0 < 1: the first attack); back at + 6, + 12, + 18
        equal 4 o.Counts.["minion:died"]
        equal 3 o.Counts.["minion:revived"]
        // it stands for about a second each time: a bite or two, not twenty
        let bites = o.DamageBy.["Bite (hit)"]
        if bites < 100.0 || bites > 800.0 then failwithf "bites %g" bites)

    test "minions: one that does not revive is gone for good; its resistances and armour count" (fun () ->
        let spirit = { Actor.make "Spirit" [ noIgnite; sure ] [ strike "Bite" 100.0 ] with Body = Some(body 100.0 None) }
        let run enemy b = Sim.run { Fight.single enemy (Actor.make "Player" [ noIgnite ] [ idle ]) 10.0 with MaxTime = 10.0; Minions = [ { spirit with Body = Some b } ] } 1u
        let o = run (attacker Physical 60.0 1.0) (body 100.0 None)
        equal 1 o.Counts.["minion:died"]
        equal false (o.Counts.ContainsKey "minion:revived")
        // 75% fire resistance: 15 a hit, it lasts seven hits and the fight has at most ten
        let o = run (attacker Fire 60.0 1.0) { body 100.0 None with Resist = Map.ofList [ Fire, 75.0 ] }
        equal 1 o.Counts.["minion:died"]
        let o = run (attacker Fire 60.0 2.0) { body 100.0 None with Resist = Map.ofList [ Fire, 75.0 ] }
        equal false (o.Counts.ContainsKey "minion:died"))

    test "minions: the revive timer; a later death adds a n-th of it, at most the full time from now" (fun () ->
        equal (6.0, 1) (Sim.nextRevival 0.0 6.0 None)
        // a second death right away: the full time from now is less than 6 + 6/2
        equal (6.1, 2) (Sim.nextRevival 0.1 6.0 (Some(6.0, 1)))
        // a second death just before they would be back: 6 + 6/2
        equal (9.0, 2) (Sim.nextRevival 5.0 6.0 (Some(6.0, 1)))
        // a third: + 6/3
        equal (11.0, 3) (Sim.nextRevival 8.9 6.0 (Some(9.0, 2)))
        // after they are back the count starts again
        equal (26.0, 1) (Sim.nextRevival 20.0 6.0 (Some(9.0, 2))))

    test "minions: a reviving minion's death gives Cast on Minion Death energy by its power" (fun () ->
        // 50 energy per power, the skeleton has 0.55: 27.5 a death, 55 needed: every second death
        let nova = { fireSpell 100.0 with Name = "Nova"; Tags = Set.ofList [ Spell; Triggered ] }
        let meta = { Meta.simple "On Death" 55.0 "Nova" with Sources = [ MinionDeaths 50.0 ] }
        let player = { Actor.make "Player" [ noIgnite ] [ idle; nova ] with Rotation = [ Actor.always idle.Name ]; Metas = [ meta ] }
        let skeleton = { Actor.make "Skeleton" [ noIgnite; sure ] [ strike "Bite" 100.0 ] with Body = Some(body 100.0 (Some 5.0)) }
        let o = Sim.run { Fight.single (attacker Physical 60.0 1.0) player 20.0 with MaxTime = 20.0; Minions = [ skeleton ] } 1u
        equal 4 o.Counts.["minion:died"]
        equal 2 o.Counts.["trigger:Nova"])

    test "hexblast: waits until a curse is half expired, detonates it, and then has nothing to detonate" (fun () ->
        let hex = { fireSpell 0.0 with Name = "Hex"; BaseDamage = Map.empty; Cooldown = Some 100.0 }
        let blast = { fireSpell 500.0 with Name = "Blast"; Mods = [ Mod.flag (Named "DetonatesCurse") ] }
        let player =
            { Actor.make "Player" [ noIgnite ] [ blast; hex; fireSpell 10.0 ] with
                Rules = [ { Hook = OnHit; Filter = Called "Hex"; Chance = 100.0; Reaction = ApplyCurse("Curse:Hex", 4.0, []) } ] }
        let o = Sim.run { Fight.single (boss 1e12) player 7.5 with MaxTime = 7.5 } 1u
        // the curse lands at 1 s and lasts until 5 s: ripe from 3 s
        equal 1 o.Counts.["curse detonated"]
        near 500.0 o.DamageBy.["Blast (hit)"])

    test "berserk: life is lost for every Rage held while Rage is not being lost" (fun () ->
        // 20 Rage kept up by regeneration, 6% of life per minute per Rage: 2% of life a second
        let jab = { Skill.make "Jab" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 1.0 ]; BaseCrit = 0.0 }
        let player =
            { Actor.make "Player" [ Mod.base' (Named "RageRegen") 50.0; Mod.base' (Named "LifeLossPercentPerMinutePerRage") 6.0 ] [ jab ] with
                MaxResources = Map.ofList [ "Rage", 20 ]; Resources = [ "Rage", 20 ]
                Vitals = Some(PlayerStats.basic 1000.0 0.0) }
        let o = Sim.run { Fight.single (boss 1e9) player 10.0 with MaxTime = 10.0 } 1u
        near 0.8 o.MinLife)

    test "warcry: empowers one attack per so much monster power; each attack uses one up" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let cry = { Skill.make "Cry" [ Warcry ] with BaseTime = 0.5; Cooldown = Some 100.0; Hits = [] }
        let mods = [ Mod.inc (Damage AllDamage) 100.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "EmpoweredAttacks") ]
        let rules = [ { Hook = OnUse; Filter = Called "Cry"; Chance = 100.0; Reaction = Empower("EmpoweredAttacks", 10.0, 50.0) } ]
        let player = { Actor.make "Player" mods [ cry; swing ] with Rules = rules }
        // a unique monster has 20 power: two empowered attacks, then plain ones
        let unique = { EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 10.0 0.0) with Life = 1e9; Mods = []; Attack = None }
        let o = Sim.run (Fight.single unique player 5.75) 1u
        // swings land at 1.5, 2.5, 3.5, 4.5, 5.5
        near (200.0 + 200.0 + 100.0 + 100.0 + 100.0) o.DamageBy.["Swing (hit)"]
        // a lone normal monster still empowers one
        let normal = { EnemyDef.make Rarity.defaults 83 Normal "Mob" (Vec.make 10.0 0.0) with Life = 1e9; Mods = []; Attack = None }
        let o1 = Sim.run (Fight.single normal player 5.75) 1u
        near (200.0 + 4.0 * 100.0) o1.DamageBy.["Swing (hit)"])

    test "a curse recast on a cursed enemy sets off what happens when the curse ends" (fun () ->
        let hex = { Skill.make "Hex" [ Spell; Curse ] with BaseTime = 1.0 }
        let blast = { fireSpell 100.0 with Name = "Blast"; Tags = Set.ofList [ Spell; Triggered ] }
        let rules =
            [ { Hook = OnHit; Filter = Called "Hex"; Chance = 100.0; Reaction = IfEnemyHas("Curse:Hex", TriggerSkill "Blast") }
              { Hook = OnHit; Filter = Called "Hex"; Chance = 100.0; Reaction = ApplyCurse("Hex", 30.0, []) } ]
        let player = { Actor.make "Player" [ noIgnite ] [ hex; blast ] with Rotation = [ Actor.always "Hex" ]; Rules = rules }
        let o = Sim.run (Fight.single (boss 1e9) player 5.5) 1u
        // five casts land (at 1 to 5 s); the first finds no curse to end
        equal 5 o.Counts.["hits:Hex"]
        equal 4 o.Counts.["trigger:Blast"])

    test "culling strike kills at the threshold for the monster's rarity, before the hit" (fun () ->
        // a unique at 5% life or less dies to any culling hit; a normal monster already at 35%
        let cull = [ noIgnite; Mod.flag (Named "CullingStrike") ]
        let unique = { EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 10.0 0.0) with Life = 1000.0; Mods = []; Attack = None }
        let o = Sim.run (Fight.single unique (Actor.make "Player" cull [ fireSpell 100.0 ]) 30.0) 1u
        // 9 hits leave 100 (10%); the 10th leaves 0 anyway: with culling the 10th would be needed only above 5%
        equal (Some 10.0) o.Ttk
        let normal = { unique with Rarity = Normal }
        // 7 hits leave 300 (30% <= 35%): the 8th culls
        equal (Some 8.0) (Sim.run (Fight.single normal (Actor.make "Player" cull [ fireSpell 100.0 ]) 30.0) 1u).Ttk
        equal (Some 10.0) (Sim.run (Fight.single normal (Actor.make "Player" [ noIgnite ] [ fireSpell 100.0 ]) 30.0) 1u).Ttk)

    test "named effects: stats grant them by rule, with the game's numbers" (fun () ->
        let sheet (mods: Mod list) : CharacterSheet =
            { Mods = mods; MainHand = None; Weapons = []; ItemDefences = []; Level = 1; ClassName = ""; ManaFlask = None; LifeFlask = None; Coverage = Coverage.empty }
        // Withered on hit: 5% increased chaos damage taken per stack, up to 10
        let chaos = { Skill.make "Bolt" [ Spell; Tag "Chaos" ] with BaseDamage = Map.ofList [ Chaos, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let rules = Behaviours.keywordRules (sheet [ Mod.base' (Named "Rule:OnHit:Withered:4") 100.0 ]) [ chaos ]
        equal 1 rules.Length
        let o = Sim.run (Fight.single (boss 1e9) { Actor.make "Player" [] [ chaos ] with Rules = rules } 12.5) 1u
        // hits at 1..12 s with 0, 1, ... 10, 10 stacks
        near (100.0 * (12.0 + 0.05 * (0.0 + 1.0 + 2.0 + 3.0 + 4.0 + 5.0 + 6.0 + 7.0 + 8.0 + 9.0 + 10.0 + 10.0))) o.DamageBy.["Bolt (hit)"]
        // "You have Unholy Might": 30% of damage as extra chaos
        let um = Behaviours.keywordMods (sheet [ Mod.flag (Named "Has:UnholyMight") ])
        near 30.0 (Mods.sumBase Ctx.empty [ GainAs(Fire, Chaos) ] (ModDb.ofList um))
        // Critical Weakness: +0.5% crit chance per stack on the enemy
        near 5.0 (Mods.sumBase { Ctx.empty with EnemyStatus = Map.ofList [ "CriticalWeakness", 10 ] } [ CritChance ] (ModDb.ofList um)))

    test "what the attacker does to the enemy: lowered resistance, increased damage taken, faster ailments" (fun () ->
        let frozenRes = Mod.base' (Named "EnemyResist:Cold") -20.0 |> Mod.whenever (EnemyHas "Frozen")
        let cold = { Skill.make "Shard" [ Spell ] with BaseDamage = Map.ofList [ Cold, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let c = { Skill.ctx cold false Map.empty (Map.ofList [ "Frozen", 1 ]) with Crit = false }
        near -20.0 (Hit.effectiveResist c (ModDb.ofList [ frozenRes ]) (enemy []) Cold)
        near 0.0 (Hit.effectiveResist { c with EnemyStatus = Map.empty } (ModDb.ofList [ frozenRes ]) (enemy []) Cold)
        // "Enemies you Curse take 20% increased Damage" adds to a 30% curse-borne increase: x1.5, not x1.56
        let aura = [ "Curse:Hex", 1, [ Mod.inc (DamageTaken AllDamage) 30.0 ] ]
        let mods = [ noIgnite; Mod.inc (Named "EnemyDamageTaken") 20.0 |> Mod.whenever (EnemyHas "Cursed") ]
        let f = { Fight.single (boss 1e9) (Actor.make "Player" mods [ fireSpell 100.0 ]) 1.5 with EnemyAuras = aura }
        near 150.0 (Sim.run f 1u).DamageBy.["Bolt (hit)"]
        // ignite dealing damage 100% faster: twice the damage per second for half the time
        let rng = Rng(1u)
        let hit = { Crit = false; Unmitigated = Map.ofList [ Fire, 1000.0 ]; Dealt = Map.ofList [ Fire, 1000.0 ] }
        let ignite m = Ailments.fromHit rng ctx (ModDb.ofList (Mod.base' (AilmentChance Ignite) 100.0 :: m)) (enemy []) hit |> List.pick (function Dot (Ignite, _, dps, dur, _) -> Some(dps, dur) | _ -> None)
        let dps, dur = ignite []
        let dps2, dur2 = ignite [ Mod.inc (Named "FasterAilments") 100.0 ]
        near (2.0 * dps) dps2
        near (dur / 2.0) dur2)

    test "crossbow: a load of bolts, then a reload" (fun () ->
        // 2 bolts per load, 0.5 s per shot, 1 s reload: two shots every 2 s
        let shot = { Skill.make "Shot" [ Attack; Projectile ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 10.0 ]; BaseTime = 0.5; Clip = 2; ReloadTime = 1.0 }
        let o = Sim.run (Fight.single (boss 1e9) (Actor.make "Player" [ noIgnite ] [ shot ]) 10.25) 1u
        // shots start at 0, 0.5 | 2, 2.5 | 4, 4.5 | 6, 6.5 | 8, 8.5 | 10
        equal 11 o.Counts.["uses:Shot"]
        equal 5 o.Counts.["reload:Shot"])

    test "crossbow: attack speed and reload speed shorten the reload, the cost is per load" (fun () ->
        let shot = { Skill.make "Shot" [ Attack; Projectile ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 10.0 ]; BaseTime = 1.0; Clip = 1; ReloadTime = 3.0; Cost = 40.0 }
        // 50% increased attack speed and 50% increased reload speed add up: reload 3 / 2 = 1.5 s; a shot takes 1 / 1.5 s
        let mods = [ noIgnite; Mod.inc Speed 50.0; Mod.inc (Named "ReloadSpeed") 50.0 ]
        let stats = PlayerStats.basic 100.0 100.0
        let player = { Actor.make "Player" mods [ shot ] with Vitals = Some stats }
        let o = Sim.run (Fight.single { boss 1e9 with Attack = None } player 8.0) 1u
        // the first load is free; shots at 0, 2.1667, 4.3333: two paid loads leave 20 mana, a third can't be paid
        equal 3 o.Counts.["uses:Shot"]
        if o.ManaStarved <= 0.0 then failwith "the third load should have waited for mana")

    test "curse aura: every enemy has it for the whole fight, bosses at half effect" (fun () ->
        let aura = [ "Curse:Flammability", 1, [ Mod.base' (Resist Fire) -40.0 ] ]
        let plain = { Fight.single (boss 1e9) (Actor.make "Player" [ noIgnite ] [ fireSpell 100.0 ]) 1.5 with EnemyAuras = aura }
        near 140.0 (Sim.run plain 1u).DamageBy.["Bolt (hit)"]
        let resistant = { boss 1e9 with Mods = [ Mod.more (Named "CurseEffectOnSelf") -50.0 ] }
        let halved = { Fight.single resistant (Actor.make "Player" [ noIgnite ] [ fireSpell 100.0 ]) 1.5 with EnemyAuras = aura }
        near 120.0 (Sim.run halved 1u).DamageBy.["Bolt (hit)"]
        // mods against cursed enemies count
        let hunter = Actor.make "Player" [ noIgnite; Mod.more (Damage AllDamage) 50.0 |> Mod.whenever (EnemyHas "Cursed") ] [ fireSpell 100.0 ]
        near 210.0 (Sim.run { plain with Player = hunter } 1u).DamageBy.["Bolt (hit)"])

    test "a skill that costs more than the whole mana pool is never used" (fun () ->
        let big = { fireSpell 100.0 with Name = "Big"; Cost = 500.0 }
        let small = { fireSpell 10.0 with Name = "Small"; Cost = 1.0 }
        let stats = { PlayerStats.basic 100.0 100.0 with ManaRegen = 10.0 }
        let player = { Actor.make "Player" [ noIgnite ] [ big; small ] with Vitals = Some stats }
        let o = Sim.run (Fight.single { boss 1e9 with Attack = None } player 5.5) 1u
        equal false (o.Counts.ContainsKey "uses:Big")
        if o.Counts.["uses:Small"] < 5 then failwith "the affordable skill should have been used instead"
        // alone, it is given up on instead of waited for
        let only = { Actor.make "Player" [ noIgnite ] [ big ] with Vitals = Some stats }
        equal 1 (Sim.run (Fight.single { boss 1e9 with Attack = None } only 5.5) 1u).Counts.["cannot afford:Big"])

    test "a skill that can't be paid for yet doesn't block the next one" (fun () ->
        let dear = { fireSpell 100.0 with Name = "Dear"; Cost = 80.0 }
        let cheap = { fireSpell 10.0 with Name = "Cheap"; Cost = 0.0 }
        let stats = { PlayerStats.basic 100.0 100.0 with ManaRegen = 10.0 }
        let player = { Actor.make "Player" [ noIgnite ] [ dear; cheap ] with Vitals = Some stats }
        let o = Sim.run (Fight.single { boss 1e9 with Attack = None } player 20.5) 1u
        // 100 mana + 10 per second over 20 s pays for three casts of Dear; Cheap fills the rest
        equal 3 o.Counts.["uses:Dear"]
        equal 18 o.Counts.["uses:Cheap"]
        near 0.0 o.ManaStarved)

    test "meta energy from melee hits and melee kills" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let nova = { fireSpell 1.0 with Name = "Nova" }
        let onHit = { Meta.simple "On Hit" 1.0 "Nova" with Sources = [ MeleeHits 1.0 ]; EnergyMult = 1e6 }
        let player = { Actor.make "Player" [ noIgnite ] [ swing; nova ] with Rotation = [ Actor.always "Swing" ]; Metas = [ onHit ] }
        let o = Sim.run (Fight.single (boss 1e12) player 5.5) 1u
        equal o.Counts.["hits:Swing"] o.Counts.["trigger:Nova"]
        // a spell is not a melee hit
        let caster = { Actor.make "Player" [ noIgnite ] [ fireSpell 100.0; nova ] with Rotation = [ Actor.always "Bolt" ]; Metas = [ onHit ] }
        equal false ((Sim.run (Fight.single (boss 1e12) caster 5.5) 1u).Counts.ContainsKey "trigger:Nova"))

    test "meta energy from dodge rolls: per unit rolled" (fun () ->
        let roll = { Skill.make "Dodge Roll" [ Tag "DodgeRoll" ] with BaseTime = 0.5; Hits = [] }
        let nova = { fireSpell 100.0 with Name = "Nova" }
        // 37 units per roll at 1 energy per unit: 100 energy every third roll
        let meta = { Meta.simple "On Dodge" 100.0 "Nova" with Sources = [ DodgeRolls 1.0 ] }
        let player = { Actor.make "Player" [ noIgnite ] [ roll; nova ] with Rotation = [ Actor.always "Dodge Roll" ]; Metas = [ meta ] }
        let o = Sim.run (Fight.single (boss 1e9) player 4.75) 1u
        equal 10 o.Counts.["uses:Dodge Roll"]
        equal 3 o.Counts.["trigger:Nova"])

    test "gem stats: a skill's internal name is not read as scope words" (fun () ->
        // Falling Thunder's stats start with "lightning_strike_": neither a lightning nor a Strike restriction
        let ids =
            [ "lightning_strike_damage_+%_final_per_power_charge"; "lightning_strike_damage_+%_final_when_charged"
              "lightning_strike_base_number_of_projectiles_per_power_charge"; "lightning_strike_maximum_action_distance_+_when_empowered"
              "active_skill_base_area_of_effect_radius" ]
        equal (Some "lightning_strike_") (Gems.skillPrefix ids)
        match Gems.gemMeaningWith (Some "lightning_strike_") ids.Head 50.0 with
        | Mods [ m ] ->
            equal (Damage AllDamage) m.Stat
            equal More m.Kind
            equal [ PerStack("PowerCharge", 1.0) ] m.Conds
        | other -> failwithf "unexpected meaning %A" other)

    DataTests.run test near (fun a b -> equal a b)

    printfn "%d passed, %d failed" passed failures
    if failures = 0 then 0 else 1
