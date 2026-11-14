/// Stats that were read and not played, or not read at all, one family after the other:
/// what each is mapped to, and that the fight does with it what the game's text says.
/// Expected values are worked out by hand from the stat's own number.
module PoEtry.StatTests

open PoEtry

let private dir = __SOURCE_DIRECTORY__ + "/../../data/game/"
let private db = lazy (GameDb.load (fun f -> System.IO.File.ReadAllText(dir + f)))
let private treeJson = lazy (System.IO.File.ReadAllText(dir + "tree.json"))

let private blank : PobBuild =
    { Level = 1; ClassName = "Witch"; Ascendancy = ""; Items = []; TreeNodes = []; TreeNodesSet1 = []; TreeNodesSet2 = []
      Groups = []; MainGroup = 1; Config = Map.empty; TreeOverrides = Map.empty; ExtraLines = [] }

let private gem name = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = false; StatSet = 1; Minion = None }
let private support name = { gem name with Level = 1; IsSupport = true }
let private group gems = { Label = ""; Slot = ""; Enabled = true; Gems = gems }

/// The fast engine gives the reference's outcome for this fight, for these seeds, and tells
/// the same of every event and hit; the fight must compile.
let private same (fight: Fight) (seeds: uint32 list) =
    match Flat.compile fight with
    | Error why -> failwithf "does not compile: %s" why
    | Ok program ->
        for seed in seeds do
            match FlatTests.check fight seed with
            | Some (Error d) -> failwithf "seed %d: %s" seed d
            | _ -> ()
        match seeds with
        | seed :: _ ->
            let parted, where = FlatTests.divergence fight program seed
            if parted || not (where.EndsWith "told the same") then failwithf "seed %d: %s" seed where
        | [] -> ()

let private seeds = [ 1u .. 12u ]

let run (test: string -> (unit -> unit) -> unit) (near: float -> float -> unit) (equal: obj -> obj -> unit) =
    let load (b: PobBuild) (o: Options) = Setup.loadWith db.Value treeJson.Value b o
    let mods id v = match StatMap.meaning id v with Mods ms -> ms | m -> failwithf "%s -> %A" id m
    let count (out: Outcome) k = Map.tryFind k out.Counts |> Option.defaultValue 0

    // ---- what a few stats are, where nothing new had to be played ----

    test "stats: cooldown recovery of spells, what empowered attacks gain, Onslaught on a weapon's killing hits" (fun () ->
        equal (box [ Mod.inc CooldownRecovery 20.0 |> Mod.whenever (SkillIs Spell) ]) (box (mods "base_spell_cooldown_speed_+%" 20.0))
        equal
            (box [ Mod.base' (GainAs(Physical, Fire)) 30.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "EmpoweredAttacks") ])
            (box (mods "empowered_attack_physical_damage_%_to_gain_as_fire" 30.0))
        // the weapon's own line: a rule for attacks, 25% on a kill, Onslaught for 4 s (20% increased skill speed)
        let b = { blank with Items = [ "Weapon 1", "Rarity: Rare\nTest Staff\nWrapped Quarterstaff\n--------\n25% chance to gain Onslaught on Killing Hits with this Weapon\n" ] }
        let c = load b Options.defaults
        let player = Setup.player c Options.defaults [] None
        match player.Rules |> List.tryPick (fun r -> match r.Hook, r.Reaction with OnKill, GrantBuff ("Onslaught", d, _, ms) -> Some(r.Filter, r.Chance, d, ms) | _ -> None) with
        | Some (filter, chance, seconds, ms) ->
            equal (box (WithTag Attack)) (box filter)
            near 25.0 chance
            near 4.0 seconds
            equal (box [ Speed, 20.0 ]) (box (ms |> List.map (fun m -> m.Stat, m.Value)))
        | None -> failwith "no Onslaught rule")

    test "stats: slowing potency of debuffs on you multiplies the Chill a monster's hit leaves" (fun () ->
        let chill lines = (load { blank with ExtraLines = lines } Options.defaults).Vitals.SelfEffect.["Chill"]
        near 1.0 (chill [])
        near 0.7 (chill [ "30% reduced Slowing Potency of Debuffs on You" ])
        // with "reduced Effect of Chill on you": each a multiplier of its own, 0.7 x 0.8
        near 0.56 (chill [ "30% reduced Slowing Potency of Debuffs on You"; "20% reduced Effect of Chill on you" ])
        // the other ailments are no slows
        near 1.0 (load { blank with ExtraLines = [ "30% reduced Slowing Potency of Debuffs on You" ] } Options.defaults).Vitals.SelfEffect.["Shock"])

    test "Ice Bite: freezing an enemy with the supported skill gives 30% of damage as extra cold for 6 s" (fun () ->
        let b = { blank with Groups = [ group [ gem "Ice Nova"; support "Ice Bite II" ] ] }
        let c = load b Options.defaults
        match Setup.pressed c Options.defaults "Ice Nova" with
        | Some p ->
            let bites = p.Rules |> List.choose (fun r -> match r.Hook, r.Filter, r.Reaction with OnInflict Freeze, Called n, GrantBuff ("Ice Bite", d, 1, ms) -> Some(n, d, ms) | _ -> None)
            // the nova itself and its cold-infused form
            equal (box [ "Ice Nova"; "Ice Nova infused" ]) (box (bites |> List.map (fun (n, _, _) -> n)))
            for _, seconds, ms in bites do
                near 6.0 seconds
                equal (box [ for t in DamageType.all -> GainAs(t, Cold), 30.0 ]) (box (ms |> List.map (fun m -> m.Stat, m.Value)))
        | None -> failwith "Ice Nova not built"
        // it is no buff to keep up by pressing the skill: the skill is played as it would be without
        match Setup.auto c Options.defaults "Ice Nova" |> List.rev |> List.tryHead with
        | Some (Spam "Ice Nova") -> ()
        | other -> failwithf "Ice Nova is played as %A" (other |> Option.map (function Spam g -> "spam " + g | KeepBuff g -> "keep buff " + g | _ -> "?"))
        // without the support: no such rule
        let plain = load { blank with Groups = [ group [ gem "Ice Nova" ] ] } Options.defaults
        match Setup.pressed plain Options.defaults "Ice Nova" with
        | Some p -> equal (box false) (box (p.Rules |> List.exists (fun r -> match r.Reaction with GrantBuff ("Ice Bite", _, _, _) -> true | _ -> false)))
        | None -> failwith "Ice Nova not built"
        // in a fight against an enemy that freezes at once the buff is there for the next casts:
        // cold damage gains 30% of itself (base 100: 130)
        let weak = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13; Mods = e.Mods @ [ Mod.more PoiseThreshold -99.999 ] })
        let o = { Options.defaults with MaxTime = 20.0; Immortal = true; Start = [ "ColdInfusion", 0 ] }
        let fightOf (c: Loaded) = Setup.fight c o (Setup.player c o [ Spam "Ice Nova" ] None) weak
        let dealt (c: Loaded) =
            let out = Sim.run (fightOf c) 3u
            (out.DamageBy |> Map.toList |> List.sumBy snd) / float (max 1 (count out "hits:Ice Nova"))
        let withBite, without = dealt c, dealt plain
        if withBite < without * 1.2 || withBite > without * 1.31 then failwithf "with Ice Bite %.0f a hit, without %.0f" withBite without
        same (fightOf c) seeds)

    test "a rule for a bar that fills: once when the enemy is frozen by the hit, not while it stays frozen" (fun () ->
        // 100 cold a hit against next to no poise: the first hit (at 0.5 s) freezes for 4 s, and
        // while the enemy is frozen no hit freezes it again: one freeze in 4.2 s, one buff
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let enemy = { EnemyDef.atLevel 83 1e9 with Mods = [ Mod.more PoiseThreshold -99.999 ] }
        let player =
            { Actor.make "Player" [] [ bolt ] with
                Rules = [ { Hook = OnInflict Freeze; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("Bitten", 2.75, 5, [ Mod.more (Damage AllDamage) 100.0 ]) } ] }
        let fight = Fight.single enemy player 4.2
        let out = Sim.run fight 1u
        equal (box 1) (box (count out "inflict:Freeze"))
        // 8 hits of 100; the five that land in the buff's 2.75 s (at 1.0 to 3.0 s) deal double
        near (100.0 * 8.0 + 100.0 * 5.0) (out.DamageBy |> Map.toList |> List.sumBy snd)
        same fight seeds
        // and over a longer time, with every freeze that follows
        same { fight with MaxTime = 30.0 } seeds)

    test "Falling Thunder: no projectiles without power charges, three for each one consumed" (fun () ->
        let staff = (Items.parse db.Value "Rarity: Normal\nWrapped Quarterstaff\n--------\n").Weapon
        match Gems.skill db.Value ModDb.empty staff { Gem = "Falling Thunder"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, cov) ->
            match s.Parts |> List.tryPick (fun p -> match p.Delivery with Projectiles shape -> Some shape | _ -> None) with
            | Some shape ->
                equal (box 0) (box shape.Count)
                equal (box (Some("PowerCharge", 3))) (box shape.PerCharge)
            | None -> failwith "no projectile part"
            // the stats about it are read, none is left over
            for id in [ "zero_projectiles_if_no_power_charges"; "active_skill_projectile_speed_+%_variation_final"; "lightning_strike_maximum_action_distance_+_when_empowered" ] do
                if cov.Unmapped |> List.exists (fun (k, _) -> k = id) || cov.Unsimulated |> List.exists (fun (k, _) -> k = id) then failwithf "%s is not understood" id
        | Error e -> failwith e)

    // ---- charms, an Ignite of one's own, how fast buffs and debuffs run out ----

    test "charms: a chance to not consume charges is rolled at every use" (fun () ->
        // a brute that hits with fire once a second; a charm for fire hits that lasts 0.3 s and
        // has the charges for two uses, with nothing to fill it again
        let brute =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Fire, 10.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let charm = { Name = "ruby"; Trigger = "Hit:Fire"; Buff = "Charm1"; Prevents = false; Duration = 0.3; ChargesPerUse = 40.0; MaxCharges = 80.0; ChargesPerSecond = 0.0; ChargeOnKillChance = 0.0; ChargeGainMult = 1.0; LifeOnUse = 0.0; ManaOnUse = 0.0; StartsRecharge = false }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, { Min = 1.0; Max = 2.0 } ] }
        let fight keep =
            let player =
                { Actor.make "Player" [ Mod.base' (Named "CharmKeepCharges") keep ] [ bolt ] with
                    Vitals = Some { PlayerStats.basic 1e6 0.0 with AilmentThreshold = 1e9; StunThreshold = 1e9 }
                    Charms = [ charm ] }
            Fight.single brute player 10.5
        let hits = count (Sim.run (fight 0.0) 1u) "enemy:hit"
        if hits < 9 then failwithf "%d hits" hits
        // without: two uses, then it is empty
        let plain = Sim.run (fight 0.0) 1u
        equal (box 2) (box (count plain "charm:ruby"))
        equal (box 0) (box (count plain "charm:kept charges"))
        // 100%: it is used at every hit and never loses a charge
        let always = Sim.run (fight 100.0) 1u
        equal (box hits) (box (count always "charm:ruby"))
        equal (box hits) (box (count always "charm:kept charges"))
        // 50%: each use that keeps its charges allows one more; two are lost in the end
        for seed in seeds do
            let half = Sim.run (fight 50.0) seed
            let used, kept = count half "charm:ruby", count half "charm:kept charges"
            if used - kept > 2 || (used < hits && used - kept <> 2) then failwithf "seed %d: %d uses, %d kept, %d hits" seed used kept hits
        same (fight 0.0) seeds
        same (fight 50.0) seeds
        same (fight 100.0) seeds
        // from the character: the line, and "reduced Charm Charges used"
        let c = load { blank with ExtraLines = [ "12% chance for Charms you use to not consume Charges" ] } Options.defaults
        near 12.0 (Mods.sumBase Ctx.empty [ Named "CharmKeepCharges" ] (ModDb.ofList c.Sheet.Mods)))

    test "an Ignite of one's own on a critical hit: a share of life and energy shield a second, as fire damage" (fun () ->
        // 1000 life and 500 energy shield, 10% a second: 150 a second for an Ignite's 4 s. A
        // spell that always crits lands every 0.5 s from 0.5 s on: burning from then to the end
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5; BaseCrit = 100.0 }
        let enemy = { EnemyDef.atLevel 83 1e9 with Mods = [] }
        let fight (vitals: PlayerStats) =
            let player =
                { Actor.make "Player" [] [ bolt ] with
                    Vitals = Some vitals
                    Rules = [ { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = IgniteSelf 10.0 } ] }
            Fight.single enemy player 4.5
        let basic = { PlayerStats.basic 1000.0 0.0 with EnergyShield = 500.0 }
        let out = Sim.run (fight basic) 1u
        near (150.0 * 4.0) out.DamageTaken
        // half of it with 50% fire resistance; "40% reduced Effect of Ignite on you"; avoided for sure
        near (75.0 * 4.0) (Sim.run (fight { basic with Resist = Map.add Fire 50.0 basic.Resist }) 1u).DamageTaken
        near (90.0 * 4.0) (Sim.run (fight { basic with SelfEffect = Map.ofList [ "Ignite", 0.6 ] }) 1u).DamageTaken
        near 0.0 (Sim.run (fight { basic with Avoid = Map.ofList [ "Ignite", 100.0 ] }) 1u).DamageTaken
        // it stops when the crits do: one crit, 4 s of burning, and then none
        let once = { fight basic with Player = { (fight basic).Player with Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.25 } ] }; MaxTime = 8.0 }
        near (150.0 * 4.0) (Sim.run once 1u).DamageTaken
        // "Debuffs on you expire 100% faster": 2 s
        near (150.0 * 2.0) (Sim.run { once with Player = { once.Player with Vitals = Some { basic with SelfDuration = Map.ofList [ "Ignite", 0.5 ] } } } 1u).DamageTaken
        same (fight basic) seeds
        same (fight { basic with Avoid = Map.ofList [ "Ignite", 40.0 ]; Resist = Map.add Fire 30.0 basic.Resist }) seeds
        same once seeds
        // the character's line becomes that rule
        let c = load { blank with ExtraLines = [ "Become Ignited when you deal a Critical Hit, taking 15% of your maximum Life and Energy Shield as Fire Damage per second" ] } Options.defaults
        let player = Setup.player c Options.defaults [] None
        equal (box [ OnCrit, 15.0 ]) (box (player.Rules |> List.choose (fun r -> match r.Reaction with IgniteSelf p -> Some(r.Hook, p) | _ -> None))))

    test "buffs on you expire faster: their time is shorter; debuffs on you likewise" (fun () ->
        let staff = "Weapon 1", "Rarity: Rare\nTest Staff\nWrapped Quarterstaff\n--------\n100% chance to gain Onslaught on Killing Hits with this Weapon\n"
        let onslaught lines =
            let c = load { blank with Items = [ staff ]; ExtraLines = lines } Options.defaults
            (Setup.player c Options.defaults [] None).Rules |> List.pick (fun r -> match r.Reaction with GrantBuff ("Onslaught", d, _, _) -> Some d | _ -> None)
        near 4.0 (onslaught [])
        // 25% faster: 4 / 1.25
        near 3.2 (onslaught [ "Buffs on you expire 25% faster" ])
        // Ice Bite's 6 s: 4.8
        let c = load { blank with Groups = [ group [ gem "Ice Nova"; support "Ice Bite II" ] ]; ExtraLines = [ "Buffs on you expire 25% faster" ] } Options.defaults
        let player = Setup.player c Options.defaults [ Spam "Ice Nova" ] None
        near 4.8 (player.Rules |> List.pick (fun r -> match r.Reaction with GrantBuff ("Ice Bite", d, _, _) -> Some d | _ -> None))
        // debuffs: the ailments a monster leaves on the player last half as long at 100% faster
        let v = (load { blank with ExtraLines = [ "Debuffs on you expire 100% faster" ] } Options.defaults).Vitals
        near 0.5 v.SelfDuration.["Ignite"]
        near 0.5 v.SelfDuration.["Chill"])

    // ---- Presence ----

    test "Presence: an enemy has its auras from when its body reaches into it" (fun () ->
        // a monster 10 m away, 0.5 m in radius, walks up at 2 m a second; an aura in the
        // Presence doubles what it takes. 100 chaos damage every 0.5 s
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let walker =
            { EnemyDef.make Rarity.defaults 70 Normal "walker" (Vec.make 100.0 0.0) with
                Life = 1e9; Mods = []; Size = 5.0; Speed = 20.0
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 1.0 ]; Accuracy = 100.0; Range = 6.0; CritChance = 0.0; Melee = true } }
        let player = { Actor.make "Player" [] [ bolt ] with Vitals = Some(PlayerStats.basic 1e6 0.0) }
        let fight presence =
            { Fight.single walker player 5.2 with Presence = presence; PresenceAuras = [ "Weakened", 1, [ Mod.inc (DamageTaken AllDamage) 100.0 ] ] }
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        // 4 m: its edge is inside at 4.5 m, after 2.75 s; the hits at 3.0 to 5.0 s deal double
        near (5.0 * 100.0 + 5.0 * 200.0) (dealt (fight 40.0))
        // 8 m: inside at 8.5 m, after 0.75 s; all but the first hit
        near (1.0 * 100.0 + 9.0 * 200.0) (dealt (fight 80.0))
        // everywhere (a fight that names no Presence): every hit; without an aura: none
        near (10.0 * 200.0) (dealt (fight infinity))
        near (10.0 * 100.0) (dealt { fight 40.0 with PresenceAuras = [] })
        // the time in the Presence is counted like any status
        near (5.2 - 3.0) (Sim.run (fight 40.0) 1u).Uptime.["Weakened"]
        for presence in [ 40.0; 80.0; infinity ] do
            same (fight presence) seeds
        // a debuff of the aura's name put on by a hit takes its place for its time; then the aura is back
        let lapsing =
            { fight 80.0 with
                Player = { player with Rules = [ { Hook = OnHit; Filter = AnySkill; Chance = 30.0; Reaction = ApplyDebuff("Weakened", 0.7, 1, [ Mod.inc (DamageTaken AllDamage) 30.0 ]) } ] }
                MaxTime = 12.0 }
        same lapsing seeds)

    test "Presence: 4 m, wider with its area, twice with a doubled radius; what the enemies in it have" (fun () ->
        let radius lines = Behaviours.presenceRadius (load { blank with ExtraLines = lines } Options.defaults).Sheet
        near 40.0 (radius [])
        // 50% increased area: the radius by the square root, cut off like a skill's (1.22 x 40)
        near 48.0 (radius [ "50% increased Presence Area of Effect" ])
        near 80.0 (radius [ "Presence Radius is doubled" ])
        near 96.0 (radius [ "50% increased Presence Area of Effect"; "Presence Radius is doubled" ])
        // 50% reduced: 0.7 x 40
        near 28.0 (radius [ "50% reduced Presence Area of Effect" ])
        let fightOf gems lines =
            let o = Options.defaults
            let c = load { blank with Groups = [ group gems ]; ExtraLines = [ "+200 to Spirit" ] @ lines } o
            Setup.fight c o (Setup.player c o [] None) (Encounter.boss 83 Rarity.defaults 30.0)
        let f = fightOf [ gem "Overwhelming Presence" ] [ "Enemies in your Presence are Blinded"; "Enemies in your Presence have Exposure"; "Enemies in your Presence have -10% to Fire Resistance"; "Presence Radius is doubled" ]
        near 80.0 f.Presence
        let names = f.PresenceAuras |> List.map (fun (n, _, _) -> n)
        for n in [ "Overwhelming Presence"; "Blinded"; "Exposure:Fire"; "Exposure:Cold"; "Exposure:Lightning"; "Presence: Fire Resistance" ] do
            if not (List.contains n names) then failwithf "no %s among %A" n names
        let modsOf n = f.PresenceAuras |> List.pick (fun (k, _, ms) -> if k = n then Some(ms |> List.map (fun m -> m.Stat, m.Kind, m.Value)) else None)
        equal (box [ Resist Cold, Base, -20.0 ]) (box (modsOf "Exposure:Cold"))
        equal (box [ Resist Fire, Base, -10.0 ]) (box (modsOf "Presence: Fire Resistance"))
        // Overwhelming Presence: the monsters' thresholds are lower, and not on the monster itself any more
        if not (modsOf "Overwhelming Presence" |> List.exists (fun (st, kind, v) -> st = AilmentThreshold && kind = More && v < 0.0)) then failwithf "%A" (modsOf "Overwhelming Presence")
        // the auras of gems are the Presence's too: Malice's Critical Weakness
        // (a stack every so often, each for 4 s: the gem's numbers at its level, at most 20 stacks)
        let malice = fightOf [ gem "Malice" ] []
        let raw = Gems.rawStats db.Value ModDb.empty { Gem = "Malice"; Level = 20; Quality = 0; StatSet = 1; Supports = [] }
        let every = raw |> List.pick (fun (k, v) -> if k = "critical_chance_against_aura_apply_brittle_every_x_ms" then Some v else None)
        equal (box [ "CriticalWeakness", min 20 (int (ceil (4000.0 / every))) ]) (box (malice.PresenceAuras |> List.map (fun (n, k, _) -> n, k)))
        equal (box true) (box malice.EnemyAuras.IsEmpty)
        // a whole fight with them is the same in both engines
        same { f with MaxTime = 6.0 } [ 1u; 2u; 3u ])

    test "Presence: enemies in it resist elemental damage by their lowest resistance" (fun () ->
        // 60% fire, 10% cold, 30% lightning resistance; 100 fire a hit, six hits
        let enemy = { EnemyDef.atLevel 83 1e9 with Mods = [ Mod.base' (Resist Fire) 60.0; Mod.base' (Resist Cold) 10.0; Mod.base' (Resist Lightning) 30.0 ] }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let noIgnite = Mod.more (AilmentChance Ignite) -100.0
        let fight (ms: Mod list) lowest =
            { Fight.single enemy (Actor.make "Player" ms [ bolt ]) 3.2 with
                PresenceAuras = (if lowest then [ "Presence: Lowest Resistance", 1, [ Mod.base' (Named "ResistByLowest") 1.0 ] ] else []) }
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near (6.0 * 40.0) (dealt (fight [ noIgnite ] false))
        // by the lowest, cold's 10%
        near (6.0 * 90.0) (dealt (fight [ noIgnite ] true))
        // penetration of the damage's own type then applies to that (and stops at zero)
        near (6.0 * 100.0) (dealt (fight [ noIgnite; Mod.base' (Penetration Fire) 20.0 ] true))
        // the Ignite of such a hit burns against the lowest too: 20 a second for 4 s from the
        // first hit (0.5 s), 2.7 s of it in the fight: x 0.9 in place of x 0.4
        near (6.0 * 90.0 + 2.7 * 20.0 * 0.9) (dealt (fight [ Mod.base' (AilmentChance Ignite) 100.0 ] true))
        near (6.0 * 40.0 + 2.7 * 20.0 * 0.4) (dealt (fight [ Mod.base' (AilmentChance Ignite) 100.0 ] false))
        same (fight [] true) seeds
        same (fight [ Mod.base' (Penetration Fire) 20.0; Mod.base' (AilmentChance Ignite) 100.0 ] true) seeds
        same (fight [] false) seeds
        // the character's line is that aura
        let o = Options.defaults
        let c = load { blank with ExtraLines = [ "Enemies in your Presence Resist Elemental Damage based on their Lowest Resistance" ] } o
        let f = Setup.fight c o (Setup.player c o [] None) (Encounter.boss 83 Rarity.defaults 30.0)
        equal (box [ "Presence: Lowest Resistance", [ Named "ResistByLowest", 1.0 ] ]) (box (f.PresenceAuras |> List.map (fun (n, _, ms) -> n, ms |> List.map (fun m -> m.Stat, m.Value)))))

    test "projectile speed as projectile damage: its increases, and a share of a spell's increased cast speed" (fun () ->
        let damageFromSpeed supports lines =
            let c = load { blank with Groups = [ group (gem "Spark" :: (supports |> List.map support)) ]; ExtraLines = lines } Options.defaults
            match Setup.pressed c Options.defaults "Spark" with
            | Some p -> p.Skill.Mods |> List.filter (fun m -> m.Source = "projectile speed") |> List.map (fun m -> m.Stat, m.Kind, m.Value)
            | None -> failwith "Spark not built"
        // without the support the speed is nothing
        equal (box ([]: (Stat * ModKind * float) list)) (box (damageFromSpeed [] [ "30% increased Projectile Speed" ]))
        // Projectile Acceleration III: 30% increased speed is 30% increased damage (its own 40% more speed is no increase)
        equal (box [ Damage AllDamage, Inc, 30.0 ]) (box (damageFromSpeed [ "Projectile Acceleration III" ] [ "30% increased Projectile Speed" ]))
        equal (box ([]: (Stat * ModKind * float) list)) (box (damageFromSpeed [ "Projectile Acceleration III" ] []))
        // with Vilenta's Propulsion 75% of increased cast speed counts as projectile speed: 30 + 0.75 x 20
        equal (box [ Damage AllDamage, Inc, 45.0 ]) (box (damageFromSpeed [ "Projectile Acceleration III"; "Vilenta's Propulsion" ] [ "30% increased Projectile Speed"; "20% increased Cast Speed" ]))
        // and in a fight: a hit deals 1.3 times as much
        let dealt lines =
            let o = { Options.defaults with MaxTime = 10.0; Immortal = true }
            let c = load { blank with Groups = [ group [ gem "Spark"; support "Projectile Acceleration III" ] ]; ExtraLines = lines } o
            let out = Sim.run (Setup.fight c o (Setup.player c o [ Spam "Spark" ] None) (Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 }))) 1u
            (Map.tryFind "Spark (hit)" out.DamageBy |> Option.defaultValue 0.0) / float (max 1 (count out "hits:Spark"))
        let fast, plain = dealt [ "30% increased Projectile Speed"; "Never deal Critical Hits" ], dealt [ "Never deal Critical Hits" ]
        if abs (fast / plain - 1.3) > 0.06 then failwithf "%.1f a hit with the speed, %.1f without" fast plain)

    test "stats: penetration of attacks, mace slams' aftershocks, what holds within 2 m or against marked or poisoned enemies" (fun () ->
        equal (box [ for t in DamageType.elemental -> Mod.base' (Penetration t) 15.0 |> Mod.whenever (SkillIs Attack) ]) (box (mods "base_attack_damage_penetrates_elemental_resist_%" 15.0))
        equal (box [ Mod.base' (Named "AftershockChance") 25.0 |> Mod.whenever (SkillIs Slam) |> Mod.whenever (SkillIs Mace) ]) (box (mods "your_mace_slam_aftershock_chance_%" 25.0))
        equal (box [ Mod.inc CritBonus 20.0 |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (EnemyHas "Close") ]) (box (mods "critical_hit_damage_bonus_+%_vs_enemies_within_2m_distance" 20.0))
        equal (box [ Mod.inc (Damage AllDamage) 20.0 |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (EnemyHas "Close") ]) (box (mods "projectile_damage_+%_vs_enemies_within_2m_distance" 20.0))
        equal (box [ Mod.inc (AilmentMagnitude None) 20.0 |> Mod.whenever (EnemyHas "Marked") ]) (box (mods "ailment_magnitude_+%_vs_marked_enemies" 20.0))
        equal (box [ Mod.base' (AilmentChance Maim) 30.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (EnemyHas "Poisoned") ]) (box (mods "maim_on_hit_%_vs_poisoned_enemies" 30.0))
        // a random element: 6% is 2% as each, of every type
        equal (box 15) (box (mods "non_skill_base_all_damage_%_to_gain_as_random_element" 6.0).Length)
        near 2.0 (mods "non_skill_base_all_damage_%_to_gain_as_random_element" 6.0).Head.Value
        // within 2 m: a projectile at an enemy 1 m off deals the more, at one 5 m off not
        let bolt = { Skill.make "Bolt" [ Spell; Projectile ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let fight distance =
            let enemy = { EnemyDef.atLevel 83 1e9 with Mods = []; Position = Vec.make distance 0.0; Size = 5.0 }
            Fight.single enemy (Actor.make "Player" (mods "projectile_damage_+%_vs_enemies_within_2m_distance" 50.0) [ bolt ]) 3.2
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near (6.0 * 150.0) (dealt (fight 15.0))
        near (6.0 * 100.0) (dealt (fight 50.0))
        same (fight 15.0) seeds
        // a character's chance of an additional Remnant counts for its skills' remnants
        let c = load { blank with Groups = [ group [ gem "Frost Bomb" ] ]; ExtraLines = [ "15% chance for Remnants you pick up to count as picking up an additional Remnant" ] } Options.defaults
        match Setup.pressed c Options.defaults "Frost Bomb" with
        | Some p -> equal (box [ 15.0 ]) (box (p.Rules |> List.choose (fun r -> match r.Reaction with DropRemnant (_, _, extra) -> Some extra | _ -> None)))
        | None -> failwith "Frost Bomb not built")

    test "knockback: a hit pushes a monster away from the player, by the skill's distance or a metre; a boss stays" (fun () ->
        let bolt (ms: Mod list) = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5; BaseCrit = 0.0; Mods = ms }
        // a monster that does not walk, 3 m away (its body 0.5 m): six hits in 3.2 s
        let dummy rarity = { EnemyDef.make Rarity.defaults 70 rarity "dummy" (Vec.make 30.0 0.0) with Life = 1e9; Mods = []; Size = 5.0; Attack = None }
        let fight rarity ms = Fight.single (dummy rarity) (Actor.make "Player" [] [ bolt ms ]) 3.2
        let ends f =
            let out = Sim.shown f 1u
            let at = out.Shown |> List.choose (function Moved (_, _, p) -> Some(Vec.length p) | _ -> None) |> List.tryLast |> Option.defaultValue 30.0
            at, count out "knockback"
        let knocks = Mod.base' (Named "KnockbackChance") 100.0
        // no knockback: it stays; with it: a metre a hit
        equal (box (30.0, 0)) (box (ends (fight Normal [])))
        equal (box (90.0, 6)) (box (ends (fight Normal [ knocks ])))
        // 50% increased distance: 1.5 m a hit; the skill's own 400 (4 m) in place of the metre
        equal (box (120.0, 6)) (box (ends (fight Normal [ knocks; Mod.inc (Named "KnockbackDistance") 50.0 ])))
        equal (box (270.0, 6)) (box (ends (fight Rare [ knocks; Mod.base' (Named "KnockbackBase") 400.0 ])))
        // a boss is not moved; a skill that cannot knock back does not
        equal (box (30.0, 0)) (box (ends (fight Unique [ knocks ])))
        equal (box (30.0, 0)) (box (ends (fight Normal [ knocks; Mod.flag (Named "CannotKnockback") ])))
        // by how close it is: 50% more at no distance, 90% less from 2 m on (its edge is 2.5 m away): 0.1 m a hit
        let scaled = [ knocks; Mod.base' (Named "KnockbackLimit") 20.0; Mod.base' (Named "KnockbackAtMin") 50.0; Mod.base' (Named "KnockbackAtMax") -90.0 ]
        let far, n = ends (fight Normal scaled)
        near 36.0 far
        equal (box 6) (box n)
        // only on a critical hit with a quarterstaff: a passive's line on a spell does nothing
        equal (box [ Mod.base' (Named "KnockbackChance") 100.0 |> Mod.whenever IsCrit |> Mod.whenever (SkillIs Quarterstaff) ]) (box (mods "knockback_on_crit_with_quarterstaff" 1.0))
        equal (box (30.0, 0)) (box (ends (fight Normal (mods "knockback_on_crit_with_quarterstaff" 1.0))))
        equal (box [ Mod.inc (Named "KnockbackDistance") 20.0 ]) (box (mods "knockback_distance_+%" 20.0))
        // Ice Nova's own numbers are read from the gem
        match Gems.skill db.Value ModDb.empty None { Gem = "Ice Nova"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, _) ->
            let value n = s.Mods |> List.sumBy (fun m -> if m.Stat = Named n then m.Value else 0.0)
            equal (box (100.0, 20.0, 50.0, -90.0)) (box (value "KnockbackChance", value "KnockbackLimit", value "KnockbackAtMin", value "KnockbackAtMax"))
        | Error e -> failwith e
        // a monster that walks and hits: pushed back a metre every half second, it has to walk
        // up again and hits less often
        let brute =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 12.0 0.0) with
                Life = 1e9; Mods = []; Size = 5.0; Speed = 12.0
                Attack = Some { Interval = 0.4; Damage = Map.ofList [ Physical, 10.0 ]; Accuracy = 1e9; Range = 6.0; CritChance = 0.0; Melee = true } }
        let melee ms = Fight.single brute { Actor.make "Player" [] [ bolt ms ] with Vitals = Some { PlayerStats.basic 1e7 0.0 with StunThreshold = 1e12 } } 20.0
        let hits ms = count (Sim.run (melee ms) 1u) "enemy:hit"
        if not (hits [ knocks ] < hits [] * 3 / 4) then failwithf "%d hits taken with knockback, %d without" (hits [ knocks ]) (hits [])
        same (melee [ knocks ]) seeds
        same (melee [ Mod.base' (Named "KnockbackChance") 40.0; Mod.inc (Named "KnockbackDistance") 30.0 ]) seeds
        same (melee scaled) seeds
        same (fight Normal [ knocks ]) seeds)

    test "a monster's movement speed: slower by what its debuffs say; an Aftershock's hits can be told apart" (fun () ->
        // the walker of the Presence test (10 m away, 2 m a second), slowed to half by the first
        // hit (0.5 s, at 9 m): it is inside a Presence of 4.1 m (its edge at 4.6 m) after 4.9 s
        // in place of 2.7 s
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let walker =
            { EnemyDef.make Rarity.defaults 70 Normal "walker" (Vec.make 100.0 0.0) with
                Life = 1e9; Mods = []; Size = 5.0; Speed = 20.0
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 1.0 ]; Accuracy = 100.0; Range = 6.0; CritChance = 0.0; Melee = true } }
        let fight (rules: Rule list) =
            let player = { Actor.make "Player" [] [ bolt ] with Vitals = Some(PlayerStats.basic 1e6 0.0); Rules = rules }
            { Fight.single walker player 7.2 with Presence = 41.0; PresenceAuras = [ "Weakened", 1, [ Mod.inc (DamageTaken AllDamage) 100.0 ] ] }
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        let slows = { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = ApplyDebuff("Jagged Ground", 100.0, 1, [ Mod.more (Named "MovementSpeed") -50.0 ]) }
        // 14 hits: unslowed, those from 3.0 s on (nine) are in the Presence; slowed, those from 5.0 s on (five)
        near (5.0 * 100.0 + 9.0 * 200.0) (dealt (fight []))
        near (9.0 * 100.0 + 5.0 * 200.0) (dealt (fight [ slows ]))
        same (fight [ slows ]) seeds
        // a slam whose Aftershock always comes: what is only for an Aftershock's hits happens with
        // the second strike of a use, not the first. Here: the enemy takes double from then on
        let slam = { Skill.make "Slam" [ Attack; Melee; Slam ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 1.0; BaseCrit = 0.0 }
        let shaken = { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = IfBuff("Aftershock", ApplyDebuff("Shaken", 100.0, 1, [ Mod.inc (DamageTaken AllDamage) 100.0 ])) }
        let slamming chance =
            let player = { Actor.make "Player" [ Mod.base' (Named "AftershockChance") chance ] [ slam ] with Rules = [ shaken ] }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = [] } player 3.2
        // three uses: 100 + 100, then 200 + 200 twice
        near 1000.0 (dealt (slamming 100.0))
        near 300.0 (dealt (slamming 0.0))
        same (slamming 100.0) seeds
        same (slamming 40.0) seeds
        // a slam's area lies on the enemy aimed at, and so does its Aftershock's: where the one
        // lands beside the other (Falling Thunder's half metre) is nothing the geometry knows
        equal (box Structural) (box (StatMap.meaning "cascade_aftershock_additional_offset_distance" -5.0))
        let onTarget = { slam with Delivery = AreaAtTarget 11.0 }
        let far = { Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Position = Vec.make 35.0 0.0; Size = 5.0 } (Actor.make "Player" [ Mod.base' (Named "AftershockChance") 100.0 ] [ onTarget ]) 3.2 with Reach = 20.0 }
        near 600.0 (dealt far))

    test "Jagged Ground: what makes it, how long it lies, how much it slows, what enemies in it take; curses that hinder" (fun () ->
        let mace = "Weapon 1", "Rarity: Normal\nPlated Mace\n--------\n"
        let rulesOf gems lines name =
            let c = load { blank with Items = [ mace ]; Groups = [ group gems ]; ExtraLines = lines } Options.defaults
            match Setup.pressed c Options.defaults name with
            | Some p -> p.Rules
            | None -> failwithf "%s not built" name
        let ground (rules: Rule list) =
            rules |> List.choose (fun r ->
                let rec find reaction gate =
                    match reaction with
                    | ApplyDebuff ("Jagged Ground", d, _, ms) -> Some(r.Hook, gate, d, ms |> List.map (fun m -> m.Stat, m.Kind, m.Value))
                    | IfBuff (b, next) -> find next b
                    | _ -> None
                find r.Reaction "")
        // Earthquake's own: every hit leaves the enemy in it for 4 s, at twice the 20% slow
        match ground (rulesOf [ gem "Earthquake" ] [] "Earthquake") with
        | (OnHit, "", d, ms) :: _ ->
            near 4.0 d
            equal (box [ Named "MovementSpeed", More, -40.0 ]) (box ms)
        | other -> failwithf "Earthquake: %A" other
        // with "40% increased Jagged Ground Duration" and "Enemies in Jagged Ground you create take 10% increased Damage"
        match ground (rulesOf [ gem "Earthquake" ] [ "40% increased Jagged Ground Duration"; "Enemies in Jagged Ground you create take 10% increased Damage" ] "Earthquake") with
        | (OnHit, "", d, ms) :: _ ->
            near 5.6 d
            equal (box [ Named "MovementSpeed", More, -40.0; DamageTaken AllDamage, Inc, 10.0 ]) (box ms)
        | other -> failwithf "Earthquake with passives: %A" other
        // Jagged Ground II: only an Aftershock's hits, 4 s, 20%
        match ground (rulesOf [ gem "Rolling Slam"; support "Jagged Ground II" ] [] "Rolling Slam") with
        | (OnHit, "Aftershock", d, ms) :: _ ->
            near 4.0 d
            equal (box [ Named "MovementSpeed", More, -20.0 ]) (box ms)
        | other -> failwithf "Jagged Ground II: %A" other
        // Jagged Ground I: a use that takes an Endurance Charge
        let one = rulesOf [ gem "Rolling Slam"; support "Jagged Ground I" ] [] "Rolling Slam"
        equal (box true) (box (one |> List.exists (fun r -> match r.Hook, r.Reaction with OnUse, SpendStack ("EnduranceCharge", SetStacks ("Jagged Ground: Rolling Slam", 1)) -> true | _ -> false)))
        match ground one with
        | (OnHit, "Jagged Ground: Rolling Slam", d, _) :: _ -> near 4.0 d
        | other -> failwithf "Jagged Ground I: %A" other
        // without any of it: no ground, whatever the passive says
        equal (box 0) (box (ground (rulesOf [ gem "Rolling Slam" ] [ "Enemies in Jagged Ground you create take 10% increased Damage" ] "Rolling Slam")).Length)
        // "Enemies you Curse are Hindered, with 15% reduced Movement Speed": with the curse
        let curse lines =
            let c = load { blank with Groups = [ group [ gem "Enfeeble" ] ]; ExtraLines = lines } Options.defaults
            match Setup.pressed c Options.defaults "Enfeeble" with
            | Some p -> p.Rules |> List.pick (fun r -> match r.Reaction with ApplyCurse (_, _, ms) -> Some(ms |> List.filter (fun m -> m.Stat = Named "MovementSpeed") |> List.map (fun m -> m.Kind, m.Value)) | _ -> None)
            | None -> failwith "Enfeeble not built"
        equal (box ([]: (ModKind * float) list)) (box (curse []))
        equal (box [ More, -15.0 ]) (box (curse [ "Enemies you Curse are Hindered, with 15% reduced Movement Speed" ])))

    test "stats: ailments that deal their damage faster are read; no stun buildup; companions' Onslaught; a weapon's lines about building" (fun () ->
        // (was shadowed by an older line that called it not simulated, while the engine has the rule)
        equal (box [ Mod.inc (Named "FasterAilments") 12.0 ]) (box (mods "damaging_ailments_deal_damage_+%_faster" 12.0))
        // an Ignite of 20 a second for 4 s, 25% faster: 25 a second for 3.2 s. One hit at 0.5 s
        // of a 3 s fight: 2.5 s of burning either way
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let burning (ms: Mod list) =
            let player = { Actor.make "Player" (Mod.base' (AilmentChance Ignite) 100.0 :: ms) [ bolt ] with Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.25 } ] }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = [] } player 3.0
        let ignite f = (Sim.run f 1u).DamageBy |> Map.tryFind "Ignite" |> Option.defaultValue 0.0
        near (2.5 * 20.0) (ignite (burning []))
        near (2.5 * 25.0) (ignite (burning (mods "damaging_ailments_deal_damage_+%_faster" 25.0)))
        same (burning (mods "damaging_ailments_deal_damage_+%_faster" 25.0)) seeds
        equal (box [ Mod.more (Buildup HeavyStun) -100.0 ]) (box (mods "active_skill_hit_damage_stun_multiplier_+%_final" -100.0))
        equal
            (box [ Mod.base' (ForMinions(Named "Rule:OnKill:Onslaught:4")) 20.0 |> Mod.whenever (SkillIs(Tag "Companion")) ])
            (box (mods "companion_onslaught_on_kill_%" 20.0))
        equal (box Structural) (box (StatMap.meaning "active_skill_override_turn_duration_ms" 100.0))
        equal (box NotOffensive) (box (StatMap.meaning "unique_jewel_nearby_disconnected_passives_can_be_allocated" 1.0)))

    // ---- allies, recovery while standing, stun recovery, what monsters never do ----

    test "stats: what allies in the Presence gain and regenerate; costs without energy shield; recovery while stationary" (fun () ->
        equal (box [ for t in DamageType.all -> ForMinions(GainAs(t, Fire)), 20.0 ]) (box (mods "allies_in_presence_damage_%_to_gain_as_fire" 20.0 |> List.map (fun m -> m.Stat, m.Value)))
        // 120% a minute is 2% a second; 600 life a minute is 10 a second
        equal (box [ Mod.base' (ForMinions LifeRegenPercent) 2.0 ]) (box (mods "allies_in_presence_life_regeneration_rate_per_minute_equal_to_their_maximum_life_%" 120.0))
        equal (box [ Mod.base' (ForMinions LifeRegen) 10.0 ]) (box (mods "allies_in_presence_life_regeneration_rate_per_minute" 600.0))
        equal (box [ Mod.more ManaCost 25.0 |> Mod.whenever (Not(WhileBuff "HasEnergyShield")) ]) (box (mods "lich_mana_cost_+%_final_if_you_have_no_energy_shield" 25.0))
        // a minion's body: 2% of its life and 10 a second
        let wolf = db.Value.Minions |> Map.toList |> List.map snd |> List.head
        let body ms = Behaviours.minionBody Ctx.empty wolf 40 ms None
        let plain = body []
        near 0.0 plain.LifeRegen
        near (plain.Life * 0.02 + 10.0) (body [ Mod.base' LifeRegenPercent 2.0; Mod.base' LifeRegen 10.0 ]).LifeRegen
        // the player stands: "while stationary" holds for its sheet (base regeneration from a line of its own)
        let regen lines = (load { blank with ExtraLines = [ "Regenerate 2% of maximum Life per second" ] @ lines } Options.defaults).Vitals.LifeRegen
        near (regen [] * 1.3) (regen [ "30% increased Life Regeneration Rate while stationary" ])
        let life = (load blank Options.defaults).Vitals.Life
        near (regen [] + life * 0.01) (regen [ "Regenerate 1% of maximum Life per second while stationary" ]))

    test "stun recovery: a light stun on the player is over sooner" (fun () ->
        near 1.2 (load { blank with ExtraLines = [ "20% increased Stun Recovery" ] } Options.defaults).Vitals.StunRecovery
        near 1.0 (load blank Options.defaults).Vitals.StunRecovery
        // a brute whose every hit stuns (0.35 s), once a second, against a spell of 0.2 s. A stun
        // delays the next cast to its end (the cast under way goes on): 0.15 to 0.35 s lost
        // each time, and with twice the recovery (0.175 s, less than a cast) at most 0.175 s
        let brute =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 100.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 2.0 } ]; BaseTime = 0.2 }
        let fight recovery =
            let vitals = { PlayerStats.basic 1e7 0.0 with StunThreshold = 1.0; AilmentThreshold = 1e9; StunRecovery = recovery }
            Fight.single brute { Actor.make "Player" [] [ bolt ] with Vitals = Some vitals } 30.0
        let uses recovery = count (Sim.run (fight recovery) 1u) "uses:Bolt"
        let slow, fast, never = uses 1.0, uses 2.0, uses 1e9
        if not (slow < fast && fast < never) then failwithf "%d casts, %d with twice the recovery, %d unstunned" slow fast never
        // 151 casts begun unstunned (the last as the fight ends); 30 stuns cost 4.5 to 10.5 s
        // (22 to 53 casts), or at most 5.25 s (26 casts)
        equal (box 151) (box never)
        if slow < never - 54 || slow > never - 22 || fast < never - 27 then failwithf "%d and %d casts" slow fast
        same (fight 1.0) seeds
        same (fight 2.0) seeds)

    test "Pin from the critical hits of physical spells; what monsters never do is understood, without effect" (fun () ->
        equal (box [ Mod.flag (Named "CanPin") |> Mod.whenever (SkillIs Spell) |> Mod.whenever IsCrit ]) (box (mods "physical_spell_damage_can_pin_on_critical_hit" 1.0))
        let enemy = { EnemyDef.atLevel 83 1e9 with Mods = [ Mod.more PoiseThreshold -99.999 ] }
        let fight crit (tags: SkillTag list) =
            let bolt = { Skill.make "Bolt" tags with BaseDamage = Map.ofList [ Physical, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = crit }
            let player = Actor.make "Player" (mods "physical_spell_damage_can_pin_on_critical_hit" 1.0 @ [ Mod.more (Buildup HeavyStun) -100.0 ]) [ bolt ]
            Fight.single enemy player 6.0
        let pins f = count (Sim.run f 1u) "inflict:Pin"
        if pins (fight 100.0 [ Spell ]) = 0 then failwith "a spell that always crits never pins"
        equal (box 0) (box (pins (fight 0.0 [ Spell ])))
        equal (box 0) (box (pins (fight 100.0 [ Attack ])))
        same (fight 100.0 [ Spell ]) seeds
        same (fight 40.0 [ Spell ]) seeds
        for id in [ "curse_effect_on_self_+%"; "damage_+%_per_active_curse_on_self"; "armour_break_taken_+%"; "deflected_hits_cannot_inflict_bleeding_on_self"
                    "enemies_in_presence_life_regeneration_+%"; "enemies_in_presence_cooldown_recovery_+%"; "heist_enchantment_resistance_mod_effect_+%" ] do
            equal (box NotOffensive) (box (StatMap.meaning id 10.0)))

    // ---- the weapon set in hand ----

    test "a character that fights with weapon set two has that set's defences in the fight, not only on its sheet" (fun () ->
        // a shield of 400 armour in set one, of 100 in set two, "+30 to Strength" on set two's weapon
        let b =
            { blank with
                Items =
                    [ "Weapon 2", "Rarity: Rare\nTest Shield\n--------\nArmour: 400"
                      "Weapon 2 Swap", "Rarity: Rare\nOther Shield\n--------\nArmour: 100"
                      "Weapon 1 Swap", "Rarity: Rare\nTest Mace\nPlated Mace\n--------\n+30 to Strength\n" ] }
        for setTwo in [ false; true ] do
            let o = { Options.defaults with SheetSetTwo = Some setTwo }
            let c = load b o
            let player = Setup.player c o [] None
            let atFight = player.VitalsAt.Value (c.Resources |> Map.ofList)
            // the sheet and the fight agree, whichever set is in hand
            near c.Vitals.Armour atFight.Armour
            near c.Vitals.Str atFight.Str
            near c.Vitals.Life atFight.Life
        let one, two = load b { Options.defaults with SheetSetTwo = Some false }, load b { Options.defaults with SheetSetTwo = Some true }
        near 30.0 (two.Vitals.Str - one.Vitals.Str)
        if not (one.Vitals.Armour > two.Vitals.Armour * 2.0) then failwithf "armour %.0f in set one, %.0f in set two" one.Vitals.Armour two.Vitals.Armour
        // and a monster's hit meets set two's armour: it takes more through the smaller shield
        let o = { Options.defaults with SheetSetTwo = Some true; MaxTime = 10.0 }
        let brute =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 0.5; Damage = Map.ofList [ Physical, 200.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let taken (c: Loaded) (o: Options) = (Sim.run (Setup.fight c o (Setup.player c o [] None) [ brute ]) 1u).DamageTaken
        let withTwo = taken two o
        let withOne = taken one { o with SheetSetTwo = Some false }
        if not (withTwo > withOne) then failwithf "took %.0f with set two in hand, %.0f with set one" withTwo withOne)

    // ---- a nova cast on a bolt ----

    test "Ice Nova cast on a Frostbolt in flight: it comes from the bolt when the bolt is nearer the target than the player" (fun () ->
        // a bolt sent at 0.5 s flies at 3 m a second; the nova lands at 1.5 s (bolt 3 m out). An
        // enemy 6 m away (0.5 m body) is in a 3 m nova there, not in one around the player
        let bolt = { Skill.make "Bolt" [ Spell; Projectile ] with BaseDamage = Map.ofList [ Cold, { Min = 0.001; Max = 0.001 } ]; BaseTime = 0.5; BaseCrit = 0.0; Mods = [ Mod.flag (Named "LaunchesBolt") ] }
        let nova life = { Skill.make "Nova" [ Spell; Area; Nova ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 } ]; BaseTime = 1.0; BaseCrit = 0.0; Delivery = AreaAroundSelf 30.0; Mods = [ Mod.base' (Named "NovaOnBoltSpeed") 30.0; Mod.base' (Named "NovaOnBoltLife") life ] }
        let fight life distance =
            let enemy = { EnemyDef.atLevel 83 1e9 with Mods = []; Position = Vec.make distance 0.0; Size = 5.0; Attack = None }
            let player =
                { Actor.make "Player" [ Mod.more (Buildup Freeze) -100.0; Mod.more (AilmentMagnitude(Some Chill)) -100.0 ] [ bolt; nova life ] with
                    Rotation = [ { Use = "Bolt"; When = fun v -> v.LastUsed <> "Bolt" }; Actor.always "Nova" ] }
            Fight.single enemy player 1.6
        let out life distance = Sim.run (fight life distance) 1u
        equal (box 1) (box (count (out 4.0 60.0) "hits:Nova"))
        equal (box 1) (box (count (out 4.0 60.0) "on bolt:Nova"))
        // the bolt is gone (it lasts 0.5 s): around the player, which does not reach
        equal (box 0) (box (count (out 0.5 60.0) "hits:Nova"))
        // an enemy nearer than the bolt (1 m): around the player, which reaches it
        equal (box 0) (box (count (out 4.0 10.0) "on bolt:Nova"))
        equal (box 1) (box (count (out 4.0 10.0) "hits:Nova"))
        same (fight 4.0 60.0) seeds
        same { fight 4.0 60.0 with MaxTime = 12.0 } seeds
        same { fight 4.0 10.0 with MaxTime = 12.0 } seeds
        // from the gems: with Frostbolt in the play style, Ice Nova gets the bolt's speed and life
        let o = Options.defaults
        let c = load { blank with Groups = [ group [ gem "Ice Nova" ]; group [ gem "Frostbolt" ] ] } o
        let player = Setup.player c o [ Spam "Frostbolt"; Spam "Ice Nova" ] None
        let value (sk: Skill) n = sk.Mods |> List.sumBy (fun m -> if m.Stat = Named n then m.Value else 0.0)
        let iceNova = player.Skills |> List.find (fun s -> s.Name = "Ice Nova")
        near 30.0 (value iceNova "NovaOnBoltSpeed")
        near 4.0 (value iceNova "NovaOnBoltLife")
        equal (box true) (box ((player.Skills |> List.find (fun s -> s.Name = "Frostbolt")).Mods |> List.exists (fun m -> m.Stat = Named "LaunchesBolt")))
        // "30% increased Projectile Speed" and a support's 40% more: 30 x 1.3 x 1.4
        let c2 = load { blank with Groups = [ group [ gem "Ice Nova" ]; group [ gem "Frostbolt"; support "Projectile Acceleration III" ] ]; ExtraLines = [ "30% increased Projectile Speed" ] } o
        let p2 = Setup.player c2 o [ Spam "Frostbolt"; Spam "Ice Nova" ] None
        near (30.0 * 1.3 * 1.4) (value (p2.Skills |> List.find (fun s -> s.Name = "Ice Nova")) "NovaOnBoltSpeed")
        // without Frostbolt in the play style: an ordinary nova
        let alone = Setup.player c o [ Spam "Ice Nova" ] None
        near 0.0 (value (alone.Skills |> List.find (fun s -> s.Name = "Ice Nova")) "NovaOnBoltSpeed"))

    // ---- what the game's API says an item's lines are ----

    test "item lines the descriptions here don't have are read from the stat ids the API sends with the item" (fun () ->
        // a Foulborn line in a wording the descriptions don't have; the API's mods give its ids
        let item (mods: string) (mutated: string) =
            Json.parse (sprintf "{\"rarity\": \"Unique\", \"name\": \"Test Globe\", \"typeLine\": \"Sacred Focus\", \"explicitMods\": [\"+10 to maximum Life\"], \"mutatedMods\": [\"%s\"], \"mods\": {\"explicit\": [%s]}}" mutated mods)
        let life = "{\"id\": \"A\", \"stats\": {\"base_maximum_life\": 10}}"
        let crit = "{\"id\": \"B\", \"stats\": {\"spell_critical_strike_chance_+%_per_100_max_mana_with_non_channelling_skills\": 3}}"
        let line = "Non-[Channelling] Skills have 3% increased [Spell] [Critical|Critical Hit Chance] per 100 maximum Mana"
        let parsed (j: Json) = Items.parse db.Value (Api.itemText j)
        let p = parsed (item (life + ", " + crit) line)
        equal (box ([]: string list)) (box p.Coverage.UnknownText)
        let ofCrit = p.Mods |> List.filter (fun m -> m.Stat = CritChance)
        equal (box [ Inc, 3.0 ]) (box (ofCrit |> List.map (fun m -> m.Kind, m.Value)))
        // the life line, read from its text, is not taken again from its ids
        near 10.0 (p.Mods |> List.sumBy (fun m -> if m.Stat = MaxLife then m.Value else 0.0))
        // a mod whose values are not the line's numbers does not explain it
        let wrong = "{\"id\": \"B\", \"stats\": {\"spell_critical_strike_chance_+%_per_100_max_mana_with_non_channelling_skills\": 4}}"
        equal (box [ "Non-Channelling Skills have 3% increased Spell Critical Hit Chance per 100 maximum Mana" ]) (box (parsed (item (life + ", " + wrong) line)).Coverage.UnknownText)
        // without the API's mods: as before, not understood
        let bare = Json.parse (sprintf "{\"rarity\": \"Unique\", \"name\": \"Test Globe\", \"typeLine\": \"Sacred Focus\", \"mutatedMods\": [\"%s\"]}" line)
        equal (box 1) (box (parsed bare).Coverage.UnknownText.Length)
        // a radius jewel's line is read where the jewel is socketed (for the passives in its
        // radius), never as the character's own from the ids
        let radius = "Notable Passive Skills in Radius also grant 10% increased Critical Damage Bonus"
        let grant = "{\"id\": \"C\", \"stats\": {\"local_jewel_notables_in_radius_grant_critical_strike_multiplier_+%\": 10}}"
        equal (box [ radius ]) (box (parsed (item grant radius)).Coverage.UnknownText))

    // ---- mana lost, Glory gained ----

    test "mana degeneration lowers mana regeneration, and mana never falls below nothing" (fun () ->
        let v lines = (load { blank with ExtraLines = lines } Options.defaults).Vitals
        let plain = v []
        // "Lose 1% of maximum Mana per Second": 4% regeneration less 1% of maximum mana
        near (plain.ManaRegen - plain.Mana * 0.01) (v [ "Lose 1% of maximum Mana per Second" ]).ManaRegen
        // and it is no regeneration: "increased Mana Regeneration Rate" does not raise it
        let more = v [ "Lose 1% of maximum Mana per Second"; "50% increased Mana Regeneration Rate" ]
        near (plain.ManaRegen * 1.5 - plain.Mana * 0.01) more.ManaRegen
        // a pool that drains: 100 mana losing 10 a second, a spell of 30 every 0.5 s while it can
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5; Cost = 30.0 }
        let fight = Fight.single { EnemyDef.atLevel 83 1e9 with Mods = [] } { Actor.make "Player" [] [ bolt ] with Vitals = Some { PlayerStats.basic 1e6 100.0 with ManaRegen = -10.0 } } 6.0
        let out = Sim.run fight 1u
        // three casts (100 -> 70 -> 35 -> 0 with the drain between), then none
        equal (box 3) (box (count out "uses:Bolt"))
        same fight seeds)

    test "increased Glory generation scales what each event builds" (fun () ->
        let mace = "Weapon 1", "Rarity: Normal\nPlated Mace\n--------\n"
        let builds lines =
            let c = load { blank with Items = [ mace ]; Groups = [ group [ gem "Hammer of the Gods" ] ]; ExtraLines = lines } Options.defaults
            let p = Setup.player c Options.defaults [ Spam "Hammer of the Gods" ] None
            p.Skills |> List.collect (fun s -> s.Mods) |> List.filter (fun m -> match m.Stat with Named n -> n.StartsWith "Builds:Glory" | _ -> false) |> List.sumBy (fun m -> m.Value)
        let plain = builds []
        if plain <= 0.0 then failwith "Hammer of the Gods builds no Glory"
        near (plain * 1.25) (builds [ "25% increased Glory generation" ]))
