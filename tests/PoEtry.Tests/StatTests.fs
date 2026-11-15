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

let private gem name = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = false; StatSet = 1; Minion = None; Count = None; Corrupted = false }
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
        // Tame Beast's capture: never used by a companion gem (the beast is captured)
        equal (box Structural) (box (StatMap.meaning "tame_beast_hinder_%" 30.0))
        equal (box Structural) (box (StatMap.meaning "skill_can_see_monster_categories" 1.0))
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
                    "enemies_in_presence_life_regeneration_+%"; "enemies_in_presence_cooldown_recovery_+%"; "heist_enchantment_resistance_mod_effect_+%"
                    "enemies_cursed_by_you_have_life_regeneration_rate_+%"; "bleeding_enemies_cannot_regenerate_life"
                    // (the player stands: what holds while it moves or sprints never does)
                    "projectile_attacks_%_chance_to_fire_2_additional_projectiles_while_moving"; "evasion_rating_+%_if_sprinting"
                    "deflection_rating_+%_while_moving"; "life_loss_%_per_minute_while_sprinting" ] do
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

    test "Abyssal Wasting lowers life regeneration, which monsters here never have: understood, without effect" (fun () ->
        for id in [ "abyssal_wasting_on_hit"; "abyssal_wasting_effect_+%"; "abyssal_wasting_infinite_duration"; "unaffected_by_elemental_weakness" ] do
            equal (box NotOffensive) (box (StatMap.meaning id 1.0))
        // what it does besides is not swallowed with it
        equal (box Unmapped) (box (StatMap.meaning "abyssal_wasting_also_prevents_enemies_from_dealing_critical_strikes" 1.0)))

    test "api: an idol with a name of its own is an idol (the property Idol): counted, and its Bonded lines apply" (fun () ->
        let idol = "{\"typeLine\": \"Carved Test\", \"properties\": [{\"name\": \"[Idol]\", \"values\": []}], \"bondedMods\": [\"[ShamanOnlyMods|Bonded]:\", \"Body Armours: 5% increased [Spirit]\"]}"
        let rune = "{\"typeLine\": \"Test Rune\", \"properties\": []}"
        let body =
            Json.parse (sprintf "{\"rarity\": \"Rare\", \"name\": \"Test Robe\", \"typeLine\": \"Silk Robe\", \"inventoryId\": \"BodyArmour\", \"runeMods\": [\"[Idol|Idols] socketed in this item gain the benefits of their [ShamanOnlyMods|Bonded] modifiers\", \"[ShamanOnlyMods|Bonded]: 5%% increased [Spirit]\"], \"socketedItems\": [%s, %s]}" idol rune)
        let text = Api.itemText body
        equal (box true) (box (text.Contains "Idols socketed: 1"))
        equal (box true) (box (text.Split('\n') |> Array.exists (fun l -> l.Trim() = "5% increased Spirit"))))

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

    test "Oil Ground: the plain barrage's hits leave the enemy in oil: Fire Exposure of 20% + 20%, 20% slower, for 6 s" (fun () ->
        let setup = { Gem = "Oil Barrage"; Level = 20; Quality = 0; StatSet = 1; Supports = [] }
        let skill = Skill.make "Oil Barrage" [ Attack; Projectile ]
        let rules = Behaviours.oilGround db.Value ModDb.empty setup skill
        equal (box [ "exposure", 40.0, 6.0; "slow", -20.0, 6.0 ])
            (box (rules |> List.map (fun r ->
                match r.Hook, r.Filter, r.Reaction with
                | OnHit, Called "Oil Barrage", ApplyExposure (Fire, a, d) -> "exposure", a, d
                | OnHit, Called "Oil Barrage", ApplyDebuff ("Oil Ground", d, 1, [ m ]) when m.Stat = Named "MovementSpeed" -> "slow", m.Value, d
                | _ -> "other", 0.0, 0.0)))
        // 50% increased skill effect duration: 9 s
        let longer = Behaviours.oilGround db.Value (ModDb.ofList [ Mod.inc SkillDuration 50.0 ]) setup skill
        equal (box [ 9.0; 9.0 ]) (box (longer |> List.map (fun r -> match r.Reaction with ApplyExposure (_, _, d) | ApplyDebuff (_, d, _, _) -> d | _ -> 0.0)))
        equal (box ([]: string list)) (box (Gems.unmodelledTypes db.Value "Oil Barrage" |> List.filter ((=) "CreatesGroundEffect"))))

    test "Combat Frenzy: a Frenzy Charge for a freeze, electrocute or pin, at most once every few seconds" (fun () ->
        let c = load { blank with Groups = [ group [ gem "Combat Frenzy" ] ]; ExtraLines = [ "+100 to Spirit" ] } Options.defaults
        let player = Setup.player c Options.defaults [] None
        let periodic = player.Periodics |> List.find (fun p -> p.Name = "Combat Frenzy")
        // the gem's own number at level 20: 6.1 s
        let raw = Gems.rawStats db.Value ModDb.empty { Gem = "Combat Frenzy"; Level = 20; Quality = 0; StatSet = 1; Supports = [] }
        near (raw |> List.pick (fun (k, v) -> if k = "skill_combat_frenzy_x_ms_cooldown" then Some(v / 1000.0) else None)) periodic.Every
        near 6.1 periodic.Every
        equal (box 1) (box player.MaxResources.["CombatFrenzyReady"])
        equal (box 3) (box (player.Rules |> List.filter (fun r -> match r.Reaction with SpendStack ("CombatFrenzyReady", GainResource ("FrenzyCharge", 1)) -> true | _ -> false)).Length)
        // in a fight: a spell that freezes or electrocutes at nearly every hit (no poise), its
        // statuses ending fast; the charges come no faster than one per 6.1 s
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 }; Lightning, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.25; BaseCrit = 0.0 }
        let seen = System.Collections.Generic.List<float * int>()
        let actor =
            { Actor.make "Player" [ Mod.flag (Named "CanElectrocute") ] [ bolt ] with
                Rules = player.Rules |> List.filter (fun r -> match r.Reaction with SpendStack ("CombatFrenzyReady", _) -> true | _ -> false)
                Periodics = [ periodic ]
                MaxResources = Map.ofList [ "CombatFrenzyReady", 1; "FrenzyCharge", 20 ]
                Resources = [ "CombatFrenzyReady", 1 ]
                Rotation = [ { Use = "Bolt"; When = fun v -> seen.Add(v.Time, Map.tryFind "FrenzyCharge" v.Buffs |> Option.defaultValue 0); true } ] }
        let fight = Fight.single { EnemyDef.atLevel 83 1e9 with Mods = [ Mod.more PoiseThreshold -99.999 ] } actor 20.0
        Sim.run fight 1u |> ignore
        let charges = seen |> Seq.map snd |> Seq.max
        // 20 s: the one ready at the start and at most one per 6.1 s after: 4
        if charges < 2 || charges > 4 then failwithf "%d charges in 20 s" charges
        same fight seeds)

    test "Conduit: a charge the character would gain goes to its allies; it holds none, from rules or by itself" (fun () ->
        equal (box [ Named "Conduit:PowerCharge"; Named "Conduit:FrenzyCharge"; Named "Conduit:EnduranceCharge" ]) (box (mods "keystone_conduit" 1.0 |> List.map (fun m -> m.Stat)))
        equal (box [ Named "Conduit:FrenzyCharge" ]) (box (mods "frenzy_only_conduit" 1.0 |> List.map (fun m -> m.Stat)))
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.25; BaseCrit = 0.0 }
        let seen = ref 0
        let fight conduit =
            let actor =
                { Actor.make "Player" (if conduit then [ Mod.flag (Named "Conduit:FrenzyCharge") ] else []) [ bolt ] with
                    Rules = [ { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("FrenzyCharge", 1) } ]
                    Periodics = [ { Name = "generate charges"; Every = 1.0; Action = GainRandom [ "FrenzyCharge"; "PowerCharge" ] } ]
                    MaxResources = Map.ofList [ "FrenzyCharge", 3; "PowerCharge", 3 ]
                    Rotation = [ { Use = "Bolt"; When = fun v -> seen.Value <- max seen.Value (Map.tryFind "FrenzyCharge" v.Buffs |> Option.defaultValue 0); true } ] }
            Fight.single (EnemyDef.atLevel 83 1e9) actor 10.0
        seen.Value <- 0
        let out = Sim.run (fight true) 1u
        equal (box 0) (box seen.Value)
        equal (box 0) (box (count out "gained:FrenzyCharge"))
        if count out "conduit:FrenzyCharge" < 30 then failwithf "%d given away" (count out "conduit:FrenzyCharge")
        // power charges are still the character's own
        if count out "gained:PowerCharge" < 1 then failwith "no power charge"
        seen.Value <- 0
        Sim.run (fight false) 1u |> ignore
        equal (box 3) (box seen.Value)
        same (fight true) seeds)

    // ---- Fork ----

    test "Fork: at the first enemy it does not pierce a projectile splits in two, 60 degrees off its line, and forked ones deal less" (fun () ->
        equal (box [ Named "ForkChance", 100.0 ]) (box (mods "projectiles_fork" 1.0 |> List.map (fun m -> m.Stat, m.Value)))
        equal (box [ Named "ForkedDamage", More ]) (box (mods "support_fork_forked_projectile_damage_+%_final" -30.0 |> List.map (fun m -> m.Stat, m.Kind)))
        equal (box [ Named "CannotFork" ]) (box (mods "projectiles_cannot_fork" 1.0 |> List.map (fun m -> m.Stat)))
        equal (box [ Named "CannotChain" ]) (box (mods "base_projectiles_cannot_chain" 1.0 |> List.map (fun m -> m.Stat)))
        let dummy name x y = { EnemyDef.atLevel 83 1e9 with Name = name; Position = Vec.make x y; Size = 4.0; Mods = []; Attack = None }
        // the projectile meets a at (26, 0); 20 further at 60 degrees either way: b and c; d behind a
        let a = { dummy "a" 30.0 0.0 with Life = 2e9 }
        let off deg = Vec.add (Vec.make 26.0 0.0) (Vec.scale 20.0 (Vec.ofAngle (deg * System.Math.PI / 180.0)))
        let b, c, d = { dummy "b" 0.0 0.0 with Position = off 60.0 }, { dummy "c" 0.0 0.0 with Position = off -60.0 }, dummy "d" 60.0 0.0
        let all = [ a; b; c; d ]
        let p = { ProjectileShape.make 1 with Range = 200.0 }
        let parted shape = Geometry.targetsParted (Rng 1u) [] infinity (Projectiles shape) 1.0 a all
        equal (box ([ "a" ], ([]: string list))) (box (parted p))
        equal (box ([ "a" ], [ "c"; "b" ])) (box (parted { p with Forks = 1 }))
        // three in place of two: one flies on along the line, to d
        equal (box ([ "a" ], [ "c"; "d"; "b" ])) (box (parted { p with Forks = 1; ForkExtraChance = 100.0 }))
        // a projectile that pierces a forks at d, whose forks find nothing
        equal (box ([ "a"; "d" ], ([]: string list))) (box (parted { p with Forks = 1; Pierce = 1 }))
        // forked projectiles fork no more than they have times left: of b's and c's forks none meets anything
        equal (box ([ "a" ], [ "c"; "b" ])) (box (parted { p with Forks = 2 }))
        // in a fight: a spell that forks, its forked projectiles dealing 30% less: a, and b and c at 70%
        let bolt fork = { Skill.make "Bolt" [ Spell; Projectile ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0; Delivery = Projectiles p; Mods = (if fork then [ Mod.base' (Named "ForkChance") 100.0 ] else []) @ [ Mod.more (Named "ForkedDamage") -30.0 ] }
        let fight fork = { Fight.single a (Actor.make "Player" [] [ bolt fork ]) 10.0 with Enemies = all }
        let total f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near 2.4 (total (fight true) / total (fight false))
        // and a skill that cannot fork does not
        let cannot = { fight true with Player = Actor.make "Player" [ Mod.flag (Named "CannotFork") ] [ bolt true ] }
        near 1.0 (total cannot / total (fight false))
        // a chain takes the bolt from a to the nearest other enemy, unless it cannot chain
        let chaining no = { fight false with Player = Actor.make "Player" ([ Mod.base' Chains 1.0 ] @ (if no then [ Mod.flag (Named "CannotChain") ] else [])) [ bolt false ] }
        near 2.0 (total (chaining false) / total (fight false))
        near 1.0 (total (chaining true) / total (fight false))
        same (chaining true) seeds
        same (fight true) seeds)

    // ---- Twister and the Whirlwind ----

    test "Twister: it consumes the Whirlwind, 80% more damage per stage and a twister more; elemental twisters gain 50% as their element" (fun () ->
        // Whirling Slash builds the whirlwind, three stages at most
        let ws = Behaviours.whirlwind db.Value { Gem = "Whirling Slash"; Level = 1; Quality = 0; StatSet = 1; Supports = [] } (Skill.make "Whirling Slash" [ Attack ])
        equal (box [ GainResource("Whirlwind", 1) ]) (box (ws |> List.map (fun r -> r.Reaction)))
        equal (box 3) (box (Behaviours.whirlwindStages db.Value { blank with Groups = [ group [ gem "Whirling Slash"; gem "Twister" ] ] }))
        let more = mods "twister_damage_+%_final_per_whirling_slash_stage" 80.0
        equal (box [ Damage AllDamage, More, 80.0; AdditionalProjectiles, Base, 1.0 ]) (box (more |> List.map (fun m -> m.Stat, m.Kind, m.Value)))
        let gain = mods "twister_gain_%_elemental_damage_of_corresponding_type" 50.0
        equal (box 15) (box gain.Length)
        equal (box [ Named "WindBoost:Cold" ]) (box (mods "wind_skills_count_as_empowered_by_chilled_ground" 1.0 |> List.map (fun m -> m.Stat)))
        // in a fight: three slashes, then a twister that consumes the three stages: 3.4 times
        // the damage of a twister without a whirlwind
        let slash = { Skill.make "Slash" [ Attack ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let twister = { Skill.make "Twister" [ Attack ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0; Consumes = Some "Whirlwind"; Mods = more @ gain }
        let fight builds boosted =
            { Actor.make "Player" [] [ slash; twister ] with
                Rules = [ { Hook = OnUse; Filter = Called "Slash"; Chance = 100.0; Reaction = GainResource("Whirlwind", 1) } ]
                MaxResources = Map.ofList [ "Whirlwind", 3 ]
                Resources = (if boosted then [ "BoostedBy:Cold", 1 ] else [])
                Rotation = [ { Use = "Slash"; When = fun v -> builds && v.Time < 1.4 && (Map.tryFind "Whirlwind" v.Buffs |> Option.defaultValue 0) < 3 }; Actor.always "Twister" ] }
            |> fun a -> { Fight.single (EnemyDef.atLevel 83 1e9) a 2.2 with Enemies = [ { EnemyDef.atLevel 83 1e9 with Mods = [] } ] }
        let twisted f = (Sim.run f 1u).DamageBy |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "Twister") |> List.sumBy snd
        let out = Sim.run (fight true false) 1u
        equal (box 3) (box (count out "gained:Whirlwind"))
        equal (box 1) (box (count out "consumed:Whirlwind"))
        // (2.2 s: three slashes and one twister, against one twister after another)
        let plain = twisted (fight false false) / float (count (Sim.run (fight false false) 1u) "hits:Twister")
        near 3.4 (twisted (fight true false) / plain)
        // a twister boosted by chilled ground: half its damage again, as cold
        near 1.5 (twisted (fight false true) / twisted (fight false false))
        same (fight true true) seeds)

    // ---- Herald of Blood ----

    test "Herald of Blood: killing a bleeding enemy with a hit sets off its explosion, of its own physical damage" (fun () ->
        let c = load { blank with Groups = [ group [ gem "Herald of Blood" ] ]; ExtraLines = [ "+100 to Spirit" ] } Options.defaults
        let skills, rules = Behaviours.heralds db.Value c.Sheet c.Build
        let boom = skills |> List.find (fun s -> s.Name = "Herald of Blood: Explosion")
        // level 20: 107 to 160 physical, whatever the weapon
        equal (box [ Physical, 107.0, 160.0 ]) (box (boom.BaseDamage |> Map.toList |> List.map (fun (t, r) -> t, r.Min, r.Max)))
        equal (box [ OnKillOf "Bleeding", TriggerSkill boom.Name ]) (box (rules |> List.map (fun r -> r.Hook, r.Reaction)))
        // a kill of a bleeding enemy (by a skill that always makes it bleed) sets it off; of one that does not bleed, nothing
        let bolt bleeds = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Physical, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0; Mods = (if bleeds then [ Mod.base' (AilmentChance Bleed) 100.0 ] else []) }
        let fight bleeds =
            let foes = [ for i in 0 .. 2 -> { EnemyDef.atLevel 83 150.0 with Name = sprintf "m%d" i; Position = Vec.make (30.0 + float i) 0.0; Size = 4.0; Mods = []; Attack = None } ]
            { Fight.single foes.Head { Actor.make "Player" [] [ bolt bleeds; boom ] with Rules = rules; Rotation = [ Actor.always "Bolt" ] } 5.0 with Enemies = foes }
        if count (Sim.run (fight true) 1u) ("uses:" + boom.Name) < 1 then failwith "no explosion"
        equal (box 0) (box (count (Sim.run (fight false) 1u) ("uses:" + boom.Name)))
        same (fight true) seeds)

    // ---- secondary radius and duration ----

    test "a part whose only radius is the secondary one has that area; the secondary duration is read where a mechanic is built" (fun () ->
        let mace = (Items.parse db.Value "Rarity: Normal\nAkoyan Club\n--------\n").Weapon
        match Gems.skill db.Value ModDb.empty mace { Gem = "Sunder"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (s, cov) ->
            // "Shockwave radius is 1.2 metres"
            let wave = s.Parts |> List.find (fun p -> p.Name.EndsWith "Shockwave")
            equal (box (AreaAtTarget 12.0)) (box wave.Delivery)
            equal (box false) (box (cov.Unsimulated |> List.exists (fun (k, _) -> k = "active_skill_base_secondary_area_of_effect_radius")))
        | Error e -> failwith e
        match Gems.skill db.Value ModDb.empty None { Gem = "Frost Bomb"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
        | Ok (_, cov) -> equal (box false) (box (cov.Unsimulated |> List.exists (fun (k, _) -> k = "base_secondary_skill_effect_duration")))
        | Error e -> failwith e)

    // ---- what blinded and marked monsters do ----

    test "enemies one blinds have less critical hit chance, enemies one marks less accuracy" (fun () ->
        let v = (load { blank with ExtraLines = [ "Enemies Blinded by you have 50% reduced Critical Hit Chance"; "Enemies you Mark have 30% reduced Accuracy Rating" ] } Options.defaults).Vitals
        near 0.5 v.BlindedEnemyCritMult
        near 0.7 v.MarkedEnemyAccuracyMult
        // a monster that always crits, blinded by every hit: no critical hit once it is blinded
        let brute =
            { EnemyDef.make Rarity.defaults 70 Unique "brute" (Vec.make 5.0 0.0) with
                Life = 1e9; Mods = []
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 10.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 100.0; Melee = true } }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.25; BaseCrit = 0.0; Mods = [ Mod.base' (AilmentChance Blind) 100.0 ] }
        let fight mult =
            let vitals = { PlayerStats.basic 1e6 0.0 with StunThreshold = 1e9; AilmentThreshold = 1e9; BlindedEnemyCritMult = mult }
            Fight.single brute { Actor.make "Player" [] [ bolt ] with Vitals = Some vitals } 10.0
        let crits m = count (Sim.run (fight m) 1u) "enemy:crit"
        if crits 1.0 < 5 then failwithf "%d crits without the line" (crits 1.0)
        // (the first hit may come before the first blind)
        if crits 0.0 > 1 then failwithf "%d crits of a blinded monster" (crits 0.0)
        same (fight 0.5) seeds)

    // ---- Combo consumed ----

    test "damage per Combo consumed: a skill that requires Combo consumes it at every use, up to 40% increased" (fun () ->
        let staff = "Weapon 1", "Rarity: Rare\nTest Staff\nWrapped Quarterstaff\n--------\n"
        let bell lines =
            let c = load { blank with Items = [ staff ]; Groups = [ group [ gem "Tempest Bell" ] ]; ExtraLines = lines } Options.defaults
            let player = Setup.player c Options.defaults [ Spam "Tempest Bell" ] None
            player.Skills |> List.find (fun s -> s.Name = "Tempest Bell")
        let combo = (bell []).Mods |> List.sumBy (fun m -> if m.Stat = Named "Requires:Combo" && m.Kind = Base then m.Value else 0.0)
        if combo <= 0.0 then failwith "Tempest Bell requires no Combo"
        let consumed (sk: Skill) = sk.Mods |> List.filter (fun m -> m.Source = "Combo consumed") |> List.map (fun m -> m.Stat, m.Kind, m.Value)
        equal (box ([]: (Stat * ModKind * float) list)) (box (consumed (bell [])))
        equal (box [ Damage AllDamage, Inc, min 40.0 (2.0 * combo) ]) (box (consumed (bell [ "Skills deal 2% increased Damage per Combo consumed, up to 40%" ])))
        equal (box [ Damage AllDamage, Inc, 40.0 ]) (box (consumed (bell [ "Skills deal 30% increased Damage per Combo consumed, up to 40%" ])))
        // "Skills lose Combo 100% faster": what was built is lost after 4 s without a gain, not 8
        let window (sk: Skill) = sk.Mods |> List.sumBy (fun m -> if m.Stat = Named "ComboWindow" then m.Value else 0.0)
        near 0.0 (window (bell []))
        near 4.0 (window (bell [ "Skills lose Combo 100% faster" ])))

    // ---- Shocked Ground ----

    test "Shocked Ground: shocking an enemy may create it; the enemy in it is Shocked for 6 s, at the base magnitude" (fun () ->
        let c = load { blank with ExtraLines = [ "50% chance on Shocking Enemies to created Shocked Ground"; "20% increased Magnitude of Shock you inflict" ] } Options.defaults
        let rules = Behaviours.shockedGround c.Sheet
        equal (box [ OnInflict Shock, 50.0, InflictStatus(Shock, Constants.baseShockMagnitude * 1.2, 6.0) ]) (box (rules |> List.map (fun r -> r.Hook, r.Chance, r.Reaction)))
        // one hit that shocks nothing itself, its rule standing for the ground: Shocked for 6 s of the 10
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.25; BaseCrit = 0.0 }
        let fight =
            let player =
                { Actor.make "Player" [] [ bolt ] with
                    Rules = [ { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = InflictStatus(Shock, 20.0, 6.0) } ]
                    Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.1 } ] }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = [] } player 10.0
        let out = Sim.run fight 1u
        near 6.0 (Map.tryFind "Shocked" out.Uptime |> Option.defaultValue 0.0)
        same fight seeds)

    // ---- Herald of Plague, and kills by damage over time ----

    test "Herald of Plague: a poisoned enemy killed by its poison passes the most damaging one on, within 1.8 m; kills by damage over time set off rules" (fun () ->
        let c = load { blank with Groups = [ group [ gem "Herald of Plague" ] ]; ExtraLines = [ "+100 to Spirit" ] } Options.defaults
        let _, rules = Behaviours.heralds db.Value c.Sheet c.Build
        let plague = rules |> List.filter (fun r -> match r.Reaction with SpreadPoison _ -> true | _ -> false)
        equal (box [ OnKillOf "Poisoned", SpreadPoison(18.0, 29.0) ]) (box (plague |> List.map (fun r -> r.Hook, r.Reaction)))
        // one bolt: a hit of 1 and a strong poison on the nearest enemy, which dies of it; one
        // other enemy 1 m off gets the poison and is Hindered, one 5 m off does not
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.25; BaseCrit = 0.0; Mods = [ Mod.base' (AilmentChance Poison) 100.0; Mod.more (AilmentMagnitude(Some Poison)) 10000.0 ] }
        let foe name x y life = { EnemyDef.atLevel 83 life with Name = name; Position = Vec.make x y; Size = 4.0; Mods = []; Attack = None }
        let fight spreads =
            let player =
                { Actor.make "Player" [] [ bolt ] with
                    Rules = (if spreads then [ { Hook = OnKillOf "Poisoned"; Filter = WithTags([], [ Herald ]); Chance = 100.0; Reaction = SpreadPoison(18.0, 100.0) } ] else [])
                          @ [ { Hook = OnKill; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("Kills", 1) } ]
                    MaxResources = Map.ofList [ "Kills", 10 ]
                    Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.1 } ] }
            { Fight.single (foe "a" 20.0 0.0 30.0) player 6.0 with Enemies = [ foe "a" 20.0 0.0 30.0; foe "b" 20.0 10.0 1e9; foe "c" 20.0 50.0 1e9 ]; Targeting = Nearest }
        let out = Sim.run (fight true) 1u
        equal (box 1) (box out.Kills.Length)
        equal (box 1) (box (count out "spread:Poison"))
        // the kill by poison counted as a kill for the rule
        equal (box 1) (box (count out "gained:Kills"))
        if (Map.tryFind "Hindered" out.Uptime |> Option.defaultValue 0.0) <= 0.0 then failwith "nobody hindered"
        let poison (o: Outcome) = Map.tryFind "Poison" o.DamageBy |> Option.defaultValue 0.0
        if poison out <= poison (Sim.run (fight false) 1u) + 1.0 then failwith "the poison did not go on"
        same (fight true) seeds)

    test "a kill by damage over time gives what a killing hit gives: mana on kill, remnants" (fun () ->
        // 10 mana and 2% of 1000 on kill; a cast of 300 makes room for it; each kill a ward remnant
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.25; BaseCrit = 0.0; Cost = 300.0; Mods = [ Mod.base' (AilmentChance Poison) 100.0; Mod.more (AilmentMagnitude(Some Poison)) 10000.0 ] }
        let foe = { EnemyDef.atLevel 83 30.0 with Mods = []; Attack = None }
        let fight =
            let player =
                { Actor.make "Player" [ Mod.base' ManaOnKill 10.0; Mod.base' (Named "ManaPercentOnKill") 2.0 ] [ bolt ] with
                    Vitals = Some(PlayerStats.basic 1e6 1000.0)
                    WardRemnants = Some { OnKillChance = 100.0; OnStunChance = 0.0; StunCooldown = 1.0; WardPerRemnant = 10.0; ExtraChance = 0.0; Delay = 0.5; Overflow = 1.0 }
                    Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.1 } ] }
            Fight.single foe player 6.0
        let out = Sim.run fight 1u
        equal (box 1) (box out.Kills.Length)
        // the hit of 1 did not kill: the poison did
        if (Map.tryFind "Poison" out.DamageBy |> Option.defaultValue 0.0) < 29.0 then failwith "not killed by its poison"
        near 30.0 (Map.tryFind "kill" out.ManaGained |> Option.defaultValue 0.0)
        equal (box 1) (box (count out "ward remnant"))
        same fight seeds)

    test "a kill by recorded damage coming due is a kill over time: rules for kills, mana on kill" (fun () ->
        // all of what the enemy lost is recorded; a bolt of 60 leaves 40 of 100, and the 60 owed kills it
        let agony = { Skill.make "Agony" [ Spell ] with Hits = []; Duration = 4.0; BaseTime = 0.5; Mods = [ Mod.base' (Named "RecordsDamage") 100.0 ] }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 60.0; Max = 60.0 } ]; BaseTime = 0.5; BaseCrit = 0.0; Cost = 300.0 }
        let fight =
            let player =
                { Actor.make "Player" [ Mod.base' ManaOnKill 10.0 ] [ agony; bolt ] with
                    Vitals = Some(PlayerStats.basic 1e6 1000.0)
                    Rules = [ { Hook = OnKill; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("Kills", 1) } ]
                    MaxResources = Map.ofList [ "Kills", 10 ]
                    Rotation = [ { Use = "Agony"; When = fun v -> v.Time < 0.1 }; { Use = "Bolt"; When = fun _ -> true } ] }
            // (a second enemy far off keeps the fight going after the kill)
            let foe name life x = { EnemyDef.atLevel 83 life with Name = name; Position = Vec.make x 0.0; Mods = []; Attack = None }
            { Fight.single (foe "a" 100.0 20.0) player 6.0 with Enemies = [ foe "a" 100.0 20.0; foe "b" 1e12 500.0 ]; Targeting = Nearest }
        let out = Sim.run fight 1u
        equal (box 1) (box out.Kills.Length)
        near 40.0 (Map.tryFind "Agony" out.DamageBy |> Option.defaultValue 0.0)
        equal (box 1) (box (count out "gained:Kills"))
        near 10.0 (Map.tryFind "kill" out.ManaGained |> Option.defaultValue 0.0)
        same fight seeds)

    // ---- life loss from hits put off ----

    test "Amanamu's Defiance and life lost over 4 seconds instead: a share of a hit's life loss prevented, a share lost over 4 s" (fun () ->
        let mods = mods "keystone_amanamus_defiance" 1.0
        let v = (load { blank with ExtraLines = [ "When taking damage from Hits, 20% of Life Loss is prevented, then 150% of Life Loss prevented this way is Lost over 4 seconds" ] } Options.defaults).Vitals
        equal (box 2) (box mods.Length)
        near 20.0 v.DeferPrevent
        near 30.0 v.DeferLose
        let w = (load { blank with ExtraLines = [ "10% of Life Loss from Hits is prevented, then that much Life is lost over 4 seconds instead" ] } Options.defaults).Vitals
        near 10.0 w.DeferPrevent
        near 10.0 w.DeferLose
        // one hit of 250 on 300 life: 50 left; with 20% put off, 100 left, then what is lost over 4 s
        let brute =
            { EnemyDef.make Rarity.defaults 70 Unique "brute" (Vec.make 5.0 0.0) with
                Life = 1e9; Mods = []
                Attack = Some { Interval = 100.0; Damage = Map.ofList [ Physical, 250.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5 }
        let fight prevent lose =
            let vitals = { PlayerStats.basic 300.0 0.0 with StunThreshold = 1e9; AilmentThreshold = 1e9; DeferPrevent = prevent; DeferLose = lose }
            Fight.single brute { Actor.make "Player" [] [ bolt ] with Vitals = Some vitals } 120.0
        let died f = (Sim.run f 1u).Died
        equal (box None) (box (died (fight 0.0 0.0)))
        // 75 more lost: 25 left
        equal (box None) (box (died (fight 20.0 30.0)))
        // 250 more lost over 4 s: dead within them
        match died (fight 20.0 100.0) with
        | Some t -> if t > 105.0 then failwithf "died at %f" t
        | None -> failwith "alive"
        same (fight 20.0 30.0) seeds
        same (fight 20.0 100.0) seeds)

    test "Darkness: damage taken is reserved from it first, and a reservation is over after its time" (fun () ->
        let v =
            (load { blank with Level = 90; ExtraLines = [ "You have no Spirit"; "Base Maximum Darkness is 100"; "Damage taken is Reserved from Darkness before being taken from Life or Energy Shield"; "+5 to Maximum Darkness per Level"; "Darkness Reservation lasts for 10 seconds"; "20% increased maximum Darkness" ] } Options.defaults)
        equal (box ([]: string list)) (box v.Sheet.Coverage.UnknownText)
        // (100 + 5 x 90) x 1.2
        near 660.0 v.Vitals.Darkness
        near 10.0 v.Vitals.DarknessSeconds
        // 250 every 4 s on 300 life. No Darkness: dead at the second hit. 400 Darkness for 10 s: the
        // first hit and 150 of the second are reserved, the third finds none and kills. For 3 s: every
        // hit finds all 400 again, and the life is never touched
        let brute =
            { EnemyDef.make Rarity.defaults 70 Unique "brute" (Vec.make 5.0 0.0) with
                Life = 1e9; Mods = []
                Attack = Some { Interval = 4.0; Damage = Map.ofList [ Physical, 250.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5 }
        let fight darkness seconds =
            let vitals = { PlayerStats.basic 300.0 0.0 with StunThreshold = 1e9; AilmentThreshold = 1e9; Darkness = darkness; DarknessSeconds = seconds }
            Fight.single brute { Actor.make "Player" [] [ bolt ] with Vitals = Some vitals } 60.0
        let run f = Sim.run f 1u
        let plain, dark = (run (fight 0.0 0.0)).Died, (run (fight 400.0 10.0)).Died
        match plain, dark with
        | Some a, Some b -> near 4.0 (b - a)
        | _ -> failwith "should have died"
        let safe = run (fight 400.0 3.0)
        equal (box None) (box safe.Died)
        near 1.0 safe.MinLife
        same (fight 400.0 10.0) seeds
        same (fight 400.0 3.0) seeds)

    // ---- Scarred Faith ----

    test "Scarred Faith: prevented physical damage recouped as energy shield per enemy power; no regeneration; none above armour" (fun () ->
        // a boss (power 20) hits for 400 physical every 5 s; 4000 armour stops half: 200 taken,
        // 200 prevented, 5% x 20 of it (200) comes back over 4 s. 300 energy shield, 10 life
        let brute =
            { EnemyDef.make Rarity.defaults 70 Unique "brute" (Vec.make 5.0 0.0) with
                Life = 1e9; Mods = []
                Attack = Some { Interval = 5.0; Damage = Map.ofList [ Physical, 400.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 1.0; Max = 1.0 } ]; BaseTime = 0.5 }
        let fight recoup cap =
            let vitals = { PlayerStats.basic 10.0 0.0 with EnergyShield = 300.0; Armour = 4000.0; StunThreshold = 1e9; AilmentThreshold = 1e9; RecoupPreventedPerPower = recoup; ESRecoverCap = cap }
            Fight.single brute { Actor.make "Player" [] [ bolt ] with Vitals = Some vitals } 12.0
        let died f = (Sim.run f 1u).Died.IsSome
        // with the recoup the shield is full again before the next hit: alive
        equal (box false) (box (died (fight 5.0 infinity)))
        // without it, or when it may not rise above 150: the second hit reaches the 10 life
        equal (box true) (box (died (fight 0.0 infinity)))
        equal (box true) (box (died (fight 5.0 150.0)))
        same (fight 5.0 infinity) seeds
        same (fight 5.0 150.0) seeds
        // the keystone's lines on the sheet
        let node = treeJson.Value |> Tree.parseNodes |> Map.toList |> List.pick (fun (id, n) -> if n.Name = "Scarred Faith" then Some id else None)
        let v = (load { blank with TreeNodes = [ node ]; ExtraLines = [ "+500 to Armour"; "+100 to maximum Energy Shield"; "Regenerate 1% of maximum Energy Shield per second" ] } Options.defaults).Vitals
        near 5.0 v.RecoupPreventedPerPower
        near v.Armour v.ESRecoverCap
        near 0.0 v.ESRegen
        near 0.0 v.EnergyShieldRecharge)

    // ---- charges ----

    test "Recover X% of maximum Life for each Endurance Charge consumed" (fun () ->
        let v = (load { blank with ExtraLines = [ "Recover 3% of maximum Life for each Endurance Charge consumed" ] } Options.defaults).Vitals
        near (v.Life * 0.03) v.LifePerEnduranceConsumed
        // a slam paid with 200 life that consumes the two charges each use brings, 30 life back for
        // each: the two engines agree (what the fight shows of life is only the lowest it fell to
        // under hits, so the amount is not read back here)
        let slam = { Skill.make "Slam" [ Attack ] with BaseDamage = Map.ofList [ Physical, { Min = 10.0; Max = 10.0 } ]; BaseTime = 1.0; BaseCrit = 0.0; Cost = 200.0; Consumes = Some "EnduranceCharge"; Mods = [ Mod.base' (Named "ConsumeChargeCount") 2.0 ] }
        let fight per =
            let player =
                { Actor.make "Player" [] [ slam ] with
                    Vitals = Some { PlayerStats.basic 1000.0 100.0 with CostsLife = true; LifePerEnduranceConsumed = per }
                    Rules = [ { Hook = OnUse; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("EnduranceCharge", 2) } ]
                    MaxResources = Map.ofList [ "EnduranceCharge", 3 ] }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } player 3.5
        equal (box 3) (box (count (Sim.run (fight 30.0) 1u) "consumed:EnduranceCharge"))
        same (fight 30.0) seeds)

    test "Every Rage also grants X% increased Stun Threshold: in the fight, by the Rage held" (fun () ->
        let c = load { blank with ExtraLines = [ "Every Rage also grants 2% increased Stun Threshold" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        let at rage = (p.VitalsAt.Value (Map.ofList [ "Rage", rage ])).StunThreshold
        near (at 0 * 1.2) (at 10))

    test "Deal no Elemental Damage, Deal no Chaos Damage: that part of a hit is dropped" (fun () ->
        let strike ms = { Skill.make "Strike" [ Attack ] with BaseDamage = Map.ofList [ Physical, { Min = 50.0; Max = 50.0 }; Fire, { Min = 100.0; Max = 100.0 }; Chaos, { Min = 30.0; Max = 30.0 } ]; BaseTime = 1.0; BaseCrit = 0.0; Mods = ms }
        let types ms = (let s = strike ms in Hit.ranges (Skill.ctx s false Map.empty Map.empty) (ModDb.ofList s.Mods) s) |> Map.toList |> List.map fst
        equal (box [ Physical; Chaos ]) (box (types (mods "deal_no_elemental_damage" 1.0)))
        equal (box [ Physical; Fire ]) (box (types (mods "base_deal_no_chaos_damage" 1.0)))
        let player = { Actor.make "Player" [] [ strike (mods "deal_no_elemental_damage" 1.0) ] with Rotation = [ { Use = "Strike"; When = fun _ -> true } ] }
        same (Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } player 5.0) seeds)

    test "increased Deflection Rating while Surrounded: only while it is" (fun () ->
        let c = load { blank with ExtraLines = [ "+1000 to Deflection Rating"; "40% increased Deflection Rating while Surrounded" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        let at st = (p.VitalsAt.Value (Map.ofList st)).Deflection
        near (at [] * 1.4) (at [ "Surrounded", 1 ]))

    test "Cannot inflict Blind; Projectiles cannot Split is what the engine does anyway" (fun () ->
        let strike ms = { Skill.make "Strike" [ Attack ] with BaseDamage = Map.ofList [ Physical, { Min = 50.0; Max = 50.0 } ]; BaseTime = 1.0; BaseCrit = 0.0; Mods = Mod.base' (AilmentChance Blind) 100.0 :: ms }
        let fight ms = Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } { Actor.make "Player" [] [ strike ms ] with Rotation = [ { Use = "Strike"; When = fun _ -> true } ] } 5.0
        let blinded ms = Map.tryFind "Blinded" (Sim.run (fight ms) 1u).Uptime |> Option.defaultValue 0.0
        if blinded [] <= 0.0 then failwith "never blinded"
        near 0.0 (blinded (mods "cannot_inflict_blind" 1.0))
        same (fight (mods "cannot_inflict_blind" 1.0)) seeds
        equal (box Structural) (box (StatMap.meaning "projectiles_cannot_split" 1.0)))

    // ---- the player's own ailments ----

    test "the player's own Ignite is a state conditions read (Ignited, a damaging ailment); max fire resistance while Ignited" (fun () ->
        // a bolt that deals twice as much while the player is Ignited; a brute whose fire hits ignite it
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let brute fire =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Fire, fire ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let fight fire state =
            let player =
                { Actor.make "Player" [ Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (WhileBuff state) ] [ bolt ] with
                    Vitals = Some { PlayerStats.basic 1e6 0.0 with AilmentThreshold = 10.0; StunThreshold = 1e12 } }
            Fight.single (brute fire) player 6.0
        let hit (o: Outcome) = Map.tryFind "Bolt (hit)" o.DamageBy |> Option.defaultValue 0.0
        for state in [ "Ignited"; "DamagingAilment" ] do
            let burnt, cold = Sim.run (fight 1000.0 state) 1u, Sim.run (fight 0.0 state) 1u
            if count burnt "player:Ignited" = 0 then failwith "never ignited"
            if hit burnt <= hit cold * 1.2 then failwithf "%s: %f against %f" state (hit burnt) (hit cold)
            same (fight 1000.0 state) seeds
        // "+5% to Maximum Fire Resistance while Ignited", and "increased Ailment and Stun Threshold while Surrounded"
        let c = load { blank with ExtraLines = [ "+300% to Fire Resistance"; "+5% to Maximum Fire Resistance while Ignited"; "20% increased Ailment and Stun Threshold while Surrounded" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        let at st = p.VitalsAt.Value (Map.ofList st)
        near ((at []).Resist.[Fire] + 5.0) (at [ "Ignited", 1 ]).Resist.[Fire]
        near ((at []).StunThreshold * 1.2) (at [ "Surrounded", 1 ]).StunThreshold
        near ((at []).AilmentThreshold * 1.2) (at [ "Surrounded", 1 ]).AilmentThreshold)

    test "Low Energy Shield is a state; Defend with X% of Armour while not on Low Energy Shield" (fun () ->
        let c = load { blank with ExtraLines = [ "+1000 to Armour"; "+100 to maximum Energy Shield"; "Defend with 200% of Armour while not on Low Energy Shield" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        let at st = (p.VitalsAt.Value (Map.ofList st)).ArmourDefend
        near 2.0 (at [])
        near 1.0 (at [ "LowEnergyShield", 1 ])
        // in a fight: a physical brute takes the shield down, the player is then on Low Energy Shield
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let brute hit =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, hit ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let fight hit defend =
            let player =
                { Actor.make "Player" ([ Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (WhileBuff "LowEnergyShield") ] @ defend) [ bolt ] with
                    Vitals = Some { PlayerStats.basic 1e6 0.0 with EnergyShield = 100.0; Armour = 1000.0; StunThreshold = 1e12; AilmentThreshold = 1e12; ESRechargeDelay = 1e9 } }
            Fight.single (brute hit) player 6.0
        let hit (o: Outcome) = Map.tryFind "Bolt (hit)" o.DamageBy |> Option.defaultValue 0.0
        if hit (Sim.run (fight 500.0 []) 1u) <= hit (Sim.run (fight 0.0 []) 1u) * 1.2 then failwith "never on Low Energy Shield"
        same (fight 500.0 []) seeds
        // armour that counts double takes less of a hit (the fight's own vitals, with ArmourDefend 2)
        let twice =
            let f = fight 500.0 []
            { f with Player = { f.Player with Vitals = f.Player.Vitals |> Option.map (fun v -> { v with ArmourDefend = 2.0 }) } }
        if (Sim.run twice 1u).DamageTaken >= (Sim.run (fight 500.0 []) 1u).DamageTaken then failwith "armour defended with no more"
        same twice seeds)

    // ---- Wind Dancer ----

    test "Wind Dancer: stages build up, a hit on the player with one held sets off Gale Force with the group's supports, which takes them" (fun () ->
        let mace = "Weapon 1", "Rarity: Normal\nPlated Mace\n--------\n"
        let c = load { blank with Items = [ mace ]; Groups = [ group [ gem "Wind Dancer"; support "Blind II" ] ]; ExtraLines = [ "+100 to Spirit" ] } Options.defaults
        match Behaviours.windDancer db.Value c.Sheet c.Build with
        | Some ([ gale ], [ rule ], [ periodic ], most) ->
            equal (box 3) (box most)
            equal (box (Some "WindDancerStage")) (box gale.Consumes)
            if not (gale.Mods |> List.exists (fun m -> m.Stat = AilmentChance Blind && m.Value > 0.0)) then failwith "Blind II does not support Gale Force"
            equal (box WhenHit) (box rule.Hook)
            near 1.12 periodic.Every
        | other -> failwithf "%A" other
        // in a fight against a brute that hits once a second: Gale Force goes off, the brute is Blinded
        let player = Setup.player c Options.defaults [] None
        let brute =
            { EnemyDef.make Rarity.defaults 70 Magic "brute" (Vec.make 5.0 0.0) with
                Life = 1e9
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 10.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let player = { player with Vitals = player.Vitals |> Option.map (fun v -> { v with Evasion = 0.0; Deflection = 0.0; StunThreshold = 1e9; AilmentThreshold = 1e9 }); VitalsAt = None }
        let fight = Fight.single brute player 10.0
        let out = Sim.run fight 1u
        let gale = "Wind Dancer: Gale Force"
        if count out ("uses:" + gale) < 3 then failwithf "%d uses" (count out ("uses:" + gale))
        if (Map.tryFind "Blinded" out.Uptime |> Option.defaultValue 0.0) <= 0.0 then failwith "never blinded"
        // each use takes all stages: never more uses than stages could build (3 at the start, one every 1.12 s)
        if count out ("uses:" + gale) > 3 + int (10.0 / 1.12) then failwith "stages not taken"
        same fight seeds)

    // ---- Flame Breath ----

    test "Empowered Flame Breath: a use with a Power Charge to consume gains 50% of its damage as lightning" (fun () ->
        let gain = mods "wyvern_devour_empowered_flame_breath_base_all_damage_%_to_gain_as_lightning" 50.0
        equal (box 5) (box gain.Length)
        // a breath of 100 fire; a charge comes with each use (before its hits), the use consumes it
        let breath = { Skill.make "Breath" [ Attack ] with BaseDamage = Map.ofList [ Fire, { Min = 100.0; Max = 100.0 } ]; BaseTime = 1.0; BaseCrit = 0.0; Consumes = Some "PowerCharge"; Mods = gain }
        let fight charged =
            let player =
                { Actor.make "Player" [ Mod.override' (AilmentChance Ignite) 0.0 ] [ breath ] with
                    Rules = (if charged then [ { Hook = OnUse; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("PowerCharge", 1) } ] else [])
                    MaxResources = Map.ofList [ "PowerCharge", 3 ]
                    Rotation = [ { Use = "Breath"; When = fun v -> v.Time < 0.1 } ] }
            Fight.single { EnemyDef.atLevel 83 1e12 with Mods = []; Attack = None } player 3.0
        let hit (o: Outcome) = Map.tryFind "Breath (hit)" o.DamageBy |> Option.defaultValue 0.0
        let plain = hit (Sim.run (fight false) 1u)
        near (plain * 1.5) (hit (Sim.run (fight true) 1u))
        same (fight true) seeds)

    // ---- durations ----

    test "durations: of Ignite, Shock and Chill; Swift Affliction's and Limit's more; flask effect; banners" (fun () ->
        let dur id v a = Ailments.duration Ctx.empty (ModDb.ofList (mods id v)) a
        for a in [ Ignite; Shock; Chill ] do near (Ailments.baseDuration a * 1.5) (dur "ignite_shock_chill_duration_+%" 50.0 a)
        near (Ailments.baseDuration Poison) (dur "ignite_shock_chill_duration_+%" 50.0 Poison)
        // "25% more Duration of Supported Skills and Damaging Ailments they inflict"
        let swift = ModDb.ofList (mods "support_swift_affliction_skill_effect_and_damaging_ailment_duration_+%_final" 25.0)
        for a in [ Ignite; Bleed; Poison ] do near (Ailments.baseDuration a * 1.25) (Ailments.duration Ctx.empty swift a)
        near (Ailments.baseDuration Shock) (Ailments.duration Ctx.empty swift Shock)
        // "increased Duration of Elemental Ailments": Ignite, Chill, Freeze, Shock, Electrocute; a stun and a pin keep theirs
        let elemental = ModDb.ofList (mods "base_elemental_status_ailment_duration_+%" 50.0)
        for a in [ Ignite; Chill; Shock ] do near (Ailments.baseDuration a * 1.5) (Ailments.duration Ctx.empty elemental a)
        for a in [ Freeze; Electrocute ] do near (Constants.buildupDuration a * 1.5) (Ailments.buildupSeconds Ctx.empty elemental a)
        for a in [ HeavyStun; Pin ] do near (Constants.buildupDuration a) (Ailments.buildupSeconds Ctx.empty elemental a)
        near (Ailments.baseDuration Poison) (Ailments.duration Ctx.empty elemental Poison)
        // a freeze lasts as long as "increased Freeze Duration" makes it, in a fight
        let frozen ms =
            let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Cold, { Min = 1e9; Max = 1e9 } ]; BaseTime = 0.25; BaseCrit = 0.0; Mods = ms }
            let player = { Actor.make "Player" [] [ bolt ] with Rotation = [ { Use = "Bolt"; When = fun v -> v.Time < 0.1 } ] }
            let fight = Fight.single { EnemyDef.atLevel 83 1e15 with Mods = []; Attack = None } player 20.0
            fight, Map.tryFind "Frozen" (Sim.run fight 1u).Uptime |> Option.defaultValue 0.0
        let plainFight, plain = frozen []
        let longerFight, longer = frozen (mods "freeze_duration_+%" 50.0)
        near (Constants.buildupDuration Freeze) plain
        near (plain * 1.5) longer
        same plainFight seeds
        same longerFight seeds
        near 1.25 (Mods.mult Ctx.empty [ SkillDuration ] swift)
        near 0.7 (Mods.mult Ctx.empty [ SkillDuration ] (ModDb.ofList (mods "support_limit_skill_effect_duration_+%_final" -30.0)))
        // "20% increased Flask Effect Duration": the same amount over 1.2 times the time
        let flask = Defence.flask "Mana" (ModDb.ofList (mods "flask_duration_+%" 20.0)) (Some((100.0, 3.0, 10.0, 60.0), 0.0))
        near 3.6 flask.Value.Duration
        near 100.0 flask.Value.Amount
        // "Banner Skills have 50% increased Duration"
        let banner lines =
            let c = load { blank with Groups = [ group [ gem "War Banner" ] ]; ExtraLines = "+100 to Spirit" :: lines } Options.defaults
            Behaviours.banners db.Value c.Sheet c.Build |> List.map (fun b -> b.Duration)
        match banner [], banner [ "Banner Skills have 50% increased Duration" ] with
        | [ plain ], [ longer ] -> near (plain * 1.5) longer
        | a, b -> failwithf "banners %A %A" a b
        // "40% increased Minion Duration", with 10% increased skill effect duration: a zombie lasts 1.5 times as long
        let zombie (ms: Mod list) =
            let setup = { Gem = "Raise Zombie"; Level = 20; Quality = 0; StatSet = 1; Supports = [] }
            match Behaviours.summoner db.Value (ModDb.ofList ms) setup (Skill.make "Raise Zombie" []) with
            | [ { Reaction = WithCorpse(Summon(_, _, d), _) } ] -> d
            | r -> failwithf "zombie %A" r
        near (zombie [] * 1.5) (zombie (mods "base_minion_duration_+%" 40.0 @ mods "skill_effect_duration_+%" 10.0))
        // "Every 10 Rage also grants 5% increased Physical Damage"
        equal (box [ Mod.inc (Damage(OfType Physical)) 5.0 |> Mod.whenever (PerStack("Rage", 10.0)) ]) (box (mods "physical_damage_+%_per_10_rage" 5.0)))

    test "what minions have while the player is in some state is read off the player (OfOwner)" (fun () ->
        equal (box [ Mod.inc (ForMinions(Damage AllDamage)) 20.0 |> Mod.whenever (OfOwner(WhileBuff "HitRecently")) ]) (box (mods "minion_damage_+%_if_enemy_hit_recently" 20.0))
        equal (box [ Mod.inc (ForMinions(Damage AllDamage)) 20.0 |> Mod.whenever (OfOwner(Not(WhileBuff "FullMana"))) ]) (box (mods "support_minion_damage_spirit_cost_minion_damage_+%_while_missing_mana" 20.0))
        equal (box [ Mod.inc (ForMinions(Damage AllDamage)) 2.0 |> Mod.whenever (OfOwner(PerStack("Dex", 5.0))) ]) (box (mods "minion_damage_+%_per_5_dex" 2.0))
        // "Allies in your Presence ... while you ...": the player's state
        match mods "allies_in_presence_have_unholy_might_while_you_not_on_low_mana" 1.0 with
        | m :: _ -> equal (box [ OfOwner(Not(WhileBuff "LowMana")) ]) (box m.Conds)
        | [] -> failwith "nothing"
        // a herald among the persistent gems: "while you are affected by a Herald"
        let c = load { blank with Groups = [ group [ gem "Herald of Ash" ] ]; ExtraLines = [ "+100 to Spirit" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        if not (p.Resources |> List.exists (fun (k, _) -> k = "AffectedByAHerald")) then failwith "no herald state")

    test "recovery of the moment: regeneration in some state, Arcane Surge's mana or life regeneration, life regenerated when stunned" (fun () ->
        let at lines (st: (string * int) list) =
            let c = load { blank with ExtraLines = lines } Options.defaults
            let p = Setup.player c Options.defaults [] None
            p.VitalsAt.Value (Map.ofList st)
        // "Regenerate 2% of maximum Life per second while affected by any Damaging Ailment"
        let line = [ "Regenerate 2% of maximum Life per second while affected by any Damaging Ailment" ]
        let plain, ailing = at line [], at line [ "DamagingAilment", 1 ]
        near (plain.LifeRegen + 0.02 * plain.Life) ailing.LifeRegen
        // "40% more Energy Shield Recharge Rate while on Low Energy Shield"
        let es = [ "+200 to maximum Energy Shield"; "40% more Energy Shield Recharge Rate while on Low Energy Shield" ]
        near ((at es []).EnergyShieldRecharge * 1.4) (at es [ "LowEnergyShield", 1 ]).EnergyShieldRecharge
        // Arcane Surge: 20% more mana regeneration, or life regeneration instead
        let surge = [ "Gain Arcane Surge when you deal a Critical Hit" ]
        near ((at surge []).ManaRegen * 1.2) (at surge [ "ArcaneSurge", 1 ]).ManaRegen
        // (a character with no way to Arcane Surge has no such line: its recovery never changes with its buffs)
        equal false (at [] []).RatesVary
        let regen = [ "Regenerate 10 Life per second"; "Arcane Surge grants more Life Regeneration Rate instead of Mana Regeneration Rate" ]
        near ((at regen []).LifeRegen * 1.2) (at regen [ "ArcaneSurge", 1 ]).LifeRegen
        near (at regen []).ManaRegen (at regen [ "ArcaneSurge", 1 ]).ManaRegen
        // "Regenerate 10% of maximum Life over 1 second when Stunned", with 20% increased life regeneration rate
        near 12.0 (at [ "Regenerate 10% of maximum Life over 1 second when Stunned"; "20% increased Life Regeneration rate" ] []).RegenOnStun
        // "50% faster start of Energy Shield Recharge if you've been Stunned Recently"
        let delay = [ "50% faster start of Energy Shield Recharge if you've been Stunned Recently" ]
        near ((at delay []).ESRechargeDelay / 1.5) (at delay [ "StunnedRecently", 1 ]).ESRechargeDelay)

    test "a state under another name is the state the fight keeps: not on Low Mana, you have Arcane Surge, Onslaught is active, holding a Shield" (fun () ->
        let conds id = match mods id 10.0 with m :: _ -> m.Conds | [] -> []
        equal (box [ Not(WhileBuff "LowMana"); SkillIs Attack ]) (box (conds "attack_speed_+%_while_not_on_low_mana"))
        equal (box [ WhileBuff "ArcaneSurge"; SkillIs Spell ]) (box (conds "spell_damage_+%_while_you_have_arcane_surge"))
        equal (box [ WhileBuff "Onslaught"; SkillIs Attack ]) (box (conds "attack_damage_+%_while_onslaught_active"))
        equal (box [ WhileBuff "WieldingShield"; SkillIs Attack ]) (box (conds "attack_damage_+%_while_holding_a_shield"))
        // a two handed weapon in hand is a state the fight starts with
        let c = load { blank with Items = [ "Weapon 1", "Rarity: Normal\nWrapped Quarterstaff\n--------\n" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        if not (p.Resources |> List.exists (fun (k, _) -> k = "WieldingTwoHandedWeapon")) then failwithf "no two handed weapon: %A" (p.Resources |> List.map fst))

    test "minion damage and attack speed on the player; grenades' cooldown recovery, cooldown uses and fuse" (fun () ->
        let sheetMods lines = (load { blank with ExtraLines = lines } Options.defaults).Sheet.Mods
        let own (ms: Mod list) stat = ms |> List.filter (fun m -> m.Stat = stat && m.Kind = Inc) |> List.sumBy (fun m -> m.Value)
        let lines = [ "Minions deal 30% increased Damage"; "Companions deal 20% increased Damage"; "Minions have 10% increased Attack Speed" ]
        near 0.0 (own (sheetMods lines) (Damage AllDamage))
        near 30.0 (own (sheetMods ("Increases and Reductions to Minion Damage also affect you" :: lines)) (Damage AllDamage))
        near 20.0 (own (sheetMods ("Increases and Reductions to Companion Damage also apply to you" :: lines)) (Damage AllDamage))
        let speed = sheetMods ("Increases and Reductions to Minion Attack Speed also affect you" :: lines) |> List.filter (fun m -> m.Stat = Speed && m.Kind = Inc)
        equal (box [ [ SkillIs Attack ] ]) (box (speed |> List.map (fun m -> m.Conds)))
        equal (box [ Mod.inc CooldownRecovery 25.0 |> Mod.whenever (SkillIs(Tag "Grenade")) ]) (box (mods "grenade_skill_cooldown_speed_+%" 25.0))
        // "+1 Cooldown Use": a cooldown skill with one stored use is used twice at the start
        let bomb = { Skill.make "Bomb" [ Spell; Tag "Grenade" ] with BaseDamage = Map.ofList [ Fire, { Min = 10.0; Max = 10.0 } ]; BaseTime = 0.5; Cooldown = Some 20.0; Mods = mods "grenade_skill_cooldown_count_+" 1.0 }
        let player = { Actor.make "Player" [] [ bomb ] with Rotation = [ { Use = "Bomb"; When = fun _ -> true } ] }
        let fight = Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } player 5.0
        equal 2 (Sim.run fight 1u).Counts.["uses:Bomb"]
        same fight seeds)

    test "Surrounded within a wider area; attack and cast speed if you've summoned a Totem Recently" (fun () ->
        let c = load { blank with ExtraLines = [ "50% increased Surrounded Area of Effect" ] } Options.defaults
        // 3 m, the area 1.5 times: the radius by the square root, cut off as a skill's
        near 36.0 c.Vitals.SurroundedRadius
        near 30.0 (load blank Options.defaults).Vitals.SurroundedRadius
        equal (box [ Mod.inc Speed 10.0 |> Mod.whenever (WhileBuff "SummonedTotemRecently") ]) (box (mods "attack_and_cast_speed_+%_on_placing_totem" 10.0))
        // placing a totem makes it so for 4 s, where something asks
        let totem = { Skill.make "Totem" [ Spell; Tag "Type:SummonsTotem" ] with Hits = [] }
        let sheet = { (load blank Options.defaults).Sheet with Mods = mods "attack_and_cast_speed_+%_on_placing_totem" 10.0 }
        match Behaviours.totemRecently sheet [ totem; Skill.make "Bolt" [ Spell ] ] with
        | [ { Filter = Called "Totem"; Reaction = GrantBuff ("SummonedTotemRecently", 4.0, 1, []) } ] -> ()
        | r -> failwithf "%A" r
        equal 0 (Behaviours.totemRecently (load blank Options.defaults).Sheet [ totem ]).Length)

    test "what sits in the sockets: per Idol in your Equipment, per non-Idol Augment" (fun () ->
        let helmet = "Helmet", "Rarity: Normal\nRusted Greathelm\n--------\nSockets filled: 3\nIdols socketed: 1\n"
        let gloves = "Gloves", "Rarity: Normal\nStocky Mitts\n--------\nSockets filled: 1\n"
        let lines = [ "+2% to all Elemental Resistances per non-Idol Augment in your Equipment"; "Companions deal 10% increased damage per Idol in your Equipment" ]
        let c = load { blank with Items = [ helmet; gloves ]; ExtraLines = lines } Options.defaults
        let plain = load { blank with Items = [ helmet; gloves ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        equal (box [ "NonIdolAugments", 3; "SocketedIdols", 1 ]) (box (p.Resources |> List.filter (fun (k, _) -> k = "SocketedIdols" || k = "NonIdolAugments") |> List.sortBy fst))
        // three augments that are not idols: +6% to each
        near (Map.find Fire plain.Vitals.Resist + 6.0) (Map.find Fire c.Vitals.Resist)
        equal (box [ OfOwner(PerStack("SocketedIdols", 1.0)); SkillIs(Tag "Companion") ]) (box (mods "companion_damage_+%_per_socketed_idol" 10.0 |> List.collect (fun m -> m.Conds))))

    test "physical damage prevented recouped as life: what armour kept off a hit comes back as life over 4 s" (fun () ->
        let sheet = { PlayerStats.basic 1000.0 100.0 with Armour = 1000.0; RecoupPreventedLife = 50.0; StartLife = 0.5 }
        let monster = { EnemyDef.atLevel 83 1e12 with Mods = []; Position = Vec.make 5.0 0.0; Attack = Some { Interval = 10.0; Damage = Map.ofList [ Physical, 100.0 ]; Accuracy = 1e9; Range = 10.0; CritChance = 0.0; Melee = true } }
        let idle = { Skill.make "Wait" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        let player = { Actor.make "Player" [] [ idle ] with Vitals = Some sheet }
        let o = Sim.run { Fight.single monster player 8.0 with MaxTime = 8.0 } 1u
        // (the life itself is not in the outcome: the fast engine is held to the reference's way of it)
        if o.DamageTaken <= 0.0 then failwith "never hit"
        same { Fight.single monster player 8.0 with MaxTime = 8.0 } seeds
        equal (box [ Mod.base' (Named "RecoupPreventedPhysicalAsLife") 7.0 ]) (box (mods "physical_damage_prevented_recouped_as_life_%" 7.0)))

    test "Headhunter: rares carry one to four modifiers of different families; a kill of one gives the player theirs for 60 s" (fun () ->
        // what a rare carries: one to four, no two of a family, of those a level 83 monster rolls
        let pool = RareMods.pool 83 |> List.map (fun (id, f, _) -> id, f) |> Map.ofList
        let rolls = [ for i in 1u .. 2000u -> RareMods.roll (Rng i) 83 ]
        for r in rolls do
            if r.Length < 1 || r.Length > 4 then failwithf "%d modifiers" r.Length
            if (r |> List.map (fun id -> pool.[id]) |> List.distinct).Length <> r.Length then failwithf "two of a family: %A" r
        // all four counts as likely: 2.5 on average
        let mean = rolls |> List.averageBy (fun r -> float r.Length)
        if abs (mean - 2.5) > 0.1 then failwithf "%.2f modifiers on average" mean
        // packs give their rares modifiers, and nothing else any
        let pack = Encounter.pack 83 Rarity.defaults [ Normal, 3; Magic, 2; Rare, 2 ] 35.0 25.0 false 1u
        for e in pack do
            if (e.Rarity = Rare) = e.Affixes.IsEmpty then failwithf "%s carries %A" e.Name e.Affixes
        // the stat: its time, and each modifier's player twin while its buff is up
        let hh = mods "gain_rare_monster_mods_on_kill_ms" 60000.0
        let buff = RareMods.buffName
        let has (m: Mod) = if not (hh |> List.exists (fun x -> x.Stat = m.Stat && x.Kind = m.Kind && x.Value = m.Value && x.Conds = m.Conds)) then failwithf "missing %A" m
        has (Mod.flag (Named "RareModsOnKill"))
        has (Mod.inc Speed 30.0 |> Mod.whenever (WhileBuff(buff "MonsterIncreasedSpeed1")))
        has (Mod.inc CritChance 300.0 |> Mod.whenever (WhileBuff(buff "MonsterCriticalStrikeChance1")))
        has (Mod.base' (Resist Fire) 50.0 |> Mod.whenever (WhileBuff(buff "MonsterFireResistance1")))
        has (Mod.base' (MaxResist Fire) 10.0 |> Mod.whenever (WhileBuff(buff "MonsterFireResistance1")))
        // an aura is for the minions too, while the player has it
        has (Mod.inc (Damage(OfType Physical)) 40.0 |> Mod.whenever (WhileBuff(buff "MonsterPhysicalDamageAura1")))
        has { Mod.inc (ForMinions(Damage(OfType Physical))) 40.0 with Conds = [ OfOwner(WhileBuff(buff "MonsterPhysicalDamageAura1")) ] }
        // Temporal Bubble slows the monsters around its bearer: nothing of it is the player's own
        if hh |> List.exists (fun m -> m.Conds = [ WhileBuff(buff "MonsterTemporalAura1") ]) then failwith "the Temporal Bubble on the player"
        // the item line makes the rule: a kill of a rare (or unique, which carries none) by the character
        let line = "When you kill a Rare monster, you gain its Modifiers for 60 seconds"
        let c = load { blank with ExtraLines = [ line ] } Options.defaults
        let plain = load blank Options.defaults
        let p = Setup.player c Options.defaults [] None
        match p.Rules |> List.filter (fun r -> match r.Reaction with GainRareMods _ -> true | _ -> false) with
        | [ { Hook = OnKillOf "RareOrUnique"; Filter = AnySkill; Chance = 100.0; Reaction = GainRareMods d } ] -> near 60.0 d
        | r -> failwithf "%A" r
        // defences while the buff is up: +50% fire resistance; 25% of the life before increases as energy shield
        let at buffs = Defence.computeWith (Map.ofList buffs) c.Bases c.Sheet
        near (Map.find Fire plain.Vitals.Resist + 50.0) (Map.find Fire (at [ buff "MonsterFireResistance1", 1 ]).Resist)
        near (plain.Vitals.EnergyShield + 0.25 * plain.Vitals.Life) (at [ buff "MonsterExtraEnergyShield1", 1 ]).EnergyShield
        near plain.Vitals.EnergyShield (at []).EnergyShield
        // in a fight: two swings of 100 kill the rare (150 life); every swing after hits the tough
        // one with 40% increased physical damage, 140, for 60 s
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let rule = { Hook = OnKillOf "RareOrUnique"; Filter = AnySkill; Chance = 100.0; Reaction = GainRareMods 60.0 }
        let player = { Actor.make "Player" (Mod.base' (Named "Accuracy") 1e9 :: (hh |> List.filter (fun m -> m.Stat <> Named "RareModsOnKill"))) [ swing ] with Rotation = [ Actor.always "Swing" ]; Rules = [ rule ] }
        let target name x life = { EnemyDef.atLevel 83 life with Name = name; Position = Vec.make x 0.0; Size = 4.0; Mods = []; Attack = None }
        let fight affixes =
            { Fight.single (target "tough" 12.0 1e9) player 8.5 with
                Enemies = [ { target "rare" 10.0 150.0 with Rarity = Rare; Affixes = affixes }; target "tough" 12.0 1e9 ]
                Targeting = Nearest }
        let o = Sim.run (fight [ "MonsterPhysicalDamageAura1"; "MonsterHinderAura1" ]) 1u
        let hits = count o "hits:Swing"
        equal 2 (count o "rare modifiers gained")
        near (150.0 + float (hits - 2) * 140.0) o.DamageBy.["Swing (hit)"]
        // a rare with nothing of the kind: 100 a swing
        let bare = Sim.run (fight [ "MonsterHinderAura1" ]) 1u
        near (150.0 + float (count bare "hits:Swing" - 2) * 100.0) bare.DamageBy.["Swing (hit)"]
        same (fight [ "MonsterPhysicalDamageAura1"; "MonsterIncreasedSpeed1"; "MonsterFireResistance1" ]) seeds)

    test "Affinity: Power Charges for Frenzy, Frenzy for Endurance, Endurance for Power; what spends them is as it was" (fun () ->
        equal (box [ Mod.flag (Named "ChargeCycle") ]) (box (mods "keystone_charge_cycle" 1.0))
        let sheet = { (load blank Options.defaults).Sheet with Mods = mods "keystone_charge_cycle" 1.0 }
        let rule r = { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = r }
        let rules =
            [ rule (GainResource("FrenzyCharge", 1)); rule (GainResource("EnduranceCharge", 2)); rule (GrantBuff("PowerCharge", 10.0, 3, []))
              rule (SpendStack("PowerCharge", GainResource("FrenzyCharge", 1))); rule (GainResource("Rage", 5)) ]
        let periodics = [ { Name = "dynamism"; Every = 2.0; Action = GainRandom [ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ] } ]
        let rules', periodics' = Behaviours.affinity sheet rules periodics
        equal
            (box [ GainResource("PowerCharge", 1); GainResource("FrenzyCharge", 2); GrantBuff("EnduranceCharge", 10.0, 3, [])
                   SpendStack("PowerCharge", GainResource("PowerCharge", 1)); GainResource("Rage", 5) ])
            (box (rules' |> List.map (fun r -> r.Reaction)))
        equal (box (GainRandom [ "EnduranceCharge"; "PowerCharge"; "FrenzyCharge" ])) (box periodics'.Head.Action)
        // without the keystone nothing changes
        equal (box rules) (box (fst (Behaviours.affinity (load blank Options.defaults).Sheet rules periodics)))
        // the keystone's line on a character
        let c = load { blank with ExtraLines = [ "Affinity" ] } Options.defaults
        equal true (Mods.flag Ctx.empty (Named "ChargeCycle") (ModDb.ofList c.Sheet.Mods)))

    test "life recovery from flasks also applies to energy shield" (fun () ->
        equal (box [ Mod.flag (Named "LifeFlaskAlsoES") ]) (box (mods "life_recovery_from_flasks_also_recovers_energy_shield" 1.0))
        let c = load { blank with ExtraLines = [ "Life Recovery from Flasks also applies to Energy Shield" ] } Options.defaults
        equal true c.Vitals.LifeFlaskAlsoES
        equal false (load blank Options.defaults).Vitals.LifeFlaskAlsoES
        // one hit of 1200 takes the 500 energy shield and leaves 300 of 1000 life: the flask is
        // drunk (150 at once, 150 over 3 s). With the line the shield gets as much, so it is
        // never empty again, and a mod of the player's that holds while it has energy shield
        // holds for every bolt
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let flask = { Amount = 300.0; Duration = 3.0; ChargesPerUse = 10.0; MaxCharges = 60.0; ChargesPerSecond = 0.0; ChargeGainMult = 1.0; Instant = 0.5 }
        let vitals also = { PlayerStats.basic 1000.0 0.0 with EnergyShield = 500.0; LifeFlask = Some flask; LifeFlaskAlsoES = also }
        let brute =
            { EnemyDef.atLevel 83 1e12 with
                Position = Vec.make 5.0 0.0; Mods = []
                Attack = Some { Interval = 9.0; Damage = Map.ofList [ Physical, 1200.0 ]; Accuracy = 1e9; Range = 10.0; CritChance = 0.0; Melee = false } }
        let fight also =
            let player = { Actor.make "Player" [ Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (WhileBuff "HasEnergyShield") ] [ bolt ] with Vitals = Some(vitals also) }
            Fight.single brute player 9.0
        let o = Sim.run (fight true) 1u
        equal 1 (count o "enemy:hit")
        equal 1 (count o "flask:life")
        near (200.0 * float (count o "hits:Bolt")) o.DamageBy.["Bolt (hit)"]
        let without = Sim.run (fight false) 1u
        if without.DamageBy.["Bolt (hit)"] >= 200.0 * float (count without "hits:Bolt") then failwith "the shield stayed up without the line"
        same (fight true) seeds)

    test "slow magnitude: the Maim and Hinder the character inflicts slow that much more" (fun () ->
        equal (box [ Mod.inc (Named "SlowMagnitude") 100.0 ]) (box (mods "base_debuff_slow_magnitude_+%" 100.0))
        // the walker of the Presence test (10 m away, 2 m a second), Hindered by every bolt from
        // the first (0.5 s, at 9 m): 30% slower, it reaches into a Presence of 4.1 m (its centre
        // at 4.6 m, 4.4 m on) after 0.5 + 44 / 14 = 3.64 s, and the hits from 4.0 s deal double;
        // with 100% increased slow magnitude 60% slower: 0.5 + 44 / 8 = 6.0 s, after the fight
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let walker =
            { EnemyDef.make Rarity.defaults 70 Normal "walker" (Vec.make 100.0 0.0) with
                Life = 1e9; Mods = []; Size = 5.0; Speed = 20.0
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 1.0 ]; Accuracy = 100.0; Range = 6.0; CritChance = 0.0; Melee = true } }
        let fight extra =
            let player = { Actor.make "Player" (Mod.base' (AilmentChance Hinder) 100.0 :: extra) [ bolt ] with Vitals = Some(PlayerStats.basic 1e6 0.0) }
            { Fight.single walker player 5.2 with Presence = 41.0; PresenceAuras = [ "Weakened", 1, [ Mod.inc (DamageTaken AllDamage) 100.0 ] ] }
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near (7.0 * 100.0 + 3.0 * 200.0) (dealt (fight []))
        near (10.0 * 100.0) (dealt (fight (mods "base_debuff_slow_magnitude_+%" 100.0)))
        same (fight (mods "base_debuff_slow_magnitude_+%" 100.0)) seeds)

    test "evasion if energy shield recharge started in the past 2 seconds: from the end of its delay since the last hit, for 2 s" (fun () ->
        equal
            (box [ Mod.inc Evasion 40.0 |> Mod.whenever (WhileBuff "ESRechargeStartedRecently") ])
            (box (mods "evasion_rating_+%_if_energy_shield_recharge_started_in_past_2_seconds" 40.0))
        // a critical hit at 0.5 s Ignites the character for 4 s (damage over time keeps the
        // recharge from starting: the last of it at the bolt at 4.5 s); with a delay of 3.9 s the
        // recharge starts at 8.4 s, so the state holds for the bolts at 8.5, 9.0, 9.5 and 10.0 s,
        // which a mod doubles: of 23 bolts of 100, four deal 200
        let spark = { Skill.make "Spark" [ Spell ] with BaseDamage = Map.empty; BaseTime = 0.5; BaseCrit = 100.0; Cooldown = Some 100.0 }
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let vitals = { PlayerStats.basic 1e6 0.0 with EnergyShield = 100.0; ESRechargeDelay = 3.9 }
        let doubled = Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (WhileBuff "ESRechargeStartedRecently")
        let player =
            { Actor.make "Player" [ doubled; Mod.more (AilmentChance Ignite) -100.0 ] [ spark; bolt ] with
                Vitals = Some vitals
                Rotation = [ Actor.always "Spark"; Actor.always "Bolt" ]
                Rules = [ { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = IgniteSelf 0.1 } ] }
        let fight = Fight.single (EnemyDef.atLevel 83 1e12) player 12.2
        let o = Sim.run fight 1u
        equal 23 (count o "hits:Bolt")
        near (19.0 * 100.0 + 4.0 * 200.0) o.DamageBy.["Bolt (hit)"]
        // without energy shield there is nothing to recharge
        near 2300.0 (Sim.run { fight with Player = { player with Vitals = Some { vitals with EnergyShield = 0.0 } } } 1u).DamageBy.["Bolt (hit)"]
        same fight seeds)

    test "states of a while ago, kept where a stat asks: stunned an enemy, a melee hit in the past 8 s, dodge rolled, counts with their most" (fun () ->
        equal (box [ Mod.inc AreaOfEffect 15.0 |> Mod.whenever (WhileBuff "StunnedEnemyRecently") ]) (box (mods "area_of_effect_+%_if_have_stunned_an_enemy_recently" 15.0))
        equal (box [ Mod.inc Evasion 30.0 |> Mod.whenever (WhileBuff "DodgeRolledRecently") ]) (box (mods "evasion_rating_+%_if_you_dodge_rolled_recently" 30.0))
        equal (box [ Mod.inc Evasion 30.0 |> Mod.whenever (WhileBuff "HitRecently") ]) (box (mods "evasion_rating_+%_if_you_have_hit_an_enemy_recently" 30.0))
        equal (box [ Mod.inc Armour 30.0 |> Mod.whenever (Not(WhileBuff "BeenHitRecently")) ]) (box (mods "armour_+%_if_you_havent_been_hit_recently" 30.0))
        equal (box [ Mod.inc (Named "StunThreshold") 30.0 |> Mod.whenever (Not(WhileBuff "StunnedRecently")) ]) (box (mods "stun_threshold_+%_when_not_stunned_recently" 30.0))
        equal (box [ Mod.inc (Named "StunThreshold") 10.0 |> Mod.whenever (PerStack("TimesHitRecently", 1.0)) ]) (box (mods "stun_threshold_+%_for_each_time_hit_recently_up_to_100%" 10.0))
        equal
            (box [ Mod.inc AreaOfEffect 8.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (PerStack("IgnitedInLast8Seconds", 1.0)) ])
            (box (mods "attack_area_of_effect_+%_per_enemy_ignited_last_8_seconds" 8.0))
        equal (box [ Mod.inc (AilmentDuration(Some Poison)) 5.0 |> Mod.whenever (PerStack("PoisonsRecently", 1.0)) ]) (box (mods "poison_duration_+%_per_poison_applied_recently" 5.0))
        equal (box [ Mod.inc SkillDuration 8.0 |> Mod.whenever (PerStack("FrozenInLast8Seconds", 1.0)) ]) (box (mods "skill_effect_duration_+%_per_enemy_frozen_last_8_seconds" 8.0))
        equal (box [ Mod.inc (Damage AllDamage) 6.0 |> Mod.whenever (PerStack("WarcriesRecently", 1.0)) ]) (box (mods "damage_+%_per_warcry_used_recently" 6.0))
        let melee = mods "projectile_damage_+%_if_youve_dealt_melee_hit_recently" 50.0
        equal (box [ Mod.inc (Damage AllDamage) 50.0 |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (WhileBuff "MeleeHitInPast8Seconds") ]) (box melee)
        // the rules: only for what is asked, a count with the stacks its "up to" leaves room for
        let sheet = (load blank Options.defaults).Sheet
        equal 0 (Behaviours.recentStates sheet []).Length
        let asked =
            { sheet with
                Mods =
                    melee @ mods "stun_threshold_+%_for_each_time_hit_recently_up_to_100%" 15.0 @ mods "attack_area_of_effect_+%_per_enemy_ignited_last_8_seconds" 8.0
                    @ mods "area_of_effect_+%_if_have_stunned_an_enemy_recently" 15.0 }
        equal
            (box [ { Hook = OnInflict HeavyStun; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("StunnedEnemyRecently", 4.0, 1, []) }
                   { Hook = OnHit; Filter = WithTag Melee; Chance = 100.0; Reaction = GrantBuff("MeleeHitInPast8Seconds", 8.0, 1, []) }
                   { Hook = OnInflict Ignite; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("IgnitedInLast8Seconds", 8.0, 5, []) }
                   { Hook = WhenHit; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("TimesHitRecently", 4.0, 6, []) } ])
            (box (Behaviours.recentStates asked []))
        // a jab at 1 s, then shots every 0.7 s: those up to 9 s (eleven) deal 50% more, the four after it do not
        let jab = { Skill.make "Jab" [ Spell; Melee ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseCrit = 0.0; Cooldown = Some 100.0 }
        let shot = { Skill.make "Shot" [ Spell; Projectile ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.7; BaseCrit = 0.0 }
        let player =
            { Actor.make "Player" melee [ jab; shot ] with
                Rotation = [ Actor.always "Jab"; Actor.always "Shot" ]
                Rules = Behaviours.recentStates { sheet with Mods = melee } [] }
        let fight = Fight.single (EnemyDef.atLevel 83 1e12) player 12.0
        let o = Sim.run fight 1u
        equal 15 (count o "hits:Shot")
        near (11.0 * 150.0 + 4.0 * 100.0) o.DamageBy.["Shot (hit)"]
        same fight seeds)

    test "stats known by what they are: damage against ailing enemies, speed with elemental skills, faster Bleeding, what the player cannot suffer" (fun () ->
        equal (box [ Mod.inc (Damage AllDamage) 20.0 |> Mod.whenever (EnemyHas "AnyAilment") ]) (box (mods "hit_damage_+%_vs_enemies_affected_by_ailments" 20.0))
        equal (box [ Mod.inc Speed 3.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (PerStack("Dex", 20.0)) ]) (box (mods "attack_speed_+%_per_20_dex" 3.0))
        equal (box [ Mod.base' (AilmentChance Bleed) 25.0 |> Mod.whenever IsCrit ]) (box (mods "bleed_on_crit_%" 25.0))
        equal (box [ Mod.base' CritChance 1.5 |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag "Fire")) ]) (box (mods "fire_spell_additional_critical_strike_chance_permyriad" 150.0))
        equal (box [ Mod.inc (AilmentMagnitude(Some Poison)) 30.0 |> Mod.whenever (Not(EnemyHas "Poisoned")) ]) (box (mods "poison_effect_+%_vs_non_poisoned_enemies" 30.0))
        equal (box [ Mod.base' (Named "Avoid:Poison") 100.0 ]) (box (mods "immune_to_poison" 1.0))
        equal (box [ Mod.more (Named "SelfEffect:Ignite") -100.0 ]) (box (mods "unaffected_by_damaging_ailments" 1.0))
        // a skill of two elements is faster once
        let speed tags = Mods.inc { Ctx.empty with SkillTags = Set.ofList tags } [ Speed ] (ModDb.ofList (mods "attack_and_cast_speed_+%_with_elemental_skills" 10.0))
        near 1.1 (speed [ Tag "Fire"; Tag "Cold" ])
        near 1.1 (speed [ Tag "Lightning" ])
        near 1.0 (speed [ Tag "Chaos" ])
        // Bleeding that deals its damage 25% faster: 1.25 times the damage a second for 1 / 1.25 of the time
        let ctx = { Ctx.empty with SkillTags = Set.ofList [ Attack ] }
        let hit = { Crit = false; Unmitigated = Map.ofList [ Physical, 1000.0 ]; Dealt = Map.ofList [ Physical, 1000.0 ] }
        let bleeds extra =
            Ailments.fromHit (Rng 1u) ctx (ModDb.ofList (Mod.base' (AilmentChance Bleed) 100.0 :: extra)) { Level = 83; Mods = ModDb.ofList [] } hit
            |> List.pick (function Dot (Bleed, _, dps, seconds, _) -> Some(dps, seconds) | _ -> None)
        let dps, seconds = bleeds []
        let fastDps, fastSeconds = bleeds (mods "faster_bleed_%" 25.0)
        near (dps * 1.25) fastDps
        near (seconds / 1.25) fastSeconds
        // "Onslaught" all the time: its 20% increased skill speed and the state by its name
        let c = load { blank with ExtraLines = [ "Onslaught" ] } Options.defaults
        let p = Setup.player c Options.defaults [] None
        equal (box [ "Onslaught", 1 ]) (box (p.Resources |> List.filter (fun (k, _) -> k = "Onslaught")))
        near 1.2 (Mods.inc Ctx.empty [ Speed ] (ModDb.ofList p.Mods)))

    test "Lose X% of maximum Life and Energy Shield when you use a Chaos Skill: those two, not the mana, and not for other skills" (fun () ->
        equal (box [ Mod.base' (Named "LoseLifeAndESOnUse") 5.0 |> Mod.whenever (SkillIs(Tag "Chaos")) ]) (box (mods "lose_%_of_life_and_energy_shield_when_you_use_a_chaos_skill" 5.0))
        let bolt tags = { Skill.make "Bolt" (Spell :: tags) with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseCrit = 0.0 }
        let fight tags =
            let player = { Actor.make "Player" (mods "lose_%_of_life_and_energy_shield_when_you_use_a_chaos_skill" 5.0) [ bolt tags ] with Vitals = Some { PlayerStats.basic 1000.0 100.0 with LifeRegen = 0.0 } }
            Fight.single (EnemyDef.atLevel 83 1e12) player 4.5
        let o = Sim.run (fight [ Tag "Chaos" ]) 1u
        // five uses begun in 4.5 s
        equal 5 (count o "pool loss on use")
        equal 0 (count (Sim.run (fight [ Tag "Fire" ]) 1u) "pool loss on use")
        same (fight [ Tag "Chaos" ]) seeds
        same (fight [ Tag "Fire" ]) seeds)

    test "Thorns: a hit that is neither attack nor spell retaliates against melee hits with the character's Thorns damage" (fun () ->
        let thornsMods = mods "thorns_minimum_base_physical_damage" 100.0 @ mods "thorns_maximum_base_physical_damage" 100.0
        equal
            (box [ Mod.base' (AddedMin Physical) 100.0 |> Mod.whenever (SkillIs(Tag "Thorns")); Mod.base' (AddedMax Physical) 100.0 |> Mod.whenever (SkillIs(Tag "Thorns")) ])
            (box thornsMods)
        equal (box [ Mod.inc (Damage AllDamage) 40.0 |> Mod.whenever (SkillIs(Tag "Thorns")) ]) (box (mods "thorns_damage_+%" 40.0))
        let sheet = (load blank Options.defaults).Sheet
        equal (box None) (box (Behaviours.thorns sheet))
        let fight melee extra =
            let sheet = { sheet with Mods = thornsMods @ extra }
            let skill, rule = (Behaviours.thorns sheet).Value
            let monster =
                { EnemyDef.atLevel 83 1e12 with
                    Mods = []; Position = Vec.make 5.0 0.0
                    Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 10.0 ]; Accuracy = 1e9; Range = 10.0; CritChance = 0.0; Melee = melee } }
            let idle = { Skill.make "Wait" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
            let player = { Actor.make "Player" sheet.Mods [ idle; skill ] with Vitals = Some(PlayerStats.basic 1e6 0.0); Rotation = [ Actor.always "Wait" ]; Rules = [ rule ] }
            Fight.single monster player 10.5
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        let hits = count (Sim.run (fight true []) 1u) "hits:Thorns"
        if hits < 9 || hits > 11 then failwithf "%d thorns hits" hits
        // every hit the monster lands is answered with 100, 140 with 40% increased Thorns damage
        near (float hits * 100.0) (dealt (fight true []))
        near (float hits * 140.0) (dealt (fight true (mods "thorns_damage_+%" 40.0)))
        // not against a hit that is no melee attack, unless Thorns retaliate against all hits
        near 0.0 (dealt (fight false []))
        near (float hits * 100.0) (dealt (fight false (mods "thorns_proc_off_any_hit" 1.0)))
        same (fight true (mods "thorns_damage_+%" 40.0)) seeds
        same (fight false (mods "thorns_proc_off_any_hit" 1.0)) seeds)

    test "Scavenged Plating: Fully Breaking Armour gives stacks by the enemy's rarity for the gem's time; an Endurance Charge when a skill Fully Breaks Armour" (fun () ->
        equal (box [ Mod.more Armour 4.0 |> Mod.whenever (PerStack("ScavengedPlating", 1.0)) ]) (box (mods "scavenged_plating_armour_+%_final_per_stack" 4.0))
        let sheet = { (load blank Options.defaults).Sheet with Mods = mods "scavenged_plating_maximum_stacks_display" 10.0 }
        let rules = Behaviours.scavengedPlating db.Value sheet blank
        equal 11 rules.Length
        equal 0 (Behaviours.scavengedPlating db.Value (load blank Options.defaults).Sheet blank).Length
        // the first bolt (0.7 s) fully breaks the armour; the stacks last 8 s: the eleven bolts from
        // 1.4 s to 8.4 s deal more for each stack, the five after it do not (the armour stays
        // broken while it is hit, so it is not Fully Broken anew)
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.7; BaseCrit = 0.0 }
        let perStack = Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (PerStack("ScavengedPlating", 1.0))
        let fight rarity =
            let player = { Actor.make "Player" [ perStack; Mod.base' (Named "ArmourBreakFlat") 1e9 ] [ bolt ] with Rules = rules }
            Fight.single { EnemyDef.atLevel 83 1e12 with Rarity = rarity } player 12.0
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near (6.0 * 100.0 + 11.0 * 1100.0) (dealt (fight Unique))
        near (6.0 * 100.0 + 11.0 * 600.0) (dealt (fight Rare))
        near (6.0 * 100.0 + 11.0 * 300.0) (dealt (fight Magic))
        near (6.0 * 100.0 + 11.0 * 200.0) (dealt (fight Normal))
        same (fight Unique) seeds
        same (fight Magic) seeds
        let smash = { Skill.make "Smash" [ Attack ] with Mods = mods "chance_to_gain_endurance_charge_on_armour_break_%" 30.0 }
        equal
            (box [ { Hook = OnFullBreak; Filter = Called "Smash"; Chance = 30.0; Reaction = GainResource("EnduranceCharge", 1) } ])
            (box (Behaviours.fullBreakRules [ smash; bolt ])))

    test "what gems say of their own skill: ailment magnitudes, damage against a Unique or an enemy with Fully Broken Armour, chaos damage breaking armour, Blazing Critical" (fun () ->
        equal (box [ Mod.more (AilmentMagnitude(Some Poison)) 30.0 ]) (box (mods "active_skill_poison_effect_+%_final" 30.0))
        equal (box [ Mod.more (AilmentMagnitude(Some Ignite)) 30.0 ]) (box (mods "support_stronger_ignites_ignite_effect_+%_final" 30.0))
        equal (box [ Mod.more (Buildup Freeze) 30.0 ]) (box (mods "support_hypothermia_hit_damage_freeze_multiplier_+%_final" 30.0))
        equal (box [ Mod.more (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "Unique") ]) (box (mods "boneshatter_damage_+%_final_if_created_from_unique" 30.0))
        equal (box [ Mod.more (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "FullyBrokenArmour") ]) (box (mods "active_skill_damage_+%_final_vs_fully_broken_armour" 30.0))
        // 30% more against a Unique enemy, nothing against a Rare one
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseCrit = 0.0 }
        let fight rarity extra =
            Fight.single { EnemyDef.atLevel 83 1e12 with Rarity = rarity } (Actor.make "Player" extra [ bolt ]) 5.5
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        near 650.0 (dealt (fight Unique (mods "boneshatter_damage_+%_final_if_created_from_unique" 30.0)))
        near 500.0 (dealt (fight Rare (mods "boneshatter_damage_+%_final_if_created_from_unique" 30.0)))
        same (fight Unique (mods "boneshatter_damage_+%_final_if_created_from_unique" 30.0)) seeds
        // chaos damage that breaks armour: 100 chaos dealt, 50% of it, three times against a Normal monster: 150 a hit
        equal (box [ Mod.base' (Named "ArmourBreakOfChaos") 50.0 ]) (box (mods "chaos_damage_%_dealt_as_armour_break" 50.0))
        let armoured = { EnemyDef.atLevel 83 1e12 with Rarity = Normal; Mods = [ Mod.base' Armour 400.0 ] }
        let breaking = Fight.single armoured (Actor.make "Player" (mods "chaos_damage_%_dealt_as_armour_break" 50.0 @ [ Mod.more (Damage AllDamage) 100.0 |> Mod.whenever (EnemyHas "FullyBrokenArmour") ]) [ bolt ]) 5.5
        // 400 armour is gone with the third hit (450 broken): the fourth and fifth deal double
        near (3.0 * 100.0 + 2.0 * 200.0) (dealt breaking)
        same breaking seeds
        // Blazing Critical: a critical hit of the supported skill gives the character's attacks fire damage for a while
        let c = load { blank with Groups = [ group [ gem "Spark"; support "Blazing Critical" ] ] } Options.defaults
        match Setup.pressed c Options.defaults "Spark" with
        | Some p ->
            match p.Rules |> List.tryPick (fun r -> match r.Hook, r.Reaction with OnCrit, GrantBuff ("Blazing Critical", seconds, 1, ms) -> Some(seconds, ms) | _ -> None) with
            | Some (seconds, ms) ->
                if seconds <= 0.0 then failwith "no duration"
                equal (box [ for t in DamageType.all -> GainAs(t, Fire) ]) (box (ms |> List.map (fun m -> m.Stat)))
                equal (box [ SkillIs Attack ]) (box (ms |> List.collect (fun m -> m.Conds) |> List.distinct))
            | None -> failwith "no Blazing Critical rule"
        | None -> failwith "Spark not built")

    test "Entangle: the fissure hits as it is made; while an enemy it hit is near one, a vine attaches every third of a second" (fun () ->
        let o = { Options.defaults with MaxTime = 10.05; Immortal = true }
        let c = load { blank with Groups = [ group [ gem "Entangle" ] ] } o
        let p = (Setup.pressed c o "Entangle").Value
        equal 0 p.Skill.Parts.Length
        equal (box [ "Entangle: Vines" ]) (box (p.Extra |> List.map (fun x -> x.Name)))
        // 3 vines on an enemy, each for 1 s: one every third of a second (not every 0.1 s)
        match p.Periodics with
        | [ { Every = every; Action = TriggerNamed "Entangle: Vines" } ] -> near (1.0 / 3.0) every
        | x -> failwithf "%A" x
        // the enemy is marked for the fissure's 8 s, and slowed by three vines of 15% each
        match p.Rules |> List.tryPick (fun r -> match r.Reaction with ApplyDebuff ("Entangled", seconds, 1, ms) -> Some(seconds, ms) | _ -> None) with
        | Some (seconds, ms) ->
            near 8.0 seconds
            equal (box [ Mod.more (Named "MovementSpeed") -45.0 ]) (box ms)
        | None -> failwith "no Entangled rule"
        let fight = Setup.fight c o (Setup.player c o [ Spam "Entangle" ] None) (Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 }))
        let out = Sim.run fight 1u
        // the first fissure lands at 0.9 s; the vines' clock strikes at 1.0 s, 1.33 s, ... 10.0 s: 28 times
        equal 28 (count out "hits:Entangle: Vines")
        // a cast every 0.9 s: 11 in 10 s
        equal 11 (count out "hits:Entangle")
        same fight seeds)

    test "secondary radius as a width or an impact: Entangle's fissure and Primal Strikes' charge are strips, a Thunderstorm strike has its impact radius" (fun () ->
        let built gems name =
            let c = load { blank with Groups = [ group gems ]; Items = [ "Weapon 1", "Rarity: Normal\nHardwood Spear\n" ] } Options.defaults
            match Setup.pressed c Options.defaults name with
            | Some p -> p.Skill
            | None -> failwithf "%s not built" name
        // "Fissure length is 8 metres", vines within 1.5 metres to either side
        equal (box (Cone(80.0, 0.0, 30.0))) (box (built [ gem "Entangle" ] "Entangle").Delivery)
        // "Storm radius is 2.3 metres", "+0.8 metres to Impact radius": a strike hits what is within 0.8 m of its target
        equal (box (AreaAtTarget 8.0)) (box (built [ gem "Thunderstorm" ] "Thunderstorm").Delivery)
        // "Charge length is 8.6 metres", "Charge width is 1.8 metres"
        let primal = built [ gem "Primal Strikes" ] "Primal Strikes"
        match primal.Parts |> List.tryFind (fun p -> p.Name.EndsWith "Spirits") with
        | Some spirits -> equal (box (Cone(86.0, 0.0, 18.0))) (box spirits.Delivery)
        | None -> failwithf "no Spirits part: %A" (primal.Parts |> List.map (fun p -> p.Name)))

    test "Arctic Armour: stages build up; a melee hit on the player sets off a burst of the gem's cold damage for every stage held and takes one stage" (fun () ->
        let o = { Options.defaults with MaxTime = 10.2 }
        let c = load { blank with Groups = [ group [ gem "Arctic Armour" ]; group [ gem "Spark" ] ]; ExtraLines = [ "+200 to Spirit" ] } o
        let raw = Gems.rawStats db.Value (ModDb.ofList []) { Gem = "Arctic Armour"; Level = 20; Quality = 0; StatSet = 1; Supports = [] } |> Map.ofList
        let lo, hi = raw.["arctic_armour_minimum_added_cold_damage_per_stack"], raw.["arctic_armour_maximum_added_cold_damage_per_stack"]
        let most, every = int raw.["maximum_number_of_arctic_armour_stationary_stacks"], raw.["base_active_skill_buff_stack_gain_frequency_ms"] / 1000.0
        match Behaviours.arcticArmour db.Value c.Sheet c.Build with
        | Some (skill, rules, clock, n) ->
            equal most n
            near every clock.Every
            equal 2 rules.Length
            equal (box [ lo; hi ]) (box (skill.Mods |> List.filter (fun m -> List.contains (PerStack("ArcticArmourStage", 1.0)) m.Conds) |> List.map (fun m -> m.Value)))
        | None -> failwith "no Arctic Armour"
        // full at the start of a fight
        let p = Setup.player c o [ Spam "Spark" ] None
        equal (box [ "ArcticArmourStage", most ]) (box (p.Resources |> List.filter (fun (k, _) -> k = "ArcticArmourStage")))
        // a brute that hits every 2.5 s (at 2.5, 5, 7.5 and 10 s): four bursts, the first with all the stages
        let brute =
            { EnemyDef.make Rarity.defaults 70 Unique "brute" (Vec.make 5.0 0.0) with
                Life = 1e12; Mods = []
                Attack = Some { Interval = 2.5; Damage = Map.ofList [ Physical, 1.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
        let fight = Setup.fight c o p [ brute ]
        let out = Sim.run fight 1u
        let bursts = count out "hits:Arctic Armour: Burst"
        if bursts < 3 || bursts > 5 then failwithf "%d bursts" bursts
        let dealt = out.DamageBy |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "Arctic Armour") |> List.sumBy snd
        // each burst deals between the least and the most of its stages (no stage is short for long: one comes back every few seconds)
        if dealt < float bursts * lo * float (most - 3) * 0.5 || dealt > float bursts * hi * float most * 4.0 then failwithf "%d bursts dealt %.0f (a stage %.0f to %.0f, %d stages)" bursts dealt lo hi most
        // a hit that is no melee attack sets off nothing
        let ranged = { brute with Attack = brute.Attack |> Option.map (fun a -> { a with Melee = false }) }
        equal 0 (count (Sim.run (Setup.fight c o p [ ranged ]) 1u) "hits:Arctic Armour: Burst")
        same fight seeds)

    test "Ancestral Bond doubles the totem limit as far as 75 Spirit a totem pays; more totems; what a totem has for every totem standing" (fun () ->
        equal (box [ Mod.flag (Named "AncestralBond") ]) (box (mods "keystone_ancestral_bond" 1.0))
        let totemsOf spirit extra =
            let b = { blank with Groups = [ group [ gem "Shockwave Totem" ] ]; ExtraLines = (if spirit > 0 then [ sprintf "+%d to Spirit" spirit ] else []) }
            let c = load b Options.defaults
            Behaviours.totems db.Value { c.Sheet with Mods = c.Sheet.Mods @ extra } b |> fst
        let bond = mods "keystone_ancestral_bond" 1.0
        equal 1 (totemsOf 0 []).Length
        // doubled, where there is Spirit for two; one at least
        equal 2 (totemsOf 200 bond).Length
        equal 1 (totemsOf 100 bond).Length
        equal 1 (totemsOf 0 bond).Length
        // "+1 to maximum number of Summoned Totems": two, four with the limit doubled and Spirit for four
        equal 2 (totemsOf 0 (mods "number_of_additional_totems_allowed" 1.0)).Length
        equal 4 (totemsOf 400 (bond @ mods "number_of_additional_totems_allowed" 1.0)).Length
        equal 3 (totemsOf 250 (bond @ mods "number_of_additional_totems_allowed" 1.0)).Length
        // "Spells Cast by Totems have 5% increased Cast Speed per Summoned Totem", "4% more Totem Damage per Summoned Totem": for two totems
        let two = totemsOf 0 (mods "number_of_additional_totems_allowed" 1.0 @ mods "totems_spells_cast_speed_+%_per_active_totem" 5.0 @ mods "totem_damage_+%_final_per_active_totem" 4.0)
        let own = two.Head.Skills.Head.Mods |> List.filter (fun m -> m.Source = "per Summoned Totem") |> List.map (fun m -> m.Stat, m.Kind, m.Value)
        equal (box [ Speed, Inc, 10.0; Damage AllDamage, More, 8.0 ]) (box own))

    test "damage taken, defences on low life, block while Surrounded, energy shield on block, counted Power of warcries, supports of a colour" (fun () ->
        equal (box [ Mod.more (Named "Taken:All") -10.0 ]) (box (mods "marauder_hidden_ascendancy_damage_taken_+%_final" -10.0))
        equal (box [ Mod.more (Named "Taken:All") 20.0 |> Mod.whenever (WhileBuff "LowLife") ]) (box (mods "hand_wraps_damage_taken_+%_final_on_low_life" 20.0))
        equal (box [ Mod.inc (Named "Block") 25.0 |> Mod.whenever (WhileBuff "Surrounded") ]) (box (mods "block_chance_+%_while_surrounded" 25.0))
        equal (box [ Mod.base' (MaxResist Fire) 2.0 |> Mod.whenever (WhileBuff "Supports5:red") ]) (box (mods "maximum_fire_resistance_+%_if_at_least_5_red_supports_socketed" 2.0))
        equal (box [ Mod.base' (Named "AllyRegenPercentOfOwnerRegen") 30.0 ]) (box (mods "total_base_life_regeneration_rate_per_minute_%_granted_to_allies_in_your_presence" 30.0))
        // 10% less damage taken on the sheet
        let plain = load blank Options.defaults
        let less = load { blank with ExtraLines = [ "Take 10% less Damage" ] } Options.defaults
        near (0.9 * Map.find Fire plain.Vitals.TakenMult) (Map.find Fire less.Vitals.TakenMult)
        // energy shield gained (or lost) on block: both engines the same
        equal (box [ Mod.base' (Named "ESOnBlock") 30.0 ]) (box (mods "energy_shield_gained_on_block" 30.0))
        let monster = { EnemyDef.atLevel 83 1e12 with Mods = []; Position = Vec.make 5.0 0.0; Attack = Some { Interval = 0.4; Damage = Map.ofList [ Fire, 60.0 ]; Accuracy = 1e9; Range = 10.0; CritChance = 0.0; Melee = true } }
        let idle = { Skill.make "Wait" [ Spell ] with Hits = []; Cooldown = Some 100.0 }
        for gain in [ 30.0; -30.0 ] do
            let vitals = { PlayerStats.basic 1e6 0.0 with EnergyShield = 300.0; Block = 50.0; ESOnBlock = gain }
            same (Fight.single monster { Actor.make "Player" [] [ idle ] with Vitals = Some vitals } 8.0) seeds
        // "50% increased total Power counted by Warcries": an attack for two thirds of the power, counted up to two thirds
        let c = load { blank with Groups = [ group [ gem "Infernal Cry" ] ] } Options.defaults
        let empower (c: Loaded) =
            (Setup.pressed c Options.defaults "Infernal Cry").Value.Rules |> List.pick (fun r -> match r.Reaction with Empower (_, per, cap) -> Some(per, cap) | _ -> None)
        let per, cap = empower c
        let per2, cap2 = empower { c with Sheet = { c.Sheet with Mods = c.Sheet.Mods @ mods "warcry_monster_power_+%" 50.0 } }
        near (per / 1.5) per2
        if cap < infinity then near (cap / 1.5) cap2)

    test "per 100 maximum Life, Armour per Strength, durations of plant, offering and totem skills" (fun () ->
        equal
            (box [ Mod.inc (Damage AllDamage) 2.0 |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxLife100", 1.0)) ])
            (box (mods "spell_damage_+%_per_100_max_life_with_non_channelling_skills" 2.0))
        equal (box [ Mod.inc SkillDuration 20.0 |> Mod.whenever (SkillIs(Tag "Plant")) ]) (box (mods "plant_skill_effect_duration_+%" 20.0))
        equal (box [ Mod.inc SkillDuration 20.0 |> Mod.whenever (SkillIs(Tag "Type:Offering")) ]) (box (mods "offering_duration_+%" 20.0))
        equal (box [ Mod.inc SkillDuration 20.0 |> Mod.whenever (SkillIs(Tag "Type:SummonsTotem")) ]) (box (mods "totem_duration_+%" 20.0))
        // the character's life in hundreds is there to count, and 1 Armour for each Strength
        let plain = load blank Options.defaults
        let c = load { blank with ExtraLines = [ "+1 to Armour per Strength" ] } Options.defaults
        equal (box [ "MaxLife100", int (c.Vitals.Life / 100.0) ]) (box (c.Resources |> List.filter (fun (k, _) -> k = "MaxLife100")))
        near (plain.Vitals.Armour + plain.Vitals.Str) c.Vitals.Armour
        // a totem placed with 20% increased Totem Duration stands 1.2 times as long
        let seconds (c: Loaded) =
            (Setup.pressed c Options.defaults "Shockwave Totem").Value.Rules |> List.pick (fun r -> match r.Reaction with GrantBuff (n, s, _, _) when n.EndsWith " buff" -> Some s | _ -> None)
        let t = load { blank with Groups = [ group [ gem "Shockwave Totem" ] ] } Options.defaults
        near (1.2 * seconds t) (seconds { t with Sheet = { t.Sheet with Mods = t.Sheet.Mods @ mods "totem_duration_+%" 20.0 } }))

    // ---- round 15: the word lists, limits, what is read by its ending ----

    test "word lists: no bare word makes a stat a non-combat one; what the words caught is mapped or reported" (fun () ->
        // loot stays loot; what scales with it, a charm's spirit and a light stun are no loot
        equal (box NotOffensive) (box (StatMap.meaning "base_item_found_rarity_+%" 10.0))
        equal (box NotOffensive) (box (StatMap.meaning "light_radius_+%" 10.0))
        equal (box Unmapped) (box (StatMap.meaning "damage_+%_per_1%_increased_item_found_quantity" 1.0))
        equal (box (Unsimulated "charms")) (box (StatMap.meaning "charm_possesed_by_bear_spirit_for_x_seconds_when_used" 10.0))
        equal (box (Unsimulated "charges")) (box (StatMap.meaning "gain_spirit_charge_on_kill_%_chance" 10.0))
        equal (box (Unsimulated "stun / ailment threshold")) (box (StatMap.meaning "always_light_stun" 1.0))
        equal (box Unmapped) (box (StatMap.meaning "despair_no_reservation" 1.0))
        equal (box Unmapped) (box (StatMap.meaning "witchhunter_up_to_damage_+%_final_against_targets_with_missing_focus" 30.0))
        // "Arrows Pierce an additional Target", "Projectiles Pierce 2 additional Targets"
        let arrow = (mods "arrow_base_number_of_targets_to_pierce" 1.0).Head
        equal (box Pierce) (box arrow.Stat)
        equal (box [ SkillIs Bow ]) (box arrow.Conds)
        near 2.0 (mods "projectile_base_number_of_targets_to_pierce" 2.0).Head.Value
        // "30% less Freeze duration on you while a Supported Skill is active"
        let freeze = (mods "support_freeze_protection_spirit_cost_freeze_duration_on_self_+%_final" -30.0).Head
        equal (box (Named "SelfDuration:Freeze", More, -30.0)) (box (freeze.Stat, freeze.Kind, freeze.Value)))

    test "limits: a standing skill is cast again a limit-th of its time after the last; +1 to Limit" (fun () ->
        let stands (c: Loaded) name =
            (Setup.pressed c Options.defaults name).Value.Rules |> List.pick (fun r -> match r.Reaction with GrantBuff (n, s, _, _) when n.EndsWith " buff" -> Some s | _ -> None)
        let c = load { blank with Groups = [ group [ gem "Firestorm" ]; group [ gem "Icestorm" ] ] } Options.defaults
        let duration name = (Setup.pressed c Options.defaults name).Value.Skill.Duration
        // Firestorm: "Limit 1 Firestorm"; Icestorm: icestorm_max_number_of_storms 3
        near (duration "Firestorm") (stands c "Firestorm")
        near (duration "Icestorm" / 3.0) (stands c "Icestorm")
        // "+1 to Limit for Elemental Skills": two firestorms, four icestorms
        let more = { c with Sheet = { c.Sheet with Mods = c.Sheet.Mods @ mods "elemental_skill_limit_+" 1.0 } }
        near (duration "Firestorm" / 2.0) (stands more "Firestorm")
        near (duration "Icestorm" / 4.0) (stands more "Icestorm"))

    test "gem stats read by their ending are not listed as not understood; skill types played for a skill" (fun () ->
        let cov name =
            match Gems.skill db.Value ModDb.empty None { Gem = name; Level = 20; Quality = 0; StatSet = 1; Supports = [] } with
            | Ok (_, cov) -> (cov.Unmapped |> List.map fst) @ (cov.Unsimulated |> List.map fst)
            | Error e -> failwith e
        let volcano, orb = cov "Volcano", cov "Orb of Storms"
        for id in [ "volcano_fires_projectiles_every_x_ms"; "volcano_maximum_number_of_stages"; "volcano_base_number_of_geysers_allowed"
                    "volcano_initial_explosion_damage_+%_final_per_additional_stage" ] do
            equal (box false) (box (List.contains id volcano))
        for id in [ "orb_of_storms_base_bolt_frequency_ms"; "orb_of_storms_maximum_number_of_hits"; "orb_of_storms_base_maximum_number_of_orbs"
                    "orb_of_storms_minimum_placement_range_%"; "skill_can_add_multiple_charges_per_action" ] do
            equal (box false) (box (List.contains id orb))
        // what the engine does not read stays listed
        equal (box true) (box (List.contains "volcano_delay_between_slams_ms" volcano))
        equal (box false) (box (List.contains "Hazard" (Gems.unmodelledTypes db.Value "Spearfield")))
        equal (box true) (box (List.contains "CreatesGroundEffect" (Gems.unmodelledTypes db.Value "Explosive Spear")))
        equal (box false) (box (List.contains "Orb" (Gems.unmodelledTypes db.Value "Orb of Storms"))))

    test "stats: elemental conversion, consumed states, culling under its other id, states of a while ago, spell increases for attacks" (fun () ->
        let cold = mods "non_skill_base_elemental_damage_%_to_convert_to_cold" 50.0
        equal (box (set [ Convert(Fire, Cold); Convert(Lightning, Cold) ])) (box (cold |> List.map (fun m -> m.Stat) |> Set.ofList))
        let frozen = mods "support_active_skill_consume_enemy_freeze_to_gain_damage_+%_final" 40.0
        equal (box [ Damage AllDamage; Named "Takes:Frozen" ]) (box (frozen |> List.map (fun m -> m.Stat)))
        equal (box [ EnemyHas "Frozen" ]) (box frozen.Head.Conds)
        equal (box (Named "CullingStrike")) (box (mods "kill_enemy_on_hit_if_under_10%_life" 1.0).Head.Stat)
        equal (box NotOffensive) (box (StatMap.meaning "damage_against_undead_+%" 20.0))
        equal (box NotOffensive) (box (StatMap.meaning "projectile_ballistic_angle_from_target_distance" 1.0))
        let c = load blank Options.defaults
        let sheet = { c.Sheet with Mods = c.Sheet.Mods @ mods "elemental_damage_+%_if_enemy_shocked_recently" 25.0 @ mods "attack_speed_+%_if_have_blocked_recently" 10.0 }
        let states = Behaviours.recentStates sheet [] |> List.choose (fun r -> match r.Hook, r.Reaction with h, GrantBuff (n, s, 1, _) -> Some(n, h, s) | _ -> None)
        equal (box true) (box (List.contains ("ShockedEnemyRecently", OnInflict Shock, 4.0) states))
        equal (box true) (box (List.contains ("BlockedRecently", OnBlock, 4.0) states))
        // "Increases and Reductions to Spell damage also apply to Attacks": 30% for spells is 30% for attacks too
        let wand = { c.Sheet with Mods = [ Mod.inc (Damage AllDamage) 30.0 |> Mod.whenever (SkillIs Spell); Mod.inc (Damage(OfType Fire)) 10.0 ] @ mods "additive_spell_damage_modifiers_apply_to_attack_damage" 1.0 }
        match Behaviours.minionIncreasesOnYou wand with
        | [ m ] -> equal (box (Damage AllDamage, Inc, 30.0, [ SkillIs Attack ])) (box (m.Stat, m.Kind, m.Value, m.Conds))
        | other -> failwithf "unexpected %A" other)

    test "minions: a support's more or less damage taken, additional physical damage reduction; Splash" (fun () ->
        let bodies gems lines =
            let c = load { blank with Groups = [ group gems ]; ExtraLines = lines } Options.defaults
            c.Minions |> List.choose (fun m -> m.Body)
        let plain = (bodies [ gem "Skeletal Warrior Minion" ] []).Head
        near 1.0 plain.Taken
        near 0.0 plain.PhysicalReduction
        // Feeding Frenzy I: "Minions from Supported Skills take 20% more Damage"; Meat Shield I: 35% less
        near 1.2 (bodies [ gem "Skeletal Warrior Minion"; support "Feeding Frenzy I" ] []).Head.Taken
        near 0.65 (bodies [ gem "Skeletal Warrior Minion"; support "Meat Shield I" ] []).Head.Taken
        let reduced = mods "minion_additional_physical_damage_reduction_%" 15.0
        equal (box (ForMinions(Named "AdditionalPhysicalReduction"))) (box reduced.Head.Stat)
        // a monster's hit of 100 physical and 100 fire on a minion without armour or resistances:
        // 200 as it is; 240 with 20% more taken; 100 x 0.85 + 100 with 15% physical reduction
        let body taken reduction : MinionBody =
            { Life = 1e6; EnergyShield = 0.0; Armour = 0.0; Evasion = 0.0; Resist = Map.empty; Melee = true; Size = 1.0; Reach = 12.0; Revive = None; Power = 1.0
              LifeRegen = 0.0; TakesForOwner = 0.0; Explodes = None; Taken = taken; PhysicalReduction = reduction }
        let bite = { Skill.make "Bite" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 1.0 ]; BaseCrit = 0.0 }
        let fight (b: MinionBody) =
            let wolf = { Actor.make "Wolf" [ Mod.base' (Named "Accuracy") 1e9 ] [ bite ] with Rotation = [ Actor.always "Bite" ]; Body = Some b }
            let enemy = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 100.0; Fire, 100.0 ]; Accuracy = 1e9; Range = 20.0; CritChance = 0.0; Melee = true } }
            { Fight.single enemy (Actor.make "Player" [] []) 5.5 with Minions = [ wolf ] }
        same (fight (body 1.0 0.0)) seeds
        same (fight (body 1.2 0.0)) seeds
        same (fight (body 1.0 15.0)) seeds
        // Splash: the strike hits the enemies within 1.5 m of the one it hit; a support's flag
        equal (box Structural) (box (StatMap.meaning "melee_splash" 1.0))
        near 15.0 Constants.splashRadius)

    test "culling: Thrill of the Kill, charges for the culled monster's rarity, a wider threshold after a cull" (fun () ->
        let swing = { Skill.make "Swing" [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, Range.fixedAt 100.0 ]; BaseCrit = 0.0; Mods = [ Mod.flag (Named "CullingStrike") ] }
        let rulesOf gemName supports = Behaviours.cullRules db.Value ModDb.empty { Gem = gemName; Level = 20; Quality = 0; StatSet = 1; Supports = supports } swing
        // Thrill of the Kill II: 25% of damage as lightning for attacks for 8 s, 40% increased shock chance
        match rulesOf "Cull the Weak" [ "Thrill of the Kill II", 1 ] |> List.choose (fun r -> match r.Hook, r.Reaction with OnKillOf "Cullable", GrantBuff ("Thrill of the Kill", s, 1, ms) -> Some(s, ms) | _ -> None) with
        | [ seconds, ms ] ->
            near 8.0 seconds
            equal (box true) (box (ms |> List.exists (fun m -> m.Stat = GainAs(Physical, Lightning) && m.Value = 25.0 && m.Conds = [ SkillIs Attack ])))
            equal (box true) (box (ms |> List.exists (fun m -> m.Stat = AilmentChance Shock && m.Kind = Inc && m.Value = 40.0)))
        | other -> failwithf "unexpected %A" other
        // Culling Strike II: 20% increased threshold for 20 s
        equal (box true)
            (box (rulesOf "Cull the Weak" [ "Culling Strike II", 1 ] |> List.exists (fun r -> match r.Reaction with GrantBuff (_, 20.0, 1, [ m ]) -> m.Stat = Named "CullingThreshold" && m.Value = 20.0 | _ -> false)))
        // Cull the Weak: 1 Frenzy Charge for a normal or magic monster, 2 for a rare, 3 for a unique one
        let rules = rulesOf "Cull the Weak" [ "Thrill of the Kill II", 1 ]
        let charges status = rules |> List.sumBy (fun r -> match r.Hook, r.Reaction with OnKillOf s, (GainResource("FrenzyCharge", n) | IfBuff (_, GainResource("FrenzyCharge", n))) when s = status -> n | _ -> 0)
        equal (box (1, 1, 1)) (box (charges "Cullable", charges "RareOrUnique", charges "Unique"))
        // in a fight: a rare of 101 life has 1 left after the first swing and is culled by the
        // second: 2 Frenzy Charges (each 100% increased damage here) and the Thrill, so every
        // swing after deals (100 + 25) x 3 to the tough one. A rare killed outright gives nothing
        let perCharge = Mod.inc (Damage AllDamage) 100.0 |> Mod.whenever (PerStack("FrenzyCharge", 1.0))
        let player (rs: Rule list) =
            { Actor.make "Player" [ Mod.base' (Named "Accuracy") 1e9; perCharge ] [ swing ] with
                Rotation = [ Actor.always "Swing" ]; Rules = rs; MaxResources = Map.ofList [ "FrenzyCharge", 3 ] }
        let target name x life = { EnemyDef.atLevel 83 life with Name = name; Position = Vec.make x 0.0; Size = 4.0; Mods = []; Attack = None }
        let fight rs rareLife =
            { Fight.single (target "tough" 12.0 1e9) (player rs) 6.5 with
                Enemies = [ { target "rare" 10.0 rareLife with Rarity = Rare }; target "tough" 12.0 1e9 ]
                Targeting = Nearest }
        let onTough (o: Outcome) kills = (count o "hits:Swing" - kills, o.DamageBy.["Swing (hit)"])
        let bareHits, bareTotal = onTough (Sim.run (fight [] 101.0) 1u) 2
        let onRare = bareTotal - float bareHits * 100.0
        let hits, total = onTough (Sim.run (fight rules 101.0) 1u) 2
        near (onRare + float hits * 375.0) total
        // killed by the swing itself, from above the threshold: no cull
        let hits, total = onTough (Sim.run (fight rules 90.0) 1u) 1
        near (90.0 + float hits * 100.0) total
        same (fight rules 101.0) seeds
        same (fight rules 90.0) seeds)

    test "life costs: a share of the mana cost paid with life, a life cost equal to the mana cost, a share of maximum life; what a skill that costs life gets" (fun () ->
        equal (box (Named "CostAsLife")) (box (mods "base_skill_cost_life_instead_of_mana_%" 50.0).Head.Stat)
        equal (box [ SkillIs Spell ]) (box (mods "spells_cost_life_instead_of_mana_%" 30.0).Head.Conds)
        // a bolt that costs 40 mana, with 100 life and 200 mana and nothing that recovers either:
        // five bolts as it is. A life cost that would leave less than 1 life is not paid
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseCrit = 0.0; Cost = 40.0 }
        let fight (ms: Mod list) =
            let player =
                { Actor.make "Player" ms [ bolt ] with
                    Rotation = [ Actor.always "Bolt" ]
                    Vitals = Some { PlayerStats.basic 100.0 200.0 with ManaRegen = 0.0 } }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } player 20.5
        let uses ms = count (Sim.run (fight ms) 1u) "uses:Bolt"
        equal 5 (uses [])
        // "50% of Mana Costs Converted to Life Costs": 20 mana and 20 life: the mana is enough
        // for ten, the life for four (100, 80, 60, 40, 20)
        equal 4 (uses (mods "base_skill_cost_life_instead_of_mana_%" 50.0))
        // "25% of ...": 30 mana and 10 life: six for the mana
        equal 6 (uses (mods "base_skill_cost_life_instead_of_mana_%" 25.0))
        // "Skills gain a Base Life Cost equal to Base Mana Cost": 40 of each, two bolts (100, 60,
        // 20); with 25% increased Life Cost Efficiency 32 life, three (100, 68, 36, 4)
        equal 2 (uses (mods "skill_cost_base_life_equal_to_base_mana" 1.0))
        equal 3 (uses (mods "skill_cost_base_life_equal_to_base_mana" 1.0 @ mods "base_life_cost_efficiency_+%" 25.0))
        // "Non-Channelling Spells cost an additional 30% of your maximum Life": 30 life beside
        // the 40 mana, three bolts (100, 70, 40, 10)
        equal 3 (uses (mods "base_life_cost_+_with_non_channelling_spells_%_maximum_life" 30.0))
        for ms in [ []; mods "base_skill_cost_life_instead_of_mana_%" 50.0; mods "skill_cost_base_life_equal_to_base_mana" 1.0
                    mods "base_life_cost_+_with_non_channelling_spells_%_maximum_life" 30.0 ] do
            same (fight ms) seeds
        // what a skill that costs life gets: the tag, and 5% of damage as physical per 20 life of
        // its cost, up to 40% (a cost of 100, half of it life: 50 life, 2 x 5%)
        let costly = { bolt with Cost = 100.0; Mods = mods "base_skill_cost_life_instead_of_mana_%" 50.0 @ mods "gain_%_of_damage_as_physical_per_20_life_cost_up_to_40_%" 5.0 }
        let built = Behaviours.lifeCost ModDb.empty costly
        equal (box true) (box (built.Tags.Contains(Tag "CostsLife")))
        equal (box [ 10.0 ]) (box (built.Mods |> List.choose (fun m -> if m.Stat = GainAs(Fire, Physical) then Some m.Value else None)))
        equal (box false) (box ((Behaviours.lifeCost ModDb.empty bolt).Tags.Contains(Tag "CostsLife")))
        let gains = mods "spells_gain_%_physical_damage_if_they_cost_life" 12.0
        equal (box true) (box (gains |> List.forall (fun m -> List.contains (SkillIs(Tag "CostsLife")) m.Conds && List.contains (SkillIs Spell) m.Conds))))

    test "physical damage reduction beside the armour's: additional, its cap, energy shield as armour, Jade; Intimidate" (fun () ->
        let basic = PlayerStats.basic 1000.0 0.0
        // 100 physical: 20% additional reduction leaves 80
        near 80.0 (Defence.taken { basic with PhysicalReduction = 20.0 } Physical false 100.0)
        // armour of 9000 against 100: 90%; with "Maximum Physical Damage Reduction is 50%": 50
        near 10.0 (Defence.taken { basic with Armour = 9000.0 } Physical false 100.0)
        near 50.0 (Defence.taken { basic with Armour = 9000.0; PhysicalReductionCap = 50.0 } Physical false 100.0)
        // 1000 energy shield as armour against 100: 1000 / (1000 + 1000) = 50%
        near 50.0 (Defence.taken { basic with EnergyShield = 1000.0; ESAsArmour = 100.0 } Physical false 100.0)
        equal (box (Named "AdditionalPhysicalReduction")) (box (mods "base_additional_physical_damage_reduction_%" 8.0).Head.Stat)
        equal (box (Named "ESAsArmour")) (box (mods "current_energy_shield_%_as_physical_damage_reduction" 100.0).Head.Stat)
        // on a character: the lines, and Jade building up to 10 stacks of 1% each
        let c = load { blank with ExtraLines = [ "Maximum Physical Damage Reduction is 50%"; "Gain a stack of Jade every second" ] } Options.defaults
        near 50.0 c.Vitals.PhysicalReductionCap
        equal (box (Some 10)) (box (Map.tryFind "Jade" c.MaxResources))
        let jade n = (Defence.computeWith (Map.ofList [ "Jade", n ]) Map.empty c.Sheet).PhysicalReduction
        near 0.0 (jade 0)
        near 7.0 (jade 7)
        equal (box [ 1.0 ]) (box (Behaviours.periodics db.Value c.Sheet c.Build 0.0 |> List.choose (fun p -> if p.Name = "jade" then Some p.Every else None)))
        // in a fight: a monster's physical hits on a player with each of them
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let fight (v: PlayerStats) =
            let player = { Actor.make "Player" [] [ bolt ] with Rotation = [ Actor.always "Bolt" ]; Vitals = Some v }
            let enemy = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = Some { Interval = 0.7; Damage = Map.ofList [ Physical, 60.0; Cold, 20.0 ]; Accuracy = 1e9; Range = 40.0; CritChance = 5.0; Melee = false } }
            Fight.single enemy player 12.5
        let tough = { PlayerStats.basic 5000.0 100.0 with EnergyShield = 800.0; Armour = 1500.0; EnergyShieldRecharge = 100.0 }
        same (fight { tough with PhysicalReduction = 15.0 }) seeds
        same (fight { tough with PhysicalReductionCap = 50.0; Armour = 30000.0 }) seeds
        same (fight { tough with ESAsArmour = 100.0 }) seeds
        // the hit count is the same and less damage arrives with the reduction
        let took v = (Sim.run (fight v) 1u).DamageTaken
        equal (box true) (box (took { tough with PhysicalReduction = 15.0 } < took tough))
        equal (box true) (box (took { tough with ESAsArmour = 100.0 } < took tough))
        // Intimidate: 10% increased damage taken and 10% reduced damage dealt
        let auras lines = Behaviours.presenceAuras (load { blank with ExtraLines = lines } Options.defaults).Sheet
        match auras [ "Enemies in your Presence are Intimidated" ] |> List.filter (fun (n, _, _) -> n = "Intimidated") with
        | [ _, 1, ms ] -> equal (box [ DamageTaken AllDamage, Inc, 10.0; Named "DamageDealt", More, -10.0 ]) (box (ms |> List.map (fun m -> m.Stat, m.Kind, m.Value)))
        | other -> failwithf "unexpected %A" other
        let sheet = { (load blank Options.defaults).Sheet with Mods = mods "enemies_intimidated_x_seconds_when_pinned_heavy_stunned_frozen_or_electrocuted" 4.0 }
        equal (box [ OnInflict Freeze; OnInflict Electrocute; OnInflict Pin; OnInflict HeavyStun ]) (box (Behaviours.intimidateOnImmobilise sheet |> List.map (fun r -> r.Hook)))
        // what monsters never leave on the player
        equal (box NotOffensive) (box (StatMap.meaning "immune_to_bleeding_while_shapeshifted" 1.0))
        let applies = mods "x%_of_armour_applies_to_elemental_damage_while_shapeshifted" 30.0
        equal 3 applies.Length
        equal (box [ WhileBuff "Shapeshifted" ]) (box applies.Head.Conds))

    test "Presence: Withered on entering it; monsters in it gain extra chaos damage; missing life recovered before a hit; what leech goes by" (fun () ->
        // (the walker of the Presence test: inside after 2.75 s; 100 chaos damage every 0.5 s)
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Chaos, { Min = 100.0; Max = 100.0 } ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let walker =
            { EnemyDef.make Rarity.defaults 70 Normal "walker" (Vec.make 100.0 0.0) with
                Life = 1e9; Mods = []; Size = 5.0; Speed = 20.0
                Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 1.0 ]; Accuracy = 100.0; Range = 6.0; CritChance = 0.0; Melee = true } }
        let player = { Actor.make "Player" [] [ bolt ] with Vitals = Some(PlayerStats.basic 1e6 0.0) }
        let weakened = [ Mod.inc (DamageTaken AllDamage) 100.0 ]
        let entering seconds = { Fight.single walker player 5.2 with Presence = 40.0; PresenceEntry = [ "Weakened", 1, weakened, seconds ] }
        let dealt f = (Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd
        // for 1.4 s from when it is inside: the hits at 3.0, 3.5 and 4.0 s deal double, and it
        // does not come again while the monster stays inside
        near (7.0 * 100.0 + 3.0 * 200.0) (dealt (entering 1.4))
        near (5.0 * 100.0 + 5.0 * 200.0) (dealt (entering 60.0))
        same (entering 1.4) seeds
        // on a character: one stack of Withered for the line's seconds
        let c = load { blank with ExtraLines = [ "Unwithered enemies are Withered for 8 seconds when they enter your Presence" ] } Options.defaults
        match Behaviours.presenceEntry c.Sheet with
        | [ "Withered", 1, [ m ], seconds ] ->
            near 8.0 seconds
            equal (box (DamageTaken(OfType Chaos), Inc, 5.0)) (box (m.Stat, m.Kind, m.Value))
        | other -> failwithf "unexpected %A" other
        // a monster's hit of 60 physical and 20 cold with 50% of damage gained as extra chaos: 40 more
        let foe = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = Some { Interval = 0.7; Damage = Map.ofList [ Physical, 60.0; Cold, 20.0 ]; Accuracy = 1e9; Range = 40.0; CritChance = 0.0; Melee = false } }
        let hitFight (v: PlayerStats) auras = { Fight.single foe { player with Rotation = [ Actor.always "Bolt" ]; Vitals = Some v } 12.5 with PresenceAuras = auras }
        let chaos = [ "Presence: Extra Chaos Damage", 1, [ Mod.base' (Named "GainAsChaos") 50.0 ] ]
        let basic = PlayerStats.basic 1e6 0.0
        let took f = (Sim.run f 1u).DamageTaken
        near (1.5 * took (hitFight basic [])) (took (hitFight basic chaos))
        same (hitFight basic chaos) seeds
        equal (box (Named "PresenceEnemyGainAsChaos")) (box (mods "enemies_in_presence_non_skill_base_all_damage_%_to_gain_as_chaos" 10.0).Head.Stat)
        // "Recover 50% of Missing Life before being Hit": 5000 life, hits of 80: 5000, 4920, then
        // half of what is missing comes back before each hit; never below 4840 (twice a hit under full)
        let healed = { PlayerStats.basic 5000.0 0.0 with MissingLifeBeforeHit = 50.0 }
        let hits = took (hitFight (PlayerStats.basic 5000.0 0.0) []) / 80.0
        near (1.0 - 80.0 * hits / 5000.0) (Sim.run (hitFight (PlayerStats.basic 5000.0 0.0) []) 1u).MinLife
        let low = (Sim.run (hitFight healed []) 1u).MinLife
        equal (box true) (box (low > 4840.0 / 5000.0 && low < 4850.0 / 5000.0))
        same (hitFight healed []) seeds
        // leech: of physical damage; with "also recovers based on Lightning damage" of that too
        let leech = mods "base_life_leech_from_all_spell_damage_permyriad" 500.0
        equal (box (Named "LifeLeech", 5.0, [ SkillIs Spell ])) (box (leech.Head.Stat, leech.Head.Value, leech.Head.Conds))
        equal 5 leech.Length
        let zap = { Skill.make "Zap" [ Attack ] with BaseDamage = Map.ofList [ Lightning, Range.fixedAt 400.0; Physical, Range.fixedAt 100.0 ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let leeching (ms: Mod list) =
            let p = { Actor.make "Player" (Mod.base' (Named "Accuracy") 1e9 :: Mod.base' (Named "LifeLeech") 10.0 :: ms) [ zap ] with Rotation = [ Actor.always "Zap" ]; Vitals = Some(PlayerStats.basic 5000.0 0.0) }
            Fight.single foe p 12.5
        let lowest ms = (Sim.run (leeching ms) 1u).MinLife
        // 10 a hit from the physical damage, 50 with the lightning: the life sinks less
        equal (box true) (box (lowest (mods "life_leech_also_recovers_based_on_lightning_damage" 1.0) > lowest [] + 0.01))
        equal (box true) (box (lowest (mods "life_leech_recovers_based_on_your_chaos_damage_instead_of_physical_damage" 1.0) < lowest [] - 0.001))
        for ms in [ []; mods "life_leech_also_recovers_based_on_elemental_damage_types" 1.0; mods "life_leech_recovers_based_on_your_chaos_damage_instead_of_physical_damage" 1.0 ] do
            same (leeching ms) seeds)

    test "Ancestral Bond: the totems and the persistent minions share the Spirit that is left, no point is spent twice" (fun () ->
        let b = { blank with Groups = [ group [ gem "Shockwave Totem" ]; group [ gem "Skeletal Warrior Minion" ] ]; ExtraLines = [ "+400 to Spirit" ] }
        let c = load b Options.defaults
        let bonded = { c.Sheet with Mods = c.Sheet.Mods @ mods "keystone_ancestral_bond" 1.0 }
        let spirit = Behaviours.spiritTotal c.Sheet b
        let countsOf sheet = Behaviours.minionCounts spirit (Behaviours.spiritUses db.Value sheet b)
        let per = (Behaviours.spiritUses db.Value c.Sheet b |> List.find (fun u -> u.Gem = "Skeletal Warrior Minion"))
        // without the keystone the minions have all of it
        equal (box (per.FreeMinions + int (floor (spirit / per.PerMinion)))) (box (countsOf c.Sheet).["Skeletal Warrior Minion"])
        equal (box false) (box ((countsOf c.Sheet).ContainsKey "Shockwave Totem"))
        // with it each has half: 75 a totem, the minions' cost a minion
        let shared = countsOf bonded
        equal (box (int (floor (spirit / 2.0 / 75.0)))) (box shared.["Shockwave Totem"])
        equal (box (per.FreeMinions + int (floor (spirit / 2.0 / per.PerMinion)))) (box shared.["Skeletal Warrior Minion"])
        // the totems that stand: the doubled limit of one, which half the Spirit pays for
        equal 2 (Behaviours.totems db.Value bonded b |> fst).Length
        equal (box true) (box (float shared.["Shockwave Totem"] * 75.0 + float (shared.["Skeletal Warrior Minion"] - per.FreeMinions) * per.PerMinion <= spirit))
        // build 017 of the corpus has the keystone and no totem gem: nothing to place
        equal 0 (Behaviours.totems db.Value { c.Sheet with Mods = bonded.Mods } { b with Groups = [ group [ gem "Spark" ] ] } |> fst).Length)

    test "Darkness: damage gained as chaos per 20 Unreserved Darkness; Fear Incarnate; minions in the Presence counted up to a cap" (fun () ->
        let gain = mods "gain_%_damage_as_chaos_from_unreserved_darkness" 10.0
        equal (box [ PerStack("UnreservedDarkness", 20.0) ]) (box gain.Head.Conds)
        // 100 Darkness, none reserved: 5 x 10% of a bolt's 100 fire as chaos, 150 a bolt
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let fight attack =
            let player =
                { Actor.make "Player" gain [ bolt ] with
                    Rotation = [ Actor.always "Bolt" ]
                    Vitals = Some { PlayerStats.basic 10000.0 0.0 with Darkness = 100.0; DarknessSeconds = 3.0 }
                    MaxResources = Map.ofList [ "UnreservedDarkness", 100 ] }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = attack } player 10.2
        let o = Sim.run (fight None) 1u
        near (150.0 * float (count o "hits:Bolt")) (o.DamageBy |> Map.toList |> List.sumBy snd)
        // a monster's hits of 45 every 0.8 s: each reserves 45 for 3 s, so between 0 and 4
        // twenties are unreserved and the bolts deal less; the same in both engines
        let hits = Some { Interval = 0.8; Damage = Map.ofList [ Physical, 45.0 ]; Accuracy = 1e9; Range = 40.0; CritChance = 0.0; Melee = false }
        let o' = Sim.run (fight hits) 1u
        let total = o'.DamageBy |> Map.toList |> List.sumBy snd
        equal (box true) (box (total < 150.0 * float (count o' "hits:Bolt") - 100.0 && total >= 100.0 * float (count o' "hits:Bolt")))
        same (fight None) seeds
        same (fight hits) seeds
        // Fear Incarnate: 10% increased Culling Strike Threshold a stack, 10 s, up to 20, on a
        // cull by a skill that has Culling Strike
        let c = load blank Options.defaults
        let sheet = { c.Sheet with Mods = c.Sheet.Mods @ mods "grant_fear_incarnate_stack_on_culling_enemies" 1.0 }
        let culling = { bolt with Name = "Reap"; Mods = [ Mod.flag (Named "CullingStrike") ] }
        match Behaviours.fearIncarnate sheet [ bolt; culling ] with
        | [ { Hook = OnKillOf "Cullable"; Filter = Called "Reap"; Reaction = GrantBuff ("Fear Incarnate", 10.0, 20, [ m ]) } ] ->
            equal (box (Named "CullingThreshold", Inc, 10.0)) (box (m.Stat, m.Kind, m.Value))
        | other -> failwithf "unexpected %A" other
        // "4% increased Attack Damage for each Minion in your Presence, up to a maximum of 80%": up to 20 minions
        let per = (mods "gain_attack_damage_+%_for_each_your_minion_in_presence_capped" 4.0).Head
        equal (box (Damage AllDamage, Inc, 4.0)) (box (per.Stat, per.Kind, per.Value))
        equal (box true) (box (List.contains (PerStack("ActiveMinionsUpTo:20", 1.0)) per.Conds))
        equal (box (Mods [ Mod.inc AreaOfEffect 20.0 |> Mod.whenever (SkillIs(Tag "Type:Hazard")) ])) (box (StatMap.meaning "hazard_area_of_effect_+%" 20.0)))

    test "stats: a spell's base critical hit chance set by a line, more immobilisation buildup, bear skills' conversion, what an orb's hits cannot do, more curse effect and delay" (fun () ->
        // "Base Critical Hit Chance for Spells is 15%": Spark's own base gives way to it; an attack keeps its own
        let c = load { blank with Groups = [ group [ gem "Spark" ] ] } Options.defaults
        let own = (Setup.player c Options.defaults [ Spam "Spark" ] None).Skills |> List.find (fun s -> s.Name = "Spark")
        let set = { c with Sheet = { c.Sheet with Mods = c.Sheet.Mods @ mods "base_spell_critical_strike_chance_override_permyriad" 1500.0 } }
        let overridden = (Setup.player set Options.defaults [ Spam "Spark" ] None).Skills |> List.find (fun s -> s.Name = "Spark")
        equal (box true) (box (own.BaseCrit <> 15.0))
        near 15.0 overridden.BaseCrit
        let buildup = mods "ascendancy_tactician_suppressing_fire_hit_damage_immobilisation_multiplier_+%_final" 40.0
        equal (box [ Buildup Freeze, More, 40.0; Buildup HeavyStun, More, 40.0; Buildup Electrocute, More, 40.0; Buildup Pin, More, 40.0 ]) (box (buildup |> List.map (fun m -> m.Stat, m.Kind, m.Value)))
        let bear = (mods "non_skill_physical_damage_%_to_convert_to_fire_with_bear_skills" 50.0).Head
        equal (box (Convert(Physical, Fire), [ SkillIs(Tag "Type:Bear") ])) (box (bear.Stat, bear.Conds))
        equal (box [ Named "LifeLeechAmount", More, -100.0; Named "ManaLeechAmount", More, -100.0 ])
            (box (mods "damage_cannot_be_reflected_or_leech_if_used_by_other_object" 1.0 |> List.map (fun m -> m.Stat, m.Kind, m.Value)))
        equal (box (Named "CurseEffect", More, 30.0)) (box (let m = (mods "support_atziri_curse_effect_+%_final" 30.0).Head in m.Stat, m.Kind, m.Value))
        equal (box (Named "CurseDelay", More, 50.0)) (box (let m = (mods "support_ritual_curse_curse_delay_+%_final" 50.0).Head in m.Stat, m.Kind, m.Value))
        equal (box Structural) (box (StatMap.meaning "supported_active_skill_gem_level_+_if_two_other_supports" 1.0))
        equal (box NotOffensive) (box (StatMap.meaning "hazard_damage_+%_final_for_first_500_ms_when_not_killed_by_self" -50.0)))

    test "Life Remnants: one from a hit every few seconds and by chance from a kill; its life comes in while it is collected" (fun () ->
        // the gem at level 20: 410 life a remnant, one from a hit every 3 s, 25% from a kill
        let c = load { blank with Groups = [ group [ gem "Life Remnants" ] ]; ExtraLines = [ "+500 to Spirit" ] } Options.defaults
        match Behaviours.lifeRemnants db.Value c.Sheet c.Build 0.5 with
        | Some r ->
            near 410.0 r.LifePerRemnant
            near 3.0 r.HitCooldown
            near 25.0 r.OnKillChance
            near 0.5 r.Delay
        | None -> failwith "no Life Remnants"
        equal (box None) (box (Behaviours.lifeRemnants db.Value (load blank Options.defaults).Sheet blank 0.5))
        // "Recover 2% of Maximum Life when you collect a Remnant": with each Life Remnant
        equal (box (Named "RemnantRecoverLifePercent")) (box (mods "remnant_recover_%_life_on_pickup" 2.0).Head.Stat)
        // in a fight: a bolt every 0.5 s, a monster's hits of 150 every second on 2000 life. A
        // remnant every 2 s gives 200 life over half a second: the life sinks by 100 in 2 s
        // where it sank by 300
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseTime = 0.5; BaseCrit = 0.0 }
        let foe = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = Some { Interval = 1.0; Damage = Map.ofList [ Physical, 150.0 ]; Accuracy = 1e9; Range = 40.0; CritChance = 0.0; Melee = false } }
        let fight (remnants: LifeRemnants option) =
            let player = { Actor.make "Player" [] [ bolt ] with Rotation = [ Actor.always "Bolt" ]; Vitals = Some(PlayerStats.basic 2000.0 0.0); LifeRemnants = remnants }
            Fight.single foe player 10.2
        let every2 = Some { LifeRemnants.OnKillChance = 0.0; HitCooldown = 2.0; LifePerRemnant = 200.0; PercentOfMaxLife = 0.0; ExtraChance = 0.0; Delay = 0.5 }
        let bare, fed = Sim.run (fight None) 1u, Sim.run (fight every2) 1u
        near (1.0 - bare.DamageTaken / 2000.0) bare.MinLife
        equal (box true) (box (count fed "life remnant" >= 5))
        // every remnant's 200 life has come in by the end, but for the last one's rest
        equal (box true) (box (fed.MinLife > bare.MinLife + 0.3))
        same (fight every2) seeds
        same (fight (Some { every2.Value with PercentOfMaxLife = 2.0; ExtraChance = 30.0; OnKillChance = 50.0 })) seeds)

    test "life costs: the life flask is drunk when life spent on a skill leaves the life below half" (fun () ->
        // 100 life, a bolt that costs 40 mana and 40 life: 60, 20 (the flask: 60 at once, 80), 40;
        // a fourth cannot be paid. Without the flask two
        let bolt = { Skill.make "Bolt" [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseCrit = 0.0; Cost = 40.0 }
        let flask = { Amount = 60.0; Duration = 3.0; ChargesPerUse = 20.0; MaxCharges = 20.0; ChargesPerSecond = 0.0; ChargeGainMult = 1.0; Instant = 1.0 }
        let fight (withFlask: bool) =
            let player =
                { Actor.make "Player" (mods "skill_cost_base_life_equal_to_base_mana" 1.0) [ bolt ] with
                    Rotation = [ Actor.always "Bolt" ]
                    Vitals = Some { PlayerStats.basic 100.0 200.0 with ManaRegen = 0.0; LifeFlask = (if withFlask then Some flask else None) } }
            Fight.single { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None } player 20.5
        let o = Sim.run (fight true) 1u
        equal 3 (count o "uses:Bolt")
        equal 1 (count o "flask:life")
        equal 2 (count (Sim.run (fight false) 1u) "uses:Bolt")
        same (fight true) seeds)

    // ---- round 16 ----

    test "corrupted gems' levels; a channel's cost by the second that goes with its speed; what counts the different skills used" (fun () ->
        // "+N to Level of all Corrupted (Spell) Skill Gems": the corrupted skill gems, a spell both
        let b =
            { blank with
                Groups = [ group [ { gem "Spark" with Corrupted = true }; { support "Pierce" with Corrupted = true } ]; group [ gem "Fireball" ]; group [ { gem "Lightning Arrow" with Corrupted = true } ] ] }
        let ms = mods "corrupted_skill_gem_level_+" 1.0 @ mods "corrupted_spell_skill_gem_level_+" 2.0
        let levels = (Setup.corruptedLevels db.Value ms b).Groups |> List.map (fun g -> g.Gems |> List.map (fun x -> x.Level))
        equal (box [ [ 23; 1 ]; [ 20 ]; [ 21 ] ]) (box levels)
        // a channel of 10 mana a second whose use takes 1 s, twice as fast: 5 mana a use; where
        // speed also affects the cost, 20 a second: 10 a use. 100 mana and none coming in: 20 uses, or 10
        let beam = { Skill.make "Beam" [ Spell; Channelling ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseCrit = 0.0; CostPerSecond = 10.0 }
        let foe = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None }
        let channel (skill: Skill) =
            let player =
                { Actor.make "Player" [ Mod.more Speed 100.0 ] [ skill ] with
                    Rotation = [ Actor.always "Beam" ]
                    Vitals = Some { PlayerStats.basic 100.0 100.0 with ManaRegen = 0.0 } }
            Fight.single foe player 30.5
        let withSpeed = { beam with Mods = mods "cast_speed_modifiers_apply_to_over_time_cost" 1.0 }
        equal 20 (count (Sim.run (channel beam) 1u) "uses:Beam")
        equal 10 (count (Sim.run (channel withSpeed) 1u) "uses:Beam")
        equal (box (mods "cast_speed_modifiers_apply_to_over_time_cost" 1.0)) (box (mods "attack_speed_modifiers_apply_to_over_time_cost" 1.0))
        same (channel beam) seeds
        same (channel withSpeed) seeds
        // "100% increased Damage for each different Spell you've Cast in the last eight seconds":
        // two spells of 100 a hit and 1 s a cast, one after the other. The first cast counts
        // itself (200), from the second on both are counted (300): 200 + 4 x 300 in 5 s
        let spell name = { Skill.make name [ Spell ] with BaseDamage = Map.ofList [ Fire, Range.fixedAt 100.0 ]; BaseCrit = 0.0 }
        let counting =
            { Actor.make "Player" [ Mod.inc (Damage AllDamage) 100.0 |> Mod.whenever (PerStack("Different:Spell:8", 1.0)) ] [ spell "One"; spell "Two"; { spell "Never" with Tags = Set.ofList [ Spell; Triggered ] } ] with
                Rotation = [ { Use = "Two"; When = fun v -> v.LastUsed = "One" }; Actor.always "One" ] }
            |> Behaviours.differentUsed
        equal 2 (counting.Mods.Length)
        equal 2 (counting.Rules.Length)
        let f = Fight.single foe counting 5.2
        near (200.0 + 4.0 * 300.0) ((Sim.run f 1u).DamageBy |> Map.toList |> List.sumBy snd)
        same f seeds
        // what the ids are: the rest of the id, once for each
        equal (box [ PerStack("Different:Warcry:4", 1.0) ]) (box (mods "damage_+%_per_different_warcry_used_recently" 5.0).Head.Conds)
        equal (box (GainAs(Physical, Fire), 4.0)) (box (let m = (mods "non_skill_base_all_damage_%_to_gain_as_fire_per_different_grenade_type_fired_in_past_8_seconds" 4.0).Head in m.Stat, m.Value))
        equal (box [ PerStack("Different:Ammo:10", 1.0); SkillIs Crossbow ]) (box (mods "crossbow_damage_+%_per_ammo_type_fired_in_past_10_seconds" 5.0).Head.Conds))

    test "what waits to be set off waits up to the skill's own limit; a lodged spear's explosion comes after its detonation time" (fun () ->
        let foe = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None }
        let hit name dmg tags = { Skill.make name tags with BaseDamage = Map.ofList [ Physical, Range.fixedAt dmg ]; BaseCrit = 0.0 }
        // a bolt whose explosion waits for a Detonator, and a Detonator with a cooldown of 5.5 s:
        // it is used at 0 (nothing waits) and at 6, after five bolts. With "Maximum of 3 active
        // Bolts" three explosions go off, without it all five
        let bolts (limit: Mod list) =
            let explosion = { hit "Bolt: Explosion" 100.0 [ Attack ] with Mods = Mod.flag (Named "DetonatedBy:Type:Detonator") :: limit }
            let bolt = { hit "Bolt" 10.0 [ Attack ] with Parts = [ explosion ] }
            let spark = { hit "Spark" 1.0 [ Attack; Tag "Type:Detonator" ] with Cooldown = Some 5.5 }
            let player = { Actor.make "Player" [ Mod.base' (Named "Accuracy") 1e9 ] [ spark; bolt ] with Rotation = [ Actor.always "Spark"; Actor.always "Bolt" ] }
            Fight.single foe player 7.5
        equal 5 (count (Sim.run (bolts []) 1u) "hits:Bolt: Explosion")
        equal 3 (count (Sim.run (bolts (mods "stormblast_bolts_maximum_active_bolt_count" 3.0)) 1u) "hits:Bolt: Explosion")
        equal (box (mods "spike_slam_max_spikes" 6.0)) (box [ Mod.base' (Named "WaitLimit") 6.0 ])
        same (bolts (mods "stormblast_bolts_maximum_active_bolt_count" 3.0)) seeds
        // a spear of 1 s a throw whose explosion comes 2.5 s after it lands: in 6.2 s six spears
        // land (at 1 .. 6) and the first three have exploded (at 3.5, 4.5, 5.5)
        let lodged =
            let explosion = { hit "Spear: Explosion" 100.0 [ Attack ] with Mods = [ Mod.base' (Named "PartDelay") 2.5 ] }
            let player = { Actor.make "Player" [ Mod.base' (Named "Accuracy") 1e9 ] [ { hit "Spear" 10.0 [ Attack ] with Parts = [ explosion ] } ] with Rotation = [ Actor.always "Spear" ] }
            Fight.single foe player 6.2
        let o = Sim.run lodged 1u
        equal 6 (count o "hits:Spear")
        equal 3 (count o "hits:Spear: Explosion")
        same lodged seeds
        // the gem: 2.5 s, and 30% less with Fast Forward if the support says so
        let b = { blank with Level = 90; Groups = [ group [ gem "Explosive Spear" ] ]; Items = [ "Weapon 1", "Rarity: Normal\nHardwood Spear\n" ] }
        let c = load b Options.defaults
        match Setup.pressed c Options.defaults "Explosive Spear" with
        | Some p ->
            let delays = p.Skill.Parts |> List.map (fun part -> part.Name, Sim.partDelay part)
            equal (box [ "Explosive Spear: Explosion", 2.5; "Explosive Spear: Frenzy-Charged", 0.0 ]) (box delays)
        | None -> failwith "Explosive Spear cannot be pressed"
        equal (box ([]: string list)) (box (Gems.unmodelledTypes db.Value "Explosive Spear" |> List.filter (fun t -> t = "DetonatesAfterTime")))
        equal (box (Mod.inc (Named "DetonationTime") 20.0)) (box (mods "skill_detonation_time_+%" 20.0).Head))

    test "Puppet Master's stacks from the minions' Command skills; an Archon of Undeath for ten seconds and its recovery" (fun () ->
        let foe = { EnemyDef.atLevel 83 1e9 with Mods = []; Attack = None }
        let hit name dmg seconds tags = { Skill.make name tags with BaseDamage = Map.ofList [ Physical, Range.fixedAt dmg ]; BaseCrit = 0.0; BaseTime = seconds }
        let sheet (ms: Mod list) = { (load blank Options.defaults).Sheet with Mods = ms }
        let idle = { Actor.make "Player" [] [] with Rotation = [] }
        let sure = [ Mod.base' (Named "Accuracy") 1e9 ]
        let dealt (o: Outcome) (k: string) = Map.tryFind k o.DamageBy |> Option.defaultValue 0.0
        // Puppet Master, 100% to gain a stack with every command, +1 to maximum stacks, 50%
        // increased effect: 15% increased damage with Command skills a stack, up to 6. The n-th
        // command hits with min(n, 6) stacks (its own stack is gained as it starts)
        let puppet = Behaviours.puppetMaster (sheet (mods "surpassing_chance_%_to_gain_1_puppeteer_stack_on_using_command_skill" 100.0 @ mods "max_puppet_master_stacks_+" 1.0 @ mods "puppet_master_effect_+%" 50.0))
        match puppet with
        | [ { Hook = OnUse; Chance = 100.0; Reaction = GrantBuff ("Puppet Master", seconds, 6, ms) } ] ->
            near 8.0 seconds
            equal (box [ Damage AllDamage, 15.0; Speed, 4.5 ]) (box (ms |> List.map (fun m -> m.Stat, m.Value)))
        | other -> failwithf "unexpected %A" other
        equal 0 (Behaviours.puppetMaster (sheet [])).Length
        let commanded =
            let minion = { Actor.make "Brute" sure [ hit "Brute: Smash" 100.0 1.0 [ Attack; Tag "Command" ]; hit "Brute: Jab" 100.0 1.0 [ Attack ] ] with Rotation = [ Actor.always "Brute: Smash" ]; Rules = puppet }
            { Fight.single foe idle 12.2 with Minions = [ minion ] }
        let o = Sim.run commanded 1u
        let n = count o "hits:Brute: Smash"
        equal (box true) (box (n >= 12))
        near ([ 1 .. n ] |> List.sumBy (fun k -> 100.0 * (1.0 + 0.15 * float (min k 6)))) (dealt o "Brute: Smash (hit)")
        same commanded seeds
        // Archon of Undeath, 100% on a command: 25% more damage for all minions for 10 s, then
        // 20 s in which none is gained. A commander that hits every 0.7 s and a follower every
        // 0.9 s, 20.2 s: 14 of 28 and 11 of 22 hits fall into the ten seconds
        let undeath = Behaviours.archonOfUndeath (sheet (mods "chance_%_to_gain_archon_of_undeath_on_using_command_skill" 100.0)) "ArchonOfUndeathOnCommand" (WithTag(Tag "Command"))
        match undeath with
        | Some (skill, rules) ->
            near 30.0 skill.Cooldown.Value
            let commander = { Actor.make "Mage" sure [ hit "Mage: Bomb" 100.0 0.7 [ Spell; Tag "Command" ]; skill ] with Rotation = [ Actor.always "Mage: Bomb" ]; Rules = rules }
            let follower = { Actor.make "Warrior" sure [ hit "Warrior: Slash" 100.0 0.9 [ Attack ] ] with Rotation = [ Actor.always "Warrior: Slash" ] }
            let f = { Fight.single foe idle 20.2 with Minions = [ commander; follower ] }
            let o = Sim.run f 1u
            equal 1 (count o "trigger:Archon of Undeath")
            near (14.0 * 125.0 + 14.0 * 100.0) (dealt o "Mage: Bomb (hit)")
            near (11.0 * 125.0 + 11.0 * 100.0) (dealt o "Warrior: Slash (hit)")
            same f seeds
        | None -> failwith "no Archon of Undeath"
        equal (box true) (box (Behaviours.archonOfUndeath (sheet []) "ArchonOfUndeathOnCommand" AnySkill).IsNone)
        equal (box (Named "ArchonOfUndeathOnOffering")) (box (mods "chance_%_to_gain_archon_of_undeath_when_you_create_an_offering" 20.0).Head.Stat)
        equal (box (Mod.inc (Named "PuppetMasterDuration") 30.0)) (box (mods "puppet_master_duration_+%" 30.0).Head))
