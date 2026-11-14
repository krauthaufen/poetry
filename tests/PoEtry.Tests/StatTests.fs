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
