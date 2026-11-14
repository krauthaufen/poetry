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
