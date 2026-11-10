namespace PoEsie

/// Everything loaded from data/game.
type GameDb =
    { Skills: Map<string, SkillData>
      Gems: Map<string, GemData>
      Weapons: Map<string, WeaponBase>
      /// Every item base name (weapons, armour, jewellery...).
      BaseNames: Set<string>
      Text: StatText.Index }

/// A weapon as equipped: base plus its local modifiers.
type Weapon =
    { Damage: Map<DamageType, Range>
      CritChance: float
      AttacksPerSecond: float
      Tags: SkillTag list }

/// What happened to every stat that went into a skill or item.
type Coverage =
    { /// Stats with no meaning in StatMap yet.
      Unmapped: (string * float) list
      /// Understood but not simulated: (stat, mechanic).
      Unsimulated: (string * string) list
      /// Lines of text that the stat descriptions didn't recognise.
      UnknownText: string list }

module Coverage =
    let empty =
        { Unmapped = []
          Unsimulated = []
          UnknownText = [] }

    let merge a b =
        { Unmapped = a.Unmapped @ b.Unmapped
          Unsimulated = a.Unsimulated @ b.Unsimulated
          UnknownText = a.UnknownText @ b.UnknownText }

/// A gem as socketed: active gem, level, quality, supports.
type GemSetup =
    { Gem: string
      Level: int
      Quality: int
      /// Which stat set of the skill (1-based). Most skills have one.
      StatSet: int
      Supports: (string * int) list }

module GameDb =
    let load (read: string -> string) =
        { Skills = GameData.parseSkills (read "skills.json")
          Gems = GameData.parseGems (read "gems.json")
          Weapons = GameData.parseWeapons (read "bases.json")
          BaseNames =
            match Json.parse (read "bases.json") with
            | JObj m -> m |> Map.toList |> List.map fst |> Set.ofList
            | _ -> Set.empty
          Text = StatText.build [ read "statdesc_stat_descriptions.json"; read "statdesc_passive_skill_stat_descriptions.json" ] }

module Gems =
    /// Gem stats at a level, as the game computes them. Interpolation 1 = value as
    /// listed; 3 = scaled by skill damage effectiveness at the stat set's actor level.
    /// Source: PoB CalcTools.buildSkillInstanceStats (formula), GameConstants (base effectiveness).
    let statsWith (altQuality: bool) (skill: SkillData) (set: StatSetData) level quality : (string * float) list =
        let levelData =
            Map.tryFind level set.Levels
            |> Option.orElse (Map.tryFind 1 set.Levels)
        let effectiveness actorLevel =
            Map.find "SkillDamageBaseEffectiveness" Tables.gameConstants
            * set.BaseEffectiveness
            * (1.0 + set.IncrementalEffectiveness * (actorLevel - 1.0))
            * ((1.0 + set.DamageIncrementalEffectiveness) ** (actorLevel - 1.0))
        let levelStats =
            match levelData with
            | None -> []
            | Some l ->
                set.Stats
                |> List.mapi (fun i stat ->
                    let raw = List.tryItem i l.Values |> Option.defaultValue 1.0
                    let value =
                        match List.tryItem i l.Interpolation with
                        | Some 3 -> System.Math.Round(effectiveness l.ActorLevel * raw)
                        | _ -> raw
                    stat, value)
        let qualityStats =
            (skill.QualityStats @ (if altQuality then skill.AltQualityStats else []))
            |> List.map (fun (s, per) -> s, System.Math.Truncate(per * float quality))
        levelStats @ set.ConstantStats @ qualityStats |> List.filter (fun (_, v) -> v <> 0.0)

    let stats skill set level quality = statsWith false skill set level quality

    let private tagOfType t =
        match t with
        | "Attack" -> Some Attack
        | "Spell" -> Some Spell
        | "Melee" -> Some Melee
        | "Projectile" -> Some Projectile
        | "Area" -> Some Area
        | "Duration" -> Some Duration
        | "Channel" -> Some Channelling
        | "Minion" -> Some Minion
        | "Totem" -> Some Totem
        | "Warcry" -> Some Warcry
        | "Herald" -> Some Herald
        | "Curse" | "AppliesCurse" -> Some Curse
        | "Nova" -> Some Nova
        | "Chains" -> Some Chaining
        | "Slam" -> Some Slam
        | "Strike" -> Some Strike
        | "Triggered" -> Some Triggered
        | "Lightning" | "Fire" | "Cold" | "Chaos" | "Physical" | "Trap" | "Mine" | "Grenade" | "Plant" -> Some(Tag t)
        | _ -> None

    /// "spell_minimum_base_fire_damage" -> (Min, Fire)
    let private baseDamageStat (id: string) =
        match id.Split('_') |> List.ofArray with
        | [ _; bound; "base"; t; "damage" ]
        | [ bound; "base"; t; "damage" ] when bound = "minimum" || bound = "maximum" ->
            DamageType.all
            |> List.tryFind (fun d -> (DamageType.name d).ToLowerInvariant() = t)
            |> Option.map (fun d -> bound = "minimum", d)
        | _ -> None

    /// Stats read directly into the skill's Delivery.
    let private deliveryStats =
        set [ "base_number_of_projectiles"; "number_of_chains"; "projectile_base_number_of_targets_to_pierce"
              "active_skill_base_area_of_effect_radius"; "projectiles_can_shotgun"; "total_projectile_spread_angle_override" ]

    /// Gem stats often carry the skill's internal name first
    /// ("lightning_strike_damage_+%_final_per_power_charge"). If an id is Unmapped,
    /// try it with 1..3 leading words removed; only a full Mods meaning is accepted.
    let private gemMeaning (id: string) v =
        match StatMap.meaning id v with
        | Unmapped
        | Unsimulated _ as original ->
            let words = id.Split('_')
            [ 1..3 ]
            |> List.tryPick (fun k ->
                if words.Length > k + 2 then
                    match StatMap.meaning (String.concat "_" (Array.skip k words)) v with
                    // marked: it came from a skill-specific stat, so it describes that skill only
                    | Mods ms -> Some(Mods(ms |> List.map (Mod.from "skill-specific")))
                    | _ -> None
                else None)
            |> Option.defaultValue original
        | m -> m

    /// Turn game stats into mods, sorting out what we couldn't use.
    let private interpret (stats: (string * float) list) =
        stats
        |> List.fold
            (fun (mods, cov: Coverage) (id, v) ->
                match baseDamageStat id with
                | Some _ -> mods, cov
                | None when deliveryStats.Contains id || id.EndsWith "_number_of_projectiles_per_power_charge" -> mods, cov
                | None ->
                    match gemMeaning id v with
                    | Mods ms -> mods @ ms, cov
                    | Local _
                    | Structural
                    | NotOffensive -> mods, cov
                    | Unsimulated m -> mods, { cov with Unsimulated = cov.Unsimulated @ [ id, m ] }
                    | Unmapped -> mods, { cov with Unmapped = cov.Unmapped @ [ id, v ] })
            ([], Coverage.empty)

    let private skillOfGem (db: GameDb) name =
        match Map.tryFind name db.Gems with
        | Some g -> Map.tryFind g.GrantedEffectId db.Skills
        | None -> Map.tryFind name db.Skills

    /// Persistent buffs (Berserk, Eternal Rage, heralds...): always on while reserved.
    let isPersistentBuff (db: GameDb) name =
        skillOfGem db name |> Option.exists (fun d -> d.Types.Contains "Persistent" && d.Types.Contains "Buff")

    /// A stat set flagged to be shown with the main set: it happens in the same use
    /// (Falling Thunder's projectiles). Other sets are alternatives (Spark's Cold-Infused).
    let private isPart (set: StatSetData) = set.Stats |> List.contains "display_statset_hide_usage_stats"

    /// One stat set of a skill as a simulator Skill.
    let private buildSet altQuality (data: SkillData) (set: StatSetData) name tags level quality (supportStats: (string * float) list) (weapon: Weapon option) =
        let gemLevel = Map.tryFind level data.Levels
        let ownStats = statsWith altQuality data set level quality
        let mods, cov = interpret (ownStats @ supportStats)
        let isAttack = data.Types.Contains "Attack"
        let baseDamage =
            if isAttack then
                weapon |> Option.map (fun w -> w.Damage) |> Option.defaultValue Map.empty
            else
                ownStats
                |> List.choose (fun (id, v) -> baseDamageStat id |> Option.map (fun (isMin, t) -> t, isMin, v))
                |> List.groupBy (fun (t, _, _) -> t)
                |> List.map (fun (t, xs) ->
                    let pick m = xs |> List.tryFind (fun (_, isMin, _) -> isMin = m) |> Option.map (fun (_, _, v) -> v) |> Option.defaultValue 0.0
                    t, { Min = pick true; Max = pick false })
                |> Map.ofList
        let setLevel = Map.tryFind level set.Levels
        let baseMultiplier =
            setLevel |> Option.bind (fun l -> l.BaseMultiplier)
            |> Option.orElse (gemLevel |> Option.bind (fun l -> l.BaseMultiplier))
            |> Option.defaultValue 1.0
        let crit =
            if isAttack then weapon |> Option.map (fun w -> w.CritChance) |> Option.defaultValue 5.0
            else
                setLevel |> Option.bind (fun l -> l.CritChance)
                |> Option.orElse (gemLevel |> Option.bind (fun l -> l.CritChance))
                |> Option.defaultValue 0.0
        let time =
            if isAttack then
                let aps = weapon |> Option.map (fun w -> w.AttacksPerSecond) |> Option.defaultValue 1.0
                let speedMult = 1.0 + (gemLevel |> Option.bind (fun l -> l.AttackSpeedMultiplier) |> Option.defaultValue 0.0) / 100.0
                match gemLevel |> Option.bind (fun l -> l.AttackTime) with
                | Some t -> t / 1000.0
                | None -> 1.0 / (aps * speedMult)
            else
                data.CastTime
        let statOf id = ownStats |> List.tryFind (fst >> (=) id) |> Option.map snd
        let flagOf id = statOf id |> Option.exists (fun v -> v <> 0.0)
        let allStats = ownStats @ supportStats
        let flagAny id = allStats |> List.exists (fun (s, v) -> s = id && v <> 0.0)
        let radius = statOf "active_skill_base_area_of_effect_radius"
        let perCharge =
            ownStats
            |> List.tryPick (fun (id, v) -> if id.EndsWith "_number_of_projectiles_per_power_charge" then Some("PowerCharge", int v) else None)
        let isProjectileSet = List.contains "base_is_projectile" set.Stats
        let delivery =
            if isProjectileSet then
                let count = statOf "base_number_of_projectiles" |> Option.map int |> Option.defaultValue (if perCharge.IsSome then 0 else 1)
                Projectiles
                    { ProjectileShape.make count with
                        // "projectiles_nova": fired in a full circle
                        Spread = (if flagAny "projectiles_nova" then 360.0 else statOf "total_projectile_spread_angle_override" |> Option.defaultValue 30.0)
                        Pierce = (if flagOf "always_pierce" then 100 else int (statOf "projectile_base_number_of_targets_to_pierce" |> Option.defaultValue 0.0))
                        Chains = int (statOf "number_of_chains" |> Option.defaultValue 0.0)
                        CanShotgun = flagAny "projectiles_can_shotgun"
                        PerCharge = perCharge
                        FromImpact = data.Types.Contains "ProjectilesNotFromUser"
                        Explosion = if data.Types.Contains "Area" && not (List.contains "is_area_damage" set.Stats) then radius else None }
            else
                match radius with
                | Some r when data.Types.Contains "Nova" -> AreaAroundSelf r
                | Some r -> AreaAtTarget r
                | _ -> Single
        { Name = name
          Tags = tags
          BaseDamage = baseDamage
          BaseMultiplier = baseMultiplier
          AddedDamageEffectiveness = 1.0
          BaseCrit = crit
          BaseTime = time
          Cooldown = gemLevel |> Option.bind (fun l -> l.Cooldown)
          Hits = [ 1.0, 1.0 ]
          Delivery = delivery
          Mods = mods
          Parts = []
          Consumes = if data.Types.Contains "SkillConsumesPowerChargesOnUse" then Some "PowerCharge" else None },
        cov

    /// Build a simulator Skill from a socketed gem. `characterMods` are needed for
    /// "+N to level of ... gems". Attacks need a weapon. Stat sets that belong to the
    /// same use become Parts of the skill.
    let skill (db: GameDb) (characterMods: ModDb) (weapon: Weapon option) (setup: GemSetup) : Result<Skill * Coverage, string> =
        match skillOfGem db setup.Gem with
        | None -> Error(sprintf "unknown gem %s" setup.Gem)
        | Some data ->
            match List.tryItem (setup.StatSet - 1) data.StatSets with
            | None -> Error(sprintf "%s has no stat set %d" setup.Gem setup.StatSet)
            | Some set ->
                let tags =
                    (data.Types |> Set.toList |> List.choose tagOfType)
                    @ (if data.Types.Contains "Attack" then weapon |> Option.map (fun w -> w.Tags) |> Option.defaultValue [] else [])
                    |> Set.ofList
                let ctx = { Ctx.empty with SkillName = data.Name; SkillTags = tags }
                let level = setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods)
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min level maxLevel
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods)
                let supportStats, supportCov =
                    setup.Supports
                    |> List.map (fun (name, lvl) ->
                        match skillOfGem db name with
                        | Some s when s.Support ->
                            s.StatSets |> List.tryHead |> Option.map (fun set -> stats s set lvl 0) |> Option.defaultValue [], Coverage.empty
                        | _ -> [], { Coverage.empty with UnknownText = [ "unknown support: " + name ] })
                    |> List.unzip
                let supportStats = List.concat supportStats
                let alt = Mods.flag ctx (Named "AltQuality") characterMods
                let main, cov = buildSet alt data set data.Name tags level quality supportStats weapon
                let parts, partCov =
                    data.StatSets
                    |> List.filter (fun s -> s <> set && isPart s)
                    |> List.map (fun s ->
                        let p, c = buildSet alt data s (data.Name + ": " + s.Label) tags level quality supportStats weapon
                        { p with Consumes = None }, c)
                    |> List.unzip
                let cov = List.fold Coverage.merge cov (supportCov @ partCov)
                Ok({ main with Parts = parts }, cov)

    /// The game stats of a socketed gem (own stat set, quality, alt quality, supports),
    /// for behaviours that need raw numbers.
    let rawStats (db: GameDb) (characterMods: ModDb) (setup: GemSetup) =
        match skillOfGem db setup.Gem with
        | None -> []
        | Some data ->
            match List.tryItem (setup.StatSet - 1) data.StatSets with
            | None -> []
            | Some set ->
                let tags = data.Types |> Set.toList |> List.choose tagOfType |> Set.ofList
                let ctx = { Ctx.empty with SkillName = data.Name; SkillTags = tags }
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min maxLevel (setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods))
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods)
                let alt = Mods.flag ctx (Named "AltQuality") characterMods
                let supports =
                    setup.Supports
                    |> List.collect (fun (name, lvl) ->
                        match skillOfGem db name with
                        | Some s when s.Support -> s.StatSets |> List.tryHead |> Option.map (fun set -> stats s set lvl 0) |> Option.defaultValue []
                        | _ -> [])
                statsWith alt data set level quality @ supports
