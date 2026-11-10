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
    let stats (skill: SkillData) (set: StatSetData) level quality : (string * float) list =
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
            skill.QualityStats |> List.map (fun (s, per) -> s, System.Math.Truncate(per * float quality))
        levelStats @ set.ConstantStats @ qualityStats |> List.filter (fun (_, v) -> v <> 0.0)

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

    /// Turn game stats into mods, sorting out what we couldn't use.
    let private interpret (stats: (string * float) list) =
        stats
        |> List.fold
            (fun (mods, cov: Coverage) (id, v) ->
                match baseDamageStat id with
                | Some _ -> mods, cov
                | None when deliveryStats.Contains id -> mods, cov
                | None ->
                    match StatMap.meaning id v with
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

    /// Build a simulator Skill from a socketed gem. `characterMods` are needed for
    /// "+N to level of ... gems". Attacks need a weapon.
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
                let gemLevel = Map.tryFind level data.Levels
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods)
                let ownStats = stats data set level quality

                let supportStats, supportCov =
                    setup.Supports
                    |> List.map (fun (name, lvl) ->
                        match skillOfGem db name with
                        | Some s when s.Support ->
                            s.StatSets |> List.tryHead |> Option.map (fun set -> stats s set lvl 0) |> Option.defaultValue [], Coverage.empty
                        | _ -> [], { Coverage.empty with UnknownText = [ "unknown support: " + name ] })
                    |> List.unzip
                let mods, cov = interpret (ownStats @ List.concat supportStats)
                let cov = List.fold Coverage.merge cov supportCov

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
                let radius = statOf "active_skill_base_area_of_effect_radius"
                let delivery =
                    match statOf "base_number_of_projectiles" with
                    | Some n when data.Types.Contains "Projectile" ->
                        Projectiles
                            { ProjectileShape.make (int n) with
                                // "projectiles_nova": fired in a full circle
                                Spread = (if flagOf "projectiles_nova" then 360.0 else statOf "total_projectile_spread_angle_override" |> Option.defaultValue 30.0)
                                Pierce = int (statOf "projectile_base_number_of_targets_to_pierce" |> Option.defaultValue 0.0)
                                Chains = int (statOf "number_of_chains" |> Option.defaultValue 0.0)
                                CanShotgun = flagOf "projectiles_can_shotgun"
                                Explosion = if data.Types.Contains "Area" then radius else None }
                    | _ when data.Types.Contains "Projectile" ->
                        Projectiles { ProjectileShape.make 1 with Explosion = if data.Types.Contains "Area" then radius else None }
                    | _ ->
                        match radius with
                        | Some r when data.Types.Contains "Nova" -> AreaAroundSelf r
                        | Some r when data.Types.Contains "Area" -> AreaAtTarget r
                        | _ -> Single
                Ok(
                    { Name = data.Name
                      Tags = tags
                      BaseDamage = baseDamage
                      BaseMultiplier = baseMultiplier
                      AddedDamageEffectiveness = 1.0
                      BaseCrit = crit
                      BaseTime = time
                      Cooldown = gemLevel |> Option.bind (fun l -> l.Cooldown)
                      Hits = [ 1.0, 1.0 ]
                      Delivery = delivery
                      Mods = mods },
                    cov
                )
