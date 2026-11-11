namespace PoEtry

/// Typed views of the exported game data (data/game/*.json, see tools/export).

type StatSetLevel =
    { Values: float list
      Interpolation: int list
      ActorLevel: float
      BaseMultiplier: float option
      CritChance: float option }

type StatSetData =
    { Label: string
      BaseEffectiveness: float
      IncrementalEffectiveness: float
      DamageIncrementalEffectiveness: float
      Stats: string list
      ConstantStats: (string * float) list
      Levels: Map<int, StatSetLevel>
      BaseFlags: Set<string> }

type SkillLevel =
    { LevelRequirement: float
      CritChance: float option
      BaseMultiplier: float option
      AttackSpeedMultiplier: float option
      Cooldown: float option
      AttackTime: float option
      StoredUses: float option
      /// Base mana cost per use.
      ManaCost: float option
      /// Support gems: cost multiplier in percent more (30 = ×1.3).
      ManaMultiplier: float option }

type SkillData =
    { Id: string
      Name: string
      Types: Set<string>
      CastTime: float
      Support: bool
      Hidden: bool
      Levels: Map<int, SkillLevel>
      StatSets: StatSetData list
      QualityStats: (string * float) list
      /// Alternate quality effects (granted by e.g. Gemling's Advanced Thaumaturgy).
      AltQualityStats: (string * float) list
      /// Minions the skill summons (ids in minions.json).
      MinionList: string list
      /// Gem colour: 1 red (Strength), 2 green (Dexterity), 3 blue (Intelligence), 0 none.
      Color: int }

/// A monster a skill summons (PoB Data/Minions, Data/Spectres: from the game's monster tables).
type MinionData =
    { Id: string
      Name: string
      /// Multipliers on the monster life / damage of the minion's level.
      Life: float
      Damage: float
      DamageSpread: float
      AttackTime: float
      CritChance: float
      SkillList: string list }

type GemData =
    { GameId: string
      Name: string
      GrantedEffectId: string
      AdditionalStatSets: string list
      Support: bool
      Tags: Set<string>
      /// Attribute requirement shares (percent of the gem's requirement): Str, Dex, Int.
      Requirements: float * float * float
      /// Further skills the gem grants (a totem's own attack: "additionalGrantedEffectId1").
      AdditionalEffects: string list
      /// Weapon base types the skill needs ("Bow", "Two Hand Mace", "Quarterstaff", "Any Martial Weapon"...).
      WeaponRequirements: string list }

type WeaponBase =
    { Name: string
      Type: string
      Damage: Map<DamageType, Range>
      CritChance: float
      AttacksPerSecond: float
      /// Base tags ("warstaff" marks a quarterstaff, "twohand", ...).
      BaseTags: Set<string> }

module GameData =
    let private statPairs j =
        Json.list j
        |> List.choose (fun p ->
            match Json.list (Some p) with
            | JStr s :: rest -> Some(s, rest |> List.tryHead |> Option.bind Json.num |> Option.defaultValue 0.0)
            | _ -> None)

    let private statSetLevel j =
        let jo = Some j
        { Values = Json.list jo |> List.map (Json.num >> Option.defaultValue 0.0)
          Interpolation = Json.list (Json.field "statInterpolation" j) |> List.map (Json.num >> Option.defaultValue 1.0 >> int)
          ActorLevel = Json.numField "actorLevel" j |> Option.defaultValue 1.0
          BaseMultiplier = Json.numField "baseMultiplier" j
          CritChance = Json.numField "critChance" j }

    let private statSet j =
        { Label = Json.strField "label" j |> Option.defaultValue ""
          BaseEffectiveness = Json.numField "baseEffectiveness" j |> Option.defaultValue 1.0
          IncrementalEffectiveness = Json.numField "incrementalEffectiveness" j |> Option.defaultValue 0.0
          DamageIncrementalEffectiveness = Json.numField "damageIncrementalEffectiveness" j |> Option.defaultValue 0.0
          Stats = Json.list (Json.field "stats" j) |> List.choose Json.str
          ConstantStats = statPairs (Json.field "constantStats" j)
          Levels = Json.items (Json.field "levels" j) |> List.map (fun (i, l) -> i, statSetLevel l) |> Map.ofList
          BaseFlags = Json.keys (Json.field "baseFlags" j) |> Set.ofList }

    let private skillLevel j =
        { LevelRequirement = Json.numField "levelRequirement" j |> Option.defaultValue 0.0
          CritChance = Json.numField "critChance" j
          BaseMultiplier = Json.numField "baseMultiplier" j
          AttackSpeedMultiplier = Json.numField "attackSpeedMultiplier" j
          Cooldown = Json.numField "cooldown" j
          AttackTime = Json.numField "attackTime" j
          StoredUses = Json.numField "storedUses" j
          ManaCost = Json.field "cost" j |> Option.bind (Json.numField "Mana")
          ManaMultiplier = Json.numField "manaMultiplier" j }

    let private skill id j =
        { Id = id
          Name = Json.strField "name" j |> Option.defaultValue id
          Types = Json.keys (Json.field "skillTypes" j) |> Set.ofList
          CastTime = Json.numField "castTime" j |> Option.defaultValue 1.0
          Support = Json.field "support" j = Some(JBool true)
          Hidden = Json.field "hidden" j = Some(JBool true)
          Levels = Json.items (Json.field "levels" j) |> List.map (fun (i, l) -> i, skillLevel l) |> Map.ofList
          StatSets = Json.list (Json.field "statSets" j) |> List.map statSet
          QualityStats = statPairs (Json.field "qualityStats" j)
          AltQualityStats = statPairs (Json.field "altQualityStats" j)
          MinionList = Json.list (Json.field "minionList" j) |> List.choose Json.str
          Color = Json.numField "color" j |> Option.map int |> Option.defaultValue 0 }

    /// minions.json
    let parseMinions (json: string) : Map<string, MinionData> =
        match Json.parse json with
        | JObj m ->
            m |> Map.map (fun id j ->
                let num k d = Json.numField k j |> Option.defaultValue d
                { Id = id
                  Name = Json.strField "name" j |> Option.defaultValue id
                  Life = num "life" 1.0
                  Damage = num "damage" 1.0
                  DamageSpread = num "damageSpread" 0.2
                  AttackTime = num "attackTime" 1.0
                  CritChance = num "critChance" 5.0
                  SkillList = Json.list (Json.field "skillList" j) |> List.choose Json.str })
        | _ -> Map.empty

    /// skills.json: grantedEffectId -> skill.
    let parseSkills (json: string) : Map<string, SkillData> =
        match Json.parse json with
        | JObj m -> m |> Map.map skill
        | _ -> Map.empty

    /// gems.json as a list (a few gem names have one entry per weapon type).
    let parseGemList (json: string) : GemData list =
        match Json.parse json with
        | JObj m ->
            m
            |> Map.toList
            |> List.map (fun (id, j) ->
                let name = Json.strField "name" j |> Option.defaultValue id
                { GameId = id
                  Name = name
                  GrantedEffectId = Json.strField "grantedEffectId" j |> Option.defaultValue ""
                  AdditionalStatSets =
                    [ 1..4 ] |> List.choose (fun k -> Json.strField (sprintf "additionalStatSet%d" k) j)
                  Support = Json.strField "gemType" j = Some "Support"
                  Tags = Json.keys (Json.field "tags" j) |> Set.ofList
                  Requirements =
                    (Json.numField "reqStr" j |> Option.defaultValue 0.0),
                    (Json.numField "reqDex" j |> Option.defaultValue 0.0),
                    (Json.numField "reqInt" j |> Option.defaultValue 0.0)
                  AdditionalEffects = [ 1..3 ] |> List.choose (fun k -> Json.strField (sprintf "additionalGrantedEffectId%d" k) j)
                  WeaponRequirements =
                    (Json.strField "weaponRequirements" j |> Option.defaultValue "").Split(',')
                    |> Array.map (fun x -> x.Trim()) |> Array.filter ((<>) "") |> List.ofArray })
        | _ -> []

    /// gems.json, keyed by gem name (the first entry of a name).
    let parseGems (json: string) : Map<string, GemData> =
        parseGemList json |> List.rev |> List.map (fun g -> g.Name, g) |> Map.ofList

    /// bases.json: weapon bases only.
    let parseWeapons (json: string) : Map<string, WeaponBase> =
        match Json.parse json with
        | JObj m ->
            m
            |> Map.toList
            |> List.choose (fun (name, j) ->
                Json.field "weapon" j
                |> Option.map (fun w ->
                    let range t =
                        let key = DamageType.name t
                        match Json.numField (key + "Min") w, Json.numField (key + "Max") w with
                        | Some lo, Some hi -> Some(t, { Min = lo; Max = hi })
                        | _ -> None
                    name,
                    { Name = name
                      Type = Json.strField "type" j |> Option.defaultValue ""
                      Damage = DamageType.all |> List.choose range |> Map.ofList
                      CritChance = Json.numField "CritChanceBase" w |> Option.defaultValue 5.0
                      AttacksPerSecond = Json.numField "AttackRateBase" w |> Option.defaultValue 1.0
                      BaseTags = Json.keys (Json.field "tags" j) |> Set.ofList }))
            |> Map.ofList
        | _ -> Map.empty
