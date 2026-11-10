namespace PoEsie

/// 2D position in game units (10 units = 1 metre). The player stands at the origin
/// facing +X.
type Vec =
    { X: float
      Y: float }

module Vec =
    let zero = { X = 0.0; Y = 0.0 }
    let make x y = { X = x; Y = y }
    let sub a b = { X = a.X - b.X; Y = a.Y - b.Y }
    let add a b = { X = a.X + b.X; Y = a.Y + b.Y }
    let scale k a = { X = a.X * k; Y = a.Y * k }
    let length a = sqrt (a.X * a.X + a.Y * a.Y)
    let dist a b = length (sub a b)
    let angle a = atan2 a.Y a.X
    let ofAngle r = { X = cos r; Y = sin r }

type Rarity =
    | Normal
    | Magic
    | Rare
    | Unique

/// What rarity does to a monster. Every number here is a setting; the defaults say
/// where they come from (see RULES.md).
type RarityProfile =
    { /// Life multiplier over the monster level's base life.
      Life: float
      /// Elemental resistances.
      ElementalResist: float
      ChaosResist: float
      /// Ailment threshold multiplier (relative to the level's base threshold).
      AilmentThreshold: float
      /// Poise (freeze / electrocute / stun buildup) multiplier.
      Poise: float
      /// More effect of curses / exposure on this monster, percent (-50 = half).
      CurseEffect: float
      ExposureEffect: float
      /// Body radius in game units.
      Size: float }

type EnemyDef =
    { Name: string
      Level: int
      Rarity: Rarity
      Life: float
      /// Resists, armour, thresholds... Defender-side mods.
      Mods: Mod list
      Position: Vec
      Size: float
      /// Bleeding deals double damage to moving enemies.
      Moving: bool }

/// Which enemy the player aims at.
type Targeting =
    | Nearest
    /// Highest rarity first, nearest among those.
    | Strongest

module Rarity =
    let name r =
        match r with
        | Normal -> "normal"
        | Magic -> "magic"
        | Rare -> "rare"
        | Unique -> "unique"

    let rank r =
        match r with
        | Normal -> 0
        | Magic -> 1
        | Rare -> 2
        | Unique -> 3

    /// A stat of the rarity's monster mods (game data, RarityData.fs), 0 if absent.
    let private stat (r: Rarity) id =
        let key =
            match r with
            | Normal -> ""
            | Magic -> "Magic"
            | Rare -> "Rare"
            | Unique -> "Unique"
        RarityData.stats
        |> List.tryPick (fun (rr, s, v) -> if rr = key && s = id then Some v else None)
        |> Option.defaultValue 0.0

    /// Default profiles, from the game's rarity monster mods: life, poise, curse and
    /// exposure effect, size. Rarity carries no resistances; those come from the
    /// monster and the area (scenario settings). Unique resists default to PoB's
    /// boss config (30%; pinnacle 50%). Ailment threshold is not changed by rarity.
    let defaults r =
        let pct id = 1.0 + stat r id / 100.0
        { Life = pct "monster_life_+%_final_from_rarity"
          ElementalResist = (if r = Unique then 30.0 else 0.0)
          ChaosResist = 0.0
          AilmentThreshold = 1.0
          Poise = pct "poise_threshold_+%_final_from_rarity"
          CurseEffect = stat r "curse_effect_on_self_+%_final_from_rarity"
          ExposureEffect = stat r "exposure_effect_on_self_+%_final_from_rarity"
          Size = (if r = Unique then 10.0 else 4.0) * pct "base_actor_scale_+%" }

module EnemyDef =
    /// A monster of the given level and rarity at a position.
    let make (profile: Rarity -> RarityProfile) level rarity name position =
        let p = profile rarity
        { Name = name
          Level = level
          Rarity = rarity
          Life = Constants.monsterLife level * p.Life
          Mods =
            [ Mod.base' Armour (Constants.monsterArmour level)
              Mod.base' (Resist Chaos) p.ChaosResist
              Mod.more AilmentThreshold ((p.AilmentThreshold - 1.0) * 100.0)
              Mod.more PoiseThreshold ((p.Poise - 1.0) * 100.0)
              Mod.more (Named "CurseEffectOnSelf") p.CurseEffect
              Mod.more (Named "ExposureEffectOnSelf") p.ExposureEffect ]
            @ Mod.resistAllElemental p.ElementalResist
          Position = position
          Size = p.Size
          Moving = false }

    /// A single target with explicit life, for tests and dummy fights.
    let atLevel level life =
        { make Rarity.defaults level Unique "Target" (Vec.make 30.0 0.0) with
            Life = life
            Mods = [ Mod.base' Armour (Constants.monsterArmour level) ] }

/// Fight setups.
module Encounter =
    /// One boss, `distance` units in front of the player.
    let boss level (profile: Rarity -> RarityProfile) distance =
        [ EnemyDef.make profile level Unique "Boss" (Vec.make distance 0.0) ]

    /// A pack: monsters placed at random (seeded) in a disc of `radius` centred
    /// `distance` units in front of the player. With `around` they surround the
    /// player in a ring at `distance` instead.
    let pack level (profile: Rarity -> RarityProfile) (counts: (Rarity * int) list) distance radius around (seed: uint32) =
        let rng = Rng(seed)
        let members = counts |> List.collect (fun (r, n) -> List.replicate n r)
        members
        |> List.mapi (fun i r ->
            let pos =
                if around then
                    let a = 2.0 * System.Math.PI * (float i + rng.Float() * 0.5) / float members.Length
                    Vec.scale (distance + (rng.Float() - 0.5) * radius) (Vec.ofAngle a)
                else
                    let a = rng.Float() * 2.0 * System.Math.PI
                    let d = radius * sqrt (rng.Float())
                    Vec.add (Vec.make distance 0.0) (Vec.scale d (Vec.ofAngle a))
            EnemyDef.make profile level r (sprintf "%s %d" (Rarity.name r) (i + 1)) pos)

/// Who a skill use hits.
module Geometry =
    /// The enemy the player aims at among the living.
    let aim targeting (alive: EnemyDef list) =
        match targeting with
        | Nearest -> alive |> List.sortBy (fun e -> Vec.length e.Position) |> List.tryHead
        | Strongest -> alive |> List.sortBy (fun e -> -(Rarity.rank e.Rarity), Vec.length e.Position) |> List.tryHead

    let private within center radius (e: EnemyDef) = Vec.dist center e.Position <= radius + e.Size

    /// Distance along a ray from `origin` in direction `dir` (unit) at which it enters
    /// the enemy's circle, if it does within `range`.
    let private rayHit (origin: Vec) (dir: Vec) range (e: EnemyDef) =
        let toE = Vec.sub e.Position origin
        let t = toE.X * dir.X + toE.Y * dir.Y
        let closest2 = (Vec.length toE) ** 2.0 - t * t
        let r2 = e.Size * e.Size
        if t < 0.0 || closest2 > r2 then None
        else
            let entry = t - sqrt (r2 - closest2)
            if entry <= range then Some entry else None

    /// Area radius after "increased area of effect": PoB calcRadius,
    /// floor(base * floor(100 * sqrt(areaMultiplier)) / 100).
    let radius baseRadius areaMult =
        floor (baseRadius * floor (100.0 * sqrt areaMult) / 100.0)

    /// Enemies hit by one use: names, each once per hit it receives.
    let targets (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list =
        match delivery with
        | Single -> [ primary.Name ]
        | AreaAtTarget r ->
            let r = radius r areaMult
            alive |> List.filter (within primary.Position r) |> List.map (fun e -> e.Name)
        | AreaAroundSelf r ->
            let r = radius r areaMult
            alive |> List.filter (within Vec.zero r) |> List.map (fun e -> e.Name)
        | Projectiles p ->
            let aimAngle = Vec.angle primary.Position
            let spread = p.Spread * System.Math.PI / 180.0
            let angles =
                if p.Count <= 1 then [ aimAngle ]
                else [ for k in 0 .. p.Count - 1 -> aimAngle - spread / 2.0 + spread * float k / float (p.Count - 1) ]
            let explode (at: Vec) =
                match p.Explosion with
                | Some r ->
                    let r = radius r areaMult
                    alive |> List.filter (within at r) |> List.map (fun e -> e.Name)
                | None -> []
            let fly (hitByUse: Set<string>) angle =
                let dir = Vec.ofAngle angle
                // Along the ray, in order; enemies this use already hit are passed through.
                let onPath =
                    alive
                    |> List.choose (fun e -> rayHit Vec.zero dir p.Range e |> Option.map (fun t -> t, e))
                    |> List.sortBy fst
                    |> List.map snd
                    |> List.filter (fun e -> p.CanShotgun || not (hitByUse.Contains e.Name))
                let pierced = onPath |> List.truncate (p.Pierce + 1)
                let rec chain (from: EnemyDef) left (seen: Set<string>) =
                    if left = 0 then []
                    else
                        alive
                        |> List.filter (fun e -> not (seen.Contains e.Name) && Vec.dist from.Position e.Position <= p.ChainRange)
                        |> List.sortBy (fun e -> Vec.dist from.Position e.Position)
                        |> List.tryHead
                        |> function
                            | Some next -> next :: chain next (left - 1) (seen.Add next.Name)
                            | None -> []
                match List.tryLast pierced with
                | None -> []
                | Some last ->
                    let chained = chain last p.Chains (pierced |> List.map (fun e -> e.Name) |> Set.ofList)
                    let struck = pierced @ chained
                    (struck |> List.map (fun e -> e.Name)) @ (struck |> List.collect (fun e -> explode e.Position))
            angles
            |> List.fold
                (fun (acc, hitByUse: Set<string>) angle ->
                    let hits = fly hitByUse angle
                    acc @ hits, hits |> List.fold (fun s n -> s.Add n) hitByUse)
                ([], Set.empty)
            |> fst
