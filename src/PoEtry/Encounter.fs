namespace PoEtry

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

/// How a monster attacks the player.
type EnemyAttack =
    { /// Seconds between hits at normal speed.
      Interval: float
      /// Average damage per hit, by type, before the player's mitigation.
      Damage: Map<DamageType, float>
      Accuracy: float
      /// Distance (game units, edge to player) from which it can hit.
      Range: float
      /// Chance (%) of a critical hit (monsters: base bonus from game data).
      CritChance: float
      /// A melee attack (stuns more; flat physical attack damage reductions apply).
      Melee: bool }

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
      Moving: bool
      Attack: EnemyAttack option
      /// Movement speed towards the player, game units per second.
      Speed: float }

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

/// Damage profiles for monster hits: share of each damage type.
module DamageProfile =
    let mixed = [ Physical, 0.4; Fire, 0.2; Cold, 0.2; Lightning, 0.2 ]
    let physical = [ Physical, 1.0 ]

module MonsterPower =
    /// Monster power by rarity for meta gem energy: normal 1, magic 2, rare 5, unique / boss 20.
    /// Community values (Mobalytics; boss 20 widely agreed, magic and rare disputed).
    let ofRarity (r: Rarity) =
        match r with
        | Normal -> 1.0
        | Magic -> 2.0
        | Rare -> 5.0
        | Unique -> 20.0

module RarityCharges =
    /// Flask charges per kill: 1 for a normal monster, scaled by the rarity's
    /// monster_base_flask_charges_percent_of_monster_power (+50% / +150% / +150%, game data).
    /// The base of 1 is a GUESS.
    let flaskChargesPerKill (r: Rarity) =
        let key = match r with Normal -> "" | Magic -> "Magic" | Rare -> "Rare" | Unique -> "Unique"
        1.0 + (RarityData.stats |> List.tryPick (fun (rr, s, v) -> if rr = key && s = "monster_base_flask_charges_percent_of_monster_power" then Some v else None) |> Option.defaultValue 0.0) / 100.0


module EnemyDef =
    /// Default attack: a hit of 1.5 x the level's monster damage, scaled like PoB's enemy
    /// presets (normal 1/4.4, standard boss 4/4.4), times the rarity's damage modifier (game
    /// data), every 1.5 s from melee range. The per-hit scale is PoB's heuristic; interval,
    /// range and type split are guesses (scenario settings).
    let defaultAttack level (rarity: Rarity) =
        let key = match rarity with Normal -> "" | Magic -> "Magic" | Rare -> "Rare" | Unique -> "Unique"
        let rarityMult =
            1.0 + (RarityData.stats |> List.tryPick (fun (r, s, v) -> if r = key && s = "monster_rarity_damage_+%_final" then Some v else None) |> Option.defaultValue 0.0) / 100.0
        let classMult = if rarity = Unique then 4.0 / 4.4 else 1.0 / 4.4
        let hit = Constants.monsterDamage level * 1.5 * classMult * rarityMult
        { Interval = 1.5
          Damage = DamageProfile.mixed |> List.map (fun (t, k) -> t, hit * k) |> Map.ofList
          Accuracy = Constants.monsterAccuracy level
          Range = 8.0
          // Monster base critical hit chance 5% (PoB enemy default)
          CritChance = 5.0
          Melee = true }

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
          Moving = false
          Attack = Some(defaultAttack level rarity)
          Speed = 35.0 }

    /// A single target with explicit life, for tests and dummy fights.
    let atLevel level life =
        { make Rarity.defaults level Unique "Target" (Vec.make 30.0 0.0) with
            Life = life
            Mods = [ Mod.base' Armour (Constants.monsterArmour level) ]
            Attack = None }

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
        // starting inside the enemy: hit immediately
        if Vec.length toE <= e.Size then Some 0.0 else
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

    /// Where a ground skill lands: `reach` in front of the player towards the aimed
    /// enemy, but not past its centre.
    let impactPoint (reach: float) (primary: EnemyDef) =
        let d = Vec.length primary.Position
        if d <= reach || d = 0.0 then primary.Position
        else Vec.scale (reach / d) primary.Position

    /// A wall segment.
    type Wall = Vec * Vec

    /// One projectile's flight: the points it passes (start, bounces, end) and the enemies it hit.
    type Path =
        { Points: Vec list
          Hit: string list
          /// Distance flown at each hit.
          Dist: float list }

    let private dot (a: Vec) (b: Vec) = a.X * b.X + a.Y * b.Y

    /// Distance along the ray to where it enters an enemy it hasn't hit yet.
    let private entry (origin: Vec) (dir: Vec) (e: EnemyDef) =
        let toE = Vec.sub e.Position origin
        if Vec.length toE <= e.Size then Some 0.0
        else
            let t = dot toE dir
            let c2 = (Vec.length toE) ** 2.0 - t * t
            if t < 0.0 || c2 > e.Size * e.Size then None else Some(t - sqrt (e.Size * e.Size - c2))

    /// Distance along the ray to a wall, and the reflected direction.
    let private wallHit (origin: Vec) (dir: Vec) ((a, b): Wall) =
        let seg = Vec.sub b a
        let denom = dir.X * seg.Y - dir.Y * seg.X
        if abs denom < 1e-12 then None
        else
            let ao = Vec.sub a origin
            let t = (ao.X * seg.Y - ao.Y * seg.X) / denom
            let u = (ao.X * dir.Y - ao.Y * dir.X) / denom
            if t > 1e-6 && u >= 0.0 && u <= 1.0 then
                let n = Vec.scale (1.0 / Vec.length seg) (Vec.make -seg.Y seg.X)
                Some(t, Vec.sub dir (Vec.scale (2.0 * dot dir n) n))
            else None

    /// Fly one projectile: enemies in its way are hit (pierce lets it continue), a wall
    /// stops it unless the terrain-chain roll lets it bounce (once).
    let trace (rng: Rng) (walls: Wall list) (p: ProjectileShape) (origin: Vec) (dir0: Vec) (alive: EnemyDef list) : Path =
        let rec go (pos: Vec) (dir: Vec) remaining (hit: string list) bounced (points: Vec list) (dists: float list) =
            let enemy =
                alive
                |> List.filter (fun e -> not (List.contains e.Name hit))
                |> List.choose (fun e -> entry pos dir e |> Option.map (fun t -> t, e))
                |> List.sortBy fst
                |> List.tryHead
            let wall = walls |> List.choose (wallHit pos dir) |> List.sortBy fst |> List.tryHead
            let wallT = wall |> Option.map fst |> Option.defaultValue infinity
            match enemy with
            | Some (t, e) when t <= remaining && t <= wallT ->
                let at = Vec.add pos (Vec.scale t dir)
                let hit = hit @ [ e.Name ]
                let dists = dists @ [ p.Range - remaining + t ]
                if hit.Length > p.Pierce then { Points = List.rev (at :: points); Hit = hit; Dist = dists }
                else go at dir (remaining - t) hit bounced points dists
            | _ ->
                match wall with
                | Some (t, reflected) when t <= remaining ->
                    let at = Vec.add pos (Vec.scale t dir)
                    if not bounced && rng.Chance p.TerrainChain then
                        // "Chain from terrain": like any chain, it seeks the nearest enemy in chain
                        // range that this projectile hasn't hit. No target: mirror bounce (guess).
                        let target =
                            alive
                            |> List.filter (fun e -> not (List.contains e.Name hit) && Vec.dist at e.Position - e.Size <= p.ChainRange)
                            |> List.sortBy (fun e -> Vec.dist at e.Position)
                            |> List.tryHead
                        let next =
                            match target with
                            | Some e -> let d = Vec.sub e.Position at in Vec.scale (1.0 / Vec.length d) d
                            | None -> reflected
                        // step off the wall so the next segment doesn't hit it again
                        go (Vec.add at (Vec.scale 1e-3 next)) next (remaining - t) hit true (at :: points) dists
                    else { Points = List.rev (at :: points); Hit = hit; Dist = dists }
                | _ -> { Points = List.rev (Vec.add pos (Vec.scale remaining dir) :: points); Hit = hit; Dist = dists }
        // an erratic projectile: it strays sideways within its band, a new offset every band
        // width travelled; each leg is flown and tested like a straight one. A wall ends
        // it, unless it bounces: then it flies on in a random direction (wiki, Bounce).
        let wander () =
            let step = max 1.0 p.Wander
            let rec fly (start: Vec) (dir: Vec) (pos: Vec) along remaining (hit: string list) (dists: float list) (points: Vec list) =
                if remaining <= 1e-9 then { Points = List.rev points; Hit = hit; Dist = dists }
                else
                    let advance = min remaining step
                    let perp = Vec.make -dir.Y dir.X
                    let next = Vec.add (Vec.add start (Vec.scale (along + advance) dir)) (Vec.scale ((rng.Float() - 0.5) * p.Wander) perp)
                    let leg = Vec.sub next pos
                    let len = Vec.length leg
                    if len < 1e-9 then fly start dir next (along + advance) (remaining - advance) hit dists points
                    else
                        let legDir = Vec.scale (1.0 / len) leg
                        let flown = p.Range - remaining
                        let enemy =
                            alive
                            |> List.filter (fun e -> not (List.contains e.Name hit))
                            |> List.choose (fun e -> entry pos legDir e |> Option.map (fun t -> t, e))
                            |> List.filter (fun (t, _) -> t <= len)
                            |> List.sortBy fst
                            |> List.tryHead
                        let wall = walls |> List.choose (wallHit pos legDir) |> List.filter (fun (t, _) -> t <= len) |> List.sortBy fst |> List.tryHead
                        match enemy, wall with
                        | Some (t, e), w when (match w with Some (wt, _) -> t <= wt | None -> true) ->
                            let at = Vec.add pos (Vec.scale t legDir)
                            let hit, dists = hit @ [ e.Name ], dists @ [ flown + t * advance / len ]
                            if hit.Length > p.Pierce then { Points = List.rev (at :: points); Hit = hit; Dist = dists }
                            else fly start dir next (along + advance) (remaining - advance) hit dists (next :: points)
                        | _, Some (t, _) when not p.Bounces ->
                            { Points = List.rev (Vec.add pos (Vec.scale t legDir) :: points); Hit = hit; Dist = dists }
                        | _, Some (t, reflected) ->
                            let at = Vec.add pos (Vec.scale t legDir)
                            // away from the wall, anywhere within 60 degrees of the mirror direction
                            let turned = Vec.ofAngle (Vec.angle reflected + (rng.Float() - 0.5) * System.Math.PI * 2.0 / 3.0)
                            fly (Vec.add at (Vec.scale 1e-3 reflected)) turned (Vec.add at (Vec.scale 1e-3 reflected)) 0.0 (remaining - t * advance / len) hit dists (at :: points)
                        | _ -> fly start dir next (along + advance) (remaining - advance) hit dists (next :: points)
            fly origin dir0 origin 0.0 p.Range [] [] [ origin ]
        if p.Wander > 0.0 then wander () else go origin dir0 p.Range [] false [ origin ] []

    /// Directions of a use's projectiles.
    let private angles (p: ProjectileShape) aimAngle =
        // a fan that widens with every projectile, up to the full circle
        let p = if p.SpreadPerProjectile > 0.0 && p.Spread < 360.0 then { p with Spread = min 360.0 (p.SpreadPerProjectile * float (p.Count - 1)) } else p
        let spread = p.Spread * System.Math.PI / 180.0
        if p.Count <= 1 then [ aimAngle ]
        elif p.Spread >= 360.0 then [ for k in 0 .. p.Count - 1 -> aimAngle + 2.0 * System.Math.PI * float k / float p.Count ]
        else [ for k in 0 .. p.Count - 1 -> aimAngle - spread / 2.0 + spread * float k / float (p.Count - 1) ]

    /// All projectile flights of one use.
    let paths (rng: Rng) (walls: Wall list) (p: ProjectileShape) (reach: float) (primary: EnemyDef) (alive: EnemyDef list) =
        let origin = if p.FromImpact then impactPoint reach primary else Vec.zero
        let alive = if p.SkipPrimary then alive |> List.filter (fun e -> e.Name <> primary.Name) else alive
        // a full circle from the player is not aimed: it lies at a random angle to the enemy
        let turn = if p.Spread >= 360.0 && not p.FromImpact && p.Count > 1 then 2.0 * System.Math.PI * rng.Float() else 0.0
        angles p (Vec.angle primary.Position + turn)
        |> List.map (fun a ->
            let dir = Vec.ofAngle a
            let p = if p.RangeVariation > 0.0 then { p with Range = p.Range * (1.0 + p.RangeVariation * (2.0 * rng.Float() - 1.0)) } else p
            trace rng walls p (Vec.add origin (Vec.scale p.StartOffset dir)) dir alive)

    /// Enemies hit by one use: names, each once per hit it receives.
    let targetsFrom (rng: Rng) (walls: Wall list) (reach: float) (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list =
        match delivery with
        | Single -> [ primary.Name ]
        | AreaAtTarget r ->
            let r = radius r areaMult
            alive |> List.filter (within primary.Position r) |> List.map (fun e -> e.Name)
        | AreaAroundSelf r ->
            let r = radius r areaMult
            alive |> List.filter (within Vec.zero r) |> List.map (fun e -> e.Name)
        | Projectiles ({ Rain = Some (area, impact) } as p) ->
            // every projectile lands at a random point of the area and hits what is in reach
            let area, impact = radius area areaMult, radius impact areaMult
            // "Can Hit each enemy only once every N seconds": what lands on an enemy too soon
            // after the last one that hit it does nothing to it
            let last = System.Collections.Generic.Dictionary<string, float>()
            [ for k in 0 .. p.Count - 1 do
                let r, a = area * sqrt (rng.Float()), 2.0 * System.Math.PI * rng.Float()
                let at =
                    if p.RainCone > 0.0 then
                        Vec.scale r (Vec.ofAngle (Vec.angle primary.Position + (a / (2.0 * System.Math.PI) - 0.5) * p.RainCone * System.Math.PI / 180.0))
                    else Vec.add primary.Position (Vec.scale r (Vec.ofAngle a))
                let now = float k * p.Every
                if not p.TouchTriggered || alive |> List.exists (within at 0.0) then
                    for e in alive |> List.filter (within at impact) do
                        let ok = p.HitInterval <= 0.0 || p.Every <= 0.0 || not (last.ContainsKey e.Name) || now - last.[e.Name] >= p.HitInterval - 1e-9
                        if ok then
                            last.[e.Name] <- now
                            yield e.Name ]
        | Projectiles ({ Scatter = Some (near, far, cone, impact) } as p) ->
            let impact = radius impact areaMult
            let away = Vec.angle primary.Position
            [ for _ in 1 .. p.Count do
                let a = away + (rng.Float() - 0.5) * cone * System.Math.PI / 180.0
                let d = near + (far - near) * rng.Float()
                let at = Vec.add primary.Position (Vec.scale d (Vec.ofAngle a))
                yield! alive |> List.filter (within at impact) |> List.map (fun e -> e.Name) ]
        | Projectiles p when p.HitInterval > 0.0 && p.Speed > 0.0 ->
            // every crossing of every projectile, in the order they happen: a target takes a
            // hit from this volley only if the last one is long enough ago; a projectile that
            // may not hit passes through, one that hits is spent (pierce: so many more)
            let crossings =
                paths rng walls { p with Pierce = 1000 } reach primary alive
                |> List.mapi (fun i path -> List.zip path.Hit path.Dist |> List.map (fun (n, d) -> d / p.Speed, i, n))
                |> List.concat
                |> List.sortBy (fun (t, _, _) -> t)
            crossings
            |> List.fold
                (fun (acc, last: Map<string, float>, used: Map<int, int>) (t, i, n) ->
                    let spent = (Map.tryFind i used |> Option.defaultValue 0) > p.Pierce
                    let tooSoon = Map.tryFind n last |> Option.exists (fun t0 -> t - t0 < p.HitInterval)
                    if spent || tooSoon then acc, last, used
                    else n :: acc, Map.add n t last, Map.add i ((Map.tryFind i used |> Option.defaultValue 0) + 1) used)
                ([], Map.empty, Map.empty)
            |> fun (acc, _, _) -> List.rev acc
        | Projectiles p ->
            let explode (at: Vec) =
                match p.Explosion with
                | Some r ->
                    let r = radius r areaMult
                    alive |> List.filter (within at r) |> List.map (fun e -> e.Name)
                | None -> []
            let byName n = alive |> List.find (fun e -> e.Name = n)
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
            paths rng walls p reach primary alive
            |> List.fold
                (fun (acc, hitByUse: Set<string>) path ->
                    // without shotgun, a later projectile ignores enemies an earlier one hit
                    let hits = if p.CanShotgun then path.Hit else path.Hit |> List.filter (fun n -> not (hitByUse.Contains n))
                    let struck = hits |> List.map byName
                    let chained =
                        match List.tryLast struck with
                        | Some last -> chain last p.Chains (hits |> Set.ofList)
                        | None -> []
                    let all = struck @ chained
                    let names = (all |> List.map (fun e -> e.Name)) @ (all |> List.collect (fun e -> explode e.Position))
                    acc @ names, names |> List.fold (fun s n -> s.Add n) hitByUse)
                ([], Set.empty)
            |> fst

    /// `targetsFrom` with the slam landing on the aimed enemy.
    let targets delivery areaMult primary alive = targetsFrom (Rng 1u) [] infinity delivery areaMult primary alive

/// Arenas: walls around the fight.
module Arena =
    let openField : Geometry.Wall list = []

    /// A round room of `radius` around `center`, as 32 wall segments.
    let round (center: Vec) radius : Geometry.Wall list =
        let n = 32
        [ for k in 0 .. n - 1 ->
            let a k = 2.0 * System.Math.PI * float k / float n
            Vec.add center (Vec.scale radius (Vec.ofAngle (a k))), Vec.add center (Vec.scale radius (Vec.ofAngle (a (k + 1)))) ]

    /// A long straight wall through `point`, perpendicular to `normal`.
    let straight (point: Vec) (normal: Vec) length : Geometry.Wall list =
        let n = Vec.scale (1.0 / max 1e-9 (Vec.length normal)) normal
        let along = Vec.make -n.Y n.X
        [ Vec.add point (Vec.scale -length along), Vec.add point (Vec.scale length along) ]

    /// A corridor of `width` along the line from the player through `towards`.
    let corridor (towards: Vec) width length : Geometry.Wall list =
        let d = Vec.scale (1.0 / max 1e-9 (Vec.length towards)) towards
        let n = Vec.make -d.Y d.X
        let side k = Vec.scale (k * width / 2.0) n
        [ for k in [ 1.0; -1.0 ] -> Vec.add (side k) (Vec.scale -length d), Vec.add (side k) (Vec.scale length d) ]
