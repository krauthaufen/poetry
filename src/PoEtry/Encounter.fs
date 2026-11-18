namespace PoEtry

/// 2D position in game units (10 units = 1 metre). The player stands at the origin
/// facing +X.
[<Struct>]
type Vec =
    { X: float
      Y: float }

module Vec =
    let zero = { X = 0.0; Y = 0.0 }
    let inline make (x: float) (y: float) : Vec = { X = x; Y = y }
    let inline sub (a: Vec) (b: Vec) : Vec = { X = a.X - b.X; Y = a.Y - b.Y }
    let inline add (a: Vec) (b: Vec) : Vec = { X = a.X + b.X; Y = a.Y + b.Y }
    let inline scale (k: float) (a: Vec) : Vec = { X = a.X * k; Y = a.Y * k }
    let inline length (a: Vec) : float = sqrt (a.X * a.X + a.Y * a.Y)
    let inline dist (a: Vec) (b: Vec) : float = length (sub a b)
    let angle a = atan2 a.Y a.X
    let ofAngle r = { X = cos r; Y = sin r }

/// What a skill use looks like on the ground, for showing a fight: an area (so long after
/// the use), a projectile's way (start, bounces, end), a line between two places (a chain, a
/// strike).
type Mark =
    | Ring of centre: Vec * radius: float * delay: float
    | Flight of points: Vec list
    | Line of from: Vec * towards: Vec

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
      Speed: float
      /// The rare monster modifiers it carries (ids of `RareModsData.mods`); only what a
      /// player gains by killing it reads them (`Reaction.GainRareMods`).
      Affixes: string list }

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

    let private potencies = [| for r in [ Normal; Magic; Rare; Unique ] -> 1.0 + stat r "slow_potency_+%_final_from_rarity" / 100.0 |]

    /// How much of a slow a monster of that rarity takes: "15% less Slow effect on Magic
    /// monsters", 30% Rare, 50% Unique (`slow_potency_+%_final_from_rarity`).
    let slowPotency r = potencies.[rank r]

    /// A slow of `more`% (negative) as that rarity takes it; what speeds it up is left alone.
    let slowed r (mult: float) = if mult < 1.0 then 1.0 - (1.0 - mult) * slowPotency r else mult

    /// The share of leech a monster of that level lets through (DefaultMonsterStats
    /// LeechResistance: none below level 31, 92% at 80).
    let leechKept level =
        let t = RarityData.leechResistance
        1.0 - t.[max 0 (min (t.Length - 1) (level - 1))] / 100.0

    /// % more damage a minion of that level deals with hits and ailments against monsters that
    /// are not unique (DefaultMonsterStats MoreDamageAgainstNonUnique: 3% at level 8, 50% from 25).
    let minionMoreAgainstNonUnique level =
        let t = RarityData.minionMoreAgainstNonUnique
        t.[max 0 (min (t.Length - 1) (level - 1))]

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
          Speed = 35.0
          Affixes = [] }

    /// A single target with explicit life, for tests and dummy fights.
    let atLevel level life =
        { make Rarity.defaults level Unique "Target" (Vec.make 30.0 0.0) with
            Life = life
            Mods = [ Mod.base' Armour (Constants.monsterArmour level) ]
            Attack = None }

/// The modifiers of rare monsters (game data, RareModsData.fs).
module RareMods =
    /// "Rare Monsters have up to 4 [modifiers] by default" (poe2db, Monster Modifiers). How
    /// many of the four a rare has is not known: a GUESS of one to four, all as likely.
    let mostPerRare = 4

    /// The buff a player has while it has a modifier of a killed rare (Headhunter).
    let buffName (id: string) = "Rare modifier: " + id

    /// The mods a monster of this level can roll: (id, family, weight).
    let pool level =
        RareModsData.mods |> List.filter (fun (_, _, lowest, _, w, _) -> lowest <= level && w > 0.0) |> List.map (fun (id, _, _, fam, w, _) -> id, fam, w)

    /// A rare's modifiers: one to `mostPerRare`, each drawn by its spawn weight from those
    /// of a family it has none of yet (the game's Families: one aura, one ground effect...).
    let roll (rng: Rng) level =
        let n = 1 + min (mostPerRare - 1) (int (rng.Float() * float mostPerRare))
        let rec draw k (left: (string * int * float) list) acc =
            if k = 0 || left.IsEmpty then List.rev acc
            else
                let total = left |> List.sumBy (fun (_, _, w) -> w)
                let x = rng.Float() * total
                let rec pick (rest: (string * int * float) list) sum =
                    match rest with
                    | [ last ] -> last
                    | (id, f, w) :: tail -> if x < sum + w then (id, f, w) else pick tail (sum + w)
                    | [] -> failwith "empty"
                let id, fam, _ = pick left 0.0
                draw (k - 1) (left |> List.filter (fun (_, f, _) -> f <> fam)) (id :: acc)
        draw n (pool level) []

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
        // (the rares' modifiers from a generator of their own: the places stay what they were)
        let modRng = Rng(seed ^^^ 0x5A17C0DEu)
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
            let e = EnemyDef.make profile level r (sprintf "%s %d" (Rarity.name r) (i + 1)) pos
            if r = Rare then { e with Affixes = RareMods.roll modRng level } else e)

/// Who a skill use hits.
module Geometry =
    /// The enemy the player aims at among the living.
    let aim targeting (alive: EnemyDef list) =
        match targeting with
        | Nearest -> alive |> List.sortBy (fun e -> Vec.length e.Position) |> List.tryHead
        | Strongest -> alive |> List.sortBy (fun e -> -(Rarity.rank e.Rarity), Vec.length e.Position) |> List.tryHead

    /// Which melee minions find room at an enemy of this radius. Each is a body of some radius
    /// (MonsterVarieties ObjectSize) with an attack that reaches so far from its centre. They
    /// stand in rings around the enemy: in ring j a minion's centre is (2j + 1) of its radii
    /// from the enemy's edge, where it takes up the angle its body covers from there; a ring
    /// is too far out for it once that distance is more than its reach. In the order given,
    /// each takes the innermost ring that has the angle left.
    let meleePlaces (enemyRadius: float) (minions: (float * float) list) : bool list =
        let place (used: Map<int, float>) (radius: float, reach: float) =
            let radius = max 0.1 radius
            let rec ring j =
                if float (2 * j + 1) * radius > reach && j > 0 then None
                else
                    let d = enemyRadius + radius * float (2 * j + 1)
                    let angle = 2.0 * asin (min 1.0 (radius / d))
                    let taken = Map.tryFind j used |> Option.defaultValue 0.0
                    if taken + angle <= 2.0 * System.Math.PI + 1e-9 then Some(j, taken + angle) else ring (j + 1)
            ring 0
        minions
        |> List.fold
            (fun (used, acc) m ->
                match place used m with
                | Some (j, taken) -> Map.add j taken used, true :: acc
                | None -> used, false :: acc)
            (Map.empty, [])
        |> snd
        |> List.rev

    let within center radius (e: EnemyDef) = Vec.dist center e.Position <= radius + e.Size

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

    /// `entry` as a number: NaN where the ray does not enter the enemy.
    let private entryAt (origin: Vec) (dir: Vec) (e: EnemyDef) : float =
        let toE = Vec.sub e.Position origin
        if Vec.length toE <= e.Size then 0.0
        else
            let t = dot toE dir
            let c2 = (Vec.length toE) ** 2.0 - t * t
            if t < 0.0 || c2 > e.Size * e.Size then nan else t - sqrt (e.Size * e.Size - c2)

    /// The enemy the ray enters first, of those it has not hit yet and (`within`) enters no
    /// further on than that, with the distance to it; of several at the same distance the
    /// first in the list. (What sorting them by the distance and taking the first gives.)
    let private nearest (origin: Vec) (dir: Vec) (hit: string list) (within: float) (alive: EnemyDef list) : (struct (float * EnemyDef)) voption =
        let mutable best : (struct (float * EnemyDef)) voption = ValueNone
        let mutable least = infinity
        let mutable rest = alive
        while not rest.IsEmpty do
            let e = rest.Head
            if hit.IsEmpty || not (List.contains e.Name hit) then
                // (the ray enters no nearer than the enemy's edge is: one whose edge is out of
                // reach, by more than any rounding, is not worked out)
                let away = Vec.dist e.Position origin
                let t = if away - e.Size > within + 1e-3 + 1e-6 * away then nan else entryAt origin dir e
                if t <= within && (best.IsNone || t < least) then
                    best <- ValueSome(struct (t, e))
                    least <- t
            rest <- rest.Tail
        best

    /// The wall the ray meets first (`within`: no further on than that), likewise.
    let private nearestWall (origin: Vec) (dir: Vec) (within: float) (walls: Wall list) : (float * Vec) option =
        if walls.IsEmpty then None
        else walls |> List.choose (wallHit origin dir) |> List.filter (fun (t, _) -> t <= within) |> List.sortBy fst |> List.tryHead

    /// A projectile that flies straight (see `trace`), from where it is on.
    let rec private straight (rng: Rng) (walls: Wall list) (p: ProjectileShape) (range: float) (pierce: int) (alive: EnemyDef list) (pos: Vec) (dir: Vec) remaining (hit: string list) bounced (points: Vec list) (dists: float list) : Path =
        let enemy = nearest pos dir hit infinity alive
        let wall = nearestWall pos dir infinity walls
        let wallT = match wall with Some (t, _) -> t | None -> infinity
        match enemy with
        | ValueSome (struct (t, e)) when t <= remaining && t <= wallT ->
            let at = Vec.add pos (Vec.scale t dir)
            let hit = hit @ [ e.Name ]
            let dists = dists @ [ range - remaining + t ]
            if hit.Length > pierce then { Points = List.rev (at :: points); Hit = hit; Dist = dists }
            else straight rng walls p range pierce alive at dir (remaining - t) hit bounced points dists
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
                    straight rng walls p range pierce alive (Vec.add at (Vec.scale 1e-3 next)) next (remaining - t) hit true (at :: points) dists
                else { Points = List.rev (at :: points); Hit = hit; Dist = dists }
            | _ -> { Points = List.rev (Vec.add pos (Vec.scale remaining dir) :: points); Hit = hit; Dist = dists }

    /// How far along its line an erratic projectile can still meet an enemy it has not hit:
    /// it never leaves the band of half that width about the line, so it meets only enemies
    /// that reach into the band, and none once it is past them all (with room to spare for
    /// rounding). Minus infinity: none at all.
    let private lastInBand (start: Vec) (dir: Vec) (halfWidth: float) (hit: string list) (alive: EnemyDef list) : float =
        let mutable last = -infinity
        let mutable rest = alive
        while not rest.IsEmpty do
            let e = rest.Head
            if hit.IsEmpty || not (List.contains e.Name hit) then
                let toE = Vec.sub e.Position start
                let along = dot toE dir
                let aside = abs (dir.X * toE.Y - dir.Y * toE.X)
                let spare = 1e-3 + 1e-6 * (abs along + aside)
                if aside <= halfWidth + e.Size + spare && along + e.Size + spare > last then last <- along + e.Size + spare
            rest <- rest.Tail
        last

    /// An erratic projectile (see `trace`): it strays sideways within its band, a new offset every band
    /// width travelled; each leg is flown and tested like a straight one. A wall ends
    /// it, unless it bounces: then it flies on in a random direction (wiki, Bounce).
    /// `keep`: whether the points it passes are wanted (they are for showing it, and for
    /// following it tick by tick).
    let private erratic (keep: bool) (rng: Rng) (walls: Wall list) (p: ProjectileShape) (range: float) (pierce: int) (origin: Vec) (dir0: Vec) (alive: EnemyDef list) : Path =
        let step = max 1.0 p.Wander
        let points = if keep then ResizeArray<Vec>() else null
        if keep then points.Add origin
        let mutable start = origin
        let mutable dir = dir0
        let mutable pos = origin
        let mutable along = 0.0
        let mutable remaining = range
        let mutable hit : string list = []
        let mutable dists : float list = []
        let mutable flying = true
        // where nothing is shown and no wall stands, the legs past the last enemy it can meet
        // are not flown: only their random numbers are drawn
        let skips = not keep && walls.IsEmpty
        let mutable until = if skips then lastInBand origin dir0 (0.5 * abs p.Wander) hit alive else infinity
        while flying && not (remaining <= 1e-9) do
          let advance = min remaining step
          if along > until then
            rng.Float() |> ignore
            remaining <- remaining - advance
          else
            let perp = Vec.make -dir.Y dir.X
            let next = Vec.add (Vec.add start (Vec.scale (along + advance) dir)) (Vec.scale ((rng.Float() - 0.5) * p.Wander) perp)
            let leg = Vec.sub next pos
            let len = Vec.length leg
            if len < 1e-9 then
                pos <- next
                along <- along + advance
                remaining <- remaining - advance
            else
                let legDir = Vec.scale (1.0 / len) leg
                let flown = range - remaining
                let enemy = nearest pos legDir hit len alive
                let wall = nearestWall pos legDir len walls
                // an enemy in the way, unless a wall comes first
                let struck =
                    match enemy with
                    | ValueSome (struct (t, _)) -> (match wall with Some (wt, _) -> t <= wt | None -> true)
                    | ValueNone -> false
                if struck then
                    let struct (t, e) = enemy.Value
                    let at = Vec.add pos (Vec.scale t legDir)
                    hit <- hit @ [ e.Name ]
                    dists <- dists @ [ flown + t * advance / len ]
                    if hit.Length > pierce then
                        if keep then points.Add at
                        flying <- false
                    else
                        pos <- next
                        along <- along + advance
                        remaining <- remaining - advance
                        if keep then points.Add next
                        if skips then until <- lastInBand start dir (0.5 * abs p.Wander) hit alive
                else
                    match wall with
                    | Some (t, _) when not p.Bounces ->
                        if keep then points.Add(Vec.add pos (Vec.scale t legDir))
                        flying <- false
                    | Some (t, reflected) ->
                        let at = Vec.add pos (Vec.scale t legDir)
                        // away from the wall, anywhere within 60 degrees of the mirror direction
                        let turned = Vec.ofAngle (Vec.angle reflected + (rng.Float() - 0.5) * System.Math.PI * 2.0 / 3.0)
                        start <- Vec.add at (Vec.scale 1e-3 reflected)
                        dir <- turned
                        pos <- Vec.add at (Vec.scale 1e-3 reflected)
                        along <- 0.0
                        remaining <- remaining - t * advance / len
                        if keep then points.Add at
                    | None ->
                        pos <- next
                        along <- along + advance
                        remaining <- remaining - advance
                        if keep then points.Add next
        { Points = (if keep then List.ofSeq points else []); Hit = hit; Dist = dists }

    /// `trace`; `keep`: see `erratic`; `range`: how far this one flies (the shape's range,
    /// or what a variation makes of it); `pierce`: how many enemies it flies on through.
    let private traceWith (keep: bool) (rng: Rng) (walls: Wall list) (p: ProjectileShape) (range: float) (pierce: int) (origin: Vec) (dir0: Vec) (alive: EnemyDef list) : Path =
        if p.Wander > 0.0 then erratic keep rng walls p range pierce origin dir0 alive
        else straight rng walls p range pierce alive origin dir0 range [] false [ origin ] []

    /// Fly one projectile: enemies in its way are hit (pierce lets it continue), a wall
    /// stops it unless the terrain-chain roll lets it bounce (once).
    let trace (rng: Rng) (walls: Wall list) (p: ProjectileShape) (origin: Vec) (dir0: Vec) (alive: EnemyDef list) : Path =
        traceWith true rng walls p p.Range p.Pierce origin dir0 alive

    /// Directions of a use's projectiles.
    let private angles (p: ProjectileShape) aimAngle =
        // a fan that widens with every projectile, up to the full circle
        let p = if p.SpreadPerProjectile > 0.0 && p.Spread < 360.0 then { p with Spread = min 360.0 (p.SpreadPerProjectile * float (p.Count - 1)) } else p
        let spread = p.Spread * System.Math.PI / 180.0
        if p.Count <= 1 then [ aimAngle ]
        elif p.Spread >= 360.0 then [ for k in 0 .. p.Count - 1 -> aimAngle + 2.0 * System.Math.PI * float k / float p.Count ]
        else [ for k in 0 .. p.Count - 1 -> aimAngle - spread / 2.0 + spread * float k / float (p.Count - 1) ]

    /// `paths`; `keep`, `pierce`: see `traceWith`. With how far each one flies.
    let private rangedPaths (keep: bool) (pierce: int) (rng: Rng) (walls: Wall list) (p: ProjectileShape) (reach: float) (primary: EnemyDef) (alive: EnemyDef list) =
        let origin = if p.FromImpact then impactPoint reach primary else Vec.zero
        let alive = if p.SkipPrimary then alive |> List.filter (fun e -> e.Name <> primary.Name) else alive
        // a full circle from the player is not aimed: it lies at a random angle to the enemy
        let turn = if p.Spread >= 360.0 && not p.FromImpact && p.Count > 1 then 2.0 * System.Math.PI * rng.Float() else 0.0
        angles p (Vec.angle primary.Position + turn)
        |> List.map (fun a ->
            let dir = Vec.ofAngle a
            let range = if p.RangeVariation > 0.0 then p.Range * (1.0 + p.RangeVariation * (2.0 * rng.Float() - 1.0)) else p.Range
            range, traceWith keep rng walls p range pierce (Vec.add origin (Vec.scale p.StartOffset dir)) dir alive)

    let private pathsWith (keep: bool) (pierce: int) (rng: Rng) (walls: Wall list) (p: ProjectileShape) (reach: float) (primary: EnemyDef) (alive: EnemyDef list) =
        rangedPaths keep pierce rng walls p reach primary alive |> List.map snd

    /// All projectile flights of one use.
    let paths (rng: Rng) (walls: Wall list) (p: ProjectileShape) (reach: float) (primary: EnemyDef) (alive: EnemyDef list) =
        pathsWith true p.Pierce rng walls p reach primary alive

    /// Projectiles that hit what is inside them as they travel (a twister): every flight is
    /// followed tick by tick, and an enemy inside one is hit, but by the whole volley no more
    /// often than once per `HitInterval`. Seconds after the launch, and who.
    let dwellHits (rng: Rng) (walls: Wall list) (p: ProjectileShape) (reach: float) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : (float * string) list =
        let r = radius p.Passing areaMult
        let tick = Constants.serverTick
        // every flight tick by tick, all the flights' places of one tick before those of the
        // next, the flights in their order (what sorting all their places by the time gives).
        // A flight is followed along its legs: `from` is what is left of its points, `gone`
        // how far along the leg it is, a tick's step further every tick.
        let step = p.Speed * tick
        let from = pathsWith true 1000 rng walls p reach primary [] |> List.map (fun path -> path.Points) |> Array.ofList
        let gone : float[] = Array.zeroCreate from.Length
        // per flight, the leg it is on, how long that is, and whether an enemy is so near the
        // leg that a place on it can reach one (no place on a leg is further from the leg's
        // start than the leg is long); where none is, the places are not worked out
        let asked : Vec list[] = Array.create from.Length []
        let legLen : float[] = Array.create from.Length nan
        let near : bool[] = Array.create from.Length false
        // when each enemy was last hit, by its place in the list (NaN: not yet)
        let last : float[] = Array.create (List.length alive) nan
        let hits = ResizeArray<float * string>()
        let mutable flying = from.Length
        let mutable k = 0
        while flying > 0 do
            let t = float k * tick
            flying <- 0
            // (a tick at which every enemy was hit too short a time ago hits none: the
            // flights only move on)
            let mutable anyDue = false
            for place in 0 .. last.Length - 1 do
                if not (t - last.[place] < p.HitInterval - 1e-9) then anyDue <- true
            for flight in 0 .. from.Length - 1 do
              // (still on the leg it was on, and nothing to hit from it: it only moves on)
              if gone.[flight] <= legLen.[flight] && not (anyDue && near.[flight]) then
                gone.[flight] <- gone.[flight] + step
                flying <- flying + 1
              else
                // (on to the leg this tick's place lies on; none: the flight is over)
                let mutable points = from.[flight]
                let mutable d = gone.[flight]
                let mutable len = nan
                let mutable looking = true
                while looking do
                    if points.IsEmpty || points.Tail.IsEmpty then
                        points <- []
                        looking <- false
                    else
                        if not (obj.ReferenceEquals(asked.[flight], points)) then
                            asked.[flight] <- points
                            let a = points.Head
                            let long = Vec.dist a points.Tail.Head
                            legLen.[flight] <- long
                            let mutable any = false
                            let mutable rest = alive
                            while not any && not rest.IsEmpty do
                                let e = rest.Head
                                let away = Vec.dist a e.Position
                                if not (away - long > r + e.Size + 1e-3 + 1e-6 * away) then any <- true
                                rest <- rest.Tail
                            near.[flight] <- any
                        len <- legLen.[flight]
                        if d <= len then looking <- false
                        else
                            d <- d - len
                            points <- points.Tail
                from.[flight] <- points
                if not points.IsEmpty then
                    flying <- flying + 1
                    gone.[flight] <- d + step
                    if anyDue && near.[flight] then
                        let a, b = points.Head, points.Tail.Head
                        let at = Vec.add a (Vec.scale (d / max 1e-9 len) (Vec.sub b a))
                        let mutable rest = alive
                        let mutable place = 0
                        while not rest.IsEmpty do
                            let e = rest.Head
                            // (never hit before, or long enough ago)
                            if not (t - last.[place] < p.HitInterval - 1e-9) && within at r e then
                                hits.Add((t, e.Name))
                                last.[place] <- t
                            rest <- rest.Tail
                            place <- place + 1
            k <- k + 1
        List.ofSeq hits

    /// The places of the numbers in rising order, equal ones in the order they stand in (a
    /// stable sort, as `List.sortBy` is one, without what that costs for a short list).
    let private rising (keys: float[]) : int[] =
        let n = keys.Length
        let order = Array.init n id
        if n > 1 then
            let spare : int[] = Array.zeroCreate n
            // merge sort, runs of 1, 2, 4... merged from one array into the other
            let mutable from = order
            let mutable into = spare
            let mutable width = 1
            while width < n do
                let mutable lo = 0
                while lo < n do
                    let mid = min n (lo + width)
                    let hi = min n (lo + 2 * width)
                    let mutable i = lo
                    let mutable j = mid
                    for k in lo .. hi - 1 do
                        // (the left one unless the right one is smaller: equal ones keep their order)
                        if i < mid && (j >= hi || not (keys.[from.[j]] < keys.[from.[i]])) then
                            into.[k] <- from.[i]
                            i <- i + 1
                        else
                            into.[k] <- from.[j]
                            j <- j + 1
                    lo <- hi
                let swap = from
                from <- into
                into <- swap
                width <- width * 2
            if not (obj.ReferenceEquals(from, order)) then
                for k in 0 .. n - 1 do
                    order.[k] <- from.[k]
        order

    /// Enemies hit by one use: names, each once per hit it receives.
    /// Whom a use hits. `marks`, if given, is told what the use looks like (the very places
    /// and ways rolled here), for showing the fight.
    /// `forked`: where the hits of forked projectiles go (None: with the others, after them).
    let private targetsWith (marks: ResizeArray<Mark> option) (forked: ResizeArray<string> option) (rng: Rng) (walls: Wall list) (reach: float) (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list =
        let mark (m: Mark) = match marks with Some ms -> ms.Add m | None -> ()
        match delivery with
        | Single ->
            mark (Line(Vec.zero, primary.Position))
            [ primary.Name ]
        | AreaAtTarget r ->
            let r = radius r areaMult
            mark (Ring(primary.Position, r, 0.0))
            alive |> List.filter (within primary.Position r) |> List.map (fun e -> e.Name)
        | AreaAroundSelf r ->
            let r = radius r areaMult
            mark (Ring(Vec.zero, r, 0.0))
            alive |> List.filter (within Vec.zero r) |> List.map (fun e -> e.Name)
        | Cone (length, angle, width) ->
            // towards the aimed-at enemy; an enemy is in it if it reaches into it
            let r = radius length areaMult
            let aim = Vec.angle primary.Position
            let dir = Vec.ofAngle aim
            let half = angle * System.Math.PI / 360.0
            mark (Line(Vec.zero, Vec.scale r (Vec.ofAngle (aim - half))))
            mark (Line(Vec.zero, Vec.scale r (Vec.ofAngle (aim + half))))
            alive
            |> List.filter (fun e ->
                let along = dot e.Position dir
                let aside = abs (dir.X * e.Position.Y - dir.Y * e.Position.X)
                within Vec.zero r e && along >= -e.Size && aside <= width / 2.0 + max 0.0 along * tan half + e.Size)
            |> List.map (fun e -> e.Name)
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
                mark (Ring(at, impact, now))
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
                mark (Ring(at, impact, 0.0))
                yield! alive |> List.filter (within at impact) |> List.map (fun e -> e.Name) ]
        | Projectiles p when p.HitInterval > 0.0 && p.Speed > 0.0 ->
            // every crossing of every projectile, in the order they happen: a target takes a
            // hit from this volley only if the last one is long enough ago; a projectile that
            // may not hit passes through, one that hits is spent (pierce: so many more)
            let flights = pathsWith marks.IsSome 1000 rng walls p reach primary alive |> Array.ofList
            let times = ResizeArray<float>(flights.Length)
            let which = ResizeArray<int>(flights.Length)
            let whom = ResizeArray<string>(flights.Length)
            for i in 0 .. flights.Length - 1 do
                let path = flights.[i]
                if marks.IsSome then mark (Flight path.Points)
                let mutable hit = path.Hit
                let mutable dist = path.Dist
                while not hit.IsEmpty do
                    times.Add(dist.Head / p.Speed)
                    which.Add i
                    whom.Add hit.Head
                    hit <- hit.Tail
                    dist <- dist.Tail
            let order = rising (times.ToArray())
            let last = System.Collections.Generic.Dictionary<string, float>()
            let used : int[] = Array.zeroCreate flights.Length
            let hits = ResizeArray<string>()
            for k in 0 .. order.Length - 1 do
                let at = order.[k]
                let t, i, n = times.[at], which.[at], whom.[at]
                let spent = used.[i] > p.Pierce
                let tooSoon =
                    match last.TryGetValue n with
                    | true, t0 -> t - t0 < p.HitInterval
                    | _ -> false
                if not (spent || tooSoon) then
                    hits.Add n
                    last.[n] <- t
                    used.[i] <- used.[i] + 1
            List.ofSeq hits
        | Projectiles ({ BlastOnly = true; Explosion = Some blast } as p) ->
            // a grenade sticks to no one: it lies where it lands and bursts there. Each lands
            // as far out as the enemy it was thrown at, in its own direction of the volley
            let r = radius blast areaMult
            let far = Vec.length primary.Position
            let turn = if p.Spread >= 360.0 && p.Count > 1 then 2.0 * System.Math.PI * rng.Float() else 0.0
            [ for a in angles p (Vec.angle primary.Position + turn) do
                let at = Vec.scale far (Vec.ofAngle a)
                mark (Flight [ Vec.zero; at ])
                mark (Ring(at, r, 0.0))
                yield! alive |> List.filter (within at r) |> List.map (fun e -> e.Name) ]
        | Projectiles p ->
            let explode (at: Vec) =
                match p.Explosion with
                | Some r ->
                    let r = radius r areaMult
                    mark (Ring(at, r, 0.0))
                    alive |> List.filter (within at r) |> List.map (fun e -> e.Name)
                | None -> []
            let byName n = alive |> List.find (fun e -> e.Name = n)
            let rec chain (from: EnemyDef) left (seen: Set<string>) =
                if left = 0 then []
                else
                    // (the nearest in range that it has not been to; of equals the first)
                    let mutable best : EnemyDef option = None
                    let mutable least = infinity
                    let mutable rest = alive
                    while not rest.IsEmpty do
                        let e = rest.Head
                        if not (seen.Contains e.Name) then
                            let d = Vec.dist from.Position e.Position
                            if d <= p.ChainRange && (best.IsNone || d < least) then
                                best <- Some e
                                least <- d
                        rest <- rest.Tail
                    match best with
                    | Some next -> next :: chain next (left - 1) (seen.Add next.Name)
                    | None -> []
            let names = ResizeArray<string>()
            let forkedNames = match forked with Some f -> f | None -> ResizeArray<string>()
            let hitByUse = System.Collections.Generic.HashSet<string>()
            // a flight: its path, how far it could fly, the hits before it (a fork's: those of
            // the projectile it split from, which it cannot hit), how many it flies on through,
            // the forks left, whether it is a fork's
            let mutable flights =
                rangedPaths marks.IsSome p.Pierce rng walls p reach primary alive
                |> List.map (fun (range, path) -> struct (path, range, 0, p.Pierce, p.Forks, false))
            while not flights.IsEmpty do
                let struct (path, range, prior, pierce, forksLeft, isFork) = flights.Head
                flights <- flights.Tail
                if marks.IsSome then mark (Flight path.Points)
                let own = if prior = 0 then path.Hit else List.skip prior path.Hit
                // (a projectile that hit nothing does nothing more)
                if not own.IsEmpty then
                    // without shotgun, a later projectile ignores enemies an earlier one hit
                    let hits = if p.CanShotgun then own else own |> List.filter (fun n -> not (hitByUse.Contains n))
                    let struck = hits |> List.map byName
                    // Fork: the first enemy it does not pierce splits it (before any chain)
                    let forks = forksLeft > 0 && path.Hit.Length > pierce && path.Points.Length >= 2
                    if forks then
                        let at = List.last path.Points
                        let before = path.Points.[path.Points.Length - 2]
                        let d = Vec.sub at before
                        let dir = Vec.scale (1.0 / max 1e-9 (Vec.length d)) d
                        let left = range - List.last path.Dist
                        let n = if rng.Chance p.ForkExtraChance then 3 else 2
                        let a = p.ForkAngle * System.Math.PI / 180.0
                        let turns = if n = 3 then [ -a; 0.0; a ] else [ -a; a ]
                        let children =
                            turns |> List.map (fun t ->
                                let nd = Vec.ofAngle (Vec.angle dir + t)
                                let child = straight rng walls p left path.Hit.Length alive at nd left path.Hit false [ at ] []
                                struct (child, left, path.Hit.Length, path.Hit.Length, forksLeft - 1, true))
                        flights <- children @ flights
                    let chained =
                        match List.tryLast struck with
                        | Some last when not forks -> chain last p.Chains ((if prior = 0 then hits else path.Hit) |> Set.ofList)
                        | _ -> []
                    (match List.tryLast struck with
                     | Some last -> chained |> List.fold (fun (from: EnemyDef) next -> mark (Line(from.Position, next.Position)); next) last |> ignore
                     | None -> ())
                    let all = struck @ chained
                    let into = if isFork then forkedNames else names
                    let from = into.Count
                    for e in all do
                        into.Add e.Name
                    for e in all do
                        for n in explode e.Position do
                            into.Add n
                    for k in from .. into.Count - 1 do
                        hitByUse.Add into.[k] |> ignore
            if forked.IsNone then names.AddRange forkedNames
            List.ofSeq names

    let targetsFrom (rng: Rng) (walls: Wall list) (reach: float) (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list =
        targetsWith None None rng walls reach delivery areaMult primary alive

    /// `targetsFrom`, the hits of forked projectiles apart: (the others, the forks').
    let targetsParted (rng: Rng) (walls: Wall list) (reach: float) (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list * string list =
        let forked = ResizeArray<string>()
        let hit = targetsWith None (Some forked) rng walls reach delivery areaMult primary alive
        hit, List.ofSeq forked

    /// `targetsFrom`, and what the use looks like.
    /// (with the forks' hits apart, as `targetsParted` has them)
    let targetsShown (rng: Rng) (walls: Wall list) (reach: float) (delivery: Delivery) (areaMult: float) (primary: EnemyDef) (alive: EnemyDef list) : string list * string list * Mark list =
        let marks = ResizeArray<Mark>()
        let forked = ResizeArray<string>()
        let hit = targetsWith (Some marks) (Some forked) rng walls reach delivery areaMult primary alive
        hit, List.ofSeq forked, List.ofSeq marks

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
