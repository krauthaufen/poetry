/// Rules: what an actor does besides hitting when something happens (`Hook` and `Reaction`).
///
/// Each kind of `Reaction` has its own function here; `perform` at the end of them says which
/// one a reaction is. A new reaction: a case in `Reaction` (Model.fs), a function here, and a
/// line in `perform`.
[<AutoOpen>]
module internal PoEtry.Engine.Reactions

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// A use of a skill that has begun, as what it sets going sees it.
type StartedUse =
    { Env: Env
      Rng: Rng
      Actor: Actor
      Skill: Skill
      /// The enemy aimed at.
      Target: EnemyState
      /// The actor's state as the use began (its cooldown started, its Rage spent).
      State: ActorState
      /// The actor's modifiers with the skill, and what they are asked with.
      Ctx: Ctx
      Mods: ModDb
      /// How long the use takes (0: triggered).
      UseTime: float
      /// When each of its hits lands, and the damage multiplier of each; the first of those times.
      Times: (float * float) list
      First: float
      /// A chained attack: this use's place in the chain.
      Step: int option }

/// What the reactions of one rule share: whose rule it is, the skill that set it off, and
/// the enemy the triggering event concerns (the one hit, or the one aimed at).
type Reacting =
    { Env: Env
      Rng: Rng
      Actor: Actor
      Skill: Skill
      Target: string option }

/// What `f` makes of the world with the enemy concerned, if there is one and it lives.
let onTarget (c: Reacting) (w: World) (f: string -> World) : World =
    match c.Target with
    | Some t when w.Enemies.[t].Life > 0.0 -> f t
    | _ -> w

// ---- conditions: reactions that let another one happen ----

/// `SpendStack`: one stack of a buff is used up, if the actor has one (else: nothing).
let spendStack (c: Reacting) buff (w: World) : World option =
    let st = Map.find c.Actor.Name w.Actors
    match Map.tryFind buff st.Buffs with
    | Some e when e.Stacks > 0 && e.Expires > w.Time ->
        let st' = if e.Stacks = 1 then { st with Buffs = Map.remove buff st.Buffs } else { st with Buffs = Map.add buff { e with Stacks = e.Stacks - 1 } st.Buffs }
        Some { w with Actors = Map.add c.Actor.Name st' w.Actors }
    | _ -> None

/// `IfBuff`: whether the actor has the buff now.
let hasBuff (c: Reacting) buff (w: World) : bool =
    match Map.tryFind buff (Map.find c.Actor.Name w.Actors).Buffs with
    | Some e when e.Stacks > 0 && e.Expires > w.Time -> true
    | _ -> false

/// `Every`: at most once in so many seconds; the world with the time noted, if it is due.
let everyDue (c: Reacting) key seconds (w: World) : World option =
    let k = "Every:" + key
    if w.Time >= (Map.tryFind k (Map.find c.Actor.Name w.Actors).Energy |> Option.defaultValue -infinity) - 1e-9 then
        { w with Counts = bump ("every:" + key) w.Counts }
        |> updateActor c.Actor.Name (fun st -> { st with Energy = Map.add k (w.Time + seconds) st.Energy })
        |> Some
    else None

/// `IfEnemyHas`: whether the enemy concerned lives and has the status.
let targetHas (c: Reacting) st (w: World) : bool =
    match c.Target with
    | Some t when w.Enemies.[t].Life > 0.0 && (status w.Enemies.[t]).ContainsKey st -> true
    | _ -> false

// ---- buffs and stacks of the actor ----

/// `SetStacks`: the buff has that many stacks from now on.
let setStacks (c: Reacting) buff n (w: World) =
    updateActor c.Actor.Name (fun st -> { st with Buffs = Map.add buff { Name = buff; Mods = []; Stacks = n; Expires = infinity } st.Buffs }) w

/// `Empower`: stacks of a buff by the power of the monsters in the fight (a warcry).
let empower (c: Reacting) buff perPower cap (w: World) =
    let power = alive w |> List.sumBy (fun e -> MonsterPower.ofRarity e.Def.Rarity)
    let n = max 1 (int (floor (min cap power / max 1.0 perPower)))
    { w with Counts = bump ("empowered:" + string n) w.Counts }
    |> updateActor c.Actor.Name (fun st -> { st with Buffs = Map.add buff { Name = buff; Mods = []; Stacks = n; Expires = infinity } st.Buffs })

/// `GrantOneOf`: one of the buffs, picked at random, in place of the others.
let grantOneOf (c: Reacting) (names: string list) duration (w: World) =
    if names.IsEmpty then w
    else
        let pick = names.[min (names.Length - 1) (int (c.Rng.Float() * float names.Length))]
        updateActor c.Actor.Name (fun st ->
            let buffs = names |> List.fold (fun b n -> Map.remove n b) st.Buffs
            { st with Buffs = Map.add pick { Name = pick; Mods = []; Stacks = 1; Expires = w.Time + duration } buffs }) w
        |> schedule (w.Time + duration) Expire

/// `GrantBuff`: a buff on the actor (one stack more of it, up to its most).
let grantBuff (c: Reacting) name duration maxStacks mods (w: World) =
    let w =
        updateActor c.Actor.Name (fun st ->
            let e = stackEffect w.Time (Map.tryFind name st.Buffs) name duration maxStacks mods
            { st with Buffs = Map.add name e st.Buffs }) w
    schedule (w.Time + duration) Expire w

/// `RemoveBuff`: the actor loses the buff.
let removeBuff (c: Reacting) name (w: World) =
    updateActor c.Actor.Name (fun st -> { st with Buffs = Map.remove name st.Buffs }) w

/// `GrantMinionBuff`: a buff on every minion there is.
let grantMinionBuff (c: Reacting) name duration mods (w: World) =
    c.Env.Fight.Minions
    |> List.fold (fun w (m: Actor) ->
        if Map.containsKey m.Name w.Actors then
            updateActor m.Name (fun st -> { st with Buffs = Map.add name { Name = name; Mods = mods; Stacks = 1; Expires = w.Time + duration } st.Buffs }) w
        else w) w
    |> schedule (w.Time + duration) Expire

/// `ChargeBuff`: a buff with a stack for every charge of a kind the actor holds.
let chargeBuff (c: Reacting) name charge perStack seconds doubleChance (w: World) =
    let held = Map.tryFind charge (Map.find c.Actor.Name w.Actors).Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
    if held = 0 then w
    else
        let stacks = if c.Rng.Chance doubleChance then 2 * held else held
        // mods are per stack: the buff's stacks scale them
        let e = { Name = name; Mods = perStack; Stacks = stacks; Expires = w.Time + seconds * float stacks }
        updateActor c.Actor.Name (fun st -> { st with Buffs = Map.add name e st.Buffs }) w
        |> schedule e.Expires Expire
        |> fun w -> { w with Counts = bump ("buff:" + name) w.Counts }

/// `GainRareMods`: the modifiers of the rare monster concerned, as buffs on the actor.
let gainRareMods (c: Reacting) duration (w: World) =
    match c.Target with
    | Some t ->
        w.Enemies.[t].Def.Affixes
        |> List.fold (fun w id ->
            let name = RareMods.buffName id
            updateActor c.Actor.Name (fun st ->
                { st with Buffs = Map.add name (stackEffect w.Time (Map.tryFind name st.Buffs) name duration 1 []) st.Buffs }) w
            |> schedule (w.Time + duration) Expire
            |> fun w -> { w with Counts = bump "rare modifiers gained" w.Counts }) w
    | None -> w

/// `DropRemnant`: a remnant drops and is picked up a little later (sometimes two).
let dropRemnant (c: Reacting) r delay extra (w: World) =
    let n = if c.Rng.Chance extra then 2 else 1
    { w with Counts = bump ("remnant:" + r) w.Counts } |> schedule (w.Time + delay) (GainStacks(c.Actor.Name, r, n))

// ---- the player's own pools ----

/// `RegenES`: energy shield regenerates for a while (one such stream at a time).
let regenES (rate: float) (duration: float) (w: World) =
    match w.Player with
    | Some p when p.Died.IsNone ->
        { w with Player = Some { p with Recoups = { Until = w.Time + duration; Rate = rate; Pool = "ShroudES" } :: (p.Recoups |> List.filter (fun r -> r.Pool <> "ShroudES")) }
                 Counts = bump "regen:ES" w.Counts }
    | _ -> w

/// `IgniteSelf`: the player is ignited, for a share of its life and energy shield a second.
let igniteSelf (c: Reacting) percent (w: World) =
    let player = c.Env.Fight.Player
    match player.Vitals, w.Player with
    | Some pools, Some p when p.Died.IsNone && c.Actor.Name = player.Name ->
        // (resistance and what there is against Ignite: with the buffs of the moment)
        let v =
            match player.VitalsAt with
            | Some at -> at (buffStacks (Map.find player.Name w.Actors))
            | None -> pools
        if c.Rng.Chance(Map.tryFind "Ignite" v.Avoid |> Option.defaultValue 0.0) then w
        else
            // a charm against Ignite answers (and is used up doing so)
            let w, prevented = charm c.Env c.Rng "Ignited" w
            if prevented then { w with Counts = bump "charm prevented:Ignited" w.Counts }
            else
                let dps =
                    percent / 100.0 * (pools.Life + pools.EnergyShield) * (Map.tryFind "Ignite" v.SelfEffect |> Option.defaultValue 1.0)
                    * (1.0 - (Map.tryFind Fire v.Resist |> Option.defaultValue 0.0) / 100.0) * (Map.tryFind Fire v.TakenMult |> Option.defaultValue 1.0)
                let until = w.Time + Constants.igniteDuration * (Map.tryFind "Ignite" v.SelfDuration |> Option.defaultValue 1.0)
                { w with
                    Player =
                        w.Player |> Option.map (fun p ->
                            // the strongest Ignite burns; one as strong starts its time again
                            let stronger = p.Dots |> List.exists (fun d -> d.Damage = Fire && d.Until > w.Time && d.PerSecond > dps)
                            if stronger then p else { p with Dots = { Until = until; PerSecond = dps; Damage = Fire } :: (p.Dots |> List.filter (fun d -> d.Damage <> Fire)) })
                    Counts = bump "player:Ignited" w.Counts }
    | _ -> w

// ---- the ground ----

/// `LayGround`: a ground surface or storm appears where the enemy concerned is.
let layGround (c: Reacting) (g: Ground) (w: World) =
    // where the enemy concerned is (if it has just died: where it was)
    let spot =
        match c.Target |> Option.bind (fun t -> Map.tryFind t w.Enemies) with
        | Some e -> Some e.Def.Position
        | None -> aimed c.Env w |> Option.map (fun e -> e.Def.Position)
    match spot with
    | None -> w
    | Some spot ->
        let live = w.Grounds |> List.filter (fun x -> x.Until > w.Time)
        let from = if g.Speed > 0.0 then Vec.zero else spot
        // (a patch poured where one of the skill lies already is that patch, for its time anew)
        let there = if g.Kind = Storm then None else live |> List.tryFindIndex (fun x -> x.Spec.Skill = g.Skill && x.Owner = c.Actor.Name && x.Kind <> Storm && x.To = spot && x.From = from)
        (match c.Env.Trace with Some tell -> tell w.Time "Ground" g.Skill "laid" g.Radius | None -> ())
        match there with
        | Some i ->
            { w with Grounds = live |> List.mapi (fun k x -> if k = i then { x with Until = w.Time + g.Seconds } else x); Counts = bump ("ground:" + g.Skill) w.Counts }
            |> schedule (w.Time + g.Seconds) Expire
        | None ->
            // "Limit N": one more takes the oldest of the skill away
            let live =
                let mine = live |> List.filter (fun x -> x.Spec.Shared = g.Shared && x.Owner = c.Actor.Name)
                if g.Limit <= 0 || mine.Length < g.Limit then live
                else
                    let gone = mine.Length - g.Limit + 1
                    live
                    |> List.fold (fun (kept, seen) x ->
                        if x.Spec.Shared = g.Shared && x.Owner = c.Actor.Name then (if seen < gone then kept, seen + 1 else x :: kept, seen + 1)
                        else x :: kept, seen) ([], 0)
                    |> fst |> List.rev
            let laid =
                { Spec = g
                  Owner = c.Actor.Name
                  ExposureEffect = Mods.inc Ctx.empty [ Named "ExposureEffect" ] (ModDb.ofList c.Actor.Mods)
                  From = from
                  To = spot
                  Born = w.Time
                  Until = w.Time + g.Seconds
                  Kind = g.Kind
                  Carried = (if g.Kind = Burning then g.Ignite else 0.0)
                  Ignite = (if g.Kind = Burning then g.Ignite else 0.0) }
            { w with Grounds = live @ [ laid ]; Counts = bump ("ground:" + g.Skill) w.Counts }
            |> schedule laid.Until Expire

// ---- corpses and summoning ----

/// `Explode`: the enemy concerned explodes (the explosion is an event of its own).
let explode (c: Reacting) percent dt radius (w: World) =
    match c.Target with
    | Some t -> schedule w.Time (Explosion(c.Actor.Name, t, percent, dt, radius)) w
    | None -> w

/// `ExplodeCorpses`: every corpse there is explodes, and is used up.
let explodeCorpses (c: Reacting) percent dt radius (w: World) =
    // (the corpses there are: of the enemies killed last, the earlier ones first)
    let n = w.Kills.Length - w.CorpsesUsed
    if n <= 0 then w
    else
        w.Kills |> List.truncate n |> List.rev
        |> List.fold (fun w (name, _, _) -> schedule w.Time (Explosion(c.Actor.Name, name, percent, dt, radius)) w) { w with CorpsesUsed = w.Kills.Length }

/// `SummonPerCorpse`: a minion of the pool for every corpse (and, where the skill says so,
/// for every killed minion whose bones are still there).
let summonPerCorpse (c: Reacting) pool duration deadMinions (w: World) =
    let size = c.Env.Fight.Minions |> List.filter (fun m -> m.Pool = Some pool) |> List.length
    let corpses = min size (w.Kills.Length - w.CorpsesUsed)
    let bones =
        if not deadMinions then []
        else
            c.Env.Fight.Minions
            |> List.filter (fun m ->
                (m.Body |> Option.exists (fun b -> b.Revive.IsSome)) && (Map.find m.Name w.Actors).Gone && not (w.Boned.Contains m.Name))
            |> List.map (fun m -> m.Name)
            |> List.truncate (size - corpses)
    if corpses + bones.Length = 0 then w
    else
        summon c.Env pool (corpses + bones.Length) duration
            { w with CorpsesUsed = w.CorpsesUsed + corpses; Boned = bones |> List.fold (fun s n -> Set.add n s) w.Boned }

/// `SummonPerWard`: all the player's ward becomes minions, one for so much of it.
let summonPerWard (c: Reacting) pool wardEach duration (w: World) =
    match w.Player with
    | Some p when wardEach > 0.0 && p.Ward >= wardEach ->
        summon c.Env pool (int (p.Ward / wardEach)) duration { w with Player = Some { p with Ward = 0.0 } }
    | _ -> w

// ---- what is put on the enemy concerned ----

/// `AttachPustule`: a pustule on the enemy, if it has none (it bursts later, see `Events`).
let attachPustule (c: Reacting) part seconds per cap (w: World) =
    onTarget c w (fun target ->
        if w.Pustules |> List.exists (fun p -> p.Enemy = target) then w
        else
            let dealt = Map.tryFind "Poison" w.DamageBy |> Option.defaultValue 0.0
            { w with
                Pustules = { Pustule.Owner = c.Actor.Name; Part = part; Enemy = target; Mark = dealt; Per = per; Cap = cap; Until = w.Time + seconds } :: w.Pustules
                Counts = bump "pustule" w.Counts }
            |> schedule (w.Time + seconds) Expire)

/// `ApplyCurse`: a curse or a mark on the enemy; over the limit, the oldest gives way.
let applyCurse (c: Reacting) (name: string) duration mods (w: World) =
    onTarget c w (fun t ->
        let k = effectOnSelf w.Enemies.[t] "CurseEffectOnSelf"
        // "Curse:X" / "Mark:X" keep their prefix; plain names are curses
        let full = if name.StartsWith "Curse:" || name.StartsWith "Mark:" then name else "Curse:" + name
        let prefix = full.Substring(0, full.IndexOf ':' + 1)
        // one curse (plus "additional curses"), one mark: the oldest gives way
        let limit = if prefix = "Mark:" then 1 else 1 + int (Mods.sumBase Ctx.empty [ Named "MaxCurses" ] (ModDb.ofList c.Actor.Mods))
        let others =
            w.Enemies.[t].Debuffs |> Map.toList
            |> List.filter (fun (n, x) -> n.StartsWith prefix && n <> full && x.Expires > w.Time)
            |> List.sortBy (fun (_, x) -> x.Expires)
        let drop = others |> List.truncate (max 0 (others.Length + 1 - limit)) |> List.map fst
        let w = updateEnemy t (fun e -> { e with Debuffs = drop |> List.fold (fun d n -> Map.remove n d) e.Debuffs }) w
        addDebuff t full duration 1 (mods |> List.map (Mod.scaled k)) w
        |> fun w -> { w with Counts = bump ("apply:" + full) w.Counts })

/// `GrowExposure`: exposure on the enemy that grows pulse by pulse (each an event).
let growExposure (c: Reacting) dt start perPulse cap every pulses duration (w: World) =
    onTarget c w (fun t ->
        [ 1 .. pulses ] |> List.fold (fun w k ->
            schedule (w.Time + every * float k) (Expose(c.Actor.Name, t, dt, min cap (start + perPulse * float k), duration)) w) w)

/// `SpreadPoison`: the killed enemy's worst Poison goes to those near where it died.
let spreadPoison (c: Reacting) radius hinder (w: World) =
    match c.Target with
    | Some t ->
        let dead = w.Enemies.[t]
        // "Spreads the most Damaging Poison" (of equals, the one with the most time left)
        let poisons =
            dead.Dots |> List.filter (fun d -> d.Ailment = Some Poison && d.Expires > w.Time)
            |> List.sortByDescending (fun d -> d.Dps, d.Expires) |> List.truncate 1
        if poisons.IsEmpty then w
        else
            let longest = poisons |> List.map (fun d -> d.Expires - w.Time) |> List.max
            alive w
            |> List.filter (fun o -> o.Def.Name <> t && Vec.dist o.Def.Position dead.Def.Position <= radius + o.Def.Size)
            |> List.fold (fun w o ->
                let w = poisons |> List.fold (fun w d -> inflict o.Def.Name c.Actor.Name (Dot(Poison, d.Type, d.Dps, d.Expires - w.Time, d.MaxStacks)) w) w
                let w = { w with Counts = bump "spread:Poison" w.Counts }
                if c.Rng.Chance hinder then addDebuff o.Def.Name "Hindered" longest 1 [] w else w) w
    | None -> w

// ---- what a use that has begun sets going (`startUse` below goes through these in order) ----

/// The use's hits, each as an event at its time: every part of the skill that happens with this use strikes.
let scheduleStrikes (u: StartedUse) (w: World) : World =
    let actor, skill, target, st, times, first, step = u.Actor, u.Skill, u.Target, u.State, u.Times, u.First, u.Step
    times
    |> List.fold
        (fun w (at, mult) ->
            // every part of the skill strikes together
            // a "Frenzy-Charged" part happens only when the use has a charge to consume,
            // and then in place of the plain explosion; other parts need a charge, an
            // infusion, or wait for another skill to set them off
            let stacks name = Map.tryFind name st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
            let charged = match skill.Consumes with Some charge -> stacks charge > 0 | None -> false
            let isCharged (p: Skill) = p.Name.EndsWith "-Charged"
            let hasCharged = skill.Parts |> List.exists isCharged
            // a part that spends Rage happens when there is enough, in place of the plain one
            let rageFor (p: Skill) = p.Mods |> List.sumBy (fun m -> if m.Stat = Named "SpendsRage" then m.Value else 0.0)
            let enraged = skill.Parts |> List.exists (fun p -> rageFor p > 0.0 && float (stacks "Rage") >= rageFor p)
            (skill :: skill.Parts)
            |> List.filter (fun p -> not (partFlag p "DetonatedByDamage"))
            // a part that happens once however long the skill keeps hitting (an eruption)
            |> List.filter (fun p -> at <= first || not (partFlag p "OncePerUse"))
            // a part that only happens when something sets it off (the Bell's shockwave)
            |> List.filter (fun p -> p.Name = skill.Name || not (partFlag p "OnlyTriggered"))
            |> List.filter (fun p ->
                if rageFor p > 0.0 then enraged
                elif partFlag p "NotEnraged" then not enraged
                elif partFlag p "NotWithCharge" then not charged
                elif isCharged p || partFlag p "OnlyWithCharge" then charged
                elif (partNamed p "DetonatedBy:").IsSome then false
                else
                    match partNamed p "OnlyInfused:" with
                    | Some infusion -> stacks infusion > 0
                    | None -> not (hasCharged && charged && p.Name.EndsWith ": Explosion"))
            |> List.filter (inStep step)
            |> List.fold (fun w (part: Skill) ->
                // a strike of the chain that hits several times, spread over the use
                let n = part.Mods |> List.tryPick (fun m -> if m.Stat = Named "SeqHits" then Some(int m.Value) else None) |> Option.defaultValue 1
                [ 1 .. n ] |> List.fold (fun w i ->
                    let passing = match part.Delivery with Projectiles p -> p.Passing > 0.0 && p.HitInterval <= 0.0 | _ -> false
                    if passing then schedule at (PassingStrike(actor.Name, part.Name, mult, target.Def.Name, at - w.Time)) w
                    else schedule ((if n = 1 then at else w.Time + (at - w.Time) * float i / float n) + (if part.Name = skill.Name then 0.0 else partDelay part)) (Strike(actor.Name, part.Name, mult, target.Def.Name)) w) w) w)
        w

/// Ice Crystals the use leaves behind (Grim Pillars, a wall of ice).
let leaveCrystals (u: StartedUse) (w: World) : World =
    // Ice Crystals the use leaves behind (Grim Pillars): each stands at a random spot of the
    // skill's area (around the user, or around the target), and those whose explosion
    // reaches the target count. They appear after the use's own hits, which do not
    // damage them.
    let rng, actor, skill, target, ctx, mods, times = u.Rng, u.Actor, u.Skill, u.Target, u.Ctx, u.Mods, u.Times
    (skill :: skill.Parts) |> List.filter (fun p -> partFlag p "Crystals") |> List.fold (fun w p ->
        let sum (q: Skill) name = q.Mods |> List.sumBy (fun m -> if m.Stat = Named name then m.Value else 0.0)
        let areaMult = Mods.mult ctx [ AreaOfEffect ] mods
        let radius (q: Skill) = match q.Delivery with AreaAroundSelf r | AreaAtTarget r -> Geometry.radius r areaMult | _ -> 0.0
        let centre = match skill.Delivery with AreaAroundSelf _ -> Vec.zero | _ -> target.Def.Position
        let spread, reach = radius skill, radius p
        let stands () =
            if spread <= 0.0 || reach <= 0.0 then true
            else
                let r, a = spread * sqrt (rng.Float()), 2.0 * System.Math.PI * rng.Float()
                Vec.dist (Vec.add centre (Vec.scale r (Vec.ofAngle a))) target.Def.Position <= reach + target.Def.Size
        // one crystal where the skill does not say how many
        let life = sum p "CrystalLife"
        let last = times |> List.map fst |> List.fold max w.Time
        // a wall: a row of crystals through the target, across the line to it. Those
        // that come up inside the monster are broken by it at once (80% less); the
        // others stand, and count if their explosion reaches it
        // (only a wall cast onto the target itself: where a bolt's two walls stand is not known)
        let wall = if p.Name = skill.Name then sum p "WallLength" else 0.0
        let row =
            if wall <= 0.0 then []
            else
                let n = max 1 (int (wall / Constants.iceCrystalWidth))
                [ for i in 0 .. n - 1 -> abs ((float i - float (n - 1) / 2.0) * Constants.iceCrystalWidth) ]
        let broken = row |> List.filter (fun d -> d <= target.Def.Size) |> List.length
        let w =
            if life > 0.0 && broken > 0 then
                [ 1 .. broken ] |> List.fold (fun w _ -> schedule (last + 1e-6) (Strike(actor.Name, p.Name, Constants.freshCrystalDamage, target.Def.Name)) w) w
            else w
        let created = if wall > 0.0 then max 1 row.Length else max 1 (int (sum p "CrystalsCreated"))
        let near =
            if wall > 0.0 then row |> List.filter (fun d -> d > target.Def.Size && d <= reach + target.Def.Size) |> List.length
            else [ 1 .. created ] |> List.filter (fun _ -> stands ()) |> List.length
        if life > 0.0 && near > 0 then
            let lasts = if p.Duration > 0.0 then p.Duration * Mods.mult ctx [ SkillDuration ] mods else 1e9
            // the limit counts every crystal; of those, the share that stands near
            let limit = max near (int (System.Math.Round(max (float created) (sum p "CrystalLimit") * float near / float created)))
            schedule (last + 1e-6) (PlantCrystals(actor.Name, p.Name, target.Def.Name, near, life, limit, lasts)) w
        else w) w

/// A seed planted in the enemy aimed at (Living Bomb).
let plantBomb (u: StartedUse) (w: World) : World =
    // a seed planted in the target (Living Bomb): it explodes once the enemy has taken
    // enough damage from then on; a new seed replaces the old one
    let actor, skill, target, ctx, mods, useTime = u.Actor, u.Skill, u.Target, u.Ctx, u.Mods, u.UseTime
    match skill.Mods |> List.tryPick (fun m -> if m.Stat = Named "DetonatedByDamage" then Some m.Value else None) with
    | Some pct ->
        let needed = pct / 100.0 * max 1.0 (Ailments.threshold (defender target))
        let lasts = if skill.Duration > 0.0 then skill.Duration * Mods.mult ctx [ SkillDuration ] mods else 1e9
        { w with
            Bombs = { Bomb.Owner = actor.Name; Skill = skill.Name; Enemy = target.Def.Name; Mark = target.Life; Needed = needed; Until = w.Time + useTime + lasts } :: (w.Bombs |> List.filter (fun b -> not (b.Owner = actor.Name && b.Skill = skill.Name)))
            Counts = bump ("planted:" + skill.Name) w.Counts }
    | None -> w

/// The Rage a part of the skill spent.
let spendPartRage (u: StartedUse) (w: World) : World =
    // the Rage a part spent
    let actor, skill, st = u.Actor, u.Skill, u.State
    let need = skill.Parts |> List.sumBy (fun p -> p.Mods |> List.sumBy (fun m -> if m.Stat = Named "SpendsRage" then m.Value else 0.0))
    match Map.tryFind "Rage" st.Buffs with
    | Some r when need > 0.0 && float r.Stacks >= need ->
        { w with Counts = bump ("enraged:" + skill.Name) w.Counts }
        |> updateActor actor.Name (fun st ->
            let left = r.Stacks - int need
            { st with Buffs = if left > 0 then Map.add "Rage" { r with Stacks = left } st.Buffs else Map.remove "Rage" st.Buffs })
    | _ -> w

/// A debuff that records damage (Inevitable Agony), on the enemy aimed at.
let startRecording (u: StartedUse) (w: World) : World =
    // a debuff that records damage (Inevitable Agony), on the enemy aimed at
    let actor, skill, target, ctx, mods, useTime = u.Actor, u.Skill, u.Target, u.Ctx, u.Mods, u.UseTime
    match skill.Mods |> List.tryPick (fun m -> if m.Stat = Named "RecordsDamage" then Some m.Value else None) with
    | Some pct when skill.Duration > 0.0 ->
        let lasts = skill.Duration * Mods.mult ctx [ SkillDuration ] mods
        { w with
            Agonies = { Agony.Owner = actor.Name; Skill = skill.Name; Enemy = target.Def.Name; Recorded = 0.0; Share = pct / 100.0; Until = w.Time + useTime + lasts } :: (w.Agonies |> List.filter (fun a -> not (a.Owner = actor.Name && a.Skill = skill.Name)))
            Counts = bump ("recording:" + skill.Name) w.Counts }
    | _ -> w

/// An infusion a part needed is used up; a part that waits to be set off is put down.
let spendInfusionOrWait (u: StartedUse) (w: World) : World =
    // an infusion a part needed is used up; a part that waits is put down
    let actor, skill, st = u.Actor, u.Skill, u.State
    skill.Parts |> List.fold (fun w p ->
        match partNamed p "OnlyInfused:", partNamed p "DetonatedBy:" with
        | Some infusion, _ when (Map.tryFind infusion st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0) > 0 ->
            updateActor actor.Name (fun st ->
                match Map.tryFind infusion st.Buffs with
                | Some e when e.Stacks > 1 -> { st with Buffs = Map.add infusion { e with Stacks = e.Stacks - 1 } st.Buffs }
                | _ -> { st with Buffs = Map.remove infusion st.Buffs }) w
        | _, Some _ -> updateActor actor.Name (fun st -> { st with Pending = Map.add p.Name (min (waitLimit p) ((Map.tryFind p.Name st.Pending |> Option.defaultValue 0) + 1)) st.Pending }) w
        | _ -> w) w

/// A detonator (or a warcry) sets off everything of its kind that waits.
let detonateWaiting (u: StartedUse) (w: World) : World =
    // a detonator (or a warcry) sets off everything of that kind that waits
    let actor, skill, target, st, times = u.Actor, u.Skill, u.Target, u.State, u.Times
    let st = Map.find actor.Name w.Actors
    actor.Skills |> List.collect (fun s -> s.Parts) |> List.fold (fun w p ->
        match partNamed p "DetonatedBy:", Map.tryFind p.Name st.Pending with
        | Some by, Some n when n > 0 && not (skill.Parts |> List.exists (fun q -> q.Name = p.Name))
                               && ((by = "Warcry" && skill.Tags.Contains Warcry) || skill.Tags.Contains(Tag by)) ->
            let last = times |> List.map fst |> List.fold max w.Time
            let w = updateActor actor.Name (fun st -> { st with Pending = Map.remove p.Name st.Pending }) w
            [ 1 .. n ] |> List.fold (fun w _ -> schedule (last + 1e-6) (Strike(actor.Name, p.Name, 1.0, target.Def.Name)) w) w
            |> fun w -> { w with Counts = bump ("detonated:" + p.Name) w.Counts }
        | _ -> w) w

/// An attack made while empowered by a warcry uses one empowerment up.
let spendEmpowerments (u: StartedUse) (w: World) : World =
    // an attack the player makes while empowered by a warcry uses one empowerment up
    let actor, skill, st, useTime, times = u.Actor, u.Skill, u.State, u.UseTime, u.Times
    if useTime > 0.0 && skill.Tags.Contains Attack && not (skill.Tags.Contains Warcry) then
        let last = times |> List.map fst |> List.fold max w.Time
        // (and by Mantra of Destruction, Temper Weapon: "EmpoweredAttacks:<skill>")
        st.Buffs |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "EmpoweredAttacks")
        |> List.fold (fun w (k, _) -> schedule (last + 1e-9) (SpendBuff(actor.Name, k)) w) w
    else w

/// A Repeatable attack uses up one of those readied for it (Ice-Tipped Arrows).
let spendRepeatable (u: StartedUse) (w: World) : World =
    // and a Repeatable attack one of those readied for it (Ice-Tipped Arrows)
    let actor, skill, st, useTime, times = u.Actor, u.Skill, u.State, u.UseTime, u.Times
    if useTime > 0.0 && skill.Tags.Contains Attack && skill.Tags.Contains(Tag "Type:Barrageable") then
        let last = times |> List.map fst |> List.fold max w.Time
        st.Buffs |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "EmpoweredRepeatable:")
        |> List.fold (fun w (k, _) -> schedule (last + 1e-9) (SpendBuff(actor.Name, k)) w) w
    else w

/// "Cannot gain Power Charges while using this Skill": until its last strike.
let lockPowerCharges (u: StartedUse) (w: World) : World =
    // "Cannot gain Power Charges while using this Skill": until its last strike
    let actor, skill, st, times = u.Actor, u.Skill, u.State, u.Times
    if partFlag skill "NoPowerChargesDuringSkill" then
        let last = times |> List.map fst |> List.fold max w.Time
        updateActor actor.Name (fun st -> { st with Energy = Map.add "PowerChargeLock" last st.Energy }) w
    else w

/// The charges the skill consumes go after its last strike (an event of its own).
let scheduleConsume (u: StartedUse) (w: World) : World =
    let actor, skill, times = u.Actor, u.Skill, u.Times
    match skill.Consumes with
    | Some charge ->
        let last = times |> List.map fst |> List.fold max w.Time
        // "Consume it when you leap": at the start, where that is what empowers the use
        let at = if partFlag skill "NoCooldownWithCharge" then w.Time else last
        schedule (at + 1e-9) (ConsumeCharges(actor.Name, charge, skill.Name)) w
    | None -> w

/// Cast on Dodge: energy for the distance a dodge roll covers.
let dodgeEnergy (u: StartedUse) (w: World) : World =
    // Cast on Dodge: energy per unit travelled by a dodge roll
    let actor, skill, ctx, mods, useTime = u.Actor, u.Skill, u.Ctx, u.Mods, u.UseTime
    if useTime > 0.0 && skill.Tags.Contains(Tag "DodgeRoll") then
        let distance = Constants.dodgeRollDistance + Mods.sumBase ctx [ Named "DodgeRollDistance" ] mods
        actor.Metas
        |> List.fold
            (fun w meta ->
                let amount = meta.Sources |> List.sumBy (function DodgeRolls per -> per * distance | _ -> 0.0)
                // the distance is covered when the roll ends
                if amount > 0.0 then schedule (w.Time + useTime) (EnergyArrives(actor.Name, meta.Name, amount * meta.EnergyMult)) w else w)
            w
    else w

// ---- the rules ----

/// What the actor's rules for `hook` do, for a use or hit of `skill`.
/// `target`: the enemy the triggering event concerns (hit enemy, or the one aimed at).
let rec react env (rng: Rng) (actor: Actor) hook (skill: Skill) (target: string option) (w: World) =
    actor.Rules
    |> List.filter (fun r -> r.Hook = hook && matches r.Filter skill)
    |> List.fold
        (fun w r ->
            if not (rng.Chance r.Chance) then
                w
            else
                perform { Reacting.Env = env; Rng = rng; Actor = actor; Skill = skill; Target = target } r.Reaction w)
        w

/// One reaction happens: which function above (or below, for those that use a skill) it is.
and perform (c: Reacting) reaction (w: World) : World =
    match reaction with
    | NoReaction -> w
    // conditions
    | SpendStack (buff, next) -> (match spendStack c buff w with Some w -> perform c next w | None -> w)
    | IfBuff (buff, next) -> if hasBuff c buff w then perform c next w else w
    | Every (key, seconds, next) -> (match everyDue c key seconds w with Some w -> perform c next w | None -> w)
    | IfEnemyHas (st, next) -> if targetHas c st w then perform c next w else w
    | WithCorpse (next, otherwise) ->
        if w.Kills.Length > w.CorpsesUsed then perform c next { w with CorpsesUsed = w.CorpsesUsed + 1 } else perform c otherwise w
    // buffs and stacks of the actor
    | SetStacks (buff, n) -> setStacks c buff n w
    | Empower (buff, perPower, cap) -> empower c buff perPower cap w
    | GrantOneOf (names, duration) -> grantOneOf c names duration w
    | GrantBuff (name, duration, maxStacks, mods) -> grantBuff c name duration maxStacks mods w
    | RemoveBuff name -> removeBuff c name w
    | GrantMinionBuff (name, duration, mods) -> grantMinionBuff c name duration mods w
    | ChargeBuff (name, charge, perStack, seconds, doubleChance) -> chargeBuff c name charge perStack seconds doubleChance w
    | GainRareMods duration -> gainRareMods c duration w
    | GainResource (r, n) -> schedule w.Time (GainStacks(c.Actor.Name, r, n)) w
    | DropRemnant (r, delay, extra) -> dropRemnant c r delay extra w
    // the player's own pools
    | RegenES (rate, duration) -> regenES rate duration w
    | IgniteSelf percent -> igniteSelf c percent w
    // the ground
    | LayGround g -> layGround c g w
    // corpses and summoning
    | Explode (percent, dt, radius) -> explode c percent dt radius w
    | ExplodeCorpses (percent, dt, radius) -> explodeCorpses c percent dt radius w
    | Summon (pool, count, duration) -> summon c.Env pool count duration w
    | SummonEmpowered (pool, count, duration, first) -> summonWith c.Env pool count duration first w
    | SummonPerCorpse (pool, duration, deadMinions) -> summonPerCorpse c pool duration deadMinions w
    | SummonPerWard (pool, wardEach, duration) -> summonPerWard c pool wardEach duration w
    // what is put on the enemy concerned
    | ApplyDebuff (name, _, _, _) when partFlag c.Skill ("CannotInflict:" + name) -> w
    | ApplyDebuff (name, duration, maxStacks, mods) -> onTarget c w (fun t -> addDebuff t name duration maxStacks mods w)
    | RemoveDebuff name -> onTarget c w (fun t -> updateEnemy t (fun en -> { en with Debuffs = Map.remove name en.Debuffs }) w)
    | ApplyCurse (name, duration, mods) -> applyCurse c name duration mods w
    | ApplyExposure (dt, amount, duration) -> onTarget c w (fun t -> expose c.Actor t dt amount duration w)
    | GrowExposure (dt, start, perPulse, cap, every, pulses, duration) -> growExposure c dt start perPulse cap every pulses duration w
    | InflictStatus (a, magnitude, duration) -> onTarget c w (fun t -> inflict t c.Actor.Name (Status(a, magnitude, duration)) w)
    | AttachPustule (part, seconds, per, cap) -> attachPustule c part seconds per cap w
    | SpreadPoison (radius, hinder) -> spreadPoison c radius hinder w
    // skills and energy
    | TriggerSkill name -> trigger c.Env c.Rng c.Actor name w
    | GainEnergy (meta, amount) -> gainEnergy c.Env c.Rng c.Actor meta amount w

/// A meta gem gains energy (more of it with what the actor has for that), and releases it
/// if that is enough.
and gainEnergy env rng (actor: Actor) metaName amount w =
    match actor.Metas |> List.tryFind (fun m -> m.Name = metaName) with
    | None -> w
    | Some meta ->
        let st = Map.find actor.Name w.Actors
        let amount =
            if not (ModDb.hasStat (fun s -> s = Named "EnergyGenerated") meta.EnergyMods) then amount
            else
                let more = Mods.sumInc ({ Ctx.empty with Buffs = buffStacks st } |> owned env w) [ Named "EnergyGenerated" ] meta.EnergyMods / 100.0
                amount * (meta.EnergyMult + more) / meta.EnergyMult
        let energy = min meta.MaxEnergy ((Map.tryFind meta.Name st.Energy |> Option.defaultValue 0.0) + amount)
        let w = updateActor actor.Name (fun st -> { st with Energy = Map.add meta.Name energy st.Energy }) w
        releaseMeta env rng actor meta w

/// Trigger the meta's spells if its energy allows: "Cast on" at maximum, Invocations
/// whenever enough is banked and the invocation is off cooldown (as many times as it covers).
and releaseMeta env rng (actor: Actor) (meta: Meta) w =
    let st = Map.find actor.Name w.Actors
    let energy = Map.tryFind meta.Name st.Energy |> Option.defaultValue 0.0
    let triggers =
        match meta.Release with
        | AtMaximum -> if energy >= meta.MaxEnergy && meta.Cost > 0.0 then 1 else 0
        | Invocation _ ->
            let cdKey = "invocation:" + meta.Name
            let readyNow = Map.tryFind cdKey st.Cooldowns |> Option.forall (fun t -> t <= w.Time)
            if readyNow && meta.Cost > 0.0 then int (energy / meta.Cost) else 0
    if triggers = 0 then w
    else
        let spent = float triggers * meta.Cost
        let refund = if rng.Chance meta.RefundChance then spent / 2.0 else 0.0
        let w =
            updateActor actor.Name (fun st ->
                let st = { st with Energy = Map.add meta.Name (energy - spent + refund) st.Energy }
                match meta.Release with
                | Invocation cd -> { st with Cooldowns = Map.add ("invocation:" + meta.Name) (w.Time + cd) st.Cooldowns }
                | AtMaximum -> st) w
        let w = { w with Counts = bump ("meta:" + meta.Name) w.Counts }
        [ 1..triggers ] |> List.fold (fun w _ -> meta.Triggers |> List.fold (fun w name -> trigger env rng actor name w) w) w

/// Start a use of a skill at the current time, aimed at the current target.
and startUse env (rng: Rng) (actor: Actor) (skill: Skill) (useTime: float) (w: World) =
    match aimed env w with
    | None -> w
    | Some target ->
        // the use's critical hit roll, for the skill and every part of it
        // (Drawn apart from the fight's stream, by who uses what for the how-many-th time: the
        // tenth use of a skill crits or not whatever else happened before it. Two fights of
        // the same seed that differ a little, a passive more, then differ by what the
        // passive does and not by which uses happen to crit.)
        let w =
            let n = Map.tryFind skill.Name (Map.find actor.Name w.Actors).Rolled |> Option.defaultValue 0
            let r = Rng(streamSeed w.Seed (actor.Name + "|" + skill.Name) n).Float()
            updateActor actor.Name (fun st -> { st with Rolled = Map.add skill.Name (n + 1) st.Rolled; CritRolls = (skill :: skill.Parts) |> List.fold (fun m p -> Map.add p.Name r m) st.CritRolls }) w
        let st = Map.find actor.Name w.Actors
        let ctx = Skill.ctx skill false (skillStacks actor skill st) (status target) |> owned env w
        let mods = skillDb env actor skill st
        // Rageforged: "Supported Skills spend X Rage if possible to deal Y% more Damage": the
        // use spends the Rage before its hits, when there is that much, and its hits deal more;
        // "Z% chance to gain the benefits of this Support without spending additional Rage"
        let rageforged, w =
            let spend = Mods.sumBase ctx [ Named "RageforgedSpend" ] mods
            match Map.tryFind "Rage" st.Buffs with
            | Some r when spend > 0.0 && float r.Stacks >= spend ->
                let more = Mods.sumBase ctx [ Named "RageforgedMore" ] mods
                let skip = Mods.sumBase ctx [ Named "RageSkipChance" ] mods
                let kept = skip > 0.0 && rng.Chance skip
                let w = { w with Counts = bump ("rageforged:" + skill.Name) w.Counts }
                let w =
                    if kept then w
                    else
                        updateActor actor.Name (fun st ->
                            let left = r.Stacks - int spend
                            { st with Buffs = if left > 0 then Map.add "Rage" { r with Stacks = left } st.Buffs else Map.remove "Rage" st.Buffs }) w
                1.0 + more / 100.0, w
            | _ -> 1.0, w
        let st = Map.find actor.Name w.Actors
        // "does not use up its Cooldown when empowered" (by what it consumes)
        let free =
            partFlag skill "NoCooldownWithCharge"
            && (match skill.Consumes with Some charge -> (Map.tryFind charge st.Buffs |> Option.exists (fun e -> e.Stacks > 0 && e.Expires > w.Time)) | None -> false)
        let w =
            match skill.Cooldown with
            | Some _ when free -> w
            | Some cd ->
                let recovery = Mods.mult ctx [ CooldownRecovery ] mods
                let uses = (skill.StoredUses + int (Mods.sumBase ctx [ Named "ExtraCooldownUses" ] mods)) * (if Mods.sumBase ctx [ Named "DoubleCooldownUses" ] mods > 0.0 then 2 else 1)
                updateActor actor.Name (fun st ->
                    let pending = Map.tryFind skill.Name st.Recharges |> Option.defaultValue [] |> List.filter (fun t -> t > w.Time)
                    let start = if actor.ParallelUses || pending.IsEmpty then w.Time else List.max pending
                    let pending = List.sort (pending @ [ start + cd / recovery ])
                    { st with
                        Recharges = Map.add skill.Name pending st.Recharges
                        // blocked once every stored use is recharging; free again when the first is back
                        Cooldowns = if pending.Length >= uses then Map.add skill.Name pending.Head st.Cooldowns else Map.remove skill.Name st.Cooldowns }) w
            | None -> w
        // "Lose X% of maximum Life, Mana and Energy Shield when you use" (Atziri's Impatience); life stays >= 1.
        // "Lose X% of maximum Life and Energy Shield when you use a Chaos Skill": those two only
        let w =
            let lose = Mods.sumBase ctx [ Named "LosePoolsOnUse" ] mods / 100.0
            let loseLifeES = lose + Mods.sumBase ctx [ Named "LoseLifeAndESOnUse" ] mods / 100.0
            match actor.Vitals, w.Player with
            | Some v, Some p when loseLifeES > 0.0 && useTime > 0.0 ->
                { w with
                    Player = Some { p with Life = max (min 1.0 p.Life) (p.Life - loseLifeES * v.Life); EnergyShield = max 0.0 (p.EnergyShield - loseLifeES * v.EnergyShield); Mana = max (min 0.0 p.Mana) (p.Mana - lose * v.Mana) }
                    Counts = bump "pool loss on use" w.Counts }
            | _ -> w
        let pattern =
            Map.tryFind skill.Name actor.HitPattern
            |> Option.defaultValue (skill.Hits |> List.map (fun (f, m) -> DuringUse f, m))
        // Sealed skills: a seal every so often (a fixed time, or a share of the use time), up to
        // a maximum; the use breaks them all. A fight starts with the seals full.
        let seals =
            let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
            let maxSeals = own "MaxSeals"
            let interval = own "SealIntervalMs" / 1000.0 + own "SealIntervalPercentOfUse" / 100.0 * useTime
            if maxSeals <= 0.0 || interval <= 0.0 || useTime <= 0.0 then 0
            else
                match Map.tryFind ("SealBreak:" + skill.Name) st.Cooldowns with
                | Some last -> int (min maxSeals (floor ((w.Time - last) / interval)))
                | None -> int maxSeals
        let sealOwn name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
        let w =
            if seals <= 0 then w
            else
                let perSeal = sealOwn "SealProjectiles"
                updateActor actor.Name (fun st ->
                    { st with
                        Cooldowns = Map.add ("SealBreak:" + skill.Name) w.Time st.Cooldowns
                        // "additional Projectiles are fired": for this use
                        Buffs =
                            if perSeal > 0.0 then
                                Map.add "SealsBroken" { Name = "SealsBroken"; Mods = [ Mod.base' AdditionalProjectiles perSeal |> Mod.whenever (SkillNamed skill.Name) ]; Stacks = seals; Expires = w.Time + useTime + skill.HitDelay + 1e-6 } st.Buffs
                            else st.Buffs }) w
                |> fun w -> { w with Counts = bump ("seals:" + skill.Name) w.Counts }
        let w =
            // a use without seals still restarts the count
            if seals = 0 && sealOwn "MaxSeals" > 0.0 && not (Map.containsKey ("SealBreak:" + skill.Name) st.Cooldowns) then
                updateActor actor.Name (fun st -> { st with Cooldowns = Map.add ("SealBreak:" + skill.Name) w.Time st.Cooldowns }) w
            else w
        // "each causing the Spell to Repeat": the hits again per seal, with the repeats' own damage
        let pattern =
            let repeats = seals * int (sealOwn "SealRepeats")
            if repeats <= 0 then pattern
            else
                let k = 1.0 + sealOwn "SealRepeatDamage" / 100.0
                pattern @ [ for i in 1 .. repeats do for (_, mult) in pattern -> After(useTime + 0.1 * float i), mult * k ]
        // Barrage readies repeats for the next Repeatable attack: its base number and one per
        // Frenzy Charge, all of which it consumes
        let w =
            let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
            if own "BarrageRepeats" <= 0.0 then w
            else
                let charges = Map.tryFind "FrenzyCharge" st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                let n = int (own "BarrageRepeats" + own "BarrageRepeatsPerFrenzy" * float charges)
                let lasts = useTime + (if skill.Duration > 0.0 then skill.Duration * Mods.mult ctx [ SkillDuration ] mods else 5.0)
                let carried = [ Mod.base' (Named "BarrageRepeatLess") (own "BarrageRepeatLess"); Mod.base' (Named "BarrageCooldownPct") (own "BarrageCooldownPct") |> Mod.from skill.Name ]
                updateActor actor.Name (fun st ->
                    { st with Buffs = st.Buffs |> Map.remove "FrenzyCharge" |> Map.add "Barrage" { Name = "Barrage"; Mods = carried; Stacks = n; Expires = w.Time + lasts } }) w
                |> schedule (w.Time + lasts) Expire
                |> fun w -> { w with Counts = bump "barrage readied" w.Counts }
        // a Repeatable attack with Barrage readied fires again for every repeat, within the
        // same (longer) use; the repeats deal less. Barrage's cooldown grows by a share of
        // the attack's time
        let pattern, w =
            match barraged skill st with
            | Some b when w.Time < b.Expires && useTime > 0.0 ->
                let value name = b.Mods |> List.sumBy (fun m -> if m.Stat = Named name then m.Value else 0.0)
                let n = b.Stacks
                let k = 1.0 - value "BarrageRepeatLess" / 100.0
                let pattern =
                    [ for i in 0 .. n do
                        for (timing, mult) in pattern ->
                            (match timing with
                             | DuringUse f -> DuringUse((float i + f) / float (n + 1))
                             | After t -> After(t + useTime * float i / float (n + 1))),
                            (if i = 0 then mult else mult * k) ]
                let source = b.Mods |> List.tryPick (fun m -> if m.Stat = Named "BarrageCooldownPct" && m.Source <> "" then Some m.Source else None)
                let extra = value "BarrageCooldownPct" / 100.0 * useTime
                let w =
                    updateActor actor.Name (fun st ->
                        let st = { st with Buffs = Map.remove "Barrage" st.Buffs }
                        match source with
                        | Some name when extra > 0.0 ->
                            // "to a maximum of 30 seconds"
                            let ready = min (w.Time + 30.0) ((Map.tryFind name st.Cooldowns |> Option.defaultValue w.Time |> max w.Time) + extra)
                            { st with Cooldowns = Map.add name ready st.Cooldowns; Recharges = Map.add name [ ready ] st.Recharges }
                        | _ -> st) w
                pattern, { w with Counts = bump ("barraged:" + skill.Name) w.Counts }
            | _ -> pattern, w
        // Flicker Strike: the strike, then so many more per Power Charge, each in the time
        // its more attack speed leaves of the first one's (the use is as much longer)
        let pattern =
            let n = int (flickers skill st)
            if n <= 0 || useTime <= 0.0 then pattern
            else
                let quick = 1.0 / (1.0 + Constants.flickerMoreSpeed / 100.0)
                let whole = 1.0 + float n * quick
                [ for i in 0 .. n do
                    let before = if i = 0 then 0.0 else 1.0 + float (i - 1) * quick
                    let own = if i = 0 then 1.0 else quick
                    for (timing, mult) in pattern ->
                        (match timing with
                         | DuringUse f -> DuringUse((before + f * own) / whole)
                         | After t -> After(t + useTime * before / whole)),
                        mult ]
        let w = { w with Counts = bump ("uses:" + skill.Name) w.Counts }
        // the Glory or Combo the skill needed is spent
        let w =
            match requirements skill with
            | [] -> w
            | reqs -> updateActor actor.Name (fun st -> { st with Energy = reqs |> List.fold (fun e (key, _) -> Map.remove key e) st.Energy }) w
        // (a skill whose rule says what it does with corpses takes them there)
        let w =
            if skill.Tags.Contains(Tag "Type:TargetsDestructibleCorpses") && w.Kills.Length > w.CorpsesUsed && (corpseRule actor skill).IsNone
            then { w with CorpsesUsed = w.CorpsesUsed + 1 } else w
        // a warp into one's own projectile: the projectile is gone (what it had still to do
        // with it), and the warp's explosion happens whatever state the enemy is in
        let w =
            match warpsInto skill with
            | Some gem when ballInFlight actor.Name gem w ->
                { w with
                    Queue =
                        w.Queue |> Map.filter (fun _ ev ->
                            match ev with
                            | Strike (a, name, _, _) | PassingStrike (a, name, _, _, _) -> not (a = actor.Name && (name = gem || name.StartsWith(gem + " ")))
                            | _ -> true)
                    Counts = bump ("warped into:" + gem) w.Counts }
                |> updateActor actor.Name (fun st ->
                    { st with Buffs = st.Buffs |> Map.remove (gem + " buff") |> Map.add ("Warped:" + skill.Name) { Name = "Warped:" + skill.Name; Mods = []; Stacks = 1; Expires = infinity } })
            | _ -> w
        // chained attacks: this use's place in the chain, and the next
        let step = seqStep skill st
        let w =
            match step with
            | Some k -> updateActor actor.Name (fun st -> { st with Energy = Map.add ("Seq:" + skill.Name) (float (k + 1)) st.Energy }) w
            | None -> w
        let w = if useTime > 0.0 then updateActor actor.Name (fun st -> { st with LastUsed = skill.Name }) w else w
        let times =
            pattern |> List.map (fun (timing, mult) ->
                (match timing with
                 | DuringUse f -> w.Time + f * useTime + skill.HitDelay
                 | After s -> w.Time + s + skill.HitDelay), mult * rageforged)
        let first = times |> List.map fst |> List.fold min infinity
        let u =
            { StartedUse.Env = env; Rng = rng; Actor = actor; Skill = skill; Target = target; State = st; Ctx = ctx; Mods = mods
              UseTime = useTime; Times = times; First = first; Step = step }
        let w = scheduleStrikes u w
        // what happens when the use lands: when its first hit is due, after that hit
        let w =
            if first < infinity && actor.Rules |> List.exists (fun r -> r.Hook = OnLand && matches r.Filter skill) then
                schedule first (Landed(actor.Name, skill.Name, target.Def.Name)) w
            else w
        let w =
            w
            |> leaveCrystals u
            |> plantBomb u
            |> spendPartRage u
            |> startRecording u
            |> spendInfusionOrWait u
            |> detonateWaiting u
            |> spendEmpowerments u
            |> spendRepeatable u
            |> lockPowerCharges u
            |> scheduleConsume u
            |> dodgeEnergy u
        // Spellslinger-style energy: per 10 ms of base cast time of spells the player casts
        let w =
            if useTime > 0.0 && skill.Tags.Contains Spell then
                actor.Metas
                |> List.fold
                    (fun w meta ->
                        let amount = meta.Sources |> List.sumBy (function CastsSpells per -> per * skill.BaseTime * 100.0 | _ -> 0.0)
                        if amount > 0.0 then gainEnergy env rng actor meta.Name (amount * meta.EnergyMult) w else w)
                    w
            else w
        react env rng actor OnUse skill (Some target.Def.Name) w

and trigger env rng (actor: Actor) name (w: World) =
    let skill = skillOf actor name
    let st = Map.find actor.Name w.Actors
    // On a map the player's triggered skill does not go any distance: at the cause while that
    // is within reach; where a dead cause was, if someone is near enough to there; else at
    // whom the player would aim at, within reach (a skill around the player itself: whoever
    // that is). Nobody: the trigger is wasted.
    let aim =
        match env.Fight.March with
        | Some _ when actor.Name = env.Fight.Player.Name ->
            let reach (e: EnemyDef) = Vec.length e.Position - e.Size <= Constants.triggerReach
            let self = match skill.Delivery with AreaAroundSelf _ -> true | _ -> false
            let usual () =
                match Geometry.aim env.Fight.Targeting (alive w |> List.map (fun e -> e.Def)) with
                | Some d when self || reach d -> Some d.Name
                | _ -> None
            match (if w.Cause = "" then None else Map.tryFind w.Cause w.Enemies) with
            | Some e when e.Life > 0.0 -> if reach e.Def then Some e.Def.Name else usual ()
            | Some e ->
                match alive w with
                | [] -> None
                | living ->
                    let near = living |> List.minBy (fun x -> Vec.dist x.Def.Position e.Def.Position)
                    if Vec.dist near.Def.Position e.Def.Position <= Constants.triggerStray && reach near.Def then Some near.Def.Name
                    elif self then usual ()
                    else None
            | None -> usual ()
        | _ -> Some w.Cause
    if aim.IsSome && ready st w.Time skill then
        { w with Counts = bump ("trigger:" + name) w.Counts; Aim = aim.Value }
        |> updateActor actor.Name (fun st -> { st with Buffs = Map.add "TriggeredRecently" { Name = "TriggeredRecently"; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs })
        |> schedule (w.Time + 4.0) Expire
        |> startUse env rng actor skill 0.0
        |> fun w -> { w with Aim = "" }
    else
        w
