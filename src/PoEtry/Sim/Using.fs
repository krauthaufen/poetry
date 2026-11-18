/// An actor is ready: what it uses next, what that costs, when its hits will land.
[<AutoOpen>]
module internal PoEtry.Engine.Using

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

let useTime env (actor: Actor) (skill: Skill) (w: World) =
    let st = Map.find actor.Name w.Actors
    let target = aimed env w |> Option.map status |> Option.defaultValue Map.empty
    let ctx = Skill.ctx skill false (buffStacks st) target |> owned env w
    let db = skillDb env actor skill st
    // a chained attack takes the time of the strike that is due (its own speed)
    let own = stepSkill skill st
    let speed = if own.Name = skill.Name then Mods.mult ctx [ Speed ] db else Mods.mult (Skill.ctx own false (buffStacks st) target |> owned env w) [ Speed ] (skillDb env actor own st)
    // "Skills take 5% longer to perform for each time they Repeat"
    let repeats = match barraged skill st with Some b when w.Time < b.Expires -> float b.Stacks | _ -> 0.0
    // "+1 second to Total Cast Time": added after speed
    own.BaseTime / speed * (1.0 + Constants.longerPerRepeat * repeats + flickers skill st / (1.0 + Constants.flickerMoreSpeed / 100.0)) + Mods.sumBase ctx [ Named "FixedUseTime" ] db

/// Life spent on a skill can leave the life below half as a hit can: the life flask is
/// drunk then, as when a hit leaves it there (`takeDamage`).
let drinkLifeFlask (pools: PlayerStats) (w: World) =
    match pools.LifeFlask, w.Player with
    | Some f, Some p when p.Died.IsNone && pools.Life > 1.0 && p.Life < 0.5 * pools.Life && w.Time >= p.LifeFlaskUntil && p.LifeFlaskCharges >= f.ChargesPerUse ->
        { w with
            Player =
                Some
                    { p with
                        LifeFlaskCharges = p.LifeFlaskCharges - f.ChargesPerUse
                        LifeFlaskUntil = w.Time + f.Duration
                        LifeFlaskRate = f.Amount * (1.0 - f.Instant) * pools.LifeRecoveryRate / f.Duration
                        Life = min (max p.Life pools.Life) (p.Life + f.Amount * f.Instant * pools.LifeRecoveryRate)
                        EnergyShield =
                            if pools.LifeFlaskAlsoES then min (max p.EnergyShield (min pools.EnergyShield pools.ESRecoverCap)) (p.EnergyShield + f.Amount * f.Instant * pools.LifeRecoveryRate)
                            else p.EnergyShield }
            Counts = bump "flask:life" w.Counts }
    | _ -> w

/// What of a use's cost is paid with life, and what is left for mana: "X% of Mana Costs
/// Converted to Life Costs" (that share of the cost), "Skills gain a Base Life Cost equal
/// to Base Mana Cost" (the cost once more, in life), "cost an additional X% of your maximum
/// Life"; the life part over "increased Life Cost Efficiency". Returns (mana, life).
let splitCost (pools: PlayerStats) (ctx: Ctx) (db: ModDb) (cost: float) : float * float =
    let share = Mods.sumBase ctx [ Named "CostAsLife" ] db
    let equal = Mods.sumBase ctx [ Named "LifeCostEqualToMana" ] db
    let ofLife = Mods.sumBase ctx [ Named "LifeCostPercentOfLife" ] db
    if share = 0.0 && equal = 0.0 && ofLife = 0.0 then cost, 0.0
    else
        let k = min 1.0 (max 0.0 (share / 100.0))
        cost * (1.0 - k), (cost * k + (if equal > 0.0 then cost else 0.0) + pools.Life * ofLife / 100.0) / Mods.inc ctx [ Named "LifeCostEfficiency" ] db

/// The mana a use needs to begin, and how far below nothing paying for it may leave the
/// mana. A cost by the second is paid while the channel goes on, and mana regenerates
/// meanwhile: the use needs what regeneration will not have covered by its end, and never
/// more than a full pool (a channel is held as long as the mana lasts); what regenerates
/// meanwhile is spent before it is there, so the mana is below nothing until then.
let manaNeeded (pools: PlayerStats) (skill: Skill) (seconds: float) (cost: float) : float * float =
    if skill.CostPerSecond <= 0.0 then cost, 0.0
    else
        let flows = max 0.0 (pools.ManaRegen * seconds)
        min pools.Mana (max 0.0 (cost - flows)), flows

/// What a use of the skill costs now: base x increased cost / (1 + cost efficiency).
let costOf (env: Env) (actor: Actor) (st: ActorState) (w: World) (skill: Skill) : float =
    let db = skillDb env actor skill st
    let costCtx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
    let extraCost =
        match actor.Vitals with
        | Some pools -> pools.Mana * Mods.sumBase costCtx [ Named "ExtraCostPercentOfMaxMana" ] db / 100.0
        | None -> 0.0
    // (a skill with a window: the cost is for opening it)
    let ownCost =
        if (window skill).IsSome then (if windowShut st w.Time skill then skill.Cost else 0.0)
        elif skill.Clip > 0 && not (st.LoadDue.Contains skill.Name) then 0.0 else skill.Cost
    // a channel: its cost per second for as long as this use takes; what it costs to
    // start is paid when the channel begins, not with every use that continues it
    let ownCost =
        if skill.CostPerSecond <= 0.0 then ownCost
        // ("Modifiers to Cast / Attack Speed also affect this Skill's cost": so much more
        // a second as the skill is faster, which is what its unhurried time costs)
        else (if st.LastUsed = skill.Name then 0.0 else ownCost) + skill.CostPerSecond * (if partFlag skill "CostOverTimeWithSpeed" then Channel.paidSeconds skill else max Constants.serverTick (useTime env actor skill w))
    (ownCost + extraCost) * Mods.mult costCtx [ ManaCost ] db / Mods.inc costCtx [ ManaCostEfficiency ] db

/// Whether the player can pay for a use of the skill now (mana, or life where that pays).
let affordable (env: Env) (actor: Actor) (st: ActorState) (w: World) (skill: Skill) : bool =
    let costOf = costOf env actor st w
    match actor.Vitals, w.Player with
    | Some pools, Some p when pools.CostsInfernalFlame -> true
    | Some pools, Some p when pools.CostsLife -> (let c = costOf skill in c <= 0.0 || p.Life - c >= 1.0)
    // (enough within rounding: a wait for mana ends with exactly the cost, give or take the last bit)
    | Some pools, Some p ->
        let cost, life = splitCost pools { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name } (skillDb env actor skill st) (costOf skill)
        let cost = if skill.CostPerSecond <= 0.0 then cost else fst (manaNeeded pools skill (max Constants.serverTick (useTime env actor skill w)) cost)
        not (p.Mana < cost - 1e-9 * max 1.0 cost) && (life <= 0.0 || p.Life - life >= 1.0)
    | _ -> true

/// What the actor does next: the first step of its rotation that is ready and can be paid
/// for; if none can be paid, the first that is ready (it waits for the mana). `v`: what the
/// actor sees (`view`), for the conditions of the steps.
let nextStep (env: Env) (actor: Actor) (st: ActorState) (v: View) (w: World) : Step option =
    // a skill that works on a corpse needs one: every kill leaves a corpse, each use takes one
    let corpses = w.Kills.Length - w.CorpsesUsed
    // (unless it does without: something else in its place, or corpses only add to it)
    let needsCorpse (s: Skill) =
        s.Tags.Contains(Tag "Type:TargetsDestructibleCorpses")
        && (match corpseRule actor s with
            | Some (SummonPerCorpse _) -> false
            | Some (WithCorpse (_, SpendStack (buff, _))) -> not (Map.tryFind buff st.Buffs |> Option.exists (fun e -> e.Stacks > 0 && e.Expires > w.Time))
            | Some _ -> true
            | None -> not (s.Tags.Contains(Tag "Type:SkillConsumesPowerChargesOnUse")))
    let usable =
        actor.Rotation |> List.filter (fun step ->
            let s = skillOf actor step.Use
            step.When v && ready st w.Time s && (corpses > 0 || not (needsCorpse s)) && built st s
            // a form one is in is not pressed again (that would end it)
            && (match partNamed s "ToggleBuff:" with Some buff -> not (st.Buffs.ContainsKey buff) | None -> true)
            // a skill that fires what has built up needs some of it
            && (not (partFlag s "NeedsCharge") || (match s.Consumes with Some charge -> (Map.tryFind charge st.Buffs |> Option.exists (fun e -> e.Stacks > 0 && e.Expires > w.Time)) | None -> true))
            // a skill with a window waits until it is charged again
            && windowOk st w.Time s
            // a warp needs something to warp into: an enemy that can be culled, or a projectile
            && (match warpsInto s with
                | Some gem -> ballInFlight actor.Name gem w || (aimed env w |> Option.exists (fun e -> (status e).ContainsKey "Cullable"))
                | None -> true)
            // a skill that is empowered by something lying about waits for it, where the character makes it
            && (not (partFlag s "NoCooldownWithCharge")
                || (match s.Consumes with
                    | Some charge -> (Map.tryFind charge actor.MaxResources |> Option.defaultValue 0) = 0 || (Map.tryFind charge st.Buffs |> Option.exists (fun e -> e.Stacks > 0 && e.Expires > w.Time))
                    | None -> true))
            // a skill that spends what has piled up on the enemy waits for the pile to be full
            && (s.Mods |> List.forall (fun m ->
                    match m.Stat with
                    | Named n when n.StartsWith "NeedsEnemyStacks:" ->
                        aimed env w |> Option.exists (fun e -> (Map.tryFind (n.Substring 17) (status e) |> Option.defaultValue 0) >= int m.Value)
                    | _ -> true))
            // Hexblast waits for a curse that can be detonated
            && (not (partFlag s "DetonatesCurse") || (aimed env w |> Option.exists (fun e -> not (ripeCurses actor e w.Time).IsEmpty)))
            // a recording debuff is left to run out
            && not (w.Agonies |> List.exists (fun a -> a.Owner = actor.Name && a.Skill = s.Name))
            // a seed still in an enemy is not planted over
            && not (w.Bombs |> List.exists (fun b -> b.Owner = actor.Name && b.Skill = s.Name)))
    usable |> List.tryFind (fun step -> affordable env actor st w (skillOf actor step.Use)) |> Option.orElse (List.tryHead usable)

/// A skill an actor is about to use, as the steps of paying for it and setting it going see it.
type ReadyUse =
    { Env: Env
      Rng: Rng
      Actor: Actor
      Who: string
      Skill: Skill
      /// The actor's state when it got ready.
      State: ActorState
      /// How long the use takes.
      Takes: float
      /// What the skill's cost is asked with, and the actor's modifiers with the skill.
      CostCtx: Ctx
      Db: ModDb }

/// What the use costs: the skill's own cost (a load of bolts or an opened window is paid once,
/// a cost by the second for the time), Archmage's share of maximum mana, cost modifiers and efficiency.
let useCost (u: ReadyUse) (w: World) : float =
    let env, rng, actor, name, skill, st, t, costCtx, db = u.Env, u.Rng, u.Actor, u.Who, u.Skill, u.State, u.Takes, u.CostCtx, u.Db
    // Archmage: spells cost an extra share of maximum mana
    let extraCost =
        match actor.Vitals with
        | Some pools -> pools.Mana * Mods.sumBase costCtx [ Named "ExtraCostPercentOfMaxMana" ] db / 100.0
        | None -> 0.0
    // crossbow ammunition: the cost is for loading, paid with the first shot of a new load
    // (a skill with a window: the cost is for opening it)
    let ownCost =
        if (window skill).IsSome then (if windowShut st w.Time skill then skill.Cost else 0.0)
        elif skill.Clip > 0 && not (st.LoadDue.Contains skill.Name) then 0.0 else skill.Cost
    let ownCost =
        if skill.CostPerSecond <= 0.0 then ownCost
        else (if st.LastUsed = skill.Name then 0.0 else ownCost) + skill.CostPerSecond * (if partFlag skill "CostOverTimeWithSpeed" then Channel.paidSeconds skill else t)
    let cost = (ownCost + extraCost) * Mods.mult costCtx [ ManaCost ] db / Mods.inc costCtx [ ManaCostEfficiency ] db
    cost

/// after the last bolt of a load: reload (attack speed and reload speed shorten it;
/// PoB: increases add up, more multiplies), unless the reload is instant
let reloadAfter (u: ReadyUse) (w: World) : World * float =
    let env, rng, actor, name, skill, st, t, costCtx, db = u.Env, u.Rng, u.Actor, u.Who, u.Skill, u.State, u.Takes, u.CostCtx, u.Db
    if skill.Clip <= 0 then w, 0.0
    else
        let st = Map.find name w.Actors
        let ctx = { costCtx with Buffs = buffStacks st } |> owned env w
        let left = (Map.tryFind skill.Name st.Ammo |> Option.defaultValue skill.Clip)
        let kept = rng.Chance(Mods.sumBase ctx [ Named "KeepAmmoChance" ] db)
        let left = if kept then left else left - 1
        let w = updateActor name (fun st -> { st with Ammo = Map.add skill.Name left st.Ammo; LoadDue = st.LoadDue.Remove skill.Name }) w
        if left > 0 then w, 0.0
        else
            let speed = (1.0 + (Mods.sumInc ctx [ Speed ] db + Mods.sumInc ctx [ Named "ReloadSpeed" ] db) / 100.0) * Mods.more ctx [ Speed ] db * Mods.more ctx [ Named "ReloadSpeed" ] db
            let instant = rng.Chance(Mods.sumBase ctx [ Named "InstantReloadChance" ] db)
            let seconds = if instant then 0.0 else skill.ReloadTime / max 0.1 speed
            let w =
                updateActor name (fun st ->
                    { st with
                        Ammo = Map.add skill.Name skill.Clip st.Ammo
                        LoadDue = st.LoadDue.Add skill.Name
                        Buffs = Map.add "ReloadedRecently" { Name = "ReloadedRecently"; Mods = []; Stacks = 1; Expires = w.Time + t + seconds + 4.0 } st.Buffs }) w
                |> schedule (w.Time + t + seconds + 4.0) Expire
            { w with Counts = bump ("reload:" + skill.Name) w.Counts }, seconds

/// The use goes ahead: a window it opens, the use itself, a reload after it, and when the actor is ready again.
let goAhead (u: ReadyUse) (w: World) : World =
    let env, rng, actor, name, skill, st, t, costCtx, db = u.Env, u.Rng, u.Actor, u.Who, u.Skill, u.State, u.Takes, u.CostCtx, u.Db
    // the use that opens a window: it lasts so long, and charges again after it
    let w =
        match window skill with
        | Some (secs, recharge) when windowShut st w.Time skill ->
            { w with Counts = bump ("window:" + skill.Name) w.Counts }
            |> updateActor name (fun st ->
                { st with Energy = st.Energy |> Map.add (Keys.window skill.Name) (w.Time + secs) |> Map.add (Keys.windowNext skill.Name) (w.Time + secs + recharge) })
        | _ -> w
    let w = startUse env rng actor skill t w
    let w, extra = reloadAfter u w
    let w =
        if env.Fight.March.IsNone || name <> env.Fight.Player.Name then w
        else { w with WalkHeldUntil = w.Time + t + extra; WalkKept = (if extra > 0.0 then 0.0 else Channel.walked skill t); Lead = t }
    schedule (w.Time + t + extra) (Ready name) w

/// drink the mana flask when mana is getting low and it isn't already running
let drinkManaFlask (u: ReadyUse) (cost: float) (w: World) : World =
    let env, rng, actor, name, skill, st, t, costCtx, db = u.Env, u.Rng, u.Actor, u.Who, u.Skill, u.State, u.Takes, u.CostCtx, u.Db
    let w =
        match actor.ManaFlask, w.Player with
        | Some f, Some p when p.Mana < 2.0 * cost && w.Time >= p.FlaskUntil && p.FlaskCharges >= f.ChargesPerUse ->
            { w with
                Player =
                    // "X% of Flask Recovery applied Instantly"
                    (let manaMax = actor.Vitals |> Option.map (fun pools -> pools.Mana) |> Option.defaultValue p.Mana
                     Some
                         { p with
                             FlaskCharges = p.FlaskCharges - f.ChargesPerUse
                             FlaskUntil = w.Time + f.Duration
                             FlaskRate = f.Amount * (1.0 - f.Instant) / f.Duration
                             Mana = min (max p.Mana manaMax) (p.Mana + f.Amount * f.Instant) })
                Counts = bump "flask:mana" w.Counts }
        | _ -> w
    w

/// The cost is paid (with mana, life or Infernal Flame) and the use goes ahead; or it
/// cannot be paid yet and the actor waits; or never, and the skill is given up.
let payAndGo (u: ReadyUse) (cost: float) (lifeCost: float) (w: World) : World =
    let env, rng, actor, name, skill, st, t, costCtx, db = u.Env, u.Rng, u.Actor, u.Who, u.Skill, u.State, u.Takes, u.CostCtx, u.Db
    match actor.Vitals, w.Player with
    | Some v, Some p when lifeCost > 0.0 && p.Life - lifeCost < 1.0 ->
        // a life cost that would kill: the skill waits (and the life flask is drunk)
        let wait = max Constants.serverTick 0.25
        { w with Player = Some { p with Starved = p.Starved + wait } } |> drinkLifeFlask v |> schedule (w.Time + wait) (Ready name)
    | Some v, Some p when v.CostsLife ->
        // Blood Magic: the cost is paid with life; a skill that would kill waits
        if p.Life - cost >= 1.0 || cost <= 0.0 then
            { w with Player = Some { p with Life = p.Life - cost } } |> goAhead u
        else
            let wait = max Constants.serverTick 0.25
            { w with Player = Some { p with Starved = p.Starved + wait } } |> schedule (w.Time + wait) (Ready name)
    | Some v, Some p when v.CostsInfernalFlame ->
        // Pyromantic Pact: the cost builds Infernal Flame (max: twice max mana); reaching the
        // maximum empties it and burns maximum life + energy shield as fire damage
        let flame = p.InfernalFlame + cost
        let w =
            if flame >= 2.0 * v.Mana then
                // a fire hit on yourself: fire resistance applies
                let fireRes = Map.tryFind Fire v.Resist |> Option.defaultValue 0.0
                let burn = (v.Life + v.EnergyShield) * Mods.sumBase Ctx.empty [ Named "InfernalFlameBurn" ] (ModDb.ofList actor.Mods) / 100.0 * (1.0 - fireRes / 100.0)
                { w with Player = Some { p with InfernalFlame = 0.0 }; Counts = bump "infernal flame burn" w.Counts }
                |> takeDamage false v (Map.ofList [ Fire, burn ])
            else { w with Player = Some { p with InfernalFlame = flame } }
        goAhead u w
    | Some v, Some p when p.Mana < cost - 1e-9 * max 1.0 cost && cost > v.Mana && skill.CostPerSecond <= 0.0 ->
        // costs more than the whole mana pool: this skill can never be used
        // (a cost by the second is paid as far as the pool goes: `manaNeeded`)
        { w with Counts = bump ("cannot afford:" + skill.Name) w.Counts }
        |> updateActor name (fun st -> { st with Cooldowns = Map.add skill.Name infinity st.Cooldowns })
        |> schedule (w.Time + Constants.serverTick) (Ready name)
    | Some v, Some p when (let need = fst (manaNeeded v skill t cost) in p.Mana < need - 1e-9 * max 1.0 need) ->
        // not enough mana: wait until regeneration and leech have covered it
        let cost = fst (manaNeeded v skill t cost)
        let rate =
            v.ManaRegen + (p.Recoups |> List.sumBy (fun r -> if r.Pool = RecoupPool.LeechMana && r.Until > w.Time then r.Rate else 0.0))
            + (if w.Time < p.FlaskUntil then p.FlaskRate else 0.0)
        let wait = max Constants.serverTick (if rate > 0.0 then (cost - p.Mana) / rate else 1.0)
        { w with Player = Some { p with Starved = p.Starved + wait } } |> schedule (w.Time + wait) (Ready name)
    | Some v, Some p ->
        { w with Player = Some { p with Mana = max (-(snd (manaNeeded v skill t cost))) (p.Mana - cost); Life = p.Life - lifeCost } }
        |> (if lifeCost > 0.0 then drinkLifeFlask v else id) |> sigilSpend name cost v.Mana |> goAhead u
    | _ -> goAhead u w

/// An actor that is free to act uses the next step of its rotation: the cost is paid (or
/// waited for), the use starts, and the actor is ready again when it is over.
let onReadyUnlocked env (rng: Rng) name (w: World) =
    let actor = actorDef env name
    let st = Map.find name w.Actors
    let v = view env w st
    match nextStep env actor st v w with
    | Some step ->
        let skill = skillOf actor step.Use
        // nothing is faster than a server tick (instant skills)
        let t = max Constants.serverTick (useTime env actor skill w)
        // mana cost: base x increased cost / (1 + cost efficiency)
        let db = skillDb env actor skill st
        let costCtx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
        let u = { ReadyUse.Env = env; Rng = rng; Actor = actor; Who = name; Skill = skill; State = st; Takes = t; CostCtx = costCtx; Db = db }
        let cost = useCost u w
        let w = drinkManaFlask u cost w
        // what of the cost is paid with life (after the flask, which goes by the whole cost)
        let cost, lifeCost =
            match actor.Vitals with
            | Some v when not v.CostsLife && not v.CostsInfernalFlame -> splitCost v costCtx db cost
            | _ -> cost, 0.0
        payAndGo u cost lifeCost w
    | None ->
        // Nothing usable: wait for the next cooldown, or a server tick.
        let next =
            st.Cooldowns |> Map.toList |> List.map snd |> List.filter (fun t -> t > w.Time && t < infinity)
            |> function
                | [] -> w.Time + Constants.serverTick
                | ts -> List.min ts
        schedule next (Ready name) w

/// Has this melee minion no room at the enemy aimed at? The melee minions in the fight
/// take their places in order (see Geometry.meleePlaces).
let crowdedOut env name (w: World) =
    match (actorDef env name).Body, aimed env w with
    | Some body, Some enemy when body.Melee ->
        let melee =
            env.Fight.Minions
            |> List.filter (fun m -> (m.Body |> Option.exists (fun b -> b.Melee)) && not (Map.find m.Name w.Actors).Gone)
        let places = Geometry.meleePlaces enemy.Def.Size (melee |> List.map (fun m -> m.Body.Value.Size, m.Body.Value.Reach))
        List.zip melee places |> List.exists (fun (m, has) -> m.Name = name && not has)
    | _ -> false

/// The actor is free to act; a stunned or frozen player waits.
let onReady env rng name (w: World) =
    let actor = actorDef env name
    let standing =
        match actor.ActiveWhile with
        | Some buff -> (Map.find env.Fight.Player.Name w.Actors).Buffs |> Map.exists (fun k e -> k = buff && e.Expires > w.Time)
        | None -> true
    match w.Player with
    | Some p when name = env.Fight.Player.Name && p.LockedUntil > w.Time -> schedule p.LockedUntil (Ready name) w
    // on a march the player walks on until an enemy is near enough to use its skills on
    // (one that is awake and comes at it: near enough by the time a use takes, as far as
    // it comes, so that the skill lands when it is there and not after)
    | Some _ when name = env.Fight.Player.Name
                  && (match env.Fight.March with
                      | Some m ->
                          not (alive w |> List.exists (fun e ->
                                  let d = Vec.length e.Def.Position - e.Def.Size
                                  let coming =
                                      match e.Def.Attack with
                                      | Some atk when w.Woken.Contains e.Def.Name -> min (max 0.0 (d - atk.Range)) (e.Def.Speed * w.Lead)
                                      | _ -> 0.0
                                  d - coming <= m.Engage))
                      | None -> false) ->
        schedule (w.Time + Constants.serverTick) (Ready name) w
    // a totem that isn't placed waits
    | _ when not standing -> schedule (w.Time + 0.1) (Ready name) w
    // a melee minion that finds no room at the enemy waits for some
    | _ when crowdedOut env name w -> schedule (w.Time + Constants.crowdedWait) (Ready name) { w with Counts = bump "minion:no room" w.Counts }
    | _ -> onReadyUnlocked env rng name w
