/// A whole fight: what is fixed for it, the world at its start, the loop over the events,
/// and the outcome read off the world at the end.
[<AutoOpen>]
module internal PoEtry.Engine.Run

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// The defences that the runs of the fight that was run last have worked out, by the
/// buffs written out (a function of the buffs: knowing them changes no result). Runs
/// of one fight follow each other, or go side by side: they share what is known.
let vitalsOfLast : (Fight * System.Collections.Generic.Dictionary<string, PlayerStats>) option[] = Array.create 1 None
let vitalsKnown (fight: Fight) : System.Collections.Generic.Dictionary<string, PlayerStats> =
    lock vitalsOfLast (fun () ->
        match vitalsOfLast.[0] with
        | Some (last, known) when obj.ReferenceEquals(last, fight) -> known
        | _ ->
            let known = System.Collections.Generic.Dictionary<string, PlayerStats>()
            vitalsOfLast.[0] <- Some(fight, known)
            known)

let envOf (trace: (float -> string -> string -> string -> float -> unit) option) (fight: Fight) =
    { Fight = fight
      Trace = trace
      Rage =
        (let db = ModDb.ofList fight.Player.Mods
         let c = Ctx.empty
         { Regen = Mods.sumBase c [ Named "RageRegen" ] db
           Loss = (if Mods.flag c (Named "NoRageLoss") db then 0.0 else Constants.rageLossPerSecond * Mods.inc c [ Named "RageDecay" ] db)
           Delay = Constants.rageLossDelay + Mods.sumBase c [ Named "RageLossDelay" ] db
           WhenHit = Mods.sumBase c [ Named "RageWhenHit" ] db
           OnKill = Mods.sumBase c [ Named "RageOnKill" ] db })
      BerserkLoss = Mods.sumBase Ctx.empty [ Named "LifeLossPercentPerMinutePerRage" ] (ModDb.ofList fight.Player.Mods)
      CharmKeep = Mods.sumBase Ctx.empty [ Named "CharmKeepCharges" ] (ModDb.ofList fight.Player.Mods)
      SlowMagnitude = Mods.inc Ctx.empty [ Named "SlowMagnitude" ] (ModDb.ofList fight.Player.Mods)
      AsksESRecharge = asksBuff fight "ESRechargeStartedRecently"
      WithBuffs = System.Collections.Generic.Dictionary()
      VitalsSeen = vitalsKnown fight
      VitalsLast = Array.create 1 None
      SkillDbs =
        Map.ofList
            [ for a in fight.Player :: fight.Minions do
                  for s in allSkills a -> (a.Name, s.Name), ModDb.ofList (a.Mods @ s.Mods) ] }

/// An actor before anything has happened to it.
let emptyActor : ActorState =
    { LastUsed = ""
      Paused = Map.empty
      Buffs = Map.empty
      Cooldowns = Map.empty
      Recharges = Map.empty
      Energy = Map.empty
      Ammo = Map.empty
      LoadDue = Set.empty
      RageAt = 0.0
      RageCarry = 0.0
      Pending = Map.empty
      CritRolls = Map.empty
      Rolled = Map.empty
      Life = 0.0
      Shield = 0.0
      HitAt = -1e9
      Gone = false
      Leaves = infinity }

/// An enemy as it comes into the fight (with the auras the fight puts on every enemy).
let freshEnemy (fight: Fight) (d: EnemyDef) : EnemyState =
    { Def = d
      Life = d.Life
      Debuffs =
        fight.EnemyAuras
        |> List.map (fun (name, stacks, mods) ->
            let k = if name.StartsWith "Curse:" then Mods.more Ctx.empty [ Named "CurseEffectOnSelf" ] (ModDb.ofList d.Mods) else 1.0
            name, { Name = name; Mods = mods |> List.map (Mod.scaled k); Stacks = stacks; Expires = infinity })
        |> Map.ofList
      Dots = []
      Buildup = Map.empty
      Immune = Map.empty
      StunnedUntil = 0.0
      StunBuildupAt = 0.0
      Flammability = []
      Db = ModDb.ofList d.Mods }

/// The player's pools when the fight starts (none for a fight in which nothing hits back
/// and nothing is paid).
let playerAtStart (fight: Fight) : PlayerState option =
    fight.Player.Vitals
    |> Option.map (fun v ->
        { Life = max 1.0 (v.Life * v.StartLife)
          EnergyShield = v.EnergyShield
          Mana = v.Mana
          LastHit = -1e9
          LeechPool = 0.0
          Died = None
          Taken = 0.0
          Starved = 0.0
          MinLife = 1.0
          ManaGained = Map.empty
          FlaskCharges = fight.Player.ManaFlask |> Option.map (fun f -> f.MaxCharges) |> Option.defaultValue 0.0
          FlaskUntil = 0.0
          FlaskRate = 0.0
          StrikeLeech = 0.0
          Ward = v.Ward
          Darkness = []
          InfernalFlame = 0.0
          LastStunRemnant = -1e9
          LastManaRemnant = -1e9
          LastLifeRemnant = -1e9
          LifeLeechPool = 0.0
          StrikeLifeLeech = 0.0
          LifeFlaskCharges = v.LifeFlask |> Option.map (fun f -> f.MaxCharges) |> Option.defaultValue 0.0
          LifeFlaskUntil = 0.0
          LifeFlaskRate = 0.0
          Dots = []
          Recoups = []
          LockedUntil = 0.0
          FreezeBuildup = 0.0
          Stuns = []
          CharmCharges = fight.Player.Charms |> List.map (fun c -> c.Buff, c.MaxCharges) |> Map.ofList })

/// The charges and the like an actor starts with: each that runs out has a random part of
/// its time behind it already (a fight starts in the middle of things).
let startingResources (rng: Rng) (a: Actor) : Map<string, Effect> =
    a.Resources
    |> List.filter (fun (_, n) -> n > 0)
    |> List.map (fun (k, n) ->
        // steady state: a random part of the duration has already passed
        let expires = match Map.tryFind k a.ResourceDurations with Some d -> rng.Float() * d | None -> infinity
        k, { Name = k; Mods = []; Stacks = n; Expires = expires })
    |> Map.ofList

/// Energy an invocation has banked before the fight (a share of its maximum, by the fight's setting).
let bankedEnergy (fight: Fight) (a: Actor) : Map<string, float> =
    a.Metas
    |> List.choose (fun m ->
        match m.Release with
        | Invocation _ -> Some(m.Name, m.MaxEnergy * fight.BankedInvocations)
        | AtMaximum -> None)
    |> Map.ofList

/// Buffs made from charges (Charged Staff) are up when the fight starts, if fights start prepared.
let startingChargeBuffs (fight: Fight) (rng: Rng) (a: Actor) (res: Map<string, Effect>) : Map<string, Effect> =
    if fight.BankedInvocations <= 0.0 then res
    else
        a.Rules
        |> List.fold
            (fun (res: Map<string, Effect>) r ->
                match r.Reaction with
                | ChargeBuff (name, charge, perStack, seconds, doubleChance) ->
                    match Map.tryFind charge res with
                    | Some c when c.Stacks > 0 ->
                        let stacks = if rng.Chance doubleChance then 2 * c.Stacks else c.Stacks
                        // a random part of the duration has already passed
                        let left = seconds * float stacks * (0.5 + 0.5 * rng.Float())
                        Map.add name { Name = name; Mods = perStack; Stacks = stacks; Expires = left } res
                    | _ -> res
                | _ -> res)
            res

/// An actor as the fight begins: what it holds, its body (a minion's), and whether it is
/// out of the fight until summoned (a temporary minion).
let actorAtStart (fight: Fight) (rng: Rng) (a: Actor) : string * ActorState =
    let res = startingResources rng a
    let banked = bankedEnergy fight a
    let res = startingChargeBuffs fight rng a res
    let life, shield = match a.Body with Some b -> b.Life, b.EnergyShield | None -> 0.0, 0.0
    a.Name, { emptyActor with Buffs = res; Energy = banked; Life = life; Shield = shield; Gone = a.Pool.IsSome }

/// The world before its first event: everyone in their place, nothing in the queue.
let blankWorld (tracing: bool) (fight: Fight) (seed: uint32) (rng: Rng) : World =
    { Player = playerAtStart fight
      Time = 0.0
      Grounds = []
      Enemies =
        fight.Enemies |> List.map (fun d -> d.Name, freshEnemy fight d)
        |> Map.ofList
      Actors =
        fight.Player :: fight.Minions
        |> List.map (actorAtStart fight rng)
        |> Map.ofList
      Queue = Map.empty
      Tracing = tracing
      Shown = []
      ShownAt = Map.empty
      Seq = 0
      Seed = seed
      Evened = 0.0
      Overkill = 0.0
      Streams = Map.empty
      DamageBy = Map.empty
      DamageByType = Map.empty
      Counts = Map.empty
      Uptime = Map.empty
      Kills = []
      CorpsesUsed = 0
      Bombs = []
      Agonies = []
      Crystals = []
      Entropy = Map.empty
      Revival = None
      Boned = Set.empty
      Pustules = []
      Cause = ""
      Aim = ""
      Walked = 0.0
      WalkHeldUntil = 0.0
      WalkKept = 1.0
      Lead = 0.0
      Front = (match fight.March with Some m -> m.Engage | None -> 0.0)
      Packs = 0
      Woken = Set.empty
      ShownWalked = 0.0 }

/// Expiry events for the resources an actor starts with.
let scheduleResourceTimers (w: World) (a: Actor) =
    a.ResourceDurations
    |> Map.fold
        (fun w k _ ->
            match Map.tryFind k (Map.find a.Name w.Actors).Buffs with
            | Some e when e.Expires < infinity -> schedule e.Expires Expire w
            | _ -> w)
        w

/// Whether anything in the fight can be hit by a monster (if not, monsters do not attack).
let nobodyToHit (fight: Fight) = fight.Player.Vitals.IsNone && fight.Minions |> List.forall (fun m -> m.Body.IsNone)

/// The first events: everyone gets ready, cycles and timers start, monsters begin to attack,
/// and the fight's time limit is set.
let firstEvents (fight: Fight) (rng: Rng) (w: World) : World =
    let actors = fight.Player :: fight.Minions
    w
    // (a temporary minion starts acting when it is summoned)
    |> fun w -> actors |> List.fold (fun w a -> if a.Pool.IsSome then w else schedule 0.0 (Ready a.Name) w) w
    |> fun w -> actors |> List.fold (fun w a -> a.Cycles |> List.fold (fun w c -> schedule 0.0 (CycleTick(a.Name, c.Name)) w) w) w
    |> fun w -> actors |> List.fold scheduleResourceTimers w
    |> fun w ->
        // periodic actions start at a random point of their first period
        actors |> List.fold (fun w a -> a.Periodics |> List.fold (fun w p -> schedule (rng.Float() * p.Every) (PeriodicTick(a.Name, p.Name)) w) w) w
    |> fun w ->
        if nobodyToHit fight then w
        else
            // monsters start attacking at random points in their first interval
            fight.Enemies
            |> List.fold (fun w e -> match e.Attack with Some a -> schedule (rng.Float() * a.Interval) (EnemyAttack e.Name) w | None -> w) w
    |> schedule fight.MaxTime Expire

/// An enemy joins a fight that is under way: it is there, shown, and starts attacking at a
/// random point of its first interval.
let enters (fight: Fight) (rng: Rng) (d: EnemyDef) (w: World) : World =
    let w = { w with Enemies = Map.add d.Name (freshEnemy fight d) w.Enemies; Shown = (if w.Tracing then Entered(w.Time, d) :: w.Shown else w.Shown) }
    if nobodyToHit fight then w
    else match d.Attack with Some a -> schedule (w.Time + rng.Float() * a.Interval) (EnemyAttack d.Name) w | None -> w

/// A march's packs: one after the other as far ahead as twice the monsters' aggro, each a
/// spacing after the one before, and after the last of them the map's boss.
let supply (fight: Fight) (rng: Rng) (w: World) : World =
    match fight.March with
    | None -> w
    | Some m ->
        let rec more (w: World) =
            if w.Front >= 2.0 * m.Aggro + m.Spacing || w.Packs > m.Packs then w
            else
                let at = w.Front + m.Spacing
                let w =
                    m.Pack w.Packs |> List.fold (fun (w: World) (d: EnemyDef) ->
                        let d = { d with Name = sprintf "%s #%d" d.Name w.Packs; Position = Vec.add (Vec.make at 0.0) d.Position }
                        enters fight rng d w) w
                more { w with Front = at; Packs = w.Packs + 1 }
        more w

/// Those that just died are replaced (a fight set to go on for ever): one of the same kind
/// comes from where the first of them began, further out and somewhat to the side, and walks in.
let replaceFallen (fight: Fight) (rng: Rng) (before: (string * Rarity * float) list) (w: World) : World =
    if not fight.Replace || obj.ReferenceEquals(w.Kills, before) then w
    else
        let rec fallen (ks: (string * Rarity * float) list) =
            if obj.ReferenceEquals(ks, before) || ks.IsEmpty then [] else ks.Head :: fallen ks.Tail
        fallen w.Kills |> List.rev |> List.fold (fun (w: World) (name, _, _) ->
            let root = fight.Enemies |> List.tryFind (fun d -> d.Name = name.TrimEnd('+'))
            match root with
            | None -> w
            | Some d ->
                let at = Vec.scale (Vec.length d.Position * 1.25 + 10.0) (Vec.ofAngle (Vec.angle d.Position + (rng.Float() - 0.5) * 1.2))
                let d = { d with Name = name + "+"; Position = at }
                enters fight rng d w) w

/// For showing the fight: how far the player has come on a march, and where the enemies
/// that moved since it was last noted are now. Nothing in the fight depends on it.
let noteForShowing (fight: Fight) (w: World) : World =
    // (a march: how far the player has come, for showing the map go by)
    let w =
        if w.Tracing && fight.March.IsSome && w.Walked - w.ShownWalked >= 1.0 then { w with Shown = Walked(w.Time, w.Walked) :: w.Shown; ShownWalked = w.Walked }
        else w
    // where the enemies are now, for showing: those that moved since last noted
    if not w.Tracing then w
    else
        alive w |> List.fold (fun (w: World) e ->
            match Map.tryFind e.Def.Name w.ShownAt with
            | Some at when Vec.dist at e.Def.Position < 1.0 -> w
            | _ -> { w with Shown = Moved(w.Time, e.Def.Name, e.Def.Position) :: w.Shown; ShownAt = Map.add e.Def.Name e.Def.Position w.ShownAt }) w

/// A fight that needs more events than this is a runaway (a zero-time loop): it is stopped.
let maxEvents = 3000000

/// Whether the fight is over: the player has died, everyone is dead (and on a march no pack
/// is left to come), nothing is left to happen, or the time is up.
let isOver (fight: Fight) (w: World) =
    let died = w.Player |> Option.exists (fun p -> p.Died.IsSome)
    // (a march has no last enemy: the packs go on)
    died || (allDead w && (match fight.March with Some m -> w.Packs > m.Packs | None -> true)) || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > fight.MaxTime

/// Event after event until the fight is over.
let rec loop env (rng: Rng) n (w: World) : World =
    let fight = env.Fight
    if isOver fight w then w
    elif n >= maxEvents then { w with Counts = bump "aborted: too many events" w.Counts }
    else
        let before = w.Kills
        let w =
            step env rng w
            |> supply fight rng
            |> replaceFallen fight rng before
            |> noteForShowing fight
        loop env rng (n + 1) w

/// What the fight came to, read off the world it ended in.
let outcomeOf (fight: Fight) (w: World) : Outcome =
    let cleared = if allDead w then w.Kills |> List.map (fun (_, _, t) -> t) |> List.fold max 0.0 |> Some else None
    { Shown = List.rev w.Shown
      Ttk = cleared
      Duration = (match cleared with Some t -> t | None -> min w.Time fight.MaxTime)
      Kills = List.rev w.Kills
      DamageBy = w.DamageBy
      DamageByType = w.DamageByType
      Counts = (if fight.March.IsSome then Map.add "march:walked" (int w.Walked) w.Counts else w.Counts)
      Uptime = w.Uptime
      Died = w.Player |> Option.bind (fun p -> p.Died)
      DamageTaken = w.Player |> Option.map (fun p -> p.Taken) |> Option.defaultValue 0.0
      ManaStarved = w.Player |> Option.map (fun p -> p.Starved) |> Option.defaultValue 0.0
      Evened = w.Evened
      Overkill = w.Overkill
      MinLife = w.Player |> Option.map (fun p -> p.MinLife) |> Option.defaultValue 1.0
      ManaGained = w.Player |> Option.map (fun p -> p.ManaGained) |> Option.defaultValue Map.empty }

/// One fight with the given seed: optionally told to a listener, optionally kept for showing.
let runWith (trace: (float -> string -> string -> string -> float -> unit) option) (tracing: bool) (fight: Fight) (seed: uint32) : Outcome =
    let fight = Fight.apart fight
    let env = envOf trace fight
    let rng = Rng(seed)
    blankWorld tracing fight seed rng
    |> firstEvents fight rng
    |> supply fight rng
    |> loop env rng 0
    |> outcomeOf fight

/// The same fight (the same numbers), and what the player could take at every `every`
/// seconds of it (`Probe.probe`).
let runProbed (every: float) (fight: Fight) (seed: uint32) : Outcome * DefenceSample list =
    let fight = Fight.apart fight
    let env = envOf None fight
    let rng = Rng(seed)
    match playerAtStart fight with
    | None -> runWith None false fight seed, []
    | Some start ->
        let rec go n (due: float) (samples: DefenceSample list) (w: World) =
            // (the world before the event that is due at or after the next sample's time)
            let samples, due =
                if not (Map.isEmpty w.Queue) && fst (Seq.head w.Queue).Key >= due && w.Player |> Option.exists (fun p -> p.Died.IsNone) then
                    { probe env start w with Time = due } :: samples, due + every * ceil ((fst (Seq.head w.Queue).Key - due) / every + 1e-9)
                else samples, due
            if isOver fight w then w, samples
            elif n >= maxEvents then { w with Counts = bump "aborted: too many events" w.Counts }, samples
            else
                let before = w.Kills
                go (n + 1) due samples (step env rng w |> supply fight rng |> replaceFallen fight rng before |> noteForShowing fight)
        let w, samples = blankWorld false fight seed rng |> firstEvents fight rng |> supply fight rng |> go 0 0.0 []
        outcomeOf fight w, List.rev samples

