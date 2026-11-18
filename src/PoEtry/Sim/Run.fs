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

/// One fight with the given seed: optionally told to a listener, optionally kept for showing.
let runWith (trace: (float -> string -> string -> string -> float -> unit) option) (tracing: bool) (fight: Fight) (seed: uint32) : Outcome =
    let fight = Fight.apart fight
    let env = envOf trace fight
    let rng = Rng(seed)
    /// Expiry events for the resources an actor starts with.
    let scheduleResourceTimers (w: World) (a: Actor) =
        a.ResourceDurations
        |> Map.fold
            (fun w k _ ->
                match Map.tryFind k (Map.find a.Name w.Actors).Buffs with
                | Some e when e.Expires < infinity -> schedule e.Expires Expire w
                | _ -> w)
            w
    let actors = fight.Player :: fight.Minions
    let empty =
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
    /// An enemy as it comes into the fight.
    let fresh (d: EnemyDef) : EnemyState =
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
    let w0 =
        { Player =
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
          Time = 0.0
          Grounds = []
          Enemies =
            fight.Enemies |> List.map (fun d -> d.Name, fresh d)
            |> Map.ofList
          Actors =
            actors
            |> List.map (fun a ->
                let res =
                    a.Resources
                    |> List.filter (fun (_, n) -> n > 0)
                    |> List.map (fun (k, n) ->
                        // steady state: a random part of the duration has already passed
                        let expires = match Map.tryFind k a.ResourceDurations with Some d -> rng.Float() * d | None -> infinity
                        k, { Name = k; Mods = []; Stacks = n; Expires = expires })
                    |> Map.ofList
                let banked =
                    a.Metas
                    |> List.choose (fun m ->
                        match m.Release with
                        | Invocation _ -> Some(m.Name, m.MaxEnergy * fight.BankedInvocations)
                        | AtMaximum -> None)
                    |> Map.ofList
                // charge buffs (Charged Staff) are up when the fight starts, if fights start prepared
                let res =
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
                let life, shield = match a.Body with Some b -> b.Life, b.EnergyShield | None -> 0.0, 0.0
                a.Name, { empty with Buffs = res; Energy = banked; Life = life; Shield = shield; Gone = a.Pool.IsSome })
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
        // (a temporary minion starts acting when it is summoned)
        |> fun w -> actors |> List.fold (fun w a -> if a.Pool.IsSome then w else schedule 0.0 (Ready a.Name) w) w
        |> fun w -> actors |> List.fold (fun w a -> a.Cycles |> List.fold (fun w c -> schedule 0.0 (CycleTick(a.Name, c.Name)) w) w) w
        |> fun w -> actors |> List.fold scheduleResourceTimers w
        |> fun w ->
            // periodic actions start at a random point of their first period
            actors |> List.fold (fun w a -> a.Periodics |> List.fold (fun w p -> schedule (rng.Float() * p.Every) (PeriodicTick(a.Name, p.Name)) w) w) w
        |> fun w ->
            if fight.Player.Vitals.IsNone && fight.Minions |> List.forall (fun m -> m.Body.IsNone) then w
            else
                // monsters start attacking at random points in their first interval
                fight.Enemies
                |> List.fold (fun w e -> match e.Attack with Some a -> schedule (rng.Float() * a.Interval) (EnemyAttack e.Name) w | None -> w) w
        |> schedule fight.MaxTime Expire
    // a fight that needs more events than this is a runaway (a zero-time loop): stop it
    // A march's packs: one after the other as far ahead as twice the monsters' aggro, each a
    // spacing after the one before, and after the last of them the map's boss.
    let supply (w: World) : World =
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
                            let w = { w with Enemies = Map.add d.Name (fresh d) w.Enemies; Shown = (if w.Tracing then Entered(w.Time, d) :: w.Shown else w.Shown) }
                            if fight.Player.Vitals.IsNone && fight.Minions |> List.forall (fun m -> m.Body.IsNone) then w
                            else match d.Attack with Some a -> schedule (w.Time + rng.Float() * a.Interval) (EnemyAttack d.Name) w | None -> w) w
                    more { w with Front = at; Packs = w.Packs + 1 }
            more w
    let w0 = supply w0
    let maxEvents = 3000000
    let rec loop n (w: World) =
        let died = w.Player |> Option.exists (fun p -> p.Died.IsSome)
        // (a march has no last enemy: the packs go on)
        if died || (allDead w && (match fight.March with Some m -> w.Packs > m.Packs | None -> true)) || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > fight.MaxTime then w
        elif n >= maxEvents then { w with Counts = bump "aborted: too many events" w.Counts }
        else
            let before = w.Kills
            let w = supply (step env rng w)
            // those that just died are replaced: one of the same kind comes from where the
            // first of them began, further out and somewhat to the side, and walks in
            let w =
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
                            let w = { w with Enemies = Map.add d.Name (fresh d) w.Enemies; Shown = (if w.Tracing then Entered(w.Time, d) :: w.Shown else w.Shown) }
                            if fight.Player.Vitals.IsNone && fight.Minions |> List.forall (fun m -> m.Body.IsNone) then w
                            else match d.Attack with Some a -> schedule (w.Time + rng.Float() * a.Interval) (EnemyAttack d.Name) w | None -> w) w
            // (a march: how far the player has come, for showing the map go by)
            let w =
                if w.Tracing && fight.March.IsSome && w.Walked - w.ShownWalked >= 1.0 then { w with Shown = Walked(w.Time, w.Walked) :: w.Shown; ShownWalked = w.Walked }
                else w
            // where the enemies are now, for showing: those that moved since last noted
            let w =
                if not w.Tracing then w
                else
                    alive w |> List.fold (fun (w: World) e ->
                        match Map.tryFind e.Def.Name w.ShownAt with
                        | Some at when Vec.dist at e.Def.Position < 1.0 -> w
                        | _ -> { w with Shown = Moved(w.Time, e.Def.Name, e.Def.Position) :: w.Shown; ShownAt = Map.add e.Def.Name e.Def.Position w.ShownAt }) w
            loop (n + 1) w
    let w = loop 0 w0
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
