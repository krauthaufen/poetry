namespace PoEsie

/// When a hit of a skill use lands.
type HitTiming =
    /// Fraction of the use time (0 = start, 1 = end). Scales with speed.
    | DuringUse of fraction: float
    /// Fixed seconds after the use starts (delayed explosions, ground effects...).
    | After of seconds: float

/// What the engine can see when deciding which skill to use next.
type View =
    { Time: float
      Buffs: Map<string, int>
      EnemyStatus: Map<string, int>
      EnemyLifeFraction: float
      Energy: Map<string, float> }

type SkillFilter =
    | AnySkill
    | WithTag of SkillTag
    | Called of string

type Hook =
    | OnUse
    | OnHit
    | OnCrit
    | OnInflict of Ailment

type Reaction =
    /// Buff on the actor that owns the rule (charges are buffs too).
    | GrantBuff of name: string * duration: float * maxStacks: int * mods: Mod list
    /// Debuff on the enemy (exposure, curses, ...). Mods are per stack, defender side.
    | ApplyDebuff of name: string * duration: float * maxStacks: int * mods: Mod list
    /// Use a skill immediately, without spending the actor's time. Respects cooldown.
    | TriggerSkill of name: string
    /// Feed a meta gem.
    | GainEnergy of meta: string * amount: float
    | RemoveBuff of name: string

type Rule =
    { Hook: Hook
      Filter: SkillFilter
      /// Percent.
      Chance: float
      Reaction: Reaction }

/// A meta gem: collects energy from rules, triggers its socketed skill when full.
type Meta =
    { Name: string
      MaxEnergy: float
      Triggers: string }

type Step =
    { Use: string
      When: View -> bool }

/// The player or a minion.
type Actor =
    { Name: string
      /// The character sheet: gear, tree, permanent buffs.
      Mods: Mod list
      Skills: Skill list
      /// Timed hit pattern per skill name; default is a single hit at the end of the use.
      HitPattern: Map<string, (HitTiming * float) list>
      /// Priority list: the first usable step is used.
      Rotation: Step list
      Rules: Rule list
      Metas: Meta list }

module Actor =
    let make name mods skills =
        { Name = name
          Mods = mods
          Skills = skills
          HitPattern = Map.empty
          Rotation = skills |> List.map (fun s -> { Use = s.Name; When = fun _ -> true })
          Rules = []
          Metas = [] }

    let always skill = { Use = skill; When = fun _ -> true }

type EnemyDef =
    { Level: int
      Life: float
      /// Resists, armour, thresholds... Defender-side mods.
      Mods: Mod list
      /// Bleeding deals double damage to moving enemies.
      Moving: bool }

module EnemyDef =
    let atLevel level life =
        { Level = level
          Life = life
          Mods = [ Mod.base' Armour (Constants.monsterArmour level) ]
          Moving = false }

type Fight =
    { Enemy: EnemyDef
      Player: Actor
      Minions: Actor list
      MaxTime: float }

/// A timed effect with stacks.
type Effect =
    { Name: string
      Mods: Mod list
      Stacks: int
      Expires: float }

type DotInstance =
    { Ailment: Ailment
      Type: DamageType
      Dps: float
      Expires: float
      MaxStacks: int
      Owner: string }

type Outcome =
    { /// Time to kill, None if the enemy survived MaxTime.
      Ttk: float option
      Duration: float
      /// Damage dealt by source: "Skill (hit)", "Ignite", ...
      DamageBy: Map<string, float>
      /// Event counts: "uses:Skill", "hits:Skill", "crits:Skill", "inflict:Ignite", "trigger:Skill"...
      Counts: Map<string, int>
      /// Seconds each enemy status was active.
      Uptime: Map<string, float> }

module Sim =
    type private Event =
        | Ready of actor: string
        | Strike of actor: string * skill: string * mult: float
        | Expire

    type private ActorState =
        { Buffs: Map<string, Effect>
          Cooldowns: Map<string, float>
          Energy: Map<string, float> }

    type private World =
        { Time: float
          Life: float
          Debuffs: Map<string, Effect>
          Dots: DotInstance list
          Buildup: Map<Ailment, float>
          Actors: Map<string, ActorState>
          Queue: Map<float * int, Event>
          Seq: int
          DamageBy: Map<string, float>
          Counts: Map<string, int>
          Uptime: Map<string, float>
          Dead: float option }

    /// Fixed for the whole fight: mod dbs built once, buffs layered on per event.
    type private Env =
        { Fight: Fight
          SkillDbs: Map<string * string, ModDb>
          EnemyDb: ModDb }

    let private bump key (m: Map<string, int>) =
        Map.add key ((Map.tryFind key m |> Option.defaultValue 0) + 1) m

    let private addf key v (m: Map<string, float>) =
        Map.add key ((Map.tryFind key m |> Option.defaultValue 0.0) + v) m

    let private schedule t ev (w: World) =
        { w with
            Queue = Map.add (t, w.Seq) ev w.Queue
            Seq = w.Seq + 1 }

    // ---- views of the world ----

    /// Damaging ailments that actually deal damage: the strongest N of each kind.
    let private activeDots (w: World) =
        w.Dots
        |> List.groupBy (fun d -> d.Ailment)
        |> List.collect (fun (_, ds) ->
            let limit = ds |> List.map (fun d -> d.MaxStacks) |> List.max
            ds |> List.sortByDescending (fun d -> d.Dps) |> List.truncate limit)

    let private enemyStatus (env: Env) (w: World) =
        let fromDebuffs = w.Debuffs |> Map.map (fun _ e -> e.Stacks)
        let fromDots =
            activeDots w
            |> List.countBy (fun d -> Ailment.status d.Ailment)
            |> List.fold (fun m (k, n) -> Map.add k n m) fromDebuffs
        if env.Fight.Enemy.Moving then Map.add "Moving" 1 fromDots else fromDots

    let private defender (env: Env) (w: World) : Defender =
        let debuffMods =
            w.Debuffs
            |> Map.toList
            |> List.collect (fun (_, e) -> e.Mods |> List.map (Mod.scaled (float e.Stacks)))
        { Level = env.Fight.Enemy.Level
          Mods = ModDb.addList debuffMods env.EnemyDb }

    let private actorDef (env: Env) name =
        if name = env.Fight.Player.Name then env.Fight.Player
        else env.Fight.Minions |> List.find (fun m -> m.Name = name)

    let private buffStacks (st: ActorState) = st.Buffs |> Map.map (fun _ e -> e.Stacks)

    /// The actor's mods while using a skill: sheet + skill mods + active buffs.
    let private skillDb env (actor: Actor) (skill: Skill) (st: ActorState) =
        let buffMods = st.Buffs |> Map.toList |> List.collect (fun (_, e) -> e.Mods |> List.map (Mod.scaled (float e.Stacks)))
        ModDb.addList buffMods (Map.find (actor.Name, skill.Name) env.SkillDbs)

    let private view env (w: World) (st: ActorState) =
        { Time = w.Time
          Buffs = buffStacks st
          EnemyStatus = enemyStatus env w
          EnemyLifeFraction = w.Life / env.Fight.Enemy.Life
          Energy = st.Energy }

    let private matches filter (skill: Skill) =
        match filter with
        | AnySkill -> true
        | WithTag t -> Set.contains t skill.Tags
        | Called n -> skill.Name = n

    // ---- time passing ----

    /// Advance the clock to t, applying damage over time and recording uptime.
    /// DoT dps is constant between events (every change is an event), so this is exact.
    let private advance env t (w: World) =
        let dt = t - w.Time
        if dt <= 0.0 || w.Dead.IsSome then
            w
        else
            let def = defender env w
            let bleedMoving = if env.Fight.Enemy.Moving then Constants.bleedMovingMultiplier else 1.0
            let parts =
                activeDots w
                |> List.map (fun d ->
                    let k = if d.Ailment = Bleed then bleedMoving else 1.0
                    Ailment.name d.Ailment, d.Dps * k * Ailments.dotTakenMult def d.Type)
            let dps = parts |> List.sumBy snd
            let span = if dps > 0.0 && w.Life <= dps * dt then w.Life / dps else dt
            let damageBy = parts |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageBy
            let uptime = enemyStatus env w |> Map.fold (fun m k _ -> addf k span m) w.Uptime
            let life = w.Life - dps * span
            { w with
                Time = w.Time + span
                Life = life
                DamageBy = damageBy
                Uptime = uptime
                Dead = if life <= 1e-9 then Some(w.Time + span) else None }

    let private prune (w: World) =
        let alive (e: Effect) = e.Expires > w.Time
        { w with
            Debuffs = w.Debuffs |> Map.filter (fun _ e -> alive e)
            Dots = w.Dots |> List.filter (fun d -> d.Expires > w.Time)
            Actors =
                w.Actors
                |> Map.map (fun _ st -> { st with Buffs = st.Buffs |> Map.filter (fun _ e -> alive e) }) }

    // ---- effects ----

    let private stackEffect now (existing: Effect option) name duration maxStacks mods =
        let stacks =
            match existing with
            | Some e when e.Expires > now -> min maxStacks (e.Stacks + 1)
            | _ -> 1
        { Name = name
          Mods = mods
          Stacks = stacks
          Expires = now + duration }

    let private updateActor name f (w: World) =
        { w with Actors = Map.add name (f (Map.find name w.Actors)) w.Actors }

    let private addDebuff name duration maxStacks mods (w: World) =
        let e = stackEffect w.Time (Map.tryFind name w.Debuffs) name duration maxStacks mods
        { w with Debuffs = Map.add name e w.Debuffs } |> schedule e.Expires Expire

    let private inflict owner (i: Inflict) (w: World) =
        match i with
        | Dot (a, t, dps, duration, maxStacks) ->
            let d =
                { Ailment = a
                  Type = t
                  Dps = dps
                  Expires = w.Time + duration
                  MaxStacks = maxStacks
                  Owner = owner }
            { w with Dots = d :: w.Dots } |> schedule d.Expires Expire
        | Status (Shock, magnitude, duration) ->
            // Shocks don't stack: keep the strongest.
            let current =
                Map.tryFind "Shocked" w.Debuffs
                |> Option.filter (fun e -> e.Expires > w.Time)
                |> Option.map (fun e -> e.Mods |> List.sumBy (fun m -> m.Value))
                |> Option.defaultValue 0.0
            if magnitude >= current then
                addDebuff "Shocked" duration 1 [ Mod.inc (DamageTaken AllDamage) magnitude |> Mod.from "Shock" ] w
            else
                w
        | Status (a, _, duration) -> addDebuff (Ailment.status a) duration 1 [] w
        | AddBuildup (a, pct) ->
            let total = (Map.tryFind a w.Buildup |> Option.defaultValue 0.0) + pct
            if total >= 100.0 && not (Map.containsKey (Ailment.status a) w.Debuffs) then
                { w with Buildup = Map.add a 0.0 w.Buildup }
                |> addDebuff (Ailment.status a) (Constants.buildupDuration a) 1 []
                |> fun w -> { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
            else
                { w with Buildup = Map.add a (min total 100.0) w.Buildup }

    // ---- using skills ----

    let private skillOf (actor: Actor) name =
        actor.Skills |> List.find (fun s -> s.Name = name)

    let private ready (st: ActorState) now (skill: Skill) =
        Map.tryFind skill.Name st.Cooldowns |> Option.forall (fun t -> t <= now)

    let rec private react env (rng: Rng) (actor: Actor) hook (skill: Skill) (w: World) =
        actor.Rules
        |> List.filter (fun r -> r.Hook = hook && matches r.Filter skill)
        |> List.fold
            (fun w r ->
                if not (rng.Chance r.Chance) then
                    w
                else
                    match r.Reaction with
                    | GrantBuff (name, duration, maxStacks, mods) ->
                        let w =
                            updateActor actor.Name (fun st ->
                                let e = stackEffect w.Time (Map.tryFind name st.Buffs) name duration maxStacks mods
                                { st with Buffs = Map.add name e st.Buffs }) w
                        schedule (w.Time + duration) Expire w
                    | RemoveBuff name ->
                        updateActor actor.Name (fun st -> { st with Buffs = Map.remove name st.Buffs }) w
                    | ApplyDebuff (name, duration, maxStacks, mods) -> addDebuff name duration maxStacks mods w
                    | TriggerSkill name -> trigger env rng actor name w
                    | GainEnergy (meta, amount) -> gainEnergy env rng actor meta amount w)
            w

    and private gainEnergy env rng actor metaName amount w =
        match actor.Metas |> List.tryFind (fun m -> m.Name = metaName) with
        | None -> w
        | Some meta ->
            let st = Map.find actor.Name w.Actors
            let energy = (Map.tryFind meta.Name st.Energy |> Option.defaultValue 0.0) + amount
            let triggered = energy >= meta.MaxEnergy && ready st w.Time (skillOf actor meta.Triggers)
            let left = if triggered then energy - meta.MaxEnergy else min energy meta.MaxEnergy
            let w = updateActor actor.Name (fun st -> { st with Energy = Map.add meta.Name left st.Energy }) w
            if triggered then trigger env rng actor meta.Triggers w else w

    /// Start a use of a skill at the current time. Returns the world and the use time.
    and private startUse env rng (actor: Actor) (skill: Skill) (useTime: float) (w: World) =
        let st = Map.find actor.Name w.Actors
        let ctx = Skill.ctx skill false (buffStacks st) (enemyStatus env w)
        let mods = skillDb env actor skill st
        let w =
            match skill.Cooldown with
            | Some cd ->
                let recovery = Mods.mult ctx [ CooldownRecovery ] mods
                updateActor actor.Name (fun st -> { st with Cooldowns = Map.add skill.Name (w.Time + cd / recovery) st.Cooldowns }) w
            | None -> w
        let pattern =
            Map.tryFind skill.Name actor.HitPattern
            |> Option.defaultValue (skill.Hits |> List.map (fun (f, m) -> DuringUse f, m))
        let w = { w with Counts = bump ("uses:" + skill.Name) w.Counts }
        let w =
            pattern
            |> List.fold
                (fun w (timing, mult) ->
                    let at =
                        match timing with
                        | DuringUse f -> w.Time + f * useTime
                        | After s -> w.Time + s
                    schedule at (Strike(actor.Name, skill.Name, mult)) w)
                w
        react env rng actor OnUse skill w

    and private trigger env rng (actor: Actor) name (w: World) =
        let skill = skillOf actor name
        let st = Map.find actor.Name w.Actors
        if ready st w.Time skill then
            { w with Counts = bump ("trigger:" + name) w.Counts } |> startUse env rng actor skill 0.0
        else
            w

    let private useTime env (actor: Actor) (skill: Skill) (w: World) =
        let st = Map.find actor.Name w.Actors
        let ctx = Skill.ctx skill false (buffStacks st) (enemyStatus env w)
        skill.BaseTime / Mods.mult ctx [ Speed ] (skillDb env actor skill st)

    let private onReady env rng name (w: World) =
        let actor = actorDef env name
        let st = Map.find name w.Actors
        let v = view env w st
        let choice =
            actor.Rotation
            |> List.tryFind (fun step -> step.When v && ready st w.Time (skillOf actor step.Use))
        match choice with
        | Some step ->
            let skill = skillOf actor step.Use
            let t = useTime env actor skill w
            startUse env rng actor skill t w |> schedule (w.Time + t) (Ready name)
        | None ->
            // Nothing usable: wait for the next cooldown, or a server tick.
            let next =
                st.Cooldowns |> Map.toList |> List.map snd |> List.filter (fun t -> t > w.Time)
                |> function
                    | [] -> w.Time + Constants.serverTick
                    | ts -> List.min ts
            schedule next (Ready name) w

    let private onStrike env (rng: Rng) name skillName mult (w: World) =
        let actor = actorDef env name
        let st = Map.find name w.Actors
        let skill = skillOf actor skillName
        let mods = skillDb env actor skill st
        let ctx = Skill.ctx skill false (buffStacks st) (enemyStatus env w)
        let hitChance = Mods.override' ctx HitChance mods |> Option.defaultValue 100.0
        if not (rng.Chance hitChance) then
            { w with Counts = bump ("miss:" + skillName) w.Counts }
        else
            let def = defender env w
            let hit = Hit.roll rng ctx mods skill def
            let hit =
                { hit with
                    Unmitigated = Dmg.map (fun _ v -> v * mult) hit.Unmitigated
                    Dealt = Dmg.map (fun _ v -> v * mult) hit.Dealt }
            let dealt = Dmg.total hit.Dealt
            let w =
                { w with
                    Life = w.Life - dealt
                    DamageBy = addf (skillName + " (hit)") dealt w.DamageBy
                    Counts = w.Counts |> bump ("hits:" + skillName) |> (if hit.Crit then bump ("crits:" + skillName) else id) }
            let w = if w.Life <= 1e-9 then { w with Dead = Some w.Time } else w
            let inflicts = Ailments.fromHit rng ctx mods def hit
            let w =
                inflicts
                |> List.fold
                    (fun w i ->
                        let w = inflict name i w
                        match i with
                        | Dot (a, _, _, _, _) | Status (a, _, _) ->
                            react env rng actor (OnInflict a) skill { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
                        | AddBuildup _ -> w)
                    w
            let w = react env rng actor OnHit skill w
            if hit.Crit then react env rng actor OnCrit skill w else w

    let private step env rng (w: World) =
        let (KeyValue ((t, sq), ev)) = Seq.head w.Queue
        let w = { w with Queue = Map.remove (t, sq) w.Queue } |> advance env t
        if w.Dead.IsSome then
            w
        else
            let w = prune w
            match ev with
            | Ready name -> onReady env rng name w
            | Strike (name, skill, mult) -> onStrike env rng name skill mult w
            | Expire -> w

    let private envOf (fight: Fight) =
        { Fight = fight
          SkillDbs =
            Map.ofList
                [ for a in fight.Player :: fight.Minions do
                      for s in a.Skills -> (a.Name, s.Name), ModDb.ofList (a.Mods @ s.Mods) ]
          EnemyDb = ModDb.ofList fight.Enemy.Mods }

    /// Simulate one fight with the given seed.
    let run (fight: Fight) (seed: uint32) : Outcome =
        let env = envOf fight
        let rng = Rng(seed)
        let actors = env.Fight.Player :: env.Fight.Minions
        let empty =
            { Buffs = Map.empty
              Cooldowns = Map.empty
              Energy = Map.empty }
        let w0 =
            { Time = 0.0
              Life = env.Fight.Enemy.Life
              Debuffs = Map.empty
              Dots = []
              Buildup = Map.empty
              Actors = actors |> List.map (fun a -> a.Name, empty) |> Map.ofList
              Queue = Map.empty
              Seq = 0
              DamageBy = Map.empty
              Counts = Map.empty
              Uptime = Map.empty
              Dead = None }
            |> fun w -> actors |> List.fold (fun w a -> schedule 0.0 (Ready a.Name) w) w
            |> schedule env.Fight.MaxTime Expire
        let rec loop (w: World) =
            if w.Dead.IsSome || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > env.Fight.MaxTime then w
            else loop (step env rng w)
        let w = loop w0
        { Ttk = w.Dead
          Duration = (match w.Dead with Some t -> t | None -> min w.Time env.Fight.MaxTime)
          DamageBy = w.DamageBy
          Counts = w.Counts
          Uptime = w.Uptime }
