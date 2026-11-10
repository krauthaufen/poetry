namespace PoEsie

/// When a hit of a skill use lands.
type HitTiming =
    /// Fraction of the use time (0 = start, 1 = end). Scales with speed.
    | DuringUse of fraction: float
    /// Fixed seconds after the use starts (delayed explosions, ground effects...).
    | After of seconds: float

/// What the engine can see when deciding which skill to use next. Enemy fields
/// describe the enemy currently aimed at.
type View =
    { Time: float
      Buffs: Map<string, int>
      EnemyStatus: Map<string, int>
      EnemyLifeFraction: float
      EnemyRarity: Rarity
      EnemiesAlive: int
      Energy: Map<string, float> }

type SkillFilter =
    | AnySkill
    | WithTag of SkillTag
    | Called of string

type Hook =
    | OnUse
    | OnHit
    | OnCrit
    | OnKill
    | OnInflict of Ailment

type Reaction =
    /// Buff on the actor that owns the rule (charges are buffs too).
    | GrantBuff of name: string * duration: float * maxStacks: int * mods: Mod list
    /// Debuff on the enemy that was hit (on use: the enemy aimed at). Mods are per stack, defender side.
    | ApplyDebuff of name: string * duration: float * maxStacks: int * mods: Mod list
    /// A curse: one of each name, its mods scaled by the enemy's curse effect.
    | ApplyCurse of name: string * duration: float * mods: Mod list
    /// Exposure: lowers one resistance, strongest applies, scaled by exposure effect.
    | ApplyExposure of damageType: DamageType * amount: float * duration: float
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
      /// Timed hit pattern per skill name; default is the skill's own Hits.
      HitPattern: Map<string, (HitTiming * float) list>
      /// Priority list: the first usable step is used.
      Rotation: Step list
      Rules: Rule list
      Metas: Meta list
      /// Charges, rage...: stacks the actor starts the fight with. They don't expire;
      /// skills that consume them remove them.
      Resources: (string * int) list }

module Actor =
    let make name mods skills =
        { Name = name
          Mods = mods
          Skills = skills
          HitPattern = Map.empty
          Rotation = skills |> List.map (fun s -> { Use = s.Name; When = fun _ -> true })
          Rules = []
          Metas = []
          Resources = [] }

    let always skill = { Use = skill; When = fun _ -> true }

type Fight =
    { Enemies: EnemyDef list
      Player: Actor
      Minions: Actor list
      Targeting: Targeting
      MaxTime: float }

module Fight =
    let single enemy player maxTime =
        { Enemies = [ enemy ]
          Player = player
          Minions = []
          Targeting = Strongest
          MaxTime = maxTime }

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
    { /// Time every enemy was dead, None if some survived MaxTime.
      Ttk: float option
      Duration: float
      /// Kill time per enemy (name, rarity, time).
      Kills: (string * Rarity * float) list
      /// Damage dealt by source: "Skill (hit)", "Ignite", ...
      DamageBy: Map<string, float>
      /// Event counts: "uses:Skill", "hits:Skill", "crits:Skill", "inflict:Ignite", "trigger:Skill"...
      Counts: Map<string, int>
      /// Seconds each status was active, averaged over the enemies alive at the time.
      Uptime: Map<string, float> }

module Sim =
    type private Event =
        | Ready of actor: string
        | Strike of actor: string * skill: string * mult: float * aimedAt: string
        /// After a use's hits: the skill consumes its charges (unless the skip roll succeeds).
        | ConsumeCharges of actor: string * charge: string * skill: string
        | Expire

    type private ActorState =
        { Buffs: Map<string, Effect>
          Cooldowns: Map<string, float>
          Energy: Map<string, float> }

    type private EnemyState =
        { Def: EnemyDef
          Life: float
          Debuffs: Map<string, Effect>
          Dots: DotInstance list
          Buildup: Map<Ailment, float>
          Db: ModDb }

    type private World =
        { Time: float
          Enemies: Map<string, EnemyState>
          Actors: Map<string, ActorState>
          Queue: Map<float * int, Event>
          Seq: int
          DamageBy: Map<string, float>
          Counts: Map<string, int>
          Uptime: Map<string, float>
          Kills: (string * Rarity * float) list }

    /// Fixed for the whole fight: mod dbs built once, buffs layered on per event.
    type private Env =
        { Fight: Fight
          SkillDbs: Map<string * string, ModDb> }

    let private allAilments =
        [ Ignite; Bleed; Poison; Shock; Chill; Freeze; Electrocute; HeavyStun; Pin; Daze; Maim; Blind; Hinder ]

    let private bump key (m: Map<string, int>) =
        Map.add key ((Map.tryFind key m |> Option.defaultValue 0) + 1) m

    let private addf key v (m: Map<string, float>) =
        Map.add key ((Map.tryFind key m |> Option.defaultValue 0.0) + v) m

    let private schedule t ev (w: World) =
        { w with
            Queue = Map.add (t, w.Seq) ev w.Queue
            Seq = w.Seq + 1 }

    let private alive (w: World) = w.Enemies |> Map.toList |> List.map snd |> List.filter (fun e -> e.Life > 0.0)
    let private allDead (w: World) = alive w |> List.isEmpty

    // ---- views of an enemy ----

    /// Damaging ailments that actually deal damage: the strongest N of each kind.
    let private activeDots (e: EnemyState) =
        e.Dots
        |> List.groupBy (fun d -> d.Ailment)
        |> List.collect (fun (_, ds) ->
            let limit = ds |> List.map (fun d -> d.MaxStacks) |> List.max
            ds |> List.sortByDescending (fun d -> d.Dps) |> List.truncate limit)

    let private status (e: EnemyState) =
        let fromDebuffs = e.Debuffs |> Map.map (fun _ x -> x.Stacks)
        let withDots =
            activeDots e
            |> List.countBy (fun d -> Ailment.status d.Ailment)
            |> List.fold (fun m (k, n) -> Map.add k n m) fromDebuffs
        let ailing = withDots |> Map.exists (fun k _ -> allAilments |> List.exists (fun a -> Ailment.status a = k))
        withDots
        |> fun m -> if e.Def.Moving then Map.add "Moving" 1 m else m
        |> fun m -> if ailing then Map.add "AnyAilment" 1 m else m
        |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Curse:") then Map.add "Cursed" 1 m else m

    let private defender (e: EnemyState) : Defender =
        let debuffMods =
            e.Debuffs
            |> Map.toList
            |> List.collect (fun (_, x) -> x.Mods |> List.map (Mod.scaled (float x.Stacks)))
        { Level = e.Def.Level
          Mods = ModDb.addList debuffMods e.Db }

    let private actorDef (env: Env) name =
        if name = env.Fight.Player.Name then env.Fight.Player
        else env.Fight.Minions |> List.find (fun m -> m.Name = name)

    let private buffStacks (st: ActorState) = st.Buffs |> Map.map (fun _ e -> e.Stacks)

    /// The actor's mods while using a skill: sheet + skill mods + active buffs.
    let private skillDb env (actor: Actor) (skill: Skill) (st: ActorState) =
        let buffMods = st.Buffs |> Map.toList |> List.collect (fun (_, e) -> e.Mods |> List.map (Mod.scaled (float e.Stacks)))
        let db = ModDb.addList buffMods (Map.find (actor.Name, skill.Name) env.SkillDbs)
        // Rage: 1% more attack damage per Rage, times rage effect, rounded down.
        match Map.tryFind "Rage" st.Buffs with
        | Some r when r.Stacks > 0 ->
            let effect = Mods.mult Ctx.empty [ Named "RageEffect" ] db
            let more = floor (float r.Stacks * effect) * Constants.moreAttackDamagePerRage
            ModDb.add db (Mod.more (Damage AllDamage) more |> Mod.whenever (SkillIs Attack) |> Mod.from "Rage")
        | _ -> db

    let private aimed (env: Env) (w: World) =
        Geometry.aim env.Fight.Targeting (alive w |> List.map (fun e -> e.Def))
        |> Option.map (fun d -> w.Enemies.[d.Name])

    let private view env (w: World) (st: ActorState) =
        let target = aimed env w
        { Time = w.Time
          Buffs = buffStacks st
          EnemyStatus = target |> Option.map status |> Option.defaultValue Map.empty
          EnemyLifeFraction = target |> Option.map (fun e -> e.Life / e.Def.Life) |> Option.defaultValue 0.0
          EnemyRarity = target |> Option.map (fun e -> e.Def.Rarity) |> Option.defaultValue Normal
          EnemiesAlive = (alive w).Length
          Energy = st.Energy }

    let private matches filter (skill: Skill) =
        match filter with
        | AnySkill -> true
        | WithTag t -> Set.contains t skill.Tags
        | Called n -> skill.Name = n

    // ---- time passing ----

    /// Advance the clock to t, applying damage over time and recording uptime.
    /// DoT dps is constant between events (every change is an event), so this is
    /// exact; an enemy dying from DoT mid-interval dies at the exact time.
    let private advance t (w: World) =
        let dt = t - w.Time
        if dt <= 0.0 then
            w
        else
            let living = alive w
            let n = float living.Length
            let step (w: World) (e: EnemyState) =
                let def = defender e
                let bleedK = if e.Def.Moving then Constants.bleedMovingMultiplier else 1.0
                let parts =
                    activeDots e
                    |> List.map (fun d ->
                        let k = if d.Ailment = Bleed then bleedK else 1.0
                        Ailment.name d.Ailment, d.Dps * k * Ailments.dotTakenMult def d.Type)
                let dps = parts |> List.sumBy snd
                let span = if dps > 0.0 && e.Life <= dps * dt then e.Life / dps else dt
                let life = e.Life - dps * span
                let dead = life <= 1e-9
                { w with
                    Enemies = Map.add e.Def.Name { e with Life = (if dead then 0.0 else life) } w.Enemies
                    DamageBy = parts |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageBy
                    Uptime = status e |> Map.fold (fun m k _ -> addf k (span / n) m) w.Uptime
                    Kills = if dead then (e.Def.Name, e.Def.Rarity, w.Time + span) :: w.Kills else w.Kills }
            let w = living |> List.fold step w
            { w with Time = t }

    let private prune (w: World) =
        let live (x: Effect) = x.Expires > w.Time
        { w with
            Enemies =
                w.Enemies
                |> Map.map (fun _ e ->
                    { e with
                        Debuffs = e.Debuffs |> Map.filter (fun _ x -> live x)
                        Dots = e.Dots |> List.filter (fun d -> d.Expires > w.Time) })
            Actors = w.Actors |> Map.map (fun _ st -> { st with Buffs = st.Buffs |> Map.filter (fun _ x -> live x) }) }

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

    let private updateEnemy name f (w: World) =
        { w with Enemies = Map.add name (f (Map.find name w.Enemies)) w.Enemies }

    let private addDebuff target name duration maxStacks mods (w: World) =
        let w =
            updateEnemy target (fun e ->
                let x = stackEffect w.Time (Map.tryFind name e.Debuffs) name duration maxStacks mods
                { e with Debuffs = Map.add name x e.Debuffs }) w
        schedule (w.Time + duration) Expire w

    /// Effect multiplier of a curse / exposure on this enemy (bosses: 50% less).
    let private effectOnSelf (e: EnemyState) stat =
        Mods.more Ctx.empty [ Named stat ] e.Db

    let private inflict target owner (i: Inflict) (w: World) =
        match i with
        | Dot (a, t, dps, duration, maxStacks) ->
            let d =
                { Ailment = a
                  Type = t
                  Dps = dps
                  Expires = w.Time + duration
                  MaxStacks = maxStacks
                  Owner = owner }
            updateEnemy target (fun e -> { e with Dots = d :: e.Dots }) w |> schedule d.Expires Expire
        | Status (Shock, magnitude, duration) ->
            // Shocks don't stack: keep the strongest.
            let e = w.Enemies.[target]
            let current =
                Map.tryFind "Shocked" e.Debuffs
                |> Option.filter (fun x -> x.Expires > w.Time)
                |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value))
                |> Option.defaultValue 0.0
            if magnitude >= current then
                addDebuff target "Shocked" duration 1 [ Mod.inc (DamageTaken AllDamage) magnitude |> Mod.from "Shock" ] w
            else
                w
        | Status (a, _, duration) -> addDebuff target (Ailment.status a) duration 1 [] w
        | AddBuildup (a, pct) ->
            let e = w.Enemies.[target]
            let total = (Map.tryFind a e.Buildup |> Option.defaultValue 0.0) + pct
            if total >= 100.0 && not (Map.containsKey (Ailment.status a) e.Debuffs) then
                updateEnemy target (fun e -> { e with Buildup = Map.add a 0.0 e.Buildup }) w
                |> addDebuff target (Ailment.status a) (Constants.buildupDuration a) 1 []
                |> fun w -> { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
            else
                updateEnemy target (fun e -> { e with Buildup = Map.add a (min total 100.0) e.Buildup }) w

    // ---- using skills ----

    let private allSkills (actor: Actor) = actor.Skills @ (actor.Skills |> List.collect (fun s -> s.Parts))

    let private skillOf (actor: Actor) name =
        allSkills actor |> List.find (fun s -> s.Name = name)

    let private ready (st: ActorState) now (skill: Skill) =
        Map.tryFind skill.Name st.Cooldowns |> Option.forall (fun t -> t <= now)

    /// `target`: the enemy the triggering event concerns (hit enemy, or the one aimed at).
    let rec private react env (rng: Rng) (actor: Actor) hook (skill: Skill) (target: string option) (w: World) =
        actor.Rules
        |> List.filter (fun r -> r.Hook = hook && matches r.Filter skill)
        |> List.fold
            (fun w r ->
                if not (rng.Chance r.Chance) then
                    w
                else
                    let onTarget f =
                        match target with
                        | Some t when w.Enemies.[t].Life > 0.0 -> f t
                        | _ -> w
                    match r.Reaction with
                    | GrantBuff (name, duration, maxStacks, mods) ->
                        let w =
                            updateActor actor.Name (fun st ->
                                let e = stackEffect w.Time (Map.tryFind name st.Buffs) name duration maxStacks mods
                                { st with Buffs = Map.add name e st.Buffs }) w
                        schedule (w.Time + duration) Expire w
                    | RemoveBuff name ->
                        updateActor actor.Name (fun st -> { st with Buffs = Map.remove name st.Buffs }) w
                    | ApplyDebuff (name, duration, maxStacks, mods) ->
                        onTarget (fun t -> addDebuff t name duration maxStacks mods w)
                    | ApplyCurse (name, duration, mods) ->
                        onTarget (fun t ->
                            let k = effectOnSelf w.Enemies.[t] "CurseEffectOnSelf"
                            addDebuff t ("Curse:" + name) duration 1 (mods |> List.map (Mod.scaled k)) w)
                    | ApplyExposure (dt, amount, duration) ->
                        onTarget (fun t ->
                            let k = effectOnSelf w.Enemies.[t] "ExposureEffectOnSelf"
                            let name = "Exposure:" + DamageType.name dt
                            let current =
                                Map.tryFind name w.Enemies.[t].Debuffs
                                |> Option.filter (fun x -> x.Expires > w.Time)
                                |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> -m.Value))
                                |> Option.defaultValue 0.0
                            if amount * k >= current then addDebuff t name duration 1 [ Mod.base' (Resist dt) (-amount * k) ] w
                            else w)
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

    /// Start a use of a skill at the current time, aimed at the current target.
    and private startUse env rng (actor: Actor) (skill: Skill) (useTime: float) (w: World) =
        match aimed env w with
        | None -> w
        | Some target ->
            let st = Map.find actor.Name w.Actors
            let ctx = Skill.ctx skill false (buffStacks st) (status target)
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
            let times =
                pattern |> List.map (fun (timing, mult) ->
                    (match timing with
                     | DuringUse f -> w.Time + f * useTime
                     | After s -> w.Time + s), mult)
            let w =
                times
                |> List.fold
                    (fun w (at, mult) ->
                        // every part of the skill strikes together
                        (skill :: skill.Parts)
                        |> List.fold (fun w part -> schedule at (Strike(actor.Name, part.Name, mult, target.Def.Name)) w) w)
                    w
            let w =
                match skill.Consumes with
                | Some charge ->
                    let last = times |> List.map fst |> List.fold max w.Time
                    schedule (last + 1e-9) (ConsumeCharges(actor.Name, charge, skill.Name)) w
                | None -> w
            react env rng actor OnUse skill (Some target.Def.Name) w

    and private trigger env rng (actor: Actor) name (w: World) =
        let skill = skillOf actor name
        let st = Map.find actor.Name w.Actors
        if ready st w.Time skill then
            { w with Counts = bump ("trigger:" + name) w.Counts } |> startUse env rng actor skill 0.0
        else
            w

    let private useTime env (actor: Actor) (skill: Skill) (w: World) =
        let st = Map.find actor.Name w.Actors
        let target = aimed env w |> Option.map status |> Option.defaultValue Map.empty
        let ctx = Skill.ctx skill false (buffStacks st) target
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

    /// Statuses the game tracks for "recently" conditions: (name, seconds).
    let private recently hit crit killed =
        [ if hit then yield "HitRecently", 4.0
          if crit then
              yield "CritRecently", 4.0
              yield "CritInPast8Seconds", 8.0
          if killed then yield "KilledRecently", 4.0 ]

    let private markRecently (actor: string) hit crit killed (w: World) =
        recently hit crit killed
        |> List.fold
            (fun w (name, secs) ->
                updateActor actor (fun st -> { st with Buffs = Map.add name { Name = name; Mods = []; Stacks = 1; Expires = w.Time + secs } st.Buffs }) w
                |> schedule (w.Time + secs) Expire)
            w

    /// One hit on one enemy.
    let private hitEnemy env (rng: Rng) (actor: Actor) (skill: Skill) mult (target: string) (w: World) =
        let e = w.Enemies.[target]
        if e.Life <= 0.0 then
            w
        else
            let st = Map.find actor.Name w.Actors
            let mods = skillDb env actor skill st
            let ctx = Skill.ctx skill false (buffStacks st) (status e)
            let hitChance = Mods.override' ctx HitChance mods |> Option.defaultValue 100.0
            if not (rng.Chance hitChance) then
                { w with Counts = bump ("miss:" + skill.Name) w.Counts }
            else
                let def = defender e
                let hit = Hit.roll rng ctx mods skill def
                let hit =
                    { hit with
                        Unmitigated = Dmg.map (fun _ v -> v * mult) hit.Unmitigated
                        Dealt = Dmg.map (fun _ v -> v * mult) hit.Dealt }
                let dealt = Dmg.total hit.Dealt
                let life = e.Life - dealt
                let dead = life <= 1e-9
                let w =
                    { w with
                        Enemies = Map.add target { e with Life = (if dead then 0.0 else life) } w.Enemies
                        DamageBy = addf (skill.Name + " (hit)") (min dealt e.Life) w.DamageBy
                        Counts = w.Counts |> bump ("hits:" + skill.Name) |> (if hit.Crit then bump ("crits:" + skill.Name) else id)
                        Kills = if dead then (target, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
                let w =
                    if dead then
                        react env rng actor OnKill skill None w
                    else
                        Ailments.fromHit rng ctx mods def hit
                        |> List.fold
                            (fun w i ->
                                let w = inflict target actor.Name i w
                                match i with
                                | Dot (a, _, _, _, _) | Status (a, _, _) ->
                                    react env rng actor (OnInflict a) skill (Some target) { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
                                | AddBuildup _ -> w)
                            w
                let w = markRecently actor.Name true hit.Crit dead w
                let w = react env rng actor OnHit skill (Some target) w
                if hit.Crit then react env rng actor OnCrit skill (Some target) w else w

    let private onStrike env (rng: Rng) name skillName mult aimedAt (w: World) =
        let actor = actorDef env name
        let skill = skillOf actor skillName
        let living = alive w |> List.map (fun e -> e.Def)
        // Aim where the use was aimed; if that enemy died meanwhile, at the next one.
        let primary =
            living |> List.tryFind (fun e -> e.Name = aimedAt)
            |> Option.orElse (Geometry.aim env.Fight.Targeting living)
        match primary with
        | None -> w
        | Some primary ->
            let st = Map.find name w.Actors
            let ctx = Skill.ctx skill false (buffStacks st) Map.empty
            let mods = skillDb env actor skill st
            let areaMult = Mods.mult ctx [ AreaOfEffect ] mods
            let delivery =
                match skill.Delivery with
                | Projectiles p ->
                    let fromCharges =
                        match p.PerCharge with
                        | Some (charge, per) -> per * (Map.tryFind charge st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0)
                        | None -> 0
                    Projectiles
                        { p with
                            Count = p.Count + fromCharges + (if p.Count + fromCharges > 0 then int (Mods.sumBase ctx [ AdditionalProjectiles ] mods) else 0)
                            Pierce = p.Pierce + int (Mods.sumBase ctx [ Pierce ] mods)
                            Chains = p.Chains + int (Mods.sumBase ctx [ Chains ] mods) }
                | d -> d
            Geometry.targets delivery areaMult primary living
            |> List.fold (fun w target -> hitEnemy env rng actor skill mult target w) w

    let private step env rng (w: World) =
        let (KeyValue ((t, sq), ev)) = Seq.head w.Queue
        let w = { w with Queue = Map.remove (t, sq) w.Queue } |> advance t
        if allDead w then
            w
        else
            let w = prune w
            match ev with
            | Ready name -> onReady env rng name w
            | Strike (name, skill, mult, aimedAt) -> onStrike env rng name skill mult aimedAt w
            | ConsumeCharges (name, charge, skillName) ->
                let actor = actorDef env name
                let st = Map.find name w.Actors
                // character + skill (gem quality, supports): the skill's own db
                let skip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] (skillDb env actor (skillOf actor skillName) st)
                if not (Map.containsKey charge st.Buffs) then w
                else
                    // "still count as consuming": the consumed-recently status starts either way
                    let recent = "Consumed" + charge + "Recently"
                    let w =
                        updateActor name (fun st -> { st with Buffs = Map.add recent { Name = recent; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
                        |> schedule (w.Time + 4.0) Expire
                    if rng.Chance skip then { w with Counts = bump ("kept:" + charge) w.Counts }
                    else
                        updateActor name (fun st -> { st with Buffs = Map.remove charge st.Buffs }) w
                        |> fun w -> { w with Counts = bump ("consumed:" + charge) w.Counts }
            | Expire -> w

    let private envOf (fight: Fight) =
        { Fight = fight
          SkillDbs =
            Map.ofList
                [ for a in fight.Player :: fight.Minions do
                      for s in allSkills a -> (a.Name, s.Name), ModDb.ofList (a.Mods @ s.Mods) ] }

    /// Simulate one fight with the given seed.
    let run (fight: Fight) (seed: uint32) : Outcome =
        let env = envOf fight
        let rng = Rng(seed)
        let actors = fight.Player :: fight.Minions
        let empty =
            { Buffs = Map.empty
              Cooldowns = Map.empty
              Energy = Map.empty }
        let w0 =
            { Time = 0.0
              Enemies =
                fight.Enemies
                |> List.map (fun d ->
                    d.Name,
                    { Def = d
                      Life = d.Life
                      Debuffs = Map.empty
                      Dots = []
                      Buildup = Map.empty
                      Db = ModDb.ofList d.Mods })
                |> Map.ofList
              Actors =
                actors
                |> List.map (fun a ->
                    let res =
                        a.Resources
                        |> List.filter (fun (_, n) -> n > 0)
                        |> List.map (fun (k, n) -> k, { Name = k; Mods = []; Stacks = n; Expires = infinity })
                        |> Map.ofList
                    a.Name, { empty with Buffs = res })
                |> Map.ofList
              Queue = Map.empty
              Seq = 0
              DamageBy = Map.empty
              Counts = Map.empty
              Uptime = Map.empty
              Kills = [] }
            |> fun w -> actors |> List.fold (fun w a -> schedule 0.0 (Ready a.Name) w) w
            |> schedule fight.MaxTime Expire
        let rec loop (w: World) =
            if allDead w || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > fight.MaxTime then w
            else loop (step env rng w)
        let w = loop w0
        let cleared = if allDead w then w.Kills |> List.map (fun (_, _, t) -> t) |> List.fold max 0.0 |> Some else None
        { Ttk = cleared
          Duration = (match cleared with Some t -> t | None -> min w.Time fight.MaxTime)
          Kills = List.rev w.Kills
          DamageBy = w.DamageBy
          Counts = w.Counts
          Uptime = w.Uptime }
