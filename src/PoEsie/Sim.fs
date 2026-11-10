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
      Energy: Map<string, float>
      /// The skill the actor used last (for cycling rotations).
      LastUsed: string }

type SkillFilter =
    | AnySkill
    | WithTag of SkillTag
    | Called of string

type Hook =
    | OnBlock
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
    /// Stacks of a resource (infusions from Powered by Verisium), capped at its maximum.
    | GainResource of resource: string * count: int
    /// A remnant of the resource that arrives after `delay` (detonation + pickup);
    /// `extraChance` % for two.
    | DropRemnant of resource: string * delay: float * extraChance: float

type Rule =
    { Hook: Hook
      Filter: SkillFilter
      /// Percent.
      Chance: float
      Reaction: Reaction }

/// A meta gem: collects energy from rules, triggers its socketed skill when full.
/// Where a meta gem's energy comes from. Amounts are energy per monster power, scaled by
/// how much of the enemy's ailment threshold the hit (or ailment) deals.
type EnergySource =
    | CritHits of perPower: float
    | InflictsAilment of ailment: Ailment * perPower: float
    /// Flat energy per block.
    | Blocks of energy: float
    /// Energy per 10 ms of base cast time of spells the player casts.
    | CastsSpells of perTenMs: float

/// "Cast on ..." gems trigger on reaching maximum energy; Invocations bank energy and are
/// used to spend it, possibly several triggers at once.
type MetaRelease =
    | AtMaximum
    | Invocation of cooldown: float

/// A meta gem: gains energy, triggers its socketed spells.
type Meta =
    { Name: string
      Release: MetaRelease
      MaxEnergy: float
      /// Energy one trigger of all socketed spells costs.
      Cost: float
      Triggers: string list
      Sources: EnergySource list
      /// Increased energy generated (gem + supports), as a multiplier.
      EnergyMult: float
      /// Chance (%) to get half the spent energy back (Energy Retention).
      RefundChance: float }

module Meta =
    /// A simple meta: flat energy from rules, triggers one skill at maximum.
    let simple name maxEnergy triggers =
        { Name = name
          Release = AtMaximum
          MaxEnergy = maxEnergy
          Cost = maxEnergy
          Triggers = [ triggers ]
          Sources = []
          EnergyMult = 1.0
          RefundChance = 0.0 }

type Step =
    { Use: string
      When: View -> bool }

/// Runic Ward Remnants (Remnants of Kalguur): kills and stuns can create remnants that grant
/// ward when collected, overflowing the maximum.
type WardRemnants =
    { OnKillChance: float
      OnStunChance: float
      StunCooldown: float
      WardPerRemnant: float
      ExtraChance: float
      /// Seconds from creation until the ward arrives (collecting + effect delay).
      Delay: float
      /// Ward can overflow to this multiple of its maximum.
      Overflow: float }

/// An Archon buff: gained by a trigger (ignite with a skill that has the chance), lasts its
/// duration, then a recovery period in which no Archon can be gained.
type Archon =
    { Name: string
      /// Stat on the skill (or character) that gives the chance on ignite.
      OnIgniteStat: string
      Duration: float
      Recovery: float
      Mods: Mod list }

/// Infusions: per-element resources that skills consume for their "-Infused" stat set.
type Infusions =
    { /// Remnants: inflicting the ailment has this % chance to drop one of the infusion.
      Remnants: (Ailment * string * float) list
      /// Chance (%) a remnant comes as two.
      ExtraRemnantChance: float
      /// Seconds from the remnant dropping to the infusion arriving (pickup).
      Delay: float
      /// Skills that consume an infusion: skill name -> (infusion, name of the infused copy).
      /// Each impact (Spell Cascade: every impact) consumes one.
      Consumers: Map<string, string * string>
      /// Used in place of any element when the element's own is missing ("VerisiumInfusion").
      Wildcard: string option }

/// Something that happens on its own every few seconds.
type PeriodicAction =
    /// One stack of a random resource that isn't at its maximum (Thaumaturgical Dynamism,
    /// Virtuous Barrier's motes).
    | GainRandom of resources: string list
    /// One stack of a uniformly random resource from the list; at its maximum it only
    /// refreshes the duration (charges).
    | GainAnyRandom of resources: string list
    /// Like GainAnyRandom, but picked with these weights.
    | GainWeighted of weights: (string * float) list
    /// One stack of each resource that has any (Charge Regulation). Charge-removal
    /// skip chances apply.
    | ConsumeEach of resources: string list

type Periodic =
    { Name: string
      Every: float
      Action: PeriodicAction }

/// A buff that switches between variants at random every period
/// (Elemental Conflux: more damage of one random element).
type Cycle =
    { Name: string
      Period: float
      Variants: Mod list list }

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
      Resources: (string * int) list
      Cycles: Cycle list
      /// The player's pools and defences. None: the actor can't be hurt and pays no costs.
      Vitals: PlayerStats option
      /// Mitigation from the current buffs and charges (evasion, armour, deflection, resists
      /// can depend on them). None: use Vitals.
      VitalsAt: (Map<string, int> -> PlayerStats) option
      ManaFlask: ManaFlask option
      Periodics: Periodic list
      /// Maximum stacks per resource (charges, rage, motes).
      MaxResources: Map<string, int>
      /// Duration per resource (charges): all stacks of a kind share it, gaining refreshes it,
      /// and they all expire together. Missing: no duration.
      ResourceDurations: Map<string, float>
      /// Consuming charges that are kept ("chance to not remove") pauses their timer for this
      /// many seconds (the timer keeps its current value). 0: no pause.
      KeptChargesPause: float
      WardRemnants: WardRemnants option
      Infusions: Infusions option
      Archons: Archon list }

module Actor =
    let make name mods skills =
        { Name = name
          Mods = mods
          Skills = skills
          HitPattern = Map.empty
          Rotation = skills |> List.map (fun s -> { Use = s.Name; When = fun _ -> true })
          Rules = []
          Metas = []
          Resources = []
          Cycles = []
          Vitals = None
          VitalsAt = None
          ManaFlask = None
          Periodics = []
          MaxResources = Map.empty
          ResourceDurations = Map.empty
          KeptChargesPause = 0.0
          WardRemnants = None
          Infusions = None
          Archons = [] }

    let always skill = { Use = skill; When = fun _ -> true }

type Fight =
    { Enemies: EnemyDef list
      Player: Actor
      Minions: Actor list
      Targeting: Targeting
      /// How far in front of the player ground skills (slams) land, game units.
      Reach: float
      /// Spell Cascade: distance between impacts, as a multiple of the impact radius.
      CascadeSpacing: float
      Walls: Geometry.Wall list
      /// Invocations start with this fraction of their maximum energy (banked while
      /// clearing before the fight; 0 = empty).
      BankedInvocations: float
      MaxTime: float }

module Fight =
    let single enemy player maxTime =
        { Enemies = [ enemy ]
          Player = player
          Minions = []
          Targeting = Strongest
          Reach = infinity
          // Spell Cascade impacts land about 1.5 radii apart (user observation)
          CascadeSpacing = 1.5
          Walls = []
          BankedInvocations = 1.0
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
      Uptime: Map<string, float>
      /// When the player died, if they did.
      Died: float option
      /// Damage the player took (after mitigation).
      DamageTaken: float
      /// Seconds the player waited for mana.
      ManaStarved: float
      /// Lowest life as a fraction of maximum.
      MinLife: float
      /// Mana by source: "regen", "leech", "kill".
      ManaGained: Map<string, float> }

module Sim =
    type private Event =
        | Ready of actor: string
        | Strike of actor: string * skill: string * mult: float * aimedAt: string
        /// After a use's hits: the skill consumes its charges (unless the skip roll succeeds).
        | ConsumeCharges of actor: string * charge: string * skill: string
        /// A cycle picks its next variant.
        | CycleTick of actor: string * cycle: string
        /// A monster hits at the player.
        | EnemyAttack of enemy: string
        | PeriodicTick of actor: string * periodic: string
        /// A ward remnant's ward arrives.
        | WardArrives of amount: float
        /// Stacks of a resource arrive (picked-up infusion remnants, Verisium).
        | GainStacks of actor: string * resource: string * count: int
        | Expire

    type private ActorState =
        { LastUsed: string
          /// Resource timers paused until this time.
          Paused: Map<string, float>
          Buffs: Map<string, Effect>
          Cooldowns: Map<string, float>
          Energy: Map<string, float> }

    type private EnemyState =
        { Def: EnemyDef
          Life: float
          Debuffs: Map<string, Effect>
          Dots: DotInstance list
          Buildup: Map<Ailment, float>
          Db: ModDb }

    type private PlayerState =
        { Life: float
          EnergyShield: float
          Mana: float
          LastHit: float
          /// Mana still to be recovered from leech.
          LeechPool: float
          Died: float option
          Taken: float
          Starved: float
          MinLife: float
          ManaGained: Map<string, float>
          FlaskCharges: float
          /// Flask recovery: running until this time, at this rate.
          FlaskUntil: float
          FlaskRate: float
          /// Largest leech instance of the current hit event (leech counts from one enemy).
          StrikeLeech: float
          Ward: float
          InfernalFlame: float
          LastStunRemnant: float }

    type private World =
        { Player: PlayerState option
          Time: float
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

    let private updateEnemyDef name (pos: Vec) (w: World) =
        { w with Enemies = w.Enemies |> Map.change name (Option.map (fun e -> { e with Def = { e.Def with Position = pos } })) }

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
        |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Exposure:") then Map.add "Exposed" 1 m else m
        |> fun m -> if e.Def.Rarity = Rare || e.Def.Rarity = Unique then Map.add "RareOrUnique" 1 m else m

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

    /// Buff stacks as a skill sees them: a skill that consumes charges counts as consuming
    /// "N additional" ones too ("50% more damage per Power Charge Consumed").
    let private skillStacks (actor: Actor) (skill: Skill) (st: ActorState) =
        let stacks = buffStacks st
        // parts (Falling Thunder's projectiles) count like the skill they belong to
        let consumes =
            skill.Consumes
            |> Option.orElse (actor.Skills |> List.tryPick (fun s -> if s.Parts |> List.exists (fun p -> p.Name = skill.Name) then s.Consumes else None))
        match consumes with
        | Some charge when Map.tryFind charge stacks |> Option.exists (fun n -> n > 0) ->
            let ctx = { Ctx.empty with SkillName = skill.Name; SkillTags = skill.Tags }
            let extra = int (Mods.sumBase ctx [ Named("Extra" + charge + "sConsumed") ] (ModDb.ofList (actor.Mods @ skill.Mods)))
            if extra > 0 then Map.add charge (stacks.[charge] + extra) stacks else stacks
        | _ -> stacks

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
          Energy = st.Energy
          LastUsed = st.LastUsed }

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
            // monsters walk towards the player until they're in attack range
            let move (w: World) (e: EnemyState) =
                match e.Def.Attack with
                | Some atk when e.Life > 0.0 && w.Player.IsSome ->
                    let st = status e
                    let stopped = [ "Frozen"; "HeavilyStunned"; "Electrocuted"; "Pinned" ] |> List.exists st.ContainsKey
                    let slow =
                        (if st.ContainsKey "Maimed" then 1.0 - Constants.maimSlow / 100.0 else 1.0)
                        * (if st.ContainsKey "Hindered" then 1.0 - Constants.hinderSlow / 100.0 else 1.0)
                        * (1.0 - (e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0) / 100.0)
                    let d = Vec.length e.Def.Position
                    let gap = d - e.Def.Size - atk.Range
                    if stopped || gap <= 0.0 then w
                    else
                        let step = min gap (e.Def.Speed * slow * dt)
                        let pos = Vec.scale ((d - step) / d) e.Def.Position
                        updateEnemyDef e.Def.Name pos w
                | _ -> w
            let w = alive w |> List.fold move w
            { w with Time = t }

    /// Mana recovery while time passes: regeneration, leech (rate-capped), ES recharge, life regen.
    let private recover (stats: PlayerStats option) (flask: ManaFlask option) dt (w: World) =
        let w =
            match flask, w.Player with
            | Some f, Some p when dt > 0.0 ->
                { w with Player = Some { p with FlaskCharges = min f.MaxCharges (p.FlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt) } }
            | _ -> w
        match stats, w.Player with
        | Some v, Some p when dt > 0.0 && p.Died.IsNone ->
            let leech = min p.LeechPool (v.Mana * Constants.maxManaLeechRate / 100.0 * dt)
            let regen = v.ManaRegen * dt
            let mana = min v.Mana (p.Mana + regen + leech)
            let es =
                if w.Time - p.LastHit >= Constants.energyShieldRechargeDelay then min v.EnergyShield (p.EnergyShield + v.EnergyShieldRecharge * dt)
                else p.EnergyShield
            let flaskT = max 0.0 (min w.Time p.FlaskUntil - (w.Time - dt))
            let flask = p.FlaskRate * flaskT
            let mana = min v.Mana (mana + flask)
            let gained = p.ManaGained |> addf "regen" (min regen (v.Mana - p.Mana)) |> addf "leech" leech |> addf "flask" flask
            { w with
                Player =
                    Some
                        { p with
                            Mana = mana
                            LeechPool = p.LeechPool - leech
                            EnergyShield = es
                            Life = min v.Life (p.Life + v.LifeRegen * dt)
                            Ward = (if p.Ward >= v.Ward then p.Ward else min v.Ward (p.Ward + v.WardRegen * dt))
                            ManaGained = gained } }
        | _ -> w

    /// Mitigated damage onto the player: "damage removed from mana first", energy shield, life
    /// down to 1, then Runic Ward; death when nothing is left.
    let private takeDamage (pools: PlayerStats) (damage: Map<DamageType, float>) (w: World) =
        match w.Player with
        | Some p when p.Died.IsNone ->
            let dmg = damage |> Map.toList |> List.sumBy snd
            let fromMana = min p.Mana (dmg * Mods.sumBase Ctx.empty [ Named "DamageRemovedFromManaFirst" ] (ModDb.ofList []) / 100.0)
            let rest = dmg - fromMana
            let toEs = min p.EnergyShield rest
            let rest = rest - toEs
            let toLife = min (max 0.0 (p.Life - 1.0)) rest
            let rest = rest - toLife
            let toWard = min p.Ward rest
            let rest = rest - toWard
            let life = p.Life - toLife - rest
            let p =
                { p with
                    Mana = p.Mana - fromMana
                    EnergyShield = p.EnergyShield - toEs
                    Ward = p.Ward - toWard
                    Life = life
                    Taken = p.Taken + dmg
                    MinLife = min p.MinLife ((max 0.0 life + p.Ward) / max 1.0 (pools.Life + pools.Ward))
                    Died = if life <= 0.0 && rest > 0.0 then Some w.Time else None }
            { w with Player = Some p }
        | _ -> w

    let private gainMana (v: PlayerStats) source amount (w: World) =
        match w.Player with
        | Some p when amount > 0.0 ->
            let m = min v.Mana (p.Mana + amount)
            { w with Player = Some { p with Mana = m; ManaGained = addf source (m - p.Mana) p.ManaGained } }
        | _ -> w

    let private prune (w: World) =
        let live (x: Effect) = x.Expires > w.Time
        { w with
            Enemies =
                w.Enemies
                |> Map.map (fun _ e ->
                    { e with
                        Debuffs = e.Debuffs |> Map.filter (fun _ x -> live x)
                        Dots = e.Dots |> List.filter (fun d -> d.Expires > w.Time) })
            Actors = w.Actors |> Map.map (fun _ st -> { st with Buffs = st.Buffs |> Map.filter (fun _ x -> live x) })
            Counts =
                w.Actors
                |> Map.fold (fun c _ st -> st.Buffs |> Map.fold (fun c k x -> if not (live x) && x.Expires < infinity && k.EndsWith "Charge" then bump ("expired:" + k) c else c) c) w.Counts }

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
        | Status (Chill, magnitude, duration) ->
            // the slow is read back by the monster's attack timing
            addDebuff target "Chilled" duration 1 [ Mod.base' (Named "ChillMagnitude") magnitude ] w
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
                    | GainEnergy (meta, amount) -> gainEnergy env rng actor meta amount w
                    | GainResource (r, n) -> schedule w.Time (GainStacks(actor.Name, r, n)) w
                    | DropRemnant (r, delay, extra) ->
                        let n = if rng.Chance extra then 2 else 1
                        { w with Counts = bump ("remnant:" + r) w.Counts } |> schedule (w.Time + delay) (GainStacks(actor.Name, r, n)))
            w

    and private gainEnergy env rng (actor: Actor) metaName amount w =
        match actor.Metas |> List.tryFind (fun m -> m.Name = metaName) with
        | None -> w
        | Some meta ->
            let st = Map.find actor.Name w.Actors
            let energy = min meta.MaxEnergy ((Map.tryFind meta.Name st.Energy |> Option.defaultValue 0.0) + amount)
            let w = updateActor actor.Name (fun st -> { st with Energy = Map.add meta.Name energy st.Energy }) w
            releaseMeta env rng actor meta w

    /// Trigger the meta's spells if its energy allows: "Cast on" at maximum, Invocations
    /// whenever enough is banked and the invocation is off cooldown (as many times as it covers).
    and private releaseMeta env rng (actor: Actor) (meta: Meta) w =
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
    and private startUse env rng (actor: Actor) (skill: Skill) (useTime: float) (w: World) =
        match aimed env w with
        | None -> w
        | Some target ->
            let st = Map.find actor.Name w.Actors
            let ctx = Skill.ctx skill false (skillStacks actor skill st) (status target)
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
            let w = if useTime > 0.0 then updateActor actor.Name (fun st -> { st with LastUsed = skill.Name }) w else w
            let times =
                pattern |> List.map (fun (timing, mult) ->
                    (match timing with
                     | DuringUse f -> w.Time + f * useTime + skill.HitDelay
                     | After s -> w.Time + s + skill.HitDelay), mult)
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

    and private trigger env rng (actor: Actor) name (w: World) =
        let skill = skillOf actor name
        let st = Map.find actor.Name w.Actors
        if ready st w.Time skill then
            { w with Counts = bump ("trigger:" + name) w.Counts }
            |> updateActor actor.Name (fun st -> { st with Buffs = Map.add "TriggeredRecently" { Name = "TriggeredRecently"; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs })
            |> schedule (w.Time + 4.0) Expire
            |> startUse env rng actor skill 0.0
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
            // mana cost: base x increased cost / (1 + cost efficiency)
            let db = skillDb env actor skill st
            let cost = skill.Cost * Mods.mult Ctx.empty [ ManaCost ] db / Mods.inc Ctx.empty [ ManaCostEfficiency ] db
            // drink the mana flask when mana is getting low and it isn't already running
            let w =
                match actor.ManaFlask, w.Player with
                | Some f, Some p when p.Mana < 2.0 * cost && w.Time >= p.FlaskUntil && p.FlaskCharges >= f.ChargesPerUse ->
                    { w with
                        Player = Some { p with FlaskCharges = p.FlaskCharges - f.ChargesPerUse; FlaskUntil = w.Time + f.Duration; FlaskRate = f.Amount / f.Duration }
                        Counts = bump "flask:mana" w.Counts }
                | _ -> w
            match actor.Vitals, w.Player with
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
                        |> takeDamage v (Map.ofList [ Fire, burn ])
                    else { w with Player = Some { p with InfernalFlame = flame } }
                startUse env rng actor skill t w |> schedule (w.Time + t) (Ready name)
            | Some v, Some p when p.Mana < cost ->
                // not enough mana: wait until regeneration and leech have covered it
                let rate =
                    v.ManaRegen + (if p.LeechPool > 0.0 then v.Mana * Constants.maxManaLeechRate / 100.0 else 0.0)
                    + (if w.Time < p.FlaskUntil then p.FlaskRate else 0.0)
                let wait = max Constants.serverTick (if rate > 0.0 then (cost - p.Mana) / rate else 1.0)
                { w with Player = Some { p with Starved = p.Starved + wait } } |> schedule (w.Time + wait) (Ready name)
            | Some _, Some p ->
                { w with Player = Some { p with Mana = p.Mana - cost } }
                |> startUse env rng actor skill t
                |> schedule (w.Time + t) (Ready name)
            | _ -> startUse env rng actor skill t w |> schedule (w.Time + t) (Ready name)
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
            let ctx = Skill.ctx skill false (skillStacks actor skill st) (status e)
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
                // ailments are rolled before the damage lands; a shock the hit applies already
                // counts for the hit itself (user observation)
                let inflicted = Ailments.fromHit rng ctx mods def hit
                let hit =
                    let currentShock =
                        Map.tryFind "Shocked" e.Debuffs
                        |> Option.filter (fun x -> x.Expires > w.Time)
                        |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value))
                        |> Option.defaultValue 0.0
                    let newShock = inflicted |> List.tryPick (function Status (Shock, m, _) when m > currentShock -> Some m | _ -> None)
                    match newShock with
                    | Some m ->
                        let withoutOld = def.Mods |> ModDb.toList |> List.filter (fun x -> x.Source <> "Shock")
                        let def' = { def with Mods = ModDb.ofList ((Mod.inc (DamageTaken AllDamage) m |> Mod.from "Shock") :: withoutOld) }
                        { hit with Dealt = Hit.mitigate ctx mods def' hit.Unmitigated }
                    | None -> hit
                let dealt = Dmg.total hit.Dealt
                let life = e.Life - dealt
                let dead = life <= 1e-9
                let w =
                    { w with
                        Enemies = Map.add target { e with Life = (if dead then 0.0 else life) } w.Enemies
                        DamageBy = addf (skill.Name + " (hit)") (min dealt e.Life) w.DamageBy
                        Counts = w.Counts |> bump ("hits:" + skill.Name) |> (if hit.Crit then bump ("crits:" + skill.Name) else id)
                        Kills = if dead then (target, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
                // mana leech from physical attack damage dealt, recovered over time
                let w =
                    match actor.Vitals, w.Player with
                    | Some v, Some p ->
                        let leech = min (Dmg.get Physical hit.Dealt) Constants.effectiveMaxDamageForLeech * Mods.sumBase ctx [ ManaLeech ] mods / 100.0
                        // leech counts from one enemy per hit event: only the largest instance
                        let extra = max 0.0 (leech - p.StrikeLeech)
                        let charges =
                            match actor.ManaFlask with
                            | Some f when dead -> min f.MaxCharges (p.FlaskCharges + RarityCharges.flaskChargesPerKill e.Def.Rarity * f.ChargeGainMult)
                            | _ -> p.FlaskCharges
                        let w = { w with Player = Some { p with LeechPool = p.LeechPool + extra; StrikeLeech = max leech p.StrikeLeech; FlaskCharges = charges } }
                        if dead then
                            let kill = Mods.sumBase Ctx.empty [ ManaOnKill ] mods + v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnKill" ] mods / 100.0
                            gainMana v "kill" kill w
                        else w
                    | _ -> w
                // a killing hit still inflicts its ailments (they count for meta energy and Archon);
                // only their damage over time on the dead monster is dropped
                let w =
                    if dead then
                        react env rng actor OnKill skill None w
                    else
                        inflicted
                        |> List.fold
                            (fun w i ->
                                let w = inflict target actor.Name i w
                                match i with
                                | Dot (a, _, _, _, _) | Status (a, _, _) ->
                                    react env rng actor (OnInflict a) skill (Some target) { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
                                | AddBuildup _ -> w)
                            w
                // meta gem energy: crits and newly inflicted freeze / shock / ignite, scaled by
                // monster power and by the share of the ailment threshold the hit deals
                // "Energy cannot be gained from direct effects of Triggered Skills" (poe2wiki, poe2db)
                let w =
                    if actor.Metas.IsEmpty || skill.Tags.Contains Triggered then w
                    else
                        let threshold = max 1.0 (Ailments.threshold def)
                        let power = MonsterPower.ofRarity e.Def.Rarity
                        let ratio types = Dmg.sumOf types hit.Unmitigated / threshold
                        let before = status e
                        let after = if dead then Map.empty else status w.Enemies.[target]
                        let newly a =
                            if dead then
                                // on a killing hit: what this hit inflicted, and buildup that would complete
                                inflicted
                                |> List.exists (fun i ->
                                    match i with
                                    | Dot (x, _, _, _, _) | Status (x, _, _) -> x = a
                                    | AddBuildup (x, pct) -> x = a && (Map.tryFind x e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0)
                            else after.ContainsKey (Ailment.status a) && not (before.ContainsKey (Ailment.status a))
                        actor.Metas
                        |> List.fold
                            (fun w meta ->
                                let amount =
                                    meta.Sources
                                    |> List.sumBy (fun src ->
                                        match src with
                                        | CritHits per when hit.Crit -> per * power * ratio DamageType.all
                                        | InflictsAilment (a, per) when newly a -> per * power * ratio (Ailments.scalesFromWith ctx mods a)
                                        | _ -> 0.0)
                                if amount > 0.0 then gainEnergy env rng actor meta.Name (amount * meta.EnergyMult) w else w)
                            w
                // infusion remnants (Siphon Elements): each freeze / shock / ignite inflicted
                let w =
                    match actor.Infusions with
                    | Some inf when not inf.Remnants.IsEmpty ->
                        let newly a =
                            match a with
                            | Freeze ->
                                if dead then
                                    inflicted |> List.exists (function AddBuildup (Freeze, pct) -> (Map.tryFind Freeze e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0 | _ -> false)
                                else (status w.Enemies.[target]).ContainsKey (Ailment.status Freeze) && not ((status e).ContainsKey (Ailment.status Freeze))
                            | _ -> inflicted |> List.exists (function Dot (x, _, _, _, _) | Status (x, _, _) -> x = a | _ -> false)
                        inf.Remnants
                        |> List.fold
                            (fun w (a, r, chance) ->
                                if newly a && rng.Chance chance then
                                    let n = if rng.Chance inf.ExtraRemnantChance then 2 else 1
                                    { w with Counts = bump ("remnant:" + r) w.Counts } |> schedule (w.Time + inf.Delay) (GainStacks(actor.Name, r, n))
                                else w)
                            w
                    | _ -> w
                // Archon: gained on a new ignite by a skill with the chance; none while one is active or
                // during the recovery period after one ends
                let w =
                    // every ignite applied by this hit counts, not only a first one
                    let ignited = inflicted |> List.exists (function Dot (Ignite, _, _, _, _) -> true | _ -> false)
                    if not ignited || actor.Archons.IsEmpty then w
                    else
                        let st = Map.find actor.Name w.Actors
                        let active = actor.Archons |> List.exists (fun a -> Map.containsKey a.Name st.Buffs)
                        let recovering = Map.tryFind "ArchonRecovery" st.Cooldowns |> Option.exists (fun t -> t > w.Time)
                        if active || recovering then w
                        else
                            actor.Archons
                            |> List.tryFind (fun a -> rng.Chance(Mods.sumBase ctx [ Named a.OnIgniteStat ] mods))
                            |> function
                                | Some a ->
                                    updateActor actor.Name (fun st ->
                                        { st with
                                            Buffs = Map.add a.Name { Name = a.Name; Mods = a.Mods; Stacks = 1; Expires = w.Time + a.Duration } st.Buffs
                                            Cooldowns = Map.add "ArchonRecovery" (w.Time + a.Duration + a.Recovery) st.Cooldowns }) w
                                    |> schedule (w.Time + a.Duration) Expire
                                    |> fun w -> { w with Counts = bump ("archon:" + a.Name) w.Counts }
                                | None -> w
                // Runic Ward remnants from kills and stuns
                let w =
                    match actor.WardRemnants, w.Player with
                    | Some r, Some p ->
                        let stunnedNow = not dead && (status w.Enemies.[target]).ContainsKey "HeavilyStunned" && not ((status e).ContainsKey "HeavilyStunned")
                        let fromKill = dead && rng.Chance r.OnKillChance
                        let fromStun = stunnedNow && w.Time - p.LastStunRemnant >= r.StunCooldown && rng.Chance r.OnStunChance
                        if fromKill || fromStun then
                            let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                            let w = if fromStun then { w with Player = Some { p with LastStunRemnant = w.Time } } else w
                            { w with Counts = bump "ward remnant" w.Counts } |> schedule (w.Time + r.Delay) (WardArrives(r.WardPerRemnant * count))
                        else w
                    | _ -> w
                let w = markRecently actor.Name true hit.Crit dead w
                let w = react env rng actor OnHit skill (Some target) w
                if hit.Crit then react env rng actor OnCrit skill (Some target) w else w

    let private markStatus actorName (name: string) (w: World) =
        updateActor actorName (fun st -> { st with Buffs = Map.add name { Name = name; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
        |> schedule (w.Time + 4.0) Expire

    let private stacksOf (st: ActorState) r = Map.tryFind r st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0

    /// Set a resource's stacks. `refresh`: a gain restarts its duration; a loss keeps it.
    let private setStacksWith (env: Env) actorName refresh r n (w: World) =
        let actor = actorDef env actorName
        let duration = Map.tryFind r actor.ResourceDurations
        let w =
            updateActor actorName (fun st ->
                if n <= 0 then { st with Buffs = Map.remove r st.Buffs }
                else
                    let expires =
                        match duration, Map.tryFind r st.Buffs with
                        | Some d, _ when refresh -> w.Time + d
                        | _, Some old -> old.Expires
                        | Some d, None -> w.Time + d
                        | None, None -> infinity
                    { st with Buffs = Map.add r { Name = r; Mods = []; Stacks = n; Expires = expires } st.Buffs }) w
        match duration with
        | Some d when refresh && n > 0 -> schedule (w.Time + d) Expire w
        | _ -> w

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
            let shape (skill: Skill) =
                let ctx = Skill.ctx skill false (skillStacks actor skill st) Map.empty
                let mods = skillDb env actor skill st
                let areaMult = Mods.mult ctx [ AreaOfEffect ] mods
                let delivery =
                    match skill.Delivery with
                    | Projectiles p ->
                        let fromCharges =
                            match p.PerCharge with
                            | Some (charge, per) -> per * (Map.tryFind charge (skillStacks actor skill st) |> Option.defaultValue 0)
                            | None -> 0
                        Projectiles
                            { p with
                                Count = p.Count + fromCharges + (if p.Count + fromCharges > 0 then int (Mods.sumBase ctx [ AdditionalProjectiles ] mods) else 0)
                                Pierce = p.Pierce + int (Mods.sumBase ctx [ Pierce ] mods)
                                Chains = p.Chains + int (Mods.sumBase ctx [ Chains ] mods)
                                TerrainChain = p.TerrainChain + Mods.sumBase ctx [ Named "TerrainChain" ] mods }
                    | d -> d
                ctx, mods, areaMult, delivery
            // Infusions: each impact consumes one of the skill's element (or the wildcard) and
            // uses the infused copy; without one, the plain skill
            let consumer = actor.Infusions |> Option.bind (fun inf -> Map.tryFind skillName inf.Consumers |> Option.map (fun c -> inf, c))
            let variant (w: World) =
                match consumer with
                | None -> skill, w
                | Some (inf, (element, infusedName)) ->
                    let st = Map.find name w.Actors
                    let have r = stacksOf st r > 0
                    let source = if have element then Some element else inf.Wildcard |> Option.filter have
                    match source with
                    | None -> skill, { w with Counts = bump ("uninfused:" + skillName) w.Counts }
                    | Some r ->
                        let infused = skillOf actor infusedName
                        let keep = Mods.sumBase Ctx.empty [ Named "InfusionKeepChance" ] (skillDb env actor infused st)
                        let w = { w with Counts = bump ("infused:" + skillName) w.Counts }
                        let w = markStatus name "ConsumedInfusionRecently" w
                        if rng.Chance keep then infused, w
                        else infused, setStacksWith env name false r (stacksOf st r - 1) w
            let w = { w with Player = w.Player |> Option.map (fun p -> { p with StrikeLeech = 0.0 }) }
            // Spell Cascade: extra impacts on either side of the target "along a randomly chosen
            // axis"; side impacts have their own less area; spacing: user observation
            let ctx0, mods0, _, _ = shape skill
            let cascades = int (Mods.sumBase ctx0 [ Named "CascadesPerSide" ] mods0)
            match skill.Delivery with
            | AreaAtTarget _ when cascades > 0 ->
                let axis = Vec.ofAngle (rng.Float() * 2.0 * System.Math.PI)
                // the centre impact first (it takes the first infusion); its radius sets the spacing
                let order = 0 :: [ for i in 1 .. cascades do yield -i; yield i ]
                order
                |> List.fold
                    (fun (w, step) k ->
                        let sk, w = variant w
                        let ctx, mods, areaMult, delivery = shape sk
                        let r = match delivery with AreaAtTarget r -> r | _ -> 0.0
                        let centreR = Geometry.radius r areaMult
                        let step = if k = 0 then centreR * env.Fight.CascadeSpacing else step
                        let rr = if k = 0 then centreR else Geometry.radius r (areaMult * Mods.more ctx [ Named "CascadeArea" ] mods)
                        let centre = Vec.add primary.Position (Vec.scale (float k * step) axis)
                        let w =
                            living
                            |> List.filter (fun e -> Vec.dist centre e.Position <= rr + e.Size)
                            |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w
                        w, step)
                    (w, 0.0)
                |> fst
            | _ ->
                let sk, w = variant w
                let _, _, areaMult, delivery = shape sk
                Geometry.targetsFrom rng env.Fight.Walls env.Fight.Reach delivery areaMult primary living
                |> List.fold (fun w target -> hitEnemy env rng actor sk mult target w) w

    /// A monster's hit at the player: evasion, deflection, armour, resistances, then
    /// energy shield and life. Frozen / stunned / electrocuted monsters don't act; chill
    /// slows them; blind lowers their accuracy.
    let private enemyAttack env (rng: Rng) name (w: World) =
        let e = w.Enemies.[name]
        match e.Def.Attack, env.Fight.Player.Vitals, w.Player with
        | Some atk, Some pools, Some p when e.Life > 0.0 && p.Died.IsNone ->
            // mitigation from the player's current buffs and charges
            let v =
                match env.Fight.Player.VitalsAt with
                | Some at -> at (buffStacks (Map.find env.Fight.Player.Name w.Actors))
                | None -> pools
            let st = status e
            let stopped = [ "Frozen"; "HeavilyStunned"; "Electrocuted" ] |> List.exists st.ContainsKey
            let chill = e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
            let next = atk.Interval / max 0.1 (1.0 - chill / 100.0)
            let w = schedule (w.Time + next) (EnemyAttack name) w
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            if stopped then
                { w with Counts = bump "enemy:stopped" w.Counts }
            elif not inRange then
                w
            else
                let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                if not (rng.Chance(Vitals.monsterHitChance v.Evasion accuracy)) then
                    { w with Counts = bump "enemy:evaded" w.Counts }
                else
                    let blockRoll () = rng.Chance v.Block
                    let blocked = blockRoll () || (v.LuckyBlock && blockRoll ())
                    let deflected = not blocked && rng.Chance(Vitals.deflectChance v.Deflection accuracy)
                    // "<from> damage taken as <to>" moves part of each type before mitigation
                    let incoming =
                        v.TakenAs
                        |> List.fold
                            (fun (m: Map<DamageType, float>) (from, into, pct) ->
                                let moved = (Map.tryFind from atk.Damage |> Option.defaultValue 0.0) * pct / 100.0
                                m |> Map.add from ((Map.tryFind from m |> Option.defaultValue 0.0) - moved)
                                  |> Map.add into ((Map.tryFind into m |> Option.defaultValue 0.0) + moved))
                            atk.Damage
                    let dmg =
                        incoming
                        |> Map.toList
                        |> List.sumBy (fun (t, d) ->
                            let d = if deflected then d * (1.0 - Vitals.deflectEffect / 100.0) else d
                            match t with
                            | Physical -> d * (1.0 - Vitals.armourReduction v.Armour d / 100.0)
                            | t -> d * (1.0 - (Map.tryFind t v.Resist |> Option.defaultValue 0.0) / 100.0))
                    let dmg = if blocked then dmg * v.BlockedDamageTaken / 100.0 else dmg
                    let w = takeDamage pools (Map.ofList [ Physical, dmg ]) { w with Player = Some p } |> fun w -> { w with Player = w.Player |> Option.map (fun p -> { p with LastHit = w.Time }) }
                    let w =
                        { w with
                            Counts = w.Counts |> bump "enemy:hit" |> (if deflected then bump "enemy:deflected" else id) |> (if blocked then bump "enemy:blocked" else id) }
                    let w =
                        match w.Player with
                        | Some p when blocked && v.WardOnBlock > 0.0 -> { w with Player = Some { p with Ward = min (max p.Ward pools.Ward) (p.Ward + v.WardOnBlock) } }
                        | _ -> w
                    if blocked then
                        // blocks feed meta gems (Cast on Block) and OnBlock rules
                        let player = env.Fight.Player
                        let w =
                            player.Metas
                            |> List.fold
                                (fun w meta ->
                                    let amount = meta.Sources |> List.sumBy (function Blocks e -> e | _ -> 0.0)
                                    if amount > 0.0 then gainEnergy env rng player meta.Name (amount * meta.EnergyMult) w else w)
                                w
                        match player.Skills |> List.tryHead with
                        | Some anySkill -> react env rng player OnBlock anySkill (Some name) w
                        | None -> w
                    else w
        | _ -> w

    /// Mark "<what>Recently" for 4 s (gained / consumed charges...).
    let private onPeriodic env (rng: Rng) actorName periodicName (w: World) =
        let actor = actorDef env actorName
        match actor.Periodics |> List.tryFind (fun p -> p.Name = periodicName) with
        | None -> w
        | Some p ->
            let w = schedule (w.Time + p.Every) (PeriodicTick(actorName, periodicName)) w
            let st = Map.find actorName w.Actors
            match p.Action with
            | GainRandom resources ->
                let open' = resources |> List.filter (fun r -> stacksOf st r < (Map.tryFind r actor.MaxResources |> Option.defaultValue 0))
                if open'.IsEmpty then w
                else
                    let r = open'.[min (open'.Length - 1) (int (rng.Float() * float open'.Length))]
                    setStacksWith env actorName true r (stacksOf st r + 1) w
                    |> markStatus actorName ("Gained" + r + "Recently")
                    |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
            | GainWeighted weights ->
                let total = weights |> List.sumBy snd
                let roll = rng.Float() * total
                let r =
                    weights
                    |> List.fold (fun (picked, acc) (k, wgt) -> match picked with Some _ -> picked, acc | None -> (if roll < acc + wgt then Some k else None), acc + wgt) (None, 0.0)
                    |> fst
                    |> Option.defaultValue (fst (List.last weights))
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
                setStacksWith env actorName true r (min maxR (stacksOf st r + 1)) w
                |> markStatus actorName ("Gained" + r + "Recently")
                |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
            | GainAnyRandom resources ->
                let r = resources.[min (resources.Length - 1) (int (rng.Float() * float resources.Length))]
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
                setStacksWith env actorName true r (min maxR (stacksOf st r + 1)) w
                |> markStatus actorName ("Gained" + r + "Recently")
                |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
            | ConsumeEach resources ->
                let skip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] (ModDb.ofList actor.Mods)
                resources
                |> List.fold
                    (fun w r ->
                        let st = Map.find actorName w.Actors
                        if stacksOf st r = 0 then w
                        else
                            let w = markStatus actorName ("Consumed" + r + "Recently") w
                            let w =
                                match actor.Vitals with
                                | Some v when r = "PowerCharge" ->
                                    gainMana v "consume" (v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] (ModDb.ofList actor.Mods) / 100.0) w
                                | _ -> w
                            if rng.Chance skip then { w with Counts = bump ("kept:" + r) w.Counts }
                            else setStacksWith env actorName false r (stacksOf st r - 1) w |> fun w -> { w with Counts = bump ("lost:" + r) w.Counts })
                    w

    let private step env rng (w: World) =
        let (KeyValue ((t, sq), ev)) = Seq.head w.Queue
        let dt = t - w.Time
        let w = { w with Queue = Map.remove (t, sq) w.Queue } |> advance t |> recover env.Fight.Player.Vitals env.Fight.Player.ManaFlask dt
        if allDead w || (w.Player |> Option.exists (fun p -> p.Died.IsSome)) then
            w
        else
            let w = prune w
            match ev with
            | Ready name -> onReady env rng name w
            | Strike (name, skill, mult, aimedAt) -> onStrike env rng name skill mult aimedAt w
            | EnemyAttack name -> enemyAttack env rng name w
            | PeriodicTick (name, periodic) -> onPeriodic env rng name periodic w
            | WardArrives amount ->
                match env.Fight.Player.Vitals, env.Fight.Player.WardRemnants, w.Player with
                | Some v, Some r, Some p -> { w with Player = Some { p with Ward = min (v.Ward * r.Overflow) (p.Ward + amount) } }
                | _ -> w
            | CycleTick (name, cycleName) ->
                let actor = actorDef env name
                match actor.Cycles |> List.tryFind (fun c -> c.Name = cycleName) with
                | None -> w
                | Some c ->
                    let pick = min (c.Variants.Length - 1) (int (rng.Float() * float c.Variants.Length))
                    let e = { Name = c.Name; Mods = c.Variants.[pick]; Stacks = 1; Expires = w.Time + c.Period }
                    updateActor name (fun st -> { st with Buffs = Map.add c.Name e st.Buffs }) w
                    |> fun w -> { w with Counts = bump (sprintf "cycle:%s:%d" c.Name pick) w.Counts }
                    |> schedule (w.Time + c.Period) (CycleTick(name, cycleName))
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
                    if rng.Chance skip then
                        let w = { w with Counts = bump ("kept:" + charge) w.Counts }
                        if actor.KeptChargesPause > 0.0 then
                            // freeze the timer: push expiry back by the part of the pause that
                            // doesn't overlap an earlier one
                            let st = Map.find name w.Actors
                            let pausedUntil = Map.tryFind charge st.Paused |> Option.defaultValue w.Time
                            let newUntil = w.Time + actor.KeptChargesPause
                            let extension = max 0.0 (newUntil - max w.Time pausedUntil)
                            match Map.tryFind charge st.Buffs with
                            | Some e when e.Expires < infinity ->
                                let e = { e with Expires = e.Expires + extension }
                                updateActor name (fun st -> { st with Buffs = Map.add charge e st.Buffs; Paused = Map.add charge newUntil st.Paused }) w
                                |> schedule e.Expires Expire
                            | _ -> w
                        else w
                    else
                        updateActor name (fun st -> { st with Buffs = Map.remove charge st.Buffs }) w
                        |> fun w -> { w with Counts = bump ("consumed:" + charge) w.Counts }
            | GainStacks (name, r, n) ->
                let actor = actorDef env name
                let st = Map.find name w.Actors
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue n
                setStacksWith env name true r (min maxR (stacksOf st r + n)) w
                |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
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
              Energy = Map.empty }
        let w0 =
            { Player =
                fight.Player.Vitals
                |> Option.map (fun v ->
                    { Life = v.Life
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
                      InfernalFlame = 0.0
                      LastStunRemnant = -1e9 })
              Time = 0.0
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
                    a.Name, { empty with Buffs = res; Energy = banked })
                |> Map.ofList
              Queue = Map.empty
              Seq = 0
              DamageBy = Map.empty
              Counts = Map.empty
              Uptime = Map.empty
              Kills = [] }
            |> fun w -> actors |> List.fold (fun w a -> schedule 0.0 (Ready a.Name) w) w
            |> fun w -> actors |> List.fold (fun w a -> a.Cycles |> List.fold (fun w c -> schedule 0.0 (CycleTick(a.Name, c.Name)) w) w) w
            |> fun w -> actors |> List.fold scheduleResourceTimers w
            |> fun w ->
                // periodic actions start at a random point of their first period
                actors |> List.fold (fun w a -> a.Periodics |> List.fold (fun w p -> schedule (rng.Float() * p.Every) (PeriodicTick(a.Name, p.Name)) w) w) w
            |> fun w ->
                if fight.Player.Vitals.IsNone then w
                else
                    // monsters start attacking at random points in their first interval
                    fight.Enemies
                    |> List.fold (fun w e -> match e.Attack with Some a -> schedule (rng.Float() * a.Interval) (EnemyAttack e.Name) w | None -> w) w
            |> schedule fight.MaxTime Expire
        let rec loop (w: World) =
            let died = w.Player |> Option.exists (fun p -> p.Died.IsSome)
            if died || allDead w || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > fight.MaxTime then w
            else loop (step env rng w)
        let w = loop w0
        let cleared = if allDead w then w.Kills |> List.map (fun (_, _, t) -> t) |> List.fold max 0.0 |> Some else None
        { Ttk = cleared
          Duration = (match cleared with Some t -> t | None -> min w.Time fight.MaxTime)
          Kills = List.rev w.Kills
          DamageBy = w.DamageBy
          Counts = w.Counts
          Uptime = w.Uptime
          Died = w.Player |> Option.bind (fun p -> p.Died)
          DamageTaken = w.Player |> Option.map (fun p -> p.Taken) |> Option.defaultValue 0.0
          ManaStarved = w.Player |> Option.map (fun p -> p.Starved) |> Option.defaultValue 0.0
          MinLife = w.Player |> Option.map (fun p -> p.MinLife) |> Option.defaultValue 1.0
          ManaGained = w.Player |> Option.map (fun p -> p.ManaGained) |> Option.defaultValue Map.empty }
