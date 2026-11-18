/// What a fight is made of while it runs: the events in the queue, the state of every actor,
/// enemy and the player, the world that holds them, and what is fixed for the whole fight.
[<AutoOpen>]
module internal PoEtry.Engine.State

open PoEtry

type Event =
    | Ready of actor: string
    | Strike of actor: string * skill: string * mult: float * aimedAt: string
    /// A strike from a projectile in flight, so long after it was launched.
    | PassingStrike of actor: string * skill: string * mult: float * aimedAt: string * elapsed: float
    /// One hit on one enemy, worked out when the skill was used (a twister reaching it).
    | DirectHit of actor: string * skill: string * mult: float * enemy: string
    /// After a use's hits: the skill consumes its charges (unless the skip roll succeeds).
    | ConsumeCharges of actor: string * charge: string * skill: string
    /// A cycle picks its next variant.
    | CycleTick of actor: string * cycle: string
    /// A monster hits at the player or one of the minions.
    | EnemyAttack of enemy: string
    /// Killed minions come back, if no other has died since this was set.
    | ReviveMinions of at: float
    /// A temporary minion's time is up (unless it was summoned anew since).
    | MinionLeaves of actor: string * at: float
    /// A use of a skill lands (see `Hook.OnLand`): whose, of what, aimed at whom.
    | Landed of actor: string * skill: string * aimedAt: string
    | PeriodicTick of actor: string * periodic: string
    /// A ward remnant's ward arrives.
    | WardArrives of amount: float
    /// A mana remnant's mana arrives.
    | ManaArrives of amount: float
    /// Stacks of a resource arrive (picked-up infusion remnants, Verisium).
    | GainStacks of actor: string * resource: string * count: int
    | Expose of actor: string * target: string * damageType: DamageType * amount: float * duration: float
    /// A killed enemy explodes (see `Reaction.Explode`).
    | Explosion of actor: string * enemy: string * percent: float * damageType: DamageType * radius: float
    /// Ice Crystals appear beside an enemy: (owner, explosion part, enemy, how many, life of one, most at once, seconds they last).
    | PlantCrystals of actor: string * part: string * enemy: string * count: int * life: float * limit: int * lasts: float
    /// One stack of a buff is used up (an empowered attack has landed).
    | SpendBuff of actor: string * buff: string
    /// A meta gem gains energy (at the end of a dodge roll).
    | EnergyArrives of actor: string * meta: string * amount: float
    | Expire

type ActorState =
    { LastUsed: string
      /// Resource timers paused until this time.
      Paused: Map<string, float>
      Buffs: Map<string, Effect>
      Cooldowns: Map<string, float>
      /// Per skill: when its spent stored uses come back.
      Recharges: Map<string, float list>
      Energy: Map<string, float>
      /// Bolts left per crossbow skill (missing: a full load), and the skills whose current
      /// load hasn't been paid for yet (the cost is paid with the first shot of a load).
      Ammo: Map<string, int>
      LoadDue: Set<string>
      /// Rage: when some was last gained, and the fraction of a point regenerated or lost so far.
      RageAt: float
      RageCarry: float
      /// Parts of skills that wait to be set off by another skill (bolts in the ground until a
      /// detonator is used): part name -> how many wait.
      Pending: Map<string, int>
      /// The critical hit roll (0..1) of the last use of each skill and its parts.
      CritRolls: Map<string, float>
      /// How many uses of each skill have had their critical hit roll.
      Rolled: Map<string, int>
      /// A minion's life and energy shield, when it was last hit, and whether it is out of
      /// the fight (killed, or not summoned).
      Life: float
      Shield: float
      HitAt: float
      Gone: bool
      /// When a temporary minion's time is up.
      Leaves: float }

type EnemyState =
    { Def: EnemyDef
      Life: float
      Debuffs: Map<string, Effect>
      Dots: DotInstance list
      Buildup: Map<Ailment, float>
      /// Debuffs a skill consumed off it, and until when it is immune to each.
      Immune: Map<string, float>
      /// When the Heavy Stun bar last grew (it drains after a while without).
      StunBuildupAt: float
      /// A light stun keeps it from acting and walking until then.
      StunnedUntil: float
      /// Flammability left by fire hits: (magnitude in percent, gone at), each on its own.
      Flammability: (float * float) list
      Db: ModDb }

type PlayerState =
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
      /// Darkness reserved by damage taken: until when, how much.
      Darkness: (float * float) list
      InfernalFlame: float
      LastStunRemnant: float
      LastManaRemnant: float
      LastLifeRemnant: float
      /// Life still to be recovered from leech.
      LifeLeechPool: float
      StrikeLifeLeech: float
      LifeFlaskCharges: float
      LifeFlaskUntil: float
      LifeFlaskRate: float
      /// Damage over time on the player (ignite): (until, damage per second, type).
      Dots: (float * float * DamageType) list
      /// Recovery streams (recoup): (until, per second, pool "Life" | "Mana" | "ES").
      Recoups: (float * float * string) list
      /// Stunned or frozen: no action before this time.
      LockedUntil: float
      FreezeBuildup: float
      /// When the player was light stunned, the past 4 s of it (each makes the next harder).
      Stuns: float list
      /// Charges per charm (by its buff name).
      CharmCharges: Map<string, float> }

/// A ground surface or storm as it is in a fight (see `Ground`).
type GroundState =
    { Spec: Ground
      Owner: string
      /// The actor's exposure effect (for the Exposure of who is in it).
      ExposureEffect: float
      /// Where it appeared and where it goes (the same place unless it moves).
      From: Vec
      To: Vec
      Born: float
      Until: float
      /// What it is now: Oil that was lit is Ignited Ground.
      Kind: GroundKind
      /// Ignited Ground: the Ignite that lit it, and the Ignite it inflicts (with what it
      /// adds). A storm: the Ignite of the ground it has absorbed (0: none yet).
      Carried: float
      Ignite: float }

type World =
    { Player: PlayerState option
      Time: float
      /// A march: how far the player has walked; until when the skill it uses holds it and
      /// what part of its walking it keeps till then; where the furthest pack's middle is
      /// (ahead of the player) and how many packs there have been.
      Walked: float
      WalkHeldUntil: float
      WalkKept: float
      /// How long the player's last use took: it starts the next one so much before those
      /// that come at it are near enough.
      Lead: float
      Front: float
      Packs: int
      /// Those that have woken (by name).
      Woken: Set<string>
      /// How far the player had walked when that was last noted for showing.
      ShownWalked: float
      /// What skills have left on the ground, oldest first.
      Grounds: GroundState list
      Enemies: Map<string, EnemyState>
      Actors: Map<string, ActorState>
      Queue: Map<float * int, Event>
      /// Whether what happens is kept for showing (`Shown`, newest first), and where the
      /// enemies were when that was last noted.
      Tracing: bool
      Shown: Shown list
      ShownAt: Map<string, Vec>
      Seq: int
      /// The fight's seed (for rolls that are drawn apart from the fight's stream).
      Seed: uint32
      /// See `Outcome.Evened`.
      Evened: float
      /// See `Outcome.Overkill`.
      Overkill: float
      /// How many events each stream of random numbers has served (see `step`).
      Streams: Map<string, int>
      DamageBy: Map<string, float>
      DamageByType: Map<string, float>
      Counts: Map<string, int>
      Uptime: Map<string, float>
      Kills: (string * Rarity * float) list
      /// Corpses already used up by corpse skills (every kill leaves one).
      CorpsesUsed: int
      /// Seeds planted in enemies (Living Bomb): owner, skill, enemy, the enemy's life when
      /// planted, the damage it takes to set the seed off, when it fizzles.
      Bombs: (string * string * string * float * float * float) list
      /// Debuffs that record a share of the damage an enemy takes and take it again as life
      /// at the end (Inevitable Agony): owner, skill, enemy, the enemy's life when applied,
      /// the share, when it ends.
      Agonies: (string * string * string * float * float * float) list // (owner, skill, enemy, hit damage recorded, share, ends at)
      /// Ice Crystals beside an enemy: (owner, explosion part, enemy, the enemy's life when
      /// they appeared, life of one crystal, how many, gone at). Newest first.
      Crystals: (string * string * string * float * float * int * float) list
      /// Evasion entropy of each defender: (value 0..99, time of the last attempted hit).
      Entropy: Map<string, float * float>
      /// Killed minions are revived at this time; how many have died since the last revival.
      Revival: (float * int) option
      /// Killed minions whose bones have been used (Unearth) since they died.
      Boned: Set<string>
      /// Pustules on enemies: owner, part, enemy, the Poison damage dealt when it was attached,
      /// Poison per 1% more damage, the most (%), when it bursts at the latest.
      Pustules: (string * string * string * float * float * float * float) list
      /// The enemy that is the cause of what happens now (the one just hit, the one whose hit
      /// was blocked; "": none), and the enemy a triggered skill is aimed at while it starts.
      Cause: string
      Aim: string }

/// Fixed for the whole fight: mod dbs built once, buffs layered on per event.
type Env =
    { Fight: Fight
      SkillDbs: Map<string * string, ModDb>
      /// An actor's mods while using a skill (`skillDb`), by actor and skill, for the buffs
      /// it was last asked with: the same buffs (the very same map) give the same mods, and
      /// a use asks many times.
      WithBuffs: System.Collections.Generic.Dictionary<string, System.Collections.Generic.Dictionary<string, Map<string, Effect> * ModDb>>
      /// The player's defences by its buffs and their stacks, written out (`vitalsNow`).
      VitalsSeen: System.Collections.Generic.Dictionary<string, PlayerStats>
      /// The buffs they were last asked with (the very same map), and what they were.
      VitalsLast: (Map<string, Effect> * PlayerStats) option[]
      /// Percent of maximum life lost per minute per Rage (Berserk).
      BerserkLoss: float
      /// Chance (%) for a charm that is used to keep its charges.
      CharmKeep: float
      /// Told what happens, if anyone listens (`traced`): time, kind, who, what, a number
      /// (an event: its place in the queue's order; a hit: what it dealt). Nothing in the
      /// fight depends on it.
      Trace: (float -> string -> string -> string -> float -> unit) option
      /// The player's rage upkeep: regeneration per second, loss per second, seconds without
      /// gaining before it is lost, rage when hit, rage on kill.
      Rage: float * float * float * float * float
      /// "Debuffs you inflict have X% increased Slow Magnitude": how much more the Maim and
      /// Hinder on the monsters slow them (the player's, for all of them).
      SlowMagnitude: float
      /// Whether anything reads the state "ESRechargeStartedRecently" (`asksBuff`): it is
      /// only kept where something does.
      AsksESRecharge: bool }

let allAilments =
    [ Ignite; Bleed; Poison; Shock; Chill; Freeze; Electrocute; HeavyStun; Pin; Daze; Maim; Blind; Hinder ]

let bump key (m: Map<string, int>) =
    Map.add key ((Map.tryFind key m |> Option.defaultValue 0) + 1) m

let addf key v (m: Map<string, float>) =
    Map.add key ((Map.tryFind key m |> Option.defaultValue 0.0) + v) m

/// The seed of the n-th draw of a stream of its own, named by `key`, in a fight of this seed.
let streamHash (key: string) : uint32 =
    let mutable h = 5381u
    for ch in key do
        h <- (((h <<< 5) &&& 0xFFFFFFFFu) + h + uint32 ch) &&& 0xFFFFFFFFu
    h

/// `streamSeed` with the stream's name already hashed (`streamHash`).
let streamSeedOf (seed: uint32) (h: uint32) (n: int) : uint32 =
    ((seed ^^^ h) + uint32 (n % 65536) * 40503u + 1u) &&& 0xFFFFFFFFu

let streamSeed (seed: uint32) (key: string) (n: int) : uint32 = streamSeedOf seed (streamHash key) n

let schedule t ev (w: World) =
    { w with
        Queue = Map.add (t, w.Seq) ev w.Queue
        Seq = w.Seq + 1 }

let alive (w: World) = w.Enemies |> Map.toList |> List.map snd |> List.filter (fun e -> e.Life > 0.0)
let allDead (w: World) = alive w |> List.isEmpty

/// Corpses used up, with the one a kill at `t` does not leave: "Monsters killed while frozen
/// will shatter, leaving behind no corpse".
let corpsesAfter (dead: bool) name t (w: World) =
    match Map.tryFind name w.Enemies with
    | Some e when dead && (e.Debuffs |> Map.tryFind "Frozen" |> Option.exists (fun x -> x.Expires > t)) -> w.CorpsesUsed + 1
    | _ -> w.CorpsesUsed

let updateEnemyDef name (pos: Vec) (w: World) =
    { w with Enemies = w.Enemies |> Map.change name (Option.map (fun e -> { e with Def = { e.Def with Position = pos } })) }

// ---- views of an enemy ----
