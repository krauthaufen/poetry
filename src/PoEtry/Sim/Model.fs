namespace PoEtry

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
      LastUsed: string
      /// What stands or lies where a skill put it (ground, a storm), counted by the skill.
      Standing: Map<string, int> }

/// What a ground surface or a storm is.
type GroundKind =
    /// Oil Ground: who stands in it is covered in Oil; it catches fire.
    | Oil
    /// Ignited Ground: who stands in it is Ignited.
    | Burning
    /// A storm that takes up the Elemental Ground Surfaces it overlaps (Tornado).
    | Storm

/// Something a skill leaves where it lands: a patch of ground, or a storm. It is a circle
/// that lasts its time; an enemy is in it while its body reaches into it.
type Ground =
    { /// The skill it is of (`View.Standing` counts it under this name).
      Skill: string
      Kind: GroundKind
      /// Its radius as it appears, game units.
      Radius: float
      /// "gain X% increased Area of Effect per second, up to a maximum of Y%" (percent).
      GrowPerSecond: float
      GrowMax: float
      Seconds: float
      /// How many of the skill there are at once (one more takes the oldest away); 0: any number.
      Limit: int
      /// Whose limit it counts against: the gem's name, the same for every copy of the gem
      /// (one pressed, one behind a meta gem).
      Shared: string
      /// Oil: what an enemy in it has, monster side (its slow, the Flammability it takes).
      Mods: Mod list
      /// Oil, and Ignited Ground that was Oil: the Exposure of an enemy in it, percent of
      /// fire resistance (before the enemy's and the actor's exposure effect).
      Exposure: float
      /// Ignited Ground: the Ignite of who stands in it, damage per second before the enemy's
      /// mitigation. Oil: what it adds, once lit, to the Ignite that lit it.
      Ignite: float
      /// How long an Ignite it inflicts lasts.
      IgniteSeconds: float
      /// A storm: for an element whose ground it has absorbed, the damage per second it deals
      /// to who is in it.
      Absorbs: (DamageType * float) list
      /// It appears where the actor stands and goes to where it was aimed, so fast (game
      /// units a second; 0: it is where it was aimed from the start).
      Speed: float }

module Channel =
    /// How far the character gets, as a part of what it walks in that time, while it uses a
    /// skill once (0: it stands; 1: as if it did nothing). A skill that cannot be used on the
    /// move: 0. Else it walks at 100 + `MoveWhileUsing` percent of its speed while `MoveHeld`
    /// percent of the use lasts, then faster by `MoveEasing` percent of its speed a second,
    /// up to its own speed (the game's "movement speed while performing action" stats).
    let walked (skill: Skill) (seconds: float) : float =
        let v name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Conds.IsEmpty && m.Kind = Base then m.Value else 0.0)
        if not (skill.Mods |> List.exists (fun m -> m.Stat = Named "UsedWhileMoving")) || seconds <= 0.0 then 0.0
        else
            let start = max 0.0 (min 1.0 (1.0 + v "MoveWhileUsing" / 100.0))
            let held = seconds * max 0.0 (min 1.0 (v "MoveHeld" / 100.0))
            let eases = v "MoveEasing" / 100.0
            let rest = seconds - held
            // from `start` up by `eases` a second, no faster than 1
            let rising = if eases <= 0.0 then rest else min rest ((1.0 - start) / eases)
            let way = start * held + start * rising + eases * rising * rising / 2.0 + (rest - rising)
            way / seconds

    /// The unhurried seconds a cost by the second is paid for: a staged channel's time without
    /// its release ("Modifiers to Cast Speed also affect this skill's cost").
    let paidSeconds (skill: Skill) =
        skill.BaseTime - (skill.Mods |> List.tryPick (fun m -> if m.Stat = Named "ChannelRelease" then Some m.Value else None) |> Option.defaultValue 0.0)

module Ground =
    /// The debuffs of an enemy in Oil, of one on Ignited Ground, and the Exposure both have.
    let oiled = "Covered in Oil"
    let ignited = "On Ignited Ground"
    let exposed = "Exposure:Oil"
    let dotLabel (skill: string) (t: DamageType) = sprintf "%s (%s over time)" skill ((DamageType.name t).ToLowerInvariant())
    let make skill kind radius seconds =
        { Skill = skill; Kind = kind; Radius = radius; GrowPerSecond = 0.0; GrowMax = 0.0; Seconds = seconds; Limit = 0; Shared = skill; Mods = []; Exposure = 0.0
          Ignite = 0.0; IgniteSeconds = Constants.igniteDuration; Absorbs = []; Speed = 0.0 }

type SkillFilter =
    | AnySkill
    | WithTag of SkillTag
    /// Has the first tag but not the second (attacks that aren't a herald's own).
    | WithTagNot of has: SkillTag * hasNot: SkillTag
    /// Has all of the first tags and none of the second.
    | WithTags of has: SkillTag list * hasNot: SkillTag list
    | Called of string

type Hook =
    | OnBlock
    | OnUse
    /// A use lands: once for a use, when its first hit is due, after that hit, whether it
    /// kills, misses or hits nothing.
    | OnLand
    | OnHit
    | OnCrit
    | OnKill
    /// The player is hit by a monster.
    | WhenHit
    /// The player is hit by a monster's melee attack (after `WhenHit`).
    | WhenHitMelee
    /// A hit of the actor's Fully Breaks the armour of an enemy whose armour was not.
    | OnFullBreak
    /// Killing an enemy that had this status ("Frozen", "Shocked"), with a skill that isn't a herald's own.
    | OnKillOf of status: string
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
    /// Exposure that grows with every pulse of an orb (Frost Bomb): `perPulse` more each
    /// `every` seconds for `pulses` pulses, up to `cap`.
    | GrowExposure of damageType: DamageType * start: float * perPulse: float * cap: float * every: float * pulses: int * duration: float
    /// Use a skill immediately, without spending the actor's time. Respects cooldown.
    | TriggerSkill of name: string
    /// Feed a meta gem.
    | GainEnergy of meta: string * amount: float
    | RemoveBuff of name: string
    /// A buff on every minion (an offering).
    | GrantMinionBuff of name: string * duration: float * mods: Mod list
    /// Temporary minions of a pool appear for `duration` seconds (infinity: until killed).
    | Summon of pool: string * count: int * duration: float
    /// Like Summon; the first of them has these mods for its time (an empowered zombie).
    | SummonEmpowered of pool: string * count: int * duration: float * first: Mod list
    /// Takes a corpse if there is one (every kill leaves one) and goes on with `then'`;
    /// with `otherwise` if there is none.
    | WithCorpse of then': Reaction * otherwise: Reaction
    /// One minion of the pool for every corpse there is, and (`deadMinions`) for every killed
    /// minion waiting to be revived, each once per death (Unearth).
    | SummonPerCorpse of pool: string * duration: float * deadMinions: bool
    /// Spends all of the player's Runic Ward: one minion of the pool per `wardEach` spent.
    | SummonPerWard of pool: string * wardEach: float * duration: float
    /// One of these buffs, picked at random, for `duration`; the others are taken away
    /// (which Ancestral Spirit a totem calls).
    | GrantOneOf of names: string list * duration: float
    /// A pustule on the enemy hit, if it has none (Toxic Domain): it stores the Poison damage
    /// dealt from then on, and bursts with the part after `seconds`, or as soon as it holds
    /// enough for its most: 1% more damage per `poisonPerPercent` stored, up to `capPercent`.
    | AttachPustule of part: string * seconds: float * poisonPerPercent: float * capPercent: float
    | NoReaction
    /// Take a debuff off the enemy concerned (a mark that has been activated).
    | RemoveDebuff of name: string
    /// Stacks of a resource (infusions from Powered by Verisium), capped at its maximum.
    | GainResource of resource: string * count: int
    /// A remnant of the resource that arrives after `delay` (detonation + pickup);
    /// `extraChance` % for two.
    | DropRemnant of resource: string * delay: float * extraChance: float
    /// A buff scaled by the charges held when used (Charged Staff): one stack per charge
    /// (`doubleChance` % for twice as many), `perStack` mods and seconds per stack. A new
    /// one replaces the old.
    | ChargeBuff of name: string * charge: string * perStack: Mod list * secondsPerStack: float * doubleChance: float
    /// If the actor has a stack of the buff: spend one and do this.
    | SpendStack of buff: string * then': Reaction
    /// Set a buff's stacks (no duration), e.g. "the next 3 attack hits".
    | SetStacks of buff: string * stacks: int
    /// Only if the enemy concerned has this status (a curse being replaced): do this.
    | IfEnemyHas of status: string * then': Reaction
    /// Only while the actor has a stack of the buff: do this.
    | IfBuff of buff: string * then': Reaction
    /// The player regenerates this much energy shield per second for the duration; a new one
    /// replaces the old ("if you have lost a Ghost Shroud Recently").
    | RegenES of perSecond: float * duration: float
    /// A warcry: empowers one attack per `perPower` monster power among the living enemies,
    /// counting up to `cap` power, at least one. The buff's stacks are the attacks left.
    | Empower of buff: string * perPower: float * cap: float
    /// The player becomes Ignited: this percent of its maximum life and energy shield per
    /// second as fire damage, for an Ignite's time ("Become Ignited when you deal a Critical
    /// Hit"). The player's fire resistance and what it has against being Ignited apply.
    | IgniteSelf of percentPerSecond: float
    /// An ailment's status on the enemy concerned, as a hit would inflict it (a Shock only
    /// where it is stronger than the one there): what standing in Shocked Ground does.
    | InflictStatus of ailment: Ailment * magnitude: float * duration: float
    /// The killed enemy's most damaging Poison goes to the other living enemies within the
    /// radius of it (with what it had left of its time), and with this chance (%) each of them
    /// is Hindered for that time too (Herald of Plague).
    | SpreadPoison of radius: float * hinderChance: float
    /// The killed enemy's modifiers (its `Affixes`) as buffs on the actor for `duration`, one
    /// buff for each (`RareMods.buffName`), a new one of a name starting its time again ("When
    /// you kill a Rare monster, you gain its Modifiers for 60 seconds").
    | GainRareMods of duration: float
    /// The enemy concerned (one that was killed) explodes: every other living enemy within
    /// `radius` of it is dealt this percentage of its maximum life as damage of the type, which
    /// each mitigates as a hit of that type (resistance, armour, damage taken). The damage is
    /// the corpse's: the modifiers to the damage of whoever caused it do not scale it. Those
    /// it kills are kills of the actor's (and may explode in turn).
    | Explode of percent: float * damageType: DamageType * radius: float
    /// Every corpse there is explodes as by `Explode`, and is used up.
    | ExplodeCorpses of percent: float * damageType: DamageType * radius: float
    /// At most once every `seconds`: if the time since this was last done is over, do this and
    /// start the time again. The time is the actor's own, by `key`; it is no cooldown (cooldown
    /// recovery does not shorten it) and no buff (nothing makes it expire faster).
    | Every of key: string * seconds: float * then': Reaction
    /// The skill leaves this where the enemy concerned is (on use: the enemy aimed at).
    | LayGround of Ground

/// "Enemies you sufficiently Overkill ... cause an explosion that Ignites nearby enemies
/// based on the Overkill damage dealt" (Herald of Ash). `Skill` is the explosion among the
/// actor's skills: its area, radius and duration are the explosion's.
type OverkillBurn =
    { Skill: string
      Filter: SkillFilter
      /// Overkill needed, percent of the killed enemy's maximum life.
      ThresholdPercent: float
      /// Ignite damage per second, percent of the overkill damage.
      PercentPerSecond: float }

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
    /// Energy per monster power for each melee hit (Fire Spell on Hit), scaled like crits.
    | MeleeHits of perPower: float
    /// Energy per monster power for each kill with a melee skill (Reaper's Invocation).
    | MeleeKills of perPower: float
    /// Energy per game unit travelled while dodge rolling (Cast on Dodge).
    | DodgeRolls of perUnit: float
    /// Flat energy per block.
    | Blocks of energy: float
    /// Energy per 10 ms of base cast time of spells the player casts.
    | CastsSpells of perTenMs: float
    /// Energy when one of the actor's reviving minions is killed, per unit of the minion's
    /// power (its life as a multiple of an ordinary monster's).
    | MinionDeaths of perPower: float

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
      /// The character's own "Meta Skills gain increased Energy" (the tree's, an item's): they
      /// add to the gem's increase, and some hold only for a while ("if you've dealt a Critical Hit Recently").
      EnergyMods: ModDb
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
          EnergyMods = ModDb.ofList []
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
    /// With this chance (0..1), one stack of a buff that lasts `duration` on its own: every
    /// stack runs out separately (Flames of Chayula). `mods` are per stack.
    | GainTimedBuff of buff: string * chance: float * duration: float * maxStacks: int * mods: Mod list
    /// The player recovers this much life at once (Time of Need).
    | HealLife of amount: float
    /// Temporary minions of a pool appear (see Reaction.Summon).
    | SummonMinions of pool: string * count: int * duration: float
    /// Takes a corpse, if there is one, and summons minions of a pool from it.
    | SummonFromCorpse of pool: string * count: int * duration: float
    /// Triggers one of the actor's skills.
    | TriggerNamed of skill: string

/// Mana Remnants: a remnant on a critical hit against an enemy with an elemental ailment (at
/// most one per `CritCooldown` seconds) and by chance on killing one; collected, each gives
/// mana, which can overflow the maximum.
/// Life Remnants: "enemies you kill have a chance to spawn a Life Remnant, and Hitting a
/// target spawns a Life Remnant every few seconds. Picking up a Life Remnant grants you Life".
/// The life of one comes in evenly over the time it takes to collect it.
type LifeRemnants =
    { OnKillChance: float
      /// Seconds between two remnants from hits.
      HitCooldown: float
      LifePerRemnant: float
      /// "Recover N% of Maximum Life when you collect a Remnant".
      PercentOfMaxLife: float
      ExtraChance: float
      /// Seconds from creation until it is collected.
      Delay: float }

type ManaRemnants =
    { OnKillChance: float
      CritCooldown: float
      ManaPerRemnant: float
      /// "Recover N% of Maximum Mana when you collect a Remnant".
      PercentOfMaxMana: float
      ExtraChance: float
      /// Seconds from creation until the mana arrives (collecting).
      Delay: float
      /// Mana can overflow to this multiple of its maximum.
      Overflow: float }

/// A banner: attacking builds its Glory (a share of the monster's power per attack hit); at
/// the full amount it is placed and its aura lasts for the duration.
type Banner =
    { Name: string
      Glory: float
      PerAttackHit: float
      Duration: float
      Mods: Mod list }

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

/// What a minion has to lose, and how it comes back.
type MinionBody =
    { Life: float
      EnergyShield: float
      Armour: float
      Evasion: float
      /// Percent, by damage type.
      Resist: Map<DamageType, float>
      /// It fights at the monster (true) or from where the player stands.
      Melee: bool
      /// Its body's radius and how far its attack reaches, game units (for the room melee
      /// minions need around an enemy).
      Size: float
      Reach: float
      /// Seconds until a killed minion is revived (counted from the last death, see the
      /// engine); None: it is gone for good.
      Revive: float option
      /// Its power for effects that count it (its life as a multiple of a monster's).
      Power: float
      /// Life regenerated per second.
      LifeRegen: float
      /// Percent of the damage of hits on its owner it takes in the owner's place.
      TakesForOwner: float
      /// It explodes when reduced to low life, with this skill of its own (Minion Instability).
      Explodes: string option
      /// What it takes of a hit's damage after its armour and resistances ("Minions take X%
      /// more / less Damage"): a multiplier, 1 where nothing says so.
      Taken: float
      /// "Minions have X% additional Physical Damage Reduction": percent, added to its armour's
      /// (up to 90% together).
      PhysicalReduction: float }

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
      /// The buffs VitalsAt looks at, where that is known: for buffs that differ in others
      /// only it gives the same. (Nothing in this engine reads it; the fast one asks VitalsAt
      /// once per set of these.)
      VitalsReads: Set<string> option
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
      /// Stored cooldown uses recharge at the same time (true) or one after another (false).
      ParallelUses: bool
      WardRemnants: WardRemnants option
      ManaRemnants: ManaRemnants option
      LifeRemnants: LifeRemnants option
      Banners: Banner list
      /// The actor only acts while the player has this buff (a totem while it stands).
      ActiveWhile: string option
      /// A minion that can be hurt and killed. None: nothing can happen to the actor.
      Body: MinionBody option
      /// A temporary minion: it is out of the fight until something summons minions of this
      /// pool; the pool's size is the most there can be at once.
      Pool: string option
      Infusions: Infusions option
      Archons: Archon list
      Charms: Charm list
      OverkillBurns: OverkillBurn list }

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
          VitalsReads = None
          ManaFlask = None
          Periodics = []
          MaxResources = Map.empty
          ResourceDurations = Map.empty
          KeptChargesPause = 0.0
          ParallelUses = false
          WardRemnants = None
          ManaRemnants = None
          LifeRemnants = None
          Banners = []
          ActiveWhile = None
          Body = None
          Pool = None
          Infusions = None
          Archons = []
          Charms = []
          OverkillBurns = [] }

    let always skill = { Use = skill; When = fun _ -> true }

/// A map walked through: packs in a line ahead, more of them as the player comes on. The
/// player walks whenever it is not held by a skill it uses (`Channel.walked`); what happens is
/// reckoned around the player, so that it is the map that comes towards it.
type March =
    { /// How fast the player walks, game units a second.
      Speed: float
      /// From one pack's middle to the next.
      Spacing: float
      /// How near the player must come for a monster to wake: it walks and attacks from then on,
      /// also when the player has gone on (none is lost: the map is cleared when all are dead).
      Aggro: float
      /// How near an enemy must be for the player to use its skills (it walks on till then).
      Engage: float
      /// The player stands while an enemy is that near (it finishes what it meets); else it
      /// walks on also then, as far as the skill it uses lets it, and may leave enemies behind.
      Hold: bool
      /// The map is rooms, a spacing long each with its pack in the middle: a pack wakes as a
      /// whole when the player comes into its room (and not by how near the player is).
      Rooms: bool
      /// How many packs the map has; after the last comes what `Pack` gives for this number
      /// (the map's boss), and the map is cleared when all of them are dead.
      Packs: int
      /// The pack of this number (0, 1, ...): its monsters, placed around its middle.
      Pack: int -> EnemyDef list }

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
      /// Debuffs every enemy has for the whole fight (curses applied as an aura): name, stacks
      /// and monster-side mods per stack; curses are scaled by the enemy's "curse effect on self".
      EnemyAuras: (string * int * Mod list) list
      /// The radius of the player's Presence, game units (infinity: every enemy is in it), and
      /// the debuffs an enemy has while it is in it (auras "on enemies in your Presence"):
      /// name, stacks, monster-side mods per stack. An enemy is in it when its body reaches
      /// into it; looked at whenever something happens in the fight.
      Presence: float
      PresenceAuras: (string * int * Mod list) list
      /// What an enemy gets once as it comes into the Presence, if it has none of it then
      /// ("Unwithered enemies are Withered for X seconds when they enter your Presence"): name,
      /// stacks, monster-side mods per stack, seconds. Again only after it has been out of it.
      PresenceEntry: (string * int * Mod list * float) list
      /// A dead enemy is replaced by a new one of its kind that walks in from further out
      /// (a pack that never ends: the fight lasts until MaxTime).
      Replace: bool
      /// The fight is a march through a map (the enemies are its packs, not `Enemies`).
      March: March option
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
          EnemyAuras = []
          Presence = infinity
          PresenceAuras = []
          PresenceEntry = []
          Replace = false
          March = None
          MaxTime = maxTime }

    /// The fight with every actor under a name of its own: the engines keep an actor's
    /// state by its name, so two of one name are told apart before a fight starts. A minion
    /// that goes by the player's name, or by that of a minion before it, plays as that name
    /// with " #2" (or the first number that is free). A fight whose names are apart already
    /// is given back as it is.
    let apart (fight: Fight) : Fight =
        let given = fight.Player.Name :: (fight.Minions |> List.map (fun m -> m.Name))
        if (List.distinct given).Length = given.Length then fight
        else
            let all = Set.ofList given
            let minions, _ =
                fight.Minions
                |> List.fold (fun (done': Actor list, taken: Set<string>) m ->
                    if not (taken.Contains m.Name) then m :: done', taken.Add m.Name
                    else
                        let rec free (k: int) =
                            let name = m.Name + " #" + string k
                            if taken.Contains name || all.Contains name then free (k + 1) else name
                        let name = free 2
                        { m with Name = name } :: done', taken.Add name) ([], Set.singleton fight.Player.Name)
            { fight with Minions = List.rev minions }

/// A timed effect with stacks.
type Effect =
    { Name: string
      Mods: Mod list
      Stacks: int
      Expires: float }

type DotInstance =
    { /// The ailment, or None for a skill's own damage over time (named by Label).
      Ailment: Ailment option
      Label: string
      Type: DamageType
      Dps: float
      Expires: float
      MaxStacks: int
      Owner: string
      /// An Ignite: what Oil it lights burns with (for a hit's Ignite its damage per second;
      /// for one that Ignited Ground inflicted, the Ignite that lit that ground).
      Carried: float }

/// What happened in a fight, for showing it: a skill use and what it looked like, a hit on
/// an enemy, where an enemy went, a hit on the player.
type Shown =
    | Used of time: float * actor: string * skill: string * marks: Mark list
    | Struck of time: float * enemy: string * damage: float * crit: bool
    | Moved of time: float * enemy: string * position: Vec
    | Hurt of time: float * enemy: string * damage: float
    | Entered of time: float * enemy: EnemyDef
    /// On a march: how far the player has walked by then (game units).
    | Walked of time: float * distance: float

type Outcome =
    { /// What happened, in order (only of a run that was asked to keep it: `Sim.shown`).
      Shown: Shown list
      /// Time every enemy was dead, None if some survived MaxTime.
      Ttk: float option
      Duration: float
      /// Kill time per enemy (name, rarity, time).
      Kills: (string * Rarity * float) list
      /// Damage dealt by source: "Skill (hit)", "Ignite", ...
      DamageBy: Map<string, float>
      /// Damage dealt by damage type ("Fire", ...; "Other": owed damage that has no type).
      DamageByType: Map<string, float>
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
      /// What the hits would have dealt more (or less) had each dealt what it deals on
      /// average over critical hit or not, in place of what its roll made it: added to the
      /// damage dealt, a fight's damage without the luck of which uses were critical hits.
      Evened: float
      /// What the hits dealt beyond the life their enemies had left: not part of the damage
      /// dealt, which ends where the life does.
      Overkill: float
      /// Lowest life as a fraction of maximum.
      MinLife: float
      /// Mana by source: "regen", "leech", "kill".
      ManaGained: Map<string, float> }
