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
      LastUsed: string }

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
      Owner: string }

/// What happened in a fight, for showing it: a skill use and what it looked like, a hit on
/// an enemy, where an enemy went, a hit on the player.
type Shown =
    | Used of time: float * actor: string * skill: string * marks: Mark list
    | Struck of time: float * enemy: string * damage: float * crit: bool
    | Moved of time: float * enemy: string * position: Vec
    | Hurt of time: float * enemy: string * damage: float
    | Entered of time: float * enemy: EnemyDef

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

module Sim =
    type private Event =
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
        /// Ice Crystals appear beside an enemy: (owner, explosion part, enemy, how many, life of one, most at once, seconds they last).
        | PlantCrystals of actor: string * part: string * enemy: string * count: int * life: float * limit: int * lasts: float
        /// One stack of a buff is used up (an empowered attack has landed).
        | SpendBuff of actor: string * buff: string
        /// A meta gem gains energy (at the end of a dodge roll).
        | EnergyArrives of actor: string * meta: string * amount: float
        | Expire

    type private ActorState =
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

    type private EnemyState =
        { Def: EnemyDef
          Life: float
          Debuffs: Map<string, Effect>
          Dots: DotInstance list
          Buildup: Map<Ailment, float>
          /// When the Heavy Stun bar last grew (it drains after a while without).
          StunBuildupAt: float
          /// Flammability left by fire hits: (magnitude in percent, gone at), each on its own.
          Flammability: (float * float) list
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
          /// Charges per charm (by its buff name).
          CharmCharges: Map<string, float> }

    type private World =
        { Player: PlayerState option
          Time: float
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
    type private Env =
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

    let private allAilments =
        [ Ignite; Bleed; Poison; Shock; Chill; Freeze; Electrocute; HeavyStun; Pin; Daze; Maim; Blind; Hinder ]

    let private bump key (m: Map<string, int>) =
        Map.add key ((Map.tryFind key m |> Option.defaultValue 0) + 1) m

    let private addf key v (m: Map<string, float>) =
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
    /// (Remembered for the last few lists of them, by the very list: sorting hundreds of
    /// stacks is asked for several times between two changes of them.)
    let private dotsSeen : (obj * DotInstance list) option[] = Array.create 32 None
    let mutable private dotsNext = 0

    let private activeDots (e: EnemyState) =
        if e.Dots.IsEmpty then []
        else
            let mutable found = None
            for i in 0 .. dotsSeen.Length - 1 do
                match dotsSeen.[i] with
                | Some (dots, active) when obj.ReferenceEquals(dots, e.Dots) -> found <- Some active
                | _ -> ()
            match found with
            | Some active -> active
            | None ->
                // (as `groupBy label |> collect (sortByDescending dps |> truncate limit)` would give
                // them: the groups in the order their labels first come, in a group the strongest
                // first and of equals the one that came first; without sorting where only the
                // few strongest of many are wanted)
                let labels = ResizeArray<string>()
                let groups = ResizeArray<ResizeArray<DotInstance>>()
                for d in e.Dots do
                    let mutable g = -1
                    for i in 0 .. labels.Count - 1 do
                        if g < 0 && labels.[i] = d.Label then g <- i
                    if g < 0 then
                        labels.Add d.Label
                        groups.Add(ResizeArray<DotInstance>())
                        g <- labels.Count - 1
                    groups.[g].Add d
                let active =
                    [ for ds in groups do
                        let mutable limit = ds.[0].MaxStacks
                        for d in ds do
                            if d.MaxStacks > limit then limit <- d.MaxStacks
                        if limit > 0 then
                            // the strongest `limit` so far, strongest first
                            let top = ResizeArray<DotInstance>()
                            for d in ds do
                                if top.Count < limit || d.Dps > top.[top.Count - 1].Dps then
                                    // after every one at least as strong
                                    let mutable at = top.Count
                                    while at > 0 && top.[at - 1].Dps < d.Dps do
                                        at <- at - 1
                                    top.Insert(at, d)
                                    if top.Count > limit then top.RemoveAt(top.Count - 1)
                            yield! top ]
                let at = dotsNext % dotsSeen.Length
                dotsNext <- at + 1
                dotsSeen.[at] <- Some (box e.Dots, active)
                active

    let private statusOf (e: EnemyState) =
        let fromDebuffs = e.Debuffs |> Map.map (fun _ x -> x.Stacks)
        let withDots =
            activeDots e
            |> List.choose (fun d -> d.Ailment |> Option.map Ailment.status)
            |> List.countBy id
            |> List.fold (fun m (k, n) -> Map.add k n m) fromDebuffs
        let ailing = withDots |> Map.exists (fun k _ -> allAilments |> List.exists (fun a -> Ailment.status a = k))
        withDots
        |> fun m -> if e.Def.Moving then Map.add "Moving" 1 m else m
        // within Culling Strike range for its rarity
        |> fun m -> if e.Life <= e.Def.Life * Constants.cullingThreshold (Rarity.name e.Def.Rarity) / 100.0 then Map.add "Cullable" 1 m else m
        |> fun m -> if ailing then Map.add "AnyAilment" 1 m else m
        |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Curse:") then Map.add "Cursed" 1 m else m
        |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Mark:") then Map.add "Marked" 1 m else m
        |> fun m -> if [ "Frozen"; "HeavilyStunned"; "Electrocuted"; "Pinned" ] |> List.exists m.ContainsKey then Map.add "Immobilised" 1 m else m
        |> fun m ->
            match [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.filter m.ContainsKey |> List.length with
            | 0 -> m
            | n -> Map.add "ElementalAilments" n m
        |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Exposure:") then Map.add "Exposed" 1 m else m
        |> fun m -> if e.Def.Rarity = Rare || e.Def.Rarity = Unique then Map.add "RareOrUnique" 1 m else m
        |> fun m -> if e.Def.Rarity = Unique then Map.add "Unique" 1 m else m
        |> fun m -> if e.Def.Rarity = Magic then Map.add "Magic" 1 m else m
        // low life: at or below 35% (PoE2); full life: untouched
        |> fun m -> if e.Life <= 0.35 * e.Def.Life then Map.add "LowLife" 1 m else m
        |> fun m -> if e.Life >= e.Def.Life then Map.add "FullLife" 1 m else m

    /// The last few enemies asked about with their statuses: one hit asks several times about
    /// the very same enemy value. (A pair is put in whole, so a reader on another thread sees
    /// an old pair or a new one, never a mixed one.)
    let private statusSeen : (EnemyState * Map<string, int>) option[] = Array.create 4 None
    let mutable private statusNext = 0

    let private status (e: EnemyState) : Map<string, int> =
        let mutable found = None
        for i in 0 .. statusSeen.Length - 1 do
            match statusSeen.[i] with
            | Some (seen, s) when obj.ReferenceEquals(seen, e) -> found <- Some s
            | _ -> ()
        match found with
        | Some s -> s
        | None ->
            let s = statusOf e
            let at = statusNext % statusSeen.Length
            statusNext <- at + 1
            statusSeen.[at] <- Some (e, s)
            s

    /// The last few enemies' own mods and debuffs with what they make together: a hit on an
    /// enemy changes its life, and a debuff put on it again its time, far more often than
    /// what the debuffs do; and every hit asks. Remembered by the enemy's own mods (the very
    /// value) and what each debuff is (its stacks and mods). (Put in whole, as above.)
    let private defenderSeen : (obj * int * (int * Mod list) list * Defender) option[] = Array.create 32 None
    let mutable private defenderNext = 0

    let private defender (e: EnemyState) : Defender =
        let now = e.Debuffs |> Map.fold (fun acc _ x -> (x.Stacks, x.Mods) :: acc) []
        let rec same (a: (int * Mod list) list) (b: (int * Mod list) list) =
            match a, b with
            | [], [] -> true
            | (n, ms) :: ra, (k, ks) :: rb -> n = k && (obj.ReferenceEquals(ms, ks) || ms = ks) && same ra rb
            | _ -> false
        let mutable found = None
        for i in 0 .. defenderSeen.Length - 1 do
            match defenderSeen.[i] with
            | Some (db, level, debuffs, d) when found.IsNone && obj.ReferenceEquals(db, e.Db) && level = e.Def.Level && same debuffs now -> found <- Some d
            | _ -> ()
        match found with
        | Some d -> d
        | None ->
            let debuffMods =
                e.Debuffs
                |> Map.toList
                |> List.collect (fun (_, x) -> x.Mods |> List.map (Mod.scaled (float x.Stacks)))
            let d = { Level = e.Def.Level; Mods = ModDb.addList debuffMods e.Db }
            let at = defenderNext % defenderSeen.Length
            defenderNext <- at + 1
            defenderSeen.[at] <- Some (box e.Db, e.Def.Level, now, d)
            d

    let private actorDef (env: Env) name =
        if name = env.Fight.Player.Name then env.Fight.Player
        else env.Fight.Minions |> List.find (fun m -> m.Name = name)

    let private buffStacks (st: ActorState) = st.Buffs |> Map.map (fun _ e -> e.Stacks)

    /// The player's buffs as the last few asked about them saw them (one hit of a minion asks
    /// several times about the same buffs). (Put in whole, as above.)
    let private ownerSeen : (Map<string, Effect> * Map<string, int>) option[] = Array.create 2 None
    let mutable private ownerNext = 0

    /// The buffs and charges of the player, as conditions on the owner read them (`OfOwner`).
    let private ownerStacks (env: Env) (w: World) : Map<string, int> =
        let buffs = (Map.find env.Fight.Player.Name w.Actors).Buffs
        let mutable found = None
        for i in 0 .. ownerSeen.Length - 1 do
            match ownerSeen.[i] with
            | Some (seen, s) when obj.ReferenceEquals(seen, buffs) -> found <- Some s
            | _ -> ()
        match found with
        | Some s -> s
        | None ->
            let s = buffs |> Map.map (fun _ e -> e.Stacks)
            let at = ownerNext % ownerSeen.Length
            ownerNext <- at + 1
            ownerSeen.[at] <- Some (buffs, s)
            s

    /// A context with the owner's buffs in it.
    let private owned (env: Env) (w: World) (ctx: Ctx) = { ctx with Owner = ownerStacks env w }

    /// The player's defences and recovery with its buffs, charges and states of the moment
    /// (`Actor.VitalsAt`; None: the player has no pools, or none that change). Remembered by
    /// all its buffs and their stacks: the same buffs, the same answer.
    let private vitalsNow (env: Env) (w: World) : PlayerStats option =
        match env.Fight.Player.Vitals, env.Fight.Player.VitalsAt with
        | Some _, Some at ->
            let st = Map.find env.Fight.Player.Name w.Actors
            match env.VitalsLast.[0] with
            | Some (asked, v) when obj.ReferenceEquals(asked, st.Buffs) -> Some v
            | _ ->
                // (written out: the buffs the defences read, where the actor says which those are)
                let key =
                    match env.Fight.Player.VitalsReads with
                    | Some names ->
                        names
                        |> Set.fold (fun (acc: System.Text.StringBuilder) k ->
                            match Map.tryFind k st.Buffs with
                            | Some e -> acc.Append(k).Append('=').Append(e.Stacks).Append(';')
                            | None -> acc) (System.Text.StringBuilder())
                        |> string
                    | None ->
                        st.Buffs |> Map.fold (fun (acc: System.Text.StringBuilder) k e -> acc.Append(k).Append('=').Append(e.Stacks).Append(';')) (System.Text.StringBuilder()) |> string
                // (what any run of the fight has worked out is kept with the fight)
                let seen = env.VitalsSeen
                let v =
                    lock seen (fun () ->
                        match seen.TryGetValue key with
                        | true, v -> v
                        | _ ->
                            let v = at (buffStacks st)
                            seen.[key] <- v
                            v)
                env.VitalsLast.[0] <- Some(st.Buffs, v)
                Some v
        | v, _ -> v

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
        let build () =
            let buffMods = st.Buffs |> Map.toList |> List.collect (fun (_, e) -> e.Mods |> List.map (Mod.scaled (float e.Stacks)))
            let db = ModDb.addList buffMods (Map.find (actor.Name, skill.Name) env.SkillDbs)
            // Rage: 1% more attack damage per Rage, times rage effect, rounded down.
            match Map.tryFind "Rage" st.Buffs with
            | Some r when r.Stacks > 0 ->
                let effect = Mods.mult Ctx.empty [ Named "RageEffect" ] db
                let more = floor (float r.Stacks * effect) * Constants.moreAttackDamagePerRage
                ModDb.add db (Mod.more (Damage AllDamage) more |> Mod.whenever (SkillIs Attack) |> Mod.from "Rage")
            | _ -> db
        let ofActor =
            match env.WithBuffs.TryGetValue actor.Name with
            | true, d -> d
            | _ ->
                let d = System.Collections.Generic.Dictionary<string, Map<string, Effect> * ModDb>()
                env.WithBuffs.[actor.Name] <- d
                d
        match ofActor.TryGetValue skill.Name with
        | true, (buffs, db) when obj.ReferenceEquals(buffs, st.Buffs) -> db
        | _ ->
            let db = build ()
            ofActor.[skill.Name] <- (st.Buffs, db)
            db

    let private aimed (env: Env) (w: World) =
        // (a triggered skill "usually targets the cause of the trigger", wiki: Trigger)
        match (if w.Aim = "" then None else Map.tryFind w.Aim w.Enemies) with
        | Some e when e.Life > 0.0 -> Some e
        // (the cause died of the hit: the skill lands where it was, on whoever is nearest to there)
        | Some e when not (allDead w) -> alive w |> List.minBy (fun x -> Vec.dist x.Def.Position e.Def.Position) |> Some
        | _ ->
            Geometry.aim env.Fight.Targeting (alive w |> List.map (fun e -> e.Def))
            |> Option.map (fun d -> w.Enemies.[d.Name])

    let private view env (w: World) (st: ActorState) =
        let target = aimed env w
        // what the player sees of the minions: how many of each pool of temporary ones are out
        // ("Out:<pool>"), and how many corpses and dead minions lie about ("Corpses")
        let seen =
            if env.Fight.Minions |> List.forall (fun m -> m.Pool.IsNone) then buffStacks st
            else
                let out =
                    env.Fight.Minions
                    |> List.filter (fun m -> m.Pool.IsSome && not (Map.find m.Name w.Actors).Gone)
                    |> List.countBy (fun m -> "Out:" + m.Pool.Value)
                let bones =
                    env.Fight.Minions
                    |> List.filter (fun m -> (m.Body |> Option.exists (fun b -> b.Revive.IsSome)) && (Map.find m.Name w.Actors).Gone && not (w.Boned.Contains m.Name))
                    |> List.length
                ("Corpses", w.Kills.Length - w.CorpsesUsed + bones) :: out
                |> List.fold (fun m (k, n) -> if n > 0 then Map.add k n m else m) (buffStacks st)
        { Time = w.Time
          Buffs = seen
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
        | WithTagNot (t, n) -> Set.contains t skill.Tags && not (Set.contains n skill.Tags)
        | WithTags (has, hasNot) -> has |> List.forall skill.Tags.Contains && not (hasNot |> List.exists skill.Tags.Contains)
        | Called n -> skill.Name = n

    /// What kills by damage over time count as for a rule's filter: no skill types.
    let dotKiller = Skill.make "damage over time" []

    // ---- time passing ----

    /// Advance the clock to t, applying damage over time and recording uptime.
    /// DoT dps is constant between events (every change is an event), so this is
    /// exact; an enemy dying from DoT mid-interval dies at the exact time.
    /// Whether a condition of the fight's (the player's mods and skills', its minions', its
    /// defences', its rules') asks about a buff of this name.
    let asksBuff (fight: Fight) (name: string) =
        let rec asks (c: Cond) =
            match c with
            | WhileBuff b | PerStack (b, _) -> b = name
            | Not inner | OfOwner inner -> asks inner
            | _ -> false
        let inMods (ms: Mod list) = ms |> List.exists (fun m -> m.Conds |> List.exists asks)
        let rec inReaction (r: Reaction) =
            match r with
            | IfBuff (b, next) -> b = name || inReaction next
            | SpendStack (_, next) | IfEnemyHas (_, next) -> inReaction next
            | WithCorpse (a, b) -> inReaction a || inReaction b
            | _ -> false
        let rec skill (s: Skill) = inMods s.Mods || s.Parts |> List.exists skill
        let actor (a: Actor) =
            inMods a.Mods
            || a.Skills |> List.exists skill
            || a.Rules |> List.exists (fun r -> inReaction r.Reaction)
            || (match a.VitalsReads with Some names -> names.Contains name | None -> a.VitalsAt.IsSome)
        actor fight.Player || fight.Minions |> List.exists actor

    let private advance env t (w: World) =
        let dt = t - w.Time
        if dt <= 0.0 then
            w
        else
            let living = alive w
            let n = float living.Length
            let step (w: World) (e: EnemyState) =
              // (nothing over time on it: only its statuses' time is counted, and it stays the
              // very value it was, so what is remembered about it stays good)
              if e.Dots.IsEmpty then
                { w with Uptime = status e |> Map.fold (fun m k _ -> addf k (dt / n) m) w.Uptime }
              else
                let def = defender e
                let bleedK = if e.Def.Moving then Constants.bleedMovingMultiplier else 1.0
                let active = activeDots e
                let parts =
                    active
                    |> List.map (fun d ->
                        let k = if d.Ailment = Some Bleed then bleedK else 1.0
                        d.Label, d.Dps * k * Ailments.dotTakenMult def d.Type)
                let types =
                    active
                    |> List.map (fun d ->
                        let k = if d.Ailment = Some Bleed then bleedK else 1.0
                        DamageType.name d.Type, d.Dps * k * Ailments.dotTakenMult def d.Type)
                let dps = parts |> List.sumBy snd
                let span = if dps > 0.0 && e.Life <= dps * dt then e.Life / dps else dt
                let life = e.Life - dps * span
                let dead = life <= 1e-9
                { w with
                    Enemies = Map.add e.Def.Name { e with Life = (if dead then 0.0 else life) } w.Enemies
                    DamageBy = parts |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageBy
                    DamageByType = types |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageByType
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
                        (if st.ContainsKey "Maimed" then max 0.0 (1.0 - Constants.maimSlow * env.SlowMagnitude / 100.0) else 1.0)
                        * (if st.ContainsKey "Hindered" then max 0.0 (1.0 - Constants.hinderSlow * env.SlowMagnitude / 100.0) else 1.0)
                        * (1.0 - (e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0) / 100.0)
                    // what its debuffs make of its movement speed (Jagged Ground, "Hindered, with 15% reduced Movement Speed")
                    let slow = if e.Debuffs.IsEmpty then slow else slow * max 0.0 (Mods.more Ctx.empty [ Named "MovementSpeed" ] (defender e).Mods)
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

    /// Auras on enemies in the player's Presence: an enemy has them from when its body reaches
    /// into the Presence, and loses them when it is out of it again. (One that is put on an
    /// enemy by something else under the same name and runs out is the aura's again.)
    let private syncPresence (fight: Fight) (w: World) =
        if fight.PresenceAuras.IsEmpty && fight.PresenceEntry.IsEmpty then w
        else
            alive w
            |> List.fold
                (fun (w: World) e ->
                    let inside = Vec.length e.Def.Position - e.Def.Size <= fight.Presence
                    let debuffs =
                        fight.PresenceAuras
                        |> List.fold
                            (fun (ds: Map<string, Effect>) (name, stacks, mods) ->
                                match Map.tryFind name ds |> Option.filter (fun x -> x.Expires > w.Time) with
                                | None when inside ->
                                    // (an exposure is as strong as the enemy lets exposure be)
                                    let mods = if name.StartsWith "Exposure:" then mods |> List.map (Mod.scaled (Mods.more Ctx.empty [ Named "ExposureEffectOnSelf" ] e.Db)) else mods
                                    Map.add name { Name = name; Mods = mods; Stacks = stacks; Expires = infinity } ds
                                | Some x when not inside && System.Double.IsPositiveInfinity x.Expires -> Map.remove name ds
                                | _ -> ds)
                            e.Debuffs
                    // what an enemy gets once as it comes in: a mark says it is inside, and is
                    // taken off when it is out again
                    let debuffs, expiries =
                        fight.PresenceEntry
                        |> List.fold
                            (fun (ds: Map<string, Effect>, ex: float list) (name, stacks, mods, seconds) ->
                                let mark = "Entered: " + name
                                match Map.containsKey mark ds with
                                | false when inside ->
                                    let ds = Map.add mark { Name = mark; Mods = []; Stacks = 1; Expires = infinity } ds
                                    if Map.tryFind name ds |> Option.exists (fun x -> x.Expires > w.Time) then ds, ex
                                    else Map.add name { Name = name; Mods = mods; Stacks = stacks; Expires = w.Time + seconds } ds, (w.Time + seconds) :: ex
                                | true when not inside -> Map.remove mark ds, ex
                                | _ -> ds, ex)
                            (debuffs, [])
                    let w = if obj.ReferenceEquals(debuffs, e.Debuffs) then w else { w with Enemies = Map.add e.Def.Name { e with Debuffs = debuffs } w.Enemies }
                    List.foldBack (fun t w -> schedule t Expire w) expiries w)
                w

    /// Mana recovery while time passes: regeneration, leech (rate-capped), ES recharge, life regen.
    /// Mitigated damage onto the player, by type: energy shield first (chaos damage removes
    /// twice as much of it), a share of the rest from mana ("damage taken from mana before
    /// life"), life down to 1, then Runic Ward; death when nothing is left. Starts recoup,
    /// and drinks the life flask when life drops below half.
    let private takeDamage (fromHit: bool) (pools: PlayerStats) (damage: Map<DamageType, float>) (w: World) =
        match w.Player with
        | Some p when p.Died.IsNone ->
            let dmg = damage |> Map.toList |> List.sumBy snd
            if dmg <= 0.0 then w
            else
            // Darkness: damage is reserved from what is not reserved of it, before anything else;
            // a reservation is over after its time
            let damage, darkness =
                if pools.Darkness <= 0.0 then damage, p.Darkness
                else
                    let live = p.Darkness |> List.filter (fun (until, _) -> until > w.Time)
                    let taken = min (max 0.0 (pools.Darkness - (live |> List.sumBy snd))) dmg
                    if taken <= 0.0 then damage, live
                    else damage |> Map.map (fun _ d -> d * (1.0 - taken / dmg)), (w.Time + pools.DarknessSeconds, taken) :: live
            let bypass = pools.BypassES / 100.0
            let es, rest =
                damage
                |> Map.fold
                    (fun (es, rest) t d ->
                        let k = if t = Chaos then 2.0 else 1.0
                        let absorbed = min (d * (1.0 - bypass)) (es / k)
                        es - absorbed * k, rest + d - absorbed)
                    (p.EnergyShield, 0.0)
            let fromMana = min p.Mana (rest * pools.ManaBeforeLife / 100.0)
            let rest = rest - fromMana
            let toLife = min (max 0.0 (p.Life - 1.0)) rest
            let rest = rest - toLife
            let toWard = min p.Ward rest
            let rest = rest - toWard
            // life loss from a hit put off: a share prevented, a share lost over 4 s (a negative recoup)
            let deferred = fromHit && pools.DeferPrevent > 0.0
            let life = p.Life - toLife * (if deferred then 1.0 - pools.DeferPrevent / 100.0 else 1.0) - rest
            let later = if deferred then toLife * pools.DeferLose / 100.0 else 0.0
            let recoups =
                pools.Recoup
                |> List.choose (fun (pool, types, pct) ->
                    let rate = match pool with "Life" -> pools.LifeRecoveryRate | "Mana" -> pools.ManaRecoveryRate | _ -> pools.ESRecoveryRate
                    let amount = (types |> List.sumBy (fun t -> Map.tryFind t damage |> Option.defaultValue 0.0)) * pct / 100.0 * rate
                    if amount > 0.0 then Some(w.Time + pools.RecoupSeconds, amount / pools.RecoupSeconds, pool) else None)
            let recoups = (if later > 0.0 then [ w.Time + Constants.deferredLossSeconds, -later / Constants.deferredLossSeconds, "Life" ] else []) @ recoups
            let p =
                { p with
                    Mana = p.Mana - fromMana
                    EnergyShield = es
                    Ward = p.Ward - toWard
                    Darkness = darkness
                    Life = life
                    Taken = p.Taken + dmg
                    Recoups = recoups @ p.Recoups
                    MinLife = min p.MinLife ((max 0.0 life + p.Ward) / max 1.0 (pools.Life + pools.Ward))
                    Died = if life <= 0.0 && rest > 0.0 then Some w.Time else None }
            match pools.LifeFlask with
            | Some f when p.Died.IsNone && pools.Life > 1.0 && life < 0.5 * pools.Life && w.Time >= p.LifeFlaskUntil && p.LifeFlaskCharges >= f.ChargesPerUse ->
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
            | _ -> { w with Player = Some p }
        | _ -> w

    /// Recovery and damage over time on the player during dt (ending now). The pools are the
    /// sheet's (`stats`); how fast they recover is of the moment (`rates`: the defences with
    /// the buffs and states that held during dt).
    let private recover (stats: PlayerStats option) (rates: PlayerStats option) (flask: ManaFlask option) (charms: Charm list) dt (w: World) =
        let w =
            match w.Player with
            | Some p when dt > 0.0 && charms |> List.exists (fun c -> c.ChargesPerSecond > 0.0) ->
                { w with
                    Player =
                        Some
                            { p with
                                CharmCharges =
                                    charms |> List.fold (fun (m: Map<string, float>) c ->
                                        Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + c.ChargesPerSecond * c.ChargeGainMult * dt)) m) p.CharmCharges } }
            | _ -> w
        let w =
            match flask, w.Player with
            | Some f, Some p when dt > 0.0 ->
                { w with Player = Some { p with FlaskCharges = min f.MaxCharges (p.FlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt) } }
            | _ -> w
        match stats, w.Player with
        | Some v, Some p when dt > 0.0 && p.Died.IsNone ->
            let r = rates |> Option.defaultValue v
            let t0 = w.Time - dt
            let overlap until = max 0.0 (min w.Time until - t0)
            let stream (pool: string) =
                let parts = p.Recoups |> List.filter (fun (_, _, pl) -> pl = pool) |> List.map (fun (until, rate, _) -> rate * overlap until)
                // leech: "only the instance with the highest recovery rate will be active" (wiki, Leech)
                if pool.StartsWith "Leech" then (if parts.IsEmpty then 0.0 else List.max parts) else List.sum parts
            // leech: each instance is recovered over one second (keyword text)
            let leech = stream "LeechMana"
            let regen = r.ManaRegen * dt
            let mana = min v.Mana (max 0.0 (p.Mana + regen + leech))
            let es =
                (if w.Time - p.LastHit >= r.ESRechargeDelay then p.EnergyShield + r.EnergyShieldRecharge * dt else p.EnergyShield)
                + r.ESRegen * dt + stream "ES" + stream "ShroudES"
                + (if v.LifeFlaskAlsoES then p.LifeFlaskRate * overlap p.LifeFlaskUntil else 0.0)
                |> min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap))
            let flask = p.FlaskRate * overlap p.FlaskUntil
            let recouped = stream "Mana"
            let mana = min v.Mana (mana + flask + recouped)
            let gained =
                p.ManaGained |> addf "regen" (min regen (v.Mana - p.Mana)) |> addf "leech" leech |> addf "flask" flask
                |> (if recouped > 0.0 then addf "recoup" recouped else id)
            let lifeLeech = stream "LeechLife"
            let lifeRecharge = if r.LifeRecharge > 0.0 && w.Time - p.LastHit >= r.ESRechargeDelay then r.LifeRecharge * dt else 0.0
            let lifeGain = r.LifeRegen * dt + lifeLeech + lifeRecharge + p.LifeFlaskRate * overlap p.LifeFlaskUntil + stream "Life"
            let lifeCharges =
                match v.LifeFlask with
                | Some f -> min f.MaxCharges (p.LifeFlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
                | None -> p.LifeFlaskCharges
            let dots =
                p.Dots
                |> List.fold (fun (m: Map<DamageType, float>) (until, dps, t) -> Map.add t ((Map.tryFind t m |> Option.defaultValue 0.0) + dps * overlap until) m) Map.empty
            let burning = dots |> Map.exists (fun _ d -> d > 0.0)
            let w =
                { w with
                    Player =
                        Some
                            { p with
                                Mana = mana
                                EnergyShield = es
                                Life = min (max p.Life v.Life) (p.Life + lifeGain)
                                LifeFlaskCharges = lifeCharges
                                Ward = (if p.Ward >= v.Ward then p.Ward else min v.Ward (p.Ward + r.WardRegen * dt))
                                Recoups = p.Recoups |> List.filter (fun (until, _, _) -> until > w.Time)
                                Dots = p.Dots |> List.filter (fun (until, _, _) -> until > w.Time)
                                // damage over time keeps energy shield from recharging
                                LastHit = (if burning then w.Time else p.LastHit)
                                ManaGained = gained } }
            // (life lost over time down to nothing: dead)
            let w =
                match w.Player with
                | Some p when p.Life <= 0.0 && p.Died.IsNone -> { w with Player = Some { p with Died = Some w.Time } }
                | _ -> w
            if burning then takeDamage false v dots w else w
        | _ -> w

    let private gainMana (v: PlayerStats) source amount (w: World) =
        match w.Player with
        | Some p when amount > 0.0 ->
            let m = min v.Mana (p.Mana + amount)
            { w with Player = Some { p with Mana = m; ManaGained = addf source (m - p.Mana) p.ManaGained } }
        | _ -> w

    let private prune (w: World) =
        let live (x: Effect) = x.Expires > w.Time
        // (what has nothing to lose stays the very value it was: nothing is built anew, and
        // what is remembered for an actor's buffs stays good)
        let enemyStale (e: EnemyState) = (e.Debuffs |> Map.exists (fun _ x -> not (live x))) || (e.Dots |> List.exists (fun d -> not (d.Expires > w.Time)))
        let actorStale (st: ActorState) = st.Buffs |> Map.exists (fun _ x -> not (live x))
        { w with
            Enemies =
                (if w.Enemies |> Map.exists (fun _ e -> enemyStale e) then
                    w.Enemies
                    |> Map.map (fun _ e ->
                        if enemyStale e then
                            { e with
                                Debuffs = e.Debuffs |> Map.filter (fun _ x -> live x)
                                Dots = e.Dots |> List.filter (fun d -> d.Expires > w.Time) }
                        else e)
                 else w.Enemies)
            Actors =
                (if w.Actors |> Map.exists (fun _ st -> actorStale st) then
                    w.Actors |> Map.map (fun _ st -> if actorStale st then { st with Buffs = st.Buffs |> Map.filter (fun _ x -> live x) } else st)
                 else w.Actors)
            Counts =
                (if w.Actors |> Map.exists (fun _ st -> actorStale st) then
                    w.Actors
                    |> Map.fold (fun c _ st -> st.Buffs |> Map.fold (fun c k x -> if not (live x) && x.Expires < infinity && k.EndsWith "Charge" then bump ("expired:" + k) c else c) c) w.Counts
                 else w.Counts) }

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
                { Ailment = Some a
                  Label = Ailment.name a
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
        | AddBuildup (a, pct, seconds) ->
            let e = w.Enemies.[target]
            // the Heavy Stun bar drains once it has gone a while without any buildup
            let had =
                let had = Map.tryFind a e.Buildup |> Option.defaultValue 0.0
                let idle = w.Time - e.StunBuildupAt - Constants.stunDecayDelay
                if a = HeavyStun && idle > 0.0 then max 0.0 (had - Constants.stunDecayPerSecond * idle) else had
            let w = if a = HeavyStun then updateEnemy target (fun e -> { e with StunBuildupAt = w.Time }) w else w
            let total = had + pct
            if total >= 100.0 && not (Map.containsKey (Ailment.status a) e.Debuffs) then
                updateEnemy target (fun e -> { e with Buildup = Map.add a 0.0 e.Buildup }) w
                |> addDebuff target (Ailment.status a) seconds 1 []
                |> fun w ->
                    // afterwards the monster is much harder to freeze / electrocute / stun / pin again for a while
                    let more, after = Constants.afterBuildup a
                    if more > 0.0 then
                        addDebuff target (Ailment.name a + "Resistant") (seconds + after) 1
                            [ Mod.more (Named(Ailment.name a + "Threshold")) more ] w
                    else w
                |> fun w -> { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
            else
                updateEnemy target (fun e -> { e with Buildup = Map.add a (min total 100.0) e.Buildup }) w

    // ---- using skills ----

    let private allSkills (actor: Actor) = actor.Skills @ (actor.Skills |> List.collect (fun s -> s.Parts))

    let private skillOf (actor: Actor) name =
        allSkills actor |> List.find (fun s -> s.Name = name)

    let private partFlag (p: Skill) (name: string) = p.Mods |> List.exists (fun m -> m.Stat = Named name)

    /// How many of a part that waits to be set off can wait at once: the skill's own limit
    /// ("Maximum of 30 active Bolts", "Limit 6 Spikes": `WaitLimit`), 20 where it has none.
    let waitLimit (p: Skill) =
        match p.Mods |> List.filter (fun m -> m.Stat = Named "WaitLimit" && m.Kind = Base) with
        | [] -> 20
        | ms -> max 1 (int (ms |> List.sumBy (fun m -> m.Value)))

    /// Seconds a part of a skill strikes after the skill's own hit (a lodged spear's explosion
    /// after its detonation time: `PartDelay`).
    let partDelay (p: Skill) = p.Mods |> List.sumBy (fun m -> if m.Stat = Named "PartDelay" && m.Kind = Base then m.Value else 0.0)
    /// The rest of a part's flag that starts with this ("OnlyInfused:ColdInfusion" -> "ColdInfusion").
    let private partNamed (p: Skill) (prefix: string) =
        p.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith prefix -> Some(n.Substring prefix.Length) | _ -> None)

    /// Exposure on an enemy: the strongest applies, scaled by exposure effect.
    let private expose (actor: Actor) (t: string) (dt: DamageType) amount duration (w: World) =
        match Map.tryFind t w.Enemies with
        | Some e when e.Life > 0.0 ->
            let k = effectOnSelf e "ExposureEffectOnSelf" * Mods.inc Ctx.empty [ Named "ExposureEffect" ] (ModDb.ofList actor.Mods)
            let name = "Exposure:" + DamageType.name dt
            let current =
                Map.tryFind name e.Debuffs
                |> Option.filter (fun x -> x.Expires > w.Time)
                |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> -m.Value))
                |> Option.defaultValue 0.0
            if amount * k >= current then addDebuff t name duration 1 [ Mod.base' (Resist dt) (-amount * k) ] w else w
        | _ -> w

    /// Mana spent while a Sigil of Power stands: a Stage for every share of maximum mana, at
    /// most one a second, up to its maximum.
    let private sigilSpend (name: string) (spent: float) (maxMana: float) (w: World) =
        let st = Map.find name w.Actors
        match st.Buffs |> Map.toList |> List.tryFind (fun (_, e) -> e.Mods |> List.exists (fun m -> m.Stat = Named "SigilMaxStages")) with
        | Some (key, sigil) when sigil.Expires > w.Time && spent > 0.0 && maxMana > 0.0 ->
            let value stat = sigil.Mods |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
            let need = value "SigilManaPercentPerStage" / 100.0 * maxMana
            let total = (Map.tryFind "SigilSpent" st.Energy |> Option.defaultValue 0.0) + spent
            let last = Map.tryFind "SigilStageAt" st.Energy |> Option.defaultValue -1.0
            if need > 0.0 && total >= need && sigil.Stacks < int (value "SigilMaxStages") && w.Time - last >= 1.0 then
                { w with Counts = bump "sigil stage" w.Counts }
                |> updateActor name (fun st ->
                    { st with
                        Buffs = Map.add key { sigil with Stacks = sigil.Stacks + 1 } st.Buffs
                        Energy = st.Energy |> Map.add "SigilSpent" (total - need) |> Map.add "SigilStageAt" w.Time })
            else updateActor name (fun st -> { st with Energy = Map.add "SigilSpent" total st.Energy }) w
        | _ -> w

    /// "Can also be used on Ball Lightning Projectiles": the skill a warp can go into, and
    /// whether one of its projectiles is still on its way (it has strikes to come).
    let private warpsInto (skill: Skill) = partNamed skill "WarpsInto:"
    let private ballInFlight (actorName: string) (gem: string) (w: World) =
        w.Queue |> Map.exists (fun _ ev ->
            match ev with
            | Strike (a, name, _, _) | PassingStrike (a, name, _, _, _) -> a = actorName && (name = gem || name.StartsWith(gem + " "))
            | _ -> false)

    /// Curses on an enemy that Hexblast can detonate: those at least half expired (their
    /// duration is the one the actor's own curse rule gives; unknown: any).
    let private ripeCurses (actor: Actor) (e: EnemyState) now =
        e.Debuffs |> Map.toList |> List.filter (fun (k, x) ->
            k.StartsWith "Curse:" && x.Expires > now
            && (actor.Rules |> List.tryPick (fun r ->
                    let rec find reaction =
                        match reaction with
                        | ApplyCurse (n, d, _) when n = k || "Curse:" + n = k -> Some d
                        | IfEnemyHas (_, next) | IfBuff (_, next) | SpendStack (_, next) -> find next
                        | _ -> None
                    find r.Reaction)
                |> Option.forall (fun d -> x.Expires - now <= d / 2.0)))
        |> List.map fst

    /// Barrage: the readied repeats waiting for the next Repeatable bow or spear attack.
    let private barraged (skill: Skill) (st: ActorState) =
        if skill.Tags.Contains(Tag "Type:Barrageable") && skill.Tags.Contains Attack then
            Map.tryFind "Barrage" st.Buffs |> Option.filter (fun b -> b.Stacks > 0)
        else None

    /// Chained attacks (Ice Strike, Tempest Flurry): which use of the chain comes next. Using
    /// another skill in between starts the chain again.
    let private seqLength (skill: Skill) =
        skill.Mods |> List.tryPick (fun m -> match m.Stat, m.Kind with Named "SeqLength", _ -> Some(int m.Value) | _ -> None) |> Option.defaultValue 0
    let private seqStep (skill: Skill) (st: ActorState) =
        let n = seqLength skill
        if n <= 0 then None
        elif st.LastUsed <> skill.Name then Some 0
        else Some(int (Map.tryFind ("Seq:" + skill.Name) st.Energy |> Option.defaultValue 0.0) % n)
    let private inStep (step: int option) (p: Skill) =
        match step, partNamed p "SeqSteps:" with
        | Some k, Some steps -> (steps: string).Split ',' |> Array.exists (fun x -> int x = k)
        | _ -> true
    /// The stat set a chained skill uses this time (the skill itself, or one of its parts).
    let private stepSkill (skill: Skill) (st: ActorState) =
        match seqStep skill st with
        | None -> skill
        | step -> (skill :: skill.Parts) |> List.tryFind (fun p -> (partNamed p "SeqSteps:").IsSome && inStep step p) |> Option.defaultValue skill

    /// Does an attack with this chance to hit land? Evasion is not rolled for each hit: the
    /// defender has an entropy value, random at the first attack and again after 100 server ticks
    /// (3.33 s) without one; each attempt adds the chance to hit, and at 100 or more it hits and
    /// 100 is taken off (wiki, Evasion). All attackers share the defender's value.
    let private lands (rng: Rng) (defender: string) (hitChance: float) (w: World) : bool * World =
        if hitChance >= 100.0 then true, w
        else
            let value =
                match Map.tryFind defender w.Entropy with
                | Some (v, at) when w.Time - at <= 100.0 * Constants.serverTick -> v
                | _ -> floor (rng.Float() * 100.0)
            let value = value + hitChance
            let hit = value >= 100.0
            hit, { w with Entropy = Map.add defender ((if hit then value - 100.0 else value), w.Time) w.Entropy }

    /// Glory and Combo a skill needs before it can be used: (counter, amount).
    let private requirements (skill: Skill) =
        let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
        [ "Glory:" + skill.Name, own "Requires:Glory" * (1.0 + own "GloryRequiredMore" / 100.0)
          "Combo:" + skill.Name, own "Requires:Combo" ]
        |> List.filter (fun (_, n) -> n > 0.0)
    let private built (st: ActorState) (skill: Skill) =
        requirements skill |> List.forall (fun (key, n) -> (Map.tryFind key st.Energy |> Option.defaultValue 0.0) >= n - 1e-9)

    /// A skill that works for a fixed time once what it needs has built up (Requiem: 12 s,
    /// then 30 s until it is charged again): (seconds it lasts, seconds to recharge).
    let private window (skill: Skill) =
        let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
        let secs = own "WindowSeconds"
        if secs > 0.0 then Some(secs, own "WindowRecharge") else None
    /// Is its window shut (the next use opens one)?
    let private windowShut (st: ActorState) now (skill: Skill) =
        (window skill).IsSome && now >= (Map.tryFind ("Window:" + skill.Name) st.Energy |> Option.defaultValue -1.0)
    /// Can it be used: inside its window, or charged for the next.
    let private windowOk (st: ActorState) now (skill: Skill) =
        not (windowShut st now skill) || now >= (Map.tryFind ("WindowNext:" + skill.Name) st.Energy |> Option.defaultValue 0.0)

    let private ready (st: ActorState) now (skill: Skill) =
        Map.tryFind skill.Name st.Cooldowns |> Option.forall (fun t -> t <= now)

    /// A minion goes out of the fight (killed, or its time is up): what it had begun is lost.
    let private leave name (w: World) =
        let its ev =
            match ev with
            | Ready n | Strike (n, _, _, _) | PassingStrike (n, _, _, _, _) | DirectHit (n, _, _, _) | Landed (n, _, _) -> n = name
            | _ -> false
        { w with Queue = w.Queue |> Map.filter (fun _ ev -> not (its ev)) }
        |> updateActor name (fun st -> { st with Gone = true; Life = 0.0; Shield = 0.0 })

    /// Temporary minions of a pool appear: those that are out first; when all are in the
    /// fight, the ones whose time would be up soonest are summoned anew.
    let private summonWith (env: Env) (pool: string) (count: int) (duration: float) (first: Mod list) (w: World) =
        env.Fight.Minions
        |> List.filter (fun m -> m.Pool = Some pool)
        |> List.sortBy (fun m -> let st = Map.find m.Name w.Actors in (if st.Gone then 0 else 1), st.Leaves)
        |> List.truncate count
        |> List.indexed
        |> List.fold
            (fun w (i, m) ->
                let was = Map.find m.Name w.Actors
                let leaves = w.Time + duration
                let life, shield = match m.Body with Some b -> b.Life, b.EnergyShield | None -> 0.0, 0.0
                let buffs (b: Map<string, Effect>) =
                    if i = 0 && not first.IsEmpty then Map.add "Empowered" { Name = "Empowered"; Mods = first; Stacks = 1; Expires = leaves } b
                    else Map.remove "Empowered" b
                let w = updateActor m.Name (fun st -> { st with Gone = false; Leaves = leaves; Life = life; Shield = shield; HitAt = w.Time; Buffs = buffs st.Buffs }) w
                let w = if was.Gone then schedule w.Time (Ready m.Name) w else w
                let w = if leaves < infinity then schedule leaves (MinionLeaves(m.Name, leaves)) w else w
                { w with Counts = bump ("summoned:" + pool) w.Counts })
            w

    let private summon env pool count duration w = summonWith env pool count duration [] w

    /// Something that uses charms happened to the player ("Frozen", "Hit:Fire", "KillRare"...).
    /// A charm for it that is running, or has the charges to be used now, answers; returns
    /// whether the occurrence is prevented (immunity charms). "X% chance for Charms you use to
    /// not consume Charges": rolled for each use.
    let private charm (env: Env) (rng: Rng) (key: string) (w: World) : World * bool =
        let player = env.Fight.Player
        match player.Vitals, w.Player with
        | Some pools, Some p when not player.Charms.IsEmpty ->
            let st = Map.find player.Name w.Actors
            player.Charms
            |> List.filter (fun c -> c.Trigger = key)
            |> List.fold
                (fun (w: World, prevented) c ->
                    let running = Map.tryFind c.Buff st.Buffs |> Option.exists (fun e -> e.Expires > w.Time)
                    let charges = match w.Player with Some p -> Map.tryFind c.Buff p.CharmCharges |> Option.defaultValue 0.0 | None -> 0.0
                    if running then w, prevented || c.Prevents
                    elif charges >= c.ChargesPerUse && c.ChargesPerUse > 0.0 then
                        let kept = rng.Chance env.CharmKeep
                        let w =
                            updateActor player.Name (fun st -> { st with Buffs = Map.add c.Buff { Name = c.Buff; Mods = []; Stacks = 1; Expires = w.Time + c.Duration } st.Buffs }) w
                            |> schedule (w.Time + c.Duration) Expire
                        { w with
                            Player =
                                w.Player |> Option.map (fun p ->
                                    { p with
                                        CharmCharges = (if kept then p.CharmCharges else Map.add c.Buff (charges - c.ChargesPerUse) p.CharmCharges)
                                        Life = min (max p.Life pools.Life) (p.Life + (if pools.Life > 1.0 then c.LifeOnUse * pools.LifeRecoveryRate else 0.0))
                                        Mana = min (max p.Mana pools.Mana) (p.Mana + c.ManaOnUse * pools.ManaRecoveryRate)
                                        LastHit = (if c.StartsRecharge then -1e9 else p.LastHit) })
                            Counts = bump ("charm:" + c.Name) w.Counts |> (if kept then bump "charm:kept charges" else id) },
                        prevented || c.Prevents
                    else w, prevented)
                (w, false)
        | _ -> w, false

    /// What a skill does with corpses, if a rule of the actor says so.
    let private corpseRule (actor: Actor) (skill: Skill) =
        actor.Rules
        |> List.tryPick (fun r ->
            match r.Reaction with
            | WithCorpse _ | SummonPerCorpse _ when r.Hook = OnUse && matches r.Filter skill -> Some r.Reaction
            | _ -> None)

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
                    let rec perform reaction (w: World) =
                      match reaction with
                      | SpendStack (buff, next) ->
                          let st = Map.find actor.Name w.Actors
                          match Map.tryFind buff st.Buffs with
                          | Some e when e.Stacks > 0 && e.Expires > w.Time ->
                              let st' = if e.Stacks = 1 then { st with Buffs = Map.remove buff st.Buffs } else { st with Buffs = Map.add buff { e with Stacks = e.Stacks - 1 } st.Buffs }
                              perform next { w with Actors = Map.add actor.Name st' w.Actors }
                          | _ -> w
                      | RegenES (rate, duration) ->
                          match w.Player with
                          | Some p when p.Died.IsNone ->
                              { w with Player = Some { p with Recoups = (w.Time + duration, rate, "ShroudES") :: (p.Recoups |> List.filter (fun (_, _, pool) -> pool <> "ShroudES")) }
                                       Counts = bump "regen:ES" w.Counts }
                          | _ -> w
                      | SetStacks (buff, n) ->
                          updateActor actor.Name (fun st -> { st with Buffs = Map.add buff { Name = buff; Mods = []; Stacks = n; Expires = infinity } st.Buffs }) w
                      | WithCorpse (next, otherwise) ->
                          if w.Kills.Length > w.CorpsesUsed then perform next { w with CorpsesUsed = w.CorpsesUsed + 1 } else perform otherwise w
                      | NoReaction -> w
                      | IfBuff (buff, next) ->
                          match Map.tryFind buff (Map.find actor.Name w.Actors).Buffs with
                          | Some e when e.Stacks > 0 && e.Expires > w.Time -> perform next w
                          | _ -> w
                      | IfEnemyHas (st, next) ->
                          match target with
                          | Some t when w.Enemies.[t].Life > 0.0 && (status w.Enemies.[t]).ContainsKey st -> perform next w
                          | _ -> w
                      | IgniteSelf percent ->
                          let player = env.Fight.Player
                          match player.Vitals, w.Player with
                          | Some pools, Some p when p.Died.IsNone && actor.Name = player.Name ->
                              // (resistance and what there is against Ignite: with the buffs of the moment)
                              let v =
                                  match player.VitalsAt with
                                  | Some at -> at (buffStacks (Map.find player.Name w.Actors))
                                  | None -> pools
                              if rng.Chance(Map.tryFind "Ignite" v.Avoid |> Option.defaultValue 0.0) then w
                              else
                                  // a charm against Ignite answers (and is used up doing so)
                                  let w, prevented = charm env rng "Ignited" w
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
                                                  let stronger = p.Dots |> List.exists (fun (u, d, t) -> t = Fire && u > w.Time && d > dps)
                                                  if stronger then p else { p with Dots = (until, dps, Fire) :: (p.Dots |> List.filter (fun (_, _, t) -> t <> Fire)) })
                                          Counts = bump "player:Ignited" w.Counts }
                          | _ -> w
                      | Empower (buff, perPower, cap) ->
                          let power = alive w |> List.sumBy (fun e -> MonsterPower.ofRarity e.Def.Rarity)
                          let n = max 1 (int (floor (min cap power / max 1.0 perPower)))
                          { w with Counts = bump ("empowered:" + string n) w.Counts }
                          |> updateActor actor.Name (fun st -> { st with Buffs = Map.add buff { Name = buff; Mods = []; Stacks = n; Expires = infinity } st.Buffs })
                      | other -> performSimple other w
                    and performSimple reaction (w: World) =
                     let onTarget f =
                        match target with
                        | Some t when w.Enemies.[t].Life > 0.0 -> f t
                        | _ -> w
                     match reaction with
                     | SpendStack _ | SetStacks _ | Empower _ | IfEnemyHas _ | IfBuff _ | WithCorpse _ | IgniteSelf _ | RegenES _ | NoReaction -> w
                     | SummonEmpowered (pool, count, duration, first) -> summonWith env pool count duration first w
                     | SummonPerCorpse (pool, duration, deadMinions) ->
                         let size = env.Fight.Minions |> List.filter (fun m -> m.Pool = Some pool) |> List.length
                         let corpses = min size (w.Kills.Length - w.CorpsesUsed)
                         let bones =
                             if not deadMinions then []
                             else
                                 env.Fight.Minions
                                 |> List.filter (fun m ->
                                     (m.Body |> Option.exists (fun b -> b.Revive.IsSome)) && (Map.find m.Name w.Actors).Gone && not (w.Boned.Contains m.Name))
                                 |> List.map (fun m -> m.Name)
                                 |> List.truncate (size - corpses)
                         if corpses + bones.Length = 0 then w
                         else
                             summon env pool (corpses + bones.Length) duration
                                 { w with CorpsesUsed = w.CorpsesUsed + corpses; Boned = bones |> List.fold (fun s n -> Set.add n s) w.Boned }
                     | GrantOneOf (names, duration) when not names.IsEmpty ->
                         let pick = names.[min (names.Length - 1) (int (rng.Float() * float names.Length))]
                         updateActor actor.Name (fun st ->
                             let buffs = names |> List.fold (fun b n -> Map.remove n b) st.Buffs
                             { st with Buffs = Map.add pick { Name = pick; Mods = []; Stacks = 1; Expires = w.Time + duration } buffs }) w
                         |> schedule (w.Time + duration) Expire
                     | GrantOneOf _ -> w
                     | AttachPustule (part, seconds, per, cap) ->
                         onTarget (fun target ->
                             if w.Pustules |> List.exists (fun (_, _, e, _, _, _, _) -> e = target) then w
                             else
                                 let dealt = Map.tryFind "Poison" w.DamageBy |> Option.defaultValue 0.0
                                 { w with
                                     Pustules = (actor.Name, part, target, dealt, per, cap, w.Time + seconds) :: w.Pustules
                                     Counts = bump "pustule" w.Counts }
                                 |> schedule (w.Time + seconds) Expire)
                     | SummonPerWard (pool, wardEach, duration) ->
                         match w.Player with
                         | Some p when wardEach > 0.0 && p.Ward >= wardEach ->
                             summon env pool (int (p.Ward / wardEach)) duration { w with Player = Some { p with Ward = 0.0 } }
                         | _ -> w
                     | GrantBuff (name, duration, maxStacks, mods) ->
                         let w =
                             updateActor actor.Name (fun st ->
                                 let e = stackEffect w.Time (Map.tryFind name st.Buffs) name duration maxStacks mods
                                 { st with Buffs = Map.add name e st.Buffs }) w
                         schedule (w.Time + duration) Expire w
                     | Summon (pool, count, duration) -> summon env pool count duration w
                     | RemoveBuff name ->
                         updateActor actor.Name (fun st -> { st with Buffs = Map.remove name st.Buffs }) w
                     | GrantMinionBuff (name, duration, mods) ->
                         env.Fight.Minions
                         |> List.fold (fun w (m: Actor) ->
                             if Map.containsKey m.Name w.Actors then
                                 updateActor m.Name (fun st -> { st with Buffs = Map.add name { Name = name; Mods = mods; Stacks = 1; Expires = w.Time + duration } st.Buffs }) w
                             else w) w
                         |> schedule (w.Time + duration) Expire
                     | ApplyDebuff (name, _, _, _) when partFlag skill ("CannotInflict:" + name) -> w
                     | ApplyDebuff (name, duration, maxStacks, mods) ->
                         onTarget (fun t -> addDebuff t name duration maxStacks mods w)
                     | ApplyCurse (name, duration, mods) ->
                         onTarget (fun t ->
                             let k = effectOnSelf w.Enemies.[t] "CurseEffectOnSelf"
                             // "Curse:X" / "Mark:X" keep their prefix; plain names are curses
                             let full = if name.StartsWith "Curse:" || name.StartsWith "Mark:" then name else "Curse:" + name
                             let prefix = full.Substring(0, full.IndexOf ':' + 1)
                             // one curse (plus "additional curses"), one mark: the oldest gives way
                             let limit = if prefix = "Mark:" then 1 else 1 + int (Mods.sumBase Ctx.empty [ Named "MaxCurses" ] (ModDb.ofList actor.Mods))
                             let others =
                                 w.Enemies.[t].Debuffs |> Map.toList
                                 |> List.filter (fun (n, x) -> n.StartsWith prefix && n <> full && x.Expires > w.Time)
                                 |> List.sortBy (fun (_, x) -> x.Expires)
                             let drop = others |> List.truncate (max 0 (others.Length + 1 - limit)) |> List.map fst
                             let w = updateEnemy t (fun e -> { e with Debuffs = drop |> List.fold (fun d n -> Map.remove n d) e.Debuffs }) w
                             addDebuff t full duration 1 (mods |> List.map (Mod.scaled k)) w
                             |> fun w -> { w with Counts = bump ("apply:" + full) w.Counts })
                     | ApplyExposure (dt, amount, duration) -> onTarget (fun t -> expose actor t dt amount duration w)
                     | GrowExposure (dt, start, perPulse, cap, every, pulses, duration) ->
                         onTarget (fun t ->
                             [ 1 .. pulses ] |> List.fold (fun w k ->
                                 schedule (w.Time + every * float k) (Expose(actor.Name, t, dt, min cap (start + perPulse * float k), duration)) w) w)
                     | RemoveDebuff name -> onTarget (fun t -> updateEnemy t (fun en -> { en with Debuffs = Map.remove name en.Debuffs }) w)
                     | InflictStatus (a, magnitude, duration) -> onTarget (fun t -> inflict t actor.Name (Status(a, magnitude, duration)) w)
                     | SpreadPoison (radius, hinder) ->
                         match target with
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
                                     let w = poisons |> List.fold (fun w d -> inflict o.Def.Name actor.Name (Dot(Poison, d.Type, d.Dps, d.Expires - w.Time, d.MaxStacks)) w) w
                                     let w = { w with Counts = bump "spread:Poison" w.Counts }
                                     if rng.Chance hinder then addDebuff o.Def.Name "Hindered" longest 1 [] w else w) w
                         | None -> w
                     | GainRareMods duration ->
                         match target with
                         | Some t ->
                             w.Enemies.[t].Def.Affixes
                             |> List.fold (fun w id ->
                                 let name = RareMods.buffName id
                                 updateActor actor.Name (fun st ->
                                     { st with Buffs = Map.add name (stackEffect w.Time (Map.tryFind name st.Buffs) name duration 1 []) st.Buffs }) w
                                 |> schedule (w.Time + duration) Expire
                                 |> fun w -> { w with Counts = bump "rare modifiers gained" w.Counts }) w
                         | None -> w
                     | TriggerSkill name -> trigger env rng actor name w
                     | GainEnergy (meta, amount) -> gainEnergy env rng actor meta amount w
                     | GainResource (r, n) -> schedule w.Time (GainStacks(actor.Name, r, n)) w
                     | ChargeBuff (name, charge, perStack, seconds, doubleChance) ->
                         let held = Map.tryFind charge (Map.find actor.Name w.Actors).Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                         if held = 0 then w
                         else
                             let stacks = if rng.Chance doubleChance then 2 * held else held
                             // mods are per stack: the buff's stacks scale them
                             let e = { Name = name; Mods = perStack; Stacks = stacks; Expires = w.Time + seconds * float stacks }
                             updateActor actor.Name (fun st -> { st with Buffs = Map.add name e st.Buffs }) w
                             |> schedule e.Expires Expire
                             |> fun w -> { w with Counts = bump ("buff:" + name) w.Counts }
                     | DropRemnant (r, delay, extra) ->
                         let n = if rng.Chance extra then 2 else 1
                         { w with Counts = bump ("remnant:" + r) w.Counts } |> schedule (w.Time + delay) (GainStacks(actor.Name, r, n))
                    perform r.Reaction w)
            w

    and private gainEnergy env rng (actor: Actor) metaName amount w =
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
    and private startUse env (rng: Rng) (actor: Actor) (skill: Skill) (useTime: float) (w: World) =
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
                        Player = Some { p with Life = max (min 1.0 p.Life) (p.Life - loseLifeES * v.Life); EnergyShield = max 0.0 (p.EnergyShield - loseLifeES * v.EnergyShield); Mana = max 0.0 (p.Mana - lose * v.Mana) }
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
            let w =
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
            // what happens when the use lands: when its first hit is due, after that hit
            let w =
                if first < infinity && actor.Rules |> List.exists (fun r -> r.Hook = OnLand && matches r.Filter skill) then
                    schedule first (Landed(actor.Name, skill.Name, target.Def.Name)) w
                else w
            // Ice Crystals the use leaves behind (Grim Pillars): each stands at a random spot of the
            // skill's area (around the user, or around the target), and those whose explosion
            // reaches the target count. They appear after the use's own hits, which do not
            // damage them.
            let w =
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
            // a seed planted in the target (Living Bomb): it explodes once the enemy has taken
            // enough damage from then on; a new seed replaces the old one
            let w =
                match skill.Mods |> List.tryPick (fun m -> if m.Stat = Named "DetonatedByDamage" then Some m.Value else None) with
                | Some pct ->
                    let needed = pct / 100.0 * max 1.0 (Ailments.threshold (defender target))
                    let lasts = if skill.Duration > 0.0 then skill.Duration * Mods.mult ctx [ SkillDuration ] mods else 1e9
                    { w with
                        Bombs = (actor.Name, skill.Name, target.Def.Name, target.Life, needed, w.Time + useTime + lasts) :: (w.Bombs |> List.filter (fun (a, s, _, _, _, _) -> not (a = actor.Name && s = skill.Name)))
                        Counts = bump ("planted:" + skill.Name) w.Counts }
                | None -> w
            // the Rage a part spent
            let w =
                let need = skill.Parts |> List.sumBy (fun p -> p.Mods |> List.sumBy (fun m -> if m.Stat = Named "SpendsRage" then m.Value else 0.0))
                match Map.tryFind "Rage" st.Buffs with
                | Some r when need > 0.0 && float r.Stacks >= need ->
                    { w with Counts = bump ("enraged:" + skill.Name) w.Counts }
                    |> updateActor actor.Name (fun st ->
                        let left = r.Stacks - int need
                        { st with Buffs = if left > 0 then Map.add "Rage" { r with Stacks = left } st.Buffs else Map.remove "Rage" st.Buffs })
                | _ -> w
            // a debuff that records damage (Inevitable Agony), on the enemy aimed at
            let w =
                match skill.Mods |> List.tryPick (fun m -> if m.Stat = Named "RecordsDamage" then Some m.Value else None) with
                | Some pct when skill.Duration > 0.0 ->
                    let lasts = skill.Duration * Mods.mult ctx [ SkillDuration ] mods
                    { w with
                        Agonies = (actor.Name, skill.Name, target.Def.Name, 0.0, pct / 100.0, w.Time + useTime + lasts) :: (w.Agonies |> List.filter (fun (a, s, _, _, _, _) -> not (a = actor.Name && s = skill.Name)))
                        Counts = bump ("recording:" + skill.Name) w.Counts }
                | _ -> w
            // an infusion a part needed is used up; a part that waits is put down
            let w =
                skill.Parts |> List.fold (fun w p ->
                    match partNamed p "OnlyInfused:", partNamed p "DetonatedBy:" with
                    | Some infusion, _ when (Map.tryFind infusion st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0) > 0 ->
                        updateActor actor.Name (fun st ->
                            match Map.tryFind infusion st.Buffs with
                            | Some e when e.Stacks > 1 -> { st with Buffs = Map.add infusion { e with Stacks = e.Stacks - 1 } st.Buffs }
                            | _ -> { st with Buffs = Map.remove infusion st.Buffs }) w
                    | _, Some _ -> updateActor actor.Name (fun st -> { st with Pending = Map.add p.Name (min (waitLimit p) ((Map.tryFind p.Name st.Pending |> Option.defaultValue 0) + 1)) st.Pending }) w
                    | _ -> w) w
            // a detonator (or a warcry) sets off everything of that kind that waits
            let w =
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
            // an attack the player makes while empowered by a warcry uses one empowerment up
            let w =
                if useTime > 0.0 && skill.Tags.Contains Attack && not (skill.Tags.Contains Warcry) then
                    let last = times |> List.map fst |> List.fold max w.Time
                    // (and by Mantra of Destruction, Temper Weapon: "EmpoweredAttacks:<skill>")
                    st.Buffs |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "EmpoweredAttacks")
                    |> List.fold (fun w (k, _) -> schedule (last + 1e-9) (SpendBuff(actor.Name, k)) w) w
                else w
            // and a Repeatable attack one of those readied for it (Ice-Tipped Arrows)
            let w =
                if useTime > 0.0 && skill.Tags.Contains Attack && skill.Tags.Contains(Tag "Type:Barrageable") then
                    let last = times |> List.map fst |> List.fold max w.Time
                    st.Buffs |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith "EmpoweredRepeatable:")
                    |> List.fold (fun w (k, _) -> schedule (last + 1e-9) (SpendBuff(actor.Name, k)) w) w
                else w
            let w =
                match skill.Consumes with
                | Some charge ->
                    let last = times |> List.map fst |> List.fold max w.Time
                    // "Consume it when you leap": at the start, where that is what empowers the use
                    let at = if partFlag skill "NoCooldownWithCharge" then w.Time else last
                    schedule (at + 1e-9) (ConsumeCharges(actor.Name, charge, skill.Name)) w
                | None -> w
            // Cast on Dodge: energy per unit travelled by a dodge roll
            let w =
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
            { w with Counts = bump ("trigger:" + name) w.Counts; Aim = w.Cause }
            |> updateActor actor.Name (fun st -> { st with Buffs = Map.add "TriggeredRecently" { Name = "TriggeredRecently"; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs })
            |> schedule (w.Time + 4.0) Expire
            |> startUse env rng actor skill 0.0
            |> fun w -> { w with Aim = "" }
        else
            w

    let private useTime env (actor: Actor) (skill: Skill) (w: World) =
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
        own.BaseTime / speed * (1.0 + Constants.longerPerRepeat * repeats) + Mods.sumBase ctx [ Named "FixedUseTime" ] db

    /// Life spent on a skill can leave the life below half as a hit can: the life flask is
    /// drunk then, as when a hit leaves it there (`takeDamage`).
    let private drinkLifeFlask (pools: PlayerStats) (w: World) =
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
    let private splitCost (pools: PlayerStats) (ctx: Ctx) (db: ModDb) (cost: float) : float * float =
        let share = Mods.sumBase ctx [ Named "CostAsLife" ] db
        let equal = Mods.sumBase ctx [ Named "LifeCostEqualToMana" ] db
        let ofLife = Mods.sumBase ctx [ Named "LifeCostPercentOfLife" ] db
        if share = 0.0 && equal = 0.0 && ofLife = 0.0 then cost, 0.0
        else
            let k = min 1.0 (max 0.0 (share / 100.0))
            cost * (1.0 - k), (cost * k + (if equal > 0.0 then cost else 0.0) + pools.Life * ofLife / 100.0) / Mods.inc ctx [ Named "LifeCostEfficiency" ] db

    let private onReadyUnlocked env (rng: Rng) name (w: World) =
        let actor = actorDef env name
        let st = Map.find name w.Actors
        let v = view env w st
        // what a use of the skill costs now: base x increased cost / (1 + cost efficiency)
        let costOf (skill: Skill) =
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
                else (if st.LastUsed = skill.Name then 0.0 else ownCost) + skill.CostPerSecond * (if partFlag skill "CostOverTimeWithSpeed" then skill.BaseTime else max Constants.serverTick (useTime env actor skill w))
            (ownCost + extraCost) * Mods.mult costCtx [ ManaCost ] db / Mods.inc costCtx [ ManaCostEfficiency ] db
        let affordable (skill: Skill) =
            match actor.Vitals, w.Player with
            | Some pools, Some p when pools.CostsInfernalFlame -> true
            | Some pools, Some p when pools.CostsLife -> (let c = costOf skill in c <= 0.0 || p.Life - c >= 1.0)
            // (enough within rounding: a wait for mana ends with exactly the cost, give or take the last bit)
            | Some pools, Some p ->
                let cost, life = splitCost pools { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name } (skillDb env actor skill st) (costOf skill)
                not (p.Mana < cost - 1e-9 * max 1.0 cost) && (life <= 0.0 || p.Life - life >= 1.0)
            | _ -> true
        // the first step that is ready and can be paid for; if none can be paid, wait for the first
        let choice =
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
                    && not (w.Agonies |> List.exists (fun (a, b, _, _, _, _) -> a = actor.Name && b = s.Name))
                    // a seed still in an enemy is not planted over
                    && not (w.Bombs |> List.exists (fun (a, b, _, _, _, _) -> a = actor.Name && b = s.Name)))
            usable |> List.tryFind (fun step -> affordable (skillOf actor step.Use)) |> Option.orElse (List.tryHead usable)
        match choice with
        | Some step ->
            let skill = skillOf actor step.Use
            // nothing is faster than a server tick (instant skills)
            let t = max Constants.serverTick (useTime env actor skill w)
            // mana cost: base x increased cost / (1 + cost efficiency)
            let db = skillDb env actor skill st
            let costCtx = { Ctx.empty with SkillTags = skill.Tags; SkillName = skill.Name }
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
                else (if st.LastUsed = skill.Name then 0.0 else ownCost) + skill.CostPerSecond * (if partFlag skill "CostOverTimeWithSpeed" then skill.BaseTime else t)
            let cost = (ownCost + extraCost) * Mods.mult costCtx [ ManaCost ] db / Mods.inc costCtx [ ManaCostEfficiency ] db
            // after the last bolt of a load: reload (attack speed and reload speed shorten it;
            // PoB: increases add up, more multiplies), unless the reload is instant
            let reload (w: World) : World * float =
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
            let go (w: World) =
                // the use that opens a window: it lasts so long, and charges again after it
                let w =
                    match window skill with
                    | Some (secs, recharge) when windowShut st w.Time skill ->
                        { w with Counts = bump ("window:" + skill.Name) w.Counts }
                        |> updateActor name (fun st ->
                            { st with Energy = st.Energy |> Map.add ("Window:" + skill.Name) (w.Time + secs) |> Map.add ("WindowNext:" + skill.Name) (w.Time + secs + recharge) })
                    | _ -> w
                let w = startUse env rng actor skill t w
                let w, extra = reload w
                schedule (w.Time + t + extra) (Ready name) w
            // drink the mana flask when mana is getting low and it isn't already running
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
            // what of the cost is paid with life (after the flask, which goes by the whole cost)
            let cost, lifeCost =
                match actor.Vitals with
                | Some v when not v.CostsLife && not v.CostsInfernalFlame -> splitCost v costCtx db cost
                | _ -> cost, 0.0
            match actor.Vitals, w.Player with
            | Some v, Some p when lifeCost > 0.0 && p.Life - lifeCost < 1.0 ->
                // a life cost that would kill: the skill waits (and the life flask is drunk)
                let wait = max Constants.serverTick 0.25
                { w with Player = Some { p with Starved = p.Starved + wait } } |> drinkLifeFlask v |> schedule (w.Time + wait) (Ready name)
            | Some v, Some p when v.CostsLife ->
                // Blood Magic: the cost is paid with life; a skill that would kill waits
                if p.Life - cost >= 1.0 || cost <= 0.0 then
                    { w with Player = Some { p with Life = p.Life - cost } } |> go
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
                go w
            | Some v, Some p when p.Mana < cost - 1e-9 * max 1.0 cost && cost > v.Mana ->
                // costs more than the whole mana pool: this skill can never be used
                { w with Counts = bump ("cannot afford:" + skill.Name) w.Counts }
                |> updateActor name (fun st -> { st with Cooldowns = Map.add skill.Name infinity st.Cooldowns })
                |> schedule (w.Time + Constants.serverTick) (Ready name)
            | Some v, Some p when p.Mana < cost - 1e-9 * max 1.0 cost ->
                // not enough mana: wait until regeneration and leech have covered it
                let rate =
                    v.ManaRegen + (p.Recoups |> List.sumBy (fun (until, rate, pool) -> if pool = "LeechMana" && until > w.Time then rate else 0.0))
                    + (if w.Time < p.FlaskUntil then p.FlaskRate else 0.0)
                let wait = max Constants.serverTick (if rate > 0.0 then (cost - p.Mana) / rate else 1.0)
                { w with Player = Some { p with Starved = p.Starved + wait } } |> schedule (w.Time + wait) (Ready name)
            | Some v, Some p ->
                { w with Player = Some { p with Mana = max 0.0 (p.Mana - cost); Life = p.Life - lifeCost } }
                |> (if lifeCost > 0.0 then drinkLifeFlask v else id) |> sigilSpend name cost v.Mana |> go
            | _ -> go w
        | None ->
            // Nothing usable: wait for the next cooldown, or a server tick.
            let next =
                st.Cooldowns |> Map.toList |> List.map snd |> List.filter (fun t -> t > w.Time && t < infinity)
                |> function
                    | [] -> w.Time + Constants.serverTick
                    | ts -> List.min ts
            schedule next (Ready name) w

    /// The actor is free to act; a stunned or frozen player waits.
    /// Has this melee minion no room at the enemy aimed at? The melee minions in the fight
    /// take their places in order (see Geometry.meleePlaces).
    let private crowdedOut env name (w: World) =
        match (actorDef env name).Body, aimed env w with
        | Some body, Some enemy when body.Melee ->
            let melee =
                env.Fight.Minions
                |> List.filter (fun m -> (m.Body |> Option.exists (fun b -> b.Melee)) && not (Map.find m.Name w.Actors).Gone)
            let places = Geometry.meleePlaces enemy.Def.Size (melee |> List.map (fun m -> m.Body.Value.Size, m.Body.Value.Reach))
            List.zip melee places |> List.exists (fun (m, has) -> m.Name = name && not has)
        | _ -> false

    let private onReady env rng name (w: World) =
        let actor = actorDef env name
        let standing =
            match actor.ActiveWhile with
            | Some buff -> (Map.find env.Fight.Player.Name w.Actors).Buffs |> Map.exists (fun k e -> k = buff && e.Expires > w.Time)
            | None -> true
        match w.Player with
        | Some p when name = env.Fight.Player.Name && p.LockedUntil > w.Time -> schedule p.LockedUntil (Ready name) w
        // a totem that isn't placed waits
        | _ when not standing -> schedule (w.Time + 0.1) (Ready name) w
        // a melee minion that finds no room at the enemy waits for some
        | _ when crowdedOut env name w -> schedule (w.Time + Constants.crowdedWait) (Ready name) { w with Counts = bump "minion:no room" w.Counts }
        | _ -> onReadyUnlocked env rng name w

    /// Statuses the game tracks for "recently" conditions: (name, seconds).
    let private recently hit crit killed =
        [ if hit then yield "HitRecently", 4.0
          if hit && not crit then yield "NonCritRecently", 4.0
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

    /// The player gains rage, up to the maximum; the loss timer starts again.
    let private gainRage (env: Env) (amount: float) (w: World) =
        let name = env.Fight.Player.Name
        let maxRage = Map.tryFind "Rage" env.Fight.Player.MaxResources |> Option.defaultValue 0
        if amount <= 0.0 || maxRage <= 0 then w
        else
            let st = Map.find name w.Actors
            setStacksWith env name false "Rage" (min maxRage (stacksOf st "Rage" + int amount)) w
            |> updateActor name (fun st -> { st with RageAt = w.Time })

    /// One hit on one enemy.
    let private hitEnemy env (rng: Rng) (actor: Actor) (skill: Skill) mult (target: string) (w: World) =
        let e = w.Enemies.[target]
        if e.Life <= 0.0 then
            w
        else
            let st = Map.find actor.Name w.Actors
            let mods = skillDb env actor skill st
            // Isolated: none of its allies within 6 m (keyword text)
            let isolated = alive w |> List.forall (fun o -> o.Def.Name = e.Def.Name || Vec.dist o.Def.Position e.Def.Position > 60.0)
            // Close Range: within 2 m of the player (keyword text)
            let close = Vec.length e.Def.Position - e.Def.Size <= 20.0
            let enemyStatus = status e |> (fun m -> if isolated then Map.add "Isolated" 1 m else m) |> (fun m -> if close then Map.add "Close" 1 m else m)
            // Revealed Weaknesses cover 45% of a rare or unique enemy's life in random segments, and
            // it has an Open Weakness while its life is inside one. Over a kill that is the share of
            // the time spent in those segments, where damage is higher: (c / m) / (1 - c + c / m).
            let enemyStatus =
                let more = Mods.sumBase Ctx.empty [ Named "OpenWeaknessMore" ] mods
                if e.Def.Rarity <> Normal && e.Def.Rarity <> Magic && Mods.flag Ctx.empty (Named "RevealWeakness") mods then
                    let c, m = Constants.revealedWeaknessLifeShare, 1.0 + more / 100.0
                    if rng.Float() < (c / m) / (1.0 - c + c / m) then Map.add "OpenWeakness" 1 enemyStatus else enemyStatus
                else enemyStatus
            let ctx = Skill.ctx skill false (skillStacks actor skill st) enemyStatus |> owned env w
            let hitChance =
                match Mods.override' ctx HitChance mods with
                | Some v -> v
                | None when skill.Tags.Contains Attack ->
                    // attacks roll accuracy against the monster's evasion; spells always hit.
                    // Actors without any accuracy (hand-built) always hit.
                    let accuracy = Mods.total ctx [ Named "Accuracy" ] mods
                    if accuracy <= 0.0 then 100.0
                    else
                        // Blind: 20% less evasion (and accuracy); Maim: 15% reduced evasion (wiki, Blind, Maim)
                        let evasion =
                            Constants.monsterEvasion e.Def.Level * Mods.mult Ctx.empty [ Evasion ] e.Db
                            * (if enemyStatus.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                            * (if enemyStatus.ContainsKey "Maimed" then 1.0 - Constants.maimEvasionReduced / 100.0 else 1.0)
                        let distance = max 0.0 (Vec.length e.Def.Position - e.Def.Size)
                        Hit.accuracyHitChance evasion (Hit.accuracyAtDistance distance accuracy)
                | None -> 100.0
            // a part that only happens against an enemy in some state ("Vs Frozen"): nothing otherwise;
            // one that consumes the state takes it off
            let onlyVs = skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "OnlyVs:" -> Some(n.Substring 7) | _ -> None)
            let consumesVs = skill.Mods |> List.exists (fun m -> m.Stat = Named "ConsumesVs")
            let states = onlyVs |> Option.map (fun (st: string) -> st.Split '|' |> List.ofArray) |> Option.defaultValue []
            // and one that gives way to another part against that state ("NotVs")
            let notVs =
                skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "NotVs:" -> Some(n.Substring 6) | _ -> None)
                |> Option.map (fun (st: string) -> st.Split '|' |> List.ofArray) |> Option.defaultValue []
            let takes = skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "Takes:" -> Some(n.Substring 6) | _ -> None)
            // Hexblast: nothing without a curse that is ripe; the curses it detonates are gone
            let ripe = if partFlag skill "DetonatesCurse" then ripeCurses actor e w.Time else []
            if partFlag skill "DetonatesCurse" && ripe.IsEmpty then w
            else
            let w =
                if ripe.IsEmpty then w
                else
                    updateEnemy target (fun en -> { en with Debuffs = ripe |> List.fold (fun m k -> Map.remove k m) en.Debuffs }) w
                    |> fun w -> { w with Counts = bump "curse detonated" w.Counts }
            // a warp into a projectile explodes there, whatever the enemy's state
            let warped = (Map.find actor.Name w.Actors).Buffs.ContainsKey("Warped:" + skill.Name)
            let w = if warped then updateActor actor.Name (fun st -> { st with Buffs = Map.remove ("Warped:" + skill.Name) st.Buffs }) w else w
            let states, consumesVs = if warped then [], false else states, consumesVs
            if not states.IsEmpty && not (states |> List.exists enemyStatus.ContainsKey) then w
            elif notVs |> List.exists enemyStatus.ContainsKey then w
            else
            let w =
                if consumesVs && not states.IsEmpty then
                    let st = states |> List.find enemyStatus.ContainsKey
                    updateEnemy target (fun en -> { en with Debuffs = Map.remove st en.Debuffs }) w |> fun w -> { w with Counts = bump ("consumed:" + st) w.Counts }
                else w
            let landed, w = lands rng target hitChance w
            if not landed then
                { w with Counts = bump ("miss:" + skill.Name) w.Counts }
            else
            // a hit that takes a state off the enemy without needing it ("Consumes Shock")
            let w =
                match takes with
                | Some st when enemyStatus.ContainsKey st ->
                    updateEnemy target (fun en -> { en with Debuffs = Map.remove st en.Debuffs }) w |> fun w -> { w with Counts = bump ("consumed:" + st) w.Counts }
                | _ -> w
            if false then w
            else
                let def = defender e
                // "Enemies you Curse / Mark take X% increased Damage": additive with what the enemy already takes
                let def =
                    match Mods.sumInc ctx [ Named "EnemyDamageTaken" ] mods with
                    | 0.0 -> def
                    | x -> { def with Mods = ModDb.add def.Mods (Mod.inc (DamageTaken AllDamage) x) }
                // one critical hit roll per use, shared by all its hits; skills that keep hitting
                // on their own (Sustained) roll for every hit
                let critRoll = if skill.Tags.Contains(Tag "Type:Sustained") then None else Map.tryFind skill.Name (Map.find actor.Name w.Actors).CritRolls
                // "If ... that hit would deal a critical hit, the chance to evade is tested again
                // independently. If this check is passed, the critical hit is downgraded to a
                // non-critical hit" (wiki, Evasion)
                let critRoll = if hitChance < 100.0 && rng.Chance(100.0 - hitChance) then Some 1.0 else critRoll
                let hit = Hit.rollWith critRoll rng ctx mods skill def
                // The same hit the other way (a critical hit if it was none, none if it was one),
                // rolled apart from the fight's stream, and with it what the hit deals on average
                // over the two, as a factor on what it did deal. (Not where a critical hit is
                // rolled in a way of its own.)
                let evenly =
                    let chance = if critRoll |> Option.exists (fun r -> r >= 1.0) then 0.0 else min 100.0 (max 0.0 (Hit.critChance ctx mods skill))
                    let was = Dmg.total hit.Dealt
                    if chance <= 0.0 || chance >= 100.0 || was <= 0.0 || Mods.flag ctx (Named "InevitableCrits") mods || Mods.flag ctx (Named "BifurcateCrits") mods then 1.0
                    else
                        let other = Dmg.total (Hit.rollWith (Some(if hit.Crit then 1.0 else 0.0)) (Rng(uint32 (w.Seq % 1000000) + 7919u)) ctx mods skill def).Dealt
                        let p = chance / 100.0
                        (if hit.Crit then p * was + (1.0 - p) * other else p * other + (1.0 - p) * was) / was
                // Culling Strike: an enemy at or below the threshold for its rarity dies, before the hit's damage
                let hit =
                    let cctx = { ctx with Crit = hit.Crit }
                    if Mods.flag cctx (Named "CullingStrike") mods
                       && e.Life <= e.Def.Life * Constants.cullingThreshold (Rarity.name e.Def.Rarity) / 100.0 * Mods.inc cctx [ Named "CullingThreshold" ] mods then
                        { hit with Dealt = Dmg.map (fun _ v -> v + e.Life) hit.Dealt }
                    else hit
                // more attack damage by distance: Close Combat, full within 1 m, none from 3.5 m;
                // Longshot, none within 3.5 m, full from 7 m
                let mult =
                    let near, far = Mods.sumBase ctx [ Named "CloseCombatMore" ] mods, Mods.sumBase ctx [ Named "FarCombatMore" ] mods
                    if near = 0.0 && far = 0.0 then mult
                    else
                        let d = max 0.0 (Vec.length e.Def.Position - e.Def.Size)
                        let closeness = max 0.0 (min 1.0 ((35.0 - d) / 25.0))
                        let farness = max 0.0 (min 1.0 ((d - 35.0) / 35.0))
                        mult * (1.0 + near * closeness / 100.0) * (1.0 + far * farness / 100.0)
                let hit =
                    { hit with
                        Unmitigated = Dmg.map (fun _ v -> v * mult) hit.Unmitigated
                        Dealt = Dmg.map (fun _ v -> v * mult) hit.Dealt }
                // ailments are rolled before the damage lands; a shock the hit applies already
                // counts for the hit itself (user observation)
                // Flammability earlier hits left on the enemy adds to this hit's chance to Ignite,
                // and this hit leaves its own
                let flammable = e.Flammability |> List.filter (fun (_, until) -> until > w.Time)
                let inflicted =
                    let total = flammable |> List.sumBy fst
                    Ailments.fromHit rng ctx mods (if total > 0.0 then { def with Mods = ModDb.add def.Mods (Mod.base' (Named "Flammability") total) } else def) hit
                let w =
                    match Ailments.flammability ctx mods def hit with
                    | f when f > 0.0 -> updateEnemy target (fun en -> { en with Flammability = (f, w.Time + Constants.flammabilityDuration) :: flammable }) w
                    | _ -> w
                // a part that only inflicts (a poison burst): what it would have dealt counts
                // for the ailment, the hit itself deals nothing
                let hit = if Mods.flag ctx (Named "NoHitDamage") mods then { hit with Dealt = Dmg.map (fun _ _ -> 0.0) hit.Dealt; Unmitigated = Dmg.map (fun _ _ -> 0.0) hit.Unmitigated } else hit
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
                (match env.Trace with Some tell -> tell w.Time (if hit.Crit then "crit" else "hit") target skill.Name dealt | None -> ())
                let w =
                    { w with
                        // the enemy as it is now: what the hit consumed stays consumed
                        Enemies = Map.add target { (Map.find target w.Enemies) with Life = (if dead then 0.0 else life) } w.Enemies
                        DamageBy = addf (skill.Name + " (hit)") (min dealt e.Life) w.DamageBy
                        // (only of a hit the enemy outlives: what a killing hit deals is the life that was left)
                        Evened = (if dead then w.Evened else w.Evened + dealt * (evenly - 1.0))
                        Overkill = w.Overkill + max 0.0 (dealt - e.Life)
                        // (a hit that deals more than the enemy has left: each type's share of what was left)
                        DamageByType = (let k = if dealt > e.Life && dealt > 0.0 then e.Life / dealt else 1.0 in hit.Dealt |> Map.fold (fun m t v -> if v > 0.0 then addf (DamageType.name t) (v * k) m else m) w.DamageByType)
                        // "25% of Hit Damage dealt to them while Debuffed": hits are what is recorded
                        Agonies = w.Agonies |> List.map (fun ((o, s, en, recorded, share, until) as a) -> if en = target && until > w.Time then (o, s, en, recorded + min dealt e.Life, share, until) else a)
                        Counts = w.Counts |> bump ("hits:" + skill.Name) |> (if hit.Crit then bump ("crits:" + skill.Name) else id)
                        Shown = (if w.Tracing then Struck(w.Time, target, min dealt e.Life, hit.Crit) :: w.Shown else w.Shown)
                        Kills = if dead then (target, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
                // overkill explosions: nearby enemies are ignited for a share of the overkill
                let w =
                    let overkill = dealt - e.Life
                    if not dead || overkill <= 0.0 || actor.OverkillBurns.IsEmpty then w
                    else
                        actor.OverkillBurns
                        |> List.filter (fun b -> matches b.Filter skill && overkill >= e.Def.Life * b.ThresholdPercent / 100.0)
                        |> List.fold (fun (w: World) b ->
                            match actor.Skills |> List.tryFind (fun x -> x.Name = b.Skill) with
                            | Some sk ->
                                let st = Map.find actor.Name w.Actors
                                let bctx = Skill.ctx sk false (skillStacks actor sk st) Map.empty |> owned env w
                                let bmods = skillDb env actor sk st
                                let radius = match sk.Delivery with | AreaAtTarget r -> Geometry.radius r (Mods.mult bctx [ AreaOfEffect ] bmods) | _ -> 0.0
                                let seconds = sk.Duration * Mods.mult bctx [ SkillDuration ] bmods
                                let dps = overkill * b.PercentPerSecond / 100.0
                                // (the explosion is shown, and its burn is the herald's in the damage shares, not any Ignite)
                                let called = (let k = b.Skill.IndexOf ':' in if k > 0 then b.Skill.Substring(0, k) else b.Skill)
                                let w = if w.Tracing then { w with Shown = Used(w.Time, actor.Name, called, [ Ring(e.Def.Position, radius, 0.0) ]) :: w.Shown } else w
                                alive w
                                |> List.filter (fun o -> o.Def.Name <> e.Def.Name && Vec.dist o.Def.Position e.Def.Position <= radius + o.Def.Size)
                                |> List.fold (fun w o ->
                                    inflict o.Def.Name actor.Name (Dot(Ignite, Fire, dps, seconds, 1)) { w with Counts = bump ("overkill ignite:" + b.Skill) w.Counts }
                                    |> updateEnemy o.Def.Name (fun x -> match x.Dots with d :: rest when d.Label = Ailment.name Ignite -> { x with Dots = { d with Label = called } :: rest } | _ -> x)) w
                            | None -> w) w
                // Armour Break: the hit breaks armour by a share of the damage it dealt; armour at
                // zero is Fully Broken: the enemy takes increased physical damage from hits
                let w =
                    let share =
                        Dmg.get Physical hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfPhysical" ] mods / 100.0
                        + Dmg.get Fire hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfFire" ] mods / 100.0
                        + Dmg.get Chaos hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfChaos" ] mods / 100.0
                        + dealt * Mods.sumBase ctx [ Named "ArmourBreakOfDamage" ] mods / 100.0
                        + Mods.sumBase ctx [ Named "ArmourBreakFlat" ] mods
                    // "Players Break 3 times Armour against Normal Monsters and 2 times Armour against Magic Monsters"
                    let broken =
                        share * Mods.mult ctx [ Named "ArmourBreakAmount" ] mods
                        * (if actor.Name <> env.Fight.Player.Name then 1.0 else match e.Def.Rarity with Normal -> 3.0 | Magic -> 2.0 | _ -> 1.0)
                    if dead || broken <= 0.0 then w
                    else
                        let baseArmour = max 0.0 (Mods.total Ctx.empty [ Armour ] e.Db)
                        let before = Map.tryFind "ArmourBreak" e.Debuffs |> Option.filter (fun x -> x.Expires > w.Time) |> Option.map (fun x -> -(x.Mods |> List.sumBy (fun m -> m.Value))) |> Option.defaultValue 0.0
                        let total = min baseArmour (before + broken)
                        let seconds = Constants.armourBreakDuration * Mods.inc ctx [ Named "ArmourBreakDuration" ] mods
                        let w = addDebuff target "ArmourBreak" seconds 1 [ Mod.base' Armour -total |> Mod.from "Armour Break" ] w
                        if total >= baseArmour then
                            let taken = Constants.fullyBrokenArmourTaken * Mods.inc ctx [ Named "FullyBrokenArmourEffect" ] mods
                            let scopes =
                                if Mods.flag ctx (Named "BrokenArmourAllDamage") mods then [ AllDamage ]
                                else
                                    [ OfType Physical ]
                                    @ (if Mods.flag ctx (Named "BrokenArmourFire") mods then [ OfType Fire ] else [])
                                    @ (if Mods.flag ctx (Named "BrokenArmourColdLightning") mods then [ OfType Cold; OfType Lightning ] else [])
                            let w = addDebuff target "FullyBrokenArmour" seconds 1 (scopes |> List.map (fun sc -> Mod.inc (DamageTaken sc) taken |> Mod.from "Fully Broken Armour")) w
                            // (a break of armour that was not fully broken already)
                            if before < baseArmour then react env rng actor OnFullBreak skill (Some target) w else w
                        else w
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
                        let leechMult = Mods.mult ctx [ Named "ManaLeechAmount" ] mods
                        let extra = extra * leechMult
                        // life: leech from physical attack damage (same one-enemy rule), gain per hit
                        // (of physical damage; "Life Leech recovers based on your Lightning / Elemental
                        // damage as well as Physical damage", "... on your Chaos damage instead of Physical damage")
                        let leechBase =
                            let also t = if Mods.flag ctx (Named("LeechFrom:" + DamageType.name t)) mods then Dmg.get t hit.Dealt else 0.0
                            (if Mods.flag ctx (Named "LeechNotFromPhysical") mods then 0.0 else Dmg.get Physical hit.Dealt) + also Lightning + also Cold + also Fire + also Chaos
                        let lifeLeechFull =
                            min leechBase Constants.effectiveMaxDamageForLeech * Mods.sumBase ctx [ Named "LifeLeech" ] mods / 100.0
                            * Mods.mult ctx [ Named "LifeLeechAmount" ] mods * v.LifeRecoveryRate
                        let lifeLeech = max 0.0 (lifeLeechFull - p.StrikeLifeLeech)
                        let onHit = Mods.sumBase ctx [ Named "LifeOnHit" ] mods * v.LifeRecoveryRate
                        let manaOnHit = Mods.sumBase ctx [ Named "ManaOnHit" ] mods * v.ManaRecoveryRate
                        let lifeCharges =
                            match v.LifeFlask with
                            | Some f when dead -> min f.MaxCharges (p.LifeFlaskCharges + RarityCharges.flaskChargesPerKill e.Def.Rarity * f.ChargeGainMult)
                            | _ -> p.LifeFlaskCharges
                        let w =
                            { w with
                                Player =
                                    Some
                                        { p with
                                            Recoups =
                                                // a leech instance comes back over one second ("Leech Life X% less quickly" stretches it)
                                                (if extra > 0.0 then [ w.Time + Constants.leechSeconds, extra / Constants.leechSeconds, "LeechMana" ] else [])
                                                @ (if lifeLeech > 0.0 && v.Life > 1.0 then
                                                       (let t = Constants.leechSeconds / max 0.05 v.LifeLeechRate
                                                        [ w.Time + t, lifeLeech / t, "LeechLife" ])
                                                   else [])
                                                @ p.Recoups
                                            StrikeLeech = max leech p.StrikeLeech
                                            StrikeLifeLeech = max lifeLeechFull p.StrikeLifeLeech
                                            FlaskCharges = charges
                                            LifeFlaskCharges = lifeCharges
                                            Life = min (max p.Life v.Life) (p.Life + (if v.Life > 1.0 then onHit else 0.0) + (if dead then v.LifeOnKill else 0.0))
                                            EnergyShield = min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap)) (p.EnergyShield + (if dead then v.ESOnKill else 0.0)) } }
                        let w = if manaOnHit > 0.0 then gainMana v "hit" manaOnHit w else w
                        if dead then
                            let kill = (Mods.sumBase Ctx.empty [ ManaOnKill ] mods + v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnKill" ] mods / 100.0) * v.ManaRecoveryRate
                            gainMana v "kill" kill w
                        else w
                    | _ -> w
                // a killing hit still inflicts its ailments (they count for meta energy and Archon);
                // only their damage over time on the dead monster is dropped
                let w =
                    if dead then
                        let w = react env rng actor OnKill skill None w
                        // what the killed enemy was (frozen, shocked...) can set things off
                        status e |> Map.fold (fun w k _ -> react env rng actor (OnKillOf k) skill (Some target) w) w
                    else
                        inflicted
                        |> List.fold
                            (fun w i ->
                                // (a bar that this hit fills: the enemy is Frozen, Electrocuted,
                                // Heavily Stunned or Pinned by it, and was not before)
                                let had = match i with AddBuildup (a, _, _) -> Map.containsKey (Ailment.status a) w.Enemies.[target].Debuffs | _ -> true
                                let w = inflict target actor.Name i w
                                match i with
                                | Dot (a, _, _, _, _) | Status (a, _, _) ->
                                    react env rng actor (OnInflict a) skill (Some target) { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
                                | AddBuildup (a, _, _) when not had && Map.containsKey (Ailment.status a) w.Enemies.[target].Debuffs ->
                                    react env rng actor (OnInflict a) skill (Some target) w
                                | AddBuildup _ -> w)
                            w
                // the skill's own damage over time: one instance per skill and type, the strongest stays
                let w =
                    if dead || skill.Dots.IsEmpty then w
                    else
                        // spell (or attack) damage mods only count if the skill says so
                        let dotCtx = if skill.Tags.Contains(Tag "SpellDot") then ctx else { ctx with SkillTags = ctx.SkillTags |> Set.remove Spell |> Set.remove Attack }
                        let duration = (if skill.Duration > 0.0 then skill.Duration else 1.0) * Mods.mult ctx [ SkillDuration ] mods
                        skill.Dots
                        |> Map.fold
                            (fun w t perSecond ->
                                let label = sprintf "%s (%s over time)" skill.Name ((DamageType.name t).ToLowerInvariant())
                                let dps = perSecond * Hit.damageMult dotCtx mods t * Mods.mult dotCtx [ Named "DotDamage" ] mods
                                let d = { Ailment = None; Label = label; Type = t; Dps = dps; Expires = w.Time + duration; MaxStacks = 1; Owner = actor.Name }
                                updateEnemy target (fun e -> { e with Dots = d :: (e.Dots |> List.filter (fun x -> x.Label <> label || x.Dps > dps)) }) w
                                |> schedule d.Expires Expire)
                            w
                // meta gem energy: crits and newly inflicted freeze / shock / ignite, scaled by
                // monster power and by the share of the ailment threshold the hit deals
                // Glory and Combo: skills that can only be used once enough has been built, count up here
                let w =
                    let needs (s: Skill) = s.Mods |> List.exists (fun m -> match m.Stat with Named n -> n.StartsWith "Requires:" | _ -> false)
                    match actor.Skills |> List.filter needs with
                    | [] -> w
                    | builders ->
                        let power = MonsterPower.ofRarity e.Def.Rarity
                        let before = status e
                        let after = if dead then Map.empty else status w.Enemies.[target]
                        let newly (st: string) = not dead && not (before.ContainsKey st) && after.ContainsKey st
                        let dealt types = Dmg.sumOf types hit.Dealt > 0.0
                        builders |> List.fold (fun w (s: Skill) ->
                            let own name = s.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
                            let ownHit = skill.Name = s.Name || s.Parts |> List.exists (fun p -> p.Name = skill.Name)
                            let glory =
                                if own "Requires:Glory" <= 0.0 || ownHit then 0.0
                                else
                                    power / 100.0
                                    * ((if skill.Tags.Contains Attack then own "Builds:Glory:AttackHit" else 0.0)
                                       + (if dealt DamageType.elemental then own "Builds:Glory:ElementalHit" else 0.0)
                                       + (if dealt [ Chaos ] then own "Builds:Glory:ChaosHit" else 0.0)
                                       + ([ "HeavilyStunned"; "Frozen"; "Ignited" ] |> List.sumBy (fun st -> if newly st then own ("Builds:Glory:" + st) else 0.0)))
                                    + ([ "Ignited"; "Shocked" ] |> List.sumBy (fun st -> if newly st then own ("Builds:GloryFlat:" + st) else 0.0))
                            // Combo: from melee attack hits with other skills
                            let combo =
                                if own "Requires:Combo" <= 0.0 || ownHit || not (skill.Tags.Contains Attack) || not (skill.Tags.Contains(Tag "Type:Melee")) then 0.0
                                else 1.0 + (if rng.Chance(own "ComboExtraChance") then 1.0 else 0.0)
                            // `per`: what the gain is limited by ("each Skill can only generate Glory once
                            // every 0.5 seconds per monster"; Combo "once per strike"); `fade`: what is
                            // lost when nothing was gained for a while
                            let add key amount cap (per: string) every (fade: float -> float -> float) (w: World) =
                                if amount <= 0.0 then w
                                else
                                    let st = Map.find actor.Name w.Actors
                                    let stamp, lastKey = key + "@" + per, key + "@last"
                                    let since k = w.Time - (Map.tryFind k st.Energy |> Option.defaultValue -1e9)
                                    if since stamp < every then w
                                    else
                                        let have = fade (Map.tryFind key st.Energy |> Option.defaultValue 0.0) (since lastKey)
                                        updateActor actor.Name (fun st ->
                                            { st with Energy = st.Energy |> Map.add key (min cap (have + amount)) |> Map.add stamp w.Time |> Map.add lastKey w.Time }) w
                            w
                            // "Skills lose 2 Glory per second if they haven't gained Glory in the past 15 seconds"
                            |> add ("Glory:" + s.Name) glory (own "Requires:Glory" * (1.0 + own "GloryRequiredMore" / 100.0)) target 0.5
                                   (fun have idle -> if idle > 15.0 && idle < 1e8 then max 0.0 (have - 2.0 * (idle - 15.0)) else have)
                            // Combo counts the strikes "in the last 8 seconds" (less, where Combo is lost faster)
                            |> add ("Combo:" + s.Name) combo (own "Requires:Combo") "strike" 1e-9
                                   (let window = match own "ComboWindow" with 0.0 -> Constants.comboWindow | w -> w
                                    fun have idle -> if idle > window && idle < 1e8 then 0.0 else have)) w
                // Banners: attack hits build Glory; a full banner is placed at once
                let w =
                    if actor.Banners.IsEmpty || not (skill.Tags.Contains Attack) then w
                    else
                        let power = MonsterPower.ofRarity e.Def.Rarity
                        actor.Banners |> List.fold (fun w b ->
                            let key = "Glory:" + b.Name
                            // once every 0.5 seconds per monster
                            let stamp = key + "@" + target
                            let fresh = w.Time - (Map.tryFind stamp (Map.find actor.Name w.Actors).Energy |> Option.defaultValue -1e9) >= 0.5
                            let w = if fresh then updateActor actor.Name (fun st -> { st with Energy = Map.add stamp w.Time st.Energy }) w else w
                            let have = (Map.tryFind key (Map.find actor.Name w.Actors).Energy |> Option.defaultValue 0.0) + (if fresh then power * b.PerAttackHit / 100.0 else 0.0)
                            let up = (Map.find actor.Name w.Actors).Buffs |> Map.tryFind ("Banner:" + b.Name) |> Option.exists (fun x -> x.Expires > w.Time)
                            if up then updateActor actor.Name (fun st -> { st with Energy = Map.add key (min b.Glory have) st.Energy }) w
                            elif have >= b.Glory then
                                updateActor actor.Name (fun st ->
                                    { st with
                                        Energy = Map.add key 0.0 st.Energy
                                        Buffs = Map.add ("Banner:" + b.Name) { Name = "Banner:" + b.Name; Mods = b.Mods; Stacks = 1; Expires = w.Time + b.Duration } st.Buffs }) w
                                |> schedule (w.Time + b.Duration) Expire
                                |> fun w -> { w with Counts = bump ("banner:" + b.Name) w.Counts }
                            else updateActor actor.Name (fun st -> { st with Energy = Map.add key have st.Energy }) w) w
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
                                    | AddBuildup (x, pct, _) -> x = a && (Map.tryFind x e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0)
                            else after.ContainsKey (Ailment.status a) && not (before.ContainsKey (Ailment.status a))
                        actor.Metas
                        |> List.fold
                            (fun w meta ->
                                let amount =
                                    meta.Sources
                                    |> List.sumBy (fun src ->
                                        match src with
                                        | CritHits per when hit.Crit -> per * power * ratio DamageType.all
                                        | MeleeHits per when skill.Tags.Contains Melee -> per * power * ratio DamageType.all
                                        | MeleeKills per when dead && skill.Tags.Contains Melee -> per * power
                                        | InflictsAilment (a, per) when newly a -> per * power * ratio (Ailments.scalesFromWith ctx mods a)
                                        | _ -> 0.0)
                                // (what this fills up and triggers is aimed at the enemy that was hit)
                                if amount > 0.0 then { gainEnergy env rng actor meta.Name (amount * meta.EnergyMult) { w with Cause = target } with Cause = w.Cause } else w)
                            w
                // infusion remnants (Siphon Elements): each freeze / shock / ignite inflicted
                let w =
                    match actor.Infusions with
                    | Some inf when not inf.Remnants.IsEmpty ->
                        let newly a =
                            match a with
                            | Freeze ->
                                if dead then
                                    inflicted |> List.exists (function AddBuildup (Freeze, pct, _) -> (Map.tryFind Freeze e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0 | _ -> false)
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
                // Mana Remnants: from a critical hit on, or the kill of, an enemy with an elemental ailment
                let w =
                    match actor.ManaRemnants, w.Player with
                    | Some r, Some p ->
                        let ailing = [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.exists (status e).ContainsKey
                        let fromKill = dead && ailing && rng.Chance r.OnKillChance
                        let fromCrit = hit.Crit && ailing && w.Time - p.LastManaRemnant >= r.CritCooldown
                        if fromKill || fromCrit then
                            let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                            let w = if fromCrit then { w with Player = Some { p with LastManaRemnant = w.Time } } else w
                            { w with Counts = bump "mana remnant" w.Counts } |> schedule (w.Time + r.Delay) (ManaArrives((r.ManaPerRemnant + r.PercentOfMaxMana / 100.0 * (actor.Vitals |> Option.map (fun v -> v.Mana) |> Option.defaultValue 0.0)) * count))
                        else w
                    | _ -> w
                // Life Remnants: from a kill, or from a hit no more than once every so often
                let w =
                    match actor.LifeRemnants, actor.Vitals, w.Player with
                    | Some r, Some v, Some p ->
                        let fromKill = dead && rng.Chance r.OnKillChance
                        let fromHit = w.Time - p.LastLifeRemnant >= r.HitCooldown
                        if fromKill || fromHit then
                            let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                            let rate = (r.LifePerRemnant + r.PercentOfMaxLife / 100.0 * v.Life) * count * v.LifeRecoveryRate / r.Delay
                            { w with
                                Player = Some { p with LastLifeRemnant = (if fromHit then w.Time else p.LastLifeRemnant); Recoups = (w.Time + r.Delay, rate, "Life") :: p.Recoups }
                                Counts = bump "life remnant" w.Counts }
                        else w
                    | _ -> w
                // Trinity: the hit's strongest element gains resonance (up to 100), the others lose some
                let w =
                    let gain = Mods.sumBase ctx [ Named "TrinityGain" ] mods
                    let elemental = DamageType.elemental |> List.map (fun t -> t, Dmg.get t hit.Unmitigated) |> List.filter (fun (_, d) -> d > 0.0)
                    if gain <= 0.0 || elemental.IsEmpty || actor.Name <> env.Fight.Player.Name then w
                    else
                        let top = elemental |> List.maxBy snd |> fst
                        let loss = Mods.sumBase ctx [ Named "TrinityLoss" ] mods
                        updateActor actor.Name (fun st ->
                            let key t = "Resonance:" + DamageType.name t
                            let get t = Map.tryFind (key t) st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                            let values = DamageType.elemental |> List.map (fun t -> t, (if t = top then min 100 (get t + int gain) else max 0 (get t - int loss)))
                            let put k n (b: Map<string, Effect>) = if n > 0 then Map.add k { Name = k; Mods = []; Stacks = n; Expires = infinity } b else Map.remove k b
                            let buffs = values |> List.fold (fun b (t, n) -> put (key t) n b) st.Buffs
                            { st with Buffs = put "Resonance" (values |> List.sumBy snd) buffs }) w
                // charms: charges from kills, and the ones used by killing a rare or unique monster
                let w =
                    if not dead || env.Fight.Player.Charms.IsEmpty then w
                    else
                        let w =
                            { w with
                                Player =
                                    w.Player |> Option.map (fun p ->
                                        { p with
                                            CharmCharges =
                                                env.Fight.Player.Charms |> List.fold (fun (m: Map<string, float>) c ->
                                                    // "granting charges equal to half of the monster's Power"
                                                    let gain = (MonsterPower.ofRarity e.Def.Rarity / 2.0 + (if rng.Chance c.ChargeOnKillChance then 1.0 else 0.0)) * c.ChargeGainMult
                                                    Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + gain)) m) p.CharmCharges }) }
                        if e.Def.Rarity = Rare || e.Def.Rarity = Unique then fst (charm env rng "KillRare" w) else w
                // rage from hitting and from kills
                let w =
                    if actor.Name <> env.Fight.Player.Name then w
                    else
                        let st = Map.find actor.Name w.Actors
                        // "Only one Hit every 0.5 seconds can cause you to gain Rage" (wiki, Rage)
                        let fresh = w.Time - (Map.tryFind "RageHitAt" st.Energy |> Option.defaultValue -1e9) >= 0.5
                        let onHit = if fresh then Mods.sumBase ctx [ Named "RageOnHit" ] mods else 0.0
                        let w = if onHit > 0.0 then updateActor actor.Name (fun st -> { st with Energy = Map.add "RageHitAt" w.Time st.Energy }) w else w
                        let _, _, _, _, onKill = env.Rage
                        gainRage env (onHit + (if dead then onKill else 0.0)) w
                // Knockback: a hit that knocks back pushes the monster straight away from the player
                // (a boss is not moved). How far: the skill's own distance, or a metre where it names
                // none, x "increased Knockback Distance"; skills that knock back "based on how close
                // they are to you" push more from close by and less towards their limit. It walks
                // back like any monster that is out of its reach. Walls are not in its way.
                let w =
                    let kctx = { ctx with Crit = hit.Crit }
                    let chance = if dead || e.Def.Rarity = Unique then 0.0 else Mods.sumBase kctx [ Named "KnockbackChance" ] mods
                    if chance > 0.0 && not (Mods.flag kctx (Named "CannotKnockback") mods) && rng.Chance chance then
                        let pos = e.Def.Position
                        let d = Vec.length pos
                        let limit = Mods.sumBase kctx [ Named "KnockbackLimit" ] mods
                        let byDistance =
                            if limit <= 0.0 then 1.0
                            else
                                let k = min 1.0 (max 0.0 (d - e.Def.Size) / limit)
                                1.0 + (Mods.sumBase kctx [ Named "KnockbackAtMin" ] mods * (1.0 - k) + Mods.sumBase kctx [ Named "KnockbackAtMax" ] mods * k) / 100.0
                        let own = Mods.sumBase kctx [ Named "KnockbackBase" ] mods
                        let far = (if own > 0.0 then own / Constants.knockbackUnitsPerDistance else Constants.knockbackDefault) * Mods.mult kctx [ Named "KnockbackDistance" ] mods * byDistance
                        if far <= 0.0 then w
                        else
                            let pushed = if d > 1e-9 then Vec.scale ((d + far) / d) pos else Vec.make far 0.0
                            { updateEnemyDef target pushed w with Counts = bump "knockback" w.Counts }
                    else w
                let w = markRecently actor.Name true hit.Crit dead w
                let w = react env rng actor OnHit skill (Some target) w
                if hit.Crit then react env rng actor OnCrit skill (Some target) w else w

    /// "Gain X% of Damage as Extra Chaos Damage per 20 Unreserved Darkness": the Darkness that
    /// is not reserved now, as the stacks of a buff the player's hits ask about. Looked at when
    /// a strike of the player's is due (a reservation ends by itself, at no event), where the
    /// character has the line (the resource "UnreservedDarkness" is among its maxima then).
    let private syncDarkness (env: Env) (actor: Actor) (w: World) =
        match actor.Vitals, w.Player with
        | Some pools, Some p when pools.Darkness > 0.0 && actor.MaxResources.ContainsKey "UnreservedDarkness" ->
            let live = p.Darkness |> List.filter (fun (until, _) -> until > w.Time)
            let n = int (floor (max 0.0 (pools.Darkness - (live |> List.sumBy snd))))
            match Map.tryFind "UnreservedDarkness" (Map.find actor.Name w.Actors).Buffs with
            | Some e when e.Stacks = n -> w
            | _ -> updateActor actor.Name (fun st -> { st with Buffs = Map.add "UnreservedDarkness" { Name = "UnreservedDarkness"; Mods = []; Stacks = n; Expires = infinity } st.Buffs }) w
        | _ -> w

    let private onStrike env (rng: Rng) name skillName mult aimedAt (elapsed: float option) (direct: bool) (w: World) =
        let actor = actorDef env name
        let skill = skillOf actor skillName
        let w = syncDarkness env actor w
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
                let ctx = Skill.ctx skill false (skillStacks actor skill st) Map.empty |> owned env w
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
                                Count =
                                    p.Count + fromCharges
                                    + (if p.Count + fromCharges > 0 && not (skill.Tags.Contains(Tag "Type:ProjectilesNumberModifiersNotApplied")) then
                                           int (Mods.sumBase ctx [ AdditionalProjectiles ] mods)
                                           + (if rng.Chance(Mods.sumBase ctx [ Named "ExtraProjectileChance2" ] mods) then 2 else 0)
                                           + (let c = Mods.sumBase ctx [ Named "ExtraProjectileChance1" ] mods
                                              int (c / 100.0) + (if rng.Chance(c - 100.0 * floor (c / 100.0)) then 1 else 0))
                                       else 0)
                                Pierce = p.Pierce + int (Mods.sumBase ctx [ Pierce ] mods) + (if rng.Chance(Mods.sumBase ctx [ Named "PierceChance" ] mods) then 1 else 0)
                                Chains = (if Mods.flag ctx (Named "CannotChain") mods then 0 else p.Chains + int (Mods.sumBase ctx [ Chains ] mods))
                                TerrainChain = p.TerrainChain + Mods.sumBase ctx [ Named "TerrainChain" ] mods
                                // "Projectiles Fork" (a chance of 100%), "Fork an additional time"
                                Forks =
                                    (let chance = Mods.sumBase ctx [ Named "ForkChance" ] mods
                                     if Mods.flag ctx (Named "CannotFork") mods || not (rng.Chance chance) then 0
                                     else p.Forks + 1 + int (Mods.sumBase ctx [ Named "AdditionalForks" ] mods))
                                ForkExtraChance = p.ForkExtraChance + Mods.sumBase ctx [ Named "ForkExtraChance" ] mods
                                ForkAngle = p.ForkAngle * Mods.inc ctx [ Named "ForkAngle" ] mods }
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
                        // "Gain a Power Charge when you consume an Elemental Infusion"
                        let w =
                            let n = int (Mods.sumBase Ctx.empty [ Named "PowerChargeOnInfusionConsumed" ] (skillDb env actor infused st))
                            if n > 0 then schedule w.Time (GainStacks(name, "PowerCharge", n)) w else w
                        if rng.Chance keep then infused, w
                        else infused, setStacksWith env name false r (stacksOf st r - 1) w
            let w = { w with Player = w.Player |> Option.map (fun p -> { p with StrikeLeech = 0.0; StrikeLifeLeech = 0.0 }) }
            // Spell Cascade: extra impacts on either side of the target "along a randomly chosen
            // axis"; side impacts have their own less area; spacing: user observation
            let ctx0, mods0, _, _ = shape skill
            let cascades = int (Mods.sumBase ctx0 [ Named "CascadesPerSide" ] mods0)
            // Triskelion Cascade: the empowered spell "becomes centred on your location, and
            // Cascades to six areas around you in a Triskelion pattern": three arms a third of a
            // turn apart, so many impacts along each, spaced like Spell Cascade's (the pattern's
            // measures are not in the data). The empowerment is used up.
            let arms = int (Mods.sumBase ctx0 [ Named "TriskelionArms" ] mods0)
            match skill.Delivery with
            | AreaAtTarget _ | AreaAroundSelf _ when arms > 0 ->
                let sk, w = variant w
                let _, _, areaMult, delivery = shape sk
                let r = match delivery with AreaAtTarget r | AreaAroundSelf r -> Geometry.radius r areaMult | _ -> 0.0
                let turn = rng.Float() * 2.0 * System.Math.PI
                let centres =
                    Vec.make 0.0 0.0
                    :: [ for a in 0 .. 2 do
                            for k in 1 .. arms ->
                                Vec.scale (float k * r * env.Fight.CascadeSpacing) (Vec.ofAngle (turn + float a * 2.0 * System.Math.PI / 3.0)) ]
                let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, centres |> List.map (fun c -> Ring(c, r, 0.0))) :: w.Shown } else w
                let w =
                    centres
                    |> List.fold
                        (fun w centre ->
                            living
                            |> List.filter (fun e -> Vec.dist centre e.Position <= r + e.Size)
                            |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w)
                        w
                updateActor actor.Name (fun st ->
                    { st with Buffs = st.Buffs |> Map.filter (fun _ e -> not (e.Mods |> List.exists (fun m -> m.Stat = Named "TriskelionArms"))) }) w
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
                        let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(centre, rr, 0.0) ]) :: w.Shown } else w
                        let w =
                            living
                            |> List.filter (fun e -> Vec.dist centre e.Position <= rr + e.Size)
                            |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w
                        w, step)
                    (w, 0.0)
                |> fst
            | _ ->
                let sk, w = variant w
                let ctx, mods, areaMult, delivery = shape sk
                // a bolt that leaves now (Frostbolt): when, and towards where it was sent
                let w =
                    if Mods.flag ctx (Named "LaunchesBolt") mods && not direct && elapsed.IsNone then
                        updateActor name (fun st -> { st with Energy = st.Energy |> Map.add "BoltAt" w.Time |> Map.add "BoltAngle" (Vec.angle primary.Position) }) w
                    else w
                // "Casting Ice Nova targeting near a Frostbolt Projectile will cause it to originate
                // from the Frostbolt instead of you": the bolt sent last, while it flies, if it is
                // nearer the enemy aimed at than the player is
                let novaAt =
                    match delivery with
                    | AreaAroundSelf _ ->
                        let speed = Mods.sumBase ctx [ Named "NovaOnBoltSpeed" ] mods
                        let st = Map.find name w.Actors
                        match Map.tryFind "BoltAt" st.Energy, Map.tryFind "BoltAngle" st.Energy with
                        | Some t0, Some angle when speed > 0.0 && w.Time - t0 >= 0.0 && w.Time - t0 < Mods.sumBase ctx [ Named "NovaOnBoltLife" ] mods ->
                            let at = Vec.scale (speed * (w.Time - t0)) (Vec.ofAngle angle)
                            if Vec.dist at primary.Position < Vec.length primary.Position then Some at else None
                        | _ -> None
                    | _ -> None
                let w = if novaAt.IsSome then { w with Counts = bump ("on bolt:" + sk.Name) w.Counts } else w
                let strike (w: World) =
                    match delivery, elapsed with
                    | AreaAroundSelf r, _ when novaAt.IsSome ->
                        let at, rr = novaAt.Value, Geometry.radius r areaMult
                        let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(at, rr, 0.0) ]) :: w.Shown } else w
                        living |> List.filter (Geometry.within at rr) |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w
                    | _ when direct ->
                        let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Line(Vec.zero, primary.Position) ]) :: w.Shown } else w
                        (if primary.Name = aimedAt then hitEnemy env rng actor sk mult aimedAt w else w)
                    | Projectiles p, None when p.Passing > 0.0 && p.HitInterval > 0.0 ->
                        // what is inside the projectiles as they travel: every hit at its own time
                        let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Flight [ Vec.zero; primary.Position ] ]) :: w.Shown } else w
                        Geometry.dwellHits rng env.Fight.Walls p env.Fight.Reach areaMult primary living
                        |> List.fold (fun w (t, target) ->
                            if t <= 0.0 then hitEnemy env rng actor sk mult target w
                            else schedule (w.Time + t) (DirectHit(name, skillName, mult, target)) w) w
                    | Projectiles p, Some t when p.Passing > 0.0 ->
                        // where the projectile is by now, on its way towards where it was aimed
                        let at = Vec.scale (p.Speed * t) (Vec.ofAngle (Vec.angle primary.Position))
                        let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(at, Geometry.radius p.Passing areaMult, 0.0) ]) :: w.Shown } else w
                        living |> List.filter (Geometry.within at (Geometry.radius p.Passing areaMult)) |> List.map (fun e -> e.Name)
                        |> List.fold (fun w target -> hitEnemy env rng actor sk mult target w) w
                    | _ ->
                        // forked projectiles: "Forked Projectiles deal N% less damage" (Fork)
                        let forkMult = mult * Mods.mult ctx [ Named "ForkedDamage" ] mods
                        let hit, forked, w =
                            if w.Tracing then
                                let hit, forked, marks = Geometry.targetsShown rng env.Fight.Walls env.Fight.Reach delivery areaMult primary living
                                hit, forked, { w with Shown = Used(w.Time, name, sk.Name, marks) :: w.Shown }
                            else
                                let hit, forked = Geometry.targetsParted rng env.Fight.Walls env.Fight.Reach delivery areaMult primary living
                                hit, forked, w
                        let w = hit |> List.fold (fun w target -> hitEnemy env rng actor sk mult target w) w
                        forked |> List.fold (fun w target -> hitEnemy env rng actor sk forkMult target w) w
                let w = strike w
                // Aftershock: a slam has a chance to hit its area once more
                if sk.Tags.Contains Slam && rng.Chance(Mods.sumBase ctx [ Named "AftershockChance" ] mods) then
                    // (while it strikes the actor has "Aftershock": for what only an Aftershock's hits do)
                    let again =
                        updateActor actor.Name (fun st -> { st with Buffs = Map.add "Aftershock" { Name = "Aftershock"; Mods = []; Stacks = 1; Expires = infinity } st.Buffs }) w
                        |> strike
                        |> updateActor actor.Name (fun st -> { st with Buffs = Map.remove "Aftershock" st.Buffs })
                    { again with Counts = bump ("aftershock:" + sk.Name) again.Counts }
                else w

    /// When killed minions come back: all together, `seconds` after the first death. Each
    /// further death while they wait adds less: the n-th adds a n-th of the time, and the wait
    /// is never more than the full time from now ("each successive minion that dies increases
    /// it by less and less (still capped to a max of 7.5 seconds)", 0.2.0e patch notes; the
    /// n-th share is the wiki's). Before 0.2.0e every death started the full wait anew.
    /// `waiting`: the time set so far and the deaths counted.
    let nextRevival (now: float) (seconds: float) (waiting: (float * int) option) : float * int =
        match waiting with
        | Some (at, n) when at > now -> min (now + seconds) (at + seconds / float (n + 1)), n + 1
        | _ -> now + seconds, 1

    /// A minion is killed: it leaves the fight; a reviving one comes back with the others
    /// once none has died for its revive time.
    let private killMinion env (rng: Rng) (minion: Actor) (w: World) =
        let body = minion.Body.Value
        let player = env.Fight.Player
        let w = leave minion.Name w
        let w = { w with Counts = bump "minion:died" w.Counts } |> markStatus player.Name "MinionDiedRecently"
        match body.Revive with
        | None -> w
        | Some seconds ->
            // "gains Energy when one of your Persistent Minions is Killed"
            let w =
                player.Metas
                |> List.fold
                    (fun w meta ->
                        let amount = meta.Sources |> List.sumBy (fun src -> match src with MinionDeaths per -> per * body.Power | _ -> 0.0)
                        if amount > 0.0 then gainEnergy env rng player meta.Name (amount * meta.EnergyMult) w else w)
                    w
            let at, n = nextRevival w.Time seconds w.Revival
            { w with Revival = Some(at, n) } |> schedule at (ReviveMinions at)

    let private reviveMinions env (at: float) (w: World) =
        match w.Revival with
        | Some (t, _) when abs (t - at) < 1e-9 ->
            env.Fight.Minions
            |> List.fold
                (fun w m ->
                    match m.Body with
                    | Some b when b.Revive.IsSome && (Map.find m.Name w.Actors).Gone ->
                        updateActor m.Name (fun st -> { st with Gone = false; Life = b.Life; Shield = b.EnergyShield; HitAt = w.Time }) w
                        |> schedule w.Time (Ready m.Name)
                        |> fun w -> { w with Counts = bump "minion:revived" w.Counts }
                    | _ -> w)
                { w with Revival = None; Boned = Set.empty }
        | _ -> w

    /// Mitigated damage onto a minion: energy shield first (chaos removes twice as much),
    /// then life. Before that it has recovered since it was last hit: its life regeneration,
    /// and its energy shield recharge once the delay has passed.
    let private hurtMinion env (rng: Rng) (minion: Actor) (damage: Map<DamageType, float>) (w: World) =
        let body = minion.Body.Value
        let st = Map.find minion.Name w.Actors
        let since = w.Time - max 0.0 st.HitAt
        let life = min body.Life (st.Life + body.LifeRegen * since)
        let shield =
            let recharging = since - Constants.energyShieldRechargeDelay
            if recharging > 0.0 then min body.EnergyShield (st.Shield + body.EnergyShield * Constants.energyShieldRechargePercent / 100.0 * recharging) else st.Shield
        let shield, rest =
            damage
            |> Map.fold
                (fun (es, rest) t d ->
                    let k = if t = Chaos then 2.0 else 1.0
                    let absorbed = min d (es / k)
                    es - absorbed * k, rest + d - absorbed)
                (shield, 0.0)
        let life = life - rest
        let w = updateActor minion.Name (fun st -> { st with Life = life; Shield = shield; HitAt = w.Time }) w
        match body.Explodes, aimed env w with
        // "explode when reduced to Low Life" (35%): its explosion strikes, and it is dead
        | Some skill, Some enemy when life <= 0.35 * body.Life ->
            onStrike env rng minion.Name skill 1.0 enemy.Def.Name None false { w with Counts = bump "minion:exploded" w.Counts }
            |> killMinion env rng minion
        | _ -> if life <= 0.0 then killMinion env rng minion w else w

    /// A monster's hit at the player: evasion, deflection, armour, resistances, then
    /// energy shield and life. Frozen / stunned / electrocuted monsters don't act; chill
    /// slows them; blind lowers their accuracy.
    let private attackPlayer env (rng: Rng) name (w: World) =
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
            // curses on the monster: action speed (Temporal Chains), damage dealt (Enfeeble)
            let debuffs = (defender e).Mods
            let actionSpeed = max 0.1 (Mods.more Ctx.empty [ Named "ActionSpeed" ] debuffs)
            let dealt = max 0.0 (Mods.more Ctx.empty [ Named "DamageDealt" ] debuffs)
            let atk = { atk with Damage = atk.Damage |> Map.map (fun _ d -> d * dealt) |> Vitals.withExtraChaos (Mods.sumBase Ctx.empty [ Named "GainAsChaos" ] debuffs) }
            let next = atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed
            let w = schedule (w.Time + next) (EnemyAttack name) w
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            if stopped then
                { w with Counts = bump "enemy:stopped" w.Counts }
            elif not inRange then
                w
            else
                let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0) * (if st.ContainsKey "Marked" then v.MarkedEnemyAccuracyMult else 1.0)
                let hitChance =
                    let h = Vitals.monsterHitChance v.Evasion accuracy
                    // Glancing Blows: the evade roll is unlucky (both of two rolls must evade)
                    if v.UnluckyEvade then 100.0 - (100.0 - h) * (100.0 - h) / 100.0 else h
                let landed, w = lands rng "Player" hitChance w
                if not landed then
                    { w with Counts = bump "enemy:evaded" w.Counts }
                else
                    // "Recover X% of Missing Life before being Hit by an Enemy"
                    let w =
                        match w.Player with
                        | Some p when v.MissingLifeBeforeHit > 0.0 && p.Life < v.Life -> { w with Player = Some { p with Life = p.Life + (v.Life - p.Life) * v.MissingLifeBeforeHit / 100.0 } }
                        | _ -> w
                    let blockRoll () = rng.Chance v.Block
                    let blocked = blockRoll () || (v.LuckyBlock && blockRoll ())
                    let deflectChance =
                        let d = Vitals.deflectChance v.Deflection accuracy
                        if v.LuckyDeflect then 100.0 - (100.0 - d) * (100.0 - d) / 100.0 else d
                    let deflected = not blocked && rng.Chance deflectChance
                    // "<from> damage taken as <to>" moves part of each type before mitigation
                    let incoming =
                        v.TakenAs
                        |> List.fold
                            (fun (m: Map<DamageType, float>) (from, into, pct) ->
                                let moved = (Map.tryFind from atk.Damage |> Option.defaultValue 0.0) * pct / 100.0
                                m |> Map.add from ((Map.tryFind from m |> Option.defaultValue 0.0) - moved)
                                  |> Map.add into ((Map.tryFind into m |> Option.defaultValue 0.0) + moved))
                            atk.Damage
                    // a critical hit is tested against evasion once more: evaded, it is an ordinary hit
                    let crit = rng.Chance(atk.CritChance * v.EnemyCritChanceMult * (if st.ContainsKey "Blinded" then v.BlindedEnemyCritMult else 1.0) * (if st.ContainsKey "Marked" then v.MarkedEnemyCritMult else 1.0)) && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
                    // "Defend with X% of Armour against Projectile Attacks / Critical Hits"
                    let armourShare = (if atk.Melee then 1.0 else v.ArmourVsProjectiles) * (if crit then v.ArmourVsCrits else 1.0) * v.ArmourDefend
                    let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 * (1.0 - v.LessCritDamageTaken / 100.0) * (if st.ContainsKey "Blinded" then v.BlindedEnemyCritBonusMult else 1.0) else 1.0
                    let playerSt = Map.find env.Fight.Player.Name w.Actors
                    let shocked =
                        Map.tryFind "Shocked" playerSt.Buffs |> Option.filter (fun x -> x.Expires > w.Time)
                        |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
                    let unmitigated = incoming |> Map.map (fun _ d -> d * critMult)
                    let taken =
                        unmitigated
                        |> Map.map (fun t d ->
                            let d = if deflected then d * (1.0 - v.DeflectEffect / 100.0) else d
                            let d =
                                match t with
                                | Physical ->
                                    let d = if atk.Melee then max 0.0 (d - v.FlatPhysicalTaken) else d
                                    Vitals.physicalLeft v (v.Armour * armourShare) (match w.Player with Some p -> p.EnergyShield | None -> 0.0) d
                                | t ->
                                    let d = d * (1.0 - (Map.tryFind t v.Resist |> Option.defaultValue 0.0) / 100.0)
                                    // "X% of Armour also applies to <type> damage"
                                    match Map.tryFind t v.ArmourAppliesTo with
                                    | Some pct -> d * (1.0 - Vitals.armourReduction (v.Armour * armourShare * pct / 100.0) d / 100.0)
                                    | None -> d
                            d * (Map.tryFind t v.TakenMult |> Option.defaultValue 1.0) * (1.0 + shocked / 100.0)
                              * (if blocked then v.BlockedDamageTaken / 100.0 else 1.0))
                    // Scarred Faith: "5% of Physical Damage prevented Recouped as Energy Shield per
                    // enemy Power": what mitigation kept of the hit's physical damage
                    let preventedRecoup =
                        if v.RecoupPreventedPerPower <= 0.0 then 0.0
                        else
                            let prevented = max 0.0 ((Map.tryFind Physical unmitigated |> Option.defaultValue 0.0) - (Map.tryFind Physical taken |> Option.defaultValue 0.0))
                            prevented * v.RecoupPreventedPerPower / 100.0 * MonsterPower.ofRarity e.Def.Rarity * v.ESRecoveryRate
                    // "X% of Physical Damage prevented Recouped as Life"
                    let preventedLife =
                        if v.RecoupPreventedLife <= 0.0 then 0.0
                        else
                            let prevented = max 0.0 ((Map.tryFind Physical unmitigated |> Option.defaultValue 0.0) - (Map.tryFind Physical taken |> Option.defaultValue 0.0))
                            prevented * v.RecoupPreventedLife / 100.0 * v.LifeRecoveryRate
                    // "x% of Damage from Hits is taken from your Companion's Life before you": the
                    // first minion that does so and is alive takes its share, as much as it has
                    let taken, w =
                        let guard =
                            env.Fight.Minions |> List.tryFind (fun m ->
                                (m.Body |> Option.exists (fun b -> b.TakesForOwner > 0.0)) && not (Map.find m.Name w.Actors).Gone)
                        match guard with
                        | Some m ->
                            let share = m.Body.Value.TakesForOwner / 100.0
                            let its = Map.find m.Name w.Actors
                            let total = taken |> Map.toList |> List.sumBy snd
                            let k = if total <= 0.0 then 0.0 else min share ((its.Life + its.Shield) / total)
                            taken |> Map.map (fun _ d -> d * (1.0 - k)),
                            hurtMinion env rng m (taken |> Map.map (fun _ d -> d * k)) { w with Counts = bump "minion:took for owner" w.Counts }
                        | None -> taken, w
                    let p = w.Player |> Option.defaultValue p
                    let w = if w.Tracing then { w with Shown = Hurt(w.Time, name, taken |> Map.fold (fun s _ v -> s + v) 0.0) :: w.Shown } else w
                    let w = takeDamage true pools taken { w with Player = Some p } |> fun w -> { w with Player = w.Player |> Option.map (fun p -> { p with LastHit = w.Time }) }
                    let w =
                        if preventedRecoup > 0.0 then
                            { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + v.RecoupSeconds, preventedRecoup / v.RecoupSeconds, "ES") :: p.Recoups }) }
                        else w
                    let w =
                        if preventedLife > 0.0 then
                            { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + v.RecoupSeconds, preventedLife / v.RecoupSeconds, "Life") :: p.Recoups }) }
                        else w
                    let w = (let _, _, _, whenHit, _ = env.Rage in gainRage env whenHit w)
                    let w = (match env.Fight.Player.Skills |> List.tryHead with Some any -> react env rng env.Fight.Player WhenHit any (Some name) w | None -> w)
                    let w = (match env.Fight.Player.Skills |> List.tryHead with Some any when atk.Melee -> react env rng env.Fight.Player WhenHitMelee any (Some name) w | _ -> w)
                    // "Rage loss is paused for 4 seconds upon gaining Rage, or after taking damage"
                    let w = updateActor env.Fight.Player.Name (fun st -> { st with RageAt = w.Time }) w
                    // charms used when a hit of their damage type is taken (they help from the next hit on)
                    let w = taken |> Map.fold (fun w t d -> if d > 0.0 then fst (charm env rng ("Hit:" + DamageType.name t) w) else w) w
                    let w = if crit then { w with Counts = bump "enemy:crit" w.Counts } else w
                    let w =
                        updateActor env.Fight.Player.Name (fun st ->
                            { st with Buffs = Map.add "BeenHitRecently" { Name = "BeenHitRecently"; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
                        |> schedule (w.Time + 4.0) Expire
                    // ailments and stuns the hit inflicts on the player (not when blocked)
                    let w =
                        if blocked then w
                        else
                            let get t = Map.tryFind t unmitigated |> Option.defaultValue 0.0
                            let avoided a = rng.Chance(Map.tryFind a v.Avoid |> Option.defaultValue 0.0)
                            let effect a = Map.tryFind a v.SelfEffect |> Option.defaultValue 1.0
                            let lasts a = Map.tryFind a v.SelfDuration |> Option.defaultValue 1.0
                            let thr = max 1.0 v.AilmentThreshold
                            let playerName = env.Fight.Player.Name
                            let buff name mods duration (w: World) =
                                updateActor playerName (fun st -> { st with Buffs = Map.add name { Name = name; Mods = mods; Stacks = 1; Expires = w.Time + duration } st.Buffs }) w
                                |> schedule (w.Time + duration) Expire
                                |> fun w -> { w with Counts = bump ("player:" + name) w.Counts }
                            let lock seconds what (w: World) =
                                { w with
                                    Player = w.Player |> Option.map (fun p -> { p with LockedUntil = max p.LockedUntil (w.Time + seconds) })
                                    Counts = bump ("player:" + what) w.Counts }
                            // a charm for this may stop it (and is used up doing so)
                            let unlessCharm key (apply: World -> World) (w: World) =
                                let w, prevented = charm env rng key w
                                if prevented then { w with Counts = bump ("charm prevented:" + key) w.Counts } else apply w
                            // ignite: strongest only
                            let w =
                                let fire = get Fire
                                if fire > 0.0 && rng.Chance(min 100.0 (fire / thr * Constants.igniteChanceMultiplier)) && not (avoided "Ignite") then
                                  w |> unlessCharm "Ignited" (fun w ->
                                    let dps =
                                        fire * Constants.ignitePercentPerSecond / 100.0 * effect "Ignite"
                                        * (1.0 - (Map.tryFind Fire v.Resist |> Option.defaultValue 0.0) / 100.0) * (Map.tryFind Fire v.TakenMult |> Option.defaultValue 1.0)
                                    let until = w.Time + Constants.igniteDuration * lasts "Ignite"
                                    { w with
                                        Player =
                                            w.Player |> Option.map (fun p ->
                                                let stronger = p.Dots |> List.exists (fun (u, d, t) -> t = Fire && u > w.Time && d >= dps)
                                                if stronger then p else { p with Dots = (until, dps, Fire) :: (p.Dots |> List.filter (fun (_, _, t) -> t <> Fire)) })
                                        Counts = bump "player:Ignited" w.Counts })
                                else w
                            let w =
                                let lightning = get Lightning
                                if lightning > 0.0 && rng.Chance(min 100.0 (lightning / thr * Constants.shockChanceMultiplier)) && not (avoided "Shock") then
                                    w |> unlessCharm "Shocked" (buff "Shocked" [ Mod.base' (Named "ShockTaken") (Constants.baseShockMagnitude * effect "Shock") ] (Constants.playerShockDuration * lasts "Shock"))
                                else w
                            let w =
                                let cold = get Cold
                                let chill = Constants.chillEffectMultiplier * cold / thr * effect "Chill"
                                let w =
                                    if chill >= 1.0 && not (avoided "Chill") then
                                        w |> unlessCharm "Slowed" (buff "Chilled" [ Mod.more Speed (-(min Constants.chillMaxEffect (max Constants.chillMinEffect chill))) ] (Constants.playerChillDuration * lasts "Chill"))
                                    else w
                                // freeze builds up with cold damage; full: frozen
                                match w.Player with
                                | Some p when cold > 0.0 && not (avoided "Freeze") ->
                                    let buildup = p.FreezeBuildup + cold * Constants.playerFreezeScale / max 1.0 v.FreezeThreshold * 100.0
                                    if buildup >= 100.0 then
                                        { w with Player = Some { p with FreezeBuildup = 0.0 } } |> unlessCharm "Frozen" (lock (Constants.playerFreezeDuration * lasts "Freeze") "Frozen")
                                    else { w with Player = Some { p with FreezeBuildup = buildup } }
                                | _ -> w
                            // light stun: chance from the hit's size against the stun threshold
                            let stunDamage =
                                unmitigated |> Map.toList
                                |> List.sumBy (fun (t, d) -> if t = Physical then d * (1.0 + Constants.monsterPhysicalStunMore / 100.0) else d)
                                |> fun d -> if atk.Melee then d * (1.0 + Constants.monsterMeleeStunMore / 100.0) else d
                            let stunChance = min 100.0 (stunDamage / max 1.0 v.StunThreshold * 100.0)
                            // players are not heavily stunned by ordinary hits (only while raising a shield,
                            // parrying or mounted: not modelled)
                            if stunChance >= Constants.playerLightStunMinimum && rng.Chance stunChance && not (rng.Chance v.AvoidStun) then
                                w |> unlessCharm "Stunned" (fun w ->
                                    lock (Constants.playerLightStunDuration / v.StunRecovery) "Stunned" w
                                    // "if you've been Stunned Recently"; "Regenerate X% of maximum Life over 1 second when Stunned"
                                    |> markStatus env.Fight.Player.Name "StunnedRecently"
                                    |> fun w ->
                                        if v.RegenOnStun <= 0.0 then w
                                        else
                                            let rate = v.RegenOnStun / 100.0 * pools.Life / Constants.stunRegenSeconds
                                            { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + Constants.stunRegenSeconds, rate, "Life") :: p.Recoups }) })
                            else w
                    let w =
                        { w with
                            Counts = w.Counts |> bump "enemy:hit" |> (if deflected then bump "enemy:deflected" else id) |> (if blocked then bump "enemy:blocked" else id) }
                    let w =
                        match w.Player with
                        | Some p when blocked ->
                            { w with
                                Player =
                                    Some
                                        { p with
                                            Ward = (if v.WardOnBlock > 0.0 then min (max p.Ward pools.Ward) (p.Ward + v.WardOnBlock) else p.Ward)
                                            Life = min (max p.Life pools.Life) (p.Life + v.LifeOnBlock)
                                            EnergyShield =
                                                (if v.ESOnBlock = 0.0 then p.EnergyShield
                                                 else max 0.0 (min (max p.EnergyShield (min pools.EnergyShield v.ESRecoverCap)) (p.EnergyShield + v.ESOnBlock)))
                                            Mana = min pools.Mana (p.Mana + v.ManaOnBlock) } }
                        | _ -> w
                    if blocked then
                        // blocks feed meta gems (Cast on Block) and OnBlock rules
                        let player = env.Fight.Player
                        let w =
                            player.Metas
                            |> List.fold
                                (fun w meta ->
                                    let amount = meta.Sources |> List.sumBy (function Blocks e -> e | _ -> 0.0)
                                    // (what a block triggers is aimed at the enemy whose hit was blocked)
                                    if amount > 0.0 then { gainEnergy env rng player meta.Name (amount * meta.EnergyMult) { w with Cause = name } with Cause = w.Cause } else w)
                                w
                        match player.Skills |> List.tryHead with
                        | Some anySkill -> react env rng player OnBlock anySkill (Some name) w
                        | None -> w
                    else w
        | _ -> w

    /// Mark "<what>Recently" for 4 s (gained / consumed charges...).
    /// A monster's hit at a minion: evasion, armour against physical damage, resistances.
    let private attackMinion env (rng: Rng) name (minion: Actor) (w: World) =
        let e = w.Enemies.[name]
        let atk = e.Def.Attack.Value
        let body = minion.Body.Value
        let st = status e
        let stopped = [ "Frozen"; "HeavilyStunned"; "Electrocuted" ] |> List.exists st.ContainsKey
        let chill = e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
        let debuffs = (defender e).Mods
        let actionSpeed = max 0.1 (Mods.more Ctx.empty [ Named "ActionSpeed" ] debuffs)
        let dealt = max 0.0 (Mods.more Ctx.empty [ Named "DamageDealt" ] debuffs)
        let atk = { atk with Damage = Vitals.withExtraChaos (Mods.sumBase Ctx.empty [ Named "GainAsChaos" ] debuffs) atk.Damage }
        let next = atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed
        let w = schedule (w.Time + next) (EnemyAttack name) w
        if stopped then { w with Counts = bump "enemy:stopped" w.Counts }
        else
            let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
            let hitChance = Vitals.monsterHitChance body.Evasion accuracy
            let landed, w = lands rng minion.Name hitChance w
            if not landed then { w with Counts = bump "minion:evaded" w.Counts }
            else
                let crit = rng.Chance atk.CritChance && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
                let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 else 1.0
                let taken =
                    atk.Damage
                    |> Map.map (fun t d ->
                        let d = d * dealt * critMult
                        match t with
                        | Physical -> d * body.Taken * (1.0 - (if body.PhysicalReduction = 0.0 then Vitals.armourReduction body.Armour d else min 90.0 (Vitals.armourReduction body.Armour d + body.PhysicalReduction)) / 100.0)
                        | t -> d * body.Taken * (1.0 - (Map.tryFind t body.Resist |> Option.defaultValue 0.0) / 100.0))
                hurtMinion env rng minion taken { w with Counts = bump "minion:hit" w.Counts }

    /// A monster's attack goes at the player or at one of the minions in its reach, each as
    /// likely as the other. A melee minion fights at the monster; the others stand where the
    /// player does.
    let private enemyAttack env (rng: Rng) name (w: World) =
        let e = w.Enemies.[name]
        let minions = env.Fight.Minions |> List.filter (fun m -> m.Body.IsSome && not (Map.find m.Name w.Actors).Gone)
        match e.Def.Attack with
        | Some atk when e.Life > 0.0 && env.Fight.Minions |> List.exists (fun m -> m.Body.IsSome) ->
            let playerUp = env.Fight.Player.Vitals.IsSome && (w.Player |> Option.exists (fun p -> p.Died.IsNone))
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            let reachable = minions |> List.filter (fun m -> m.Body.Value.Melee || inRange)
            let n = reachable.Length + (if playerUp && inRange then 1 else 0)
            let pick = if n > 0 then int (rng.Float() * float n) else 0
            if pick < reachable.Length then attackMinion env rng name reachable.[pick] w
            elif playerUp then attackPlayer env rng name w
            else schedule (w.Time + atk.Interval) (EnemyAttack name) w
        | _ -> attackPlayer env rng name w

    /// Conduit: "If you would gain a Charge, Allies in your Presence gain that Charge instead".
    let private conduit (actor: Actor) (r: string) =
        r.EndsWith "Charge" && Mods.flag Ctx.empty (Named("Conduit:" + r)) (ModDb.ofList actor.Mods)

    /// "Recover X% of maximum Life for each Endurance Charge consumed": the player's, for so many.
    let private enduranceLife (actor: Actor) (n: int) (w: World) =
        match actor.Vitals, w.Player with
        | Some v, Some p when n > 0 && v.LifePerEnduranceConsumed > 0.0 && v.Life > 1.0 ->
            { w with Player = Some { p with Life = min (max p.Life v.Life) (p.Life + v.LifePerEnduranceConsumed * float n) } }
        | _ -> w

    let private onPeriodic env (rng: Rng) actorName periodicName (w: World) =
        let actor = actorDef env actorName
        match actor.Periodics |> List.tryFind (fun p -> p.Name = periodicName) with
        | None -> w
        | Some p ->
            let w = schedule (w.Time + p.Every) (PeriodicTick(actorName, periodicName)) w
            let st = Map.find actorName w.Actors
            let gain r n w =
                if conduit actor r then { w with Counts = bump ("conduit:" + r) w.Counts }
                else
                    setStacksWith env actorName true r n w
                    |> markStatus actorName ("Gained" + r + "Recently")
                    |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
            match p.Action with
            | GainRandom resources ->
                let open' = resources |> List.filter (fun r -> stacksOf st r < (Map.tryFind r actor.MaxResources |> Option.defaultValue 0))
                if open'.IsEmpty then w
                else
                    let r = open'.[min (open'.Length - 1) (int (rng.Float() * float open'.Length))]
                    gain r (stacksOf st r + 1) w
            | GainWeighted weights ->
                let total = weights |> List.sumBy snd
                let roll = rng.Float() * total
                let r =
                    weights
                    |> List.fold (fun (picked, acc) (k, wgt) -> match picked with Some _ -> picked, acc | None -> (if roll < acc + wgt then Some k else None), acc + wgt) (None, 0.0)
                    |> fst
                    |> Option.defaultValue (fst (List.last weights))
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
                gain r (min maxR (stacksOf st r + 1)) w
            | GainAnyRandom resources ->
                let r = resources.[min (resources.Length - 1) (int (rng.Float() * float resources.Length))]
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
                gain r (min maxR (stacksOf st r + 1)) w
            | SummonMinions (pool, count, duration) -> summon env pool count duration w
            | SummonFromCorpse (pool, count, duration) ->
                if w.Kills.Length > w.CorpsesUsed then summon env pool count duration { w with CorpsesUsed = w.CorpsesUsed + 1 } else w
            | TriggerNamed skill -> trigger env rng (actorDef env actorName) skill w
            | HealLife amount ->
                match env.Fight.Player.Vitals, w.Player with
                | Some v, Some p when p.Died.IsNone && v.Life > 1.0 ->
                    { w with Player = Some { p with Life = min (max p.Life v.Life) (p.Life + amount * v.LifeRecoveryRate) }; Counts = bump "healed" w.Counts }
                | _ -> w
            | GainTimedBuff (buff, chance, duration, maxStacks, mods) ->
                if rng.Float() >= chance || stacksOf st buff >= maxStacks then w
                else
                    updateActor actorName (fun st ->
                        let n = stacksOf st buff + 1
                        { st with Buffs = Map.add buff { Name = buff; Mods = mods; Stacks = n; Expires = infinity } st.Buffs }) w
                    |> schedule (w.Time + duration) (SpendBuff(actorName, buff))
                    |> fun w -> { w with Counts = bump ("gained:" + buff) w.Counts }
            | ConsumeEach resources ->
                let skip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] (ModDb.ofList actor.Mods)
                resources
                |> List.fold
                    (fun w r ->
                        let st = Map.find actorName w.Actors
                        if stacksOf st r = 0 then w
                        else
                            let w = markStatus actorName ("Consumed" + r + "Recently") w
                            // (mana for consuming power charges: once, kept or not)
                            let w =
                                match actor.Vitals with
                                | Some v when r = "PowerCharge" ->
                                    gainMana v "consume" (v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] (ModDb.ofList actor.Mods) / 100.0) w
                                | _ -> w
                            if rng.Chance skip then { w with Counts = bump ("kept:" + r) w.Counts }
                            else
                                setStacksWith env actorName false r (stacksOf st r - 1) w |> fun w -> { w with Counts = bump ("lost:" + r) w.Counts }
                                |> fun w -> if r = "EnduranceCharge" then enduranceLife actor 1 w else w)
                    w

    /// Whether an enemy that died of damage over time at `at` is the player's kill: some damage
    /// over time of the player's was on it then (one only of minions is theirs, as their hits are).
    let private playersDotKill env (e: EnemyState) at =
        e.Dots |> List.exists (fun d -> d.Owner = env.Fight.Player.Name && d.Expires >= at)

    /// What a kill by damage over time gives besides the rules for kills: what a killing hit gives
    /// where the game does not tie it to a hit. The player's kill: flask charges, life, energy
    /// shield and mana on kill (the character's own, no skill's), Runic Ward and Mana Remnants, Rage
    /// on kill, killed recently; any kill: charm charges and what kills of rare monsters set off.
    /// Meta energy from kills is from melee attack kills only (Reaper's Invocation), never these.
    let private dotKillGains env (rng: Rng) (e: EnemyState) mine (w: World) =
        let player = env.Fight.Player
        let w =
            match player.Vitals, w.Player with
            | Some v, Some p when mine ->
                let per = RarityCharges.flaskChargesPerKill e.Def.Rarity
                let w =
                    { w with
                        Player =
                            Some
                                { p with
                                    FlaskCharges = (match player.ManaFlask with Some f -> min f.MaxCharges (p.FlaskCharges + per * f.ChargeGainMult) | None -> p.FlaskCharges)
                                    LifeFlaskCharges = (match v.LifeFlask with Some f -> min f.MaxCharges (p.LifeFlaskCharges + per * f.ChargeGainMult) | None -> p.LifeFlaskCharges)
                                    Life = min (max p.Life v.Life) (p.Life + v.LifeOnKill)
                                    EnergyShield = min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap)) (p.EnergyShield + v.ESOnKill) } }
                let sheet = ModDb.ofList player.Mods
                gainMana v "kill" ((Mods.sumBase Ctx.empty [ ManaOnKill ] sheet + v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnKill" ] sheet / 100.0) * v.ManaRecoveryRate) w
            | _ -> w
        let w =
            match player.WardRemnants, w.Player with
            | Some r, Some _ when mine && rng.Chance r.OnKillChance ->
                let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                { w with Counts = bump "ward remnant" w.Counts } |> schedule (w.Time + r.Delay) (WardArrives(r.WardPerRemnant * count))
            | _ -> w
        let w =
            match player.ManaRemnants, w.Player with
            | Some r, Some _ when mine ->
                let ailing = [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.exists (status e).ContainsKey
                if ailing && rng.Chance r.OnKillChance then
                    let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                    { w with Counts = bump "mana remnant" w.Counts } |> schedule (w.Time + r.Delay) (ManaArrives((r.ManaPerRemnant + r.PercentOfMaxMana / 100.0 * (player.Vitals |> Option.map (fun v -> v.Mana) |> Option.defaultValue 0.0)) * count))
                else w
            | _ -> w
        let w =
            match player.LifeRemnants, player.Vitals, w.Player with
            | Some r, Some v, Some p when mine ->
                if rng.Chance r.OnKillChance then
                    let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                    let rate = (r.LifePerRemnant + r.PercentOfMaxLife / 100.0 * v.Life) * count * v.LifeRecoveryRate / r.Delay
                    { w with Player = Some { p with Recoups = (w.Time + r.Delay, rate, "Life") :: p.Recoups }; Counts = bump "life remnant" w.Counts }
                else w
            | _ -> w
        let w =
            if player.Charms.IsEmpty then w
            else
                let w =
                    { w with
                        Player =
                            w.Player |> Option.map (fun p ->
                                { p with
                                    CharmCharges =
                                        player.Charms |> List.fold (fun (m: Map<string, float>) c ->
                                            let gain = (MonsterPower.ofRarity e.Def.Rarity / 2.0 + (if rng.Chance c.ChargeOnKillChance then 1.0 else 0.0)) * c.ChargeGainMult
                                            Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + gain)) m) p.CharmCharges }) }
                if e.Def.Rarity = Rare || e.Def.Rarity = Unique then fst (charm env rng "KillRare" w) else w
        if not mine then w
        else
            let _, _, _, _, onKill = env.Rage
            gainRage env onKill w |> markRecently player.Name false false true

    /// An enemy killed by damage over time or recorded damage coming due: the player's kill
    /// (`mine`) sets off the player's rules for kills; what a kill gives follows.
    let private killedOverTime env rng name mine (w: World) =
        let player = env.Fight.Player
        let w =
            if not mine then w
            else
                let w = react env rng player OnKill dotKiller None w
                status w.Enemies.[name] |> Map.fold (fun w k _ -> react env rng player (OnKillOf k) dotKiller (Some name) w) w
        dotKillGains env rng w.Enemies.[name] mine w

    let private step env rng (w: World) =
        let (KeyValue ((t, sq), ev)) = Seq.head w.Queue
        let dt = t - w.Time
        (match env.Trace with
         | Some tell ->
             match ev with
             | Ready a -> tell t "Ready" a "" (float sq)
             | Strike (a, s, _, _) -> tell t "Strike" a s (float sq)
             | PassingStrike (a, s, _, _, _) -> tell t "PassingStrike" a s (float sq)
             | DirectHit (a, s, _, _) -> tell t "DirectHit" a s (float sq)
             | ConsumeCharges (a, c, _) -> tell t "ConsumeCharges" a c (float sq)
             | CycleTick (a, c) -> tell t "CycleTick" a c (float sq)
             | EnemyAttack e -> tell t "EnemyAttack" e "" (float sq)
             | ReviveMinions _ -> tell t "ReviveMinions" "" "" (float sq)
             | MinionLeaves (a, _) -> tell t "MinionLeaves" a "" (float sq)
             | Landed (a, s, _) -> tell t "Landed" a s (float sq)
             | PeriodicTick (a, p) -> tell t "PeriodicTick" a p (float sq)
             | WardArrives _ -> tell t "WardArrives" "" "" (float sq)
             | ManaArrives _ -> tell t "ManaArrives" "" "" (float sq)
             | GainStacks (a, r, _) -> tell t "GainStacks" a r (float sq)
             | Expose (a, e, _, _, _) -> tell t "Expose" a e (float sq)
             | PlantCrystals (a, p, _, _, _, _, _) -> tell t "PlantCrystals" a p (float sq)
             | SpendBuff (a, b) -> tell t "SpendBuff" a b (float sq)
             | EnergyArrives (a, m, _) -> tell t "EnergyArrives" a m (float sq)
             | Expire -> tell t "Expire" "" "" (float sq)
         | None -> ())
        // What a skill's hits roll comes from a stream of that skill alone (who strikes with
        // what: its first event, its second, ...), not from the one stream of the fight: its
        // tenth strike rolls the same whatever else happened before. Two fights of one seed
        // that differ a little (a passive more) then differ by what that does, not by luck.
        let rng, w =
            let key =
                match ev with
                | Strike (a, s, _, _) | PassingStrike (a, s, _, _, _) | DirectHit (a, s, _, _) -> "S|" + a + "|" + s
                | ConsumeCharges (a, _, s) -> "C|" + a + "|" + s
                | EnemyAttack e -> "E|" + e
                | PeriodicTick (a, p) -> "P|" + a + "|" + p
                | _ -> ""
            if key = "" then rng, w
            else
                let n = Map.tryFind key w.Streams |> Option.defaultValue 0
                Rng(streamSeed w.Seed key n), { w with Streams = Map.add key (n + 1) w.Streams }
        let w = { w with Queue = Map.remove (t, sq) w.Queue }
        let killsBefore = w.Kills.Length
        let w = advance env t w
        // the player's kills by damage over time set off the player's rules for kills, as by a
        // skill that is neither attack nor spell, and give what a kill gives (`dotKillGains`)
        let w =
            let n = w.Kills.Length - killsBefore
            if n <= 0 then w
            else
                w.Kills |> List.truncate n |> List.rev
                |> List.fold (fun w (name, _, at) -> killedOverTime env rng name (playersDotKill env w.Enemies.[name] at) w) w
        let w = w |> syncPresence env.Fight
        let rates = match env.Fight.Player.Vitals with Some v when not v.RatesVary -> Some v | _ -> vitalsNow env w
        let w = w |> recover env.Fight.Player.Vitals rates env.Fight.Player.ManaFlask env.Fight.Player.Charms dt
        if allDead w || (w.Player |> Option.exists (fun p -> p.Died.IsSome)) then
            w
        else
            let w = prune w
            // seeds: set off by the damage their enemy has taken (or its death: the blast then
            // strikes the enemy aimed at), gone when their time is up
            let w =
                if w.Bombs.IsEmpty then w
                else
                    w.Bombs |> List.fold (fun w ((owner, skill, enemy, mark, needed, until) as bomb) ->
                        let drop w = { w with Bombs = w.Bombs |> List.filter (fun b -> b <> bomb) }
                        match Map.tryFind enemy w.Enemies with
                        | Some e when e.Life > 0.0 && mark - e.Life >= needed ->
                            drop w |> schedule w.Time (Strike(owner, skill, 1.0, enemy))
                        | Some e when e.Life > 0.0 ->
                            if w.Time >= until then { drop w with Counts = bump ("fizzled:" + skill) w.Counts } else w
                        | _ ->
                            match aimed env w with
                            | Some other -> drop w |> schedule w.Time (Strike(owner, skill, 1.0, other.Def.Name))
                            | None -> drop w) w
            // pustules: burst when their time is up or they hold all the Poison they can use
            let w =
                if w.Pustules.IsEmpty then w
                else
                    w.Pustules |> List.fold (fun w ((owner, part, enemy, mark, per, cap, until) as pustule) ->
                        let drop w = { w with Pustules = w.Pustules |> List.filter (fun x -> x <> pustule) }
                        match Map.tryFind enemy w.Enemies with
                        | Some e when e.Life > 0.0 ->
                            let stored = (Map.tryFind "Poison" w.DamageBy |> Option.defaultValue 0.0) - mark
                            let more = if per > 0.0 then min cap (stored / per) else 0.0
                            if w.Time >= until || (cap > 0.0 && more >= cap) then drop w |> schedule w.Time (Strike(owner, part, 1.0 + more / 100.0, enemy)) else w
                        | _ -> drop w) w
            // Ice Crystals: destroyed once the enemy beside them has lost as much life as one
            // withstands (the damage dealt there since they appeared); each then explodes
            let w =
                if w.Crystals.IsEmpty then w
                else
                    w.Crystals |> List.fold (fun w ((owner, part, enemy, mark, life, count, until) as c) ->
                        let drop w = { w with Crystals = w.Crystals |> List.filter (fun x -> x <> c) }
                        match Map.tryFind enemy w.Enemies with
                        | Some e when e.Life > 0.0 && mark - e.Life >= life ->
                            [ 1 .. count ] |> List.fold (fun w _ -> schedule w.Time (Strike(owner, part, 1.0, enemy)) w) (drop w)
                            |> fun w -> { w with Counts = bump ("shattered:" + part) w.Counts }
                        | Some e when e.Life > 0.0 && w.Time < until -> w
                        | _ -> drop w) w
            // recorded damage is taken again as life when the debuff ends, or as soon as that kills
            let w =
                if w.Agonies.IsEmpty then w
                else
                    // (whose debuff killed which enemy, in order)
                    let killed = ResizeArray<string * string>()
                    w.Agonies |> List.fold (fun w ((owner, skill, enemy, mark, share, until) as a) ->
                        let drop w = { w with Agonies = w.Agonies |> List.filter (fun x -> x <> a) }
                        match Map.tryFind enemy w.Enemies with
                        | Some e when e.Life > 0.0 ->
                            let owed = share * mark
                            if w.Time >= until || owed >= e.Life then
                                let dealt = min owed e.Life
                                let dead = owed >= e.Life
                                if dead then killed.Add((enemy, owner))
                                { drop w with
                                    Enemies = Map.add enemy { e with Life = (if dead then 0.0 else e.Life - dealt) } w.Enemies
                                    DamageBy = addf skill dealt w.DamageBy
                                    DamageByType = addf "Other" dealt w.DamageByType
                                    Kills = if dead then (enemy, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
                            else w
                        | _ -> drop w) w
                    // what they killed, in order: the player's kill when the debuff was the player's
                    |> fun w ->
                        killed |> Seq.fold (fun w (name, owner) -> killedOverTime env rng name (owner = env.Fight.Player.Name) w) w
            // Demon Form: a Demonflame for every second in the form (at most 10), life lost for
            // each; at 1 life the form ends and its cooldown starts
            let w =
                let name = env.Fight.Player.Name
                let st = Map.find name w.Actors
                match Map.tryFind "Demon Form" st.Buffs, w.Player with
                | Some form, Some p ->
                    let value stat = form.Mods |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
                    let flames = min 10 (int (w.Time - (form.Expires - 1e9)))
                    // the life lost since the last event, at the Demonflame held then
                    let before = max 0 (min 10 (int (w.Time - dt - (form.Expires - 1e9))))
                    let life = p.Life - value "DemonLifeLossPerStack" * float before * max 0.0 dt
                    if life <= 1.0 then
                        let skill = form.Mods |> List.tryPick (fun m -> if m.Source <> "" then Some m.Source else None) |> Option.defaultValue ""
                        let back = w.Time + value "DemonCooldown"
                        { w with Player = Some { p with Life = min p.Life 1.0 }; Counts = bump "demon form ended" w.Counts }
                        |> updateActor name (fun st ->
                            { st with
                                Buffs = st.Buffs |> Map.remove "Demon Form" |> Map.remove "Demonflame"
                                Cooldowns = Map.add skill back st.Cooldowns
                                Recharges = Map.add skill [ back ] st.Recharges })
                    else
                        { w with Player = Some { p with Life = life } }
                        |> updateActor name (fun st ->
                            if flames > 0 then { st with Buffs = Map.add "Demonflame" { Name = "Demonflame"; Mods = []; Stacks = flames; Expires = infinity } st.Buffs } else st)
                | _ -> w
            // rage: regenerated all the time, lost once none has been gained for a while
            let w =
                let regen, loss, delay, _, _ = env.Rage
                let name = env.Fight.Player.Name
                let st = Map.find name w.Actors
                let have = stacksOf st "Rage"
                let maxRage = Map.tryFind "Rage" env.Fight.Player.MaxResources |> Option.defaultValue 0
                if dt <= 0.0 || maxRage <= 0 || (regen <= 0.0 && have = 0) then w
                else
                    // Rage is lost from when the delay is over: of dt, the part after it
                    let losingFor = max 0.0 (w.Time - max (w.Time - dt) (st.RageAt + delay))
                    // Berserk: life lost for every Rage held while Rage is not being lost (not below 1)
                    let berserkFor = if loss <= regen then dt else dt - losingFor
                    let w =
                        match env.Fight.Player.Vitals, w.Player with
                        | Some v, Some p when berserkFor > 0.0 && have > 0 && env.BerserkLoss > 0.0 ->
                            let life = max (min p.Life 1.0) (p.Life - env.BerserkLoss / 100.0 / 60.0 * float have * v.Life * berserkFor)
                            { w with Player = Some { p with Life = life; MinLife = min p.MinLife (life / max 1.0 v.Life) } }
                        | _ -> w
                    let carry = st.RageCarry + regen * dt - loss * losingFor
                    let whole = int (System.Math.Truncate carry)
                    let n = max 0 (min maxRage (have + whole))
                    let w = if n <> have then setStacksWith env name false "Rage" n w else w
                    updateActor name (fun st -> { st with RageCarry = (if n = 0 && carry < 0.0 then 0.0 elif n = maxRage && carry > 0.0 then 0.0 else carry - float whole) }) w
            // the player's life and mana state, as buffs conditions can read
            // "Your Life cannot change while you have Energy Shield": no recovery above where it started
            let w =
                match env.Fight.Player.Vitals, w.Player with
                | Some v, Some p when v.LifeFrozenWithES && p.EnergyShield > 0.0 && p.Life > v.StartLife * v.Life ->
                    { w with Player = Some { p with Life = v.StartLife * v.Life } }
                // reserved life: what is left is the most there can be
                | Some v, Some p when v.LifeReserved > 0.0 && p.Life > (1.0 - v.LifeReserved) * v.Life ->
                    { w with Player = Some { p with Life = (1.0 - v.LifeReserved) * v.Life } }
                | _ -> w
            let w =
                match env.Fight.Player.Vitals, w.Player with
                | Some v, Some p ->
                    let states =
                        [ "FullLife", p.Life >= v.Life
                          "LowLife", v.Life > 1.0 && p.Life <= 0.35 * v.Life
                          "FullMana", p.Mana >= v.Mana
                          "LowMana", p.Mana <= 0.35 * v.Mana
                          "HasEnergyShield", p.EnergyShield > 0.0
                          "FullEnergyShield", v.EnergyShield > 0.0 && p.EnergyShield >= v.EnergyShield
                          // a flask drunk and still working (a constant one is a resource the fight starts with)
                          "ManaFlaskEffect", (w.Time < p.FlaskUntil || stacksOf (Map.find env.Fight.Player.Name w.Actors) "ConstantManaFlask" > 0)
                          "LifeFlaskEffect", (w.Time < p.LifeFlaskUntil || stacksOf (Map.find env.Fight.Player.Name w.Actors) "ConstantLifeFlask" > 0)
                          "FlaskEffect",
                          (w.Time < p.FlaskUntil || w.Time < p.LifeFlaskUntil
                           || (let st = Map.find env.Fight.Player.Name w.Actors in stacksOf st "ConstantManaFlask" > 0 || stacksOf st "ConstantLifeFlask" > 0))
                          "Leeching", p.Recoups |> List.exists (fun (until, _, pool) -> pool.StartsWith "Leech" && until > w.Time)
                          // surrounded: at least 5 monsters within 3 m (PoB SurroundedRadiusBase, game constant)
                          "Surrounded", (alive w |> List.filter (fun e -> Vec.length e.Def.Position - e.Def.Size <= v.SurroundedRadius) |> List.length) >= max 1 v.SurroundedAt
                          // the player's own ailments: Ignite is the only damaging one a monster's hit or the
                          // character itself puts on it (Shocked, Chilled, Frozen, Stunned are buffs of their own)
                          "Ignited", p.Dots |> List.exists (fun (until, _, t) -> t = Fire && until > w.Time)
                          "DamagingAilment", p.Dots |> List.exists (fun (until, _, t) -> t = Fire && until > w.Time)
                          // on Low Energy Shield: at most 35% of its maximum (DefaultLowStatusThresholdPercent); none without any
                          "LowEnergyShield", v.EnergyShield > 0.0 && p.EnergyShield <= Constants.lowStatusShare * v.EnergyShield
                          // energy shield recharge started in the past 2 seconds: its delay since the last
                          // hit (or damage over time) is over, by no more than 2 s
                          "ESRechargeStartedRecently", env.AsksESRecharge && v.EnergyShield > 0.0 && w.Time - p.LastHit >= v.ESRechargeDelay && w.Time - p.LastHit < v.ESRechargeDelay + Constants.esRechargeStartedRecently ]
                    // charge states: "while at maximum Power Charges", "while you have no Frenzy Charges"
                    let playerSt = Map.find env.Fight.Player.Name w.Actors
                    let states =
                        states
                        @ [ for kind in [ "Power"; "Frenzy"; "Endurance" ] do
                                let n = Map.tryFind (kind + "Charge") playerSt.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                                let maxN = Map.tryFind (kind + "Charge") env.Fight.Player.MaxResources |> Option.defaultValue 0
                                yield "AtMaximum" + kind + "Charges", maxN > 0 && n >= maxN
                                yield "No" + kind + "Charges", n = 0
                                yield "YouHaveNo" + kind + "Charges", n = 0 ]
                    updateActor env.Fight.Player.Name (fun st ->
                        { st with
                            Buffs =
                                states |> List.fold (fun (b: Map<string, Effect>) (k, on) ->
                                    if on then (if b.ContainsKey k then b else Map.add k { Name = k; Mods = []; Stacks = 1; Expires = infinity } b)
                                    else Map.remove k b) st.Buffs }) w
                | _ -> w
            let gone name = Map.tryFind name w.Actors |> Option.exists (fun st -> st.Gone)
            match ev with
            // a minion that is out of the fight does nothing, and what it had begun is lost
            | Ready name | Strike (name, _, _, _) | PassingStrike (name, _, _, _, _) | DirectHit (name, _, _, _) when gone name -> w
            | Ready name -> onReady env rng name w
            | Strike (name, skill, mult, aimedAt) -> onStrike env rng name skill mult aimedAt None false w
            | PassingStrike (name, skill, mult, aimedAt, elapsed) -> onStrike env rng name skill mult aimedAt (Some elapsed) false w
            | DirectHit (name, skill, mult, enemy) -> onStrike env rng name skill mult enemy None true w
            | EnemyAttack name -> enemyAttack env rng name w
            | ReviveMinions at -> reviveMinions env at w
            | MinionLeaves (name, at) ->
                let st = Map.find name w.Actors
                if st.Gone || st.Leaves <> at then w else { leave name w with Counts = bump "minion:left" w.Counts }
            | Landed (name, skill, aimedAt) ->
                let actor = actorDef env name
                react env rng actor OnLand (skillOf actor skill) (Some aimedAt) w
            | PeriodicTick (name, periodic) -> onPeriodic env rng name periodic w
            | ManaArrives amount ->
                match env.Fight.Player.Vitals, env.Fight.Player.ManaRemnants, w.Player with
                | Some v, Some r, Some p -> { w with Player = Some { p with Mana = min (max p.Mana (v.Mana * r.Overflow)) (p.Mana + amount * v.ManaRecoveryRate) } }
                | _ -> w
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
                // a channel that takes a charge for so long of it: each use takes one with the
                // chance its own time is of that (the same charges per second, at random moments)
                let every = Mods.sumBase Ctx.empty [ Named "ConsumeChargeEveryMs" ] (skillDb env actor (skillOf actor skillName) st) / 1000.0
                let spared = every > 0.0 && not (rng.Chance(100.0 * min 1.0 (useTime env actor (skillOf actor skillName) w / every)))
                if not (Map.containsKey charge st.Buffs) || spared then w
                else
                    // "still count as consuming": the consumed-recently status starts either way
                    let recent = "Consumed" + charge + "Recently"
                    let w =
                        updateActor name (fun st -> { st with Buffs = Map.add recent { Name = recent; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
                        |> schedule (w.Time + 4.0) Expire
                    // "Recover N% of maximum Mana when you consume a Power Charge": once for a use
                    // that consumes them, however many, and whether they are kept or not (user,
                    // in game: "happens but only once per Falling Thunder or Charged Staff")
                    let w =
                        match actor.Vitals with
                        | Some v when charge = "PowerCharge" ->
                            gainMana v "consume" (v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] (ModDb.ofList actor.Mods) / 100.0) w
                        | _ -> w
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
                        // one charge, a set number, or all of them (0)
                        let count = int (Mods.sumBase Ctx.empty [ Named "ConsumeChargeCount" ] (skillDb env actor (skillOf actor skillName) st))
                        let had = match Map.tryFind charge st.Buffs with Some e -> e.Stacks | None -> 0
                        let w = if charge = "EnduranceCharge" then enduranceLife actor (if count > 0 && had > count then count else had) w else w
                        updateActor name (fun st ->
                            match Map.tryFind charge st.Buffs with
                            | Some e when count > 0 && e.Stacks > count -> { st with Buffs = Map.add charge { e with Stacks = e.Stacks - count } st.Buffs }
                            | _ -> { st with Buffs = Map.remove charge st.Buffs }) w
                        |> fun w -> { w with Counts = bump ("consumed:" + charge) w.Counts }
            | PlantCrystals (owner, part, enemy, count, life, limit, lasts) ->
                match Map.tryFind enemy w.Enemies with
                | Some e when e.Life > 0.0 ->
                    // more than the limit: the oldest go (without exploding)
                    let all = (owner, part, enemy, e.Life, life, count, w.Time + lasts) :: w.Crystals
                    let kept, _ =
                        all |> List.fold (fun (kept, n) ((o, p, _, _, _, c, _) as x) ->
                            if o = owner && p = part then (if n + c <= max count limit then x :: kept, n + c else kept, n + c) else x :: kept, n) ([], 0)
                    { w with Crystals = List.rev kept; Counts = bump ("crystals:" + part) w.Counts }
                | _ -> w
            | Expose (name, target, dt, amount, duration) -> expose (actorDef env name) target dt amount duration w
            // Conduit: "If you would gain a Charge, Allies in your Presence gain that Charge
            // instead": the actor gains nothing (what an ally holds, no minion reads)
            | GainStacks (name, r, _) when conduit (actorDef env name) r ->
                { w with Counts = bump ("conduit:" + r) w.Counts }
            | GainStacks (name, r, n) ->
                let actor = actorDef env name
                let st = Map.find name w.Actors
                let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue n
                // "N% chance to gain an additional (Power) Charge when you gain a Charge"
                let n =
                    if not (r.EndsWith "Charge") then n
                    else
                        match actor.Skills |> List.tryHead with
                        | Some sk ->
                            let db = skillDb env actor sk st
                            let chance = Mods.sumBase Ctx.empty [ Named("ExtraChargeChance:" + r); Named "ExtraChargeChance:Any" ] db
                            if chance > 0.0 && rng.Chance chance then n + 1 else n
                        | None -> n
                setStacksWith env name true r (min maxR (stacksOf st r + n)) w
                |> (if r.EndsWith "Charge" then markStatus name ("Gained" + r + "Recently") else id)
                |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
            | EnergyArrives (name, meta, amount) -> gainEnergy env rng (actorDef env name) meta amount w
            | SpendBuff (name, buff) ->
                updateActor name (fun st ->
                    match Map.tryFind buff st.Buffs with
                    | Some e when e.Stacks > 1 -> { st with Buffs = Map.add buff { e with Stacks = e.Stacks - 1 } st.Buffs }
                    | _ -> { st with Buffs = Map.remove buff st.Buffs }) w
            | Expire -> w

    /// The defences that the runs of the fight that was run last have worked out, by the
    /// buffs written out (a function of the buffs: knowing them changes no result). Runs
    /// of one fight follow each other, or go side by side: they share what is known.
    let private vitalsOfLast : (Fight * System.Collections.Generic.Dictionary<string, PlayerStats>) option[] = Array.create 1 None
    let private vitalsKnown (fight: Fight) : System.Collections.Generic.Dictionary<string, PlayerStats> =
        lock vitalsOfLast (fun () ->
            match vitalsOfLast.[0] with
            | Some (last, known) when obj.ReferenceEquals(last, fight) -> known
            | _ ->
                let known = System.Collections.Generic.Dictionary<string, PlayerStats>()
                vitalsOfLast.[0] <- Some(fight, known)
                known)

    let private envOf (trace: (float -> string -> string -> string -> float -> unit) option) (fight: Fight) =
        { Fight = fight
          Trace = trace
          Rage =
            (let db = ModDb.ofList fight.Player.Mods
             let c = Ctx.empty
             Mods.sumBase c [ Named "RageRegen" ] db,
             (if Mods.flag c (Named "NoRageLoss") db then 0.0 else Constants.rageLossPerSecond * Mods.inc c [ Named "RageDecay" ] db),
             Constants.rageLossDelay + Mods.sumBase c [ Named "RageLossDelay" ] db,
             Mods.sumBase c [ Named "RageWhenHit" ] db,
             Mods.sumBase c [ Named "RageOnKill" ] db)
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

    /// Simulate one fight with the given seed.
    let private runWith (trace: (float -> string -> string -> string -> float -> unit) option) (tracing: bool) (fight: Fight) (seed: uint32) : Outcome =
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
                      CharmCharges = fight.Player.Charms |> List.map (fun c -> c.Buff, c.MaxCharges) |> Map.ofList })
              Time = 0.0
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
              Aim = "" }
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
        let maxEvents = 3000000
        let rec loop n (w: World) =
            let died = w.Player |> Option.exists (fun p -> p.Died.IsSome)
            if died || allDead w || Map.isEmpty w.Queue || fst (Seq.head w.Queue).Key > fight.MaxTime then w
            elif n >= maxEvents then { w with Counts = bump "aborted: too many events" w.Counts }
            else
                let before = w.Kills
                let w = step env rng w
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
          Counts = w.Counts
          Uptime = w.Uptime
          Died = w.Player |> Option.bind (fun p -> p.Died)
          DamageTaken = w.Player |> Option.map (fun p -> p.Taken) |> Option.defaultValue 0.0
          ManaStarved = w.Player |> Option.map (fun p -> p.Starved) |> Option.defaultValue 0.0
          Evened = w.Evened
          Overkill = w.Overkill
          MinLife = w.Player |> Option.map (fun p -> p.MinLife) |> Option.defaultValue 1.0
          ManaGained = w.Player |> Option.map (fun p -> p.ManaGained) |> Option.defaultValue Map.empty }

    /// Simulate one fight with the given seed.
    let run (fight: Fight) (seed: uint32) : Outcome = runWith None false fight seed

    /// The same fight (the same numbers), with what happened kept for showing it.
    let shown (fight: Fight) (seed: uint32) : Outcome = runWith None true fight seed

    /// The same fight (the same numbers), telling `tell` every event as it is taken from the
    /// queue (time, kind, who, what, its number in the queue's order) and every hit that lands
    /// (time, "hit" or "crit", the enemy, the skill, the damage dealt): for comparing another
    /// engine with this one, event by event.
    let traced (tell: float -> string -> string -> string -> float -> unit) (fight: Fight) (seed: uint32) : Outcome =
        runWith (Some tell) false fight seed
