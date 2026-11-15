namespace PoEtry

/// A flask as equipped (values as the game shows them).
type ManaFlask =
    { Amount: float
      Duration: float
      ChargesPerUse: float
      MaxCharges: float
      /// Charges gained per second (belt generation etc.), and the multiplier on charges gained.
      ChargesPerSecond: float
      ChargeGainMult: float
      /// Share (0..1) of the recovery applied at once ("X% of Flask Recovery applied Instantly").
      Instant: float }

/// A charm: used by itself when its condition occurs, if it has the charges; its effect
/// lasts for its duration. While it lasts the condition can't occur again (immunity charms),
/// and the occurrence that used it is prevented.
type Charm =
    { Name: string
      /// What uses it: "Frozen", "Stunned", "Ignited", "Shocked", "Slowed", "Bleeding", "Poisoned",
      /// "KillRare", "Hit:Fire", "Hit:Cold", "Hit:Lightning", "Hit:Chaos".
      Trigger: string
      /// The buff on the player while it lasts; its other effects are sheet mods conditional on it.
      Buff: string
      /// The charm stops its trigger (immunity to freeze, "cannot be stunned").
      Prevents: bool
      Duration: float
      ChargesPerUse: float
      MaxCharges: float
      ChargesPerSecond: float
      /// Chance (%) to gain a charge on any kill, and the multiplier on charges gained.
      ChargeOnKillChance: float
      ChargeGainMult: float
      LifeOnUse: float
      ManaOnUse: float
      /// "Energy Shield Recharge starts on use".
      StartsRecharge: bool }

/// The player's pools, defences and recovery, computed from the character sheet.
type PlayerStats =
    { Str: float
      Dex: float
      Int: float
      Life: float
      Mana: float
      EnergyShield: float
      Armour: float
      Evasion: float
      Deflection: float
      /// Capped resistances.
      Resist: Map<DamageType, float>
      LifeRegen: float
      ManaRegen: float
      EnergyShieldRecharge: float
      /// Chance (%) to block a hit, whether it rolls twice (lucky), % of a blocked hit still taken.
      Block: float
      LuckyBlock: bool
      BlockedDamageTaken: float
      /// Life the fight starts with, as a share of the maximum (a build that sits on low life).
      StartLife: float
      /// Share of maximum life that is reserved (0..1): life never rises above the rest.
      LifeReserved: float
      /// "Your Life cannot change while you have Energy Shield".
      LifeFrozenWithES: bool
      /// Enemies needed nearby to count as Surrounded (5, less with "require N fewer enemies"),
      /// within this distance (3 m, more with "increased Surrounded Area of Effect").
      SurroundedAt: int
      SurroundedRadius: float
      /// "increased Stun Recovery": a stun is over this many times as fast (1: as it is).
      StunRecovery: float
      /// Energy shield is not recovered above this (Scarred Faith: the armour; infinity: no cap).
      ESRecoverCap: float
      /// Percent of the physical damage a hit's mitigation prevents that is recouped as energy
      /// shield, per unit of the hitting monster's power (Scarred Faith).
      RecoupPreventedPerPower: float
      /// "X% of Physical Damage prevented Recouped as Life": percent of the physical damage a
      /// hit's mitigation prevents.
      RecoupPreventedLife: float
      /// Runic Ward: takes damage once life is down to 1; regenerates all the time.
      Ward: float
      WardRegen: float
      WardOnBlock: float
      /// Pyromantic Pact: skill costs build Infernal Flame instead of spending mana.
      CostsInfernalFlame: bool
      /// "<from>" damage taken as "<to>": (from, to, percent).
      TakenAs: (DamageType * DamageType * float) list
      /// Seconds without being hit before energy shield recharges; ES regenerated per second anyway.
      ESRechargeDelay: float
      ESRegen: float
      /// % of damage (after energy shield) taken from mana before life.
      ManaBeforeLife: float
      /// % of damage that skips energy shield.
      BypassES: float
      /// % of armour that also applies to hits of this type.
      ArmourAppliesTo: Map<DamageType, float>
      /// Multiplier on damage taken, by type ("increased / reduced damage taken").
      TakenMult: Map<DamageType, float>
      /// Flat physical damage less from attack hits.
      FlatPhysicalTaken: float
      /// Enemy critical hits deal this % less extra damage.
      LessCritDamageTaken: float
      /// Multiplier on monsters' chance to crit you; share of armour (1 = all) that defends
      /// against projectile attacks and against critical hits.
      EnemyCritChanceMult: float
      /// "Enemies Blinded by you have X% reduced Critical Hit Chance", "Enemies you Mark have X%
      /// reduced Accuracy Rating": multipliers on a monster's hit while it is so.
      BlindedEnemyCritMult: float
      /// Of the life a hit takes: the share (%) prevented, and the share (%) then lost over
      /// `Constants.deferredLossSeconds` (Amanamu's Defiance: 20 and 30).
      DeferPrevent: float
      DeferLose: float
      MarkedEnemyAccuracyMult: float
      ArmourVsProjectiles: float
      ArmourVsCrits: float
      /// "Defend with X% of Armour" (of the moment's conditions): the share of armour that counts against every hit.
      ArmourDefend: float
      StunThreshold: float
      AilmentThreshold: float
      FreezeThreshold: float
      AvoidStun: float
      /// Chance (%) to avoid an ailment, multiplier on its effect and duration on you, by ailment name.
      Avoid: Map<string, float>
      SelfEffect: Map<string, float>
      SelfDuration: Map<string, float>
      LifeOnKill: float
      /// Life recovered for each Endurance Charge consumed.
      LifePerEnduranceConsumed: float
      ESOnKill: float
      LifeOnBlock: float
      ManaOnBlock: float
      /// Recoup: (pool "Life" | "Mana" | "ES", damage types it counts, percent), recovered over RecoupSeconds.
      Recoup: (string * DamageType list * float) list
      RecoupSeconds: float
      /// Multipliers on all recovery of the pool.
      LifeRecoveryRate: float
      ManaRecoveryRate: float
      ESRecoveryRate: float
      LifeFlask: ManaFlask option
      /// Blood Magic: skill costs are paid with life.
      CostsLife: bool
      /// Eternal Youth: life recharges (per second, after the recharge delay) instead of energy shield.
      LifeRecharge: float
      /// Glancing Blows: chance to evade is unlucky, chance to deflect is lucky.
      UnluckyEvade: bool
      LuckyDeflect: bool
      /// Multiplier on how fast leeched life is recovered.
      LifeLeechRate: float
      /// Deflected hits deal this % less.
      DeflectEffect: float
      /// "Regenerate X% of maximum Life over 1 second when Stunned": the share (%) of maximum
      /// life a light stun regenerates over a second (with the regeneration and recovery rates).
      RegenOnStun: float }

module PlayerStats =
    /// Bare pools with no defences or recovery: a starting point for hand-built players.
    let basic life mana : PlayerStats =
        { Str = 0.0; Dex = 0.0; Int = 0.0; Life = life; Mana = mana; EnergyShield = 0.0; Armour = 0.0; Evasion = 0.0
          Deflection = 0.0; Resist = Map.ofList [ Fire, 0.0; Cold, 0.0; Lightning, 0.0; Chaos, 0.0 ]
          LifeRegen = 0.0; ManaRegen = 0.0; EnergyShieldRecharge = 0.0; Block = 0.0; LuckyBlock = false
          BlockedDamageTaken = 0.0; Ward = 0.0; WardRegen = 0.0; WardOnBlock = 0.0; CostsInfernalFlame = false; TakenAs = []
          ESRechargeDelay = 4.0; ESRegen = 0.0; ManaBeforeLife = 0.0; BypassES = 0.0; ArmourAppliesTo = Map.empty
          TakenMult = Map.empty; FlatPhysicalTaken = 0.0; LessCritDamageTaken = 0.0; EnemyCritChanceMult = 1.0; BlindedEnemyCritMult = 1.0; MarkedEnemyAccuracyMult = 1.0; DeferPrevent = 0.0; DeferLose = 0.0; ArmourVsProjectiles = 1.0; ArmourVsCrits = 1.0; ArmourDefend = 1.0; StunThreshold = life
          AilmentThreshold = life / 2.0; FreezeThreshold = life / 2.0; AvoidStun = 0.0; Avoid = Map.empty
          SelfEffect = Map.empty; SelfDuration = Map.empty; LifeOnKill = 0.0; LifePerEnduranceConsumed = 0.0; ESOnKill = 0.0; LifeOnBlock = 0.0
          ManaOnBlock = 0.0; Recoup = []; RecoupSeconds = 4.0; LifeRecoveryRate = 1.0; ManaRecoveryRate = 1.0
          ESRecoveryRate = 1.0; LifeFlask = None; CostsLife = false; LifeRecharge = 0.0; UnluckyEvade = false
          LuckyDeflect = false; LifeLeechRate = 1.0; DeflectEffect = 40.0; StartLife = 1.0; LifeReserved = 0.0; LifeFrozenWithES = false; SurroundedAt = 5; SurroundedRadius = Constants.surroundedRadius; StunRecovery = 1.0; ESRecoverCap = infinity; RecoupPreventedPerPower = 0.0; RecoupPreventedLife = 0.0; RegenOnStun = 0.0 }

/// Hit resolution against the player (formulas: PoB CalcDefence, constants: game data).
module Vitals =
    /// Chance (%) a monster hit lands against this evasion (PoB calcs.monsterHitChance).
    let monsterHitChance evasion accuracy =
        if accuracy < 0.0 then 5.0
        else max 5.0 (min 100.0 ((1.0 - 0.95 * evasion / (evasion + 4.0 * accuracy)) * 100.0))

    /// Chance (%) a hit is deflected (PoB calcs.deflectChance), capped at 95.
    let deflectChance deflection accuracy =
        if deflection < 1.0 then 0.0
        else max 0.0 (min 95.0 (100.0 - (accuracy / (accuracy + deflection * 0.12) * 150.0 - 50.0)))

    /// Deflected hits deal this much less (GameConstants BasePercentDamageDeflected).
    let deflectEffect = Map.find "BasePercentDamageDeflected" Tables.gameConstants

    /// Player armour: A / (A + 10 D), capped at 90%.
    let armourReduction armour damage =
        if armour <= 0.0 || damage <= 0.0 then 0.0 else min 90.0 (armour / (armour + 10.0 * damage) * 100.0)
