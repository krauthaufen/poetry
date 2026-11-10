namespace PoEsie

/// A flask as equipped (values as the game shows them).
type ManaFlask =
    { Amount: float
      Duration: float
      ChargesPerUse: float
      MaxCharges: float
      /// Charges gained per second (belt generation etc.), and the multiplier on charges gained.
      ChargesPerSecond: float
      ChargeGainMult: float }

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
      StunThreshold: float
      AilmentThreshold: float
      FreezeThreshold: float
      AvoidStun: float
      /// Chance (%) to avoid an ailment, multiplier on its effect and duration on you, by ailment name.
      Avoid: Map<string, float>
      SelfEffect: Map<string, float>
      SelfDuration: Map<string, float>
      LifeOnKill: float
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
      LifeFlask: ManaFlask option }

module PlayerStats =
    /// Bare pools with no defences or recovery: a starting point for hand-built players.
    let basic life mana : PlayerStats =
        { Str = 0.0; Dex = 0.0; Int = 0.0; Life = life; Mana = mana; EnergyShield = 0.0; Armour = 0.0; Evasion = 0.0
          Deflection = 0.0; Resist = Map.ofList [ Fire, 0.0; Cold, 0.0; Lightning, 0.0; Chaos, 0.0 ]
          LifeRegen = 0.0; ManaRegen = 0.0; EnergyShieldRecharge = 0.0; Block = 0.0; LuckyBlock = false
          BlockedDamageTaken = 0.0; Ward = 0.0; WardRegen = 0.0; WardOnBlock = 0.0; CostsInfernalFlame = false; TakenAs = []
          ESRechargeDelay = 4.0; ESRegen = 0.0; ManaBeforeLife = 0.0; BypassES = 0.0; ArmourAppliesTo = Map.empty
          TakenMult = Map.empty; FlatPhysicalTaken = 0.0; LessCritDamageTaken = 0.0; StunThreshold = life
          AilmentThreshold = life / 2.0; FreezeThreshold = life / 2.0; AvoidStun = 0.0; Avoid = Map.empty
          SelfEffect = Map.empty; SelfDuration = Map.empty; LifeOnKill = 0.0; ESOnKill = 0.0; LifeOnBlock = 0.0
          ManaOnBlock = 0.0; Recoup = []; RecoupSeconds = 4.0; LifeRecoveryRate = 1.0; ManaRecoveryRate = 1.0
          ESRecoveryRate = 1.0; LifeFlask = None }

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
