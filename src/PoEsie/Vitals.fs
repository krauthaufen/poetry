namespace PoEsie

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
      BlockedDamageTaken: float }

/// A mana flask as equipped (values as the game shows them).
type ManaFlask =
    { Amount: float
      Duration: float
      ChargesPerUse: float
      MaxCharges: float
      /// Charges gained per second (belt generation etc.), and the multiplier on charges gained.
      ChargesPerSecond: float
      ChargeGainMult: float }

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
