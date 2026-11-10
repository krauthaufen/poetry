namespace PoEsie

/// Named game constants. Each one says where it comes from; see RULES.md for
/// how confident we are in the mechanic that uses it.
module Constants =
    let private gc key = Map.find key Tables.gameConstants
    let private mc key = Map.find key Tables.monsterConstants

    let private byLevel (table: float[]) level =
        table.[(max 1 (min level table.Length)) - 1]

    // ---- crit ----
    /// Character.ot base_critical_hit_damage_bonus
    let playerBaseCritBonus = 100.0
    let monsterBaseCritBonus = mc "base_critical_hit_damage_bonus"

    /// Order of "gain as extra" and conversion: gain first, then convert everything.
    let gainBeforeConversion = true

    // ---- mitigation ----
    /// PoB data.misc.ArmourRatio: reduction = A / (A + ratio * damage)
    let armourRatio = 10.0
    let enemyMaxPhysReduction = mc "maximum_physical_damage_reduction_%"
    let enemyBaseMaxResist = mc "base_maximum_all_resistances_%"
    let resistFloor = -200.0
    let resistCeiling = 90.0

    // ---- damaging ailments: % of the hit dealt per second, base duration ----
    let bleedPercentPerSecond = gc "BleedingHitDamagePercentPerMinute" / 60.0
    let ignitePercentPerSecond = gc "IgniteHitDamagePercentPerMinute" / 60.0
    let poisonPercentPerSecond = gc "PoisonHitDamagePercentPerMinute" / 60.0
    let bleedDuration = gc "BaseBleedingDuration"
    let igniteDuration = gc "BaseIgniteDuration"
    let poisonDuration = gc "BasePoisonDuration"
    let bleedMovingMultiplier = gc "BloodstainedMultiplierWhenMovingOrBleedingAggravated"

    // ---- threshold ailments ----
    let igniteChanceMultiplier = gc "IgniteChanceMultiplier"
    let shockChanceMultiplier = gc "ShockChanceMultiplier"
    let baseShockMagnitude = gc "BaseShockMagnitude"
    let shockDuration = gc "BaseShockDuration"
    let chillDuration = gc "BaseChillDuration"
    let chillEffectMultiplier = gc "ChillEffectMultiplier"
    let chillMaxEffect = gc "ChillMaxEffect"

    // ---- buildup (poise) ailments ----
    let buildupScale a =
        match a with
        | Freeze -> gc "FreezeDamageScale"
        | Electrocute -> gc "ElectrocuteDamageScale"
        | HeavyStun -> gc "HeavyStunDamageScale"
        | Pin -> gc "PinDamageScale"
        | _ -> 0.0

    /// Seconds the target stays frozen / electrocuted / stunned / pinned.
    let buildupDuration a =
        match a with
        | Freeze -> gc "FreezeDuration"
        | Electrocute -> gc "ElectrocuteDuration" / 1000.0
        | HeavyStun -> 3.0 // UNVERIFIED placeholder, no game constant found; see RULES.md
        | Pin -> gc "PinDuration"
        | _ -> 0.0

    // ---- monster tables ----
    let monsterLife level = byLevel Tables.monsterLife level
    let monsterArmour level = byLevel Tables.monsterArmour level
    let monsterEvasion level = byLevel Tables.monsterEvasion level
    let ailmentThreshold level = byLevel Tables.monsterAilmentThreshold level
    let poiseThreshold level = byLevel Tables.monsterPoiseThreshold level
    let monsterAccuracy level = byLevel Tables.monsterAccuracy level
    /// DefaultMonsterStats damage column.
    let monsterDamage level = byLevel Tables.monsterDamage level

    // ---- charges and rage ----
    /// GameConstants BaseMaximumRage; Character.ot max_*_charges.
    let baseMaxRage = gc "BaseMaximumRage"
    let baseMaxCharges = 3.0
    /// Character.ot attack_damage_final_permyriad_per_rage = 100: 1% more attack damage per Rage.
    let moreAttackDamagePerRage = 1.0

    // ---- player recovery ----
    /// GameConstants EffectiveMaxDamageForLeech: a hit leeches as if it dealt at most this much.
    let effectiveMaxDamageForLeech = Map.find "EffectiveMaxDamageForLeech" Tables.gameConstants
    /// Leech recovers at most this % of maximum mana per second (PoE convention; guess for PoE2).
    let maxManaLeechRate = 20.0
    /// Seconds without being hit before energy shield recharges (PoB EnergyShieldRechargeDelay).
    let energyShieldRechargeDelay = 4.0

    /// Blinded monsters have this much less accuracy. The buff (BuffDefinitions "blinded":
    /// accuracy_rating_+%_final_from_blinded) carries no base value in the data; GUESS.
    let blindAccuracyLess = 20.0

    /// Maim: maim_movement_speed_+%_final -30 (BuffTemplates LabyrinthTrapMaim; base value not in data).
    let maimSlow = 30.0
    /// Hinder: hinder_movement_speed_+%_final; no value in the data, GUESS.
    let hinderSlow = 30.0

    /// Server tick, PoB data.misc.ServerTickTime.
    let serverTick = 0.033
