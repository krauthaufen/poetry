namespace PoEtry

/// Named game constants. Each one says where it comes from; see RULES.md for
/// how confident we are in the mechanic that uses it.
module Constants =
    let private gc key = Map.find key Tables.gameConstants
    let private mc key = Map.find key Tables.monsterConstants

    let private byLevel (table: float[]) level =
        table.[(max 1 (min level table.Length)) - 1]

    // ---- crit ----
    /// Dodge roll: distance in game units and seconds it takes. GUESS (about 3.7 m in 0.55 s);
    /// the game's values are not in the exported data.
    let dodgeRollDistance = 37.0
    let dodgeRollSeconds = 0.55
    /// Elemental resistances are lowered by 10% per act of the campaign: -60% in the endgame
    /// (PoB CalcSetup resistancePenalty "Endgame (-60%)"; shown in game on the character sheet).
    let endgameResistPenalty = -60.0
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
        // Monster.ot base_heavy_stun_duration_ms
        | HeavyStun -> Map.find "base_heavy_stun_duration_ms" Tables.monsterConstants / 1000.0
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

    // ---- ailments and stuns on the player (GameConstants "...Player") ----
    let playerChillDuration = gc "BaseChillDurationPlayer"
    let playerShockDuration = gc "BaseShockDurationPlayer"
    let playerFreezeScale = gc "FreezeDamageScalePlayer"
    let playerFreezeDuration = gc "FreezeDurationPlayer"
    /// A hit can light-stun only if its chance is at least this (LightStunMinimumChancePlayer).
    let playerLightStunMinimum = gc "LightStunMinimumChancePlayer"
    /// Monster hits: more stun from melee and from physical damage (monster object stats).
    let monsterMeleeStunMore = mc "melee_hit_damage_stun_multiplier_+%_final_from_ot"
    let monsterPhysicalStunMore = mc "physical_hit_damage_stun_multiplier_+%_final_from_ot"
    /// After a heavy stun the stun threshold is higher for a while (GameConstants
    /// HeavyStunThresholdModifier / HeavyStunModifierDuration, and their "...Player" versions).
    let heavyStunThresholdMore = gc "HeavyStunThresholdModifier"
    let heavyStunThresholdSeconds = gc "HeavyStunModifierDuration"
    let playerHeavyStunScale = gc "HeavyStunDamageScalePlayer"
    let playerHeavyStunThresholdMore = gc "HeavyStunThresholdModifierPlayer"
    let playerHeavyStunThresholdSeconds = gc "HeavyStunModifierDurationPlayer"
    /// Heavy stun on the player: seconds stunned. GUESS: the monsters' own value (Monster.ot);
    /// the player's is not in the exported data.
    let playerHeavyStunDuration = Map.find "base_heavy_stun_duration_ms" Tables.monsterConstants / 1000.0
    /// Stun buildup starts to decay this long after the last hit, by this share of the bar per second (Monster.ot poise_decay_*).
    let poiseDecayDelay = Map.find "poise_decay_delay_ms" Tables.monsterConstants / 1000.0
    let poiseDecayPerSecond = Map.find "poise_decay_%_per_second" Tables.monsterConstants
    /// Seconds a light stun interrupts the player. GUESS (no player constant in the data).
    let playerLightStunDuration = 0.35

    /// Blinded monsters have this much less accuracy. The buff (BuffDefinitions "blinded":
    /// accuracy_rating_+%_final_from_blinded) carries no base value in the data; GUESS.
    let blindAccuracyLess = 20.0

    /// Maim: maim_movement_speed_+%_final -30 (BuffTemplates LabyrinthTrapMaim; base value not in data).
    let maimSlow = 30.0
    /// Hinder: hinder_movement_speed_+%_final; no value in the data, GUESS.
    let hinderSlow = 30.0

    /// Meta gem trigger cost: fixed added cast time (`total_cast_time_+_ms`, Comet +1 s)
    /// counts this many times. Comet = 1 s + 2 x 1 s = 300 energy (forum bug reports
    /// 3623092 / 3860952, user recollection); the tooltip formula would give 200.
    let addedCastTimeEnergyWeight = 2.0

    /// Server tick, PoB data.misc.ServerTickTime.
    let serverTick = 0.033
