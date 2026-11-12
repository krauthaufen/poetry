namespace PoEtry

/// Named game constants. Each one says where it comes from; see RULES.md for
/// how confident we are in the mechanic that uses it.
module Constants =
    let private gc key = Map.find key Tables.gameConstants
    let private mc key = Map.find key Tables.monsterConstants

    let private byLevel (table: float[]) level =
        table.[(max 1 (min level table.Length)) - 1]

    // ---- crit ----
    /// Rage is lost at this rate per second once none has been gained for the delay
    /// (GameConstants BaseRageLossPerMinute, BaseRageLossDelayMs).
    let rageLossPerSecond = Map.find "BaseRageLossPerMinute" Tables.gameConstants / 60.0
    let rageLossDelay = Map.find "BaseRageLossDelayMs" Tables.gameConstants / 1000.0
    /// Culling Strike: enemies at or below this share of life (%) are killed, checked before
    /// the hit's damage (GameConstants CullingStrike<Rarity>Threshold; keyword text).
    let cullingThreshold (rarity: string) =
        match rarity with
        | "magic" -> gc "CullingStrikeMagicThreshold"
        | "rare" -> gc "CullingStrikeRareThreshold"
        | "unique" -> gc "CullingStrikeUniqueThreshold"
        | _ -> gc "CullingStrikeNormalThreshold"
    /// Armour Break (game keyword text): armour at zero is Fully Broken for 12 seconds, and the
    /// enemy takes 20% increased physical damage from hits.
    /// A leech instance is recovered over this many seconds (keyword text).
    let leechSeconds = 1.0
    /// Player hits: physical damage and melee damage each cause 50% more Heavy Stun buildup,
    /// multiplicative with each other (keyword text).
    let playerPhysicalStunMore = 50.0
    let playerMeleeStunMore = 50.0
    let armourBreakDuration = 12.0
    let fullyBrokenArmourTaken = 20.0
    /// Dodge roll: distance in game units and seconds it takes (game files, skill
    /// DodgeRollPlayer: dodge_roll_base_travel_distance 46, cast time 750 ms, not modifiable).
    let dodgeRollDistance = 46.0
    let dodgeRollSeconds = 0.75
    /// Elemental resistances are lowered by 10% per act of the campaign: -60% in the endgame
    /// (PoB CalcSetup resistancePenalty "Endgame (-60%)"; shown in game on the character sheet).
    let endgameResistPenalty = -60.0
    /// Character.ot base_critical_hit_damage_bonus
    let playerBaseCritBonus = 100.0
    let monsterBaseCritBonus = mc "base_critical_hit_damage_bonus"
    /// Monster.ot poise_decay_delay_ms, poise_decay_%_per_second: a monster's Heavy Stun bar
    /// drains by so much of the bar per second once it has gone so long without any buildup.
    let stunDecayDelay = mc "poise_decay_delay_ms" / 1000.0
    let stunDecayPerSecond = mc "poise_decay_%_per_second"

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
    let flammabilityDuration = gc "BaseFlammabilityDuration"
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
    /// DefaultMonsterStats MinionLife / MinionDamage: what a player's minion of that level has.
    let allyLife level = byLevel Tables.allyLife level
    let allyDamage level = byLevel Tables.allyDamage level
    /// MinionGemLevelScaling.dat: the level of a minion from a gem of that level.
    let minionLevel gemLevel = int (byLevel Tables.minionLevel gemLevel)
    /// Spectres and companions are real monsters: their damage comes from the monster column,
    /// brought up to a minion's and then 25% more ("server side mod added in 0.3", PoB
    /// SpectreBeastDamageFixup; not in the data).
    let spectreDamageFixup = 1.25

    // ---- charges and rage ----
    /// GameConstants BaseMaximumRage; Character.ot max_*_charges.
    let baseMaxRage = gc "BaseMaximumRage"
    let baseMaxCharges = 3.0
    /// Character.ot attack_damage_final_permyriad_per_rage = 100: 1% more attack damage per Rage.
    let moreAttackDamagePerRage = 1.0

    // ---- player recovery ----
    /// GameConstants EffectiveMaxDamageForLeech: a hit leeches as if it dealt at most this much.
    let effectiveMaxDamageForLeech = Map.find "EffectiveMaxDamageForLeech" Tables.gameConstants
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
    /// After a monster has been frozen, electrocuted, heavily stunned or pinned, its threshold
    /// for that is higher for a while (GameConstants <X>ThresholdModifier, <X>ModifierDuration).
    let afterBuildup a =
        match a with
        | Freeze -> gc "FreezeThresholdModifier", gc "FreezeModifierDuration"
        | Electrocute -> gc "ElectrocuteThresholdModifier", gc "ElectrocuteModifierDuration"
        | HeavyStun -> gc "HeavyStunThresholdModifier", gc "HeavyStunModifierDuration"
        | Pin -> gc "PinThresholdModifier", gc "PinModifierDuration"
        | _ -> 0.0, 0.0
    /// A chill is at least this strong (keyword text; PoB ailmentData.Chill.min).
    let chillMinEffect = 30.0
    /// Seconds a light stun interrupts the player. GUESS (no player constant in the data).
    let playerLightStunDuration = 0.35

    /// "Being Blinded causes 20% less Accuracy Rating and Evasion Rating" (wiki, Blind; the
    /// buff in BuffDefinitions carries no base value).
    let blindAccuracyLess = 20.0

    /// Maim: maim_movement_speed_+%_final -30 (BuffTemplates LabyrinthTrapMaim; base value not in data).
    let maimSlow = 30.0
    /// Maim also gives 15% reduced Evasion Rating (wiki, Maim).
    let maimEvasionReduced = 15.0
    /// "Hinder is a Debuff that Slows movement speed by 30% unless otherwise specified" (wiki,
    /// Hinder; no value in the data).
    let hinderSlow = 30.0

    /// Meta gem trigger cost: fixed added cast time (`total_cast_time_+_ms`, Comet +1 s)
    /// counts this many times. Comet = 1 s + 2 x 1 s = 300 energy (forum bug reports
    /// 3623092 / 3860952, user recollection); the tooltip formula would give 200.
    let addedCastTimeEnergyWeight = 2.0

    /// Server tick, PoB data.misc.ServerTickTime.
    let serverTick = 0.033

    /// Inevitable Critical Hits: "30% less Critical Damage Bonus for each time Critical Hit chance was re-rolled" (keyword text).
    let inevitableCritLessPerReroll = 0.3

    /// Revealing Weaknesses: "occupy a total of 45% of their maximum Life" (keyword text).
    let revealedWeaknessLifeShare = 0.45

    /// Slow projectiles that hit from a distance as they pass (game table Projectiles,
    /// ProjectileSpeed): Ball Lightning 400. In tenths of a unit per second: Spark's 1000 is the
    /// "10 metres per second" poe2db shows for it.
    let passingProjectileSpeed = Map.ofList [ "Ball Lightning", 40.0 ]

    /// "Skills take 5% longer to perform for each time they Repeat" (keyword text, Repeating Skills).
    let longerPerRepeat = 0.05

    /// Seconds between the pulses of a Frost Bomb (its exposure grows with each): not in the
    /// data; 0.25 s lets one bomb reach its cap just as it detonates. A guess.
    let orbPulseInterval = 0.25

    /// Speed (units per second) of projectiles whose reach is their speed times their lifetime
    /// (game table Projectiles, ProjectileSpeed in tenths of a unit per second, see
    /// `passingProjectileSpeed`): Spark 1000.
    let erraticProjectileSpeed = Map.ofList [ "Spark", 100.0 ]

    /// Projectiles that live for a fixed time: speed (units per second, game table Projectiles:
    /// IceShotShard 2100 tenths) and lifetime in seconds ("Shards last 200ms", wiki Ice Shot).
    let timedProjectiles = Map.ofList [ "Ice Shot", (210.0, 0.2) ]

    /// Degrees per unit of `total_projectile_spread_angle_override`: Ice Shot's shards have 300
    /// and spray "within a roughly 135 degree cone" (wiki); the patch that raised the cone
    /// "by approximately 50%" started from "a 90 Degree Cone", so 200 was 90 degrees.
    let spreadOverrideDegrees = 0.45

    /// Width (units) of one Ice Crystal of a wall: a wall has one per so much of its length
    /// (Frost Wall, 5.6 m: seven). Not in the data. A guess.
    let iceCrystalWidth = 8.0

    /// "80% less damage if destroyed within 0.5 seconds by something other than you" (Frost
    /// Wall, wiki Ice Crystal): a crystal that comes up inside a large monster is broken by it.
    let freshCrystalDamage = 0.2

    /// How far (units) a twister strays to either side of its line ("moves forward
    /// erratically"): not in the data. A guess: twice its own radius.
    let twisterWander = 10.0

    /// Spearfield: "Spears emerge within a 6.9 metre length cone" in front of the player. The
    /// length is the skill's radius; the angle is in neither the data nor the wiki. A guess.
    let spearfieldCone = 90.0

    /// Degrees between neighbouring erratic projectiles (Spark's spray widens with every
    /// spark; the angle is not in the data). A guess.
    let erraticFanStep = 15.0
