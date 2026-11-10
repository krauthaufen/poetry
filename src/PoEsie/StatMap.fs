namespace PoEsie

/// Stats that modify the item they're on rather than the character.
type LocalStat =
    | LocalDamageInc
    | LocalAddedMin of DamageType
    | LocalAddedMax of DamageType
    | LocalAttackSpeedInc
    | LocalCritChance
    | LocalCritChanceInc
    | LocalOther of string

/// What a game stat means to the simulator.
type Meaning =
    | Mods of Mod list
    | Local of LocalStat * float
    /// Understood, but has no effect on offence (life, mana, defences, rarity...).
    | NotOffensive
    /// Understood and offensive, but the engine doesn't model the mechanic yet.
    | Unsimulated of mechanic: string
    /// Engine-internal flags of the game (targeting, visuals, pathing): no meaning for us.
    | Structural
    | Unmapped

/// Game stat id + value -> modifiers. This file is the simulator's knowledge of
/// what stats mean. Ids are read word by word; an id containing any word we
/// don't understand is Unmapped rather than guessed.
module StatMap =
    let private damageWord w =
        match w with
        | "physical" -> Some(OfType Physical)
        | "fire" -> Some(OfType Fire)
        | "cold" -> Some(OfType Cold)
        | "lightning" -> Some(OfType Lightning)
        | "chaos" -> Some(OfType Chaos)
        | "elemental" -> Some Elemental
        | _ -> None

    let private typeWord w =
        match damageWord w with
        | Some (OfType t) -> Some t
        | _ -> None

    /// Words that restrict which skills a mod applies to.
    type private Scope =
        | Cond of Cond
        | MinionWrap
        | Filler

    let private scopeWord w =
        match w with
        | "attack" | "attacks" -> Some(Cond(SkillIs Attack))
        | "spell" | "spells" -> Some(Cond(SkillIs Spell))
        | "melee" -> Some(Cond(SkillIs Melee))
        | "projectile" -> Some(Cond(SkillIs Projectile))
        | "area" -> Some(Cond(SkillIs Area))
        | "totem" -> Some(Cond(SkillIs Totem))
        | "trap" -> Some(Cond(SkillIs(Tag "Trap")))
        | "mine" -> Some(Cond(SkillIs(Tag "Mine")))
        | "warcry" -> Some(Cond(SkillIs Warcry))
        | "bow" -> Some(Cond(SkillIs Bow))
        | "crossbow" -> Some(Cond(SkillIs Crossbow))
        | "mace" -> Some(Cond(SkillIs Mace))
        | "spear" -> Some(Cond(SkillIs Spear))
        | "quarterstaff" -> Some(Cond(SkillIs Quarterstaff))
        | "sword" -> Some(Cond(SkillIs Sword))
        | "axe" -> Some(Cond(SkillIs Axe))
        | "dagger" -> Some(Cond(SkillIs Dagger))
        | "claw" -> Some(Cond(SkillIs Claw))
        | "flail" -> Some(Cond(SkillIs Flail))
        | "wand" -> Some(Cond(SkillIs Wand))
        | "staff" -> Some(Cond(SkillIs Staff))
        | "unarmed" -> Some(Cond(SkillIs Unarmed))
        | "minion" | "minions" -> Some MinionWrap
        | "two" -> Some(Cond(SkillIs(Tag "TwoHanded")))
        | "one" -> Some(Cond(SkillIs(Tag "OneHanded")))
        | "handed" | "weapon" | "weapons" -> Some Filler
        | "herald" -> Some(Cond(SkillIs Herald))
        | "strength" -> Some(Cond(SkillIs(Tag "StrRequirement")))
        | "dexterity" -> Some(Cond(SkillIs(Tag "DexRequirement")))
        | "intelligence" -> Some(Cond(SkillIs(Tag "IntRequirement")))
        | "channelled" | "channelling" -> Some(Cond(SkillIs Channelling))
        | "triggered" -> Some(Cond(SkillIs Triggered))
        | "grenade" -> Some(Cond(SkillIs(Tag "Grenade")))
        | "plant" -> Some(Cond(SkillIs(Tag "Plant")))
        | "lightning" -> Some(Cond(SkillIs(Tag "Lightning")))
        | "fire" -> Some(Cond(SkillIs(Tag "Fire")))
        | "cold" -> Some(Cond(SkillIs(Tag "Cold")))
        | "chaos" -> Some(Cond(SkillIs(Tag "Chaos")))
        | "physical" -> Some(Cond(SkillIs(Tag "Physical")))
        | "base" | "global" | "non" | "skill" | "skills" | "with" | "hit" | "hits" | "of" | "to" | "your" -> Some Filler
        | _ -> None

    /// Enemy status words, as they appear in "vs bleeding enemies".
    let private statusWord w =
        match w with
        | "bleeding" -> Some "Bleeding"
        | "poisoned" -> Some "Poisoned"
        | "ignited" | "burning" -> Some "Ignited"
        | "shocked" -> Some "Shocked"
        | "chilled" -> Some "Chilled"
        | "frozen" -> Some "Frozen"
        | "electrocuted" -> Some "Electrocuted"
        | "blinded" -> Some "Blinded"
        | "dazed" -> Some "Dazed"
        | "maimed" -> Some "Maimed"
        | "pinned" -> Some "Pinned"
        | "cursed" -> Some "Cursed"
        | "stunned" -> Some "HeavilyStunned"
        | _ -> None

    let private titleCase (words: string list) =
        words |> List.map (fun w -> if w = "" then w else string (System.Char.ToUpperInvariant w.[0]) + w.Substring 1) |> String.concat ""

    /// Pull conditions out of the words: "vs <status> enemies", "against <status> enemies",
    /// "while <state...>" (a buff the configuration has to provide, e.g. "WhileShapeshifted").
    let private conditions (words: string list) =
        let rec go acc rest =
            match rest with
            | ("vs" | "against") :: st :: "enemies" :: tail when (statusWord st).IsSome ->
                go (EnemyHas (statusWord st).Value :: acc) tail
            | "to" :: "rare" :: "and" :: "unique" :: "enemies" :: tail
            | ("vs" | "against") :: "rare" :: "and" :: "unique" :: "enemies" :: tail ->
                go (EnemyHas "RareOrUnique" :: acc) tail
            | ("vs" | "against") :: "enemies" :: "affected" :: "by" :: "status" :: "ailments" :: tail ->
                go (EnemyHas "AnyAilment" :: acc) tail
            | ("vs" | "against") :: "enemies" :: "on" :: "low" :: "life" :: tail -> go (EnemyHas "LowLife" :: acc) tail
            | ("vs" | "against") :: "enemies" :: "on" :: "full" :: "life" :: tail -> go (EnemyHas "FullLife" :: acc) tail
            | ("when" | "while") :: "on" :: "low" :: "life" :: tail -> go (WhileBuff "LowLife" :: acc) tail
            | ("when" | "while") :: "on" :: "full" :: "life" :: tail -> go (WhileBuff "FullLife" :: acc) tail
            | ("when" | "while") :: "on" :: "low" :: "mana" :: tail -> go (WhileBuff "LowMana" :: acc) tail
            | ("when" | "while") :: "on" :: "full" :: "mana" :: tail -> go (WhileBuff "FullMana" :: acc) tail
            | "if" :: ("crit" | "crits") :: "recently" :: tail | "if" :: "have" :: "crit" :: "recently" :: tail ->
                go (WhileBuff "CritRecently" :: acc) tail
            | "if" :: ("hit" | "hits") :: "recently" :: tail -> go (WhileBuff "HitRecently" :: acc) tail
            | "if" :: "consumed" :: c :: "charge" :: "recently" :: tail when c = "power" || c = "frenzy" || c = "endurance" ->
                go (WhileBuff("Consumed" + titleCase [ c; "charge" ] + "Recently") :: acc) tail
            | "if" :: "enemy" :: "killed" :: "recently" :: tail
            | "if" :: "killed" :: "recently" :: tail -> go (WhileBuff "KilledRecently" :: acc) tail
            | "per" :: c :: "charge" :: tail when c = "power" || c = "frenzy" || c = "endurance" ->
                go (PerStack(titleCase [ c; "charge" ], 1.0) :: acc) tail
            | "per" :: "rage" :: tail -> go (PerStack("Rage", 1.0) :: acc) tail
            | "with" :: c :: "charges" :: tail when c = "power" || c = "frenzy" || c = "endurance" ->
                go (WhileBuff(titleCase [ c; "charge" ]) :: acc) tail
            | "when" :: "charged" :: tail -> go (WhileBuff "PowerCharge" :: acc) tail
            | "in" :: "weapon" :: "set" :: "one" :: tail -> go (Not(SkillIs(Tag "WeaponSet2")) :: acc) tail
            | "in" :: "weapon" :: "set" :: "two" :: tail -> go (SkillIs(Tag "WeaponSet2") :: acc) tail
            | "while" :: state when not state.IsEmpty -> List.rev acc, [], Some(WhileBuff(titleCase state))
            | w :: tail ->
                let a, ws, extra = go acc tail
                a, w :: ws, extra
            | [] -> List.rev acc, [], None
        let conds, rest, whileCond = go [] words
        conds @ Option.toList whileCond, rest

    /// Read scope words; None if any word is unknown.
    let private scopes words =
        let conds, words = conditions words
        let parsed = words |> List.map scopeWord
        if List.forall Option.isSome parsed then Some((conds |> List.map Cond) @ List.choose id parsed) else None

    let private apply (scope: Scope list) (m: Mod) =
        let conds = scope |> List.choose (function Cond c -> Some c | _ -> None)
        let m = { m with Conds = conds @ m.Conds }
        if List.contains MinionWrap scope then { m with Stat = ForMinions m.Stat } else m

    let private split (s: string) = s.Split('_') |> List.ofArray

    /// "<scope words> [type] damage [scope words] +%" and friends.
    let private damageMod kind (words: string list) value =
        match List.tryFindIndex ((=) "damage") words with
        | None -> None
        | Some i ->
            let around = List.take i words @ List.skip (i + 1) words
            let types = around |> List.choose damageWord
            let others = around |> List.filter (fun w -> (damageWord w).IsNone)
            match scopes others, types with
            | Some sc, [] -> Some [ apply sc (Mod.make kind (Damage AllDamage) value) ]
            | Some sc, [ t ] -> Some [ apply sc (Mod.make kind (Damage t) value) ]
            | _ -> None

    let private ailmentWord w =
        match w with
        | "ignite" -> Some Ignite
        | "bleed" | "bleeding" -> Some Bleed
        | "poison" -> Some Poison
        | "shock" -> Some Shock
        | "chill" -> Some Chill
        | "freeze" -> Some Freeze
        | "electrocute" -> Some Electrocute
        | "daze" -> Some Daze
        | "maim" -> Some Maim
        | "blind" -> Some Blind
        | "pin" -> Some Pin
        | _ -> None

    let private allAilments = [ Ignite; Bleed; Poison; Shock; Chill; Freeze; Electrocute; Daze; Maim; Blind; Hinder; Pin ]

    /// Explicit meanings for ids that don't follow a pattern.
    /// The player's pools, defences, resistances, recovery and costs.
    let private defensive (id: string) (v: float) : Meaning option =
        let one m = Some(Mods [ m ])
        let each stats = Some(Mods(stats |> List.map (fun s -> Mod.base' s v)))
        match id with
        | "additional_strength" -> one (Mod.base' (Attribute "Str") v)
        | "additional_dexterity" -> one (Mod.base' (Attribute "Dex") v)
        | "additional_intelligence" -> one (Mod.base' (Attribute "Int") v)
        | "additional_all_attributes" -> each [ Attribute "Str"; Attribute "Dex"; Attribute "Int" ]
        | "additional_strength_and_dexterity" -> each [ Attribute "Str"; Attribute "Dex" ]
        | "additional_strength_and_intelligence" -> each [ Attribute "Str"; Attribute "Int" ]
        | "additional_dexterity_and_intelligence" -> each [ Attribute "Dex"; Attribute "Int" ]
        | "you_and_allies_in_presence_all_damage_can_ignite" | "all_damage_can_ignite" -> one (Mod.flag (Named "AllDamageIgnites"))
        | "all_damage_can_chill" -> one (Mod.flag (Named "AllDamageChills"))
        | "all_damage_can_shock" -> one (Mod.flag (Named "AllDamageShocks"))
        | "gain_archon_elemental_when_you_ignite_enemy_chance_%" -> one (Mod.base' (Named "ElementalArchonOnIgnite") v)
        | "archon_effect_on_you_+%" -> one (Mod.inc (Named "ArchonEffect") v)
        | "archon_duration_+%" -> one (Mod.inc (Named "ArchonDuration") v)
        | "archon_delay_expires_x%_faster" -> one (Mod.inc (Named "ArchonRecoveryFaster") v)
        | "archon_no_recovery_period" -> one (Mod.flag (Named "ArchonNoRecovery"))
        | "archon_grants_all_elemental_resistance_+%" | "archon_grants_base_critical_strike_multiplier_+"
        | "archon_grants_critical_strike_chance_+%" | "archon_grants_movement_speed_+%" ->
            one (Mod.base' (Named("ArchonGrants:" + id)) v)
        | "base_maximum_ward" -> one (Mod.base' (Named "Ward") v)
        | "maximum_ward_+%" -> one (Mod.inc (Named "Ward") v)
        | "ward_regeneration_rate_+%" -> one (Mod.inc (Named "WardRegen") v)
        | "recover_X_ward_on_block" -> one (Mod.base' (Named "WardOnBlock") v)
        | "remnant_effect_+%" -> one (Mod.inc (Named "RemnantEffect") v)
        | "chance_%_to_create_additional_remnant" -> one (Mod.base' (Named "ExtraRemnantChance") v)
        | "chance_to_not_consume_infusion_%" -> one (Mod.base' (Named "InfusionKeepChance") v)
        | "infusion_duration_+%" -> one (Mod.inc (Named "InfusionDuration") v)
        | "additional_maximum_infusion_stacks" -> one (Mod.base' (Named "MaxInfusions") v)
        | "maximum_fire_infusion_stacks" -> one (Mod.base' (Named "MaxFireInfusion") v)
        | "maximum_cold_infusion_stacks" -> one (Mod.base' (Named "MaxColdInfusion") v)
        | "maximum_lightning_infusion_stacks" -> one (Mod.base' (Named "MaxLightningInfusion") v)
        | "maximum_verisium_infusion_stacks" -> one (Mod.base' (Named "MaxVerisiumInfusion") v)
        | "remnant_delay_X_ms" -> one (Mod.base' (Named "RemnantDelayMs") v)
        | "hit_for_%_max_life_es_on_max_infernal_flame" -> one (Mod.base' (Named "InfernalFlameBurn") v)
        | "lose_%_of_infernal_flame_on_reaching_max" | "lose_%_of_max_infernal_flame_per_minute" -> Some NotOffensive
        | "base_block_chance_luck" -> one (Mod.flag (Named "LuckyBlock"))
        | "base_block_%_damage_taken" -> one (Mod.base' (Named "BlockedDamageTaken") v)
        | "block_chance_+%" -> one (Mod.inc (Named "Block") v)
        | "additional_block_%" -> one (Mod.base' (Named "Block") v)
        | "additional_maximum_block_%" -> one (Mod.base' (Named "MaxBlock") v)
        | "base_maximum_life" -> one (Mod.base' MaxLife v)
        | "maximum_life_+%" -> one (Mod.inc MaxLife v)
        | "base_maximum_mana" -> one (Mod.base' MaxMana v)
        | "maximum_mana_+%" -> one (Mod.inc MaxMana v)
        | "base_maximum_energy_shield" -> one (Mod.base' MaxEnergyShield v)
        | "maximum_energy_shield_+%" -> one (Mod.inc MaxEnergyShield v)
        | "base_evasion_rating" -> one (Mod.base' Evasion v)
        | "evasion_rating_+%" -> one (Mod.inc Evasion v)
        | "base_physical_damage_reduction_rating" -> one (Mod.base' Armour v)
        | "physical_damage_reduction_rating_+%" -> one (Mod.inc Armour v)
        | "evasion_and_physical_damage_reduction_rating_+%" -> Some(Mods [ Mod.inc Evasion v; Mod.inc Armour v ])
        | "armour_and_energy_shield_+%" -> Some(Mods [ Mod.inc Armour v; Mod.inc MaxEnergyShield v ])
        | "evasion_and_energy_shield_+%" -> Some(Mods [ Mod.inc Evasion v; Mod.inc MaxEnergyShield v ])
        | "global_defences_+%" | "armour_evasion_energy_shield_+%" | "global_armour_evasion_energy_shield_+%" ->
            Some(Mods [ Mod.inc Armour v; Mod.inc Evasion v; Mod.inc MaxEnergyShield v ])
        | "armour_evasion_energy_shield_+%_while_wielding_quarterstaff" ->
            Some(Mods([ Armour; Evasion; MaxEnergyShield ] |> List.map (fun st -> Mod.inc st v |> Mod.whenever (WhileBuff "WieldingQuarterstaff"))))
        | "maximum_energy_shield_+1_per_x_body_armour_evasion_rating" -> one (Mod.base' (Named "EnergyShieldPerBodyEvasion") v)
        | "body_armour_evasion_rating_+%" -> one (Mod.inc (Named "BodyArmourEvasion") v)
        | "maximum_energy_shield_from_body_armour_+%" -> one (Mod.inc (Named "BodyArmourEnergyShield") v)
        | "body_armour_armour_+%" -> one (Mod.inc (Named "BodyArmourArmour") v)
        | "base_deflection_rating_%_of_evasion_rating" -> one (Mod.base' (Named "DeflectionPercentOfEvasion") v)
        | "base_resist_all_elements_%" -> each (DamageType.elemental |> List.map Resist)
        | "additional_maximum_all_elemental_resistances_%" -> each (DamageType.elemental |> List.map MaxResist)
        | "base_chaos_damage_resistance_%" -> one (Mod.base' (Resist Chaos) v)
        | "life_regeneration_rate_per_minute_%" -> one (Mod.base' LifeRegenPercent (v / 60.0))
        | "base_life_regeneration_rate_per_minute" -> one (Mod.base' LifeRegen (v / 60.0))
        | "life_regeneration_rate_+%" -> one (Mod.inc LifeRegen v)
        | "mana_regeneration_rate_+%" -> one (Mod.inc ManaRegen v)
        | "base_mana_cost_efficiency_+%" -> one (Mod.inc ManaCostEfficiency v)
        | "base_spell_mana_cost_efficiency_+%" -> one (Mod.inc ManaCostEfficiency v |> Mod.whenever (SkillIs Spell))
        | "base_mana_cost_-%" -> one (Mod.inc ManaCost -v)
        | "generate_x_charges_for_any_flask_per_minute" -> one (Mod.base' (Named "FlaskChargesPerSecond") (v / 60.0))
        | "base_mana_gained_on_enemy_death" -> one (Mod.base' ManaOnKill v)
        | "recover_%_maximum_mana_on_kill" -> one (Mod.base' (Named "ManaPercentOnKill") v)
        | "recover_x%_of_maximum_mana_when_you_consume_a_power_charge" -> one (Mod.base' (Named "ManaPercentOnConsumePower") v)
        | "base_mana_leech_from_physical_attack_damage_permyriad" -> one (Mod.base' ManaLeech (v / 100.0) |> Mod.whenever (SkillIs Attack))
        | "base_damage_removed_from_mana_before_life_%" -> one (Mod.base' (Named "DamageRemovedFromManaFirst") v)
        // ---- resistances ----
        | "additional_maximum_all_resistances_%" -> each (DamageType.all |> List.filter ((<>) Physical) |> List.map MaxResist)
        | "fire_and_cold_damage_resistance_%" -> each [ Resist Fire; Resist Cold ]
        | "fire_and_lightning_damage_resistance_%" -> each [ Resist Fire; Resist Lightning ]
        | "cold_and_lightning_damage_resistance_%" -> each [ Resist Cold; Resist Lightning ]
        // ---- energy shield ----
        | "energy_shield_delay_-%" -> one (Mod.inc (Named "ESRechargeFaster") v)
        | "energy_shield_recharge_rate_+%" -> one (Mod.inc (Named "ESRechargeRate") v)
        | "base_energy_shield_regeneration_rate_per_minute_%" -> one (Mod.base' (Named "ESRegenPercent") (v / 60.0))
        | "regenerate_energy_shield_instead_of_life" -> one (Mod.flag (Named "RegenESInsteadOfLife"))
        | "maximum_life_%_to_convert_to_maximum_energy_shield" -> one (Mod.base' (Named "LifeConvertedToES") v)
        | "maximum_life_%_to_gain_as_maximum_energy_shield" -> one (Mod.base' (Named "LifeGainedAsES") v)
        | "base_all_damage_bypass_energy_shield_%" -> one (Mod.base' (Named "BypassES") v)
        // ---- recovery rates ----
        | "life_recovery_rate_+%" -> one (Mod.inc (Named "LifeRecoveryRate") v)
        | "mana_recovery_rate_+%" -> one (Mod.inc (Named "ManaRecoveryRate") v)
        | "energy_shield_recovery_rate_+%" -> one (Mod.inc (Named "ESRecoveryRate") v)
        // ---- on kill / hit / block ----
        | "recover_%_maximum_life_on_kill" -> one (Mod.base' (Named "LifePercentOnKill") v)
        | "base_life_gained_on_enemy_death" -> one (Mod.base' (Named "LifeOnKill") v)
        | "recover_energy_shield_%_on_kill" -> one (Mod.base' (Named "ESPercentOnKill") v)
        | "base_life_gain_per_target" -> one (Mod.base' (Named "LifeOnHit") v |> Mod.whenever (SkillIs Attack))
        | "mana_gain_per_target" -> one (Mod.base' (Named "ManaOnHit") v |> Mod.whenever (SkillIs Attack))
        | "base_life_gained_on_spell_hit" -> one (Mod.base' (Named "LifeOnHit") v |> Mod.whenever (SkillIs Spell))
        | "life_gained_on_block" | "recover_X_life_on_block" -> one (Mod.base' (Named "LifeOnBlock") v)
        | "mana_gained_on_block" -> one (Mod.base' (Named "ManaOnBlock") v)
        // ---- leech ----
        | "base_life_leech_from_physical_attack_damage_permyriad" -> one (Mod.base' (Named "LifeLeech") (v / 100.0) |> Mod.whenever (SkillIs Attack))
        | "base_life_leech_amount_+%" -> one (Mod.inc (Named "LifeLeechAmount") v)
        | "base_mana_leech_amount_+%" -> one (Mod.inc (Named "ManaLeechAmount") v)
        | "base_cannot_leech_mana" -> one (Mod.more (Named "ManaLeechAmount") -100.0)
        // ---- recoup: part of damage taken comes back over 4 s ----
        | "damage_taken_goes_to_life_over_4_seconds_%" -> one (Mod.base' (Named "RecoupLife") v)
        | "damage_taken_goes_to_mana_%" -> one (Mod.base' (Named "RecoupMana") v)
        | "physical_damage_taken_recouped_as_life_%" -> one (Mod.base' (Named "RecoupLife:Physical") v)
        | "recoup_%_elemental_damage_as_energy_shield" -> one (Mod.base' (Named "RecoupES:Elemental") v)
        | "recoup_speed_+%" -> one (Mod.inc (Named "RecoupSpeed") v)
        // ---- flasks ----
        | "flask_life_to_recover_+%" -> one (Mod.inc (Named "LifeFlaskAmount") v)
        | "flask_mana_to_recover_+%" -> one (Mod.inc (Named "ManaFlaskAmount") v)
        | "flask_life_and_mana_to_recover_+%" -> Some(Mods [ Mod.inc (Named "LifeFlaskAmount") v; Mod.inc (Named "ManaFlaskAmount") v ])
        | "flask_life_recovery_rate_+%" -> one (Mod.inc (Named "LifeFlaskRate") v)
        | "flask_mana_recovery_rate_+%" -> one (Mod.inc (Named "ManaFlaskRate") v)
        | "flask_charges_gained_+%" -> Some(Mods [ Mod.inc (Named "LifeFlaskChargesGained") v; Mod.inc (Named "ManaFlaskChargesGained") v ])
        | "life_flask_charges_gained_+%" -> one (Mod.inc (Named "LifeFlaskChargesGained") v)
        | "mana_flask_charges_gained_+%" -> one (Mod.inc (Named "ManaFlaskChargesGained") v)
        | "flask_charges_used_+%" -> Some(Mods [ Mod.inc (Named "LifeFlaskChargesUsed") v; Mod.inc (Named "ManaFlaskChargesUsed") v ])
        | "flask_mana_charges_used_+%" -> one (Mod.inc (Named "ManaFlaskChargesUsed") v)
        | "flask_life_charges_used_+%" -> one (Mod.inc (Named "LifeFlaskChargesUsed") v)
        | "generate_x_charges_for_life_flasks_per_minute" -> one (Mod.base' (Named "LifeFlaskChargesPerSecond") (v / 60.0))
        | "generate_x_charges_for_mana_flasks_per_minute" -> one (Mod.base' (Named "ManaFlaskChargesPerSecond") (v / 60.0))
        // ---- mitigation ----
        | "armour_%_applies_to_fire_cold_lightning_damage" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Named("ArmourAppliesTo:" + DamageType.name t)) v)))
        | "base_damage_taken_+%" -> one (Mod.inc (Named "Taken:All") v)
        | "physical_attack_damage_taken_+" -> one (Mod.base' (Named "FlatPhysicalAttackTaken") v)
        | "deflection_rating_+%" -> one (Mod.inc Deflection v)
        | "additional_block_chance_against_projectiles_%" -> one (Mod.base' (Named "BlockVsProjectiles") v)
        | "base_self_critical_strike_multiplier_-%" -> one (Mod.base' (Named "LessCritDamageTaken") v)
        // ---- thresholds, stuns and ailments on yourself ----
        | "stun_threshold_+" -> one (Mod.base' (Named "StunThreshold") v)
        | "stun_threshold_+%" -> one (Mod.inc (Named "StunThreshold") v)
        | "stun_threshold_+_from_%_maximum_energy_shield" -> one (Mod.base' (Named "StunThresholdFromES") v)
        | "ailment_threshold_+" -> one (Mod.base' (Named "PlayerAilmentThreshold") v)
        | "ailment_threshold_+%" -> one (Mod.inc (Named "PlayerAilmentThreshold") v)
        | "ailment_threshold_+_from_%_maximum_energy_shield" -> one (Mod.base' (Named "AilmentThresholdFromES") v)
        | "freeze_threshold_+%" -> one (Mod.inc (Named "FreezeThreshold") v)
        | "gain_%_maximum_energy_shield_as_freeze_threshold_+" -> one (Mod.base' (Named "FreezeThresholdFromES") v)
        | "avoid_stun_%" -> one (Mod.base' (Named "AvoidStun") v)
        | "avoid_all_elemental_status_%" -> Some(Mods([ "Ignite"; "Chill"; "Freeze"; "Shock" ] |> List.map (fun a -> Mod.base' (Named("Avoid:" + a)) v)))
        | "base_avoid_ignite_%" -> one (Mod.base' (Named "Avoid:Ignite") v)
        | "base_avoid_chill_%" -> one (Mod.base' (Named "Avoid:Chill") v)
        | "base_avoid_freeze_%" -> one (Mod.base' (Named "Avoid:Freeze") v)
        | "base_avoid_shock_%" -> one (Mod.base' (Named "Avoid:Shock") v)
        | "base_cannot_be_ignited" -> one (Mod.base' (Named "Avoid:Ignite") 100.0)
        | "base_cannot_be_chilled_or_frozen" -> Some(Mods [ Mod.base' (Named "Avoid:Chill") 100.0; Mod.base' (Named "Avoid:Freeze") 100.0 ])
        | "cannot_be_poisoned" -> one (Mod.base' (Named "Avoid:Poison") 100.0)
        | "shocked_effect_on_self_+%" -> one (Mod.inc (Named "SelfEffect:Shock") v)
        | "chill_effectiveness_on_self_+%" -> one (Mod.inc (Named "SelfEffect:Chill") v)
        | "ignite_effect_on_self_+%" -> one (Mod.inc (Named "SelfEffect:Ignite") v)
        | "self_bleed_duration_+%" -> one (Mod.inc (Named "SelfDuration:Bleed") v)
        | "self_poison_duration_+%" -> one (Mod.inc (Named "SelfDuration:Poison") v)
        | "base_self_ignite_duration_-%" -> one (Mod.inc (Named "SelfDuration:Ignite") -v)
        | "base_self_chill_duration_-%" -> one (Mod.inc (Named "SelfDuration:Chill") -v)
        | "base_self_freeze_duration_-%" -> one (Mod.inc (Named "SelfDuration:Freeze") -v)
        | "base_self_shock_duration_-%" -> one (Mod.inc (Named "SelfDuration:Shock") -v)
        | "self_elemental_status_duration_-%" ->
            Some(Mods([ "Ignite"; "Chill"; "Freeze"; "Shock" ] |> List.map (fun a -> Mod.inc (Named("SelfDuration:" + a)) -v)))
        | "base_all_ailment_duration_on_self_+%" ->
            Some(Mods([ "Ignite"; "Chill"; "Freeze"; "Shock"; "Bleed"; "Poison" ] |> List.map (fun a -> Mod.inc (Named("SelfDuration:" + a)) v)))
        | _ ->
            // "base_<type>_damage_resistance_%", "base_maximum_<type>_damage_resistance_%",
            // "<type>_hit_and_dot_damage_%_taken_as_<type>"
            match split id with
            | [ from; "hit"; "and"; "dot"; "damage"; "%"; "taken"; "as"; into ] when (typeWord from).IsSome && (typeWord into).IsSome ->
                one (Mod.base' (Named(sprintf "TakenAs:%s:%s" (DamageType.name (typeWord from).Value) (DamageType.name (typeWord into).Value))) v)
            | [ "base"; t; "damage"; "resistance"; "%" ] | [ t; "damage"; "resistance"; "%" ] when (typeWord t).IsSome -> one (Mod.base' (Resist (typeWord t).Value) v)
            // "physical_damage_taken_%_as_fire", "base_fire_hit_damage_taken_%_as_physical"
            | [ from; "damage"; "taken"; "%"; "as"; into ] | [ "base"; from; "hit"; "damage"; "taken"; "%"; "as"; into ] when (typeWord from).IsSome && (typeWord into).IsSome ->
                one (Mod.base' (Named(sprintf "TakenAs:%s:%s" (DamageType.name (typeWord from).Value) (DamageType.name (typeWord into).Value))) v)
            | [ t; "damage"; "taken"; "+%" ] when (typeWord t).IsSome -> one (Mod.inc (Named("Taken:" + DamageType.name (typeWord t).Value)) v)
            | [ "base"; "armour"; "%"; "applies"; "to"; t; "damage" ] when (typeWord t).IsSome -> one (Mod.base' (Named("ArmourAppliesTo:" + DamageType.name (typeWord t).Value)) v)
            | [ "base"; "maximum"; t; "damage"; "resistance"; "%" ] when (typeWord t).IsSome -> one (Mod.base' (MaxResist (typeWord t).Value) v)
            | _ -> None

    let private explicit (id: string) (v: float) : Meaning option =
        let one m = Some(Mods [ m ])
        match id with
        | "critical_strike_chance_+%" -> one (Mod.inc CritChance v)
        | "spell_critical_strike_chance_+%" -> one (Mod.inc CritChance v |> Mod.whenever (SkillIs Spell))
        | "attack_critical_strike_chance_+%" -> one (Mod.inc CritChance v |> Mod.whenever (SkillIs Attack))
        | "base_critical_strike_multiplier_+" -> one (Mod.base' CritBonus v)
        | "base_spell_critical_strike_multiplier_+" -> one (Mod.base' CritBonus v |> Mod.whenever (SkillIs Spell))
        | "base_attack_critical_strike_multiplier_+" -> one (Mod.base' CritBonus v |> Mod.whenever (SkillIs Attack))
        | "critical_strike_multiplier_+%" -> one (Mod.inc CritBonus v)
        | "attack_speed_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Attack))
        | "base_cast_speed_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Spell))
        | "skill_speed_+%" -> one (Mod.inc Speed v)
        | "action_speed_+%" -> one (Mod.inc Speed v)
        | "minion_attack_speed_+%" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (SkillIs Attack))
        | "minion_cast_speed_+%" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (SkillIs Spell))
        | "minion_attack_and_cast_speed_+%" -> one (Mod.inc (ForMinions Speed) v)
        | "base_cooldown_speed_+%" -> one (Mod.inc CooldownRecovery v)
        | "base_cooldown_speed_+%_final" -> one (Mod.more CooldownRecovery v)
        | "support_double_number_of_cooldown_uses" -> one (Mod.base' (Named "DoubleCooldownUses") 1.0)
        | "lose_%_of_life_es_and_mana_when_skill_is_used" -> one (Mod.base' (Named "LosePoolsOnUse") v)
        | "support_cooldown_reduction_cooldown_recovery_+%" -> one (Mod.inc CooldownRecovery v)
        | "skill_effect_duration_+%" -> one (Mod.inc SkillDuration v)
        | "support_more_duration_skill_effect_duration_+%_final"
        | "support_reduced_duration_skill_effect_duration_+%_final" -> one (Mod.more SkillDuration v)
        | "ailment_chance_+%" -> Some(Mods(allAilments |> List.map (fun a -> Mod.inc (AilmentChance a) v)))
        | "ailment_effect_+%" | "base_ailment_effect_+%" -> one (Mod.inc (AilmentMagnitude None) v)
        | "base_damaging_ailment_effect_+%" ->
            Some(Mods(Ailment.damaging |> List.map (fun a -> Mod.inc (AilmentMagnitude(Some a)) v)))
        | "hit_damage_freeze_multiplier_+%" -> one (Mod.inc (Buildup Freeze) v)
        | "hit_damage_stun_multiplier_+%" -> one (Mod.inc (Buildup HeavyStun) v)
        | "hit_damage_electrocute_multiplier_+%" -> one (Mod.inc (Buildup Electrocute) v)
        | "base_chance_to_inflict_bleeding_%" | "bleed_on_hit_with_attacks_%" -> one (Mod.base' (AilmentChance Bleed) v)
        | "base_chance_to_poison_on_hit_%" -> one (Mod.base' (AilmentChance Poison) v)
        | "base_chance_to_daze_%" -> one (Mod.base' (AilmentChance Daze) v)
        | "attacks_chance_to_blind_on_hit_%" -> one (Mod.base' (AilmentChance Blind) v |> Mod.whenever (SkillIs Attack))
        | "global_chance_to_blind_on_hit_%" -> one (Mod.base' (AilmentChance Blind) v)
        | "chance_to_deal_double_damage_%" -> one (Mod.base' DoubleDamageChance v)
        | "base_reduce_enemy_elemental_resistance_%" ->
            Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Penetration t) v)))
        | "global_cannot_crit" -> one (Mod.override' CritChance 0.0)
        | "damaging_ailment_duration_+%" ->
            Some(Mods(Ailment.damaging |> List.map (fun a -> Mod.inc (AilmentDuration(Some a)) v)))
        | "hit_damage_pin_multiplier_+%" -> one (Mod.inc (Buildup Pin) v)
        | "base_chance_to_shock_%" -> one (Mod.base' (AilmentChance Shock) v)
        | "base_chance_to_ignite_%" -> one (Mod.base' (AilmentChance Ignite) v)
        | "attacks_inflict_bleeding_on_hit" -> one (Mod.base' (AilmentChance Bleed) 100.0 |> Mod.whenever (SkillIs Attack))
        | "cannot_cause_bleeding" -> one (Mod.more (AilmentChance Bleed) -100.0)
        | "reduce_enemy_elemental_resistance_%" ->
            Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Penetration t) v)))
        | "critical_hit_damaging_ailment_effect_+%" ->
            Some(Mods(Ailment.damaging |> List.map (fun a -> Mod.inc (AilmentMagnitude(Some a)) v |> Mod.whenever IsCrit)))
        | "damaging_ailments_deal_damage_+%_faster" | "faster_burn_%" -> Some(Unsimulated "faster ailments")
        | "non_skill_base_all_damage_%_to_gain_as_cold_fire_lightning" ->
            Some(Mods [ for into in DamageType.elemental do for from in DamageType.all do if from <> into then yield Mod.base' (GainAs(from, into)) v ])
        | "attack_additional_critical_strike_chance_permyriad" -> one (Mod.base' CritChance (v / 100.0) |> Mod.whenever (SkillIs Attack))
        | "additional_critical_strike_chance_permyriad" -> one (Mod.base' CritChance (v / 100.0))
        | "all_attributes_+%" -> Some(Mods([ "Str"; "Dex"; "Int" ] |> List.map (fun a -> Mod.inc (Attribute a) v)))
        | "strength_+%" -> one (Mod.inc (Attribute "Str") v)
        | "dexterity_+%" -> one (Mod.inc (Attribute "Dex") v)
        | "intelligence_+%" -> one (Mod.inc (Attribute "Int") v)
        // "Buff grants X% increased damage per Charge": every charge of every kind counts
        | "charge_regulation_damage_per_charge_granted_+%" ->
            Some(Mods([ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ] |> List.map (fun c -> Mod.inc (Damage AllDamage) v |> Mod.whenever (PerStack(c, 1.0)))))
        | "quarterstaff_skills_that_consume_power_charges_count_as_consuming_x_additional_power_charges" ->
            one (Mod.base' (Named "ExtraPowerChargesConsumed") v |> Mod.whenever (SkillIs Quarterstaff))
        | "skills_that_consume_power_charges_count_as_consuming_x_additional_power_charges" ->
            one (Mod.base' (Named "ExtraPowerChargesConsumed") v)
        | "consume_frenzy_power_and_endurance_charge_every_x_ms" -> one (Mod.base' (Named "ConsumeEachChargeEveryMs") v)
        | "ascendancy_gemling_charge_generator_buff_display" | "gain_random_charge_every_X_ms_from_total_socketed_gem_attribute_requirements" ->
            one (Mod.flag (Named "GeneratesRandomCharges"))
        | "base_active_skill_buff_stack_gain_frequency_ms" -> one (Mod.base' (Named "MoteGainEveryMs") v)
        | "skill_specific_stat_description_mode" -> Some Structural
        | "charge_duration_+%" ->
            Some(Mods([ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ] |> List.map (fun c -> Mod.inc (Named(c + "Duration")) v)))
        | "power_charge_duration_+%" -> one (Mod.inc (Named "PowerChargeDuration") v)
        | "frenzy_charge_duration_+%" | "base_frenzy_charge_duration_+%" -> one (Mod.inc (Named "FrenzyChargeDuration") v)
        | "endurance_charge_duration_+%" | "base_endurance_charge_duration_+%" -> one (Mod.inc (Named "EnduranceChargeDuration") v)
        | "energy_generated_+%" -> one (Mod.inc (Named "EnergyGenerated") v)
        | "elemental_penetration_can_go_down_to_override" -> one (Mod.base' (Named "PenetrationFloor") v)
        // ---- keystones ----
        // "30% less Critical Damage Bonus when on Full Life, 30% more when on Low Life"
        | "keystone_pain_attunement" ->
            Some(Mods [ Mod.more CritBonus (-(abs v)) |> Mod.whenever (WhileBuff "FullLife"); Mod.more CritBonus (abs v) |> Mod.whenever (WhileBuff "LowLife") ])
        // despite its name a plain "X% more Critical Damage Bonus" (Grinning Immolation)
        | "pain_attunement_keystone_critical_strike_multiplier_+%_final" -> one (Mod.more CritBonus v)
        | "keystone_zealots_oath" -> Some(Mods [ Mod.flag (Named "RegenESInsteadOfLife"); Mod.flag (Named "NoESRecharge") ])
        | "keystone_iron_reflexes" -> one (Mod.flag (Named "EvasionToArmour"))
        // "75% of Damage Converted to Fire Damage; Deal no Non-Fire Damage"
        | "keystone_avatar_of_fire" ->
            Some(Mods((DamageType.all |> List.filter ((<>) Fire) |> List.map (fun t -> Mod.base' (Convert(t, Fire)) v)) @ [ Mod.flag (Named "DealNoNonFire") ]))
        | "keystone_glancing_blows" -> Some(Mods [ Mod.flag (Named "UnluckyEvade"); Mod.flag (Named "LuckyDeflect") ])
        | "keystone_oasis" -> Some(Mods [ Mod.more (Named "LifeFlaskAmount") v; Mod.more (Named "ManaFlaskAmount") v ])
        | "keystone_eternal_youth" -> Some(Mods [ Mod.flag (Named "LifeRechargesInsteadOfES"); Mod.more (Named "LifeFlaskAmount") (-v) ])
        // "All Damage is taken from Mana before Life; 50% less Mana Recovery Rate"
        | "keystone_mana_shield" -> Some(Mods [ Mod.base' (Named "DamageRemovedFromManaFirst") 100.0; Mod.more (Named "ManaRecoveryRate") (-v) ])
        | "keystone_blood_magic" -> one (Mod.flag (Named "BloodMagic"))
        // "Convert 100% of maximum Energy Shield to maximum Mana; Mana Costs are Doubled"
        | "keystone_eldritch_battery" -> Some(Mods [ Mod.base' (Named "ESConvertedToMana") v; Mod.more ManaCost 100.0 ])
        | "keystone_chaos_inoculation" -> one (Mod.flag (Named "Keystone:Chaos Inoculation"))
        | "cannot_be_light_stunned" -> one (Mod.base' (Named "AvoidStun") 100.0)
        | "accuracy_rating_is_doubled" -> one (Mod.more (Named "Accuracy") 100.0)
        // "25% more Skill Speed while Off Hand is empty and you have a One-Handed Martial Weapon"
        | "keystone_dance_with_death" -> one (Mod.more Speed v |> Mod.whenever (WhileBuff "OneHandedEmptyOffHand"))
        | "keystone_fire_spells_become_chaos_spells" ->
            one (Mod.base' (Convert(Fire, Chaos)) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag "Fire")))
        // aggravated bleeding deals damage as though the target were moving
        | "aggravate_inflicted_bleeding" ->
            one (Mod.more (AilmentMagnitude(Some Bleed)) ((Constants.bleedMovingMultiplier - 1.0) * 100.0) |> Mod.whenever (Not(EnemyHas "Moving")))
        | "number_of_additional_curses_allowed" -> one (Mod.base' (Named "MaxCurses") v)
        | "keystone_elemental_equilibrium" -> one (Mod.flag (Named "InfusionRemnantsRotate"))
        // "Storm and Plant Spells deal 50% more damage"
        | "marauder_hidden_ascendancy_damage_+%_final" ->
            Some(Mods([ "Type:Storm"; "Type:Plant" ] |> List.map (fun t -> Mod.more (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag t)))))
        | "base_life_leech_does_not_stop_at_full_life" | "can_wield_2h_axe_sword_mace_in_one_hand" -> Some Structural
        | "keystone_conduit" | "minions_get_amulet_stats_instead_of_you" | "can_have_2_companions" | "keystone_ancestral_bond" ->
            Some(Unsimulated "minions / allies")
        | "keystone_bulwark" -> Some(Unsimulated "movement")
        | "critical_strike_chance_+%_if_triggered_skill_recently" -> one (Mod.inc CritChance v |> Mod.whenever (WhileBuff "TriggeredRecently"))
        | "critical_strike_multiplier_+_if_have_not_dealt_critical_strike_recently" -> one (Mod.inc CritBonus v |> Mod.whenever (Not(WhileBuff "CritRecently")))
        | "ailment_chance_+%_vs_exposed" ->
            Some(Mods(allAilments |> List.map (fun a -> Mod.inc (AilmentChance a) v |> Mod.whenever (EnemyHas "Exposed"))))
        | id when id.StartsWith "typed_skill_level:" ->
            one (Mod.base' (Named "GemLevel") v |> Mod.whenever (SkillIs(Tag("Type:" + id.Substring 18))))
        | id when id.StartsWith "specific_skill_level:" ->
            one (Mod.base' (Named "GemLevel") v |> Mod.whenever (SkillIs(Tag("Gem:" + id.Substring 21))))
        | "triggered_skill_damage_+%" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Triggered))
        | "cast_on_crit_global_critical_hit_chance_granted_+%" -> one (Mod.inc CritChance v)
        | "support_spell_cascade_number_of_cascades_per_side" -> one (Mod.base' (Named "CascadesPerSide") v)
        | "support_spell_cascade_area_of_effect_+%_for_cascade" -> one (Mod.more (Named "CascadeArea") v)
        | "trigger_skills_refund_half_energy_spent_chance_%" -> one (Mod.base' (Named "EnergyRefundChance") v)
        | id when id.Contains "_centienergy_" || id.StartsWith "generic_ongoing_trigger_" || id = "total_cast_time_+_ms" || id.EndsWith "_fall_time_ms" ->
            Some Structural
        | "max_power_charges" -> one (Mod.base' (Named "MaxPowerCharge") v)
        | "max_frenzy_charges" -> one (Mod.base' (Named "MaxFrenzyCharge") v)
        | "max_endurance_charges" -> one (Mod.base' (Named "MaxEnduranceCharge") v)
        | "maximum_rage" -> one (Mod.base' (Named "MaxRage") v)
        // Berserk's alternate quality: maximum Rage while Berserk is active (character-wide)
        | "berserk_maximum_rage_granted_+" -> one (Mod.base' (Named "MaxRage") v)
        | "charge_skip_consume_chance_%" -> one (Mod.base' (Named "ChargeSkipConsume") v)
        | "skill_base_rage_effect_+%_to_apply" | "rage_effect_+%" -> one (Mod.inc (Named "RageEffect") v)
        | "chance_%_to_double_effect_of_removing_charges" -> Some(Unsimulated "double effect of removing charges")
        | "treat_enemy_resistances_as_negated_on_elemental_damage_hit_%_chance" ->
            one (Mod.base' (Named "NegateEnemyResistChance") v)
        | "projectiles_nova" -> Some Structural
        | "projectile_chance_to_chain_1_extra_time_from_terrain_%" -> one (Mod.base' (Named "TerrainChain") v)
        | "ascendancy_gemling_enable_thaumaturgy_quality_stats" -> one (Mod.flag (Named "AltQuality"))
        | "all_skill_gem_quality_+" -> one (Mod.base' (Named "GemQuality") v)
        | "damage_+%_if_have_crit_in_past_8_seconds" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (WhileBuff "CritInPast8Seconds"))
        | _ -> None

    // ---- patterns over the words of the id (reversed, so suffixes come first) ----

    /// "<words with 'damage'> +% [final] [condition words]": increased, or more if "final".
    let private scaledDamage rev v =
        let words = List.rev rev
        match List.tryFindIndex ((=) "+%") words with
        | Some i when i > 0 ->
            let after = List.skip (i + 1) words
            let kind, after =
                match after with
                | "final" :: rest -> More, rest
                | rest -> Inc, rest
            damageMod kind (List.take i words @ after) v
        | _ -> None

    /// "<scope> minimum|maximum added <type> damage"
    let private addedDamage rev v =
        match rev with
        | "damage" :: t :: "added" :: bound :: scope when (typeWord t).IsSome && (bound = "minimum" || bound = "maximum") ->
            let t = (typeWord t).Value
            let stat = if bound = "minimum" then AddedMin t else AddedMax t
            scopes (List.rev scope) |> Option.map (fun sc -> [ apply sc (Mod.base' stat v) ])
        | _ -> None

    /// "base reduce enemy <type> resistance %": penetration
    let private penetration rev v =
        match rev with
        | "%" :: "resistance" :: t :: "enemy" :: "reduce" :: scope when (typeWord t).IsSome ->
            scopes (List.rev scope) |> Option.map (fun sc -> [ apply sc (Mod.base' (Penetration (typeWord t).Value) v) ])
        | _ -> None

    /// "non skill base all damage % to gain as <type>"
    let rec private gainAs rev v =
        match rev with
        | ("attacks" | "spells" as w) :: "with" :: rest ->
            gainAs rest v |> Option.map (List.map (Mod.whenever (SkillIs(if w = "attacks" then Attack else Spell))))
        | t :: "as" :: "gain" :: "to" :: "%" :: "damage" :: src :: scope when (typeWord t).IsSome ->
            let into = (typeWord t).Value
            let sources =
                match src with
                | "all" -> Some DamageType.all
                | "elemental" -> Some DamageType.elemental
                | s -> typeWord s |> Option.map List.singleton
            match sources, scopes (List.rev scope) with
            | Some srcs, Some sc -> Some(srcs |> List.filter ((<>) into) |> List.map (fun s -> apply sc (Mod.base' (GainAs(s, into)) v)))
            | _ -> None
        | _ -> None

    /// "<type> damage % to convert to <type>"
    let private convert rev v =
        match rev with
        | into :: "to" :: "convert" :: "to" :: "%" :: "damage" :: from :: scope when (typeWord into).IsSome && (typeWord from).IsSome ->
            scopes (List.rev scope)
            |> Option.map (fun sc -> [ apply sc (Mod.base' (Convert((typeWord from).Value, (typeWord into).Value)) v) ])
        | _ -> None

    /// "<ailment> chance|effect|duration +%"
    let private ailmentStat rev v =
        match rev with
        | "+%" :: what :: a :: scope when (ailmentWord a).IsSome ->
            let a = (ailmentWord a).Value
            let stat =
                match what with
                | "chance" -> Some(AilmentChance a)
                | "effect" | "magnitude" -> Some(AilmentMagnitude(Some a))
                | "duration" -> Some(AilmentDuration(Some a))
                | _ -> None
            match stat, scopes (List.rev scope) with
            | Some s, Some sc -> Some [ apply sc (Mod.inc s v) ]
            | _ -> None
        | _ -> None

    /// "crit" is "critical strike" in newer ids.
    let private normaliseCrit (words: string list) =
        let rec go ws =
            match ws with
            | "critical" :: "hit" :: "damage" :: "bonus" :: rest -> "critical" :: "strike" :: "multiplier" :: go rest
            | "crit" :: rest -> "critical" :: "strike" :: go rest
            | w :: rest -> w :: go rest
            | [] -> []
        go words

    /// "<scope> critical strike chance +% [final] [conditions]", "<scope> critical strike multiplier +".
    let private crit rev v =
        let words = normaliseCrit (List.rev rev)
        match List.tryFindIndex (fun w -> w = "+%" || w = "+") words with
        | None -> None
        | Some i ->
            let marker = words.[i]
            let isFinal, tail =
                match List.skip (i + 1) words with
                | "final" :: t -> true, t
                | t -> false, t
            let make stat kind scope =
                scopes (List.rev scope @ tail) |> Option.map (fun sc -> [ apply sc (Mod.make kind stat v) ])
            match List.rev (List.take i words) with
            | "chance" :: "strike" :: "critical" :: scope ->
                make CritChance (if isFinal then More elif marker = "+%" then Inc else Base) scope
            | "multiplier" :: "strike" :: "critical" :: scope ->
                make CritBonus (if marker = "+" then Base elif isFinal then More else Inc) scope
            | _ -> None

    /// "<scope> attack speed +% [final] [conditions]", "cast speed", "attack and cast speed".
    let private speed rev v =
        let words = List.rev rev
        match List.tryFindIndex ((=) "+%") words with
        | None -> None
        | Some i ->
            let isFinal, tail =
                match List.skip (i + 1) words with
                | "final" :: t -> true, t
                | t -> false, t
            let kind = if isFinal then More else Inc
            let make extra scope =
                scopes (List.rev scope @ tail)
                |> Option.map (fun sc -> [ apply sc (Mod.make kind Speed v |> fun m -> List.foldBack Mod.whenever extra m) ])
            match List.rev (List.take i words) with
            | "speed" :: "cast" :: "and" :: "attack" :: rest -> make [] rest
            | "speed" :: "attack" :: rest -> make [ SkillIs Attack ] rest
            | "speed" :: "cast" :: rest -> make [ SkillIs Spell ] rest
            | "speed" :: "skill" :: rest -> make [] rest
            | _ -> None

    /// "<scope> skill gem level +": levels of matching skill gems.
    let private gemLevel rev v =
        match rev with
        | "+" :: "level" :: "gem" :: "skill" :: rest ->
            let words = List.rev rest
            let elements = words |> List.choose typeWord |> List.map (fun t -> SkillIs(Tag(DamageType.name t)))
            let others = words |> List.filter (fun w -> (typeWord w).IsNone)
            match others with
            | [] -> Some [ Mod.base' (Named "GemLevel") v |> fun m -> List.foldBack Mod.whenever elements m ]
            | _ ->
                scopes others
                |> Option.map (fun sc -> [ apply sc (Mod.base' (Named "GemLevel") v) |> fun m -> List.foldBack Mod.whenever elements m ])
        | _ -> None

    /// "<scope> hit damage <ailment> multiplier +%": buildup of freeze / stun / electrocute / pin.
    let private buildup rev v =
        match rev with
        | "+%" :: "multiplier" :: a :: "damage" :: "hit" :: scope ->
            let ailment =
                match a with
                | "freeze" -> Some Freeze
                | "stun" -> Some HeavyStun
                | "electrocute" -> Some Electrocute
                | "pin" -> Some Pin
                | _ -> None
            match ailment, scopes (List.rev scope) with
            | Some a, Some sc -> Some [ apply sc (Mod.inc (Buildup a) v) ]
            | _ -> None
        | _ -> None

    /// "<scope> area of effect +% [final] [conditions]"
    let private areaOfEffect rev v =
        let words = List.rev rev
        match List.tryFindIndex ((=) "+%") words with
        | Some i when i >= 3 && words.[i - 3] = "area" && words.[i - 2] = "of" && words.[i - 1] = "effect" ->
            let isFinal, tail =
                match List.skip (i + 1) words with
                | "final" :: t -> true, t
                | t -> false, t
            scopes (List.take (i - 3) words @ tail)
            |> Option.map (fun sc -> [ apply sc (Mod.make (if isFinal then More else Inc) AreaOfEffect v) ])
        | _ -> None

    /// "<defences> +% [final] [conditions]": armour / evasion / energy shield combinations.
    let private defenceScaled rev v =
        let words = List.rev rev
        match List.tryFindIndex ((=) "+%") words with
        | Some i when i > 0 ->
            let isFinal, tail =
                match List.skip (i + 1) words with
                | "final" :: t -> true, t
                | t -> false, t
            let head = List.take i words |> List.filter (fun w -> w <> "and" && w <> "global" && w <> "base" && w <> "rating")
            let stats =
                head |> List.fold (fun acc w ->
                    match acc, w with
                    | Some xs, "armour" -> Some(Armour :: xs)
                    | Some xs, "evasion" -> Some(Evasion :: xs)
                    | Some xs, ("energy" | "shield") -> Some(if List.contains MaxEnergyShield xs then xs else MaxEnergyShield :: xs)
                    | _ -> None) (Some [])
            match stats, scopes tail with
            | Some (_ :: _ as sts), Some sc -> Some(sts |> List.rev |> List.map (fun st -> apply sc (Mod.make (if isFinal then More else Inc) st v)))
            | _ -> None
        | _ -> None

    /// "gem barrier <colour> grants <stat>": the stat per mote of that colour (Virtuous Barrier).
    let rec private perMote rev v =
        match List.rev rev with
        | "gem" :: "barrier" :: colour :: "grants" :: rest when not rest.IsEmpty ->
            let inner = List.rev rest
            let direct =
                match defensive (String.concat "_" rest) v with
                | Some (Mods ms) -> Some ms
                | _ -> None
            direct
            |> Option.orElse ([ defenceScaled; scaledDamage ] |> List.tryPick (fun r -> r inner v))
            |> Option.map (List.map (Mod.whenever (PerStack("Mote:" + colour, 1.0))))
        | _ -> None

    let private rules = [ perMote; defenceScaled; areaOfEffect; buildup; scaledDamage; addedDamage; penetration; gainAs; convert; ailmentStat; crit; speed; gemLevel ]

    let private pattern (id: string) (v: float) : Meaning option =
        let rev = List.rev (split id)
        rules |> List.tryPick (fun rule -> rule rev v) |> Option.map Mods

    /// Item-local stats: they change the weapon, not the character.
    let private local (id: string) v =
        match split id with
        | "local" :: rest ->
            let lv =
                match rest with
                | [ "physical"; "damage"; "+%" ] -> LocalDamageInc
                | [ bound; "added"; t; "damage" ] when (typeWord t).IsSome && bound = "minimum" -> LocalAddedMin (typeWord t).Value
                | [ bound; "added"; t; "damage" ] when (typeWord t).IsSome && bound = "maximum" -> LocalAddedMax (typeWord t).Value
                | [ "attack"; "speed"; "+%" ] -> LocalAttackSpeedInc
                | [ "critical"; "strike"; "chance" ] -> LocalCritChance
                | [ "critical"; "strike"; "chance"; "+%" ] -> LocalCritChanceInc
                | _ -> LocalOther id
            Some(Local(lv, v))
        | _ -> None

    /// Stats with no effect on a fight as we simulate it: loot, light, experience, what can be
    /// equipped or reserved. (The variant is still called NotOffensive.)
    let private noCombatWords =
        set [ "rarity"; "quantity"; "light"; "gold"; "experience"; "fish"; "fishing"; "spirit"; "reservation"
              "requirements"; "requirement"; "footprints"; "currency"; "slots" ]

    /// Offensive mechanics the engine doesn't model yet, by keyword.
    let private unsimulated =
        [ "area", "area of effect"
          "projectile", "projectile count / speed / chaining"
          "pierce", "projectile count / speed / chaining"
          "chain", "projectile count / speed / chaining"
          "accuracy", "accuracy vs evasion"
          "curse", "curse effect"
          "aura", "aura effect"
          "mark", "mark effect"
          "exposure", "exposure effect"
          "rage", "rage"
          "charge", "charges"
          "charges", "charges"
          "totem", "totems"
          "warcry", "warcries"
          "thorns", "thorns"
          "glory", "glory"
          "infusion", "infusions"
          "companion", "companions"
          "minion", "minion stats beyond damage and speed"
          "slow", "slows"
          "debuff", "debuffs"
          "duration", "durations"
          "cooldown", "cooldowns"
          "empowered", "empowered attacks"
          "movement", "movement"
          "velocity", "movement"
          "sprint", "movement"
          "dodge", "movement"
          "flask", "flasks"
          "charm", "charms"
          "life", "life / recovery"
          "mana", "mana / recovery"
          "energy", "energy shield / recovery"
          "leech", "life / recovery"
          "armour", "defences"
          "evasion", "defences"
          "block", "defences"
          "resistance", "defences"
          "taken", "damage taken"
          "threshold", "stun / ailment threshold"
          "stun", "stun / ailment threshold"
          "self", "effects on self"
          "presence", "presence / allies"
          "allies", "presence / allies" ]

    let private isNonOffensive (id: string) =
        let words = split id
        // A penetration / enemy debuff line mentions resistance but is offensive.
        words |> List.exists noCombatWords.Contains

    let private direct id value =
        match explicit id value with
        | Some m -> Some m
        | None ->
            match defensive id value with
            | Some m -> Some m
            | None -> pattern id value

    /// Gem stats carry the gem's name: "support_controlled_destruction_spell_damage_+%_final".
    /// Try the id with 1..4 name words removed after the prefix.
    let private withoutPrefix (id: string) value =
        let strip prefixWords =
            let words = split id
            [ 1..4 ]
            |> List.tryPick (fun k ->
                if words.Length > prefixWords + k then
                    direct (String.concat "_" (List.skip (prefixWords + k) words)) value
                else
                    None)
        if id.StartsWith "support_" then strip 1
        elif id.StartsWith "active_skill_" then direct (id.Substring 13) value
        else None

    let private structural (id: string) =
        let words = split id
        let has w = List.contains w words
        has "index" || has "visual" || has "show" || has "display" || has "cursor" || has "pathing" || has "contact"
        || has "collide" || has "targets" || id.StartsWith "can_perform_skill" || id.StartsWith "base_is_"
        || id.StartsWith "is_" || id.EndsWith "_step_distance"

    /// What a stat means. Local stats first, then the explicit table, then patterns,
    /// then gem-name prefixes, then the non-offensive bucket. Anything else is Unmapped.
    let meaning (id: string) (value: float) : Meaning =
        match local id value with
        | Some m -> m
        | None ->
            // explicit and pattern meanings first; the structural heuristic only filters the rest
            match direct id value with
            | Some m -> m
            | None when structural id -> Structural
            | None ->
                match withoutPrefix id value with
                | Some m -> m
                | None ->
                    if isNonOffensive id then
                        NotOffensive
                    else
                        let words = split id
                        match unsimulated |> List.tryFind (fun (w, _) -> List.contains w words) with
                        | Some (_, mechanic) -> Unsimulated mechanic
                        | None -> Unmapped
