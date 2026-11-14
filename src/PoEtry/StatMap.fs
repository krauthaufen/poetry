namespace PoEtry

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
        | "bow" | "bows" -> Some(Cond(SkillIs Bow))
        | "crossbow" | "crossbows" -> Some(Cond(SkillIs Crossbow))
        | "slam" | "slams" -> Some(Cond(SkillIs Slam))
        | "strike" | "strikes" -> Some(Cond(SkillIs Strike))
        | "curse" -> Some(Cond(SkillIs Curse))
        | "mace" | "maces" -> Some(Cond(SkillIs Mace))
        | "spear" | "spears" -> Some(Cond(SkillIs Spear))
        | "quarterstaff" | "quarterstaves" -> Some(Cond(SkillIs Quarterstaff))
        | "sword" | "swords" -> Some(Cond(SkillIs Sword))
        | "axe" | "axes" -> Some(Cond(SkillIs Axe))
        | "dagger" | "daggers" -> Some(Cond(SkillIs Dagger))
        | "claw" | "claws" -> Some(Cond(SkillIs Claw))
        | "flail" | "flails" -> Some(Cond(SkillIs Flail))
        | "wand" | "wands" -> Some(Cond(SkillIs Wand))
        | "staff" | "staves" -> Some(Cond(SkillIs Staff))
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
        | "immobilised" -> Some "Immobilised"
        | "exposed" -> Some "Exposed"
        | "marked" -> Some "Marked"
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
            | ("vs" | "against") :: "rare" :: "or" :: "unique" :: ("enemy" | "enemies") :: tail -> go (EnemyHas "RareOrUnique" :: acc) tail
            | "if" :: "rare" :: "or" :: "unique" :: "enemy" :: "nearby" :: tail -> go (EnemyHas "RareOrUnique" :: acc) tail
            | "when" :: "hit" :: tail | "if" :: "have" :: "been" :: "hit" :: "recently" :: tail | "if" :: "been" :: "hit" :: "recently" :: tail ->
                go (WhileBuff "BeenHitRecently" :: acc) tail
            | "if" :: "have" :: "not" :: "been" :: "hit" :: "recently" :: tail | "if" :: "not" :: "hit" :: "recently" :: tail ->
                go (Not(WhileBuff "BeenHitRecently") :: acc) tail
            | "for" :: "4" :: "seconds" :: "on" :: "crit" :: tail -> go (WhileBuff "CritRecently" :: acc) tail
            | ("while" | "if") :: "surrounded" :: tail -> go (WhileBuff "Surrounded" :: acc) tail
            | "per" :: "enemy" :: "elemental" :: "ailment" :: tail -> go (PerEnemyStack("ElementalAilments", 1.0) :: acc) tail
            // "... per 15 strength": the character's attributes are stacks "Str" / "Dex" / "Int"
            | "per" :: n :: ("strength" | "dexterity" | "intelligence" as attr) :: tail when n <> "" && n |> Seq.forall System.Char.IsDigit ->
                go (PerStack((match attr with "strength" -> "Str" | "dexterity" -> "Dex" | _ -> "Int"), float n) :: acc) tail
            // "during any Flask effect", "while using a Flask", "during Mana Flask effect"
            | "during" :: "any" :: "flask" :: "effect" :: tail | "during" :: "flask" :: "effect" :: tail | "while" :: "using" :: "flask" :: tail
            | "during" :: "life" :: "or" :: "mana" :: "flask" :: "effect" :: tail -> go (WhileBuff "FlaskEffect" :: acc) tail
            | "during" :: "mana" :: "flask" :: "effect" :: tail | "during" :: "mana" :: "flask" :: tail -> go (WhileBuff "ManaFlaskEffect" :: acc) tail
            | "during" :: "life" :: "flask" :: "effect" :: tail | "while" :: "using" :: "life" :: "flask" :: tail -> go (WhileBuff "LifeFlaskEffect" :: acc) tail
            // "per 20 Spirit": the character's total Spirit as stacks
            | "per" :: n :: "spirit" :: tail when n <> "" && n |> Seq.forall System.Char.IsDigit -> go (PerStack("Spirit", float n) :: acc) tail
            // "... if 10 red supports socketed", "... if majority blue supports socketed"
            | "if" :: "10" :: colour :: "supports" :: "socketed" :: tail -> go (WhileBuff("Supports10:" + colour) :: acc) tail
            | "if" :: "majority" :: colour :: "supports" :: "socketed" :: tail -> go (WhileBuff("SupportsMajority:" + colour) :: acc) tail
            | "if" :: "have" :: "consumed" :: "infusion" :: "recently" :: tail -> go (WhileBuff "ConsumedInfusionRecently" :: acc) tail
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
            // Grand Spectrum: "per socketed Grand Spectrum"
            | "per" :: "stackable" :: "unique" :: "jewel" :: tail -> go (PerStack("GrandSpectrum", 1.0) :: acc) tail
            | "with" :: c :: "charges" :: tail when c = "power" || c = "frenzy" || c = "endurance" ->
                go (WhileBuff(titleCase [ c; "charge" ]) :: acc) tail
            | "when" :: "charged" :: tail -> go (WhileBuff "PowerCharge" :: acc) tail
            | "in" :: "weapon" :: "set" :: "one" :: tail -> go (Not(SkillIs(Tag "WeaponSet2")) :: acc) tail
            | "in" :: "weapon" :: "set" :: "two" :: tail -> go (SkillIs(Tag "WeaponSet2") :: acc) tail
            | "while" :: "you" :: "have" :: "energy" :: "shield" :: tail -> go (WhileBuff "HasEnergyShield" :: acc) tail
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
        | "chaos_damage_resistance_is_doubled" -> one (Mod.flag (Named "ChaosResistDoubled"))
        | "override_maximum_damage_resistance_%" -> one (Mod.override' (Named "MaxResistOverride") v)
        | "gain_%_life_from_body_es" -> one (Mod.base' (Named "LifeFromBodyES") v)
        | "maximum_life_+%_if_10_red_supports_socketed" -> one (Mod.inc MaxLife v |> Mod.whenever (WhileBuff "Supports10:red"))
        | "maximum_mana_+%_if_10_blue_supports_socketed" -> one (Mod.inc MaxMana v |> Mod.whenever (WhileBuff "Supports10:blue"))
        | "body_armour_+%" -> one (Mod.inc (Named "BodyArmourArmour") v)
        | "essence_grants_armour_evasion_energy_shield_+%" -> Some(Mods [ Mod.inc Armour v; Mod.inc Evasion v; Mod.inc MaxEnergyShield v ])
        | "witchhunter_armour_evasion_+%_final" -> Some(Mods [ Mod.more Armour v; Mod.more Evasion v ])
        | "base_deflection_rating_%_of_armour" -> one (Mod.base' (Named "DeflectionPercentOfArmour") v)
        | "evasion_rating_%_to_gain_as_ailment_threshold" -> one (Mod.base' (Named "AilmentThresholdFromEvasion") v)
        | "evasion_+%_per_10_intelligence" -> one (Mod.inc Evasion v |> Mod.whenever (PerStack("Int", 10.0)))
        | "stun_threshold_+_per_dexterity" -> one (Mod.base' (Named "StunThreshold") v |> Mod.whenever (PerStack("Dex", 1.0)))
        | "evasion_rating_+_per_1_helmet_energy_shield" -> one (Mod.base' (Named "EvasionPerHelmetES") v)
        | "maximum_life_+%_per_equipped_corrupted_item" -> one (Mod.inc MaxLife v |> Mod.whenever (PerStack("CorruptedItems", 1.0)))
        | "mana_%_to_gain_as_armour" -> one (Mod.base' (Named "ManaGainedAsArmour") v)
        | "self_take_no_extra_damage_from_critical_strikes" -> one (Mod.base' (Named "LessCritDamageTaken") 100.0)
        | "no_inherent_mana_regeneration" -> one (Mod.flag (Named "NoInherentManaRegen"))
        | "regenerate_mana_equal_to_x%_of_life_per_minute" -> one (Mod.base' (Named "ManaRegenPercentOfLife") (v / 60.0))
        | "energy_shield_delay_-%_while_affected_by_archon" -> one (Mod.inc (Named "ESRechargeFaster") v |> Mod.whenever (WhileBuff "Elemental Archon"))
        | "maximum_life_mana_and_energy_shield_+%" -> Some(Mods [ Mod.inc MaxLife v; Mod.inc MaxMana v; Mod.inc MaxEnergyShield v ])
        // Grand Spectrum: "per socketed Grand Spectrum"
        // Lord of the Wilds: "50% less Spirit", "Non-Minion Skills have 50% less Reservation Efficiency"
        | "keystone_lord_of_the_wilds" ->
            Some(Mods [ Mod.more (Named "Spirit") -v; Mod.more (Named "ReservationEfficiency") -v |> Mod.whenever (Not(SkillIs(Tag "Type:Minion"))) ])
        // read by Behaviours.spiritTotal from the sheet's own numbers
        | "spirit_+_if_at_least_200_dexterity" -> one (Mod.base' (Named "SpiritIfAtLeast|Dex|200") v)
        | "spirit_+_if_at_least_200_intelligence" -> one (Mod.base' (Named "SpiritIfAtLeast|Int|200") v)
        | "spirit_+_if_at_least_200_strength" -> one (Mod.base' (Named "SpiritIfAtLeast|Str|200") v)
        // Beidat's Will: "+1 to Maximum Spirit per 25 Maximum Life" (the value is the 25)
        | "ascendancy_beidats_will_spirit_+_per_X_maximum_life" -> one (Mod.base' (Named "SpiritPerLife") v)
        | "spirit_+_per_empty_charm_slot" -> one (Mod.base' (Named "SpiritPerEmptyCharmSlot") v)
        // "+200 to Armour for each Connected Notable Passive Skill Allocated": Pob.character counts them
        | "connected_notables_grant_armour_display" -> one (Mod.base' (Named "ArmourPerConnectedNotable") v)
        // only for what the shield / body armour itself shows (Defence.slotInc, slotMore)
        | "shield_armour_evasion_energy_shield_+%" -> Some(Mods [ for d in [ "Armour"; "Evasion"; "ES" ] -> Mod.inc (Named(sprintf "SlotInc:%s:Weapon 2" d)) v ])
        | "ascendancy_titan_damage_reduction_rating_from_body_armour_+%_final" -> one (Mod.more (Named "SlotMore:Armour:Body Armour") v)
        // Flames of Chayula (Behaviours.periodics)
        | "breach_flame_effects_doubled" -> one (Mod.flag (Named "BreachFlameDoubled"))
        | "can_only_spawn_purple_flame_of_chayula_remnants" -> one (Mod.flag (Named "BreachFlameOnly:purple"))
        | "can_only_spawn_red_flame_of_chayula_remnants" -> one (Mod.flag (Named "BreachFlameOnly:red"))
        | "can_only_spawn_blue_flame_of_chayula_remnants" -> one (Mod.flag (Named "BreachFlameOnly:blue"))
        | "your_life_cannot_change_while_you_have_energy_shield" -> one (Mod.flag (Named "LifeFrozenWithES"))
        // "You require N fewer enemies to be Surrounded" (the value is negative)
        | "required_enemies_to_be_considered_surrounded_offset" -> one (Mod.base' (Named "SurroundedOffset") v)
        | "remnant_recover_%_mana_on_pickup" -> one (Mod.base' (Named "RemnantRecoverManaPercent") v)
        // Mana Remnants' own numbers: read by Behaviours.manaRemnants
        | "mana_remnants_mana_gain_per_globe" | "mana_remnants_spawn_remnant_on_crit_vs_elemental_ailment_enemy_every_X_ms"
        | "mana_remnants_chance_to_spawn_orb_on_killing_enemy_with_elemental_ailment_%" -> Some Structural
        // supports: Deadly Poison, Unseen, Ignition
        | "support_deadly_poison_poison_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Poison)) v)
        | "support_unseen_critical_damage_multiplier_+%_final_vs_blinded_enemies" -> one (Mod.more CritBonus v |> Mod.whenever (EnemyHas "Blinded"))
        | "support_ignition_chance_to_ignite_+%_final" -> one (Mod.more (AilmentChance Ignite) v)
        // Jagged Ground (Behaviours.jaggedGround): what enemies in it take, how long it lies and
        // how much it slows; what makes one is read there by its id
        | "jagged_ground_enemy_damage_taken_+%" -> one (Mod.base' (Named "JaggedGroundDamageTaken") v)
        | "jagged_ground_duration_+%" -> one (Mod.inc (Named "JaggedGroundDuration") v)
        | "jagged_ground_effect_+%" -> one (Mod.inc (Named "JaggedGroundEffect") v)
        | "support_jagged_ground_base_duration_ms" | "skill_jagged_ground_base_duration_ms" | "jagged_ground_duration_ms" | "can_create_jagged_ground"
        | "cannot_create_jagged_ground" | "aftershocks_create_jagged_ground" | "support_create_jagged_ground_if_consumed_endurance_charge"
        | "support_consume_X_endurance_charges_on_use" -> Some Structural
        // "Enemies you Curse are Hindered, with 15% reduced Movement Speed" (Behaviours.curseOf)
        | "enemies_you_curse_have_15%_hinder" -> one (Mod.flag (Named "CursesHinder"))
        // Ice Bite's buff on freezing an enemy: read by Behaviours.iceBite
        | "support_ice_bite_buff_grant_%_added_cold_attack_damage" | "support_ice_bite_base_buff_duration" -> Some Structural
        // "Casting Ice Nova targeting near a Frostbolt Projectile will cause it to originate from
        // the Frostbolt": read by Setup.player (onBolts)
        | "ice_nova_number_of_frost_bolts_to_cast_on" -> Some Structural
        // sealed skills (Salvo, Unleash, Zarokh's Revolt, Freezing Salvo): read by the engine at use
        | "base_maximum_seals_for_skill" -> one (Mod.base' (Named "MaxSeals") v)
        | "base_skill_seal_gain_interval_ms" -> one (Mod.base' (Named "SealIntervalMs") v)
        | "support_%_of_total_cast_time_as_base_skill_seal_gain_interval" -> one (Mod.base' (Named "SealIntervalPercentOfUse") v)
        | "skill_additional_projectiles_per_seal_broken" -> one (Mod.base' (Named "SealProjectiles") v)
        | "skill_rapid_fire_repeats_per_broken_seal" -> one (Mod.base' (Named "SealRepeats") v)
        | "support_spell_rapid_fire_repeat_use_damage_+%_final" -> one (Mod.base' (Named "SealRepeatDamage") v)
        | "base_skill_is_sealed_skill" -> Some Structural
        // "N% increased Damage per 1% Chance to Block": the sheet's block chance as stacks
        | "damage_+%_per_1%_block_chance" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (PerStack("BlockChance", 1.0)))
        // Magma Barrier: "increased Block chance" while it is active
        | "skill_igneous_shield_grants_block_chance_+%" -> one (Mod.inc (Named "Block") v)
        | "hits_ignore_elemental_resistances_vs_frozen_enemies" -> one (Mod.flag (Named "IgnoreElementalResist") |> Mod.whenever (EnemyHas "Frozen"))
        | "critical_strikes_ignore_positive_elemental_resistances" -> one (Mod.flag (Named "IgnoreElementalResist") |> Mod.whenever IsCrit)
        | "critical_strikes_ignore_lightning_resistance" -> one (Mod.flag (Named "IgnoreResist:Lightning") |> Mod.whenever IsCrit)
        // Amazon: "Reveal Weaknesses against Rare and Unique enemies", "more damage / Skill Speed against
        // enemies with an Open Weakness" (the engine opens the weakness for a share of the hits)
        | "unique_reveal_weakness" -> one (Mod.flag (Named "RevealWeakness"))
        | "damage_+%_final_against_bloodlusting_enemies" -> Some(Mods [ Mod.more (Damage AllDamage) v |> Mod.whenever (EnemyHas "OpenWeakness"); Mod.base' (Named "OpenWeaknessMore") v ])
        | "skill_speed_+%_against_bloodlusting_enemies" -> one (Mod.inc Speed v |> Mod.whenever (WhileBuff "OpenWeaknessInPresence"))
        | "damage_+%_per_5_of_your_lowest_attribute" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (PerStack("LowestAttribute", 5.0)))
        // Glory and Combo: what a skill needs before it can be used, and what builds it (the engine
        // counts per skill; "mp_%" is a share of the monster's power per event)
        | "active_skill_requires_X_glory" -> one (Mod.base' (Named "Requires:Glory") v)
        | "support_glory_required_+%_final" -> one (Mod.base' (Named "GloryRequiredMore") v)
        | "active_skill_generates_mp_%_glory_per_attack_hit" -> one (Mod.base' (Named "Builds:Glory:AttackHit") v)
        | "active_skill_generates_mp_%_glory_per_elemental_hit" -> one (Mod.base' (Named "Builds:Glory:ElementalHit") v)
        | "active_skill_generates_mp_%_glory_per_chaos_hit" -> one (Mod.base' (Named "Builds:Glory:ChaosHit") v)
        | "active_skill_generates_mp_%_glory_per_heavy_stun" -> one (Mod.base' (Named "Builds:Glory:HeavilyStunned") v)
        | "active_skill_generates_mp_%_glory_per_freeze" -> one (Mod.base' (Named "Builds:Glory:Frozen") v)
        | "active_skill_generates_mp_%_glory_per_ignite" -> one (Mod.base' (Named "Builds:Glory:Ignited") v)
        | "active_skill_hundred_times_generates_glory_per_ignite" -> one (Mod.base' (Named "Builds:GloryFlat:Ignited") (v / 100.0))
        | "active_skill_hundred_times_generates_glory_per_shock" -> one (Mod.base' (Named "Builds:GloryFlat:Shocked") (v / 100.0))
        | "active_skill_required_number_of_combo_stacks" | "support_combo_finisher_required_number_of_combo_stacks" -> one (Mod.base' (Named "Requires:Combo") v)
        | "support_combo_finisher_damage_+%_final" -> one (Mod.more (Damage AllDamage) v)
        | "additional_combo_gain_chance_%" -> one (Mod.base' (Named "ComboExtraChance") v)
        | "living_bomb_damage_threshold_%_of_ailment_threshold" -> one (Mod.base' (Named "DetonatedByDamage") v)
        // Bloodstone Lances: "Spear Skills inflict a Bloodstone Lance on Hit, up to a maximum of 30"
        | "spear_skills_inflict_bloodstone_lance_on_hit" -> one (Mod.base' (Named "Rule:OnHit:BloodstoneLance:1000000") 100.0 |> Mod.whenever (SkillIs Spear))
        | "damage_per_bloodstone_lance_+%_final" -> one (Mod.more (Damage AllDamage) v |> Mod.whenever (PerEnemyStack("BloodstoneLance", 1.0)))
        | "cannot_inflict_bloodstone_lance" -> one (Mod.flag (Named "CannotInflict:BloodstoneLance"))
        // "Create a Fragment of Divinity in your Presence every 4 seconds"
        | "manifest_a_fragment_of_divinity_in_your_presence_every_4_seconds" -> one (Mod.base' (Named "DivineFragmentEvery") 4.0)
        // Ice Crystals: what one withstands, how many a use makes, how many there can be
        | "frost_wall_maximum_life" -> one (Mod.base' (Named "CrystalLife") v)
        | "wall_maximum_length" -> one (Mod.base' (Named "WallLength") v)
        | "grim_pillars_number_of_pillars_to_create" -> one (Mod.base' (Named "CrystalsCreated") v)
        | "base_number_of_grim_pillars_allowed" -> one (Mod.base' (Named "CrystalLimit") v)
        | "empower_barrage_base_number_of_barrage_repeats" -> one (Mod.base' (Named "BarrageRepeats") v)
        | "empower_barrage_number_of_barrage_repeats_per_frenzy_charge" -> one (Mod.base' (Named "BarrageRepeatsPerFrenzy") v)
        | "empower_barrage_damage_-%_final_with_repeated_projectiles" -> one (Mod.base' (Named "BarrageRepeatLess") v)
        | "empower_barrage_cooldown_%_of_attack_time" -> one (Mod.base' (Named "BarrageCooldownPct") v)
        // Charge Regulation: "more Skill Speed while you have a Frenzy Charge", "more Critical Hit
        // Chance while you have a Power Charge", "more Defences while you have an Endurance Charge"
        | "charge_mastery_skill_speed_+%_with_frenzy_charges" | "charge_mastery_skill_speed_+%_final_with_frenzy_charges" ->
            one (Mod.more Speed v |> Mod.whenever (WhileBuff "FrenzyCharge"))
        | "charge_mastery_crit_chance_+%_final_with_power_charges" -> one (Mod.more CritChance v |> Mod.whenever (WhileBuff "PowerCharge"))
        | "charge_mastery_armour_evasion_energy_shield_+%_final_with_endurance_charges" | "charge_mastery_defences_+%_final_with_endurance_charges" ->
            Some(Mods [ for st in [ Armour; Evasion; MaxEnergyShield ] -> Mod.more st v |> Mod.whenever (WhileBuff "EnduranceCharge") ])
        | "curse_pain_synchronisation_base_recorded_damage_%_to_apply" -> one (Mod.base' (Named "RecordsDamage") v)
        | "banner_aura_effect_+%" -> one (Mod.inc (Named "BannerEffect") v)
        | "attacks_roll_crits_twice" -> one (Mod.flag (Named "BifurcateCrits") |> Mod.whenever (SkillIs Attack))
        | "oracle_hits_eventually_crit_with_critical_strike_multiplier_+%_final_per_failed_roll" -> one (Mod.flag (Named "InevitableCrits"))
        // Direstrike: "increased Attack Damage while on Low Life while a Supported Skill is active"
        | "support_attack_damage_spirit_cost_attack_damage_+%_on_low_life" ->
            one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "LowLife"))
        | "life_reserved_by_stat_only_for_midnight_bargain_and_infernalist_%" -> one (Mod.base' (Named "LifeReservedPercent") v)
        | "armour_+_per_1_spirit" -> one (Mod.base' Armour v |> Mod.whenever (PerStack("Spirit", 1.0)))
        | "double_evasion_rating_from_gloves_helmets_boots" -> one (Mod.flag (Named "DoubleEvasionHelmetGlovesBoots"))
        | "halve_evasion_rating_from_body" -> one (Mod.flag (Named "HalveEvasionBodyArmour"))
        | "resist_all_elements_%_per_power_charge" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Resist t) v |> Mod.whenever (PerStack("PowerCharge", 1.0)))))
        | "elemental_resistance_%_per_stackable_unique_jewel" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Resist t) v |> Mod.whenever (PerStack("GrandSpectrum", 1.0)))))
        | "spirit_+%_per_stackable_unique_jewel" -> one (Mod.inc (Named "Spirit") v |> Mod.whenever (PerStack("GrandSpectrum", 1.0)))
        | "maximum_life_+%_per_stackable_unique_jewel" -> one (Mod.inc MaxLife v |> Mod.whenever (PerStack("GrandSpectrum", 1.0)))
        | "double_evasion_rating_if_you_havent_been_hit_recently" -> one (Mod.more Evasion 100.0 |> Mod.whenever (Not(WhileBuff "BeenHitRecently")))
        | "evasion_rating_%_to_gain_as_armour" -> one (Mod.base' (Named "EvasionGainedAsArmour") v)
        | "armour_+_per_1_boots_energy_shield" -> one (Mod.base' (Named "ArmourPerBootsES") v)
        | "reload_speed_+%" -> one (Mod.inc (Named "ReloadSpeed") v)
        | "%_chance_for_crossbow_reload_to_be_instant" -> one (Mod.base' (Named "InstantReloadChance") v)
        | "crossbow_attack_%_chance_to_not_consume_ammo_if_reloaded_recently" -> one (Mod.base' (Named "KeepAmmoChance") v |> Mod.whenever (WhileBuff "ReloadedRecently"))
        | "dodge_roll_base_travel_distance" -> one (Mod.base' (Named "DodgeRollDistance") v)
        // the player's Presence (4 m; Setup.fight): its area, "Presence Radius is doubled", and
        // what enemies in it have (auras: Behaviours.presenceAuras)
        | "presence_area_+%" -> one (Mod.inc (Named "PresenceArea") v)
        | "unique_double_presence_radius" -> one (Mod.flag (Named "PresenceRadiusDoubled"))
        | "base_enemies_in_your_presence_are_hindered" -> one (Mod.flag (Named "PresenceAura:Hindered"))
        | "enemies_in_presence_are_blinded" -> one (Mod.flag (Named "PresenceAura:Blinded"))
        | "enemies_in_presence_have_exposure" -> one (Mod.flag (Named "PresenceAura:Exposure"))
        | "enemies_in_presence_elemental_damage_resisted_by_lowest_elemental_resistance" -> one (Mod.flag (Named "PresenceAura:LowestResist"))
        | "enemies_in_presence_have_fire_resistance_%" -> one (Mod.base' (Named "PresenceResist:Fire") v)
        | "charm_charges_gained_+%" -> one (Mod.inc (Named "CharmChargesGained") v)
        | "primalist_charm_charges_gained_+%_final" -> one (Mod.more (Named "CharmChargesGained") v)
        | "charm_charges_used_+%" -> one (Mod.inc (Named "CharmChargesUsed") v)
        // "X% chance for Charms you use to not consume Charges": rolled by the engine at each use
        | "charms_%_chance_to_not_consume_charges" -> one (Mod.base' (Named "CharmKeepCharges") v)
        // "Become Ignited when you deal a Critical Hit, taking X% of your maximum Life and Energy
        // Shield as Fire Damage per second" (Behaviours.selfIgnite)
        | "infernalist_burn_life_and_es_%_per_second_if_crit_recently" -> one (Mod.base' (Named "IgniteSelfOnCrit") v)
        // "Buffs on you expire X% faster" (Behaviours.buffsExpire), "Debuffs on you expire X%
        // faster" (the ailments a monster's hit leaves on the player: Defence)
        | "buff_time_passed_+%_only_buff_category" -> one (Mod.inc (Named "BuffExpiry") v)
        | "debuff_time_passed_+%" -> one (Mod.inc (Named "DebuffExpiry") v)
        | "charm_duration_+%" -> one (Mod.inc (Named "CharmDuration") v)
        | "active_skill_chill_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Chill)) v)
        | "active_skill_shock_chance_+%_final" -> one (Mod.more (AilmentChance Shock) v)
        | "active_skill_hit_damage_freeze_multiplier_+%_final" -> one (Mod.more (Buildup Freeze) v)
        // (-100: "Cannot cause Stun buildup")
        | "active_skill_hit_damage_stun_multiplier_+%_final" -> one (Mod.more (Buildup HeavyStun) v)
        // "Companions have X% chance to gain Onslaught on Kill" (4 s where no time is named, as the player's)
        | "companion_onslaught_on_kill_%" -> one (Mod.base' (ForMinions(Named "Rule:OnKill:Onslaught:4")) v |> Mod.whenever (SkillIs(Tag "Companion")))
        // how long the character takes to turn to where a skill is aimed: the player stands and
        // faces what it aims at
        | "active_skill_override_turn_duration_ms" -> Some Structural
        | "never_freeze" -> one (Mod.more (Buildup Freeze) -100.0)
        | "cannot_inflict_elemental_ailments" ->
            Some(Mods [ Mod.more (AilmentChance Ignite) -100.0; Mod.more (AilmentChance Shock) -100.0; Mod.more (AilmentMagnitude(Some Chill)) -100.0; Mod.more (Buildup Freeze) -100.0; Mod.more (Buildup Electrocute) -100.0 ])
        | "ailment_chance_+%_vs_rare_or_unique_enemy" ->
            Some(Mods [ for a in [ Ignite; Shock; Bleed; Poison ] -> Mod.inc (AilmentChance a) v |> Mod.whenever (EnemyHas "RareOrUnique") ])
        | "cast_speed_for_cold_skills_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag "Cold")))
        | "chance_for_extra_damage_roll_%" -> one (Mod.base' (Named "LuckyDamageChance") v)
        | "spell_damage_+%_per_100_maximum_mana" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (PerStack("MaxMana100", 1.0)))
        | "spell_damage_+%_per_100_max_mana_with_non_channelling_skills" ->
            one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxMana100", 1.0)))
        | "spell_critical_strike_chance_+%_per_100_max_mana_with_non_channelling_skills" ->
            one (Mod.inc CritChance v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxMana100", 1.0)))
        | "critical_strike_multiplier_+_if_gained_power_charge_recently" -> one (Mod.inc CritBonus v |> Mod.whenever (WhileBuff "GainedPowerChargeRecently"))
        | "chance_to_gain_1_more_power_charge_%" -> one (Mod.base' (Named "ExtraChargeChance:PowerCharge") v)
        | "chance_to_gain_1_more_frenzy_charge_%" -> one (Mod.base' (Named "ExtraChargeChance:FrenzyCharge") v)
        | "chance_to_gain_1_more_endurance_charge_%" -> one (Mod.base' (Named "ExtraChargeChance:EnduranceCharge") v)
        | "chance_to_gain_1_more_charge_%" -> one (Mod.base' (Named "ExtraChargeChance:Any") v)
        | "gain_a_power_charge_when_you_consume_an_elemental_infusion" -> one (Mod.base' (Named "PowerChargeOnInfusionConsumed") 1.0)
        | "critical_strike_multiplier_+_if_have_dealt_non_crit_recently" -> one (Mod.inc CritBonus v |> Mod.whenever (WhileBuff "NonCritRecently"))
        | "damage_+%_if_triggered_skill_recently" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (WhileBuff "TriggeredRecently"))
        | "gain_accuracy_rating_equal_to_intelligence" -> one (Mod.flag (Named "AccuracyFromIntelligence"))
        | "support_inspiration_cost_+%_final" -> one (Mod.more ManaCost v)
        | "minions_deal_%_of_physical_damage_as_additional_chaos_damage" -> one (Mod.base' (ForMinions(GainAs(Physical, Chaos))) v)
        // more attack damage by distance to the target (see Sim.hitEnemy)
        | "support_close_combat_attack_damage_+%_final_from_distance" -> one (Mod.base' (Named "CloseCombatMore") v |> Mod.whenever (SkillIs Attack))
        | "support_far_combat_attack_damage_+%_final_from_distance" -> one (Mod.base' (Named "FarCombatMore") v |> Mod.whenever (SkillIs Attack))
        // seconds added to the use time, not shortened by speed
        | "total_attack_time_+_ms" | "total_cast_time_+_ms" -> one (Mod.base' (Named "FixedUseTime") (v / 1000.0))
        // how a skill is aimed and animated, where remnants can be collected, things monsters never do to the player
        | "critical_chance_against_aura_apply_brittle_every_x_ms" | "skill_withering_presence_frequency_ms" | "herald_of_thunder_storm_max_hits" | "herald_of_ash_overkill_threshold_%" | "herald_of_ash_burning_%_overkill_damage_per_minute" | "action_requires_aiming_stance" | "action_required_target_facing_angle_tolerance_degrees" | "crossbow_barrage_recoil_per_shot"
        | "crossbow_barrage_total_recoil_buff_count" | "crossbow_barrage_debuff_duration_ms" | "crossbow_barrage_attack_time_ratio_%"
        | "base_minimum_channel_time_ms" | "base_melee_dash_range" | "main_hand_weapon_range_+"
        | "remnant_pickup_range_+%" | "immune_to_maim" | "immune_to_hinder" | "enemy_extra_damage_rolls_chance_%"
        | "action_can_be_used_in_upward_aiming_stance" | "projectiles_crossbow_barrage" -> Some Structural
        // armour break (see Sim.hitEnemy)
        | "armour_break_physical_damage_%_dealt_as_armour_break" -> one (Mod.base' (Named "ArmourBreakOfPhysical") v)
        | "armour_break_equal_to_%_physical_damage_dealt_spells" -> one (Mod.base' (Named "ArmourBreakOfPhysical") v |> Mod.whenever (SkillIs Spell))
        | "armour_break_fire_damage_%_dealt_as_armour_break" -> one (Mod.base' (Named "ArmourBreakOfFire") v)
        | "armour_break_damage_%_dealt_as_armour_break" -> one (Mod.base' (Named "ArmourBreakOfDamage") v)
        | "apply_X_armour_break_on_hit" -> one (Mod.base' (Named "ArmourBreakFlat") v)
        | "armour_break_amount_+%" -> one (Mod.inc (Named "ArmourBreakAmount") v)
        | "armour_break_amount_+%_vs_enemies_affected_by_ailments" -> one (Mod.inc (Named "ArmourBreakAmount") v |> Mod.whenever (EnemyHas "AnyAilment"))
        | "support_increased_armour_break_armour_break_amount_+%_final" -> one (Mod.more (Named "ArmourBreakAmount") v)
        | "armour_break_and_sundered_armour_duration_+%" -> one (Mod.inc (Named "ArmourBreakDuration") v)
        | "broken_armour_and_sundered_armour_debuff_effect_+%" -> one (Mod.inc (Named "FullyBrokenArmourEffect") v)
        | "damage_+%_against_enemies_with_fully_broken_armour" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (EnemyHas "FullyBrokenArmour"))
        | "fully_broken_armour_and_sundered_armour_you_inflict_applies_to_all_damage" -> one (Mod.flag (Named "BrokenArmourAllDamage"))
        | "fully_broken_armour_and_sundered_armour_you_inflict_also_applies_to_fire_damage" -> one (Mod.flag (Named "BrokenArmourFire"))
        | "fully_broken_armour_and_sundered_armour_you_inflict_also_applies_to_cold_and_lightning_damage" -> one (Mod.flag (Named "BrokenArmourColdLightning"))
        // rage (see Sim: gained on hits, kills and when hit; lost after a while without gaining)
        | "gain_x_rage_on_hit" | "base_gain_x_rage_on_hit" | "local_gain_X_rage_on_hit" -> one (Mod.base' (Named "RageOnHit") v |> Mod.whenever (SkillIs Attack))
        | "gain_x_rage_on_melee_hit" -> one (Mod.base' (Named "RageOnHit") v |> Mod.whenever (SkillIs Melee))
        | "gain_x_rage_on_hit_with_axes" -> one (Mod.base' (Named "RageOnHit") v |> Mod.whenever (SkillIs Axe))
        | "gain_x_rage_when_hit" -> one (Mod.base' (Named "RageWhenHit") v)
        | "gain_rage_on_kill" -> one (Mod.base' (Named "RageOnKill") v)
        | "base_rage_regeneration_per_minute" | "ceaseless_rage_base_rage_regeneration_per_minute" -> one (Mod.base' (Named "RageRegen") (v / 60.0))
        | "no_inherent_rage_loss" -> one (Mod.flag (Named "NoRageLoss"))
        | "rage_decay_speed_+%" -> one (Mod.inc (Named "RageDecay") v)
        | "rage_loss_delay_ms_+" -> one (Mod.base' (Named "RageLossDelay") (v / 1000.0))
        | "additional_base_critical_strike_chance" -> one (Mod.base' CritChance (v / 100.0))
        | "base_penetrate_elemental_resistances_%" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Penetration t) v)))
        | "all_skill_gem_level_+" -> one (Mod.base' (Named "GemLevel") v)
        | "attack_speed_while_dual_wielding_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "DualWielding"))
        | "critical_strike_chance_+%_if_have_not_crit_recently" -> one (Mod.inc CritChance v |> Mod.whenever (Not(WhileBuff "CritRecently")))
        | "number_of_additional_poison_stacks" -> one (Mod.base' (AilmentStacks Poison) v)
        | "poison_on_critical_strike" -> one (Mod.base' (AilmentChance Poison) 100.0 |> Mod.whenever IsCrit)
        | "attacks_num_of_additional_chains" -> one (Mod.base' Chains v |> Mod.whenever (SkillIs Attack))
        | "critical_strike_multiplier_with_dagger_+" -> one (Mod.inc CritBonus v |> Mod.whenever (SkillIs Dagger))
        | "invocation_spell_damage_+%" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs Triggered))
        | "cannot_be_blinded" | "you_cannot_be_hindered" | "weapon_swap_speed_+%" -> Some Structural
        | "active_skill_ignite_chance_+%_final" -> one (Mod.more (AilmentChance Ignite) v)
        | "active_skill_ignite_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Ignite)) v)
        | "never_shock" -> one (Mod.more (AilmentChance Shock) -100.0)
        | "never_ignite" -> one (Mod.more (AilmentChance Ignite) -100.0)
        | "global_poison_on_hit" -> one (Mod.base' (AilmentChance Poison) 100.0)
        | "global_maim_on_hit" -> one (Mod.base' (AilmentChance Maim) 100.0)
        | "base_lightning_damage_can_electrocute" -> one (Mod.flag (Named "CanElectrocute"))
        | "active_skill_electrocutes_as_though_dealt_damage_+%_final" -> one (Mod.more (Buildup Electrocute) v)
        | "active_skill_reload_speed_+%_final" -> one (Mod.more (Named "ReloadSpeed") v)
        // read by the skill builder itself (base damage, damage over time, bolts), or about how
        // the skill is aimed, animated and interrupted
        | "action_attack_or_cast_time_uses_animation_length" | "spell_maximum_action_distance_+%" | "attack_maximum_action_distance_+"
        | "melee_range_+" | "skill_can_fire_wand_projectiles" | "base_skill_can_be_blocked" | "projectiles_not_offset"
        | "skill_does_not_pathfind" | "skill_disabled_unless_cloned" | "projectiles_fire_at_ground" | "skill_cannot_be_stunned"
        | "skill_cannot_be_knocked_back" | "skill_cannot_be_frozen" | "skill_cannot_be_electrocuted" | "skill_cannot_be_interrupted"
        | "base_skill_is_totemified" | "skill_is_deploy_skill" | "skill_is_command" | "minions_do_not_move_to_skill_target"
        | "melee_defer_damage_prediction" | "action_do_not_face_target" | "usable_with_talisman" | "ignores_proximity_shield"
        | "disable_quadruped_head_control" | "ballistic_projectiles_always_bounce" | "range_backflip_conditional_distance"
        | "channel_start_lock_cancelling_of_attack_time_%" | "base_skill_enabled_while_mounted" | "support_double_barrel_number_of_crossbow_bolts_+"
        | "base_number_of_crossbow_bolts" | "number_of_projectiles_cannot_be_modified" -> Some Structural
        // read where the skill is built: the infusion a skill consumes (Behaviours.infusedCopy);
        // a projectile part that has only what its charges give it (Gems: no base count beside
        // "..._number_of_projectiles_per_power_charge", so none without charges)
        | "active_skill_consumes_a_fire_infusion" | "active_skill_consumes_a_cold_infusion" | "active_skill_consumes_a_lightning_infusion"
        | "zero_projectiles_if_no_power_charges" -> Some Structural
        // how far away the skill can be aimed (Falling Thunder with charges): the player stands
        // and every enemy can be aimed at, as with "attack_maximum_action_distance_+"
        | "lightning_strike_maximum_action_distance_+_when_empowered" -> Some Structural
        | id when id.StartsWith "triggered_by_" || id.StartsWith "monster_"
                  || ((id.StartsWith "spell_minimum_base_" || id.StartsWith "spell_maximum_base_") && id.EndsWith "_damage")
                  || (id.StartsWith "base_" && id.EndsWith "_damage_to_deal_per_minute") -> Some Structural
        // Knockback (Sim.hitEnemy): which hits knock back, how far (a skill's own distance; more
        // or less by how close the enemy is), and what scales it. How fast the monster is pushed
        // is nothing here: it is where it lands at once.
        | "global_knockback" | "knockback" -> one (Mod.base' (Named "KnockbackChance") 100.0)
        | "knockback_on_crit_with_quarterstaff" -> one (Mod.base' (Named "KnockbackChance") 100.0 |> Mod.whenever IsCrit |> Mod.whenever (SkillIs Quarterstaff))
        | "area_skill_knockback_chance_%" -> one (Mod.base' (Named "KnockbackChance") v |> Mod.whenever (SkillIs Area))
        | "cannot_knockback" -> one (Mod.flag (Named "CannotKnockback"))
        | "base_knockback_distance" -> one (Mod.base' (Named "KnockbackBase") v)
        | "knockback_distance_+%" -> one (Mod.inc (Named "KnockbackDistance") v)
        | "support_arms_length_knockback_distance_+%_final" | "active_skill_knockback_distance_+%_final" -> one (Mod.more (Named "KnockbackDistance") v)
        | "generic_knockback_distance_limit" -> one (Mod.base' (Named "KnockbackLimit") v)
        | "generic_knockback_+%_final_at_min_distance" -> one (Mod.base' (Named "KnockbackAtMin") v)
        | "generic_knockback_+%_final_at_max_distance" -> one (Mod.base' (Named "KnockbackAtMax") v)
        | "base_knockback_speed_+%" -> Some Structural
        // warcries: the attacks they empower, and the cry itself
        | "empowered_attack_damage_+%" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "EmpoweredAttacks"))
        // "Empowered Attacks Gain X% of Physical Damage as Extra Fire damage"
        | "empowered_attack_physical_damage_%_to_gain_as_fire" ->
            one (Mod.base' (GainAs(Physical, Fire)) v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "EmpoweredAttacks"))
        | "warcry_speed_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Warcry))
        | "warcry_cooldown_speed_+%" -> one (Mod.inc CooldownRecovery v |> Mod.whenever (SkillIs Warcry))
        | "support_voranas_siege_hit_damage_+%_final_vs_isolated_enemies" -> one (Mod.more (Damage AllDamage) v |> Mod.whenever (EnemyHas "Isolated"))
        // the game's standard named effects (see Behaviours.keywordMods / keywordRules).
        // "Rule:<hook>:<effect>:<seconds>" is a chance (%) to gain / inflict it.
        | "have_unholy_might" -> one (Mod.flag (Named "Has:UnholyMight"))
        | "grant_unholy_might_to_self_while_not_on_low_mana" -> one (Mod.flag (Named "Has:UnholyMight") |> Mod.whenever (Not(WhileBuff "LowMana")))
        | "unholy_might_while_you_have_no_energy_shield" -> one (Mod.flag (Named "Has:UnholyMight") |> Mod.whenever (Not(WhileBuff "HasEnergyShield")))
        | "minions_have_unholy_might" -> Some(Mods [ for t in DamageType.all -> Mod.base' (ForMinions(GainAs(t, Chaos))) 30.0 ])
        | "base_unholy_might_granted_magnitude_+%" -> one (Mod.inc (Named "UnholyMightMagnitude") v)
        | "onslaught_effect_+%" -> one (Mod.inc (Named "OnslaughtEffect") v)
        | "arcane_surge_effect_on_self_+%" -> one (Mod.inc (Named "ArcaneSurgeEffect") v)
        | "withered_magnitude_+%" -> one (Mod.inc (Named "WitheredMagnitude") v)
        | "tailwind_effect_on_self_+%" -> one (Mod.inc (Named "TailwindEffect") v)
        | "gain_arcane_surge_on_crit_%_chance" -> one (Mod.base' (Named "Rule:OnCrit:ArcaneSurge:10") v)
        | "gain_arcane_surge_on_hit_%_chance" -> one (Mod.base' (Named "Rule:OnHit:ArcaneSurge:10") v |> Mod.whenever (SkillIs Spell))
        | "gain_arcane_surge_on_kill_chance_%" -> one (Mod.base' (Named "Rule:OnKill:ArcaneSurge:10") v)
        | "onslaught_buff_duration_on_kill_ms" -> one (Mod.base' (Named(sprintf "Rule:OnKill:Onslaught:%g" (v / 1000.0))) 100.0)
        | "onslaught_time_granted_on_kill_ms" -> one (Mod.base' (Named "Rule:OnKill:Onslaught:4") v)
        // "X% chance to gain Onslaught on Killing Hits with this Weapon" (the weapon's own line,
        // read without "local_": for attacks), "... on Killing Blow with Axes": the text names
        // no duration; 4 s as "for 4 seconds on Kill" (guess)
        | "chance_to_gain_onslaught_on_killing_blow_%" | "chance_to_gain_onslaught_on_kill_for_4_seconds_%" -> one (Mod.base' (Named "Rule:OnKill:Onslaught:4") v)
        | "chance_to_gain_onslaught_on_kill_with_axes_%" -> one (Mod.base' (Named "Rule:OnKill:Onslaught:4") v |> Mod.whenever (SkillIs Axe))
        | "chance_to_gain_onslaught_on_kill_for_10_seconds_%" -> one (Mod.base' (Named "Rule:OnKill:Onslaught:10") v)
        | "onslaught_on_crit_duration_ms" -> one (Mod.base' (Named(sprintf "Rule:OnCrit:Onslaught:%g" (v / 1000.0))) 100.0)
        | "withered_on_hit_chance_%" | "withered_on_hit_for_4_seconds_%_chance" -> one (Mod.base' (Named "Rule:OnHit:Withered:4") v)
        | "withered_on_hit_for_2_seconds_%_chance" | "local_withered_on_hit_for_2_seconds_%_chance" -> one (Mod.base' (Named "Rule:OnHit:Withered:2") v)
        | "spells_have_x%_chance_inflict_withered_on_hit" -> one (Mod.base' (Named "Rule:OnHit:Withered:4") v |> Mod.whenever (SkillIs Spell))
        | "withered_on_chaos_damage_hit_chance_%" -> one (Mod.base' (Named "Rule:OnHit:Withered:4") v |> Mod.whenever (SkillIs(Tag "Chaos")))
        | "gain_tailwind_stack_on_skill_use" -> one (Mod.base' (Named "Rule:OnUse:Tailwind:3600") 100.0)
        | "lose_all_tailwind_when_hit" -> one (Mod.base' (Named "Rule:WhenHit:-Tailwind:0") 100.0)
        | "enemies_in_presence_gain_critical_weakness_every_second_for_seconds" -> one (Mod.base' (Named "CriticalWeaknessAura") v)
        | "local_culling_strike" | "culling_strike" -> one (Mod.flag (Named "CullingStrike"))
        | "spells_have_culling_strike" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (SkillIs Spell))
        | "crits_have_culling_strike" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever IsCrit)
        | "bow_attacks_have_culling_strike" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (SkillIs Bow))
        | "culling_strike_vs_cursed_enemies" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (EnemyHas "Cursed"))
        | "culling_strike_vs_marked_enemy" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (EnemyHas "Marked"))
        | "culling_strike_on_burning_enemies" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (EnemyHas "Ignited"))
        | "support_culling_strike_vs_rare_or_unique_enemy" -> one (Mod.flag (Named "CullingStrike") |> Mod.whenever (EnemyHas "RareOrUnique"))
        | "culling_strike_threshold_+%" -> one (Mod.inc (Named "CullingThreshold") v)
        | "culling_strike_threshold_+%_vs_rare_or_unique_monsters" -> one (Mod.inc (Named "CullingThreshold") v |> Mod.whenever (EnemyHas "RareOrUnique"))
        | "culling_strike_threshold_+%_vs_immobilised_enemies" -> one (Mod.inc (Named "CullingThreshold") v |> Mod.whenever (EnemyHas "Immobilised"))
        // what the attacker does to the enemy's resistance and damage taken (see Hit / Sim.hitEnemy)
        | "freeze_applies_cold_resistance_+" -> one (Mod.base' (Named "EnemyResist:Cold") v |> Mod.whenever (EnemyHas "Frozen"))
        | "enemies_damage_taken_+%_while_cursed" -> one (Mod.inc (Named "EnemyDamageTaken") v |> Mod.whenever (EnemyHas "Cursed"))
        | "marked_enemy_damage_taken_+%" -> one (Mod.inc (Named "EnemyDamageTaken") v |> Mod.whenever (EnemyHas "Marked"))
        | "damaging_ailments_deal_damage_+%_faster" -> one (Mod.inc (Named "FasterAilments") v)
        | "faster_burn_%" -> one (Mod.inc (Named "FasterIgnite") v)
        | "exposure_effect_+%" -> one (Mod.inc (Named "ExposureEffect") v)
        | "hit_damage_stun_multiplier_+%_vs_enemies_at_close_range" -> one (Mod.inc (Buildup HeavyStun) v |> Mod.whenever (EnemyHas "Close"))
        | "passive_overwhelming_strike_hit_damage_stun_multiplier_+%_final_with_crits" -> one (Mod.more (Buildup HeavyStun) v |> Mod.whenever IsCrit)
        | "non_damaging_ailment_effect_+%_on_self" -> Some(Mods [ Mod.inc (Named "SelfEffect:Chill") v; Mod.inc (Named "SelfEffect:Shock") v ])
        | "base_enemy_critical_strike_chance_+%_against_self" -> one (Mod.inc (Named "EnemyCritChanceAgainstSelf") v)
        | "defend_with_%_armour_against_ranged_attacks" -> one (Mod.base' (Named "DefendWithArmourVsProjectiles") v)
        | "defend_with_%_armour_against_critical_strikes" -> one (Mod.base' (Named "DefendWithArmourVsCrits") v)
        | "mark_use_speed_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs(Tag "Type:Mark")))
        | "base_mana_cost_efficiency_+%_of_mark_skills" -> one (Mod.inc ManaCostEfficiency v |> Mod.whenever (SkillIs(Tag "Type:Mark")))
        | "summon_totem_cast_speed_+%" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs(Tag "Type:SummonsTotem")))
        // Spirit and what reserves it (see Behaviours.spirit)
        | "base_spirit" | "base_spirit_from_equipment" -> one (Mod.base' (Named "Spirit") v)
        | "spirit_+%" -> one (Mod.inc (Named "Spirit") v)
        | "body_armour_grants_spirit_+%" -> one (Mod.inc (Named "Spirit") v)
        | "+1_spirit_per_X_energy_shield_on_body_armour" -> one (Mod.base' (Named "SpiritPerBodyES") v)
        | "+1_spirit_per_X_evasion_rating_on_body_armour" -> one (Mod.base' (Named "SpiritPerBodyEvasion") v)
        | "max_life_%_as_spirit" -> one (Mod.base' (Named "SpiritFromLife") v)
        | "cannot_gain_spirit_from_equipment" -> one (Mod.flag (Named "NoSpiritFromEquipment"))
        | "spirit_does_not_exist" -> one (Mod.flag (Named "NoSpirit"))
        | "base_spirit_reservation_efficiency_+%" | "base_reservation_efficiency_+%" -> one (Mod.inc (Named "ReservationEfficiency") v)
        | "reservation_efficiency_+%_of_meta_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Meta")))
        | "reservation_efficiency_+%_of_herald_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Herald")))
        | "reservation_efficiency_+%_of_minion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Minion")))
        | "reservation_efficiency_+%_of_companion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Companion")))
        | "tactician_spirit_reservation_+%_final_for_permanent_buffs" -> one (Mod.more (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:Buff")))
        | "unique_spirit_reservations_are_halved" -> one (Mod.more (Named "Reservation") -50.0)
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
        // "while stationary": the player stands (RULES.md). (The pools' recovery is worked out
        // once, for the standing character: one that is taken to move, because it gains from
        // moving, keeps these all the same.)
        | "life_regeneration_rate_+%_while_stationary" -> one (Mod.inc LifeRegen v |> Mod.whenever (Not(WhileBuff "Moving")))
        | "life_regeneration_rate_per_minute_%_while_stationary" -> one (Mod.base' LifeRegenPercent (v / 60.0) |> Mod.whenever (Not(WhileBuff "Moving")))
        | "mana_regeneration_rate_+%_while_stationary" -> one (Mod.inc ManaRegen v |> Mod.whenever (Not(WhileBuff "Moving")))
        // "X% increased Stun Recovery": a light stun on the player is over that much sooner
        | "base_stun_recovery_+%" -> one (Mod.inc (Named "StunRecovery") v)
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
        // "X% increased Slowing Potency of Debuffs on You": a multiplier of its own on how much
        // a slow on the player slows (the Chill of a monster's cold hit is the one slow there is)
        | "base_slow_potency_+%" -> one (Mod.inc (Named "SlowPotencyOnSelf") v)
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
            // "fire_and_cold_damage_resistance_%", "lightning_and_chaos_damage_resistance_%"
            | [ a; "and"; b; "damage"; "resistance"; "%" ] when (typeWord a).IsSome && (typeWord b).IsSome ->
                each [ Resist (typeWord a).Value; Resist (typeWord b).Value ]
            | [ "base"; "armour"; "%"; "applies"; "to"; t; "damage" ] when (typeWord t).IsSome -> one (Mod.base' (Named("ArmourAppliesTo:" + DamageType.name (typeWord t).Value)) v)
            | [ "base"; "maximum"; t; "damage"; "resistance"; "%" ] when (typeWord t).IsSome -> one (Mod.base' (MaxResist (typeWord t).Value) v)
            | _ -> None

    /// Stats that are no modifiers: the numbers of a mechanic, read by their id where that
    /// mechanic is built (the function named with each group), whatever gem carries them.
    let private readWhereBuilt =
        set [ // Behaviours.ghostDance, periodics (Time of Need, Flames of Chayula)
              "ghost_dance_max_stacks"; "skill_base_ghost_dance_grants_%_evasion_as_es_regeneration_per_minute_if_have_lost_ghost_dance_shroud_recently"
              "skill_time_of_need_gain_x_life"; "breach_flame_spawn_rate_ms"; "breach_flame_chaos_addition_%"
              // Behaviours.tempestBell
              "bell_hit_limit"; "bell_shockwave_cooldown_ms"
              // Gems: shield attacks, a support's quality for its skill, how projectiles fly and
              // land, channels released at the perfect moment, poison for the skill's duration
              "off_hand_weapon_minimum_physical_damage"; "off_hand_weapon_maximum_physical_damage"; "supported_active_skill_gem_quality_%"
              "twister_hit_interval_ms"; "twister_base_movement_speed"; "minimum_per_projectile_spread_angle_degrees"; "rain_hit_delay_ms"
              "spear_wall_cm_per_spear"; "nova_skills_cast_at_target_location"; "projectiles_barrage"; "poison_duration_is_skill_duration"
              "channel_end_duration_as_%_of_attack_time"; "channel_skill_end_animation_duration_multiplier_permyriad"
              // Behaviours.minions, summons, totems: how many, how they come and go
              "skill_wolf_pack_size"; "base_number_of_totems_allowed"; "base_number_of_zombies_allowed"; "max_azmerian_swarms"
              "skill_grants_%_chance_to_summon_azmerian_swarm_on_kill_affected_by_elemental_ailment"
              "skill_grants_%_chance_to_summon_azmerian_swarm_on_hitting_unique_or_rare_enemy_affected_by_elemental_ailment"
              "skill_raging_spirits_max_per_action"; "ravenous_swarm_maximum_swarm_count"; "minion_base_resummon_time_ms"
              "companion_takes_%_damage_before_you_from_support"; "explode_on_low_life_%_maximum_life_to_deal"; "base_totem_duration"
              "active_skill_minion_1%_attack_speed_+%_per_X_player_dexterity"; "minion_1%_damage_+%_per_X_player_strength"
              "wardbound_minions_summon_minion_for_every_X_runic_ward_spent"; "wardbound_minion_maximum_number_of_casts"
              // Behaviours.warcry, curseDelay, impendingDoom, useRules (a skill's exposure)
              "seismic_cry_slam_skill_additional_aftershocks"; "warcry_empowers_per_X_monster_power"; "warcry_empowers_per_X_monster_power_mp_cap"
              "curse_delay_duration_ms"; "trigger_vicious_hex_explosion_when_curse_ends"
              "active_skill_all_elemental_exposure_magnitude"; "active_skill_all_elemental_exposure_compounding_magnitude"
              "active_skill_all_elemental_exposure_compounding_magnitude_cap" ]

    let private explicit (id: string) (v: float) : Meaning option =
        let one m = Some(Mods [ m ])
        match id with
        | id when readWhereBuilt.Contains id -> Some Structural
        // (a support's "more / less life" for the minions of its gem: Behaviours.minions)
        | id when id.StartsWith "support_" && id.EndsWith "minion_life_+%_final" -> Some Structural
        // where a Flame of Chayula appears and from how far it is seen: every flame is taken
        // to be picked up at once (RULES.md, Flames of Chayula)
        | "breach_flame_spawn_radius" | "breach_walk_base_visibility_range" -> Some Structural
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
        | "minion_elemental_resistance_%" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (ForMinions(Resist t)) v)))
        | "minion_chaos_resistance_%" -> one (Mod.base' (ForMinions(Resist Chaos)) v)
        | "minion_maximum_all_elemental_resistances_%" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (ForMinions(MaxResist t)) v)))
        | "temporary_minion_limit_+" -> one (Mod.base' (ForMinions(Named "TemporaryMinionLimit")) v)
        | "minion_cooldown_recovery_+%" -> one (Mod.inc (ForMinions CooldownRecovery) v)
        | "minion_command_skill_cooldown_speed_+%" -> one (Mod.inc (ForMinions CooldownRecovery) v |> Mod.whenever (SkillIs(Tag "Command")))
        | "minion_commanded_skill_damage_+%" -> one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (SkillIs(Tag "Command")))
        | "minion_command_skill_skill_speed_+%" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (SkillIs(Tag "Command")))
        | "companion_%_damage_as_chaos" -> Some(Mods [ for t in DamageType.all -> Mod.base' (ForMinions(GainAs(t, Chaos))) v |> Mod.whenever (SkillIs(Tag "Companion")) ])
        // per different type of persistent minion, per minion: counted when the fight is set up
        | "minion_commanded_skill_damage_+%_per_different_persistent_minion_in_presence" ->
            one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (SkillIs(Tag "Command")) |> Mod.whenever (PerStack("PersistentMinionTypes", 1.0)))
        | "support_varied_troops_damage_+%_final_per_different_persistent_ominion" -> one (Mod.more (Damage AllDamage) v |> Mod.whenever (PerStack("PersistentMinionTypes", 1.0)))
        | "damage_+%_per_active_minion" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (PerStack("ActiveMinions", 1.0)))
        | "damage_+%_per_different_companion_in_presence" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (PerStack("CompanionTypes", 1.0)))
        | "companions_you_control_gain_damage_+%_against_enemies_marked_by_you" ->
            one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (SkillIs(Tag "Companion")) |> Mod.whenever (EnemyHas "Marked"))
        // (how long a minion's Withered lasts is not in the stat: 4 s as for the player's)
        | "minions_have_%_chance_to_inflict_wither_on_hit" -> one (Mod.base' (ForMinions(Named "Rule:OnHit:Withered:4")) v)
        // "Companions gain added Attack damage equal to x% of your main hand Weapon's damage"
        | "weapon_damage_%_granted_to_companions" -> one (Mod.base' (Named "CompanionWeaponDamage") v)
        // "x% of Damage from Hits is taken from your Damageable Companion's Life before you"
        | "companion_takes_%_damage_before_you" -> one (Mod.base' (Named "CompanionTakesDamage") v)
        // "Allies in your Presence Regenerate X% of their Maximum Life per second", "... X Life per
        // second", "... X% of your Maximum Life per second": the minions' regeneration
        // (Behaviours.minionBody; the minions are taken to be in the Presence)
        | "allies_in_presence_life_regeneration_rate_per_minute_equal_to_their_maximum_life_%" -> one (Mod.base' (ForMinions LifeRegenPercent) (v / 60.0))
        | "allies_in_presence_life_regeneration_rate_per_minute" -> one (Mod.base' (ForMinions LifeRegen) (v / 60.0))
        | "allies_in_presence_life_regeneration_rate_per_minute_equal_to_your_maximum_life_%" -> one (Mod.base' (Named "AllyRegenPercentOfOwnerLife") (v / 60.0))
        // "You and Allies in your Presence have Unholy Might while you are not on Low Mana": the
        // minions' part (the player's own is "grant_unholy_might_to_self_while_not_on_low_mana";
        // that the player is not on low mana is not looked at for them)
        | "allies_in_presence_have_unholy_might_while_you_not_on_low_mana" -> Some(Mods [ for t in DamageType.all -> Mod.base' (ForMinions(GainAs(t, Chaos))) 30.0 ])
        | "minion_resummon_speed_+%" -> one (Mod.inc (ForMinions(Named "MinionReviveSpeed")) v)
        | "minion_maximum_life_%_to_gain_as_maximum_energy_shield" -> one (Mod.base' (ForMinions(Named "LifeAsExtraEnergyShield")) v)
        | "minion_attack_speed_+%" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (SkillIs Attack))
        | "minion_cast_speed_+%" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (SkillIs Spell))
        | "minion_attack_and_cast_speed_+%" -> one (Mod.inc (ForMinions Speed) v)
        | "base_cooldown_speed_+%" -> one (Mod.inc CooldownRecovery v)
        // "Supported Spells have X% increased Cooldown Recovery Rate"
        | "base_spell_cooldown_speed_+%" -> one (Mod.inc CooldownRecovery v |> Mod.whenever (SkillIs Spell))
        | "base_cooldown_speed_+%_final" -> one (Mod.more CooldownRecovery v)
        // "Attack Damage Penetrates X% of Enemy Elemental Resistances"
        | "base_attack_damage_penetrates_elemental_resist_%" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Penetration t) v |> Mod.whenever (SkillIs Attack))))
        // "X% chance for Mace Slam Skills you use yourself to cause an additional Aftershock"
        | "your_mace_slam_aftershock_chance_%" -> one (Mod.base' (Named "AftershockChance") v |> Mod.whenever (SkillIs Slam) |> Mod.whenever (SkillIs Mace))
        // "X% increased Magnitude of Non-Damaging Ailments you inflict with Critical Hits"
        | "non_damaging_ailment_effect_+%_with_critical_strikes" -> Some(Mods([ Shock; Chill ] |> List.map (fun a -> Mod.inc (AilmentMagnitude(Some a)) v |> Mod.whenever IsCrit)))
        // "X% increased Magnitude of Ailments you inflict against Marked Enemies"
        | "ailment_magnitude_+%_vs_marked_enemies" -> one (Mod.inc (AilmentMagnitude None) v |> Mod.whenever (EnemyHas "Marked"))
        // "Projectiles have X% increased Critical Damage Bonus against Enemies within 2m", "Projectiles
        // deal X% increased Damage with Hits against Enemies within 2m" (Close Range: within 2 m)
        | "critical_hit_damage_bonus_+%_vs_enemies_within_2m_distance" -> one (Mod.inc CritBonus v |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (EnemyHas "Close"))
        | "projectile_damage_+%_vs_enemies_within_2m_distance" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (EnemyHas "Close"))
        // "X% chance for Attacks to Maim on Hit against Poisoned Enemies"
        | "maim_on_hit_%_vs_poisoned_enemies" -> one (Mod.base' (AilmentChance Maim) v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (EnemyHas "Poisoned"))
        // "Gain X% of Damage as Extra Damage of a random Element": a third of it as each (the
        // average; which element, and how often it is chosen anew, is not in the text)
        | "non_skill_base_all_damage_%_to_gain_as_random_element" ->
            Some(Mods [ for into in DamageType.elemental do for from in DamageType.all do yield Mod.base' (GainAs(from, into)) (v / 3.0) ])
        | "grenade_skill_number_of_additional_projectiles" -> one (Mod.base' AdditionalProjectiles v |> Mod.whenever (SkillIs(Tag "Grenade")))
        | "hazard_damage_+%" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs(Tag "Type:Hazard")))
        // "X% chance for Remnants you pick up to count as picking up an additional Remnant": as a
        // chance of an additional Remnant (the two added up: a guess)
        | "created_remnants_have_%_chance_to_duplicate_pick_up_results" -> one (Mod.base' (Named "ExtraRemnantChance") v)
        // "X% more Mana Cost of Skills if you have no Energy Shield"
        | "lich_mana_cost_+%_final_if_you_have_no_energy_shield" -> one (Mod.more ManaCost v |> Mod.whenever (Not(WhileBuff "HasEnergyShield")))
        // "Physical Spell Critical Hits build Pin"
        | "physical_spell_damage_can_pin_on_critical_hit" -> one (Mod.flag (Named "CanPin") |> Mod.whenever (SkillIs Spell) |> Mod.whenever IsCrit)
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
        | "non_skill_base_all_damage_%_to_gain_as_cold_fire_lightning" ->
            Some(Mods [ for into in DamageType.elemental do for from in DamageType.all do yield Mod.base' (GainAs(from, into)) v ])
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
        | "energy_generated_+%_if_crit_recently" -> one (Mod.inc (Named "EnergyGenerated") v |> Mod.whenever (WhileBuff "CritRecently"))
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
        | "modifiers_to_fire_resistance_also_apply_to_cold_lightning_resistance_at_%_value" -> one (Mod.base' (Named "FireResToColdLightning") v)
        | "modifiers_to_maximum_fire_resistance_apply_to_maximum_cold_and_lightning_resistance" -> one (Mod.flag (Named "MaxFireResToColdLightning"))
        // "X% more maximum Life" and the like: one text, several ids (an ascendancy's, a unique's)
        | "titan_maximum_life_+%_final" | "oracle_maximum_life_+%_final" | "vivisection_maximum_life_+%_final"
        | "unique_jewel_reserved_blood_maximum_life_+%_final" | "maximum_life_+%_final_from_caster_weapon_runic_ward_socketable" -> one (Mod.more MaxLife v)
        | "oracle_maximum_mana_+%_final" | "vivisection_maximum_mana_+%_final" -> one (Mod.more MaxMana v)
        | "vivisection_spirit_+%_final" -> one (Mod.more (Named "Spirit") v)
        | "master_of_elements_evasion_rating_+%_final" -> one (Mod.more Evasion v)
        // "+X to A per N B on Equipped <item>": Pob.character turns these into flat amounts
        | "gain_X_max_life_per_8_armour_on_equipped_helmet" -> one (Mod.base' (Named "PerItemDefence|MaxLife|Helmet|Armour|8") v)
        | "energy_shield_+_per_8_helmet_armour" -> one (Mod.base' (Named "PerItemDefence|MaxEnergyShield|Helmet|Armour|8") v)
        | "evasion_rating_+_per_1_armour_on_gloves" -> one (Mod.base' (Named "PerItemDefence|Evasion|Gloves|Armour|1") v)
        | "energy_shield_+_per_8_evasion_on_boots" -> one (Mod.base' (Named "PerItemDefence|MaxEnergyShield|Boots|Evasion|8") v)
        | "armour_+_per_1_boots_energy_shield" -> one (Mod.base' (Named "PerItemDefence|Armour|Boots|MaxEnergyShield|1") v)
        | "armour_+_per_1_helmet_maximum_energy_shield" -> one (Mod.base' (Named "PerItemDefence|Armour|Helmet|MaxEnergyShield|1") v)
        | "evasion_rating_+_per_1_helmet_energy_shield" -> one (Mod.base' (Named "PerItemDefence|Evasion|Helmet|MaxEnergyShield|1") v)
        | "maximum_energy_shield_+_per_6_body_armour_evasion_rating" -> one (Mod.base' (Named "PerItemDefence|MaxEnergyShield|Body Armour|Evasion|6") v)
        | "maximum_energy_shield_+_per_5_armour_on_shield" -> one (Mod.base' (Named "PerItemDefence|MaxEnergyShield|Weapon 2|Armour|5") v)
        | "evasion_rating_+_per_5_maximum_energy_shield_on_shield" -> one (Mod.base' (Named "PerItemDefence|Evasion|Weapon 2|MaxEnergyShield|5") v)
        | "physical_damage_reduction_rating_per_5_evasion_on_shield" -> one (Mod.base' (Named "PerItemDefence|Armour|Weapon 2|Evasion|5") v)
        // "increased effect of / bonuses gained from": Pob.sheet scales the mods of that source
        | "small_passives_effect_+%" -> one (Mod.inc (Named "EffectOf:SmallPassives") v)
        | "phylactery_jewel_socket_effect_+%" -> one (Mod.inc (Named "EffectOf:AscendancyJewel") v)
        | "quiver_mod_effect_+%" -> one (Mod.inc (Named "EffectOf:Quiver") v)
        | "equipped_ring1_effect_of_bonuses_+%" -> one (Mod.inc (Named "EffectOf:Ring 1") v)
        | "equipped_ring2_effect_of_bonuses_+%" -> one (Mod.inc (Named "EffectOf:Ring 2") v)
        | "equipped_jewellery_effect_of_bonuses_+%" -> Some(Mods [ for s in [ "Ring 1"; "Ring 2"; "Ring 3"; "Amulet" ] -> Mod.inc (Named("EffectOf:" + s)) v ])
        | "gain_no_inherent_bonus_from_strength" -> one (Mod.flag (Named "NoStrengthBonus"))
        | "keystone_blood_magic" -> one (Mod.flag (Named "BloodMagic"))
        // "Convert 100% of maximum Energy Shield to maximum Mana; Mana Costs are Doubled"
        | "keystone_eldritch_battery" -> Some(Mods [ Mod.base' (Named "ESConvertedToMana") v; Mod.more ManaCost 100.0 ])
        | "keystone_chaos_inoculation" -> one (Mod.flag (Named "Keystone:Chaos Inoculation"))
        // ---- engine-internal or cosmetic gem stats ----
        | "base_deal_no_damage" | "base_deal_no_damage_over_time" | "skill_desired_amount_override" | "number_of_broken_faces"
        | "has_modular_projectiles_enabled" | "should_use_additive_aiming_animation" | "skill_can_fire_arrows"
        | "action_can_be_used_in_aiming_stance" | "action_allowed_queue_time_override_ms" | "active_skill_ignore_setting_aim_stance"
        | "can_be_used_instantly_in_any_set" | "skill_override_pvp_scaling_time_ms" | "terrain_arrow_attachment_chance_reduction_+%"
        | "triggerable_in_any_set" | "attack_is_not_melee_override" | "immune_to_corrupted_blood" | "minions_are_gigantic"
        | "spark_min_time_between_target_clearing_ms" | "active_skill_additional_projectiles_description_mode"
        | "item_can_have_catalyst_quality_in_addition_to_base_quality" | "skill_empower_limitation_specifier_for_stat_description"
        | "phylactery_can_only_contain_non_unique_jewel" | "has_unique_chaos_shrine_effect" -> Some Structural
        | id when id.StartsWith "dummy_stat_" || id.StartsWith "warping_rune_add_item_tag_" || id.StartsWith "quality_display_"
                  || id.StartsWith "pvp_" || id.StartsWith "replace_" || id.EndsWith "_art_variation" -> Some Structural
        // read by id where the mechanic lives (Behaviours): remnants, infusions, conflux, motes
        | id when id.StartsWith "ward_remnants_" || id.StartsWith "mana_remnants_mana_gain" || id.StartsWith "mana_remnants_spawn" || id.StartsWith "mana_remnants_chance" || id.StartsWith "infusion_remnants_" || id.StartsWith "skill_elemental_conflux_"
                  || id = "base_remnant_duration_ms" || id = "verisium_infusion_skill_number_of_verisium_infusion_to_grant" -> Some Structural
        // ---- more offence ----
        // projectile speed changes nothing here: projectiles land when the use hits
        // "increased Projectile Speed": nothing by itself (below), but kept for "Increases and
        // reductions to Projectile speed also apply to Projectile Damage" (Gems.speedAsDamage)
        | "base_projectile_speed_+%" | "projectile_speed_+%" -> one (Mod.inc (Named "ProjectileSpeed") v)
        | "spell_skill_projectile_speed_+%" -> one (Mod.inc (Named "ProjectileSpeed") v |> Mod.whenever (SkillIs Spell))
        | "projectile_speed_+%_with_crossbow_skills" -> one (Mod.inc (Named "ProjectileSpeed") v |> Mod.whenever (SkillIs Crossbow))
        | "projectile_speed_additive_modifiers_also_apply_to_projectile_damage" -> one (Mod.flag (Named "ProjectileSpeedAsDamage"))
        | "arrow_speed_additive_modifiers_also_apply_to_bow_damage" -> one (Mod.flag (Named "ProjectileSpeedAsDamage") |> Mod.whenever (SkillIs Bow))
        // "X% of increases and reductions to Cast speed also apply to Projectile Speed"
        | "cast_speed_additive_modifiers_also_apply_to_projectile_speed_at_%_value" -> one (Mod.base' (Named "CastSpeedAsProjectileSpeed") v)
        // (how much the speed varies from one projectile to the next: read for shards that live
        // a fixed time, Gems `RangeVariation`; for the others as their speed)
        | "active_skill_projectile_speed_+%_variation_final" -> Some NotOffensive
        // (the projectile speed of minions and of a map's monsters is not the player's)
        | id when (id.StartsWith "minion_" || id.StartsWith "map_" || id.StartsWith "monster_") && (id.EndsWith "projectile_speed_+%" || id.EndsWith "projectile_speed_+%_final") -> Some NotOffensive
        // the others: how fast a bolt that is followed flies (Frostbolt, for Ice Nova); nothing else
        | id when id.EndsWith "projectile_speed_+%" -> one (Mod.inc (Named "ProjectileSpeed") v)
        | id when id.EndsWith "projectile_speed_+%_final" -> one (Mod.more (Named "ProjectileSpeed") v)
        | "base_cooldown_modifiable_repeat_interval_ms" | "cooldown_recovery_modifiers_also_apply_to_repeat_interval" -> Some Structural
        | "base_chance_to_pierce_%" -> one (Mod.base' (Named "PierceChance") v)
        | "always_pierce" -> one (Mod.base' Pierce 99.0)
        // "X% chance to fire an additional Projectile" (above 100%: one for sure and a chance of another)
        | "chance_to_fire_1_additional_projectile_%_with_rollover" -> one (Mod.base' (Named "ExtraProjectileChance1") v)
        | "chance_to_fire_1_additional_projectile_%_with_rollover_with_bow_attacks" -> one (Mod.base' (Named "ExtraProjectileChance1") v |> Mod.whenever (SkillIs Bow))
        | "aura_effect_+%" | "non_curse_aura_effect_+%" -> one (Mod.inc (Named "AuraEffect") v)
        | "flask_recovery_amount_%_to_recover_instantly" -> one (Mod.base' (Named "FlaskInstant") v)
        | "immune_to_chill_if_majority_blue_supports_socketed" -> one (Mod.base' (Named "Avoid:Chill") 100.0 |> Mod.whenever (WhileBuff "SupportsMajority:blue"))
        | "immune_to_ignite_if_majority_red_supports_socketed" -> one (Mod.base' (Named "Avoid:Ignite") 100.0 |> Mod.whenever (WhileBuff "SupportsMajority:red"))
        | "immune_to_shock_if_majority_green_supports_socketed" -> one (Mod.base' (Named "Avoid:Shock") 100.0 |> Mod.whenever (WhileBuff "SupportsMajority:green"))
        | "physical_damage_from_hits_%_taken_as_random_element" ->
            Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.base' (Named("TakenAs:Physical:" + DamageType.name t)) (v / 3.0))))
        | "energy_shield_from_focus_+%" -> one (Mod.inc (Named "SlotInc:ES:Weapon 2") v)
        | "energy_shield_from_helmet_+%" -> one (Mod.inc (Named "SlotInc:ES:Helmet") v)
        | "energy_shield_from_gloves_+%" -> one (Mod.inc (Named "SlotInc:ES:Gloves") v)
        | "energy_shield_from_boots_+%" -> one (Mod.inc (Named "SlotInc:ES:Boots") v)
        | "maim_on_hit_%" -> one (Mod.base' (AilmentChance Maim) v)
        | "base_skill_cost_efficiency_+%" -> one (Mod.inc ManaCostEfficiency v)
        | "number_of_additional_projectiles" -> one (Mod.base' AdditionalProjectiles v)
        | "number_of_additional_arrows" -> one (Mod.base' AdditionalProjectiles v |> Mod.whenever (SkillIs Bow))
        // "X% chance for Spell Skills to fire 2 additional Projectiles": the average
        | "spell_skills_fire_2_additional_projectiles_final_chance_%" -> one (Mod.base' (Named "ExtraProjectileChance2") v |> Mod.whenever (SkillIs Spell))
        | "attacks_roll_crits_twice" -> one (Mod.flag (Named "LuckyCrit") |> Mod.whenever (SkillIs Attack))
        | id when id.StartsWith "maximum_critical_strike_chance_is_%" -> one (Mod.base' (Named "MaxCritChance") v)
        // Where an Aftershock (or a cascaded copy) lands beside where the slam did: Falling
        // Thunder's is half a metre off. The stat has no text; read off its name. Here a slam's
        // area lies on the enemy it is aimed at, and its Aftershock on the same enemy (RULES.md,
        // "Aftershock"): the geometry has no place for the half metre between the two.
        | "cascade_aftershock_additional_offset_distance" -> Some Structural
        | "your_slam_aftershock_chance_%" | "slam_aftershock_chance_%" -> one (Mod.base' (Named "AftershockChance") v |> Mod.whenever (SkillIs Slam))
        | "hit_damage_immobilisation_multiplier_+%" ->
            Some(Mods([ Freeze; HeavyStun; Electrocute; Pin ] |> List.map (fun a -> Mod.inc (Buildup a) v)))
        | "non_damaging_ailment_effect_+%" -> Some(Mods([ Shock; Chill ] |> List.map (fun a -> Mod.inc (AilmentMagnitude(Some a)) v)))
        | "base_damage_%_deflected" -> one (Mod.base' (Named "DeflectEffect") v)
        | "slows_have_no_potency_on_you" -> one (Mod.more (Named "SlowPotencyOnSelf") -100.0)
        // ---- persistent buff gems ----
        // Archmage: non-channelling spells gain X% of damage as lightning per 100 maximum mana, and cost more
        | "archmage_all_damage_%_to_gain_as_lightning_to_grant_to_non_channelling_spells_per_100_max_mana" ->
            Some(Mods(DamageType.all |> List.filter ((<>) Lightning) |> List.map (fun t ->
                Mod.base' (GainAs(t, Lightning)) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxMana100", 1.0)))))
        | "archmage_max_mana_permyriad_to_add_to_non_channelled_spell_mana_cost" ->
            one (Mod.base' (Named "ExtraCostPercentOfMaxMana") (v / 100.0) |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)))
        // Trinity: X% more elemental damage per 50 resonance; resonance per hit
        | "trinity_damage_+%_final_to_grant_per_50_resonance" -> one (Mod.more (Damage Elemental) v |> Mod.whenever (PerStack("Resonance", 50.0)))
        | "trinity_resonance_to_grant" -> one (Mod.base' (Named "TrinityGain") v)
        | "trinity_loss_per_hit" -> one (Mod.base' (Named "TrinityLoss") v)
        | "trinity_resonance_decay_delay_ms" | "trinity_resonance_loss_per_second" -> Some Structural
        // Wind Dancer: more evasion per stage; taken at full stages (they return within seconds)
        | "wind_dancer_evasion_rating_+%_final_per_stage" -> one (Mod.more Evasion (v * 3.0))
        | "wind_dancer_stages_gained_every_x_ms" | "wind_dancer_maximum_number_of_stages" -> Some Structural
        // Overwhelming Presence: monsters near you are easier to stun and to inflict ailments on
        | "skill_overwhelming_pressure_aura_enemy_ailment_threshold_+%" -> one (Mod.more (Named "EnemyAura:AilmentThreshold") v)
        | "skill_overwhelming_pressure_aura_enemy_stun_threshold_+%" -> one (Mod.more (Named "EnemyAura:PoiseThreshold") v)
        | "damage_over_time_+%" -> one (Mod.inc (Named "DotDamage") v)
        | "damage_over_time_+%_final" -> one (Mod.more (Named "DotDamage") v)
        | "base_skill_effect_duration" | "spell_damage_modifiers_apply_to_skill_dot" -> Some Structural
        | "global_always_hit" | "always_hit" -> one (Mod.override' HitChance 100.0)
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
        | "curse_effect_+%" -> one (Mod.inc (Named "CurseEffect") v)
        | "curse_delay_+%" | "base_curse_delay_+%" -> one (Mod.inc (Named "CurseDelay") v)
        | "unique_no_curse_delay" -> one (Mod.flag (Named "NoCurseDelay"))
        | "base_curse_duration_+%" | "curse_skill_effect_duration_+%" -> one (Mod.inc (Named "CurseDuration") v)
        | "mark_effect_+%" -> one (Mod.inc (Named "MarkEffect") v)
        | "mark_skill_duration_+%" | "mark_duration_+%" -> one (Mod.inc (Named "MarkDuration") v)
        | "keystone_elemental_equilibrium" -> one (Mod.flag (Named "InfusionRemnantsRotate"))
        // "Storm and Plant Spells deal 50% more damage"
        | "marauder_hidden_ascendancy_damage_+%_final" ->
            Some(Mods([ "Type:Storm"; "Type:Plant" ] |> List.map (fun t -> Mod.more (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag t)))))
        | "base_life_leech_does_not_stop_at_full_life" | "can_wield_2h_axe_sword_mace_in_one_hand" -> Some Structural
        | "keystone_conduit" | "minions_get_amulet_stats_instead_of_you" | "keystone_ancestral_bond" ->
            Some(Unsimulated "minions / allies")
        | "keystone_bulwark" -> Some(Unsimulated "movement")
        // "When you kill a Rare monster, you gain its Modifiers for 60 seconds": the monsters of a
        // fight have no modifiers of their own (only what their rarity makes of them)
        | "gain_rare_monster_mods_on_kill_ms" -> Some(Unsimulated "rare monsters' modifiers")
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
        // Berserk: "lose X% of maximum Life per minute per Rage while not losing Rage"
        | "skill_base_life_loss_%_per_minute_per_rage_while_not_losing_rage_to_apply" -> one (Mod.base' (Named "LifeLossPercentPerMinutePerRage") v)
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
        // "minimum added fire damage to attacks", "... to attacks and spells"
        | "attacks" :: "to" :: "damage" :: t :: "added" :: [ bound ] when (typeWord t).IsSome && (bound = "minimum" || bound = "maximum") ->
            let t = (typeWord t).Value
            Some [ Mod.base' (if bound = "minimum" then AddedMin t else AddedMax t) v |> Mod.whenever (SkillIs Attack) ]
        | "spells" :: "and" :: "attacks" :: "to" :: "damage" :: t :: "added" :: [ bound ] when (typeWord t).IsSome && (bound = "minimum" || bound = "maximum") ->
            let t = (typeWord t).Value
            Some [ Mod.base' (if bound = "minimum" then AddedMin t else AddedMax t) v ]
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
            | Some srcs, Some sc -> Some(srcs |> List.map (fun s -> apply sc (Mod.base' (GainAs(s, into)) v)))
            | _ -> None
        // "Gain X% of Damage as Extra Fire Damage": of all damage
        | [ t; "as"; "gain"; "to"; "%"; "damage" ] when (typeWord t).IsSome ->
            Some(DamageType.all |> List.map (fun s -> Mod.base' (GainAs(s, (typeWord t).Value)) v))
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

    /// "<scope> accuracy rating [+%] [conditions]"
    let private accuracy rev v =
        let words = List.rev rev
        let kind, words =
            match List.tryFindIndex ((=) "+%") words with
            | Some i -> Inc, List.take i words @ List.skip (i + 1) words
            | None -> Base, words
        match List.tryFindIndex ((=) "accuracy") words with
        | Some i when i + 1 < words.Length && words.[i + 1] = "rating" ->
            scopes (List.take i words @ List.skip (i + 2) words)
            |> Option.map (fun sc -> [ apply sc (Mod.make kind (Named "Accuracy") v) ])
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

    let private rules = [ perMote; accuracy; defenceScaled; areaOfEffect; buildup; scaledDamage; addedDamage; penetration; gainAs; convert; ailmentStat; crit; speed; gemLevel ]

    /// Try the pattern rules on the whole id; failing that, pull the conditions out of it
    /// ("..._vs_shocked_enemies", "..._when_on_low_life") and try what is left.
    let private pattern (id: string) (v: float) : Meaning option =
        let words = split id
        let tryRules (ws: string list) =
            let rev = List.rev ws
            rules |> List.tryPick (fun rule -> rule rev v)
        match tryRules words with
        | Some ms -> Some(Mods ms)
        | None ->
            let conds, rest = conditions words
            if conds.IsEmpty || rest.IsEmpty then None
            else tryRules rest |> Option.map (fun ms -> Mods(ms |> List.map (fun m -> { m with Conds = conds @ m.Conds })))

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

    /// The player's own movement speed (what is changed is the id's beginning; when, its end):
    /// the player of a fight stands where the fight begins (RULES.md, "The player stands"),
    /// so how fast it could walk, sprint or move while using a skill changes nothing.
    let private ownMovement (id: string) =
        [ "base_movement_velocity_+%"; "movement_speed_+%"; "movement_velocity_+%"; "sprint_movement_speed_+%"
          "movement_speed_penalty_+%_while_performing"; "movement_speed_acceleration_+%_per_second_while_performing_action"
          "movement_speed_while_performing_action_locked_duration_%"; "ignore_armour_movement_penalties"
          "dodge_roll_speed_+%"; "dodge_roll_travel_distance_+" ]
        |> List.exists id.StartsWith
        || id.EndsWith "movement_speed_penalty_+%_final_while_performing_action"

    /// What monsters never do in a fight as it is simulated: they hit, with their damage and
    /// their accuracy, and nothing else (RULES.md, "Monster hits", "Not modelled: monster
    /// skills"). They put no curse on the player, break none of its armour, leave no Maim and
    /// no Bleeding on it, and they neither regenerate life nor have skills with cooldowns. A
    /// stat about one of these changes nothing here; each with what it would be about.
    let private monstersNeverDo (id: string) =
        [ // "increased / reduced effect of Curses on you", "increased Damage per Curse on you"
          "curse_effect_on_self_+%"; "damage_+%_per_active_curse_on_self"
          // "increased / reduced Armour Break taken"
          "armour_break_taken_+%"
          // "Deflected Hits cannot inflict Maim / Bleeding on you"
          "deflected_hits_cannot_directly_inflict_maim_on_self"; "deflected_hits_cannot_inflict_bleeding_on_self"
          // "Enemies in your Presence have X% reduced Life Regeneration rate / Cooldown Recovery Rate"
          "enemies_in_presence_life_regeneration_+%"; "enemies_in_presence_cooldown_recovery_+%"
          // the player is never Heavily Stunned (RULES.md, "Heavy stun on the player"), and of the
          // damaging ailments only Ignite is ever on it: "increased Attack Damage if you have been
          // Heavy Stunned Recently", "Cannot be Heavy Stunned while Sprinting", "Damaging Ailments
          // Cannot Be inflicted on you while you already have one"
          "attack_damage_+%_if_been_heavy_stunned_recently"; "cannot_be_heavy_stunned_while_sprinting"; "cannot_have_more_than_1_damaging_ailment" ]
        |> List.contains id

    /// What decides how a character can be built, and is all there in the build as it is read:
    /// another ring slot (the ring is among the items), companions beyond the first (they are
    /// among the gems), passives allocated without a path to them (they are among the
    /// passives), more room in the inventory, which beasts can be tamed. And "Gain the
    /// benefits of Bonded modifiers on Runes and Idols": read where the items are (RULES.md,
    /// "Bonded: lines").
    let private howItIsBuilt (id: string) =
        [ "enable_ring_slot_3"; "titan_additional_inventory"; "can_have_2_companions"; "can_have_unlimited_companions"
          "ascendancy_disconnected_passives_can_be_allocated_around_allocated_keystones"; "local_unique_jewel_nearby_disconnected_passives_can_be_allocated"
          "unique_jewel_nearby_disconnected_passives_can_be_allocated"
          "tame_beast_can_target_unique_beasts"; "ascendancy_shaman_gain_additional_socketable_mods" ]
        |> List.contains id

    /// "X% increased Explicit Resistance Modifier magnitudes" and the like on an item: the
    /// item's lines already show the changed values (RULES.md, "Item modifier magnitudes").
    let private shownOnTheItem (id: string) =
        id.StartsWith "heist_enchantment_" && id.EndsWith "_mod_effect_+%"

    let private isNonOffensive (id: string) =
        let words = split id
        // A penetration / enemy debuff line mentions resistance but is offensive.
        words |> List.exists noCombatWords.Contains || ownMovement id || monstersNeverDo id || shownOnTheItem id || howItIsBuilt id

    let rec private direct (id: string) value =
        match explicit id value with
        | Some m -> Some m
        | None ->
            match defensive id value with
            | Some m -> Some m
            | None ->
                match pattern id value with
                | Some m -> Some m
                | None ->
                    // "allies in your presence": your minions (other players aren't simulated)
                    let wrapMinions (m: Meaning) =
                        match m with
                        | Mods ms -> Some(Mods(ms |> List.map (fun x -> match x.Stat with ForMinions _ -> x | st -> { x with Stat = ForMinions st })))
                        | _ -> None
                    // what minions have: "minion_maximum_life_+%", "minion_elemental_resistance_%"
                    let minionOwn =
                        if id.StartsWith "minion_" then
                            let rest = id.Substring 7
                            explicit rest value |> Option.orElse (explicit ("base_" + rest) value) |> Option.orElse (defensive rest value) |> Option.bind wrapMinions
                        else None
                    // "Companions have / deal ...": what minions have, for the minions that are companions
                    let companionOwn =
                        if id.StartsWith "companion_" then
                            match direct ("minion_" + id.Substring 10) value with
                            | Some (Mods ms) -> Some(Mods(ms |> List.map (Mod.whenever (SkillIs(Tag "Companion")))))
                            | _ -> None
                        else None
                    if minionOwn.IsSome then minionOwn
                    elif companionOwn.IsSome then companionOwn
                    elif id.StartsWith "allies_in_presence_" then direct (id.Substring 19) value |> Option.bind wrapMinions
                    elif id.StartsWith "you_and_allies_in_presence_" then
                        match direct (id.Substring 27) value with
                        | Some (Mods ms as own) -> (match wrapMinions own with Some (Mods ws) -> Some(Mods(ms @ ws)) | _ -> Some own)
                        | other -> other
                    else
                    // aura gems: "base_skill_buff_<stat>_to_apply" grants <stat> (Purity of Fire, Discipline)
                    let prefix = "base_skill_buff_"
                    if id.StartsWith prefix && (id.EndsWith "_to_apply" || id.EndsWith "_to_grant") then
                        let inner = id.Substring(prefix.Length, id.Length - prefix.Length - 9)
                        match inner with
                        | "total_maximum_energy_shield_+" -> Some(Mods [ Mod.base' MaxEnergyShield value ])
                        | _ -> direct inner value |> Option.orElse (direct ("base_" + inner) value)
                    else None

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
