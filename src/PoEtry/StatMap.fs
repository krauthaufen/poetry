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

    /// "while <state>" under another name than the one the fight keeps the state by: "while not
    /// on Low Mana", "while you have Arcane Surge", "while Onslaught is active", "while holding a
    /// Shield", "while you have no Energy Shield"
    let private stateAlias (name: string) : Cond option =
        match name with
        | "NotOnLowMana" | "NotLowMana" -> Some(Not(WhileBuff "LowMana"))
        | "YouHaveArcaneSurge" -> Some(WhileBuff "ArcaneSurge")
        | "OnslaughtActive" | "OnslaughtIsActive" -> Some(WhileBuff "Onslaught")
        | "YouUnarmed" -> Some(WhileBuff "Unarmed")
        | "HoldingAShield" | "HoldingShield" -> Some(WhileBuff "WieldingShield")
        | "HoldingBow" -> Some(WhileBuff "WieldingBow")
        | "HoldingStaff" -> Some(WhileBuff "WieldingStaff")
        | "HaveAnyFrenzyCharges" -> Some(Not(WhileBuff "NoFrenzyCharges"))
        | "NoEnergyShield" -> Some(Not(WhileBuff "HasEnergyShield"))
        | _ -> None

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
            // states the character's rules keep where a stat asks for them (Behaviours.recentStates)
            | "if" :: "you" :: "havent" :: "been" :: "hit" :: "recently" :: tail | "if" :: "havent" :: "been" :: "hit" :: "recently" :: tail ->
                go (Not(WhileBuff "BeenHitRecently") :: acc) tail
            | "if" :: "you" :: "have" :: "hit" :: "an" :: "enemy" :: "recently" :: tail -> go (WhileBuff "HitRecently" :: acc) tail
            | "when" :: "not" :: "stunned" :: "recently" :: tail -> go (Not(WhileBuff "StunnedRecently") :: acc) tail
            | "if" :: "you" :: "dodge" :: "rolled" :: "recently" :: tail | "if" :: "dodge" :: "rolled" :: "recently" :: tail ->
                go (WhileBuff "DodgeRolledRecently" :: acc) tail
            | "if" :: "not" :: "dodge" :: "rolled" :: "recently" :: tail -> go (Not(WhileBuff "DodgeRolledRecently") :: acc) tail
            | "if" :: "have" :: "stunned" :: "an" :: "enemy" :: "recently" :: tail | "if" :: "stunned" :: "an" :: "enemy" :: "recently" :: tail ->
                go (WhileBuff "StunnedEnemyRecently" :: acc) tail
            | "if" :: "youve" :: "dealt" :: "melee" :: "hit" :: "recently" :: tail -> go (WhileBuff "MeleeHitInPast8Seconds" :: acc) tail
            | "per" :: "warcry" :: "used" :: "recently" :: tail -> go (PerStack("WarcriesRecently", 1.0) :: acc) tail
            | "per" :: "poison" :: "applied" :: "recently" :: tail -> go (PerStack("PoisonsRecently", 1.0) :: acc) tail
            | "per" :: "enemy" :: "ignited" :: "last" :: "8" :: "seconds" :: tail -> go (PerStack("IgnitedInLast8Seconds", 1.0) :: acc) tail
            | "per" :: "enemy" :: "frozen" :: "last" :: "8" :: "seconds" :: tail -> go (PerStack("FrozenInLast8Seconds", 1.0) :: acc) tail
            | "for" :: "each" :: "time" :: "hit" :: "recently" :: "up" :: "to" :: "100%" :: tail -> go (PerStack("TimesHitRecently", 1.0) :: acc) tail
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
            | ("when" | "while") :: "on" :: "full" :: "energy" :: "shield" :: tail | "on" :: "full" :: "energy" :: "shield" :: tail -> go (WhileBuff "FullEnergyShield" :: acc) tail
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
            | "per" :: n :: "rage" :: tail when n <> "" && n |> Seq.forall System.Char.IsDigit -> go (PerStack("Rage", float n) :: acc) tail
            // Grand Spectrum: "per socketed Grand Spectrum"
            | "per" :: "stackable" :: "unique" :: "jewel" :: tail -> go (PerStack("GrandSpectrum", 1.0) :: acc) tail
            | "with" :: c :: "charges" :: tail when c = "power" || c = "frenzy" || c = "endurance" ->
                go (WhileBuff(titleCase [ c; "charge" ]) :: acc) tail
            | "when" :: "charged" :: tail -> go (WhileBuff "PowerCharge" :: acc) tail
            | "in" :: "weapon" :: "set" :: "one" :: tail -> go (Not(SkillIs(Tag "WeaponSet2")) :: acc) tail
            | "in" :: "weapon" :: "set" :: "two" :: tail -> go (SkillIs(Tag "WeaponSet2") :: acc) tail
            | "while" :: "you" :: "have" :: "energy" :: "shield" :: tail -> go (WhileBuff "HasEnergyShield" :: acc) tail
            | "while" :: state when not state.IsEmpty -> List.rev acc, [], Some(stateAlias (titleCase state) |> Option.defaultValue (WhileBuff(titleCase state)))
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
        // "Lose X% of maximum Mana per Second" (per minute in the id; "not in grace": once the
        // area's grace period is over, as it is in a fight)
        | "mana_degeneration_%_per_minute_not_in_grace" -> one (Mod.base' (Named "ManaDegenPercent") (v / 60.0))
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
        // Oil Ground's exposure and slow: read by Behaviours.oilGround; how it ignites (an Ignite
        // from Ignited Ground or a Detonator) is not modelled
        | "skill_base_oil_exposure_-_to_total_elemental_resistance" | "skill_base_oil_movement_speed_+%_final_to_apply" -> Some Structural
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
        // "X% increased Glory generation": scales what each event builds (Setup.player, Behaviours.banners)
        | "glory_generation_+%" -> one (Mod.inc (Named "GloryGeneration") v)
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
        // "Skills deal X% increased Damage per Combo consumed, up to 40%" (Setup.player: a skill
        // consumes the Combo it requires)
        | "combo_finisher_damage_+%_up_to_40%" -> one (Mod.base' (Named "DamagePerComboConsumed") v)
        // "Skills lose Combo X% faster": the 8 s it lasts without a gain, shorter (Setup.player)
        | "combo_falloff_speed_+%" -> one (Mod.inc (Named "ComboFalloff") v)
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
        // "Deal no Elemental Damage", "Deal no Chaos Damage"
        | "deal_no_elemental_damage" -> Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.flag (Named("DealNo:" + DamageType.name t)))))
        | "base_deal_no_chaos_damage" -> one (Mod.flag (Named "DealNo:Chaos"))
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
        // Fork: "Projectiles Fork" is a chance of 100% to fork once; times more, three in place
        // of two, the angle, and what the forked ones deal
        | "projectiles_fork" -> one (Mod.base' (Named "ForkChance") 100.0)
        | "arrows_fork" -> one (Mod.base' (Named "ForkChance") 100.0 |> Mod.whenever (SkillIs Bow))
        | "attack_projectiles_fork" -> one (Mod.base' (Named "ForkChance") 100.0 |> Mod.whenever (SkillIs Attack))
        | "projectiles_from_spells_fork" -> one (Mod.base' (Named "ForkChance") 100.0 |> Mod.whenever (SkillIs Spell))
        | "projectile_chance_to_fork_%" -> one (Mod.base' (Named "ForkChance") v)
        | "number_of_additional_forks_base" -> one (Mod.base' (Named "AdditionalForks") v)
        | "attack_projectiles_fork_additional_times" -> one (Mod.base' (Named "AdditionalForks") v |> Mod.whenever (SkillIs Attack))
        | "projectiles_cannot_fork" -> one (Mod.flag (Named "CannotFork"))
        // "Projectiles cannot Chain": whatever else gives chains
        | "base_projectiles_cannot_chain" -> one (Mod.flag (Named "CannotChain"))
        // "Projectiles cannot Split": the engine splits no projectile, whatever gives splits, which
        // is just what this asks
        | "projectiles_cannot_split" -> Some Structural
        // "Cannot inflict Blind"
        | "cannot_inflict_blind" -> one (Mod.more (AilmentChance Blind) -100.0)
        | "chance_to_fork_extra_projectile_%" -> one (Mod.base' (Named "ForkExtraChance") v)
        | "forking_angle_+%" -> one (Mod.inc (Named "ForkAngle") v)
        | "support_fork_forked_projectile_damage_+%_final" -> one (Mod.more (Named "ForkedDamage") v)
        // Twister: "Consumes Whirlwinds to create an additional twister and deal X% more damage
        // per Whirlwind stage" (the Whirlwind's stages are stacks, see Behaviours.whirlwind);
        // "X% chance for an additional twister"; "Elemental twisters Gain X% of damage as damage
        // of the corresponding Type" (a twister boosted by an element, see Setup)
        | "twister_damage_+%_final_per_whirling_slash_stage" ->
            Some(Mods [ Mod.more (Damage AllDamage) v |> Mod.whenever (PerStack("Whirlwind", 1.0)); Mod.base' AdditionalProjectiles 1.0 |> Mod.whenever (WhileBuff "Whirlwind") ])
        | "twister_%_chance_for_additional_twister" -> one (Mod.base' (Named "ExtraProjectileChance1") v)
        | "twister_gain_%_elemental_damage_of_corresponding_type" ->
            Some(Mods [ for e in DamageType.elemental do
                            for t in DamageType.all -> Mod.base' (GainAs(t, e)) v |> Mod.whenever (WhileBuff("BoostedBy:" + DamageType.name e)) ])
        // "Wind Skills which can be boosted by Elemental Ground Surfaces count as being boosted by
        // Chilled / Ignited / Shocked Ground", "... can be boosted by multiple ..."
        | "wind_skills_count_as_empowered_by_chilled_ground" -> one (Mod.flag (Named "WindBoost:Cold"))
        | "wind_skills_count_as_empowered_by_ignited_ground" -> one (Mod.flag (Named "WindBoost:Fire"))
        | "wind_skills_count_as_empowered_by_shocked_ground" -> one (Mod.flag (Named "WindBoost:Lightning"))
        | "wind_skills_can_be_empowered_by_multiple_elements" -> one (Mod.flag (Named "WindBoostMultiple"))
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
        // what the monsters one blinds or marks do to the character with their hits
        | "enemies_you_blind_have_critical_strike_chance_+%" -> one (Mod.inc (Named "BlindedEnemyCritChance") v)
        | "marked_enemy_accuracy_rating_+%" -> one (Mod.inc (Named "MarkedEnemyAccuracy") v)
        | "defend_with_%_armour_against_ranged_attacks" -> one (Mod.base' (Named "DefendWithArmourVsProjectiles") v)
        | "defend_with_%_armour_against_critical_strikes" -> one (Mod.base' (Named "DefendWithArmourVsCrits") v)
        // "Defend with X% of Armour while not on Low Energy Shield", "... while on Low Energy Shield",
        // "... while you have Energy Shield": against every hit, while it holds
        | "defend_with_%_of_armour_while_not_on_low_energy_shield" -> one (Mod.base' (Named "DefendWithArmour") v |> Mod.whenever (Not(WhileBuff "LowEnergyShield")))
        | "defend_with_%_armour_when_low_energy_shield" -> one (Mod.base' (Named "DefendWithArmour") v |> Mod.whenever (WhileBuff "LowEnergyShield"))
        | "defend_with_%_armour_while_you_have_energy_shield" -> one (Mod.base' (Named "DefendWithArmour") v |> Mod.whenever (WhileBuff "HasEnergyShield"))
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
        // Spell damage for Spirit: "increased Spell Damage while on full Energy Shield while a Supported Skill is active"
        | "support_spell_damage_spirit_cost_spell_damage_+%_on_full_energy_shield" ->
            one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (WhileBuff "FullEnergyShield"))
        | "spirit_+_per_2_levels" -> one (Mod.base' (Named "SpiritPerTwoLevels") v)
        | "base_spirit_per_socketed_idol" -> one (Mod.base' (Named "Spirit") v |> Mod.whenever (PerStack("SocketedIdols", 1.0)))
        | "ascendancy_shaman_+_spirit_per_empty_charm_slot" -> one (Mod.base' (Named "SpiritPerEmptyCharmSlot") v)
        | "unique_prism_guardian_spirit_+_per_X_maximum_life" -> one (Mod.base' (Named "SpiritPerLife") v)
        | "cannot_gain_spirit_from_equipment" -> one (Mod.flag (Named "NoSpiritFromEquipment"))
        | "spirit_does_not_exist" -> one (Mod.flag (Named "NoSpirit"))
        | "base_spirit_reservation_efficiency_+%" | "base_reservation_efficiency_+%" -> one (Mod.inc (Named "ReservationEfficiency") v)
        | "reservation_efficiency_+%_of_meta_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Meta")))
        | "reservation_efficiency_+%_of_herald_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Herald")))
        | "reservation_efficiency_+%_of_minion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Minion")))
        | "reservation_efficiency_+%_of_companion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Companion")))
        | "reservation_efficiency_+%_of_non_minion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (Not(SkillIs(Tag "Type:Minion"))))
        | "reservation_efficiency_+%_of_remnant_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:GeneratesRemnants")))
        | "reservation_efficiency_+%_of_skeleton_minion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:CreatesSkeletonMinion")))
        | "reservation_efficiency_+%_of_undead_minion_skills" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:CreatesUndeadMinion")))
        | "reservation_efficiency_+%_of_skills_per_socketed_idol" -> one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (PerStack("SocketedIdols", 1.0)))
        | "buff_skills_spirit_reservation_efficiency_+%_per_100_maximum_life" ->
            one (Mod.inc (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Buff")) |> Mod.whenever (PerStack("MaxLife100", 1.0)))
        // "increased Reservation": the skill costs so much more Spirit
        | "base_reservation_+%" -> one (Mod.inc (Named "Reservation") v)
        | "minion_reservation_+%" -> one (Mod.inc (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:Minion")))
        | "companion_reservation_+%" -> one (Mod.inc (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:Companion")))
        | "demon_minion_reservation_+%" -> one (Mod.inc (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:CreatesDemonMinion")))
        | "skeleton_minion_reservation_+%" -> one (Mod.inc (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:CreatesSkeletonMinion")))
        | "undead_minion_reservation_+%" -> one (Mod.inc (Named "Reservation") v |> Mod.whenever (SkillIs(Tag "Type:CreatesUndeadMinion")))
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
        // a state of the fight's (Sim.step: the recharge delay since the last hit is over, by
        // no more than 2 s)
        | "evasion_rating_+%_if_energy_shield_recharge_started_in_past_2_seconds" -> one (Mod.inc Evasion v |> Mod.whenever (WhileBuff "ESRechargeStartedRecently"))
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
        // "+X% to Maximum Fire Resistance while Ignited": while the player is
        | "base_maximum_fire_damage_resistance_%_while_ignited" -> one (Mod.base' (MaxResist Fire) v |> Mod.whenever (WhileBuff "Ignited"))
        // "Stun and Ailment Threshold X% increased while Surrounded"
        | "stun_and_ailment_threshold_+%_while_surrounded" ->
            Some(Mods [ Mod.inc (Named "StunThreshold") v |> Mod.whenever (WhileBuff "Surrounded"); Mod.inc (Named "PlayerAilmentThreshold") v |> Mod.whenever (WhileBuff "Surrounded") ])
        | "base_chaos_damage_resistance_%" -> one (Mod.base' (Resist Chaos) v)
        | "life_regeneration_rate_per_minute_%" -> one (Mod.base' LifeRegenPercent (v / 60.0))
        | "base_life_regeneration_rate_per_minute" -> one (Mod.base' LifeRegen (v / 60.0))
        | "life_regeneration_rate_+%" -> one (Mod.inc LifeRegen v)
        // "while stationary": the player stands (RULES.md). (The pools' recovery is worked out
        // once, for the standing character: one that is taken to move, because it gains from
        // moving, keeps these all the same.)
        // the player stands: what holds while moving does not (the pools' recovery is worked out
        // for the standing character; RULES.md, "While stationary")
        | "mana_regeneration_rate_+%_while_moving" -> Some NotOffensive
        // recovery that holds in some state of the player: read at the moment (RULES.md, "Recovery of the moment")
        | "life_regeneration_rate_per_minute_%_while_affected_by_damaging_ailment" -> one (Mod.base' LifeRegenPercent (v / 60.0) |> Mod.whenever (WhileBuff "DamagingAilment"))
        | "life_regeneration_rate_per_minute_%_when_on_low_life" -> one (Mod.base' LifeRegenPercent (v / 60.0) |> Mod.whenever (WhileBuff "LowLife"))
        | "life_regeneration_rate_per_minute_%_while_surrounded" -> one (Mod.base' LifeRegenPercent (v / 60.0) |> Mod.whenever (WhileBuff "Surrounded"))
        | "life_regeneration_rate_per_minute_%_if_have_been_hit_recently" -> one (Mod.base' LifeRegenPercent (v / 60.0) |> Mod.whenever (WhileBuff "BeenHitRecently"))
        | "life_regeneration_rate_+%_while_on_low_life" -> one (Mod.inc LifeRegen v |> Mod.whenever (WhileBuff "LowLife"))
        | "life_regeneration_rate_+%_while_shapeshifted" -> one (Mod.inc LifeRegen v |> Mod.whenever (WhileBuff "Shapeshifted"))
        | "life_regeneration_rate_+%_while_using_life_flask" -> one (Mod.inc LifeRegen v |> Mod.whenever (WhileBuff "LifeFlaskEffect"))
        | "mana_regeneration_rate_+%_on_full_life" -> one (Mod.inc ManaRegen v |> Mod.whenever (WhileBuff "FullLife"))
        | "djinn_ascendancy_energy_shield_recharge_rate_+%_final_on_low_energy_shield" -> one (Mod.more (Named "ESRechargeRate") v |> Mod.whenever (WhileBuff "LowEnergyShield"))
        | "energy_shield_delay_-%_when_not_on_full_life" -> one (Mod.inc (Named "ESRechargeFaster") v |> Mod.whenever (Not(WhileBuff "FullLife")))
        | "energy_shield_delay_-%_if_stunned_recently" -> one (Mod.inc (Named "ESRechargeFaster") v |> Mod.whenever (WhileBuff "StunnedRecently"))
        // "Regenerate X% of maximum Life over 1 second when Stunned"
        | "regenerate_%_life_over_1_second_when_stunned" -> one (Mod.base' (Named "RegenLifeOnStun") v)
        // "Arcane Surge grants more Life Regeneration Rate instead of Mana Regeneration Rate"
        | "arcane_surge_mana_regen_modifier_on_you_applies_to_life_regen_instead" -> one (Mod.flag (Named "ArcaneSurgeRegenToLife"))
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
        | "recover_%_life_per_endurance_charge_consumed" -> one (Mod.base' (Named "LifePercentPerEnduranceConsumed") v)
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
        // "X% of base maximum Life to gain as total Energy Shield" (the monsters' and rare monster
        // modifiers' stat, no description of its own): of the life before it is increased (Defence)
        | "base_maximum_life_%_to_gain_as_total_energy_shield" -> one (Mod.base' (Named "BaseLifeGainedAsES") v)
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
        | "deflection_rating_+%_while_surrounded" -> one (Mod.inc Deflection v |> Mod.whenever (WhileBuff "Surrounded"))
        | "additional_block_chance_against_projectiles_%" -> one (Mod.base' (Named "BlockVsProjectiles") v)
        | "base_self_critical_strike_multiplier_-%" -> one (Mod.base' (Named "LessCritDamageTaken") v)
        // ---- thresholds, stuns and ailments on yourself ----
        | "stun_threshold_+" -> one (Mod.base' (Named "StunThreshold") v)
        | "stun_threshold_+%" -> one (Mod.inc (Named "StunThreshold") v)
        // "Every Rage also grants X% increased Stun Threshold"
        | "stun_threshold_+%_per_rage" -> one (Mod.inc (Named "StunThreshold") v |> Mod.whenever (PerStack("Rage", 1.0)))
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
        // "Debuffs you inflict have X% increased Slow Magnitude": the Maim and Hinder the
        // character's hits leave slow the monsters that much more (Sim.Env.SlowMagnitude)
        | "base_debuff_slow_magnitude_+%" -> one (Mod.inc (Named "SlowMagnitude") v)
        // "Life Recovery from Flasks also applies to Energy Shield" (PlayerStats.LifeFlaskAlsoES)
        | "life_recovery_from_flasks_also_recovers_energy_shield" -> one (Mod.flag (Named "LifeFlaskAlsoES"))
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
              // Gems (Twister: what it consumes, that it bounces off walls), Behaviours.whirlwind
              "twister_consumes_wind_storm"; "twister_bounce_check_angle"; "sandstorm_swipe_max_stages"
              // Gems: an attack's own damage in place of the weapon's (a herald's explosion)
              "main_hand_weapon_minimum_physical_damage"; "main_hand_weapon_maximum_physical_damage"
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
        // Combat Frenzy's time between charges: read by Behaviours.combatFrenzy
        | "skill_combat_frenzy_x_ms_cooldown" -> Some Structural
        // Tame Beast's own use: hindering and capturing a beast, and which monsters it can see.
        // A companion gem is that gem with the beast already captured, and the capture is never
        // used in a fight
        | "tame_beast_hinder_%" | "skill_can_see_monster_categories" -> Some Structural
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
        // minions' part (the player's own is "grant_unholy_might_to_self_while_not_on_low_mana"),
        // while the player is not on low mana
        | "allies_in_presence_have_unholy_might_while_you_not_on_low_mana" ->
            Some(Mods [ for t in DamageType.all -> Mod.base' (ForMinions(GainAs(t, Chaos))) 30.0 |> Mod.whenever (OfOwner(Not(WhileBuff "LowMana"))) ])
        // what minions have while the player is in some state ("if you've Hit Recently", "while
        // you are affected by a Herald", "while you are not on full Mana"): read off the player's
        // buffs and states (`OfOwner`)
        // "Increases and Reductions to Minion Damage / Companion Damage / Minion Attack Speed also
        // affect you" (Behaviours.minionIncreasesOnYou)
        | "minion_damage_increases_and_reductions_also_affects_you" -> one (Mod.flag (Named "MinionDamageAffectsYou"))
        | "companion_damage_increases_and_reductions_also_affects_you" -> one (Mod.flag (Named "CompanionDamageAffectsYou"))
        | "additive_modifiers_to_minion_attack_speed_also_affect_you" -> one (Mod.flag (Named "MinionAttackSpeedAffectsYou"))
        // "X% of Physical Damage prevented Recouped as Life": of a monster's hit
        | "physical_damage_prevented_recouped_as_life_%" -> one (Mod.base' (Named "RecoupPreventedPhysicalAsLife") v)
        // "Companions deal X% increased damage per Idol in your Equipment", "+X% to all Elemental
        // Resistances per non-Idol Augment in your Equipment": counted when the character is read
        | "companion_damage_+%_per_socketed_idol" ->
            one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (SkillIs(Tag "Companion")) |> Mod.whenever (OfOwner(PerStack("SocketedIdols", 1.0))))
        | "resist_all_elements_%_per_socketed_non_idol_augment" -> Some(Mods(Mod.resistAllElemental v |> List.map (Mod.whenever (PerStack("NonIdolAugments", 1.0)))))
        // "X% increased Surrounded Area of Effect": the area enemies surround the player in
        | "surrounded_area_of_effect_+%" -> one (Mod.inc (Named "SurroundedArea") v)
        // "X% increased Attack and Cast Speed if you've summoned a Totem Recently" (Behaviours.totemRecently)
        | "attack_and_cast_speed_+%_on_placing_totem" -> one (Mod.inc Speed v |> Mod.whenever (WhileBuff "SummonedTotemRecently"))
        // grenades: "X% increased Cooldown Recovery Rate for Grenade Skills", "+1 Cooldown Use",
        // "X% increased Grenade fuse duration" (Gems.fuse)
        | "grenade_skill_cooldown_speed_+%" -> one (Mod.inc CooldownRecovery v |> Mod.whenever (SkillIs(Tag "Grenade")))
        | "grenade_skill_cooldown_count_+" -> one (Mod.base' (Named "ExtraCooldownUses") v |> Mod.whenever (SkillIs(Tag "Grenade")))
        | "grenade_fuse_duration_+%" -> one (Mod.inc (Named "GrenadeFuse") v)
        // Rageforged: "Supported Skills spend X Rage if possible to deal Y% more Damage", "Z% chance
        // to gain the benefits of this Support without spending additional Rage" (Sim.startUse)
        | "support_rageforged_consumes_X_rage_to_enrage_skill_use" -> one (Mod.base' (Named "RageforgedSpend") v)
        | "support_rageforged_enraged_damage_+%_final" -> one (Mod.base' (Named "RageforgedMore") v)
        | "rage_skip_consume_chance_%" -> one (Mod.base' (Named "RageSkipChance") v)
        | "minion_damage_+%_if_enemy_hit_recently" -> one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (OfOwner(WhileBuff "HitRecently")))
        | "minion_damage_+%_while_affected_by_a_herald" -> one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (OfOwner(WhileBuff "AffectedByAHerald")))
        | "minion_attack_and_cast_speed_+%_while_you_are_affected_by_a_herald" -> one (Mod.inc (ForMinions Speed) v |> Mod.whenever (OfOwner(WhileBuff "AffectedByAHerald")))
        | "support_minion_damage_spirit_cost_minion_damage_+%_while_missing_mana" -> one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (OfOwner(Not(WhileBuff "FullMana"))))
        | "minion_damage_+%_per_5_dex" -> one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (OfOwner(PerStack("Dex", 5.0))))
        | "minion_critical_strike_multiplier_+_per_stackable_unique_jewel" -> one (Mod.base' (ForMinions CritBonus) v |> Mod.whenever (OfOwner(PerStack("GrandSpectrum", 1.0))))
        // "Companions in your Presence deal X% increased Damage / have Onslaught while you are
        // Shapeshifted" (Onslaught: its 20% increased skill speed, not changed by the player's
        // Onslaught effect)
        | "companions_in_presence_damage_+%_while_you_are_shapeshifted" ->
            one (Mod.inc (ForMinions(Damage AllDamage)) v |> Mod.whenever (SkillIs(Tag "Companion")) |> Mod.whenever (OfOwner(WhileBuff "Shapeshifted")))
        | "companions_in_presence_have_onslaught_while_you_are_shapeshifted" ->
            one (Mod.inc (ForMinions Speed) Constants.onslaughtSpeed |> Mod.whenever (SkillIs(Tag "Companion")) |> Mod.whenever (OfOwner(WhileBuff "Shapeshifted")) |> Mod.from "Onslaught")
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
        // "Supported Skills have X% more Skill Effect Duration" (Limit)
        | "support_limit_skill_effect_duration_+%_final" -> one (Mod.more SkillDuration v)
        // "X% more Duration of Supported Skills and Damaging Ailments they inflict" (Swift Affliction)
        | "support_swift_affliction_skill_effect_and_damaging_ailment_duration_+%_final" ->
            Some(Mods(Mod.more SkillDuration v :: (Ailment.damaging |> List.map (fun a -> Mod.more (AilmentDuration(Some a)) v))))
        // "X% increased Duration of Ignite, Shock and Chill on Enemies"
        | "ignite_shock_chill_duration_+%" -> Some(Mods([ Ignite; Shock; Chill ] |> List.map (fun a -> Mod.inc (AilmentDuration(Some a)) v)))
        // "X% increased Duration of Elemental Ailments on Enemies": Ignite, Chill, Freeze, Shock, Electrocute
        | "base_elemental_status_ailment_duration_+%" ->
            Some(Mods([ Ignite; Chill; Freeze; Shock; Electrocute ] |> List.map (fun a -> Mod.inc (AilmentDuration(Some a)) v)))
        // "X% increased Flask Effect Duration": a life or mana flask's recovery takes longer, the amount the same
        | "flask_duration_+%" -> one (Mod.inc (Named "FlaskDuration") v)
        // "Banner Skills have X% increased Duration"
        | "banner_duration_+%" -> one (Mod.inc (Named "BannerDuration") v)
        // "Empowered Flame Breath Gains X% of damage as Lightning damage": empowered by the Power
        // Charge the use consumes, which goes when its hits are done; a use with a charge is one
        | "wyvern_devour_empowered_flame_breath_base_all_damage_%_to_gain_as_lightning" ->
            Some(Mods(DamageType.all |> List.map (fun t -> Mod.base' (GainAs(t, Lightning)) v |> Mod.whenever (WhileBuff "PowerCharge"))))
        // "X% increased Minion Duration": of temporary minions
        | "base_minion_duration_+%" -> one (Mod.inc (Named "MinionDuration") v)
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
        // "Hits always Ignite", "Hits always Shock": a chance no less can bring under 100%
        | "always_ignite" -> one (Mod.base' (AilmentChance Ignite) 1e6)
        | "always_shock" -> one (Mod.base' (AilmentChance Shock) 1e6)
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
        // Affinity: "Gain Power Charges instead of Frenzy Charges, Frenzy Charges instead of
        // Endurance Charges, Endurance Charges instead of Power Charges" (Behaviours.affinity)
        | "keystone_charge_cycle" -> one (Mod.flag (Named "ChargeCycle"))
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
        // "Gloves you equip have their Base Type transformed to Fists of Stone ... Explicit Modifiers
        // transformed": the gloves the API sends are the transformed ones, read as they are shown
        | "ascendancy_hand_wraps" -> Some Structural
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
        // Wind Dancer: more evasion per stage held (Behaviours.windDancer: stages build up, a hit
        // on the player takes them)
        | "wind_dancer_evasion_rating_+%_final_per_stage" -> one (Mod.more Evasion v |> Mod.whenever (PerStack("WindDancerStage", 1.0)))
        // (read where Gale Force is built: Behaviours.windDancer)
        | "wind_dancer_stages_gained_every_x_ms" | "wind_dancer_maximum_number_of_stages" | "chance_to_trigger_wind_dancer_on_taken_hit_%"
        | "wind_dancer_damage_+%_final_per_stage" | "wind_dancer_area_of_effect_+%_final_per_stage" -> Some Structural
        | "wind_dancer_knockback_+%_final_per_stage" -> one (Mod.more (Named "KnockbackDistance") v |> Mod.whenever (PerStack("WindDancerStage", 1.0)))
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
        // Conduit: "If you would gain a Charge, Allies in your Presence gain that Charge instead"
        // (and the same for one kind of charge); what the allies hold, nothing a minion does reads
        | "keystone_conduit" -> Some(Mods([ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ] |> List.map (fun c -> Mod.flag (Named("Conduit:" + c)))))
        | "power_only_conduit" | "generate_power_charges_for_allies_in_your_presence" -> one (Mod.flag (Named "Conduit:PowerCharge"))
        | "frenzy_only_conduit" | "generate_frenzy_charges_for_allies_in_your_presence" -> one (Mod.flag (Named "Conduit:FrenzyCharge"))
        | "endurance_only_conduit" | "generate_endurance_charges_for_allies_in_your_presence" -> one (Mod.flag (Named "Conduit:EnduranceCharge"))
        | "minions_get_amulet_stats_instead_of_you" ->
            Some(Unsimulated "minions / allies")
        | "keystone_bulwark" -> Some(Unsimulated "movement")
        // "When taking damage from Hits, 20% of Life Loss is prevented, then 150% of Life Loss
        // prevented this way is Lost over 4 seconds"; "X% of Life Loss from Hits is prevented,
        // then that much Life is lost over 4 seconds instead"
        | "keystone_amanamus_defiance" -> Some(Mods [ Mod.base' (Named "DeferLifeLossPrevent") 20.0; Mod.base' (Named "DeferLifeLossLose") 30.0 ])
        | "lose_%_of_life_loss_over_4_seconds_instead" -> Some(Mods [ Mod.base' (Named "DeferLifeLossPrevent") v; Mod.base' (Named "DeferLifeLossLose") v ])
        // "X% chance on Shocking Enemies to create Shocked Ground" (Behaviours.shockedGround)
        | "chance_%_to_create_shocking_ground_on_shock" -> one (Mod.base' (Named "ShockedGroundOnShock") v)
        // Herald of Blood's explosion: "partially based on their accumulated Blood Loss", "has a
        // chance to Aggravate Bleeding" (the explosion itself is played, Behaviours.heralds)
        | "skill_herald_bleeding_enemies_explode_for_%_blood_loss_as_unscalable_physical_damage" -> Some(Unsimulated "blood loss")
        | "skill_herald_of_blood_explosion_aggravate_bleeding_chance_%" -> Some(Unsimulated "aggravating bleeding")
        // Herald of Plague: "killing a Poisoned enemy spreads its Poison to other nearby enemies,
        // and has a chance to also Hinder them"
        | "herald_of_agony_poison_spread_on_kill_radius_base_magnitude_to_grant" | "hinder_chance_%_on_spreading_poioson" -> Some Structural
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
        // "increased Damage with Hits against Enemies affected by Ailments"
        | "hit_damage_+%_vs_enemies_affected_by_ailments" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (EnemyHas "AnyAilment"))
        // "increased Attack and Cast Speed with Elemental Skills": a skill of any of the three elements, once
        | "attack_and_cast_speed_+%_with_elemental_skills" ->
            let tag t = SkillIs(Tag t)
            Some(Mods [ Mod.inc Speed v |> Mod.whenever (tag "Fire")
                        Mod.inc Speed v |> Mod.whenever (tag "Cold") |> Mod.whenever (Not(tag "Fire"))
                        Mod.inc Speed v |> Mod.whenever (tag "Lightning") |> Mod.whenever (Not(tag "Fire")) |> Mod.whenever (Not(tag "Cold")) ])
        | "attack_speed_+%_per_20_dex" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (PerStack("Dex", 20.0)))
        // "increased Damage against Heavy Stunned enemies" (the id says melee, the game's text does not)
        | "melee_damage_+%_vs_heavy_stunned_enemies" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (EnemyHas "HeavilyStunned"))
        | "bleed_on_crit_%" -> one (Mod.base' (AilmentChance Bleed) v |> Mod.whenever IsCrit)
        // "Bleeding you inflict deals Damage X% faster"
        | "faster_bleed_%" -> one (Mod.inc (Named "FasterBleed") v)
        | "fire_spell_additional_critical_strike_chance_permyriad" ->
            one (Mod.base' CritChance (v / 100.0) |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag "Fire")))
        // "increased Magnitude of Poison on targets that are not Poisoned"
        | "poison_effect_+%_vs_non_poisoned_enemies" -> one (Mod.inc (AilmentMagnitude(Some Poison)) v |> Mod.whenever (Not(EnemyHas "Poisoned")))
        // what the player cannot suffer: of these only its own Ignite is ever on it
        | "immune_to_poison" -> one (Mod.base' (Named "Avoid:Poison") 100.0)
        | "base_cannot_be_electrocuted" -> one (Mod.base' (Named "Avoid:Electrocute") 100.0)
        | "unaffected_by_damaging_ailments" -> one (Mod.more (Named "SelfEffect:Ignite") -100.0)
        // "Lose X% of maximum Life and Energy Shield when you use a Chaos Skill"
        | "lose_%_of_life_and_energy_shield_when_you_use_a_chaos_skill" -> one (Mod.base' (Named "LoseLifeAndESOnUse") v |> Mod.whenever (SkillIs(Tag "Chaos")))
        // "Onslaught", "You have Arcane Surge": the effect all the time (Behaviours.keywordMods)
        | "base_should_have_onslaught_from_stat" -> one (Mod.flag (Named "Has:Onslaught"))
        // ---- what gems say of their own skill, known by what it is ----
        | "active_skill_poison_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Poison)) v)
        | "support_stronger_ignites_ignite_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Ignite)) v)
        | "support_deep_cuts_bleeding_effect_+%_final" -> one (Mod.more (AilmentMagnitude(Some Bleed)) v)
        | "active_skill_damaging_ailment_effect_+%_final" -> Some(Mods([ Ignite; Bleed; Poison ] |> List.map (fun a -> Mod.more (AilmentMagnitude(Some a)) v)))
        | "support_hypothermia_hit_damage_freeze_multiplier_+%_final" -> one (Mod.more (Buildup Freeze) v)
        // "Chills Enemies as though dealing X% more damage": a Chill's magnitude is its damage's share of the threshold
        | "active_skill_chill_as_though_damage_+%_final" -> one (Mod.more (AilmentMagnitude(Some Chill)) v)
        | "support_additional_accurary_rating_+%_final" -> one (Mod.more (Named "Accuracy") v)
        | "support_astral_projection_aoe_+%_final" -> one (Mod.more AreaOfEffect v)
        | "support_biting_frost_damage_+%_final_vs_frozen_unique_enemies" ->
            one (Mod.more (Damage AllDamage) v |> Mod.whenever (EnemyHas "Frozen") |> Mod.whenever (EnemyHas "Unique"))
        // "more damage with Hits for explosions / Shockwaves originating from a Unique enemy": the
        // enemy hit is taken for the one it starts from (with one enemy it is)
        | "snipe_damage_+%_final_if_created_from_unique" | "frag_rounds_damage_+%_final_if_created_from_unique" | "boneshatter_damage_+%_final_if_created_from_unique" ->
            one (Mod.more (Damage AllDamage) v |> Mod.whenever (EnemyHas "Unique"))
        | "active_skill_damage_+%_final_vs_fully_broken_armour" -> one (Mod.more (Damage AllDamage) v |> Mod.whenever (EnemyHas "FullyBrokenArmour"))
        | "attack_damage_is_lucky_if_surrounded" ->
            one (Mod.base' (Named "LuckyDamageChance") 100.0 |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "Surrounded"))
        | "gain_x_rage_on_attack_hit" -> one (Mod.base' (Named "RageOnHit") v |> Mod.whenever (SkillIs Attack))
        | "chaos_damage_%_dealt_as_armour_break" -> one (Mod.base' (Named "ArmourBreakOfChaos") v)
        // Retreating / Advancing Assault: more damage after a hit of the other kind (Behaviours.recentStates)
        | "support_retreating_assault_projectile_damage_+%_final_if_melee_hit_in_past_8_seconds" ->
            one (Mod.more (Damage AllDamage) v |> Mod.whenever (SkillIs Projectile) |> Mod.whenever (WhileBuff "MeleeHitInPast8Seconds"))
        | "support_advancing_assault_melee_damage_+%_final_if_projectile_attack_damage_hit_in_past_8_seconds" ->
            one (Mod.more (Damage AllDamage) v |> Mod.whenever (SkillIs Melee) |> Mod.whenever (WhileBuff "ProjectileAttackHitInPast8Seconds"))
        // Blazing Critical's buff on a critical hit (read where it is built: Behaviours.iceBite)
        | "support_blazing_crits_gain_%_fire_damage_with_attacks_on_critical_hit" | "support_blazing_crits_base_duration_ms" -> Some Structural
        // a detonation after a time: read where a skill's hit waits for it (Gems: grenades, Frost
        // Bomb; Behaviours.toxicDomain), elsewhere not played
        | "base_skill_detonation_time" | "support_fast_forward_detonation_time_+%_final" | "skill_detonation_time_+%" -> Some(Unsimulated "detonation time")
        // how a skill is aimed, queued and animated, and markers the game keeps for its own
        // texts: a fight that plays uses by their times and hits by their places has nothing
        // these could change
        | "channel_start_lock_cancelling_scales_with_cast_speed" | "channel_start_lock_cancelling_scales_with_attack_speed"
        | "active_skill_generic_grace_period_time_ms" | "action_requires_upward_aiming_stance" | "skill_has_trigger_from_unique_item"
        | "skill_using_no_fear" | "supported_by_greatwood" | "disangage_autotargeting_angle_%" | "skill_moving_start_slowdown"
        | "console_cast_on_self_custom_range" | "hide_minion_frame" | "active_skill_facing_angle_turn_duration_ms" | "override_turn_duration_ms"
        | "disable_quadruped_head_movement_when_turning" | "firewall_attached_projectile_effect_mtx" | "skill_can_be_queued_on_cooldown"
        | "leap_slam_animation_distance_threshold" | "armour_broken_skill_event_variation" -> Some Structural
        // the player's own leap or dash (RULES.md, "The player stands"): how far it may go
        | "leap_maximum_allowed_path_distance" | "melee_dash_placement_added_range" -> Some NotOffensive
        // "+X seconds to base use time when Shapeshifting": a character that uses shapeshift
        // skills is in its animal form for the whole fight and never changes into it
        | "use_time_+_ms_if_shapeshift_required" -> Some NotOffensive
        // "per 100 maximum Life": the character's maximum life as stacks (Setup.loadWith)
        | "spell_damage_+%_per_100_max_life_with_non_channelling_skills" ->
            one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxLife100", 1.0)))
        | "spell_critical_strike_chance_+%_per_100_max_life_with_non_channelling_skills" ->
            one (Mod.inc CritChance v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxLife100", 1.0)))
        | "spell_ailment_magnitude_+%_per_100_max_life_with_non_channelling_skills" ->
            one (Mod.inc (AilmentMagnitude None) v |> Mod.whenever (SkillIs Spell) |> Mod.whenever (Not(SkillIs Channelling)) |> Mod.whenever (PerStack("MaxLife100", 1.0)))
        // "Attacks have added Physical damage equal to X% of maximum Life": X for every full 100 life
        | "attack_added_physical_damage_%_of_maximum_life" ->
            Some(Mods [ for st in [ AddedMin Physical; AddedMax Physical ] -> Mod.base' st v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (PerStack("MaxLife100", 1.0)) ])
        | "gain_armour_equal_to_strength" -> one (Mod.base' Armour 1.0 |> Mod.whenever (PerStack("Str", 1.0)))
        // durations of a kind of skill
        | "plant_skill_effect_duration_+%" -> one (Mod.inc SkillDuration v |> Mod.whenever (SkillIs(Tag "Plant")))
        | "offering_duration_+%" -> one (Mod.inc SkillDuration v |> Mod.whenever (SkillIs(Tag "Type:Offering")))
        | "totem_duration_+%" -> one (Mod.inc SkillDuration v |> Mod.whenever (SkillIs(Tag "Type:SummonsTotem")))
        | "projectile_ailment_chance_+%" -> Some(Mods(allAilments |> List.map (fun a -> Mod.inc (AilmentChance a) v |> Mod.whenever (SkillIs Projectile))))
        | "ailment_duration_+%_vs_cursed_enemy" -> one (Mod.inc (AilmentDuration None) v |> Mod.whenever (EnemyHas "Cursed"))
        | "energy_shield_gained_on_block" -> one (Mod.base' (Named "ESOnBlock") v)
        // damage the character takes
        | "marauder_hidden_ascendancy_damage_taken_+%_final" -> one (Mod.more (Named "Taken:All") v)
        | "hand_wraps_damage_taken_+%_final_on_low_life" -> one (Mod.more (Named "Taken:All") v |> Mod.whenever (WhileBuff "LowLife"))
        | "ascendancy_shaman_reactive_growth_elemental_damage_taken_+%_final" ->
            Some(Mods(DamageType.elemental |> List.map (fun t -> Mod.more (Named("Taken:" + DamageType.name t)) v)))
        | "armour_and_evasion_on_low_life_+%" -> Some(Mods [ Mod.inc Armour v |> Mod.whenever (WhileBuff "LowLife"); Mod.inc Evasion v |> Mod.whenever (WhileBuff "LowLife") ])
        | "fire_damage_resistance_%_when_on_low_life" -> one (Mod.base' (Resist Fire) v |> Mod.whenever (WhileBuff "LowLife"))
        | "block_chance_+%_while_surrounded" -> one (Mod.inc (Named "Block") v |> Mod.whenever (WhileBuff "Surrounded"))
        // "X% of your Base Life Regeneration is granted to Allies in your Presence" (Behaviours.minions)
        | "total_base_life_regeneration_rate_per_minute_%_granted_to_allies_in_your_presence" -> one (Mod.base' (Named "AllyRegenPercentOfOwnerRegen") v)
        // totems: more of them, and what each has for every one standing (Behaviours.totems)
        | "keystone_ancestral_bond" -> one (Mod.flag (Named "AncestralBond"))
        | "number_of_additional_totems_allowed" -> one (Mod.base' (Named "AdditionalTotems") v)
        | "attack_skills_additional_ballista_totems_allowed" | "additional_ballista_totems_allowed" -> one (Mod.base' (Named "AdditionalBallistaTotems") v)
        | "totems_spells_cast_speed_+%_per_active_totem" -> one (Mod.base' (Named "TotemCastSpeedPerTotem") v)
        | "totems_attack_speed_+%_per_active_totem" -> one (Mod.base' (Named "TotemAttackSpeedPerTotem") v)
        | "totem_damage_+%_final_per_active_totem" -> one (Mod.base' (Named "TotemDamagePerTotem") v)
        // a shield skill's damage from the shield's armour or evasion (read where it is built: Gems.skill)
        | id when (id.StartsWith "off_hand_minimum_added_physical_damage_per_" || id.StartsWith "off_hand_maximum_added_physical_damage_per_") && id.Contains "_shield_" -> Some Structural
        // "X% increased total Power counted by Warcries" (Behaviours.warcry)
        | "warcry_monster_power_+%" -> one (Mod.inc (Named "WarcryPower") v)
        // "if you have at least 5 Red Support Gems Socketed" (the counts: Setup.loadWith)
        | "maximum_fire_resistance_+%_if_at_least_5_red_supports_socketed" -> one (Mod.base' (MaxResist Fire) v |> Mod.whenever (WhileBuff "Supports5:red"))
        | "maximum_cold_resistance_+%_if_at_least_5_blue_supports_socketed" -> one (Mod.base' (MaxResist Cold) v |> Mod.whenever (WhileBuff "Supports5:blue"))
        | "maximum_lightning_resistance_+%_if_at_least_5_green_supports_socketed" -> one (Mod.base' (MaxResist Lightning) v |> Mod.whenever (WhileBuff "Supports5:green"))
        | "additional_maximum_all_elemental_resistances_%_if_at_least_5_red_green_blue_supports_socketed" ->
            Some(Mods(DamageType.elemental |> List.map (fun t ->
                Mod.base' (MaxResist t) v |> Mod.whenever (WhileBuff "Supports5:red") |> Mod.whenever (WhileBuff "Supports5:green") |> Mod.whenever (WhileBuff "Supports5:blue"))))
        // "while your Companion is in your Presence": while the character has one (Setup.loadWith)
        | "global_armour_evasion_energy_shield_while_in_presence_of_companion_+%" ->
            Some(Mods([ Armour; Evasion; MaxEnergyShield ] |> List.map (fun st -> Mod.inc st v |> Mod.whenever (WhileBuff "CompanionInPresence"))))
        | "damage_+%_while_in_presence_of_companion" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (WhileBuff "CompanionInPresence"))
        | "attack_speed_+%_while_in_presence_of_companion" -> one (Mod.inc Speed v |> Mod.whenever (SkillIs Attack) |> Mod.whenever (WhileBuff "CompanionInPresence"))
        // Arctic Armour's stages and its burst (read where they are built: Behaviours.arcticArmour)
        | "arctic_armour_minimum_added_cold_damage_per_stack" | "arctic_armour_maximum_added_cold_damage_per_stack"
        | "maximum_number_of_arctic_armour_stationary_stacks" -> Some Structural
        | "arctic_armour_armour_granted_+%_per_stage" -> one (Mod.inc Armour v |> Mod.whenever (PerStack("ArcticArmourStage", 1.0)))
        // Entangle's vines (read where they are built: Behaviours.entangle)
        | "entangle_apply_vines_every_X_ms" | "entangle_vines_maximum_number_of_vines_allowed" | "entangle_vine_duration_ms"
        | "entangle_base_movement_speed_+%_final_per_applied_stack" -> Some Structural
        // "Limit N Fissures": a fissure hits once, as it is made, and the vines are counted for
        // each enemy, not for each fissure, so how many fissures stand changes nothing here
        | "base_number_of_entangle_roots_allowed" -> Some NotOffensive
        // fissures that branch or come in greater number, and plants that are Overgrown: not played
        | "number_of_branching_fissures" | "skill_additional_fissure_chance_%" -> Some(Unsimulated "fissures")
        | "entangle_vines_additional_number_of_vines_allowed_when_wet" | "entangle_duration_speed_+%_when_wet" | "plant_skill_damage_+%_final_when_wet" -> Some(Unsimulated "overgrown plants")
        // Thorns: "X to Y Physical Thorns damage" is the damage of the hit that retaliates
        // (Behaviours.thorns), "increased Thorns damage" what that hit deals more of
        | id when (id.StartsWith "thorns_minimum_base_" || id.StartsWith "thorns_maximum_base_") && id.EndsWith "_damage" && (typeWord (id.Split('_').[3])).IsSome ->
            let t = (typeWord (id.Split('_').[3])).Value
            one (Mod.base' (if id.StartsWith "thorns_minimum" then AddedMin t else AddedMax t) v |> Mod.whenever (SkillIs(Tag "Thorns")))
        | "thorns_damage_+%" -> one (Mod.inc (Damage AllDamage) v |> Mod.whenever (SkillIs(Tag "Thorns")))
        | "thorns_proc_off_any_hit" -> one (Mod.flag (Named "ThornsAnyHit"))
        // Scavenged Plating: Armour and Thorns per stack; the stacks come from Fully Breaking
        // Armour (Behaviours.scavengedPlating)
        | "scavenged_plating_armour_+%_final_per_stack" -> one (Mod.more Armour v |> Mod.whenever (PerStack("ScavengedPlating", 1.0)))
        | "scavenged_plating_thorns_minimum_physical_damage" ->
            one (Mod.base' (AddedMin Physical) v |> Mod.whenever (SkillIs(Tag "Thorns")) |> Mod.whenever (PerStack("ScavengedPlating", 1.0)))
        | "scavenged_plating_thorns_maximum_physical_damage" ->
            one (Mod.base' (AddedMax Physical) v |> Mod.whenever (SkillIs(Tag "Thorns")) |> Mod.whenever (PerStack("ScavengedPlating", 1.0)))
        | "scavenged_plating_maximum_stacks_display" -> one (Mod.base' (Named "ScavengedPlatingMax") v)
        // "X% chance to gain an Endurance Charge when Supported Skills Fully Break Armour" (Behaviours.fullBreakRules)
        | "chance_to_gain_endurance_charge_on_armour_break_%" -> one (Mod.base' (Named "EnduranceChargeOnFullBreak") v)
        | "base_should_have_arcane_surge_from_stat" -> one (Mod.flag (Named "Has:ArcaneSurge"))
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

    /// States the fight keeps for the player whatever asks for them, or that the character's
    /// rules keep where a stat does (Behaviours.recentStates).
    let private keptStates =
        set [ "BeenHitRecently"; "HitRecently"; "CritRecently"; "KilledRecently"; "StunnedRecently"; "DodgeRolledRecently"; "StunnedEnemyRecently"
              "MeleeHitInPast8Seconds"; "WarcriesRecently"; "PoisonsRecently"; "IgnitedInLast8Seconds"; "FrozenInLast8Seconds"; "TimesHitRecently"
              "LowLife"; "FullLife"; "LowMana"; "FullMana"; "Surrounded"; "Shapeshifted"; "FullEnergyShield" ]

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
            else
                let withConds ms = Mods(ms |> List.map (fun (m: Mod) -> { m with Conds = conds @ m.Conds }))
                match tryRules rest with
                | Some ms -> Some(withConds ms)
                | None ->
                    // what is left is a stat known by its id ("stun_threshold_+%"), where the
                    // condition is a state the fight keeps
                    let rec kept (c: Cond) =
                        match c with
                        | WhileBuff b | PerStack (b, _) -> keptStates.Contains b
                        | Not inner | OfOwner inner -> kept inner
                        | _ -> true
                    if not (conds |> List.forall kept) then None
                    else
                        let rest = String.concat "_" rest
                        match explicit rest v |> Option.orElse (defensive rest v) with
                        | Some (Mods ms) when not ms.IsEmpty -> Some(withConds ms)
                        | _ -> None

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
        // what holds only while the player moves or sprints, which it never does: "X% chance for
        // Projectile Attacks to fire 2 additional Projectiles while moving", "increased Evasion /
        // Deflection Rating while moving / Sprinting", "Life Regeneration while moving", "Lose Life
        // while Sprinting", "Slows have no Potency on you while Sprinting", the ground a pair of
        // boots leaves while moving
        || List.contains id
            [ "projectile_attacks_%_chance_to_fire_2_additional_projectiles_while_moving"; "evasion_rating_+%_if_sprinting"
              "deflection_rating_+%_while_moving"; "life_regeneration_rate_+%_while_moving"; "life_loss_%_per_minute_while_sprinting"
              "slows_have_no_potency_on_you_while_sprinting"; "unique_boots_secondary_ground_shock_while_moving"
              "unique_boots_secondary_ground_ignite_while_moving_base_fire_damage_%_of_life" ]

    /// How fast minions get where they fight, and how far off a totem can be put down: melee
    /// minions fight at the monster from the start and the others stand where the player
    /// does, totems all stand after one placement (RULES.md, "Who a monster attacks",
    /// "Totems"). Nothing in a fight walks to its place, so these change nothing.
    let private alliesInPlace (id: string) =
        List.contains id
            [ "minion_movement_speed_+%"; "companion_movement_speed_%"; "companion_movement_speed_+%"; "tame_beasts_unique_movement_velocity_+%"
              "totem_placement_range_+%" ]

    /// What nothing in a fight can hurt: totems ("totem life is ignored", RULES.md, "Totems")
    /// and offerings (a buff on the minions for its time). Their life, resistances and
    /// regeneration change nothing.
    let private cannotBeHurt (id: string) =
        List.contains id
            [ "totem_life_+%"; "totem_elemental_resistance_%"; "totem_chaos_resistance_%"; "totems_regenerate_%_life_per_minute"
              "offering_life_+%"; "offerings_cannot_be_damaged_if_created_recently" ]

    /// What the character gives its allies that no minion here reads: charges ("X% chance to
    /// grant an Endurance / Frenzy / Power Charge to Allies in your Presence on Hit": as with
    /// Conduit, what the allies hold changes nothing they do), and the character's aura buffs,
    /// which are its own here, so "Your Aura Buffs do not affect Allies" takes nothing away.
    let private alliesReadNothing (id: string) =
        List.contains id
            [ "chance_to_grant_endurance_charge_to_nearby_allies_on_hit_%"; "chance_to_grant_frenzy_charge_to_nearby_allies_on_hit_%"
              "chance_to_grant_power_charge_to_nearby_allies_on_hit_%"; "buff_auras_dont_affect_allies" ]

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
          // "Targets Cursed by you have X% increased Life Regeneration Rate", "Bleeding Enemies /
          // Enemies with Broken Armour cannot Regenerate Life"
          "enemies_cursed_by_you_have_life_regeneration_rate_+%"; "bleeding_enemies_cannot_regenerate_life"; "broken_armour_enemies_cannot_regenerate_life"
          // the player is never Heavily Stunned (RULES.md, "Heavy stun on the player"), and of the
          // damaging ailments only Ignite is ever on it: "increased Attack Damage if you have been
          // Heavy Stunned Recently", "Cannot be Heavy Stunned while Sprinting", "Damaging Ailments
          // Cannot Be inflicted on you while you already have one"
          "attack_damage_+%_if_been_heavy_stunned_recently"; "cannot_be_heavy_stunned_while_sprinting"; "cannot_have_more_than_1_damaging_ailment"
          // "Hexproof": monsters here lay no curses on the player; "X% less Life / Mana / Energy
          // Shield Leeched from you": monsters do not leech
          "hexproof"; "life_leeched_from_-permyriad"; "mana_leeched_from_-permyriad"; "energy_shield_leeched_from_-permyriad" ]
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
        words |> List.exists noCombatWords.Contains || ownMovement id || alliesInPlace id || cannotBeHurt id || alliesReadNothing id || monstersNeverDo id || shownOnTheItem id || howItIsBuilt id

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
                    // what allies in the Presence have while "you" are in some state: the player's state
                    let rec ofYou (c: Cond) =
                        match c with
                        | WhileBuff _ | PerStack _ -> OfOwner c
                        | Not inner -> Not(ofYou inner)
                        | c -> c
                    let wrapAllies (m: Meaning) =
                        wrapMinions m |> Option.map (function Mods ms -> Mods(ms |> List.map (fun x -> { x with Conds = x.Conds |> List.map ofYou })) | m -> m)
                    if minionOwn.IsSome then minionOwn
                    elif companionOwn.IsSome then companionOwn
                    elif id.StartsWith "allies_in_presence_" then direct (id.Substring 19) value |> Option.bind wrapAllies
                    elif id.StartsWith "you_and_allies_in_presence_" then
                        match direct (id.Substring 27) value with
                        | Some (Mods ms as own) -> (match wrapAllies own with Some (Mods ws) -> Some(Mods(ms @ ws)) | _ -> Some own)
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

    /// Rare monster modifiers that act on the enemies around their bearer, not on it: a player
    /// that has them would slow and drain the monsters (Temporal Bubble's lower action speed and
    /// slower cooldowns, Drought's flask charges), which is not built (RULES.md, "Rare monster
    /// modifiers").
    let private actsOnEnemies = set [ "MonsterTemporalAura1"; "MonsterFlaskRemovalAura1" ]

    /// Rare monster modifiers that are auras for their bearer and its allies ("Allies Deal
    /// Extra Physical Damage", BuffDefinitions): the player's minions have them too.
    let private alliesToo = set [ "MonsterPhysicalDamageAura1"; "MonsterIncreasedSpeedAura1"; "MonsterEnergyShieldAura1"; "MonsterResistanceAura1" ]

    /// "When you kill a Rare monster, you gain its Modifiers for 60 seconds" (Headhunter): that
    /// the character has it (the rule is made in Behaviours.rareModsOnKill), and for every modifier a
    /// rare can have, what its player twin's stats mean ("PlayerMonster..." in Mods.dat,
    /// RareModsData) while the player has its buff. Of those stats only what is understood as
    /// modifiers counts; the rest is listed in RULES.md.
    let rec private rareModsOnKill () =
        Mods(
            Mod.flag (Named "RareModsOnKill")
            :: [ for (id, _, _, _, _, stats) in RareModsData.mods do
                    if not (actsOnEnemies.Contains id) then
                        let buff = RareMods.buffName id
                        for (stat, v) in stats do
                            match meaning stat v with
                            | Mods ms ->
                                for m in ms do
                                    yield m |> Mod.whenever (WhileBuff buff) |> Mod.from ("Rare modifier " + id)
                                    if alliesToo.Contains id then
                                        match m.Stat with
                                        | ForMinions _ -> ()
                                        | st -> yield { m with Stat = ForMinions st; Conds = OfOwner(WhileBuff buff) :: m.Conds } |> Mod.from ("Rare modifier " + id)
                            | _ -> () ])

    /// What a stat means. Local stats first, then the explicit table, then patterns,
    /// then gem-name prefixes, then the non-offensive bucket. Anything else is Unmapped.
    and meaning (id: string) (value: float) : Meaning =
        if id = "gain_rare_monster_mods_on_kill_ms" then rareModsOnKill () else
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
