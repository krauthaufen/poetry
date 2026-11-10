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
            | ("vs" | "against") :: "enemies" :: "affected" :: "by" :: "status" :: "ailments" :: tail ->
                go (EnemyHas "AnyAilment" :: acc) tail
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
            | "in" :: "weapon" :: "set" :: "one" :: tail -> go acc tail
            | "in" :: "weapon" :: "set" :: "two" :: tail -> go (WhileBuff "WeaponSetTwo" :: acc) tail
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
        | "global_defences_+%" | "armour_evasion_energy_shield_+%" ->
            Some(Mods [ Mod.inc Armour v; Mod.inc Evasion v; Mod.inc MaxEnergyShield v ])
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
        | "base_mana_gained_on_enemy_death" -> one (Mod.base' ManaOnKill v)
        | "recover_%_maximum_mana_on_kill" -> one (Mod.base' (Named "ManaPercentOnKill") v)
        | "recover_x%_of_maximum_mana_when_you_consume_a_power_charge" -> one (Mod.base' (Named "ManaPercentOnConsumePower") v)
        | "base_mana_leech_from_physical_attack_damage_permyriad" -> one (Mod.base' ManaLeech (v / 100.0) |> Mod.whenever (SkillIs Attack))
        | "base_damage_removed_from_mana_before_life_%" -> one (Mod.base' (Named "DamageRemovedFromManaFirst") v)
        | _ ->
            // "base_<type>_damage_resistance_%", "base_maximum_<type>_damage_resistance_%"
            match split id with
            | [ "base"; t; "damage"; "resistance"; "%" ] when (typeWord t).IsSome -> one (Mod.base' (Resist (typeWord t).Value) v)
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
        | "skill_effect_duration_+%" -> one (Mod.inc SkillDuration v)
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
        | "base_resist_all_elements_%" -> Some NotOffensive
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

    let private rules = [ areaOfEffect; buildup; scaledDamage; addedDamage; penetration; gainAs; convert; ailmentStat; crit; speed; gemLevel ]

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

    let private nonOffensiveWords =
        set [ "life"; "mana"; "energy"; "evasion"; "armour"; "block"; "movement"; "velocity"; "flask"; "charm"
              "rarity"; "quantity"; "light"; "radius"; "stun"; "threshold"; "regeneration"; "leech"; "recovery"
              "recover"; "resistance"; "resist"; "strength"; "dexterity"; "intelligence"; "attributes"; "spirit"; "deflection"
              "presence"; "reservation"; "cost"; "self"; "taken"; "dodge"; "experience"; "gold"; "item"; "currency" ]

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
          "empowered", "empowered attacks" ]

    let private isNonOffensive (id: string) =
        let words = split id
        // A penetration / enemy debuff line mentions resistance but is offensive.
        not (List.contains "enemy" words) && words |> List.exists nonOffensiveWords.Contains

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
        | None when structural id -> Structural
        | None ->
            match direct id value with
            | Some m -> m
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
