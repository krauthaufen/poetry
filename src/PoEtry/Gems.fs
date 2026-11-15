namespace PoEtry

/// Everything loaded from data/game.
type GameDb =
    { Skills: Map<string, SkillData>
      Gems: Map<string, GemData>
      /// Every entry of a gem name (Mace Strike: one per weapon type).
      GemVariants: Map<string, GemData list>
      Weapons: Map<string, WeaponBase>
      Minions: Map<string, MinionData>
      /// Every item base name (weapons, armour, jewellery...).
      BaseNames: Set<string>
      Text: StatText.Index
      /// Stat id -> how the game describes it (first variant), for classifying gem stats.
      Descriptions: Map<string, string> }

/// A weapon as equipped: base plus its local modifiers.
type Weapon =
    { Damage: Map<DamageType, Range>
      CritChance: float
      AttacksPerSecond: float
      /// Crossbows: seconds to reload, local reload speed included (0: no ammunition).
      ReloadTime: float
      Tags: SkillTag list }

/// What happened to every stat that went into a skill or item.
type Coverage =
    { /// Stats with no meaning in StatMap yet.
      Unmapped: (string * float) list
      /// Understood but not simulated: (stat, mechanic).
      Unsimulated: (string * string) list
      /// Lines of text that the stat descriptions didn't recognise.
      UnknownText: string list }

module Coverage =
    let empty =
        { Unmapped = []
          Unsimulated = []
          UnknownText = [] }

    let merge a b =
        { Unmapped = a.Unmapped @ b.Unmapped
          Unsimulated = a.Unsimulated @ b.Unsimulated
          UnknownText = a.UnknownText @ b.UnknownText }

/// A gem as socketed: active gem, level, quality, supports.
type GemSetup =
    { Gem: string
      Level: int
      Quality: int
      /// Which stat set of the skill (1-based). Most skills have one.
      StatSet: int
      Supports: (string * int) list }

module GameDb =
    let load (read: string -> string) =
        { Skills = GameData.parseSkills (read "skills.json")
          Gems = GameData.parseGems (read "gems.json")
          GemVariants = GameData.parseGemList (read "gems.json") |> List.groupBy (fun g -> g.Name) |> Map.ofList
          Weapons = GameData.parseWeapons (read "bases.json")
          Minions = GameData.parseMinions (read "minions.json")
          BaseNames =
            match Json.parse (read "bases.json") with
            | JObj m -> m |> Map.toList |> List.map fst |> Set.ofList
            | _ -> Set.empty
          Text = StatText.build [ read "statdesc_stat_descriptions.json"; read "statdesc_passive_skill_stat_descriptions.json" ]
          Descriptions =
            let rec walk (acc: Map<string, string>) (j: Json) =
                match j with
                | JObj m ->
                    let acc =
                        match Map.tryFind "stats" m, Map.tryFind "1" m with
                        | Some stats, Some variants ->
                            let text = Json.list (Some variants) |> List.tryHead |> Option.bind (Json.strField "text")
                            match text with
                            | Some t -> Json.list (Some stats) |> List.choose Json.str |> List.fold (fun a st -> if Map.containsKey st a then a else Map.add st t a) acc
                            | None -> acc
                        | _ -> acc
                    m |> Map.fold (fun a k v -> if k = "stats" then a else walk a v) acc
                | JArr xs -> xs |> List.fold walk acc
                | _ -> acc
            [ "statdesc_skill_stat_descriptions.json"; "statdesc_specific.json"; "statdesc_gem_stat_descriptions.json"
              "statdesc_active_skill_gem_stat_descriptions.json"; "statdesc_stat_descriptions.json" ]
            |> List.fold (fun acc f -> walk acc (Json.parse (read f))) Map.empty }

module Gems =
    /// Gem stats at a level, as the game computes them. Interpolation 1 = value as
    /// listed; 3 = scaled by skill damage effectiveness at the stat set's actor level.
    /// Source: PoB CalcTools.buildSkillInstanceStats (formula), GameConstants (base effectiveness).
    let statsWith (altQuality: bool) (skill: SkillData) (set: StatSetData) level quality : (string * float) list =
        let levelData =
            Map.tryFind level set.Levels
            |> Option.orElse (Map.tryFind 1 set.Levels)
        let effectiveness actorLevel =
            Map.find "SkillDamageBaseEffectiveness" Tables.gameConstants
            * set.BaseEffectiveness
            * (1.0 + set.IncrementalEffectiveness * (actorLevel - 1.0))
            * ((1.0 + set.DamageIncrementalEffectiveness) ** (actorLevel - 1.0))
        let levelStats =
            match levelData with
            | None -> []
            | Some l ->
                set.Stats
                |> List.mapi (fun i stat ->
                    let raw = List.tryItem i l.Values |> Option.defaultValue 1.0
                    let value =
                        match List.tryItem i l.Interpolation with
                        | Some 3 -> System.Math.Round(effectiveness l.ActorLevel * raw)
                        | _ -> raw
                    stat, value)
        let qualityStats =
            (skill.QualityStats @ (if altQuality then skill.AltQualityStats else []))
            |> List.map (fun (s, per) -> s, System.Math.Truncate(per * float quality))
        levelStats @ set.ConstantStats @ qualityStats |> List.filter (fun (_, v) -> v <> 0.0)

    let stats skill set level quality = statsWith false skill set level quality

    let private tagOfType t =
        match t with
        | "Attack" -> Some Attack
        | "Spell" -> Some Spell
        | "Melee" -> Some Melee
        | "Projectile" -> Some Projectile
        | "Area" -> Some Area
        | "Duration" -> Some Duration
        | "Channel" -> Some Channelling
        | "Minion" -> Some Minion
        | "Totem" -> Some Totem
        | "Warcry" -> Some Warcry
        | "Herald" -> Some Herald
        | "Curse" | "AppliesCurse" -> Some Curse
        | "Nova" -> Some Nova
        | "Chains" -> Some Chaining
        | "Slam" -> Some Slam
        | "Strike" -> Some Strike
        | "Triggered" -> Some Triggered
        | "Lightning" | "Fire" | "Cold" | "Chaos" | "Physical" | "Trap" | "Mine" | "Grenade" | "Plant" -> Some(Tag t)
        | _ -> None

    /// "spell_minimum_base_fire_damage" -> (Min, Fire)
    let private baseDamageStat (id: string) =
        match id.Split('_') |> List.ofArray with
        | [ _; bound; "base"; t; "damage" ]
        | [ bound; "base"; t; "damage" ] when bound = "minimum" || bound = "maximum" ->
            DamageType.all
            |> List.tryFind (fun d -> (DamageType.name d).ToLowerInvariant() = t)
            |> Option.map (fun d -> bound = "minimum", d)
        | _ -> None

    /// Stats read directly into the skill's Delivery.
    let private deliveryStats =
        set [ "base_number_of_projectiles"; "number_of_chains"; "projectile_base_number_of_targets_to_pierce"
              "active_skill_base_area_of_effect_radius"; "projectiles_can_shotgun"; "total_projectile_spread_angle_override" ]

    /// Gem stats often carry the skill's internal name first
    /// ("lightning_strike_damage_+%_final_per_power_charge"). If an id is Unmapped,
    /// try it with 1..3 leading words removed; only a full Mods meaning is accepted.
    /// A gem stat's meaning: as on items, or with the skill-name prefix stripped (marked skill-specific).
    let gemMeaning (id: string) v =
        match StatMap.meaning id v with
        | Unmapped
        | Unsimulated _ as original ->
            let words = id.Split('_')
            [ 1..3 ]
            |> List.tryPick (fun k ->
                if words.Length > k + 2 then
                    match StatMap.meaning (String.concat "_" (Array.skip k words)) v with
                    // marked: it came from a skill-specific stat, so it describes that skill only
                    | Mods ms -> Some(Mods(ms |> List.map (Mod.from ("skill-specific:" + id))))
                    | _ -> None
                else None)
            |> Option.defaultValue original
        | m -> m

    /// The skill's internal name as its stats carry it ("lightning_strike_" on Falling
    /// Thunder's stats): the longest word prefix (1 to 3 words) that at least two of the
    /// skill's stats share and that doesn't start with a generic stat word. Words of that
    /// name must not be read as scopes ("strike", "staff", "lightning").
    let private genericStart =
        set [ "base"; "active"; "skill"; "support"; "global"; "minimum"; "maximum"; "spell"; "attack"; "number"; "projectile"
              "projectiles"; "is"; "can"; "cannot"; "display"; "quality"; "total"; "movement"; "never"; "always"; "melee"
              "main"; "off"; "non"; "additional"; "damage"; "critical"; "chance"; "virtual"; "local"; "minion"; "minions" ]

    let skillPrefix (ids: string list) =
        let words = ids |> List.distinct |> List.map (fun (id: string) -> id.Split('_') |> List.ofArray)
        // the prefix most of the stats share; of equally common ones, the longest
        [ 1; 2; 3 ]
        |> List.collect (fun k ->
            words
            |> List.filter (fun ws -> ws.Length > k + 1 && not (genericStart.Contains ws.Head))
            |> List.countBy (List.truncate k)
            |> List.filter (fun (_, n) -> n >= 2)
            |> List.map (fun (ws, n) -> ws, n, k))
        |> List.sortByDescending (fun (_, n, k) -> n, k)
        |> List.tryHead
        |> Option.map (fun (ws, _, _) -> String.concat "_" ws + "_")

    /// A gem stat's meaning when the skill's internal name is known: the rest of the id first.
    let gemMeaningWith (prefix: string option) (id: string) v =
        match prefix with
        | Some p when id.StartsWith p && id.Length > p.Length ->
            match StatMap.meaning (id.Substring p.Length) v with
            | Mods ms -> Mods(ms |> List.map (Mod.from ("skill-specific:" + id)))
            | _ -> gemMeaning id v
        | _ -> gemMeaning id v

    /// "Limit N" of what stands where it was cast (see `limitOf`).
    let private standingLimitStat (k: string) =
        k.EndsWith "_max_number_of_storms" || k.EndsWith "_maximum_number_of_storms" || k.EndsWith "_base_maximum_number_of_orbs"
        || k.EndsWith "_maximum_number_of_spheres" || k.EndsWith "_ground_limit"
        || List.contains k [ "number_of_tornados_allowed"; "base_number_of_ice_volcanos_allowed"; "volcano_base_number_of_geysers_allowed"
                             "base_number_of_awakened_forests_allowed"; "base_number_of_overcharged_spears_allowed" ]

    /// A gem's own stats that are no modifiers but numbers read by how their ids end, where the
    /// skill is built, for a skill of the type that is built so: the stages of a staged channel
    /// and what a stage adds (`buildSet`), how often and how many times a skill that keeps
    /// hitting hits and how many of it can stand (Behaviours.sustained), volleys, the time
    /// between the charges a channel takes, and how long something falls (`buildSet`).
    let private readByEnding (types: Set<string>) (id: string) =
        let ends (es: string list) = es |> List.exists (fun (e: string) -> id.EndsWith e)
        (types.Contains "Channel"
         && ends [ "_maximum_stages"; "_max_stages"; "_max_number_of_stages"; "_maximum_number_of_stages"; "damage_+%_final_per_stage"; "damage_+%_final_per_stack"
                   "damage_+%_final_per_additional_stage"; "all_damage_%_to_gain_as_fire_per_stage"; "radius_+_per_stage"
                   "_initial_explosion_number_of_additional_projectiles_per_additional_stage" ])
        || (types.Contains "Sustained"
            && (ends [ "_frequency_ms"; "_pulse_delay_ms"; "_pulse_interval_ms"; "_every_x_ms"; "_fireball_delay_ms"; "_hit_interval_ms"; "_maximum_number_of_hits"; "_number_of_pulses" ]
                || standingLimitStat id))
        || ends [ "_number_of_volleys"; "_volley_delay_ms"; "_consume_power_charge_per_X_ms"; "_fall_time_ms" ]

    /// Turn game stats into mods, sorting out what we couldn't use. `own`: the skill types of
    /// the gem whose own stats these are (None: a support's).
    let private interpret (own: Set<string> option) (prefix: string option) (stats: (string * float) list) =
        stats
        |> List.fold
            (fun (mods, cov: Coverage) (id, v) ->
                match baseDamageStat id with
                | Some _ -> mods, cov
                | None when deliveryStats.Contains id || id.EndsWith "_number_of_projectiles_per_power_charge" -> mods, cov
                | None when (match own with Some types -> readByEnding types id | None -> false) -> mods, cov
                | None ->
                    match gemMeaningWith prefix id v with
                    | Mods ms -> mods @ ms, cov
                    | Local _
                    | Structural
                    | NotOffensive -> mods, cov
                    | Unsimulated m -> mods, { cov with Unsimulated = cov.Unsimulated @ [ id, m ] }
                    | Unmapped -> mods, { cov with Unmapped = cov.Unmapped @ [ id, v ] })
            ([], Coverage.empty)

    /// Accents in names differ between sources ("Oisín's Oath" / "Oisin's Oath").
    let private plain (name: string) =
        // a plain table instead of Unicode normalisation, which Fable doesn't have
        let from = "áàâäãåéèêëíìîïóòôöõúùûüýñçÁÀÂÄÃÅÉÈÊËÍÌÎÏÓÒÔÖÕÚÙÛÜÝÑÇ"
        let into = "aaaaaaeeeeiiiiooooouuuuyncAAAAAAEEEEIIIIOOOOOUUUUYNC"
        name |> String.map (fun ch -> match from.IndexOf ch with | -1 -> ch | k -> into.[k])

    let private skillOfGem (db: GameDb) (name: string) =
        let find n =
            match Map.tryFind n db.Gems with
            | Some g -> Map.tryFind g.GrantedEffectId db.Skills
            | None -> Map.tryFind n db.Skills
        find name |> Option.orElse (if plain name <> name then find (plain name) else None)
        // a companion or spectre the source names by its monster ("Companion: Quill Crab"): the
        // gem that holds it (Tame Beast with a captured beast, Spectre: {0}); the monster is
        // found by name where minions are built
        |> Option.orElse (
            if name.StartsWith "Companion: " then Map.tryFind "TameBeastPlayer" db.Skills
            elif name.StartsWith "Spectre: " then Map.tryFind "SummonSpectrePlayer" db.Skills
            else None)
        // a player's skill that has no gem of its own, by its name (Punch, the unarmed attack)
        |> Option.orElse (
            db.Skills |> Map.tryPick (fun id d -> if d.Name = name && id.EndsWith "Player" && not d.Support then Some d else None))

    /// A support gem's data. A name can belong to a support and to a skill ("Unleash"): in a
    /// support socket it is the support.
    let private supportOfGem (db: GameDb) (name: string) =
        Map.tryFind name db.GemVariants |> Option.defaultValue []
        |> List.tryPick (fun g -> Map.tryFind g.GrantedEffectId db.Skills |> Option.filter (fun s -> s.Support))
        |> Option.orElse (skillOfGem db name)

    /// Does a weapon (None: unarmed) satisfy a gem's weapon requirements? Requirements name
    /// base types; a weapon carries its own as the tag "Base:<type>".
    let weaponMatches (reqs: string list) (weapon: Weapon option) =
        let bases = weapon |> Option.map (fun w -> w.Tags |> List.choose (function Tag t when t.StartsWith "Base:" -> Some(t.Substring 5) | _ -> None)) |> Option.defaultValue []
        let ranged = bases |> List.exists (fun b -> b = "Bow" || b = "Crossbow")
        reqs.IsEmpty
        || reqs |> List.exists (fun r ->
            if r.Contains "Unarmed" && weapon.IsNone then true
            elif r.StartsWith "Any" && r.Contains "Shield" then true
            elif r.StartsWith "Any" then
                weapon.IsSome && not (r.Contains "Melee" && ranged) && not (r.Contains "non-Talisman" && List.contains "Talisman" bases)
            else List.contains r bases || r = "Armoured Shield")

    /// The weapon a gem's skill uses: the first of the character's weapons (set one, then
    /// set two) that satisfies one of the gem's variants; skills without a requirement
    /// use the first weapon.
    let chooseWeapon (db: GameDb) (name: string) (weapons: Weapon list) : Weapon option =
        let variants = Map.tryFind name db.GemVariants |> Option.orElse (Map.tryFind (plain name) db.GemVariants) |> Option.defaultValue []
        let reqs = variants |> List.collect (fun g -> g.WeaponRequirements)
        // both weapon sets can hold a weapon the skill may use (a utility spear and the real
        // one): the API does not say which set a gem is bound to, so the stronger weapon is taken
        let strength (w: Weapon) = (w.Damage |> Map.toList |> List.sumBy (fun (_, r) -> (r.Min + r.Max) / 2.0)) * w.AttacksPerSecond
        if reqs.IsEmpty then List.tryHead weapons
        else
            match weapons |> List.filter (fun w -> weaponMatches reqs (Some w)) with
            | [] -> if weaponMatches reqs None then None else List.tryHead weapons
            | fitting -> Some(fitting |> List.maxBy strength)

    /// Skill data for a gem with this weapon: gems with one entry per weapon type
    /// (Mace Strike) resolve to the matching one.
    let private skillOfGemFor (db: GameDb) (name: string) (weapon: Weapon option) =
        match Map.tryFind name db.GemVariants with
        | Some (_ :: _ :: _ as variants) ->
            variants
            |> List.tryFind (fun g -> not g.WeaponRequirements.IsEmpty && weaponMatches g.WeaponRequirements weapon)
            |> Option.bind (fun g -> Map.tryFind g.GrantedEffectId db.Skills)
            |> Option.orElse (skillOfGem db name)
        | _ -> skillOfGem db name

    /// The shot a crossbow ammunition gem loads: the gem's additional granted skill.
    let private shotOf (db: GameDb) (name: string) =
        Map.tryFind name db.Gems |> Option.orElse (Map.tryFind (plain name) db.Gems)
        |> Option.bind (fun g -> g.AdditionalEffects |> List.tryPick (fun id -> Map.tryFind id db.Skills))

    /// Persistent buffs (Berserk, Eternal Rage, heralds...): always on while reserved.
    /// Does a buff gem's stat affect the character (true) or only the skill itself?
    /// The game's own description says so: "Buff grants ...", "Grants ...", "... you ...".
    let grantsToCharacter (db: GameDb) (statId: string) =
        match Map.tryFind statId db.Descriptions with
        | Some t ->
            let t = t.ToLowerInvariant()
            t.StartsWith "buff grants" || t.StartsWith "grants" || t.Contains " you " || t.Contains "your " || t.StartsWith "you "
        | None -> false

    let isMeta (db: GameDb) name =
        skillOfGem db name |> Option.exists (fun d -> d.Types.Contains "Meta")

    /// Skill data for a gem name.
    let data (db: GameDb) name = skillOfGem db name

    /// Skills that put something down which explodes after the skill's detonation time, the
    /// explosion being a part of its own (Behaviours.detonation): a lodged Explosive Spear.
    let lodged = set [ "Explosive Spear" ]

    /// Skill types that only say which supports fit or how the skill is aimed and animated:
    /// nothing to simulate.
    let private descriptiveTypes =
        // (Travel, Jumping, Movement: the skill moves the player, who stands in a fight, see
        // RULES.md "The player stands"; its hit is played where it lands)
        set [ "Travel"; "Jumping"; "Movement"; "Triggerable"; "Totemable"; "Trappable"; "Mineable"; "Multicastable"; "Unleashable"; "UsableWhileMoving"; "Cascadable"
              "Barrageable"; "MirageArcherCanUse"; "AttackInPlace"; "AttackInPlaceIsDefault"; "Damage"; "UsableWhileShapeshifted"
              "UsableWhileMounted"; "NoAttackOrCastTime"; "ProjectileSpeed"; "ProjectileNumber"; "ProjectilesFromUser"; "RangedAttack"
              "MeleeSingleTarget"; "AreaSpell"; "CanCancelActions"; "UseGlobalStats"; "GamepadDeflectable"; "SingleLevelSkill"
              "InstantNoRepeatWhenHeld"; "InstantShiftAttackForLeftMouse"; "GamepadDoNotForceSkillAtLocation"
              "GamepadDoNotChannelSkillAtLocation"; "ToggleSpawnedObjectTargetable_DefaultOn"; "ToggleSpawnedObjectTargetable_DefaultOff"
              "PersistentShowsCastTime"; "ReserveInAllSets"; "MultipleReservation"; "HasReservation"; "ReservesManually"; "OngoingSkill"
              "SkillGrantedBySupport"; "NonWeaponAttack"; "Invokable"; "CannotSpiritStrike"; "CannotTerrainChain"; "CannotChain"
              "ProjectileNoCollision"; "GroundTargetedProjectile"; "SingleMainProjectile"; "ProjectilesNumberModifiersNotApplied"
              "CanHaveMultipleOngoingSkillInstances"; "AffectedByCooldownRate"; "TotemCastsAlone"; "UsedByTotem"; "UsedByProxy"
              "UsedByClone"; "Bow"; "Spear"; "CrossbowSkill"; "Wind"; "Nature"; "Necrotic"; "Bear"; "Wolf"; "Wyvern"; "Palm" ]

    /// Skill types the simulator acts on (see RULES.md for how far each goes).
    let private modelledTypes =
        set [ "Attack"; "Spell"; "Melee"; "Projectile"; "Area"; "Duration"; "Nova"; "Slam"; "Strike"; "Chains"; "Triggered"
              "Fire"; "Cold"; "Lightning"; "Chaos"; "Physical"; "Cooldown"; "Instant"; "Buff"; "Persistent"; "Aura"; "Herald"
              "Meta"; "Invocation"; "Triggers"; "InbuiltTrigger"; "GeneratesEnergy"; "ConsumesCharges"; "RequiresCharges"
              "SkillConsumesPowerChargesOnUse"; "HasUsageCondition"; "AppliesCurse"; "Curse"; "Mark"; "Limit"
              "CreatesMinion"; "Minion"; "Companion"; "CreatesCompanion"; "SummonsTotem"; "SummonsAttackTotem"; "Totem"
              "GeneratesInfusion"; "GeneratesRemnants"; "DamageOverTime"; "Grenade"; "Plant"; "Storm"
              "SkillConsumesFrenzyChargesOnUse"; "SkillConsumesEnduranceChargesOnUse"; "Sustained"; "Shapeshift"; "Channel"; "Rain"
              "CrossbowAmmoSkill"; "GainsStages"; "RequiresShield"; "Warcry"
              "CommandableMinion"; "MinionsAreUndamagable"; "TargetsDestructibleCorpses"; "IceCrystal"; "Wall"; "Detonator"
              "GeneratesCharges"; "ComboStacking" ]

    /// Skill types that say what a skill does, for the skills that are played as the type
    /// says (each with its row in RULES.md); for every other skill of the type it stays "not
    /// modelled". An empty list: the type is played, or changes nothing, for every skill.
    let private playedTypes : Map<string, string list> =
        Map.ofList
            [ // "Skills that keep hitting", "Fuses", and the limit of an orb ("Limits")
              "Orb", []
              // "Spearfield": the spears go off where an enemy stands on them
              "Hazard", [ "Spearfield" ]
              // "Chained attacks": the last strike of the chain is its own stat set
              "PerformsFinalStrike", [ "Ice Strike"; "Primal Strikes"; "Shred"; "Tempest Flurry" ]
              // "Parts that need something", "Parts for one state of the enemy", "Consumed states"
              "SkillConsumesParried", [ "Disengage"; "Fangs of Frost" ]
              "SkillConsumesShock", [ "Primal Strikes"; "Siphoning Strike"; "Shockchain Arrow" ]
              "SkillConsumesFreeze", [ "Fragmentation Rounds"; "Glacial Cascade"; "Icestorm"; "Snipe" ]
              "SkillConsumesBleeding", [ "Blood Hunt"; "Herald of Blood" ]
              // "Tempest Bell": the bell stands for its number of hits; "Flame Breath", "Tempest
              // Bell": when the cooldown starts
              "ObjectDurability", [ "Tempest Bell" ]
              "ManualCooldownConsumption", [ "Flame Breath"; "Tempest Bell"; "Demon Form" ]
              // "What empowered attacks get", "Ice-Tipped Arrows", "Triskelion": the next attacks are empowered
              "EmpowersOtherSkill", [ "Seismic Cry"; "Infernal Cry"; "Ancestral Cry"; "Fortifying Cry"; "Ice-Tipped Arrows"; "Triskelion Cascade" ]
              // "Entangle", "More parts of one use", "Detonation": the fissure is the skill's own part
              "CreatesFissure", [ "Entangle"; "Molten Crash"; "Forge Hammer"; "Starborn Onslaught" ]
              // "Minion Instability": which minions the support's explosion is for
              "MinionsCanExplode", []
              // what a support can be socketed with, what a line can name, where a projectile
              // starts and what the skill can be used with: nothing a fight plays
              "CanBecomeArrowRain", []; "CreatesDemonMinion", []; "NoDualWield", []; "ProjectilesNotFromUser", [] ]

    /// What a gem's skill does that the simulator has no model for: its skill types that are
    /// neither descriptive nor modelled ("Channel", "GainsStages", "Shapeshift", "Rain"...).
    let unmodelledTypes (db: GameDb) name =
        skillOfGem db name
        // an ammunition gem is judged by the shot it loads
        |> Option.map (fun d -> if d.Types.Contains "CrossbowAmmoSkill" then shotOf db name |> Option.defaultValue d else d)
        |> Option.map (fun d ->
            // (a detonation after a time is played for grenades and Frost Bomb: their hit's delay)
            let fused = d.Types.Contains "Grenade" || d.Name = "Frost Bomb" || lodged.Contains d.Name
            // (what Grim Pillars and Oil Barrage leave on the ground is played: Ice Crystals, Oil Ground)
            let grounded = d.Name = "Grim Pillars" || d.Name = "Oil Barrage"
            let played t = match Map.tryFind t playedTypes with Some [] -> true | Some names -> List.contains d.Name names | None -> false
            d.Types |> Set.filter (fun t -> not (descriptiveTypes.Contains t) && not (modelledTypes.Contains t) && not (played t) && not (fused && t = "DetonatesAfterTime") && not (grounded && t = "CreatesGroundEffect")) |> Set.toList)
        |> Option.defaultValue []

    /// "Limit N" of a skill whose effect stands where it was cast (a storm, an orb, a sphere, a
    /// volcano, a patch of ground): the gem's own stat for it. So many can stand at once; one
    /// more takes the oldest away.
    let limitOf (raw: (string * float) list) : int option =
        raw |> List.tryPick (fun (k, v) -> if v > 0.0 && standingLimitStat k then Some(int v) else None)

    let isPersistentBuff (db: GameDb) name =
        skillOfGem db name |> Option.exists (fun d -> d.Types.Contains "Persistent" && d.Types.Contains "Buff")

    /// A stat set flagged to be shown with the main set: it happens in the same use
    /// (Falling Thunder's projectiles). Other sets are alternatives (Spark's Cold-Infused).
    /// "X-Infused" stat sets are what the skill does when it consumes an infusion: an
    /// alternative, not a part.
    let private isPart (set: StatSetData) =
        set.Stats |> List.contains "display_statset_hide_usage_stats" && not (set.Label.EndsWith "-Infused")

    /// What a part of a skill needs before it happens, where the skill's data has no flag for it
    /// and only the description says so: (skill, stat set label or "" for the skill itself) ->
    /// conditions. "Vs:A|B" needs the enemy in one of these states ("!" at the end: the state is
    /// consumed); "Charge": a charge to consume; "Infused:X": an infusion of that kind, consumed;
    /// "DetonatedBy:T": it waits until a skill of type T (or a warcry) is used; "NoHit": it
    /// deals no hit damage of its own; "Ignites": it always ignites.
    let private partConditions : Map<string * string, string list> =
        Map.ofList
            [ ("Siphoning Strike", "Shockwave"), [ "Vs:Shocked!" ]
              ("Shockburst Rounds", "Pulse"), [ "Vs:Shocked|Electrocuted" ]
              ("Shockchain Arrow", "Shockwave"), [ "Vs:Shocked|Electrocuted" ]
              ("Primal Strikes", "Spirits"), [ "Vs:Shocked" ]
              ("Snipe", "Icy Blast"), [ "Vs:Frozen!" ]
              ("Snipe", "Explosion"), [ "NotVs:Frozen" ]
              // attacks chained by using the skill again and again: which uses of the chain
              // ("steps/length") a stat set is for, and how often it strikes
              ("Ice Strike", ""), [ "Seq:0,1/3" ]
              ("Ice Strike", "Third Strike"), [ "Seq:2/3" ]
              ("Tempest Flurry", ""), [ "Seq:0,1/4" ]
              ("Tempest Flurry", "Third Strike"), [ "Seq:2/4"; "Hits:3" ]
              ("Tempest Flurry", "Fourth Strike"), [ "Seq:3/4" ]
              ("Primal Strikes", ""), [ "Seq:0,1/3" ]
              ("Primal Strikes", "Final Strike"), [ "Seq:2/3"; "Takes:Shocked" ]
              ("Shred", ""), [ "Seq:0,1/6" ]
              ("Shred", "Fast Strikes"), [ "Seq:2,3,4/6" ]
              ("Shred", "Final Strike"), [ "Seq:5/6"; "Hits:2" ]
              // "Consumes Power Charges if possible to instead Channel a sustained barrage of
              // electrified Oil": the Empowered set in place of the plain one while a charge lasts
              ("Oil Barrage", ""), [ "NotWithCharge" ]
              ("Oil Barrage", "Empowered"), [ "Part"; "Charge" ]
              // the pustule's burst happens when the pustule does (see Behaviours.toxicDomain)
              ("Toxic Domain", ""), [ "NoHit" ]
              ("Toxic Domain", "Pustule Explosion"), [ "Part"; "OnKill" ]
              // "Simultaneously fires a Physical Projectile and a Chaos Projectile"
              ("Coiling Bolts", "Chaos"), [ "Part" ]
              // what springs from the enemy hit and goes for others: beams "at nearby enemies",
              // bolts "at multiple other enemies", shrapnel "in a cone behind it"
              ("Lightning Arrow", "Beam"), [ "Part"; "Behind" ]
              ("Lightning Spear", "Lightning Bolts"), [ "Part"; "Behind" ]
              ("Molten Blast", "Shrapnel"), [ "Part"; "Behind" ]
              // more of the same use
              ("Detonating Arrow", "Explosion"), [ "Part" ]
              ("Shield Charge", "Collision"), [ "Part" ]
              ("Molten Crash", ""), [ "Hits:2" ]
              ("Molten Crash", "Fissures"), [ "Part" ]
              ("Starborn Onslaught", "Star Fall"), [ "Part" ]
              ("Wind Serpent's Fury", "Serpent"), [ "Part"; "Charge" ]
              ("Earthshatter", "Explosion"), [ "Part"; "DetonatedBy:Warcry" ]
              // "destroying all Bloodstone Lances on each enemy ... to cause a damaging explosion"
              ("Shattering Spite", ""), [ "Vs:BloodstoneLance!"; "NeedsStacks:BloodstoneLance:30" ]
              // "will Consume [a Fragment of Divinity] when you leap to become Empowered ... does not
              // use up its Cooldown when empowered"
              ("Righteous Descent", ""), [ "Consumes:DivineFragment"; "NotWithCharge" ]
              ("Righteous Descent", "Empowered"), [ "Part"; "Charge" ]
              // the pillars stay as Ice Crystals and explode when destroyed
              ("Grim Pillars", "Explosion"), [ "Part"; "Crystals" ]
              ("Frozen Locus", "Explosion"), [ "Crystals" ]
              // "Create a wall of Ice Crystals ... The Crystals explode if sufficiently damaged"
              ("Frost Wall", ""), [ "Crystals" ]
              ("Glacial Bolt", "Wall"), [ "Crystals" ]
              ("Detonate Living", "Explosion"), [ "Part"; "Vs:Cullable" ]
              // an explosion for each ailment consumed
              ("Elemental Sundering", "Cold"), [ "Part"; "Vs:Frozen!" ]
              ("Elemental Sundering", "Fire"), [ "Part"; "Vs:Ignited!" ]
              ("Elemental Sundering", "Lightning"), [ "Part"; "Vs:Shocked!" ]
              ("Snap", ""), [ "Vs:Frozen!" ]
              ("Snap", "Lightning"), [ "Part"; "Vs:Shocked!" ]
              ("Snap", "Fire"), [ "Part"; "Vs:Ignited!" ]
              ("Acidic Concoction", "Acidic Burst"), [ "Part"; "Vs:Poisoned!" ]
              ("Frost Darts", "Ice Chunk"), [ "Part"; "Vs:Chilled|Frozen" ]
              ("Frost Darts", "Explosion"), [ "Part"; "Infused:ColdInfusion" ]
              ("Fireball", "Firebolts"), [ "Part"; "Infused:FireInfusion"; "Behind" ]
              ("Ember Fusillade", "Beam"), [ "Part"; "Infused:LightningInfusion" ]
              // "Detonate a Curse ... Can only detonate Curses for which at least half of the duration has expired"
              ("Hexblast", ""), [ "DetonatesCurse" ]
              // "When this skill kills an enemy ... crushing them to create a shockwave"
              ("Crushing Fear", "Shockwave"), [ "Part"; "OnKill" ]
              // the form a skill takes when it has something to spend, in place of the plain one
              ("Rend", ""), [ "NotWithCharge" ]
              ("Rend", "Lightning-Charged"), [ "Part" ]
              ("Furious Slam", ""), [ "NotEnraged" ]
              ("Furious Slam", "Enraged"), [ "Part"; "SpendsRage:10" ]
              // the spears a standing enemy touches explode at once (four of them: guess)
              // and, set off within half a second of appearing, deal 80% less
              ("Spearfield", "Explosion"), [ "Part"; "Field"; "Less:80" ]
              // the eruption happens once; the sprays that follow rain down around the volcano
              ("Volcano", ""), [ "Once" ]
              ("Volcano", "Projectiles"), [ "Rain" ]
              // "Anguish impacts within a 2.4 metre radius of the target", each with its own impact radius
              ("Requiem", "Requiem"), [ "Rain" ]
              ("Requiem", ""), [ "Rain" ]
              // "All loaded bolts are fired at once, causing them to rain down over the target area"
              ("Hailstorm Rounds", "Impact"), [ "Rain" ]
              ("Hailstorm Rounds", ""), [ "Rain" ]
              // "sprays a cone of ice shards when it hits a target": on from that target
              ("Ice Shot", "Shards"), [ "Behind" ]
              // one use, several strikes: five stabs before the final one; both gouges hit
              ("Rapid Assault", ""), [ "Hits:5" ]
              ("Cross Slash", ""), [ "Hits:2" ]
              // released with Perfect Timing: the perfect strike takes the place of the normal one
              ("Perfect Strike", ""), [ "NoHit" ]
              ("Gathering Storm", ""), [ "NoHit" ]
              ("Icestorm", "Improved Bolt"), [ "Vs:Chilled|Frozen!" ]
              ("Frostflame Nova", "Volatile Ice"), [ "Vs:Ignited" ]
              ("Disengage", "Shockwave"), [ "Vs:Parried!" ]
              ("Fangs of Frost", "Explosion"), [ "Vs:Parried!" ]
              ("Boneshatter", "Shockwave"), [ "Vs:HeavilyStunned" ]
              ("Wing Blast", "Shockwave"), [ "Vs:HeavilyStunned" ]
              ("Blood Hunt", "Explosion"), [ "Vs:Bleeding!" ]
              ("Gas Arrow", "Explosion"), [ "Vs:Ignited" ]
              ("Gas Grenade", "Explosion"), [ "Vs:Ignited" ]
              ("Stormblast Bolts", "Explosion"), [ "DetonatedBy:Type:Detonator" ]
              ("Rapid Assault", "Explosion"), [ "DetonatedBy:Type:Detonator" ]
              ("Storm Lance", "Detonation"), [ "DetonatedBy:Type:Detonator" ]
              ("Forge Hammer", "Fissures"), [ "DetonatedBy:Warcry" ]
              ("Glacial Lance", "Explosion"), [ "Charge" ]
              ("Spear Throw", "Explosion"), [ "Charge" ]
              ("Firestorm", "Ice Bolts"), [ "Infused:ColdInfusion" ]
              ("Firestorm", "Lightning Bolts"), [ "Infused:LightningInfusion" ]
              ("Incinerate", "Ignited Ground"), [ "Infused:FireInfusion"; "NoHit" ]
              ("Solar Orb", "Ignite"), [ "NoHit"; "Ignites" ]
              ("Flame Wall", "Projectile Damage"), [ "NoHit" ]
              ("Gathering Storm", "Tempest Shockwave"), [ "Vs:TempestBell" ]
              ("Lightning Warp", ""), [ "Vs:Cullable"; "WarpsInto:Ball Lightning" ]
              ("His Vile Intrusion", ""), [ "Vs:Cullable" ]
              ("Detonate Living", ""), [ "Vs:Cullable" ]
              ("Power Siphon", ""), [ "Vs:Cullable" ]
              // "Leaving the area collapses the storm": the character stands in it until a twister
              // consumes it (Behaviours.whirlwind)
              ("Whirling Slash", "Whirlwind"), [ "NoHit" ] ]

    /// Stat sets whose two radii are a length and a width ("Fissure length is", "Charge length
    /// is / Charge width is", "Explosion length is"): a strip from the player towards the enemy
    /// aimed at, so long and so wide. The width as a multiple of the secondary radius: the
    /// charge's is its width; Entangle's fissure is taken to be as wide as its vines reach to
    /// either side ("Attaches Vines to enemies within 1.5 metres"); Shield Wall's has no text
    /// and is taken for the width too.
    let private strips = Map.ofList [ ("Entangle", "Fissures"), 2.0; ("Primal Strikes", "Spirits"), 1.0; ("Shield Wall", "Shield Wall"), 1.0 ]

    /// Stat sets that strike one enemy in their area with an impact of the secondary radius
    /// ("Storm radius is", "to Impact radius"; "Hits one target every X second").
    let private impactIsSecondary = set [ "Thunderstorm", "Thunderstorm" ]

    /// Skills whose `base_secondary_skill_effect_duration` is read where their mechanic is built.
    let private secondaryDurationRead = set [ "Freezing Mark"; "Frost Bomb"; "Kelari's Malediction" ]

    let private conditionMods (skill: string) (label: string) (isMain: bool) : Mod list =
        (Map.tryFind (skill, label) partConditions |> Option.defaultValue [])
        @ (if isMain then Map.tryFind (skill, "") partConditions |> Option.defaultValue [] else [])
        |> List.collect (fun c ->
            if c.StartsWith "Vs:" then
                let states = c.Substring 3
                if states.EndsWith "!" then [ Mod.flag (Named("OnlyVs:" + states.TrimEnd '!')); Mod.flag (Named "ConsumesVs") ] else [ Mod.flag (Named("OnlyVs:" + states)) ]
            elif c.StartsWith "NotVs:" || c.StartsWith "Takes:" || c.StartsWith "WarpsInto:" then [ Mod.flag (Named c) ]
            elif c.StartsWith "Seq:" then
                let parts = (c.Substring 4).Split '/'
                [ Mod.flag (Named("SeqSteps:" + parts.[0])); Mod.base' (Named "SeqLength") (float parts.[1]) ]
            elif c.StartsWith "Less:" then [ Mod.more (Damage AllDamage) (-(float (c.Substring 5))) ]
            elif c = "NotWithCharge" || c = "NotEnraged" || c = "DetonatesCurse" then [ Mod.flag (Named c) ]
            elif c.StartsWith "SpendsRage:" then [ Mod.base' (Named "SpendsRage") (float (c.Substring 11)) ]
            elif c = "OnKill" then [ Mod.flag (Named "OnlyTriggered"); Mod.flag (Named "TriggeredOnKill") ]
            elif c = "Once" then [ Mod.flag (Named "OncePerUse") ]
            elif c.StartsWith "Hits:" then [ Mod.base' (Named "SeqHits") (float (c.Substring 5)) ]
            elif c = "Charge" then [ Mod.flag (Named "OnlyWithCharge") ]
            elif c.StartsWith "Infused:" then [ Mod.flag (Named("OnlyInfused:" + c.Substring 8)) ]
            elif c.StartsWith "DetonatedBy:" then [ Mod.flag (Named c) ]
            elif c.StartsWith "NeedsStacks:" then
                let parts = c.Split ':'
                [ Mod.base' (Named("NeedsEnemyStacks:" + parts.[1])) (float parts.[2]) ]
            elif c.StartsWith "Consumes:" then [ Mod.flag (Named c); Mod.base' (Named "ConsumeChargeCount") 1.0; Mod.flag (Named "NoCooldownWithCharge") ]
            elif c = "Crystals" then [ Mod.flag (Named "DetonatedBy:CrystalBreak"); Mod.flag (Named "Crystals") ]
            elif c = "NoHit" then [ Mod.flag (Named "NoHitDamage") ]
            elif c = "Ignites" then [ Mod.base' (AilmentChance Ignite) 100.0 ]
            else [])

    /// One stat set of a skill as a simulator Skill.
    /// Does a type expression (postfix: types, "AND", "OR", "NOT") hold for a skill's types?
    let private typesMatch (expr: string list) (types: Set<string>) =
        let stack =
            expr |> List.fold (fun (st: bool list) tok ->
                match tok, st with
                | "AND", a :: b :: rest -> (a && b) :: rest
                | "OR", a :: b :: rest -> (a || b) :: rest
                | "NOT", a :: rest -> (not a) :: rest
                | ("AND" | "OR" | "NOT"), _ -> st
                | t, _ -> types.Contains t :: st) []
        stack |> List.exists id

    /// Can this support gem support a skill with these types? It needs one of its required
    /// types (none required: any skill) and none of its excluded ones. The types known here
    /// are those of a gem's first effect only (a buff's triggered effect or a minion's skills
    /// have their own), so a "no" is only trusted where another gem in the group says "yes".
    let canSupport (support: SkillData) (types: Set<string>) =
        (support.RequireTypes.IsEmpty || typesMatch support.RequireTypes types)
        && not (not support.ExcludeTypes.IsEmpty && typesMatch support.ExcludeTypes types)

    /// The setup of a skill that comes with a gem (a support's triggered skill, a gem's second
    /// effect): the gem's level and quality, and of its supports those that can support a
    /// skill of these types (Spell Cascade needs a Cascadable spell: the curse, not the Doom
    /// Blast it sets off).
    let granted (db: GameDb) (setup: GemSetup) (id: string) : GemSetup =
        match Map.tryFind id db.Skills with
        | Some d ->
            { setup with
                Gem = id
                Supports =
                    setup.Supports |> List.filter (fun (name, _) ->
                        // (the support that grants the skill is the skill's)
                        let grants = db.GemVariants |> Map.tryFind name |> Option.defaultValue [] |> List.exists (fun g -> List.contains id g.AdditionalEffects)
                        match supportOfGem db name with
                        | Some s when not grants -> canSupport s d.Types
                        | _ -> true) }
        | None -> { setup with Gem = id }

    let private buildSet altQuality (data: SkillData) (set: StatSetData) name tags level quality (supportStats: (string * float) list) (weapon: Weapon option) (shieldArmour: float, shieldEvasion: float) (stageCap: float) =
        let gemLevel = Map.tryFind level data.Levels
        let ownStats = statsWith altQuality data set level quality
        // the skill's internal name, from all its stat sets
        let prefix = skillPrefix (data.StatSets |> List.collect (fun st -> st.Stats @ (st.ConstantStats |> List.map fst)))
        let ownMods, ownCov = interpret (Some data.Types) prefix ownStats
        // the skill's own conversion is the first of the two conversion steps
        let ownMods = ownMods |> List.map (fun m -> match m.Stat with Convert (a, b) -> { m with Stat = SkillConvert(a, b) } | _ -> m)
        let supportMods, supportCov = interpret None None supportStats
        let mods, cov = ownMods @ supportMods, Coverage.merge ownCov supportCov
        let isAttack = data.Types.Contains "Attack"
        // Shield attacks (Resonating Shield, Shield Charge, Raise Shield, Parry) hit with the
        // shield: "off_hand_weapon_minimum/maximum_physical_damage" plus
        // "off_hand_..._added_physical_damage_per_N_shield_armour" (or "..._evasion_rating").
        let shieldDamage =
            let sum (pred: string -> bool) = ownStats |> List.filter (fun (id, _) -> pred id) |> List.sumBy snd
            let per (bound: string) =
                ownStats |> List.sumBy (fun (id, v) ->
                    let prefix = "off_hand_" + bound + "_added_physical_damage_per_"
                    if id.StartsWith prefix then
                        let rest = id.Substring(prefix.Length).Split('_')
                        match System.Double.TryParse rest.[0] with
                        | true, n when n > 0.0 -> v * floor ((if id.Contains "evasion" then shieldEvasion else shieldArmour) / n)
                        | _ -> 0.0
                    else 0.0)
            let lo = sum ((=) "off_hand_weapon_minimum_physical_damage") + per "minimum"
            let hi = sum ((=) "off_hand_weapon_maximum_physical_damage") + per "maximum"
            if hi > 0.0 then Some(Map.ofList [ Physical, { Min = lo; Max = hi } ]) else None
        let weapon = if shieldDamage.IsSome then None else weapon
        // An attack that brings its own damage ("main_hand_weapon_minimum_fire_damage": thrown
        // flasks, runes, a herald's explosion): those numbers stand in for the weapon's, with the
        // skill's own attack time and critical hit chance.
        let ownWeaponDamage =
            let pick (bound: string) =
                ownStats |> List.choose (fun (id, v) ->
                    let prefix = "main_hand_weapon_" + bound + "_"
                    if id.StartsWith prefix && id.EndsWith "_damage" then
                        DamageType.all |> List.tryFind (fun t -> (DamageType.name t).ToLowerInvariant() = id.Substring(prefix.Length, id.Length - prefix.Length - 7)) |> Option.map (fun t -> t, v)
                    else None)
            let lo, hi = pick "minimum", pick "maximum"
            hi |> List.filter (fun (_, v) -> v > 0.0)
            |> List.map (fun (t, v) -> t, { Min = lo |> List.sumBy (fun (t2, x) -> if t2 = t then x else 0.0); Max = hi |> List.sumBy (fun (t2, x) -> if t2 = t then x else 0.0) })
            |> Map.ofList
        let ownWeapon = isAttack && not ownWeaponDamage.IsEmpty
        let weapon = if ownWeapon then None else weapon
        let baseDamage =
            if shieldDamage.IsSome then shieldDamage.Value
            elif ownWeapon then ownWeaponDamage
            elif isAttack then
                match weapon with
                // Hollow Palm Technique: unarmed physical damage of the skill's level
                | Some w when List.contains (Tag "HollowPalm") w.Tags ->
                    let lo, hi = Constants.hollowPalmDamage.[max 0 (min (Constants.hollowPalmDamage.Length - 1) (level - 1))]
                    Map.ofList [ Physical, { Min = lo; Max = hi } ]
                | _ -> weapon |> Option.map (fun w -> w.Damage) |> Option.defaultValue Map.empty
            else
                // a stat listed more than once adds up (infused sets list base damage twice)
                ownStats
                |> List.choose (fun (id, v) -> baseDamageStat id |> Option.map (fun (isMin, t) -> t, isMin, v))
                |> List.groupBy (fun (t, _, _) -> t)
                |> List.map (fun (t, xs) ->
                    let total m = xs |> List.filter (fun (_, isMin, _) -> isMin = m) |> List.sumBy (fun (_, _, v) -> v)
                    t, { Min = total true; Max = total false })
                |> List.filter (fun (_, r) -> r.Max > 0.0)
                |> Map.ofList
        let setLevel = Map.tryFind level set.Levels
        let baseMultiplier =
            setLevel |> Option.bind (fun l -> l.BaseMultiplier)
            |> Option.orElse (gemLevel |> Option.bind (fun l -> l.BaseMultiplier))
            |> Option.defaultValue 1.0
        let crit =
            if isAttack && not ownWeapon then weapon |> Option.map (fun w -> w.CritChance) |> Option.defaultValue 5.0
            else
                setLevel |> Option.bind (fun l -> l.CritChance)
                |> Option.orElse (gemLevel |> Option.bind (fun l -> l.CritChance))
                |> Option.defaultValue 0.0
        let time =
            if isAttack then
                let aps =
                    match ownStats |> List.tryPick (fun (id, v) -> if id = "main_hand_base_weapon_attack_duration_ms" then Some v else None) with
                    | Some ms when ownWeapon && ms > 0.0 -> 1000.0 / ms
                    | _ -> weapon |> Option.map (fun w -> w.AttacksPerSecond) |> Option.defaultValue 1.0
                let speedMult = 1.0 + (gemLevel |> Option.bind (fun l -> l.AttackSpeedMultiplier) |> Option.defaultValue 0.0) / 100.0
                match gemLevel |> Option.bind (fun l -> l.AttackTime) with
                | Some t -> t / 1000.0
                | None -> 1.0 / (aps * speedMult)
            else
                data.CastTime
        let statOf id =
            match ownStats |> List.filter (fst >> (=) id) with
            | [] -> None
            | xs -> Some(xs |> List.sumBy snd)
        let flagOf id = statOf id |> Option.exists (fun v -> v <> 0.0)
        let allStats = ownStats @ supportStats
        let flagAny id = allStats |> List.exists (fun (s, v) -> s = id && v <> 0.0)
        // Channelled skills that gain stages (Flameblast, Supercharged Slam, Detonating Arrow,
        // Volcano): one use is the channel to full stages and the release. Each stage takes the
        // skill's own time; the release carries the per-stage damage and radius.
        let stages =
            if not (data.Types.Contains "Channel") then 0.0
            else
                ownStats
                |> List.tryPick (fun (id, v) ->
                    if [ "_maximum_stages"; "_max_stages"; "_max_number_of_stages"; "_maximum_number_of_stages" ] |> List.exists (fun (e: string) -> id.EndsWith e) then Some v else None)
                |> Option.defaultValue 0.0
        let staged = stages > 1.0
        // (held for fewer stages where the mana does not pay for all of them: `Setup.pressed`)
        let stages = if staged && stageCap >= 1.0 then min stages stageCap else stages
        let perStage (endings: string list) =
            ownStats |> List.filter (fun (id, _) -> endings |> List.exists (fun (e: string) -> id.EndsWith e)) |> List.sumBy snd
        let stageMods =
            if not staged then []
            else
                let more = perStage [ "damage_+%_final_per_stage"; "damage_+%_final_per_stack" ] * stages + perStage [ "damage_+%_final_per_additional_stage" ] * (stages - 1.0)
                let gainFire = perStage [ "all_damage_%_to_gain_as_fire_per_stage" ] * stages
                [ yield Mod.base' (Named "ChannelStages") stages |> Mod.from "stages"
                  if more <> 0.0 then yield Mod.more (Damage AllDamage) more |> Mod.from "stages"
                  if gainFire > 0.0 then
                      for t in DamageType.all do
                          yield Mod.base' (GainAs(t, Fire)) gainFire |> Mod.from "stages" ]
        let mods = mods @ stageMods
        let time =
            if staged then time * stages + (statOf "channel_skill_end_animation_duration_override_ms" |> Option.defaultValue 0.0) / 1000.0 else time
        // A channelled skill with a cooldown (Flame Breath) is held for its minimum channel time:
        // one use lasts that long and hits once per its own time throughout.
        let minChannel = (statOf "base_minimum_channel_time_ms" |> Option.defaultValue 0.0) / 1000.0
        let held = data.Types.Contains "Channel" && not staged && time > 0.0 && minChannel > time && (gemLevel |> Option.bind (fun l -> l.Cooldown)).IsSome
        let heldHits = if held then (let n = max 1 (int (minChannel / time)) in [ for k in 1 .. n -> float k / float n, 1.0 ]) else [ 1.0, 1.0 ]
        let time = if held then minChannel else time
        // Perfect Timing: the skill's attack time is the channel up to the perfect moment; the
        // release that follows is a share of it
        let time =
            if data.Types.Contains "PerfectTiming" then
                let release = (statOf "channel_end_duration_as_%_of_attack_time" |> Option.defaultValue 0.0) / 100.0
                let anim = statOf "channel_skill_end_animation_duration_multiplier_permyriad" |> Option.map (fun v -> v / 10000.0) |> Option.defaultValue 1.0
                time * (1.0 + release * anim)
            else time
        let radius =
            let r = statOf "active_skill_base_area_of_effect_radius"
            let extra = if staged then perStage [ "radius_+_per_stage" ] * stages else 0.0
            if extra > 0.0 then Some((r |> Option.defaultValue 0.0) + extra) else r
        let perCharge =
            ownStats
            |> List.tryPick (fun (id, v) -> if id.EndsWith "_number_of_projectiles_per_power_charge" then Some("PowerCharge", int v) else None)
        let isProjectileSet = List.contains "base_is_projectile" set.Stats
        let secondary = statOf "active_skill_base_secondary_area_of_effect_radius"
        // things that burst out of the ground all over the skill's area and wait there
        // ("Creates 1 Spear per N metres of Radius"): each at a random spot, going off (secondary
        // radius) when an enemy touches it
        let field =
            match Map.tryFind (data.Name, set.Label) partConditions |> Option.exists (List.contains "Field"), radius, (data.StatSets |> List.tryPick (fun st -> statsWith altQuality data st level quality |> List.tryPick (fun (k, v) -> if k = "spear_wall_cm_per_spear" then Some v else None))), statOf "active_skill_base_secondary_area_of_effect_radius" with
            | true, Some r, Some cm, Some blast when cm > 0.0 ->
                Some(Projectiles { ProjectileShape.make (max 1 (int (r / (cm / 10.0)))) with Rain = Some(r, blast); RainCone = Constants.spearfieldCone; TouchTriggered = true; FromImpact = true })
            | _ -> None
        let delivery =
            if field.IsSome then field.Value
            elif isProjectileSet then
                // shards sprayed on from the enemy the projectile hit
                let behind = Map.tryFind (data.Name, set.Label) partConditions |> Option.exists (List.contains "Behind")
                let rains = Map.tryFind (data.Name, set.Label) partConditions |> Option.exists (List.contains "Rain")
                let count = statOf "base_number_of_projectiles" |> Option.map int |> Option.defaultValue (if perCharge.IsSome then 0 else 1)
                Projectiles
                    { ProjectileShape.make count with
                        // "projectiles_nova": fired in a full circle
                        // "projectiles_barrage": fired one after the other at the target, so each can hit it
                        // "travel erratically along the ground": they stray within
                        // "spark_pathing_width_bounds", as far as speed x lifetime takes them, in a
                        // fan that widens with their number
                        Wander = statOf "spark_pathing_width_bounds" |> Option.defaultValue (if (statOf "twister_base_movement_speed").IsSome then Constants.twisterWander else 0.0)
                        // (a twister "bounces" off walls in any direction: twister_bounce_check_angle 360)
                        Bounces = flagOf "spark_projectile_bounces_off_walls" || flagOf "twister_bounce_check_angle"
                        Speed =
                            (if (statOf "spark_pathing_width_bounds").IsSome then Map.tryFind data.Name Constants.erraticProjectileSpeed |> Option.defaultValue 0.0
                             else
                                 // a twister's own speed ("twister_base_movement_speed", units per second)
                                 statOf "twister_base_movement_speed"
                                 |> Option.orElse (Map.tryFind data.Name Constants.passingProjectileSpeed)
                                 |> Option.defaultValue 0.0)
                        // "The Projectile itself does not Hit enemies, but repeatedly discharges bolts
                        // of Lightning at nearby enemies": within the skill's radius of where it is
                        // "repeatedly Hitting enemies within": what is inside the twister as it moves
                        Passing =
                            (if (Map.containsKey data.Name Constants.passingProjectileSpeed && set.Label = data.Name) || (statOf "twister_base_movement_speed").IsSome
                             then radius |> Option.defaultValue 0.0 else 0.0)
                        Range =
                            (match statOf "spark_pathing_width_bounds", Map.tryFind data.Name Constants.erraticProjectileSpeed, statOf "base_skill_effect_duration" with
                             | Some _, Some speed, Some ms -> speed * ms / 1000.0
                             | _ when (statOf "twister_base_movement_speed").IsSome ->
                                 // it lives for its duration and gets as far as its speed takes it
                                 (statOf "twister_base_movement_speed").Value * (statOf "base_skill_effect_duration" |> Option.defaultValue 0.0) / 1000.0
                             | _ ->
                                 // shards that live for a fixed time fly as far as their speed takes them
                                 match (if behind then Map.tryFind data.Name Constants.timedProjectiles else None) with
                                 | Some (speed, life) -> speed * life
                                 | None -> (ProjectileShape.make 1).Range)
                        RangeVariation = (if behind then (statOf "active_skill_projectile_speed_+%_variation_final" |> Option.defaultValue 0.0) / 100.0 else 0.0)
                        SpreadPerProjectile = (if (statOf "spark_pathing_width_bounds").IsSome then Constants.erraticFanStep else statOf "minimum_per_projectile_spread_angle_degrees" |> Option.defaultValue 0.0)
                        // "can Hit the same target no more than once every N seconds"; a rain: "Can Hit
                        // each enemy only once every N seconds", one arrow every so often
                        HitInterval = ((statOf "spark_min_time_between_target_clearing_ms" |> Option.defaultValue 0.0) + (statOf "rain_hit_delay_ms" |> Option.defaultValue 0.0) + (statOf "twister_hit_interval_ms" |> Option.defaultValue 0.0)) / 1000.0
                        Every = (statOf "rain_of_arrows_delay_per_arrow" |> Option.defaultValue 0.0) / 1000.0
                        Spread = (if flagAny "projectiles_nova" then 360.0 elif flagOf "projectiles_barrage" then 0.0 else (statOf "total_projectile_spread_angle_override" |> Option.map (fun v -> v * Constants.spreadOverrideDegrees) |> Option.defaultValue 30.0))
                        Pierce = (if flagOf "always_pierce" then 100 else int (statOf "projectile_base_number_of_targets_to_pierce" |> Option.defaultValue 0.0))
                        Chains = int (statOf "number_of_chains" |> Option.defaultValue 0.0)
                        // projectiles that drop on their targets, one each, in a number the skill
                        // sets itself (Dark Effigy's "one per afflicted enemy, up to 100"): never two on one enemy
                        // "If multiple projectiles are fired simultaneously, only one of them can
                        // collide with the same target, unless otherwise stated" (wiki, Projectile).
                        // Stated: "These fragments can Merge", "Multiple Projectiles can hit the same
                        // enemy", "fired in sequence"; and what springs from an impact or rains down
                        CanShotgun =
                            // (the rule is for a volley leaving the player, "ProjectilesFromUser": embers
                            // hovering overhead, grenades and what drops from the sky land one by one)
                            (flagOf "projectiles_barrage" || flagAny "projectiles_can_shotgun" || behind || rains || not (data.Types.Contains "ProjectilesFromUser")
                             || [ "can Merge"; "can hit the same"; "Hit the same target"; "in sequence" ] |> List.exists data.Description.Contains)
                            && not (data.Types.Contains "ProjectileNoCollision" && data.Types.Contains "ProjectilesNumberModifiersNotApplied")
                            // Spiral Volley: "Each target can only be Hit once"
                            && not (data.Description.Contains "Each target can only be Hit once")
                        PerCharge = perCharge
                        FromImpact = data.Types.Contains "ProjectilesNotFromUser" || behind
                        SkipPrimary = behind
                        // a rain over the skill's area, each projectile with its own small impact
                        Rain =
                            match statOf "active_skill_base_area_of_effect_radius", statOf "active_skill_base_secondary_area_of_effect_radius" with
                            | Some a, Some b when (count >= 2 && data.Types.Contains "Rain" && b < a) || rains -> Some(max a b, min a b)
                            | _ -> None
                        Explosion = if data.Types.Contains "Area" && not (List.contains "is_area_damage" set.Stats) then radius else None }
            else
                // a part whose only radius is the skill's secondary one ("Explosion radius is",
                // "Shockwave radius is" in its own description): that is its area
                match radius |> Option.orElse secondary with
                | Some r when secondary.IsSome && strips.ContainsKey(data.Name, set.Label) -> Cone(r, 0.0, strips.[data.Name, set.Label] * secondary.Value)
                | Some _ when secondary.IsSome && impactIsSecondary.Contains(data.Name, set.Label) -> AreaAtTarget secondary.Value
                // a nova is around the caster (Ice Nova too: it is cast on a Frostbolt only when one
                // is flying, user observation), unless Astral Projection puts it at the target
                | Some r when data.Types.Contains "Nova" && flagAny "nova_skills_cast_at_target_location" -> AreaAtTarget r
                | Some r when data.Types.Contains "Nova" -> AreaAroundSelf r
                | Some r -> AreaAtTarget r
                // "Supported Skills deal Splash Damage to surrounding targets", "Supported Minions'
                // Strikes have Melee Splash": the strike also hits the other enemies around the one it hit
                | _ when (flagAny "melee_splash" || flagAny "minion_melee_splash") && data.Types.Contains "Melee" -> AreaAtTarget Constants.splashRadius
                | _ -> Single
        { Name = name
          Tags = tags
          BaseDamage = baseDamage
          BaseMultiplier = baseMultiplier
          AddedDamageEffectiveness = 1.0
          BaseCrit = crit
          BaseTime = time
          Cooldown = gemLevel |> Option.bind (fun l -> l.Cooldown)
          StoredUses = gemLevel |> Option.bind (fun l -> l.StoredUses) |> Option.map int |> Option.defaultValue 1
          // a skill that comes down in several volleys (Death From Above: six) hits with each
          Hits =
            (match ownStats |> List.tryPick (fun (id, v) -> if id.EndsWith "_number_of_volleys" && v > 1.0 then Some(int v) else None) with
             | Some n when not held ->
                 // "..._volley_delay_ms" apart, the first as the use ends (as shares of the use time)
                 match ownStats |> List.tryPick (fun (id, v) -> if id.EndsWith "_volley_delay_ms" && v > 0.0 then Some(v / 1000.0) else None) with
                 | Some delay when time > 0.0 -> [ for k in 1 .. n -> 1.0 + float (k - 1) * delay / time, 1.0 ]
                 | _ -> [ for k in 1 .. n -> float k / float n, 1.0 ]
             | _ -> heldHits)
          Delivery = delivery
          // A part that only poisons ("display_statset_no_hit_damage", "skill poisons without
          // hit": Poisonburst Arrow's burst): it deals no hit damage, always poisons for what
          // the hit would have dealt, and "poison duration is skill duration"
          Mods =
            (let has id = set.Stats |> List.contains id
             let burstDuration = (statOf "base_skill_effect_duration" |> Option.defaultValue 0.0) / 1000.0
             mods
             @ (if has "display_statset_no_hit_damage" && has "display_skill_poisons_without_hit" then
                    [ Mod.flag (Named "NoHitDamage") |> Mod.whenever (SkillNamed name); Mod.base' (AilmentChance Poison) 100.0 |> Mod.whenever (SkillNamed name) ]
                else [])
             // (all only for this part's own hits: a buff gem's modifiers go to the character)
             @ (conditionMods data.Name set.Label (List.tryHead data.StatSets = Some set) |> List.map (Mod.whenever (SkillNamed name)))
             // a set that says it deals no damage ("base_deal_no_damage") beside sets that do
             @ (if has "base_deal_no_damage" && data.StatSets.Length > 1 && not (data.Types.Contains "Buff") && not (data.Types.Contains "Meta")
                then [ Mod.flag (Named "NoHitDamage") |> Mod.whenever (SkillNamed name) ] else [])
             // "Consumes a Frenzy Charge if possible": one charge per use; "Consume 3 Endurance
             // Charges": three; "Consumes your Power Charges", "Consume all": every one held
             @ (let words = data.Description.Split([| ' '; ','; '.' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
                let rec find (ws: string list) =
                    match ws with
                    | ("Consumes" | "Consume") :: n :: kind :: charge :: _ when (kind = "Power" || kind = "Frenzy" || kind = "Endurance") && charge.StartsWith "Charge" ->
                        (match n with
                         | "a" | "an" -> Some 1.0
                         | _ -> (match System.Double.TryParse n with | true, v -> Some v | _ -> None))
                    | _ :: rest -> find rest
                    | [] -> None
                // "..._consume_power_charge_per_X_ms": one charge for so long of the channel
                match ownStats |> List.tryPick (fun (k, v) -> if k.EndsWith "_consume_power_charge_per_X_ms" && v > 0.0 then Some v else None), find words with
                | Some ms, _ -> [ Mod.base' (Named "ConsumeChargeCount") 1.0; Mod.base' (Named "ConsumeChargeEveryMs") ms ]
                | None, Some n -> [ Mod.base' (Named "ConsumeChargeCount") n ]
                | None, None -> [])
             // a stat set for one state of the enemy ("Vs Frozen", "Vs Armour Broken", "Vs Dazed")
             @ (if set.Label.StartsWith "Vs " then
                    let state = match set.Label.Substring 3 with "Armour Broken" -> "FullyBrokenArmour" | other -> other.Replace(" ", "")
                    [ Mod.flag (Named("OnlyVs:" + state)) ]
                    @ (if data.Types.Contains "SkillConsumesFreeze" || data.Types.Contains "ConsumesFullyBrokenArmour" then [ Mod.flag (Named "ConsumesVs") ] else [])
                else [])
             @ (if has "poison_duration_is_skill_duration" && burstDuration > 0.0 then [ Mod.override' (Named "BasePoisonDuration") burstDuration ] else []))
          Parts = []
          Consumes =
            [ "Power"; "Frenzy"; "Endurance" ]
            |> List.tryFind (fun kind -> data.Types.Contains(sprintf "SkillConsumes%sChargesOnUse" kind))
            |> Option.map (fun kind -> kind + "Charge")
            // or something else the description names (a Fragment of Divinity)
            |> Option.orElse (Map.tryFind (data.Name, "") partConditions |> Option.bind (List.tryPick (fun (c: string) -> if c.StartsWith "Consumes:" then Some(c.Substring 9) else None)))
            // "If a twister touches a Whirlwind from your other skills, it Consumes the Whirlwind"
            |> Option.orElse (if ownStats |> List.exists (fun (k, _) -> k = "twister_consumes_wind_storm") then Some "Whirlwind" else None)
          Cost = gemLevel |> Option.bind (fun l -> l.ManaCost) |> Option.defaultValue 0.0
          // "comet_fall_time_ms": hits land this long after the cast
          HitDelay =
            (ownStats |> List.filter (fun (id, _) -> id.EndsWith "_fall_time_ms") |> List.sumBy snd) / 1000.0
            // a grenade, a Frost Bomb: "explodes when its fuse expires", "When the Orb's duration
            // ends, it Detonates": the hit comes after the detonation time (x Fast Forward's less)
            + (if data.Types.Contains "DetonatesAfterTime" && (data.Types.Contains "Grenade" || data.Name = "Frost Bomb") then
                   (statOf "base_skill_detonation_time" |> Option.defaultValue 0.0) / 1000.0
                   * (1.0 + (allStats |> List.filter (fun (id, _) -> id = "support_fast_forward_detonation_time_+%_final") |> List.sumBy snd) / 100.0)
               else 0.0)
          // "base_chaos_damage_to_deal_per_minute": damage over time on what the skill hits
          Dots =
            DamageType.all
            |> List.choose (fun t ->
                let id = sprintf "base_%s_damage_to_deal_per_minute" ((DamageType.name t).ToLowerInvariant())
                match statOf id with
                | Some v when v > 0.0 -> Some(t, v / 60.0)
                | _ -> None)
            |> Map.ofList
          Duration = (statOf "base_skill_effect_duration" |> Option.defaultValue 0.0) / 1000.0
          Clip = 0
          ReloadTime = 0.0
          CostPerSecond = (gemLevel |> Option.bind (fun l -> l.ManaPerMinute) |> Option.defaultValue 0.0) / 60.0 },
        // the secondary radius is read where this set's delivery is built from it: a field's
        // blasts, a rain's impacts, a part with no other radius
        (let used =
            secondary.IsSome
            && (field.IsSome
                || (match delivery with
                    | Projectiles { Rain = Some _ } -> true
                    | Projectiles _ -> false
                    | _ -> radius.IsNone || strips.ContainsKey(data.Name, set.Label) || impactIsSecondary.Contains(data.Name, set.Label)))
         let cov = if used then { cov with Unsimulated = cov.Unsimulated |> List.filter (fun (k, _) -> k <> "active_skill_base_secondary_area_of_effect_radius") } else cov
         // a staged channel's release animation is part of its use (above)
         let cov = if stages > 0.0 then { cov with Unsimulated = cov.Unsimulated |> List.filter (fun (k, _) -> k <> "channel_skill_end_animation_duration_override_ms") } else cov
         // the secondary duration is read where the skill's mechanic is built: Freezing Mark's
         // buff (Behaviours.markBuff), Frost Bomb's exposure (useRules), the time between Kelari's
         // Malediction's corpses (summons)
         let cov = if secondaryDurationRead.Contains data.Name then { cov with Unsimulated = cov.Unsimulated |> List.filter (fun (k, _) -> k <> "base_secondary_skill_effect_duration") } else cov
         // the detonation time is read where the hit waits for it (HitDelay above) and for Toxic Domain's pustule
         if (data.Types.Contains "DetonatesAfterTime" && (data.Types.Contains "Grenade" || data.Name = "Frost Bomb" || lodged.Contains data.Name)) || data.Name = "Toxic Domain" then
             { cov with Unsimulated = cov.Unsimulated |> List.filter (fun (k, _) -> k <> "base_skill_detonation_time" && k <> "support_fast_forward_detonation_time_+%_final") }
         else cov)

    /// Build a simulator Skill from a socketed gem. `characterMods` are needed for
    /// "+N to level of ... gems". Attacks need a weapon. Stat sets that belong to the
    /// same use become Parts of the skill.
    /// "Increases and reductions to Projectile speed also apply to Projectile Damage" (a
    /// support's, or a character's for its bows): what the character and the skill have of
    /// increased projectile speed, and for a spell that share of its increased cast speed
    /// which "also applies to Projectile Speed", is increased damage of the skill's
    /// projectiles. Worked out when the skill is built: what holds only for a while in a
    /// fight (a buff's cast speed) is not counted.
    /// "X% increased Grenade fuse duration": a grenade's hit comes that much later.
    let private fuse (characterMods: ModDb) (sk: Skill) : Skill =
        if sk.HitDelay <= 0.0 || not (sk.Tags.Contains(Tag "Grenade")) then sk
        else
            let ctx = { Ctx.empty with SkillName = sk.Name; SkillTags = sk.Tags }
            let k = Mods.inc ctx [ Named "GrenadeFuse" ] (ModDb.addList sk.Mods characterMods) * Mods.inc ctx [ Named "DetonationTime" ] (ModDb.addList sk.Mods characterMods)
            if k = 1.0 then sk else { sk with HitDelay = sk.HitDelay * max 0.0 k }

    let private speedAsDamage (characterMods: ModDb) (sk: Skill) : Skill =
        if not (sk.Tags.Contains Projectile) then sk
        else
            let ctx = { Ctx.empty with SkillName = sk.Name; SkillTags = sk.Tags }
            let db = ModDb.addList sk.Mods characterMods
            if not (Mods.flag ctx (Named "ProjectileSpeedAsDamage") db) then sk
            else
                let fromCast = if sk.Tags.Contains Spell then Mods.sumInc ctx [ Speed ] db * Mods.sumBase ctx [ Named "CastSpeedAsProjectileSpeed" ] db / 100.0 else 0.0
                let speed = Mods.sumInc ctx [ Named "ProjectileSpeed" ] db + fromCast
                if speed = 0.0 then sk else { sk with Mods = sk.Mods @ [ Mod.inc (Damage AllDamage) speed |> Mod.from "projectile speed" ] }

    let rec skill (db: GameDb) (characterMods: ModDb) (weapon: Weapon option) (setup: GemSetup) : Result<Skill * Coverage, string> =
        match skillOfGemFor db setup.Gem weapon with
        | None -> Error(sprintf "unknown gem %s" setup.Gem)
        | Some ammo when ammo.Types.Contains "CrossbowAmmoSkill" ->
            // A crossbow gem grants "Load X" (bolts per load, the cost) and the shot itself
            // (gems.json additionalGrantedEffectId1): the shot is what is used, the load is
            // its ammunition. Reloading takes the weapon's reload time.
            match shotOf db setup.Gem with
            | None -> Error(sprintf "%s has no shot skill" setup.Gem)
            | Some shot ->
                let shot = { shot with Levels = shot.Levels |> Map.map (fun lvl l -> { l with ManaCost = Map.tryFind lvl ammo.Levels |> Option.bind (fun a -> a.ManaCost) }) }
                skillFromData db characterMods weapon setup shot
                |> Result.map (fun (s, cov) ->
                    let supportBolts =
                        setup.Supports |> List.sumBy (fun (name, lvl) ->
                            match supportOfGem db name with
                            | Some sd when sd.Support -> sd.StatSets |> List.tryHead |> Option.map (fun set -> stats sd set lvl 0 |> List.sumBy (fun (id, v) -> if id.Contains "number_of_crossbow_bolts" then v else 0.0)) |> Option.defaultValue 0.0
                            | _ -> 0.0)
                    let bolts =
                        ammo.StatSets |> List.tryHead
                        |> Option.map (fun set -> stats ammo set setup.Level 0 |> List.sumBy (fun (id, v) -> if id = "base_number_of_crossbow_bolts" then v else 0.0))
                        |> Option.defaultValue 1.0
                    let ammoStats = ammo.StatSets |> List.tryHead |> Option.map (fun set -> stats ammo set setup.Level 0) |> Option.defaultValue []
                    let pick (ending: string) = ammoStats |> List.tryPick (fun (id, v) -> if id.EndsWith ending && v > 0.0 then Some v else None)
                    match pick "_active_duration_ms", pick "_buff_count_requirement", pick "_number_of_stacks_per_minute" with
                    | Some ms, Some needed, Some perMinute when ammoStats |> List.exists (fun (id, v) -> id = "crossbow_ammo_has_no_ammo" && v <> 0.0) ->
                        // "Does not use Ammunition": it works for a fixed time once enough has built
                        // up ("Requires 100 anguish to use", "Accumulates 3.33 anguish per second")
                        { s with
                            Clip = 0
                            ReloadTime = 0.0
                            Mods = Mod.base' (Named "WindowSeconds") (ms / 1000.0) :: Mod.base' (Named "WindowRecharge") (needed / (perMinute / 60.0)) :: s.Mods }, cov
                    | _ when ammoStats |> List.exists (fun (id, v) -> id = "crossbow_ammo_skill_has_passive_reloading" && v <> 0.0) ->
                        // "Passively generate bolts ... at a frequency equal to reload time, up to a cap.
                        // ... All loaded bolts are fired at once": no clip; what has built up is the
                        // number of projectiles, and all of it is spent
                        { s with
                            Clip = 0
                            ReloadTime = 0.0
                            Consumes = Some "StoredBolts"
                            Delivery = (match s.Delivery with Projectiles p -> Projectiles { p with Count = 0; PerCharge = Some("StoredBolts", 1) } | d -> d)
                            Mods =
                                Mod.base' (Named "StoredBoltsMax") (bolts + supportBolts)
                                :: Mod.base' (Named "StoredBoltEvery") (weapon |> Option.map (fun w -> w.ReloadTime) |> Option.defaultValue 0.0)
                                :: Mod.flag (Named "NeedsCharge") :: s.Mods }, cov
                    | _ ->
                    { s with Clip = max 1 (int (bolts + supportBolts)); ReloadTime = weapon |> Option.map (fun w -> w.ReloadTime) |> Option.defaultValue 0.0 }, cov)
        | Some data -> skillFromData db characterMods weapon setup data

    /// A minion's skill: its values scale with the minion's level, not a gem level
    /// (stat sets with few levels use the actor's level, PoB buildSkillInstanceStats).
    and minionSkill (db: GameDb) (ownerMods: ModDb) (weapon: Weapon option) (skillId: string) (minionLevel: float) (supports: (string * int) list) =
        match Map.tryFind skillId db.Skills with
        | None -> Error(sprintf "unknown minion skill %s" skillId)
        | Some data ->
            let atLevel (set: StatSetData) =
                if set.Levels.Count < 5 then { set with Levels = set.Levels |> Map.map (fun _ l -> { l with ActorLevel = minionLevel }) } else set
            let data = { data with StatSets = data.StatSets |> List.map atLevel }
            skillFromData db ownerMods weapon { Gem = skillId; Level = 1; Quality = 0; StatSet = 1; Supports = supports } data

    /// Quality a skill gets from its supports (Dialla's Desire: "supported_active_skill_gem_quality_%").
    and supportQuality (db: GameDb) (setup: GemSetup) : int =
        setup.Supports
        |> List.sumBy (fun (name, lvl) ->
            match supportOfGem db name with
            | Some s when s.Support ->
                s.StatSets |> List.tryHead
                |> Option.map (fun set -> stats s set lvl 0 |> List.sumBy (fun (id, v) -> if id = "supported_active_skill_gem_quality_%" then v else 0.0))
                |> Option.defaultValue 0.0
            | _ -> 0.0)
        |> int

    /// Levels a skill gets from its supports: a Mastery's "+1 Level of supported Cold Skills",
    /// Dialla's Desire, Uhtred's lineage supports (those count the other supports beside them).
    and supportLevels (db: GameDb) (setup: GemSetup) : int =
        let others = setup.Supports.Length - 1
        let types = skillOfGem db setup.Gem |> Option.map (fun d -> d.Types) |> Option.defaultValue Set.empty
        // ("supported_cold_skill_gem_level_+": for skills of that type only)
        let fits (id: string) =
            [ "cold", "Cold"; "fire", "Fire"; "lightning", "Lightning"; "chaos", "Chaos"; "physical", "Physical"; "minion", "Minion" ]
            |> List.forall (fun (word, t) -> not (id.StartsWith("supported_" + word + "_")) || types.Contains t)
        setup.Supports
        |> List.sumBy (fun (name, lvl) ->
            match supportOfGem db name with
            | Some s when s.Support ->
                s.StatSets |> List.tryHead
                |> Option.map (fun set ->
                    stats s set lvl 0
                    |> List.sumBy (fun (id, v) ->
                        if not (id.StartsWith "supported_" && id.Contains "_skill_gem_level_+") || not (fits id) then 0.0
                        elif id.EndsWith "_if_no_other_supports" then (if others = 0 then v else 0.0)
                        elif id.EndsWith "_if_one_other_support" then (if others = 1 then v else 0.0)
                        elif id.EndsWith "_if_two_other_supports" then (if others = 2 then v else 0.0)
                        else v))
                |> Option.defaultValue 0.0
            | _ -> 0.0)
        |> int

    and private skillFromData (db: GameDb) (characterMods: ModDb) (weapon: Weapon option) (setup: GemSetup) (data: SkillData) : Result<Skill * Coverage, string> =
            match List.tryItem (setup.StatSet - 1) data.StatSets with
            | None -> Error(sprintf "%s has no stat set %d" setup.Gem setup.StatSet)
            | Some set ->
                let requirementTags =
                    match Map.tryFind setup.Gem db.Gems with
                    | Some g ->
                        let s, d, i = g.Requirements
                        [ if s > 0.0 then Tag "StrRequirement"
                          if d > 0.0 then Tag "DexRequirement"
                          if i > 0.0 then Tag "IntRequirement" ]
                    | None -> []
                let tags =
                    (data.Types |> Set.toList |> List.choose tagOfType)
                    @ (if data.Types.Contains "Attack" then weapon |> Option.map (fun w -> w.Tags) |> Option.defaultValue [] else [])
                    @ requirementTags
                    @ [ Tag("Gem:" + data.Name) ]
                    // every skill type, for conditions that name one ("Nova Skill Gems")
                    @ (data.Types |> Set.toList |> List.map (fun t -> Tag("Type:" + t)))
                    // "Spell damage modifiers apply to this skill's damage over time"
                    @ (if set.Stats |> List.contains "spell_damage_modifiers_apply_to_skill_dot" then [ Tag "SpellDot" ] else [])
                    |> Set.ofList
                let ctx = { Ctx.empty with SkillName = data.Name; SkillTags = tags }
                let level = setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods) + supportLevels db setup
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min level maxLevel
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods) + supportQuality db setup
                let supportStats, supportCov =
                    setup.Supports
                    |> List.map (fun (name, lvl) ->
                        match supportOfGem db name with
                        | Some s when s.Support ->
                            s.StatSets |> List.tryHead |> Option.map (fun set -> stats s set lvl 0) |> Option.defaultValue [], Coverage.empty
                        | _ -> [], { Coverage.empty with UnknownText = [ "unknown support: " + name ] })
                    |> List.unzip
                let supportStats = List.concat supportStats
                let alt = Mods.flag ctx (Named "AltQuality") characterMods
                let shield = Mods.sumBase Ctx.empty [ Named "ShieldArmour" ] characterMods, Mods.sumBase Ctx.empty [ Named "ShieldEvasion" ] characterMods
                let stageCap = Mods.sumBase ctx [ Named "StageCap" ] characterMods
                let main, cov = buildSet alt data set data.Name tags level quality supportStats weapon shield stageCap
                let parts, partCov =
                    data.StatSets
                    // a set shown with the main one, or the one that does the damage when the main
                    // set deals none ("dps_display_statset_index": Conductive Runes' explosion)
                    |> List.filter (fun s ->
                        s <> set
                        && (isPart s
                            || (Map.tryFind (data.Name, s.Label) partConditions |> Option.exists (List.exists (fun (c: string) -> c.StartsWith "Seq:" || c = "Part")))
                            || (set.Stats |> List.contains "base_deal_no_damage" && not (s.Label.EndsWith "-Infused")
                                && set.ConstantStats |> List.exists (fun (k, _) -> k = "dps_display_statset_index"))))
                    |> List.map (fun s ->
                        let p, c = buildSet alt data s (data.Name + ": " + s.Label) tags level quality supportStats weapon shield stageCap
                        { p with Consumes = None }, c)
                    // a spell set whose own damage is nothing at this level (Exsanguinate's "Damage over
                    // Time": 0 to 0; its damage over time is the Bleeding) is not a hit
                    |> List.filter (fun (p, _) ->
                        let declared = data.StatSets |> List.exists (fun s -> data.Name + ": " + s.Label = p.Name && s.Stats |> List.exists (fun (id: string) -> id.StartsWith "spell_minimum_base_"))
                        not (declared && not (data.Types.Contains "Attack") && p.Dots.IsEmpty && p.BaseDamage |> Map.forall (fun _ r -> r.Max <= 0.0)))
                    |> List.unzip
                // "Channelling this Skill for longer makes the initial eruption more violent": the
                // eruption's own spray has so many more projectiles per stage after the first
                let parts =
                    let own = statsWith alt data set level quality
                    let pick (ending: string) = own |> List.tryPick (fun (k, v) -> if k.EndsWith ending then Some v else None)
                    match pick "_initial_explosion_number_of_additional_projectiles_per_additional_stage", pick "_maximum_number_of_stages" with
                    | Some per, Some stages when per > 0.0 && stages > 1.0 ->
                        parts
                        |> List.collect (fun (part: Skill) ->
                            match part.Delivery with
                            | Projectiles shape when shape.Rain.IsSome ->
                                [ part
                                  { part with
                                      Name = data.Name + ": Eruption " + part.Name.Substring(data.Name.Length + 2)
                                      Delivery = Projectiles { shape with Count = int (per * (stages - 1.0)) }
                                      Mods = Mod.flag (Named "OncePerUse") :: part.Mods } ]
                            | _ -> [ part ])
                    | _ -> parts
                let cov = List.fold Coverage.merge cov (supportCov @ partCov)
                // supports multiply the cost: "Cost Multiplier 130%" is manaMultiplier 30
                let costMult =
                    setup.Supports
                    |> List.choose (fun (name, lvl) -> supportOfGem db name |> Option.bind (fun s -> Map.tryFind (max 1 lvl) s.Levels |> Option.orElse (Map.tryFind 1 s.Levels)))
                    |> List.fold (fun k l -> k * (1.0 + (l.ManaMultiplier |> Option.defaultValue 0.0) / 100.0)) 1.0
                let main, parts = speedAsDamage characterMods main |> fuse characterMods, parts |> List.map (speedAsDamage characterMods >> fuse characterMods)
                Ok({ main with Parts = parts; Cost = main.Cost * costMult; CostPerSecond = main.CostPerSecond * costMult }, cov)

    /// The game stats of a socketed gem (own stat set, quality, alt quality, supports),
    /// for behaviours that need raw numbers.
    /// The context mods on a gem are read in: its name and what its skill types say.
    let ctxOf (db: GameDb) (gem: string) : Ctx =
        let tags =
            match skillOfGem db gem with
            | Some data ->
                // ("Skills with an Intelligence requirement": by the gem's attribute requirements)
                let required =
                    match Map.tryFind gem db.Gems with
                    | Some g ->
                        let s, d, i = g.Requirements
                        [ if s > 0.0 then Tag "StrRequirement"
                          if d > 0.0 then Tag "DexRequirement"
                          if i > 0.0 then Tag "IntRequirement" ]
                    | None -> []
                (data.Types |> Set.toList |> List.choose tagOfType) @ (data.Types |> Set.toList |> List.map (fun t -> Tag("Type:" + t))) @ [ Tag("Gem:" + data.Name) ] @ required
            | None -> []
        { Ctx.empty with SkillName = gem; SkillTags = Set.ofList tags }

    /// The level a socketed gem really has: its own, what the character adds and what its supports
    /// add. (`weapons`: an attack is made with one of them, and what that weapon is counts: a
    /// weapon of the second set gives its skills what holds "in weapon set two".)
    let level (db: GameDb) (characterMods: ModDb) (weapons: Weapon list) (setup: GemSetup) : int =
        match skillOfGem db setup.Gem with
        | None -> setup.Level
        | Some data ->
            let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
            let ctx = ctxOf db setup.Gem
            let ctx =
                if not (data.Types.Contains "Attack") then ctx
                else
                    match chooseWeapon db setup.Gem weapons with
                    | Some w -> { ctx with SkillTags = Set.union ctx.SkillTags (Set.ofList w.Tags) }
                    | None -> ctx
            min maxLevel (setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods) + supportLevels db setup)

    /// Levels the character's mods add to a gem ("+ to Level of all Spell Skills").
    let bonusLevels (db: GameDb) (characterMods: ModDb) (gem: string) =
        int (Mods.sumBase (ctxOf db gem) [ Named "GemLevel" ] characterMods)

    let rawStats (db: GameDb) (characterMods: ModDb) (setup: GemSetup) =
        match skillOfGem db setup.Gem with
        | None -> []
        | Some data ->
            match List.tryItem (setup.StatSet - 1) data.StatSets with
            | None -> []
            | Some set ->
                let ctx = ctxOf db setup.Gem
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min maxLevel (setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods) + supportLevels db setup)
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods) + supportQuality db setup
                let alt = Mods.flag ctx (Named "AltQuality") characterMods
                let supports =
                    setup.Supports
                    |> List.collect (fun (name, lvl) ->
                        match supportOfGem db name with
                        | Some s when s.Support -> s.StatSets |> List.tryHead |> Option.map (fun set -> stats s set lvl 0) |> Option.defaultValue []
                        | _ -> [])
                statsWith alt data set level quality @ supports

    /// Attribute requirement of a skill gem at a level (PoB calcLib.getGemStatRequirement):
    /// round((5 + (level - 3) * 1.7) * (share / 100) ^ 0.9) + 4, 0 below 8 and for supports.
    let attributeRequirement level (share: float) isSupport =
        if share <= 0.0 || isSupport then 0.0
        else
            let req = System.Math.Round((5.0 + float (level - 3) * 1.7) * ((share / 100.0) ** 0.9)) + 4.0
            if req < 8.0 then 0.0 else req

    /// The index (1-based) of a skill's "-Infused" stat set, if it has one.
    let infusedSet (db: GameDb) name =
        skillOfGem db name
        |> Option.bind (fun d -> d.StatSets |> List.tryFindIndex (fun s -> s.Label.EndsWith "-Infused"))
        |> Option.map (fun i -> i + 1)
