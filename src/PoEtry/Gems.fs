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

    /// Turn game stats into mods, sorting out what we couldn't use.
    let private interpret (prefix: string option) (stats: (string * float) list) =
        stats
        |> List.fold
            (fun (mods, cov: Coverage) (id, v) ->
                match baseDamageStat id with
                | Some _ -> mods, cov
                | None when deliveryStats.Contains id || id.EndsWith "_number_of_projectiles_per_power_charge" -> mods, cov
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

    /// Skill types that only say which supports fit or how the skill is aimed and animated:
    /// nothing to simulate.
    let private descriptiveTypes =
        set [ "Triggerable"; "Totemable"; "Trappable"; "Mineable"; "Multicastable"; "Unleashable"; "UsableWhileMoving"; "Cascadable"
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
              "CrossbowAmmoSkill"; "GainsStages"; "RequiresShield"; "Warcry" ]

    /// What a gem's skill does that the simulator has no model for: its skill types that are
    /// neither descriptive nor modelled ("Channel", "GainsStages", "Shapeshift", "Rain"...).
    let unmodelledTypes (db: GameDb) name =
        skillOfGem db name
        // an ammunition gem is judged by the shot it loads
        |> Option.map (fun d -> if d.Types.Contains "CrossbowAmmoSkill" then shotOf db name |> Option.defaultValue d else d)
        |> Option.map (fun d -> d.Types |> Set.filter (fun t -> not (descriptiveTypes.Contains t) && not (modelledTypes.Contains t)) |> Set.toList)
        |> Option.defaultValue []

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
              // the pillars stay as Ice Crystals and explode when destroyed
              ("Grim Pillars", "Explosion"), [ "Part"; "Crystals" ]
              ("Frozen Locus", "Explosion"), [ "Crystals" ]
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
              ("Spearfield", "Explosion"), [ "Part"; "Hits:4"; "Less:80" ]
              // the eruption happens once; the sprays that follow rain down around the volcano
              ("Volcano", ""), [ "Once" ]
              ("Volcano", "Projectiles"), [ "Rain" ]
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
              ("Power Siphon", ""), [ "Vs:Cullable" ] ]

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

    let private buildSet altQuality (data: SkillData) (set: StatSetData) name tags level quality (supportStats: (string * float) list) (weapon: Weapon option) (shieldArmour: float, shieldEvasion: float) =
        let gemLevel = Map.tryFind level data.Levels
        let ownStats = statsWith altQuality data set level quality
        // the skill's internal name, from all its stat sets
        let prefix = skillPrefix (data.StatSets |> List.collect (fun st -> st.Stats @ (st.ConstantStats |> List.map fst)))
        let ownMods, ownCov = interpret prefix ownStats
        let supportMods, supportCov = interpret None supportStats
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
                weapon |> Option.map (fun w -> w.Damage) |> Option.defaultValue Map.empty
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
        let perStage (endings: string list) =
            ownStats |> List.filter (fun (id, _) -> endings |> List.exists (fun (e: string) -> id.EndsWith e)) |> List.sumBy snd
        let stageMods =
            if not staged then []
            else
                let more = perStage [ "damage_+%_final_per_stage"; "damage_+%_final_per_stack" ] * stages + perStage [ "damage_+%_final_per_additional_stage" ] * (stages - 1.0)
                let gainFire = perStage [ "all_damage_%_to_gain_as_fire_per_stage" ] * stages
                [ if more <> 0.0 then yield Mod.more (Damage AllDamage) more |> Mod.from "stages"
                  if gainFire > 0.0 then
                      for t in DamageType.all do
                          if t <> Fire then yield Mod.base' (GainAs(t, Fire)) gainFire |> Mod.from "stages" ]
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
        let delivery =
            if isProjectileSet then
                // shards sprayed on from the enemy the projectile hit
                let behind = Map.tryFind (data.Name, set.Label) partConditions |> Option.exists (List.contains "Behind")
                let rains = Map.tryFind (data.Name, set.Label) partConditions |> Option.exists (List.contains "Rain")
                let count = statOf "base_number_of_projectiles" |> Option.map int |> Option.defaultValue (if perCharge.IsSome then 0 else 1)
                Projectiles
                    { ProjectileShape.make count with
                        // "projectiles_nova": fired in a full circle
                        // "projectiles_barrage": fired one after the other at the target, so each can hit it
                        Spread = (if flagAny "projectiles_nova" then 360.0 elif flagOf "projectiles_barrage" then 0.0 else statOf "total_projectile_spread_angle_override" |> Option.defaultValue 30.0)
                        Pierce = (if flagOf "always_pierce" then 100 else int (statOf "projectile_base_number_of_targets_to_pierce" |> Option.defaultValue 0.0))
                        Chains = int (statOf "number_of_chains" |> Option.defaultValue 0.0)
                        // projectiles that drop on their targets, one each, in a number the skill
                        // sets itself (Dark Effigy's "one per afflicted enemy, up to 100"): never two on one enemy
                        CanShotgun =
                            not (data.Types.Contains "ProjectileNoCollision" && data.Types.Contains "ProjectilesNumberModifiersNotApplied")
                            // Spiral Volley: "Each target can only be Hit once"
                            && not (data.Description.Contains "Each target can only be Hit once")
                        PerCharge = perCharge
                        FromImpact = data.Types.Contains "ProjectilesNotFromUser" || behind
                        SkipPrimary = behind
                        // a rain over the skill's area, each projectile with its own small impact
                        Rain =
                            match statOf "active_skill_base_area_of_effect_radius", statOf "active_skill_base_secondary_area_of_effect_radius" with
                            | Some a, Some b when count >= 2 && ((data.Types.Contains "Rain" && b < a) || rains) -> Some(max a b, min a b)
                            | _ -> None
                        Explosion = if data.Types.Contains "Area" && not (List.contains "is_area_damage" set.Stats) then radius else None }
            else
                match radius with
                // "ice_nova_number_of_frost_bolts_to_cast_on": cast on a Frostbolt near the target
                | Some r when (statOf "ice_nova_number_of_frost_bolts_to_cast_on" |> Option.defaultValue 0.0) > 0.0 -> AreaAtTarget r
                | Some r when data.Types.Contains "Nova" -> AreaAroundSelf r
                | Some r -> AreaAtTarget r
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
             | Some n when not held -> [ for k in 1 .. n -> float k / float n, 1.0 ]
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
                match find words with
                | Some n -> [ Mod.base' (Named "ConsumeChargeCount") n ]
                | None -> [])
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
          Cost = gemLevel |> Option.bind (fun l -> l.ManaCost) |> Option.defaultValue 0.0
          // "comet_fall_time_ms": hits land this long after the cast
          HitDelay = (ownStats |> List.filter (fun (id, _) -> id.EndsWith "_fall_time_ms") |> List.sumBy snd) / 1000.0
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
        cov

    /// Build a simulator Skill from a socketed gem. `characterMods` are needed for
    /// "+N to level of ... gems". Attacks need a weapon. Stat sets that belong to the
    /// same use become Parts of the skill.
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
                let level = setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods)
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min level maxLevel
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods)
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
                let main, cov = buildSet alt data set data.Name tags level quality supportStats weapon shield
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
                        let p, c = buildSet alt data s (data.Name + ": " + s.Label) tags level quality supportStats weapon shield
                        { p with Consumes = None }, c)
                    |> List.unzip
                let cov = List.fold Coverage.merge cov (supportCov @ partCov)
                // supports multiply the cost: "Cost Multiplier 130%" is manaMultiplier 30
                let costMult =
                    setup.Supports
                    |> List.choose (fun (name, lvl) -> supportOfGem db name |> Option.bind (fun s -> Map.tryFind (max 1 lvl) s.Levels |> Option.orElse (Map.tryFind 1 s.Levels)))
                    |> List.fold (fun k l -> k * (1.0 + (l.ManaMultiplier |> Option.defaultValue 0.0) / 100.0)) 1.0
                Ok({ main with Parts = parts; Cost = main.Cost * costMult; CostPerSecond = main.CostPerSecond * costMult }, cov)

    /// The game stats of a socketed gem (own stat set, quality, alt quality, supports),
    /// for behaviours that need raw numbers.
    let rawStats (db: GameDb) (characterMods: ModDb) (setup: GemSetup) =
        match skillOfGem db setup.Gem with
        | None -> []
        | Some data ->
            match List.tryItem (setup.StatSet - 1) data.StatSets with
            | None -> []
            | Some set ->
                let tags = (data.Types |> Set.toList |> List.choose tagOfType) @ (data.Types |> Set.toList |> List.map (fun t -> Tag("Type:" + t))) @ [ Tag("Gem:" + data.Name) ] |> Set.ofList
                let ctx = { Ctx.empty with SkillName = data.Name; SkillTags = tags }
                let maxLevel = data.Levels |> Map.toList |> List.map fst |> List.fold max 1
                let level = min maxLevel (setup.Level + int (Mods.sumBase ctx [ Named "GemLevel" ] characterMods))
                let quality = setup.Quality + int (Mods.sumBase ctx [ Named "GemQuality" ] characterMods)
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
