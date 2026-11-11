namespace PoEtry

/// Projectile behaviour of a skill. Distances in game units (10 = 1 metre).
type ProjectileShape =
    { Count: int
      /// Total fan angle in degrees across all projectiles.
      Spread: float
      Pierce: int
      Chains: int
      ChainRange: float
      Range: float
      /// Several projectiles of one use may hit the same enemy (each projectile still
      /// hits an enemy at most once). Geometry decides how many actually do.
      CanShotgun: bool
      /// Radius of an explosion where a projectile hits, if any.
      Explosion: float option
      /// Extra projectiles per stack of a charge: ("PowerCharge", 3).
      PerCharge: (string * int) option
      /// Projectiles start where the skill lands (on the aimed enemy), not at the player.
      FromImpact: bool
      /// Chance (%) to bounce off a wall once ("chain 1 extra time from terrain").
      TerrainChain: float
      /// Projectiles appear this far from their origin, along their direction (game units).
      StartOffset: float }

/// How a skill use finds its targets.
type Delivery =
    /// The aimed-at enemy only (melee strikes, single-target hits).
    | Single
    /// Everything within the radius of the aimed-at enemy.
    | AreaAtTarget of radius: float
    /// Everything within the radius of the player.
    | AreaAroundSelf of radius: float
    | Projectiles of ProjectileShape

module ProjectileShape =
    let make count =
        { Count = count
          Spread = 30.0
          Pierce = 0
          Chains = 0
          ChainRange = 30.0
          Range = 100.0
          CanShotgun = true
          Explosion = None
          PerCharge = None
          FromImpact = false
          TerrainChain = 0.0
          StartOffset = 0.0 }

/// A skill as the engine sees it: where its damage comes from and how long it takes.
type Skill =
    { Name: string
      Tags: Set<SkillTag>
      /// Base damage per type. For attacks this is the weapon's (local mods already applied).
      BaseDamage: Map<DamageType, Range>
      /// "Attack damage: 120% of base" / spell base multiplier.
      BaseMultiplier: float
      /// Spells: share of added damage the skill receives (1.0 = 100%).
      AddedDamageEffectiveness: float
      /// Percent, before modifiers. Weapon crit for attacks.
      BaseCrit: float
      /// Seconds per use with no speed modifiers (1 / weapon APS * skill time multiplier for attacks).
      BaseTime: float
      Cooldown: float option
      /// Uses stored before the cooldown blocks the skill (gem level data; Second Wind doubles).
      StoredUses: int
      /// Hits produced by one use: (fraction of the use time, damage multiplier). Default: one hit at the end.
      Hits: (float * float) list
      /// How a use finds its targets.
      Delivery: Delivery
      /// Mods that only exist while using this skill (gem stats, supports).
      Mods: Mod list
      /// Other parts of the same use (e.g. Falling Thunder's slam + its projectiles).
      Parts: Skill list
      /// Charge this skill consumes on use, e.g. "PowerCharge".
      Consumes: string option
      /// Mana per use before cost modifiers (gem cost × support multipliers).
      Cost: float
      /// Seconds between the use and its hits (a comet falling).
      HitDelay: float
      /// Damage over time the skill applies to what it hits: base damage per second by type
      /// ("base_<type>_damage_to_deal_per_minute"), for Duration seconds (x skill duration mods).
      Dots: Map<DamageType, float>
      /// Base duration of the skill's effect in seconds (0: none).
      Duration: float
      /// Crossbow ammunition: bolts per load (0: the skill uses none) and seconds to reload
      /// before speed modifiers. The cost is paid per load.
      Clip: int
      ReloadTime: float
      /// Channelled skills: mana per second while channelling, on top of Cost at the start.
      CostPerSecond: float }

module Skill =
    let make name tags =
        { Name = name
          Tags = Set.ofList tags
          BaseDamage = Map.empty
          BaseMultiplier = 1.0
          AddedDamageEffectiveness = 1.0
          BaseCrit = 5.0
          BaseTime = 1.0
          Cooldown = None
          StoredUses = 1
          Hits = [ 1.0, 1.0 ]
          Delivery = Single
          Mods = []
          Parts = []
          Consumes = None
          Cost = 0.0
          HitDelay = 0.0
          Dots = Map.empty
          Duration = 0.0
          Clip = 0
          ReloadTime = 0.0
          CostPerSecond = 0.0 }

    let ctx (skill: Skill) crit buffs enemyStatus =
        { Crit = crit
          SkillName = skill.Name
          SkillTags = skill.Tags
          Buffs = buffs
          EnemyStatus = enemyStatus }

/// The target, as far as a single hit is concerned.
type Defender =
    { Level: int
      /// Mods describing the defender: base resists, armour, plus active debuffs
      /// (exposure, curses, shock, ...).
      Mods: ModDb }

type HitResult =
    { Crit: bool
      /// After all attacker modifiers, before enemy mitigation. Ailments scale from this.
      Unmitigated: Dmg
      /// Actually taken by the enemy.
      Dealt: Dmg }

module Hit =
    let private scopesFor t =
        [ Damage AllDamage; Damage(OfType t) ]
        @ (if DamageType.isElemental t then [ Damage Elemental ] else [])

    /// Fractions per destination type for one source type, scaled down if they exceed 100%.
    /// Increased and more damage that applies to damage of this type with this skill.
    let damageMult ctx mods t = Mods.mult ctx (scopesFor t) mods

    let private conversionShares ctx mods stat from =
        let raw =
            DamageType.all
            |> List.filter (fun into -> into > from)
            |> List.map (fun into -> into, max 0.0 (Mods.sumBase ctx [ stat (from, into) ] mods) / 100.0)
        let total = raw |> List.sumBy snd
        if total > 1.0 then raw |> List.map (fun (t, v) -> t, v / total) else raw

    /// Push base damage through conversion (skill first, then global on what is left),
    /// type by type in conversion order, so converted damage can convert again further down.
    let private hasAny pred (db: ModDb) = db |> Map.exists (fun stat _ -> pred stat)

    let convert ctx (mods: ModDb) (initial: Map<DamageType, Range>) : Map<DamageType, Range> =
        if not (mods |> hasAny (function SkillConvert _ | Convert _ -> true | _ -> false)) then initial else
        let get t (m: Map<DamageType, Range>) = Map.tryFind t m |> Option.defaultValue Range.zero
        let addTo t r m = Map.add t (Range.add (get t m) r) m
        DamageType.all
        |> List.fold
            (fun (acc: Map<DamageType, Range>) from ->
                let pool = get from acc
                let skillShares = conversionShares ctx mods SkillConvert from
                let skillTotal = skillShares |> List.sumBy snd
                let globalShares = conversionShares ctx mods Convert from
                let globalTotal = globalShares |> List.sumBy snd
                let kept = (1.0 - skillTotal) * (1.0 - globalTotal)
                let moved =
                    DamageType.all
                    |> List.filter (fun into -> into > from)
                    |> List.map (fun into ->
                        let share t xs = xs |> List.tryFind (fst >> (=) t) |> Option.map snd |> Option.defaultValue 0.0
                        into, share into skillShares + (1.0 - skillTotal) * share into globalShares)
                let acc = Map.add from (Range.scale kept pool) acc
                moved |> List.fold (fun a (into, k) -> if k > 0.0 then addTo into (Range.scale k pool) a else a) acc)
            initial
        |> Map.filter (fun _ r -> r.Max > 0.0)

    /// "Gain X% of A damage as extra B", taken from post-conversion damage.
    let gain ctx (mods: ModDb) (converted: Map<DamageType, Range>) =
        if not (mods |> hasAny (function GainAs _ -> true | _ -> false)) then converted else
        let extra =
            [ for KeyValue (from, r) in converted do
                  for into in DamageType.all do
                      let pct = Mods.sumBase ctx [ GainAs(from, into) ] mods
                      if pct > 0.0 then yield into, Range.scale (pct / 100.0) r ]
        extra
        |> List.fold
            (fun m (t, r) -> Map.add t (Range.add (Map.tryFind t m |> Option.defaultValue Range.zero) r) m)
            converted

    /// Base damage ranges before inc/more: skill base + added damage, times the base multiplier.
    let baseDamage ctx mods (skill: Skill) =
        let addedEffect = skill.AddedDamageEffectiveness * Mods.mult ctx [ AddedDamageEffect ] mods
        DamageType.all
        |> List.choose (fun t ->
            let b = Map.tryFind t skill.BaseDamage |> Option.defaultValue Range.zero
            let added =
                { Min = Mods.sumBase ctx [ AddedMin t ] mods
                  Max = Mods.sumBase ctx [ AddedMax t ] mods }
            let r = Range.scale skill.BaseMultiplier (Range.add b (Range.scale addedEffect added))
            if r.Max > 0.0 then Some(t, r) else None)
        |> Map.ofList

    /// Damage ranges ready to roll. "Gain as extra" is taken from the base damage first, and
    /// conversion then applies to base and gained damage alike (PoB does it the other way round;
    /// the in-game damage split of a fully converted skill points to this order, see RULES.md).
    let ranges ctx mods skill =
        let r =
            if Constants.gainBeforeConversion then baseDamage ctx mods skill |> gain ctx mods |> convert ctx mods
            else baseDamage ctx mods skill |> convert ctx mods |> gain ctx mods
        // Avatar of Fire: "Deal no Non-Fire Damage"
        if Mods.flag ctx (Named "DealNoNonFire") mods then r |> Map.filter (fun t _ -> t = Fire) else r

    /// Chance (%) an attack hits: 1.25 A / (A + 0.3 E), between 5 and 100 (PoB calcs.hitChance).
    let accuracyHitChance (evasion: float) (accuracy: float) =
        if accuracy <= 0.0 then 5.0
        else max 5.0 (min 100.0 (floor (1.25 * accuracy / (accuracy + 0.3 * evasion) * 100.0 + 0.5)))

    /// Accuracy falls off with distance: full up to 2 m, 90% less at 9 m and beyond
    /// (Character.ot accuracy_rating_+%_final_at_max_distance_scaled, PoB falloff 20..90 units).
    let accuracyAtDistance (distance: float) (accuracy: float) =
        let d = max 20.0 (min 90.0 distance)
        accuracy * (1.0 - (d - 20.0) / 70.0 * 0.9)

    let critChance ctx mods (skill: Skill) =
        match Mods.override' ctx CritChance mods with
        | Some v -> v
        | None ->
            let baseCrit = skill.BaseCrit + Mods.sumBase ctx [ CritChance ] mods
            let chance = min 100.0 (max 0.0 (baseCrit * Mods.mult ctx [ CritChance ] mods))
            // "maximum Critical Hit Chance is X%"
            let cap = Mods.sumBase ctx [ Named "MaxCritChance" ] mods
            let chance = if cap > 0.0 then min cap chance else chance
            // "roll Critical Hits twice": lucky
            if Mods.flag ctx (Named "LuckyCrit") mods then 100.0 - (100.0 - chance) * (100.0 - chance) / 100.0 else chance

    /// Multiplier applied to crit damage (2.0 = +100% bonus).
    let critMultiplier ctx mods =
        let bonus = Constants.playerBaseCritBonus + Mods.sumBase ctx [ CritBonus ] mods
        1.0 + max 0.0 (bonus * Mods.mult ctx [ CritBonus ] mods) / 100.0

    /// Roll one hit's damage before mitigation.
    let rollUnmitigated (rng: Rng) (ctx: Ctx) mods (skill: Skill) (crit: bool) : Dmg =
        let ctx = { ctx with Crit = crit }
        // lucky: always, or by chance ("X% chance for Damage with Hits to be Lucky")
        let lucky = Mods.flag ctx LuckyDamage mods || rng.Chance(Mods.sumBase ctx [ Named "LuckyDamageChance" ] mods)
        let critMult = if crit then critMultiplier ctx mods else 1.0
        let doubled = rng.Chance(Mods.sumBase ctx [ DoubleDamageChance ] mods)
        ranges ctx mods skill
        |> Map.map (fun t r ->
            let roll () = rng.Between(r.Min, r.Max)
            let rolled = if lucky then max (roll ()) (roll ()) else roll ()
            rolled
            * Mods.mult ctx (scopesFor t) mods
            * critMult
            * (if doubled then 2.0 else 1.0))

    /// Effective resistance after exposure/curses (already in the defender's mods) and
    /// penetration. Penetration cannot take resistance below zero.
    let effectiveResist (attackerCtx: Ctx) attackerMods (defender: Defender) t =
        let dctx = Ctx.empty
        let maxRes =
            min Constants.resistCeiling (Constants.enemyBaseMaxResist + Mods.sumBase dctx [ MaxResist t ] defender.Mods)
        let res =
            // "Enemies Frozen by this Skill have -X% to Cold Resistance": lowered by the attacker, for its hits
            Mods.sumBase dctx [ Resist t ] defender.Mods + Mods.sumBase attackerCtx [ Named("EnemyResist:" + DamageType.name t) ] attackerMods
            |> min maxRes
            |> max Constants.resistFloor
        let pen = Mods.sumBase attackerCtx [ Penetration t ] attackerMods
        // penetration stops at 0, or lower with "can penetrate down to a minimum of X"
        let floor =
            if DamageType.isElemental t then
                match Mods.override' attackerCtx (Named "PenetrationFloorOverride") attackerMods with
                | Some f -> f
                | None -> min 0.0 (Mods.sumBase attackerCtx [ Named "PenetrationFloor" ] attackerMods)
            else 0.0
        if res > floor then max floor (res - pen) else res

    /// Armour: reduction = A / (A + 10 * damage), plus flat reduction, capped at 75% for monsters.
    let physicalReduction (defender: Defender) damage =
        let dctx = Ctx.empty
        let armour = max 0.0 (Mods.total dctx [ Armour ] defender.Mods)
        let fromArmour = if armour <= 0.0 && damage <= 0.0 then 0.0 else armour / (armour + Constants.armourRatio * damage) * 100.0
        min Constants.enemyMaxPhysReduction (fromArmour + Mods.sumBase dctx [ PhysicalReduction ] defender.Mods)

    let damageTakenMult (defender: Defender) t =
        Mods.mult Ctx.empty (scopesFor t |> List.map (function Damage s -> DamageTaken s | s -> s)) defender.Mods

    /// "Treat enemy resistances as negated": positive elemental resistance counts as
    /// its negative. Rolled per hit when it's a chance.
    let mitigateWith (negate: bool) ctx mods (defender: Defender) (unmitigated: Dmg) : Dmg =
        unmitigated
        |> Map.map (fun t v ->
            let reduction =
                match t with
                | Physical -> physicalReduction defender v
                | _ ->
                    let r = effectiveResist ctx mods defender t
                    if negate && DamageType.isElemental t && r > 0.0 then -r else r
            v * (1.0 - reduction / 100.0) * damageTakenMult defender t)

    let mitigate ctx mods defender unmitigated = mitigateWith false ctx mods defender unmitigated

    /// Full hit: crit roll, damage roll, mitigation.
    let roll (rng: Rng) (ctx: Ctx) mods (skill: Skill) (defender: Defender) =
        let chance = critChance ctx mods skill
        let first = rng.Chance chance
        // Inevitable Critical Hits: a hit that does not roll a critical hit rolls again until it
        // does, with 30% less critical damage bonus for each re-roll (none left after four)
        let rerolls =
            if first || chance <= 0.0 || not (Mods.flag ctx (Named "InevitableCrits") mods) then 0
            else
                let rec go n = if n >= 4 || rng.Chance chance then n else go (n + 1)
                go 1
        // Bifurcating critical hits: the chance is rolled twice; one success is a critical hit,
        // two apply the critical damage bonus twice
        let second = rerolls = 0 && chance > 0.0 && Mods.flag ctx (Named "BifurcateCrits") mods && rng.Chance chance
        let crit = first || second || rerolls > 0
        let ctx = { ctx with Crit = crit }
        let unmitigated = rollUnmitigated rng ctx mods skill crit
        let unmitigated =
            if first && second then
                let full = critMultiplier ctx mods
                Dmg.map (fun _ v -> v * (1.0 + 2.0 * (full - 1.0)) / full) unmitigated
            elif rerolls = 0 then unmitigated
            else
                let full = critMultiplier ctx mods
                let kept = max 0.0 (1.0 - Constants.inevitableCritLessPerReroll * float rerolls)
                Dmg.map (fun _ v -> v * (1.0 + (full - 1.0) * kept) / full) unmitigated
        { Crit = crit
          Unmitigated = unmitigated
          Dealt = mitigateWith (rng.Chance(Mods.sumBase ctx [ Named "NegateEnemyResistChance" ] mods)) ctx mods defender unmitigated }
