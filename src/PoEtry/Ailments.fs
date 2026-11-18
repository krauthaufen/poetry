namespace PoEtry

/// What a single hit does to the target besides its damage.
type Inflict =
    /// Damage over time. Dps is before the enemy's resistances, which are applied
    /// continuously while it ticks (so debuffs applied later still count).
    | Dot of ailment: Ailment * damageType: DamageType * dps: float * duration: float * maxStacks: int
    /// A status with a magnitude (shock: % increased damage taken, chill: % slow, 0 for plain statuses).
    | Status of ailment: Ailment * magnitude: float * duration: float
    /// Add buildup (percent of the full bar) towards freeze / electrocute / heavy stun / pin,
    /// and how long the status lasts if this fills the bar (`buildupSeconds`).
    | AddBuildup of ailment: Ailment * percent: float * seconds: float

module Ailments =
    /// Damage types each ailment scales from, and the type it deals.
    let scalesFrom a =
        match a with
        | Ignite -> [ Fire ]
        | Bleed -> [ Physical ]
        | Poison -> [ Physical; Chaos ]
        | Shock -> [ Lightning ]
        | Chill
        | Freeze -> [ Cold ]
        | Electrocute -> [ Lightning ]
        // "All non-Cold damage from a hit adds Heavy Stun buildup" (wiki, Stun)
        | HeavyStun -> DamageType.all |> List.filter ((<>) Cold)
        | Pin -> [ Physical ]
        | Daze
        | Maim
        | Blind
        | Hinder -> []

    let dealsType a =
        match a with
        | Ignite -> Fire
        | Bleed -> Physical
        | _ -> Chaos

    let percentPerSecond a =
        match a with
        | Ignite -> Constants.ignitePercentPerSecond
        | Bleed -> Constants.bleedPercentPerSecond
        | Poison -> Constants.poisonPercentPerSecond
        | _ -> 0.0

    let baseDuration a =
        match a with
        | Ignite -> Constants.igniteDuration
        | Bleed -> Constants.bleedDuration
        | Poison -> Constants.poisonDuration
        | Shock -> Constants.shockDuration
        | Chill -> Constants.chillDuration
        | Freeze | Electrocute | HeavyStun | Pin -> Constants.buildupDuration a
        // game keyword text: Daze 8 s; Maim, Blind, Hinder 4 s
        | Daze -> 8.0
        | Maim | Blind | Hinder -> 4.0

    let magnitudeMult (ctx: Ctx) mods a =
        Mods.mult ctx [ AilmentMagnitude(Some a); AilmentMagnitude None ] mods

    let duration (ctx: Ctx) mods a =
        // "Base Bleeding Duration is 1 second" (Crimson Assault)
        let baseD =
            match a with
            | Bleed -> Mods.override' ctx (Named "BaseBleedDuration") mods |> Option.defaultValue (baseDuration a)
            // "Poison duration is 3 seconds" (a skill's own)
            | Poison -> Mods.override' ctx (Named "BasePoisonDuration") mods |> Option.defaultValue (baseDuration a)
            | _ -> baseDuration a
        baseD * Mods.mult ctx [ AilmentDuration(Some a); AilmentDuration None ] mods

    /// How long a filled bar's status lasts: Freeze and Electrocute are ailments, which their
    /// duration modifiers lengthen ("increased Freeze Duration on Enemies"); Heavy Stun and Pin
    /// are not, and last the game's fixed time.
    let buildupSeconds (ctx: Ctx) mods a =
        match a with
        | Freeze | Electrocute -> duration ctx mods a
        | _ -> Constants.buildupDuration a

    let maxStacks (ctx: Ctx) mods a =
        1 + int (Mods.sumBase ctx [ AilmentStacks a ] mods)

    let threshold (defender: Defender) =
        Constants.ailmentThreshold defender.Level * Mods.mult Ctx.empty [ AilmentThreshold ] defender.Mods

    let poise (defender: Defender) a =
        let extra =
            match a with
            // each is raised for a while after the monster has suffered it
            | Freeze | Electrocute -> [ PoiseThreshold; AilmentThreshold; Named(Ailment.name a + "Threshold") ]
            | _ -> [ PoiseThreshold; Named(Ailment.name a + "Threshold") ]
        Constants.poiseThreshold defender.Level * Mods.mult Ctx.empty extra defender.Mods

    let private chanceMods (ctx: Ctx) mods a =
        Mods.sumBase ctx [ AilmentChance a ] mods, Mods.mult ctx [ AilmentChance a ] mods

    /// Damage types an ailment can be made to scale from besides its own ("Fire Damage from
    /// Hits Contributes to Shock Chance", "Elemental Damage also Contributes to Bleeding
    /// Magnitude"), and its own types it can be made not to scale from ("... instead of
    /// Flammability and Ignite Magnitudes"). Each is a flag of the attacker
    /// (`alsoFromStat`, `notFromStat`); `sourcesChanged` is set with any of them.
    let alsoFrom =
        [ Shock, Fire; Shock, Cold; Shock, Chaos; Ignite, Cold; Ignite, Lightning; Freeze, Lightning; Freeze, Fire; Freeze, Physical
          Chill, Physical; Chill, Fire; Chill, Lightning; Bleed, Fire; Bleed, Cold; Bleed, Lightning ]
    let notFrom = [ Ignite, Fire; Chill, Cold; Freeze, Cold; Shock, Lightning ]
    let alsoFromStat (a: Ailment) (t: DamageType) = "AilmentAlsoFrom:" + Ailment.name a + ":" + DamageType.name t
    let notFromStat (a: Ailment) (t: DamageType) = "AilmentNotFrom:" + Ailment.name a + ":" + DamageType.name t
    let sourcesChanged = "AilmentSourcesChanged"

    /// Damage types an ailment scales from for this attacker ("All damage can ignite" etc.).
    let scalesFromWith (ctx: Ctx) mods a =
        let all flag = Mods.flag ctx (Named flag) mods
        let basic =
            match a with
            | Ignite when all "AllDamageIgnites" -> DamageType.all
            | Chill when all "AllDamageChills" -> DamageType.all
            | Shock when all "AllDamageShocks" -> DamageType.all
            | _ -> scalesFrom a
        if not (all sourcesChanged) then basic
        else
            DamageType.all |> List.filter (fun t ->
                (List.contains t basic || (List.contains (a, t) alsoFrom && all (alsoFromStat a t)))
                && not (List.contains (a, t) notFrom && all (notFromStat a t)))

    /// What an enemy covered in Oil has, monster side: Flammability inflicted on it is so much
    /// more ("200% more Magnitude of Flammability inflicted on them"), and a Detonator's hit is
    /// sure to Ignite it ("Detonator Skills are guaranteed to Ignite").
    let flammabilityTaken = "FlammabilityTaken"
    let coveredInOil = "CoveredInOil"
    let detonator = Tag "Type:Detonator"

    /// Chance in percent that this hit inflicts the ailment.
    let chance (ctx: Ctx) mods defender (hit: HitResult) a =
        let baseChance, mult = chanceMods ctx mods a
        let fromDamage multiplier =
            Dmg.sumOf (scalesFromWith ctx mods a) hit.Unmitigated / threshold defender * multiplier
        let c =
            match a with
            // the chance to Ignite is "the total Magnitude of Flammability on the target, including
            // the amount added by that Hit" (keyword text): what earlier hits left is on the defender
            | Ignite ->
                if ctx.SkillTags.Contains detonator && Mods.sumBase Ctx.empty [ Named coveredInOil ] defender.Mods > 0.0 then 100.0
                else
                    (fromDamage Constants.igniteChanceMultiplier + baseChance) * mult * Mods.more Ctx.empty [ Named flammabilityTaken ] defender.Mods
                    + Mods.sumBase Ctx.empty [ Named "Flammability" ] defender.Mods
            | Shock -> (fromDamage Constants.shockChanceMultiplier + baseChance) * mult
            | _ -> baseChance * mult
        min 100.0 (max 0.0 c)

    /// The Flammability a hit adds to its target: 1% per 5% of the ailment threshold its fire
    /// damage is (up to 100%), for `flammabilityDuration` seconds, each on its own.
    let flammability (ctx: Ctx) mods defender (hit: HitResult) =
        let ctx = { ctx with Crit = hit.Crit }
        let baseChance, mult = chanceMods ctx mods Ignite
        if Dmg.sumOf (scalesFromWith ctx mods Ignite) hit.Unmitigated <= 0.0 then 0.0
        else min 100.0 (max 0.0 ((Dmg.sumOf (scalesFromWith ctx mods Ignite) hit.Unmitigated / threshold defender * Constants.igniteChanceMultiplier + baseChance) * mult * Mods.more Ctx.empty [ Named flammabilityTaken ] defender.Mods))

    /// Everything this hit inflicts, rolling chances with the given Rng.
    let fromHit (rng: Rng) (ctx: Ctx) mods (defender: Defender) (hit: HitResult) : Inflict list =
        let ctx = { ctx with Crit = hit.Crit }
        let source a = Dmg.sumOf (scalesFromWith ctx mods a) hit.Unmitigated
        let rolled a = source a > 0.0 && rng.Chance(chance ctx mods defender hit a)

        let damaging =
            [ for a in Ailment.damaging do
                  if rolled a then
                      // "deal damage X% faster": the same total in less time
                      let faster = Mods.inc ctx (Named "FasterAilments" :: (if a = Ignite then [ Named "FasterIgnite" ] elif a = Bleed then [ Named "FasterBleed" ] else [])) mods
                      let dps = source a * percentPerSecond a / 100.0 * magnitudeMult ctx mods a * faster
                      yield Dot(a, dealsType a, dps, duration ctx mods a / max 0.1 faster, maxStacks ctx mods a) ]

        let shock =
            [ if rolled Shock then
                  yield Status(Shock, Constants.baseShockMagnitude * magnitudeMult ctx mods Shock, duration ctx mods Shock) ]

        let chill =
            let cold = source Chill
            let effect = Constants.chillEffectMultiplier * cold / threshold defender * magnitudeMult ctx mods Chill
            [ if effect >= Constants.chillMinEffect then
                  // a chill below 30% is discarded; at most 50%
                  yield Status(Chill, min Constants.chillMaxEffect effect, duration ctx mods Chill) ]

        let flat =
            [ for a in [ Daze; Maim; Blind; Hinder ] do
                  if rng.Chance(chance ctx mods defender hit a) then
                      yield Status(a, 0.0, duration ctx mods a) ]

        let buildup =
            [ for a in Ailment.buildup do
                  let enabled =
                      match a with
                      | Freeze | HeavyStun -> true
                      | Electrocute | Pin -> Mods.flag ctx (Named("Can" + Ailment.name a)) mods
                      | _ -> false
                  let dmg =
                      match a with
                      // heavy stun: physical damage and melee damage each build 50% more, and a
                      // Dazed enemy takes 50% more stun buildup (keyword text)
                      | HeavyStun ->
                          (source a + Dmg.get Physical hit.Unmitigated * Constants.playerPhysicalStunMore / 100.0)
                          * (if ctx.SkillTags.Contains Melee then 1.0 + Constants.playerMeleeStunMore / 100.0 else 1.0)
                          * (if ctx.EnemyStatus.ContainsKey "Dazed" then 1.5 else 1.0)
                      | _ -> source a
                  if enabled && dmg > 0.0 then
                      let pct =
                          dmg * Constants.buildupScale a / poise defender a * Mods.mult ctx [ Buildup a ] mods * 100.0
                      yield AddBuildup(a, pct, buildupSeconds ctx mods a) ]

        damaging @ shock @ chill @ flat @ buildup

    /// Multiplier the enemy applies to a damage-over-time of this type right now.
    let dotTakenMult (defender: Defender) t =
        let dctx = Ctx.empty
        let own (t: DamageType) =
            Mods.sumBase dctx [ Resist t ] defender.Mods
            |> min (min Constants.resistCeiling (Constants.enemyBaseMaxResist + Mods.sumBase dctx [ MaxResist t ] defender.Mods))
            |> max Constants.resistFloor
        let resist =
            match t with
            | Physical -> 0.0 // bleeding ignores armour
            // "Resist Elemental Damage based on their Lowest Resistance"
            | _ when DamageType.isElemental t && Mods.sumBase dctx [ Named "ResistByLowest" ] defender.Mods > 0.0 -> DamageType.elemental |> List.map own |> List.min
            | _ -> own t
        (1.0 - resist / 100.0) * Hit.damageTakenMult defender t * Mods.mult dctx [ DotTaken ] defender.Mods
