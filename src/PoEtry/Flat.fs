namespace PoEtry

/// The fast fight engine. `Sim` is the reference: it says what a fight is, and it stays the
/// way to get a replay (`Sim.shown`). This one plays the same fight with the same random
/// numbers: `compile` turns a fight into a program (names become numbers, what does not
/// change is worked out once) or says what the fight has that is not built here yet; `run`
/// plays a program with a seed.
///
/// The outcome is the reference's but for rounding: what is counted (events, kills, who
/// died) is the same, numbers agree to about 1e-9. Sums are not always taken in the
/// reference's order here (unconditional modifiers are added up ahead of the fight, time and
/// damage over time are added when a state ends, not event by event). A fight is chaotic,
/// so a last-bit difference can in principle flip a roll or a threshold and send the two
/// engines different ways; the tests look for that (`FlatTests.verdict`) and have not met it.
///
/// What makes it fast: the state is a set of arrays changed in place and the events wait in
/// a heap; a skill's modifiers are resolved ahead of the fight into numbers and short lists
/// of conditions on numbered buffs and statuses (see `Query`), and a hit's answers are kept
/// from hit to hit while what they depend on is the same; an enemy's modifiers as a defender
/// are a short list of its own. The arithmetic of a hit follows `Hit` and `Ailments` line by
/// line; who is hit is `Geometry`'s own code.
///
/// Played: one player (no minions, no totems) against any number of enemies that are not
/// replaced; skills with parts, cooldowns, costs and charges; the rules whose reactions are
/// listed in `Re`; ailments, damage over time, curses, exposure, armour break; meta gems;
/// cycles, periodic actions, archons, rage, charms; the player's pools, with monsters
/// walking up and hitting back. Everything else is refused by `compile`, by name
/// (`unsupported` lists it): the lists at `build` say what.
///
/// One thing is taken for granted of a fight: the defences for a set of buffs
/// (`Actor.VitalsAt`) are a function of the buffs (of those `Actor.VitalsReads` names, if
/// it names any). They are asked once per set of buffs, where the reference asks at every
/// hit of a monster; `compile` already asks for the sets it sees come up.
///
/// Nothing here outlives a run: all that changes belongs to the `State` of one `run`, so
/// several runs of one program can go on at once.
module Flat =
    open System.Collections.Generic

    // ---- the kinds of event (see `Sim.Event`) ----

    [<Literal>]
    let private EReady = 0
    [<Literal>]
    let private EStrike = 1
    [<Literal>]
    let private EPassing = 2
    [<Literal>]
    let private EDirect = 3
    [<Literal>]
    let private EConsume = 4
    [<Literal>]
    let private ECycle = 5
    [<Literal>]
    let private EGainStacks = 6
    [<Literal>]
    let private EExpose = 7
    [<Literal>]
    let private ESpendBuff = 8
    [<Literal>]
    let private EExpire = 9
    [<Literal>]
    let private EPeriodic = 10
    [<Literal>]
    let private EEnemyAttack = 11
    [<Literal>]
    let private EEnergy = 12

    let private kindName kind =
        match kind with
        | EReady -> "Ready"
        | EStrike -> "Strike"
        | EPassing -> "PassingStrike"
        | EDirect -> "DirectHit"
        | EConsume -> "ConsumeCharges"
        | ECycle -> "CycleTick"
        | EGainStacks -> "GainStacks"
        | EExpose -> "Expose"
        | ESpendBuff -> "SpendBuff"
        | EPeriodic -> "PeriodicTick"
        | EEnemyAttack -> "EnemyAttack"
        | EEnergy -> "EnergyArrives"
        | _ -> "Expire"

    let private ailmentIndex (a: Ailment) =
        match a with
        | Ignite -> 0
        | Bleed -> 1
        | Poison -> 2
        | Shock -> 3
        | Chill -> 4
        | Freeze -> 5
        | Electrocute -> 6
        | HeavyStun -> 7
        | Pin -> 8
        | Daze -> 9
        | Maim -> 10
        | Blind -> 11
        | Hinder -> 12

    let private allAilments =
        [ Ignite; Bleed; Poison; Shock; Chill; Freeze; Electrocute; HeavyStun; Pin; Daze; Maim; Blind; Hinder ]

    let private typeIndex (t: DamageType) =
        match t with
        | Physical -> 0
        | Lightning -> 1
        | Cold -> 2
        | Fire -> 3
        | Chaos -> 4

    // ---- modifiers, resolved ahead of the fight ----
    //
    // The reference asks a ModDb with a Ctx at every hit: it walks the mods of a stat, newest
    // first, and tests each one's conditions against maps of names. Here a question (a kind of
    // mod, a list of stats) is worked out once per skill into the values of the mods that
    // answer it, in the reference's order: those whose conditions the skill alone decides are
    // kept or dropped for good, a run of unconditional ones at the start is folded into one
    // number, and what is left carries conditions on numbered buffs and statuses. The mods of
    // buffs come first in that order (they are the newest); they are kept apart, per stat,
    // and made anew when a buff with mods comes or goes.

    /// A condition of a mod, its names as numbers (-1: a name nothing in the fight has).
    type private CC =
        | CTrue
        | CFalse
        | CCrit
        | CTag of SkillTag
        | CNamed of string
        | CBuff of int
        | CStatus of int
        | CPerStack of int * float
        | CPerEnemy of int * float
        | CNot of CC

    /// One mod's part in an answer (`Slot` < 0), or the place where the buffs' mods of a
    /// stat go (`Slot`: which).
    type private Item =
        { V: float
          Conds: CC[]
          Stacky: bool
          Slot: int }

    /// A question, ready: the answer so far (`Init`, of the items before `Start`) and the items to go through.
    type private Query =
        { Items: Item[]
          Start: int
          Init: float
          /// Whether the answer can change with the buffs, and with the enemy's statuses.
          OnBuffs: bool
          OnStatus: bool
          /// Whether buffs' mods have a part in it (they can have conditions on anything).
          HasSlot: bool }

    /// What the conditions are tested against: the buffs and statuses as they were when the
    /// hit (or the use) began, whatever it has changed since.
    type private Ev() =
        member val Q : Query[] = [||] with get, set
        member val Crit = false with get, set
        member val Tags : Set<SkillTag> = Set.empty with get, set
        member val Name = "" with get, set
        member val BOn = false with get, set
        member val BHas : bool[] = [||] with get, set
        member val BStacks : int[] = [||] with get, set
        /// The charge the skill counts more stacks of, and how many more.
        member val Extra = -1 with get, set
        member val ExtraN = 0 with get, set
        member val SOn = false with get, set
        member val SHas : bool[] = [||] with get, set
        member val SStacks : int[] = [||] with get, set
        member val Iso = false with get, set
        member val Close = false with get, set
        member val Open = false with get, set
        member val IsoId = -1 with get, set
        member val CloseId = -1 with get, set
        member val OpenId = -1 with get, set
        member val Dyn : Item[][] = [||] with get, set
        /// The buffs' mods before the skill's part in their conditions was decided.
        member val DynRaw : Item[][] = [||] with get, set
        /// Whether what is asked is asked as an attack (for Rage).
        member val AsAttack = false with get, set
        member val RageSlot = -1 with get, set
        /// Rage's more attack damage for the skill (NaN: none).
        member val RageMore = nan with get, set
        member val Found = false with get, set
        /// What the hit inflicts (see `fromHit`).
        member val Inflicted : ResizeArray<Inflict> = ResizeArray<Inflict>() with get
        // answers kept from hit to hit (see `keepAnswers`): the answers by question (and
        // again for critical hits), each with the counts of changes it was worked out at
        member val Keeping = false with get, set
        /// Whether a buff's mod has a condition on the enemy.
        member val DynOnStatus = false with get, set
        member val KVal : float[] = [||] with get, set
        member val KBuffs : int[] = [||] with get, set
        member val KStatus : int[] = [||] with get, set
        member val GBuffs = 0 with get, set
        member val GStatus = 0 with get, set
        // a hit's numbers, by damage type
        member val RMin : float[] = Array.zeroCreate 5 with get
        member val RMax : float[] = Array.zeroCreate 5 with get
        member val RHas : bool[] = Array.zeroCreate 5 with get
        member val OMin : float[] = Array.zeroCreate 5 with get
        member val OMax : float[] = Array.zeroCreate 5 with get
        member val OHas : bool[] = Array.zeroCreate 5 with get
        member val Has : bool[] = Array.zeroCreate 5 with get
        member val Un : float[] = Array.zeroCreate 5 with get
        member val De : float[] = Array.zeroCreate 5 with get
        member val Has2 : bool[] = Array.zeroCreate 5 with get
        member val Un2 : float[] = Array.zeroCreate 5 with get
        member val De2 : float[] = Array.zeroCreate 5 with get

    let private hasStatus (ev: Ev) (i: int) =
        i >= 0 && ((ev.SOn && ev.SHas.[i]) || (ev.Iso && i = ev.IsoId) || (ev.Close && i = ev.CloseId) || (ev.Open && i = ev.OpenId))

    let private statusStacks (ev: Ev) (i: int) =
        if i < 0 then 0
        elif (ev.Open && i = ev.OpenId) || (ev.Close && i = ev.CloseId) || (ev.Iso && i = ev.IsoId) then 1
        elif ev.SOn && ev.SHas.[i] then ev.SStacks.[i]
        else 0

    let private buffStacksOf (ev: Ev) (b: int) =
        if ev.BOn && b >= 0 && ev.BHas.[b] then
            let n = ev.BStacks.[b]
            if b = ev.Extra && n > 0 then n + ev.ExtraN else n
        else 0

    let rec private holds (ev: Ev) (c: CC) : bool =
        match c with
        | CTrue | CPerStack _ | CPerEnemy _ -> true
        | CFalse -> false
        | CCrit -> ev.Crit
        | CTag t -> ev.Tags.Contains t
        | CNamed n -> ev.Name = n
        | CBuff b -> ev.BOn && b >= 0 && ev.BHas.[b]
        | CStatus i -> hasStatus ev i
        | CNot c -> not (holds ev c)

    let private passes (ev: Ev) (it: Item) =
        let mutable ok = true
        for c in it.Conds do
            if ok && not (holds ev c) then ok <- false
        ok

    /// The mod's value where its conditions hold (`Mod.eval`).
    let private amount (ev: Ev) (it: Item) =
        if it.Stacky then
            let mutable k = 1.0
            for c in it.Conds do
                match c with
                | CPerStack (b, every) -> k <- k * floor (float (buffStacksOf ev b) / every)
                | CPerEnemy (i, every) -> k <- k * floor (float (statusStacks ev i) / every)
                | _ -> ()
            it.V * k
        else it.V

    /// The sum of the base (or the increased) mods (`Mods.sumBase`, `Mods.sumInc`).
    let private qSum (ev: Ev) (q: Query) : float =
        let mutable acc = q.Init
        for i in q.Start .. q.Items.Length - 1 do
            let it = q.Items.[i]
            if it.Slot < 0 then (if it.Conds.Length = 0 || passes ev it then acc <- acc + amount ev it)
            else
                for d in ev.Dyn.[it.Slot] do
                    if d.Conds.Length = 0 || passes ev d then acc <- acc + amount ev d
        acc

    /// The product of the more mods (`Mods.more`).
    let private qMore (ev: Ev) (q: Query) : float =
        let mutable acc = q.Init
        for i in q.Start .. q.Items.Length - 1 do
            let it = q.Items.[i]
            if it.Slot < 0 then (if it.Conds.Length = 0 || passes ev it then acc <- acc * (1.0 + amount ev it / 100.0))
            else
                // (Rage's more attack damage is the newest mod of all)
                if it.Slot = ev.RageSlot && ev.AsAttack && not (System.Double.IsNaN ev.RageMore) then acc <- acc * (1.0 + ev.RageMore / 100.0)
                for d in ev.Dyn.[it.Slot] do
                    if d.Conds.Length = 0 || passes ev d then acc <- acc * (1.0 + amount ev d / 100.0)
        acc

    let private qFlag (ev: Ev) (q: Query) : bool =
        let mutable acc = q.Init <> 0.0
        if not acc then
            for i in q.Start .. q.Items.Length - 1 do
                let it = q.Items.[i]
                if it.Slot < 0 then (if (it.Conds.Length = 0 || passes ev it) && amount ev it <> 0.0 then acc <- true)
                else
                    for d in ev.Dyn.[it.Slot] do
                        if (d.Conds.Length = 0 || passes ev d) && amount ev d <> 0.0 then acc <- true
        acc

    /// The override, if there is one (`ev.Found`): the last of the mods in their order.
    let private qOver (ev: Ev) (q: Query) : float =
        let mutable v = 0.0
        ev.Found <- false
        for i in q.Start .. q.Items.Length - 1 do
            let it = q.Items.[i]
            if it.Slot < 0 then
                if it.Conds.Length = 0 || passes ev it then
                    v <- amount ev it
                    ev.Found <- true
            else
                for d in ev.Dyn.[it.Slot] do
                    if d.Conds.Length = 0 || passes ev d then
                        v <- amount ev d
                        ev.Found <- true
        v

    /// The place of a kept answer, if it is still good (-1 - place if it must be worked out).
    let inline private kept (ev: Ev) (q: Query) (i: int) : int =
        let k = if ev.Crit then i + ev.Q.Length else i
        if ev.KBuffs.[k] >= 0 && (not q.OnBuffs || ev.KBuffs.[k] = ev.GBuffs) && (not (q.OnStatus || (q.HasSlot && ev.DynOnStatus)) || ev.KStatus.[k] = ev.GStatus) then k
        else
            ev.KBuffs.[k] <- ev.GBuffs
            ev.KStatus.[k] <- ev.GStatus
            -1 - k

    let private sumQ (ev: Ev) (i: int) : float =
        let q = ev.Q.[i]
        if q.Items.Length = 0 then q.Init
        elif not ev.Keeping then qSum ev q
        else
            let k = kept ev q i
            if k >= 0 then ev.KVal.[k]
            else
                let v = qSum ev q
                ev.KVal.[-1 - k] <- v
                v

    let private moreQ (ev: Ev) (i: int) : float =
        let q = ev.Q.[i]
        if q.Items.Length = 0 then q.Init
        elif not ev.Keeping then qMore ev q
        else
            let k = kept ev q i
            if k >= 0 then ev.KVal.[k]
            else
                let v = qMore ev q
                ev.KVal.[-1 - k] <- v
                v

    let private flagQ (ev: Ev) (i: int) : bool =
        let q = ev.Q.[i]
        if q.Items.Length = 0 then q.Init <> 0.0
        elif not ev.Keeping then qFlag ev q
        else
            let k = kept ev q i
            if k >= 0 then ev.KVal.[k] <> 0.0
            else
                let v = qFlag ev q
                ev.KVal.[-1 - k] <- (if v then 1.0 else 0.0)
                v

    let inline private incQ (ev: Ev) (i: int) = 1.0 + sumQ ev i / 100.0
    /// increased x more of a question asked as two (`Mods.mult`): the increased at i, the more at i + 1.
    let inline private multQ (ev: Ev) (i: int) = max 0.0 ((1.0 + sumQ ev i / 100.0) * moreQ ev (i + 1))

    let rec private ofEnemy (c: CC) =
        match c with
        | CStatus _ | CPerEnemy _ -> true
        | CNot inner -> ofEnemy inner
        | _ -> false

    let rec private ofSkill (c: CC) =
        match c with
        | CTag _ | CNamed _ -> true
        | CNot inner -> ofSkill inner
        | _ -> false

    /// The buffs' mods with the skill's part in their conditions decided: for this skill
    /// (its tags, its name), or for no skill at all.
    let private resolveDyn (table: Item[][]) (tags: Set<SkillTag>) (name: string) : Item[][] =
        let rec decide (c: CC) =
            match c with
            | CTag t -> if tags.Contains t then CTrue else CFalse
            | CNamed n -> if name = n then CTrue else CFalse
            | CNot inner ->
                match decide inner with
                | CTrue -> CFalse
                | CFalse -> CTrue
                | x -> CNot x
            | x -> x
        table |> Array.map (fun items ->
            if items |> Array.exists (fun it -> it.Conds |> Array.exists ofSkill) then
                items |> Array.choose (fun it ->
                    let conds = it.Conds |> Array.map decide
                    if conds |> Array.contains CFalse then None
                    else Some { it with Conds = conds |> Array.filter (fun c -> c <> CTrue) })
            else items)

    /// A stat and a kind as one key.
    let private statKey (stat: Stat) (kind: ModKind) : string =
        let c = StatKey.code stat
        (if c >= 0 then "\u0003" + string c else StatKey.name stat) + "|" + string (ModKind.number kind)

    /// A mod's conditions with their names as numbers. `skill`: the tags and name of the
    /// skill, where it is known (its conditions are then decided here).
    let private compileConds (buffId: string -> int) (statusId: string -> int) (skill: (Set<SkillTag> * string) option) (conds: Cond list) : CC[] option * bool =
        let rec one (c: Cond) : CC =
            match c with
            | IsCrit -> CCrit
            | SkillIs t -> (match skill with Some (tags, _) -> (if tags.Contains t then CTrue else CFalse) | None -> CTag t)
            | SkillNamed n -> (match skill with Some (_, name) -> (if name = n then CTrue else CFalse) | None -> CNamed n)
            | WhileBuff b -> CBuff(buffId b)
            | EnemyHas st -> CStatus(statusId st)
            | PerStack (b, every) -> CPerStack(buffId b, every)
            | PerEnemyStack (st, every) -> CPerEnemy(statusId st, every)
            | Not inner ->
                match one inner with
                | CTrue | CPerStack _ | CPerEnemy _ -> CFalse
                | CFalse -> CTrue
                | x -> CNot x
        let all = conds |> List.map one
        if all |> List.contains CFalse then None, false
        else
            let kept = all |> List.filter (fun c -> c <> CTrue) |> Array.ofList
            Some kept, kept |> Array.exists (fun c -> match c with CPerStack _ | CPerEnemy _ -> true | _ -> false)

    /// A question from its items in the reference's order: what can be worked out now is.
    let private makeQuery (kind: ModKind) (items: Item[]) : Query =
        let fixed' (it: Item) = it.Slot < 0 && it.Conds.Length = 0
        let rec onBuffs (c: CC) = match c with CBuff _ | CPerStack _ -> true | CNot inner -> onBuffs inner | _ -> false
        let rec onStatus (c: CC) = match c with CStatus _ | CPerEnemy _ -> true | CNot inner -> onStatus inner | _ -> false
        // (a buff's mods can have conditions on anything)
        let deps (q: Query) =
            { q with
                OnBuffs = q.Items |> Array.exists (fun it -> it.Slot >= 0 || it.Conds |> Array.exists onBuffs)
                OnStatus = q.Items |> Array.exists (fun it -> it.Conds |> Array.exists onStatus)
                HasSlot = q.Items |> Array.exists (fun it -> it.Slot >= 0) }
        deps <|
        match kind with
        | Flag ->
            if items |> Array.exists (fun it -> fixed' it && it.V <> 0.0) then { Items = [||]; Start = 0; Init = 1.0; OnBuffs = false; OnStatus = false; HasSlot = false }
            else { Items = items |> Array.filter (fun it -> not (fixed' it)); Start = 0; Init = 0.0; OnBuffs = false; OnStatus = false; HasSlot = false }
        | Override ->
            // (the last one that holds is the answer: nothing before the last sure one matters)
            let mutable from = 0
            for i in 0 .. items.Length - 1 do
                if fixed' items.[i] then from <- i
            { Items = (if items.Length = 0 then items else items.[from..]); Start = 0; Init = 0.0; OnBuffs = false; OnStatus = false; HasSlot = false }
        | _ ->
            // the mods without conditions are worked out now, whatever comes between them (the
            // reference takes each in its turn: the same but for rounding where a conditional
            // one, or a buff's, counts too)
            let mutable acc = if kind = More then 1.0 else 0.0
            for it in items do
                if fixed' it then acc <- (if kind = More then acc * (1.0 + it.V / 100.0) else acc + it.V)
            { Items = items |> Array.filter (fun it -> not (fixed' it)); Start = 0; Init = acc; OnBuffs = false; OnStatus = false; HasSlot = false }

    // ---- the questions the engine asks of a skill ----

    [<Literal>]
    let private MSkill = 0
    /// Asked with nothing known (`Ctx.empty`): no skill, no buffs, no enemy.
    [<Literal>]
    let private MEmpty = 1
    /// Asked as the skill's damage over time is: without the skill being a spell or an attack.
    [<Literal>]
    let private MDot = 2

    let private sites = ResizeArray<int * ModKind * Stat list>()
    let private site (mode: int) (kind: ModKind) (stats: Stat list) : int =
        sites.Add((mode, kind, stats))
        sites.Count - 1
    /// The increased and the more of the stats (for `multQ`).
    let private siteMult (mode: int) (stats: Stat list) : int =
        let i = site mode Inc stats
        site mode More stats |> ignore
        i
    let private named (n: string) = [ Named n ]
    let private perType (mode: int) (kind: ModKind) (f: DamageType -> Stat list) : int[] =
        DamageType.all |> List.map (fun t -> site mode kind (f t)) |> Array.ofList
    let private perAilment (make: Ailment -> int) : int[] = allAilments |> List.map make |> Array.ofList
    let private scopes (t: DamageType) = [ Damage AllDamage; Damage(OfType t) ] @ (if DamageType.isElemental t then [ Damage Elemental ] else [])

    let private qOpenMore = site MEmpty Base (named "OpenWeaknessMore")
    let private qReveal = site MEmpty Flag (named "RevealWeakness")
    let private qHitChance = site MSkill Override [ HitChance ]
    let private qAccuracy = site MSkill Base (named "Accuracy")
    let private qAccuracyMult = siteMult MSkill (named "Accuracy")
    let private qEnemyTaken = site MSkill Inc (named "EnemyDamageTaken")
    let private qCritOver = site MSkill Override [ CritChance ]
    let private qCritBase = site MSkill Base [ CritChance ]
    let private qCritMult = siteMult MSkill [ CritChance ]
    let private qMaxCrit = site MSkill Base (named "MaxCritChance")
    let private qLuckyCrit = site MSkill Flag (named "LuckyCrit")
    let private qInevitable = site MSkill Flag (named "InevitableCrits")
    let private qBifurcate = site MSkill Flag (named "BifurcateCrits")
    let private qLucky = site MSkill Flag [ LuckyDamage ]
    let private qLuckyChance = site MSkill Base (named "LuckyDamageChance")
    let private qCritBonus = site MSkill Base [ CritBonus ]
    let private qCritBonusMult = siteMult MSkill [ CritBonus ]
    let private qDouble = site MSkill Base [ DoubleDamageChance ]
    let private qAddedEffect = siteMult MSkill [ AddedDamageEffect ]
    let private qAddedMin = perType MSkill Base (fun t -> [ AddedMin t ])
    let private qAddedMax = perType MSkill Base (fun t -> [ AddedMax t ])
    let private pairs (make: DamageType * DamageType -> Stat) =
        [| for from in DamageType.all do
            for into in DamageType.all -> site MSkill Base [ make (from, into) ] |]
    let private qGain = pairs GainAs
    let private qSkillConvert = pairs SkillConvert
    let private qConvert = pairs Convert
    let private qNoNonFire = site MSkill Flag (named "DealNoNonFire")
    let private qDamage = DamageType.all |> List.map (fun t -> siteMult MSkill (scopes t)) |> Array.ofList
    let private qNegate = site MSkill Base (named "NegateEnemyResistChance")
    let private qEnemyResist = perType MSkill Base (fun t -> named ("EnemyResist:" + DamageType.name t))
    let private qPen = perType MSkill Base (fun t -> [ Penetration t ])
    let private qPenFloorOver = site MSkill Override (named "PenetrationFloorOverride")
    let private qPenFloor = site MSkill Base (named "PenetrationFloor")
    let private qIgnoreElemental = site MSkill Flag (named "IgnoreElementalResist")
    let private qIgnoreResist = perType MSkill Flag (fun t -> named ("IgnoreResist:" + DamageType.name t))
    let private qCulling = site MSkill Flag (named "CullingStrike")
    let private qCullThreshold = site MSkill Inc (named "CullingThreshold")
    let private qCloseMore = site MSkill Base (named "CloseCombatMore")
    let private qFarMore = site MSkill Base (named "FarCombatMore")
    let private qAllIgnites = site MSkill Flag (named "AllDamageIgnites")
    let private qAllChills = site MSkill Flag (named "AllDamageChills")
    let private qAllShocks = site MSkill Flag (named "AllDamageShocks")
    let private qAilChance = perAilment (fun a -> site MSkill Base [ AilmentChance a ])
    let private qAilChanceMult = perAilment (fun a -> siteMult MSkill [ AilmentChance a ])
    let private qFaster = site MSkill Inc (named "FasterAilments")
    let private qFasterIgnite = site MSkill Inc [ Named "FasterAilments"; Named "FasterIgnite" ]
    let private qMagnitude = perAilment (fun a -> siteMult MSkill [ AilmentMagnitude(Some a); AilmentMagnitude None ])
    let private qDuration = perAilment (fun a -> siteMult MSkill [ AilmentDuration(Some a); AilmentDuration None ])
    let private qBleedDuration = site MSkill Override (named "BaseBleedDuration")
    let private qPoisonDuration = site MSkill Override (named "BasePoisonDuration")
    let private qAilStacks = perAilment (fun a -> site MSkill Base [ AilmentStacks a ])
    let private qCanElectrocute = site MSkill Flag (named "CanElectrocute")
    let private qCanPin = site MSkill Flag (named "CanPin")
    let private qBuildup = perAilment (fun a -> siteMult MSkill [ Buildup a ])
    let private qNoHitDamage = site MSkill Flag (named "NoHitDamage")
    let private qBreakPhysical = site MSkill Base (named "ArmourBreakOfPhysical")
    let private qBreakFire = site MSkill Base (named "ArmourBreakOfFire")
    let private qBreakDamage = site MSkill Base (named "ArmourBreakOfDamage")
    let private qBreakFlat = site MSkill Base (named "ArmourBreakFlat")
    let private qBreakAmount = siteMult MSkill (named "ArmourBreakAmount")
    let private qBreakDuration = site MSkill Inc (named "ArmourBreakDuration")
    let private qBrokenEffect = site MSkill Inc (named "FullyBrokenArmourEffect")
    let private qBrokenAll = site MSkill Flag (named "BrokenArmourAllDamage")
    let private qBrokenFire = site MSkill Flag (named "BrokenArmourFire")
    let private qBrokenColdLightning = site MSkill Flag (named "BrokenArmourColdLightning")
    let private qManaLeech = site MSkill Base [ ManaLeech ]
    let private qManaLeechAmount = siteMult MSkill (named "ManaLeechAmount")
    let private qLifeLeech = site MSkill Base (named "LifeLeech")
    let private qLifeLeechAmount = siteMult MSkill (named "LifeLeechAmount")
    let private qLifeOnHit = site MSkill Base (named "LifeOnHit")
    let private qManaOnHit = site MSkill Base (named "ManaOnHit")
    let private qManaOnKill = site MEmpty Base [ ManaOnKill ]
    let private qManaPercentOnKill = site MEmpty Base (named "ManaPercentOnKill")
    let private qSkillDuration = siteMult MSkill [ SkillDuration ]
    let private qDotDamageAs = DamageType.all |> List.map (fun t -> siteMult MDot (scopes t)) |> Array.ofList
    let private qDotDamage = siteMult MDot (named "DotDamage")
    let private qSpellDotDamage = siteMult MSkill (named "DotDamage")
    let private qRageOnHit = site MSkill Base (named "RageOnHit")
    let private qArea = siteMult MSkill [ AreaOfEffect ]
    let private qProjectiles = site MSkill Base [ AdditionalProjectiles ]
    let private qExtraProjectiles2 = site MSkill Base (named "ExtraProjectileChance2")
    let private qExtraProjectiles1 = site MSkill Base (named "ExtraProjectileChance1")
    let private qPierce = site MSkill Base [ Pierce ]
    let private qPierceChance = site MSkill Base (named "PierceChance")
    let private qChains = site MSkill Base [ Chains ]
    let private qTerrainChain = site MSkill Base (named "TerrainChain")
    let private qAftershock = site MSkill Base (named "AftershockChance")
    let private qCooldownRecovery = siteMult MSkill [ CooldownRecovery ]
    let private qDoubleUses = site MSkill Base (named "DoubleCooldownUses")
    let private qLosePools = site MSkill Base (named "LosePoolsOnUse")
    let private qDodgeDistance = site MSkill Base (named "DodgeRollDistance")
    let private qSpeed = siteMult MSkill [ Speed ]
    let private qFixedUse = site MSkill Base (named "FixedUseTime")
    let private qExtraCost = site MSkill Base (named "ExtraCostPercentOfMaxMana")
    let private qManaCost = siteMult MSkill [ ManaCost ]
    let private qCostEfficiency = site MSkill Inc [ ManaCostEfficiency ]
    let private qSkipConsume = site MEmpty Base (named "ChargeSkipConsume")
    let private qConsumeEvery = site MEmpty Base (named "ConsumeChargeEveryMs")
    let private qConsumeCount = site MEmpty Base (named "ConsumeChargeCount")
    let private qRageEffect = siteMult MEmpty (named "RageEffect")

    let private damageTypes = Array.ofList DamageType.all

    // ---- a program ----

    /// A reaction with its names as numbers (see `Reaction`).
    type private Re =
        | RGrantBuff of buff: int * duration: float * maxStacks: int * mods: Mod list
        /// `blocked`: by skill, whether the skill cannot inflict it.
        | RApplyDebuff of status: int * duration: float * maxStacks: int * mods: Mod list * blocked: bool[]
        /// `mods`: by enemy, scaled by its curse effect. `others`: the statuses of the same
        /// kind (curses, or marks), in the order of their names. `limit`: how many of the kind
        /// an enemy can have.
        | RApplyCurse of status: int * duration: float * mods: Mod list[] * others: int[] * limit: int * count: int
        | RApplyExposure of damageType: DamageType * amount: float * duration: float
        | RGrowExposure of damageType: DamageType * start: float * perPulse: float * cap: float * every: float * pulses: int * duration: float
        | RTrigger of skill: int
        | RRemoveBuff of buff: int
        | RRemoveDebuff of status: int
        | RGainResource of buff: int * count: int
        | RDropRemnant of buff: int * delay: float * extraChance: float * count: int
        | RSpendStack of buff: int * next: Re
        | RSetStacks of buff: int * stacks: int
        | RIfEnemyHas of status: string * next: Re
        | RIfBuff of buff: int * next: Re
        | RRegenES of perSecond: float * duration: float * count: int
        /// `meta`: -1 if the actor has no meta gem of the name (nothing happens then).
        | RGainEnergy of meta: int * amount: float
        /// `counts`: by the number of attacks empowered, where the count of it goes.
        | REmpower of buff: int * perPower: float * cap: float * counts: int[]
        | RNone

    /// What happens on its own every few seconds, with its names as numbers (see `PeriodicAction`).
    type private PAction =
        /// The resources with the most stacks each can have.
        | PGainRandom of (int * int)[]
        | PGainAnyRandom of (int * int)[]
        | PGainWeighted of (int * int * float)[]
        | PConsumeEach of int[]
        | PGainTimedBuff of buff: int * chance: float * duration: float * maxStacks: int * mods: Mod list
        | PHealLife of amount: float * count: int
        | PTrigger of skill: int

    type private PeriodicInfo =
        { Every: float
          Action: PAction
          /// The name of its stream of random numbers.
          Key: uint32 }

    type private MetaInfo =
        { Meta: Meta
          Triggers: int[]
          /// Whether the character has "increased Energy" of its own for it.
          HasEnergyMods: bool
          Released: int }

    type private CharmInfo =
        { Charm: Charm
          CBuff: int
          Used: int }

    type private CRule =
        { /// By skill: whether the rule's filter lets the skill through.
          Applies: bool[]
          Chance: float
          Re: Re
          /// Of a rule for kills of enemies in some state: the state.
          Killed: string }

    type private SkillInfo =
        { Skill: Skill
          /// The character's mods and the skill's (`Sim.Env.SkillDbs`).
          Db: ModDb
          /// The skill and its parts.
          Parts: int[]
          /// The buff the skill consumes (-1: none).
          Consumes: int
          /// The charge whose stacks the skill counts more of ("N additional consumed"), and
          /// how many more (see `Sim.skillStacks`).
          StackCharge: string
          StackBuff: int
          StackExtra: int
          /// The answers to the engine's questions (see `sites`), and to each archon's.
          Q: Query[]
          QArchon: Query[]
          /// Base damage by type.
          BaseMin: float[]
          BaseMax: float[]
          /// Whether a mod of the character or the skill gains damage as extra, or converts it.
          HasGain: bool
          HasConvert: bool
          DotTags: Set<SkillTag>
          SpellDot: bool
          IsMelee: bool
          /// The buff whose stacks add projectiles (-1: none).
          PerChargeBuff: int
          /// When a use's hits land: whether as a share of the use time (else seconds after
          /// its start), the share or the seconds, the damage multiplier.
          PatShare: bool[]
          PatAt: float[]
          PatMult: float[]
          CostCtx: Ctx
          // where its numbers go
          Source: int
          CUses: int
          CHits: int
          CCrits: int
          CMiss: int
          CTrigger: int
          CAftershock: int
          CCannotAfford: int
          CKept: int
          CConsumed: int
          // the names of its streams of random numbers
          StrikeKey: uint32
          ConsumeKey: uint32
          RollKey: uint32
          Sustained: bool
          IsAttack: bool
          IsSlam: bool
          Passing: bool
          /// Its damage over time: type, per second, label, where the damage goes.
          Dots: (DamageType * float * string * int)[]
          // what a part needs to happen (see `Sim.startUse`)
          OncePerUse: bool
          OnlyTriggered: bool
          NotEnraged: bool
          NotWithCharge: bool
          WithCharge: bool
          Explosion: bool
          HasChargedPart: bool
          NeedsCharge: bool
          /// An attack that uses up empowerments (a warcry's), and one that uses up readied repeats.
          SpendsEmpowered: bool
          SpendsRepeats: bool }

    type private EnemyInfo =
        { Def: EnemyDef
          Db: ModDb
          CurseK: float
          ExposureK: float
          Evasion: float
          BaseArmour: float
          CullLife: float
          /// Its own mods as a defender has them: newest first, with their values.
          Own: (Stat * ModKind * float * bool)[]
          /// The debuffs it has for the whole fight: status, stacks, mods.
          Auras: (int * int * Mod list) list }

    type private CycleInfo =
        { Buff: int
          Period: float
          Variants: Mod list[]
          Picked: int[] }

    type private ArchonInfo =
        { Buff: int
          Stat: string
          Duration: float
          Recovery: float
          AMods: Mod list
          Gained: int }

    /// A fight made ready to be played. Nothing in it changes.
    type Program =
        private
            { Fight: Fight
              Vitals: PlayerStats option
              BuffNames: string[]
              /// The defences for sets of buffs (see `compile`): the buffs written out, as `vitalsKey` writes them.
              VitalsKnown: Dictionary<string, PlayerStats>
              /// The buffs the defences depend on (`Actor.VitalsReads`; all of them where that is not said).
              VitalsOn: int[]
              BuffIds: Dictionary<string, int>
              /// Where the mods of buffs go, by stat and kind; how many such places; those of
              /// gained and of converted damage; Rage's.
              DynKeys: Dictionary<string, int>
              DynCount: int
              NoDyn: Item[][]
              NoItems: Item[]
              GainSlots: int[]
              ConvSlots: int[]
              RageSlot: int
              /// By buff: the chance of one more when it is gained (asked of the first skill).
              QExtra: Query[]
              SIso: int
              SClose: int
              SOpen: int
              /// The statuses an enemy's state gives it, in `derivedStatuses`' order.
              SDerived: int[]
              /// The buffs in the order of their names (the order the reference's maps go through them).
              BuffOrder: int[]
              StatusNames: string[]
              StatusOrder: int[]
              StatusIds: Dictionary<string, int>
              StatusCurse: bool[]
              StatusMark: bool[]
              StatusExposure: bool[]
              CountNames: string[]
              SourceNames: string[]
              Skills: SkillInfo[]
              /// The skill the mods of a gained charge are asked of (the actor's first).
              FirstSkill: int
              Rotation: (int * (View -> bool))[]
              OnUse: CRule[]
              OnHit: CRule[]
              OnCrit: CRule[]
              OnKill: CRule[]
              OnKillOf: CRule[]
              /// By ailment.
              OnInflict: CRule[][]
              WhenHit: CRule[]
              OnBlock: CRule[]
              Metas: MetaInfo[]
              Charms: CharmInfo[]
              /// By what a charm stops: where the count of it being stopped goes.
              CPrevented: Dictionary<string, int>
              Periodics: PeriodicInfo[]
              /// Rage: regenerated per second, lost per second, seconds without gaining before it
              /// is lost, gained when hit, gained on kill; life lost per minute per Rage (Berserk).
              RageRegen: float
              RageLoss: float
              RageDelay: float
              RageWhenHit: float
              RageOnKill: float
              BerserkLoss: float
              MaxRage: int
              /// The enemies that attack, in the order the fight names them, and the names of
              /// their streams of random numbers (by enemy).
              Attackers: int[]
              AttackKeys: uint32[]
              /// The character's own chance to keep a charge, and the mana (a share of the most)
              /// that consuming a Power Charge gives.
              SheetSkip: float
              ConsumePowerMana: float
              Enemies: EnemyInfo[]
              EnemyIds: Dictionary<string, int>
              Cycles: CycleInfo[]
              Archons: ArchonInfo[]
              /// The resources the fight starts with: buff, stacks, seconds they last (if they do).
              Resources: (int * int * float option) list
              /// By buff: seconds its stacks last (NaN: for ever), the most stacks (the least int: not said).
              ResourceSeconds: float[]
              ResourceMax: int[]
              /// The resources with a duration, in the order of their names.
              Timed: int[]
              /// The buffs that empower attacks, and those that are repeats readied for one, in
              /// the order of their names.
              Empowering: int[]
              Readied: int[]
              /// By buff: the "Gained<buff>Recently" buff, whether its name ends with "Charge",
              /// what is asked for a chance of one more, where its counts go.
              GainedRecently: int[]
              ConsumedRecently: int[]
              CKeptOf: int[]
              CLostOf: int[]
              IsCharge: bool[]
              CGained: int[]
              CExpired: int[]
              /// Ailments: where their damage goes, their statuses, their counts.
              AilmentSource: int[]
              AilmentStatus: int[]
              AilmentResistant: int[]
              CInflict: int[]
              ExposureStatus: int[]
              // buffs and statuses the engine itself reads and writes
              BRage: int
              BHit: int
              BNonCrit: int
              BCrit: int
              BCrit8: int
              BKilled: int
              BTriggered: int
              BShocked: int
              BChilled: int
              BBeenHit: int
              CStopped: int
              CEvaded: int
              CEnemyHit: int
              CDeflected: int
              CBlocked: int
              CEnemyCrit: int
              CFlaskLife: int
              CPlayerIgnited: int
              CPlayerShocked: int
              CPlayerChilled: int
              CPlayerFrozen: int
              CPlayerStunned: int
              /// The player's own states (full life, low mana...), in the reference's order.
              BStates: int[]
              BCharges: int[]
              BConstantMana: int
              BConstantLife: int
              SShocked: int
              SChilled: int
              SArmourBreak: int
              SFullyBroken: int
              CFlaskMana: int
              CPoolLoss: int
              CAborted: int
              HasKillOf: bool }

    /// Names that get numbers as they are first asked for.
    type private Names() =
        let ids = Dictionary<string, int>()
        let names = ResizeArray<string>()
        member _.Id(name: string) : int =
            match ids.TryGetValue name with
            | true, i -> i
            | _ ->
                let i = names.Count
                ids.[name] <- i
                names.Add name
                i
        member _.Names = names.ToArray()
        member _.Ids = ids
        /// The numbers in the order of their names: the order a map keyed by the names has.
        member _.Order =
            names.ToArray() |> Array.mapi (fun i n -> n, i) |> Array.sortBy fst |> Array.map snd

    // ---- what a fight may not have ----

    /// Stats that make the reference do something that is not built here. A fight with a mod
    /// of one of them, wherever the mod is, is not compiled.
    let private unbuiltStats =
        [ "CascadesPerSide", "Spell Cascade"
          "TriskelionArms", "Triskelion Cascade"
          "TrinityGain", "Trinity"
          "SigilMaxStages", "Sigil of Power"
          "DetonatedByDamage", "seeds set off by damage (Living Bomb)"
          "DetonatesCurse", "curse detonation (Hexblast)"
          "NoCooldownWithCharge", "skills that skip their cooldown when empowered"
          "Crystals", "Ice Crystals"
          "ConsumesVs", "parts against an enemy state"
          "SeqLength", "chained attacks"
          "SeqHits", "chained attacks"
          "MaxSeals", "sealed skills"
          "BarrageRepeats", "Barrage"
          "WindowSeconds", "skills with a window (Requiem)"
          "SpendsRage", "parts that spend Rage"
          "RecordsDamage", "debuffs that record damage (Inevitable Agony)" ]

    let private unbuiltPrefixes =
        [ "DetonatedBy:", "parts set off by another skill"
          "OnlyInfused:", "infusions"
          "SeqSteps:", "chained attacks"
          "WarpsInto:", "warps into projectiles"
          "ToggleBuff:", "forms that are switched on"
          "OnlyVs:", "parts against an enemy state"
          "NotVs:", "parts against an enemy state"
          "Takes:", "hits that consume an enemy state"
          "NeedsEnemyStacks:", "skills that wait for stacks on the enemy"
          "Requires:", "Glory and Combo" ]

    /// Buffs the reference treats in a way of its own that is not built here.
    let private unbuiltBuffs = [ "Demon Form", "Demon Form"; "Barrage", "Barrage" ]
    let private unbuiltBuffPrefixes = [ "Warped:", "warps into projectiles" ]

    let private recentlyNames = [ "HitRecently"; "NonCritRecently"; "CritRecently"; "CritInPast8Seconds"; "KilledRecently" ]

    let private stateNames =
        [ "FullLife"; "LowLife"; "FullMana"; "LowMana"; "HasEnergyShield"; "ManaFlaskEffect"; "LifeFlaskEffect"; "FlaskEffect"; "Leeching"; "Surrounded" ]
        @ [ for kind in [ "Power"; "Frenzy"; "Endurance" ] do
                yield "AtMaximum" + kind + "Charges"
                yield "No" + kind + "Charges"
                yield "YouHaveNo" + kind + "Charges" ]

    let private derivedStatuses =
        [ "Moving"; "Cullable"; "AnyAilment"; "Cursed"; "Marked"; "Immobilised"; "ElementalAilments"; "Exposed"; "RareOrUnique"; "LowLife"; "FullLife" ]

    let private matches filter (skill: Skill) =
        match filter with
        | AnySkill -> true
        | WithTag t -> Set.contains t skill.Tags
        | WithTagNot (t, n) -> Set.contains t skill.Tags && not (Set.contains n skill.Tags)
        | WithTags (has, hasNot) -> has |> List.forall skill.Tags.Contains && not (hasNot |> List.exists skill.Tags.Contains)
        | Called n -> skill.Name = n

    let private reactionName (r: Reaction) =
        match r with
        | GrantBuff _ -> "GrantBuff"
        | ApplyDebuff _ -> "ApplyDebuff"
        | ApplyCurse _ -> "ApplyCurse"
        | ApplyExposure _ -> "ApplyExposure"
        | GrowExposure _ -> "GrowExposure"
        | TriggerSkill _ -> "TriggerSkill"
        | GainEnergy _ -> "GainEnergy"
        | RemoveBuff _ -> "RemoveBuff"
        | GrantMinionBuff _ -> "GrantMinionBuff"
        | Summon _ -> "Summon"
        | SummonEmpowered _ -> "SummonEmpowered"
        | WithCorpse _ -> "WithCorpse"
        | SummonPerCorpse _ -> "SummonPerCorpse"
        | SummonPerWard _ -> "SummonPerWard"
        | GrantOneOf _ -> "GrantOneOf"
        | AttachPustule _ -> "AttachPustule"
        | NoReaction -> "NoReaction"
        | RemoveDebuff _ -> "RemoveDebuff"
        | GainResource _ -> "GainResource"
        | DropRemnant _ -> "DropRemnant"
        | ChargeBuff _ -> "ChargeBuff"
        | SpendStack _ -> "SpendStack"
        | SetStacks _ -> "SetStacks"
        | IfEnemyHas _ -> "IfEnemyHas"
        | IfBuff _ -> "IfBuff"
        | RegenES _ -> "RegenES"
        | Empower _ -> "Empower"

    /// The program of a fight, or everything about the fight that is not built here (each
    /// thing once, in no particular order).
    let private build (fight: Fight) : Result<Program, string list> =
        let player = fight.Player
        let no = ResizeArray<string>()
        let reject (what: string) = if not (no.Contains what) then no.Add what
        let buffs = Names()
        let statuses = Names()
        let counts = Names()
        let sources = Names()

        // ---- the fight ----
        if not fight.Minions.IsEmpty then reject "minions and totems"
        if fight.Replace then reject "enemies that are replaced"
        if fight.Enemies.IsEmpty then reject "a fight without enemies"
        if (fight.Enemies |> List.map (fun e -> e.Name) |> List.distinct).Length <> fight.Enemies.Length then reject "enemies of the same name"
        // (the player's evasion entropy goes by this name in the reference, whatever the player is called)
        if fight.Enemies |> List.exists (fun e -> e.Name = "Player") then reject "an enemy called Player"

        // ---- the player ----
        if (player.Metas |> List.map (fun m -> m.Name) |> List.distinct).Length <> player.Metas.Length then reject "meta gems of the same name"
        if player.WardRemnants.IsSome then reject "Runic Ward remnants"
        if player.ManaRemnants.IsSome then reject "mana remnants"
        if not player.Banners.IsEmpty then reject "banners"
        if player.ActiveWhile.IsSome || player.Body.IsSome || player.Pool.IsSome then reject "a player that is a minion"
        // (infusions that nothing drops and nothing consumes do nothing)
        if player.Infusions |> Option.exists (fun i -> not i.Remnants.IsEmpty || not i.Consumers.IsEmpty) then reject "infusions"
        if not player.OverkillBurns.IsEmpty then reject "overkill explosions (Herald of Ash)"
        if player.Vitals |> Option.exists (fun v -> v.CostsInfernalFlame) then reject "Infernal Flame"

        // ---- its skills ----
        let all = player.Skills @ (player.Skills |> List.collect (fun s -> s.Parts))
        if (all |> List.map (fun s -> s.Name) |> List.distinct).Length <> all.Length then reject "skills or parts of the same name"
        if player.Skills |> List.exists (fun s -> s.Parts |> List.exists (fun p -> not p.Parts.IsEmpty)) then reject "parts that have parts"
        if all |> List.exists (fun s -> s.Clip > 0) then reject "crossbow ammunition"
        // (the reference keeps these beside the skills' cooldowns, under these names)
        if all |> List.exists (fun s -> s.Name = "ArchonRecovery" || s.Name.StartsWith "invocation:" || s.Name.StartsWith "SealBreak:") then reject "a skill named like a cooldown of the engine's own"
        if all |> List.exists (fun s -> s.Tags.Contains(Tag "Type:TargetsDestructibleCorpses")) then reject "corpse skills"
        let skillIds = Dictionary<string, int>()
        all |> List.iteri (fun i s -> if not (skillIds.ContainsKey s.Name) then skillIds.[s.Name] <- i)
        let skillId (what: string) (name: string) =
            match skillIds.TryGetValue name with
            | true, i -> i
            | _ ->
                reject (what + " of a skill the player does not have")
                0

        // ---- where the mods of buffs go ----
        let dynKeys = Dictionary<string, int>()
        let dynStats = ResizeArray<Stat>()
        let dynOf (stat: Stat) (kind: ModKind) =
            let key = statKey stat kind
            if not (dynKeys.ContainsKey key) then
                dynKeys.[key] <- dynStats.Count
                dynStats.Add stat
            dynKeys.[key]
        let buffMods (mods: Mod list) =
            for m in mods do
                dynOf m.Stat m.Kind |> ignore
        let rageSlot = dynOf (Damage AllDamage) More
        // (what a monster's hit can leave on the player)
        dynOf (Named "ShockTaken") Base |> ignore
        dynOf Speed More |> ignore

        // ---- every mod there can be ----
        let checkMods (mods: Mod list) =
            for m in mods do
                match m.Stat with
                | Named n ->
                    for (stat, what) in unbuiltStats do
                        if n = stat then reject what
                    for (prefix, what) in unbuiltPrefixes do
                        if n.StartsWith prefix then reject what
                | _ -> ()
        checkMods player.Mods
        for s in all do
            checkMods s.Mods
        let buff (name: string) =
            for (b, what) in unbuiltBuffs do
                if name = b then reject what
            for (prefix, what) in unbuiltBuffPrefixes do
                if name.StartsWith prefix then reject what
            buffs.Id name

        // ---- names the engine itself uses ----
        let bRage = buffs.Id "Rage"
        let bRecently = recentlyNames |> List.map buffs.Id |> Array.ofList
        let bTriggered = buffs.Id "TriggeredRecently"
        let bShocked = buffs.Id "Shocked"
        let bChilled = buffs.Id "Chilled"
        let bBeenHit = buffs.Id "BeenHitRecently"
        let bStates = stateNames |> List.map buffs.Id |> Array.ofList
        let bCharges = [ "PowerCharge"; "FrenzyCharge"; "EnduranceCharge" ] |> List.map buffs.Id |> Array.ofList
        let bConstantMana = buffs.Id "ConstantManaFlask"
        let bConstantLife = buffs.Id "ConstantLifeFlask"
        let ailmentStatus = allAilments |> List.map (fun a -> statuses.Id(Ailment.status a)) |> Array.ofList
        let ailmentResistant = allAilments |> List.map (fun a -> statuses.Id(Ailment.name a + "Resistant")) |> Array.ofList
        let ailmentSource = allAilments |> List.map (fun a -> sources.Id(Ailment.name a)) |> Array.ofList
        let cInflict = allAilments |> List.map (fun a -> counts.Id("inflict:" + Ailment.name a)) |> Array.ofList
        let exposureStatus = DamageType.all |> List.map (fun t -> statuses.Id("Exposure:" + DamageType.name t)) |> Array.ofList
        for n in derivedStatuses do
            statuses.Id n |> ignore
        let sArmourBreak = statuses.Id "ArmourBreak"
        let sFullyBroken = statuses.Id "FullyBrokenArmour"

        // ---- resources ----
        let resources =
            player.Resources
            |> List.filter (fun (_, n) -> n > 0)
            |> List.map (fun (k, n) -> buff k, n, Map.tryFind k player.ResourceDurations)
        for KeyValue (k, _) in player.ResourceDurations do
            buff k |> ignore
        for KeyValue (k, _) in player.MaxResources do
            buffs.Id k |> ignore

        // ---- rules ----
        let sheet = ModDb.ofList player.Mods
        let maxCurses = 1 + int (Mods.sumBase Ctx.empty [ Named "MaxCurses" ] sheet)
        let enemies = fight.Enemies |> List.sortBy (fun e -> e.Name) |> Array.ofList
        let curseK (e: EnemyDef) = Mods.more Ctx.empty [ Named "CurseEffectOnSelf" ] (ModDb.ofList e.Mods)
        // (curses name the others of their kind, which are only all known once every rule
        // has been read: they are filled in below)
        let curses = ResizeArray<string * int>()
        let rec reaction (r: Reaction) : Re =
            match r with
            | GrantBuff (name, duration, maxStacks, mods) ->
                checkMods mods
                buffMods mods
                RGrantBuff(buff name, duration, maxStacks, mods)
            | ApplyDebuff (name, duration, maxStacks, mods) ->
                let blocked = all |> List.map (fun s -> s.Mods |> List.exists (fun m -> m.Stat = Named("CannotInflict:" + name))) |> Array.ofList
                RApplyDebuff(statuses.Id name, duration, maxStacks, mods, blocked)
            | ApplyCurse (name, duration, mods) ->
                let full = if name.StartsWith "Curse:" || name.StartsWith "Mark:" then name else "Curse:" + name
                let prefix = full.Substring(0, full.IndexOf ':' + 1)
                let id = statuses.Id full
                curses.Add(prefix, id)
                RApplyCurse(id, duration, enemies |> Array.map (fun e -> let k = curseK e in mods |> List.map (Mod.scaled k)), [||], (if prefix = "Mark:" then 1 else maxCurses), counts.Id("apply:" + full))
            | ApplyExposure (dt, amount, duration) -> RApplyExposure(dt, amount, duration)
            | GrowExposure (dt, start, perPulse, cap, every, pulses, duration) -> RGrowExposure(dt, start, perPulse, cap, every, pulses, duration)
            | TriggerSkill name -> RTrigger(skillId "a trigger" name)
            | RemoveBuff name -> RRemoveBuff(buffs.Id name)
            | RemoveDebuff name -> RRemoveDebuff(statuses.Id name)
            | GainResource (r, n) -> RGainResource(buff r, n)
            | DropRemnant (r, delay, extra) -> RDropRemnant(buff r, delay, extra, counts.Id("remnant:" + r))
            | SpendStack (b, next) -> RSpendStack(buffs.Id b, reaction next)
            | SetStacks (b, n) -> RSetStacks(buff b, n)
            | IfEnemyHas (st, next) -> RIfEnemyHas(st, reaction next)
            | IfBuff (b, next) -> RIfBuff(buffs.Id b, reaction next)
            | RegenES (rate, duration) -> RRegenES(rate, duration, counts.Id "regen:ES")
            | GainEnergy (meta, amount) -> RGainEnergy((match player.Metas |> List.tryFindIndex (fun m -> m.Name = meta) with Some i -> i | None -> -1), amount)
            | Empower (b, perPower, cap) ->
                // (the most there can be: with every enemy alive)
                let power = fight.Enemies |> List.sumBy (fun e -> MonsterPower.ofRarity e.Rarity)
                let most = max 1 (int (floor (min cap power / max 1.0 perPower)))
                REmpower(buff b, perPower, cap, Array.init (most + 1) (fun n -> counts.Id("empowered:" + string n)))
            | NoReaction -> RNone
            | other ->
                reject ("the reaction " + reactionName other)
                RNone
        // (a charge buff is up when the fight starts, whatever its rule's hook is)
        if player.Rules |> List.exists (fun r -> match r.Reaction with ChargeBuff _ -> true | _ -> false) then reject "the reaction ChargeBuff"
        let rule (r: Rule) : CRule =
            { Applies = all |> List.map (matches r.Filter) |> Array.ofList
              Chance = r.Chance
              Re = reaction r.Reaction
              Killed = (match r.Hook with OnKillOf st -> st | _ -> "") }
        let rulesOf (pick: Hook -> bool) = player.Rules |> List.filter (fun r -> pick r.Hook) |> List.map rule |> Array.ofList
        let onUse = rulesOf (fun h -> h = OnUse)
        let onHit = rulesOf (fun h -> h = OnHit)
        let onCrit = rulesOf (fun h -> h = OnCrit)
        let onKill = rulesOf (fun h -> h = OnKill)
        let onKillOf = rulesOf (fun h -> match h with OnKillOf _ -> true | _ -> false)
        let onInflict = allAilments |> List.map (fun a -> rulesOf (fun h -> h = OnInflict a)) |> Array.ofList
        let whenHit = rulesOf (fun h -> h = WhenHit)
        let onBlock = rulesOf (fun h -> h = OnBlock)

        // ---- meta gems ----
        let metas =
            player.Metas
            |> List.map (fun m ->
                ({ Meta = m
                   Triggers = m.Triggers |> List.map (skillId "a meta gem's trigger") |> Array.ofList
                   HasEnergyMods = ModDb.hasStat (fun st -> st = Named "EnergyGenerated") m.EnergyMods
                   Released = counts.Id("meta:" + m.Name) }: MetaInfo))
            |> Array.ofList

        // ---- charms ----
        let charms =
            player.Charms
            |> List.map (fun c -> ({ Charm = c; CBuff = buff c.Buff; Used = counts.Id("charm:" + c.Name) }: CharmInfo))
            |> Array.ofList
        let cPrevented = Dictionary<string, int>()
        for key in [ "Ignited"; "Shocked"; "Slowed"; "Frozen"; "Stunned" ] do
            cPrevented.[key] <- counts.Id("charm prevented:" + key)

        // ---- what happens on its own ----
        // (the ticks of two periodics of one name would share a stream of random numbers)
        if (player.Periodics |> List.map (fun pd -> pd.Name) |> List.distinct).Length <> player.Periodics.Length then reject "periodic actions of the same name"
        let consumedOf = ResizeArray<string>()
        let gainedOf = ResizeArray<string>()
        let maxOf (r: string) = Map.tryFind r player.MaxResources |> Option.defaultValue 0
        let periodics =
            player.Periodics
            |> List.map (fun pd ->
                // (a tick names its periodic: of two of one name the first answers both)
                let pd = player.Periodics |> List.find (fun o -> o.Name = pd.Name)
                let action =
                    match pd.Action with
                    | GainRandom rs ->
                        gainedOf.AddRange rs
                        PGainRandom(rs |> List.map (fun r -> buff r, maxOf r) |> Array.ofList)
                    | GainAnyRandom rs ->
                        if rs.IsEmpty then reject "a periodic gain of nothing"
                        gainedOf.AddRange rs
                        PGainAnyRandom(rs |> List.map (fun r -> buff r, maxOf r) |> Array.ofList)
                    | GainWeighted ws ->
                        if ws.IsEmpty then reject "a periodic gain of nothing"
                        gainedOf.AddRange(ws |> List.map fst)
                        PGainWeighted(ws |> List.map (fun (r, w) -> buff r, maxOf r, w) |> Array.ofList)
                    | ConsumeEach rs ->
                        consumedOf.AddRange rs
                        PConsumeEach(rs |> List.map buff |> Array.ofList)
                    | GainTimedBuff (b, chance, duration, maxStacks, mods) ->
                        checkMods mods
                        buffMods mods
                        PGainTimedBuff(buff b, chance, duration, maxStacks, mods)
                    | HealLife amount -> PHealLife(amount, counts.Id "healed")
                    | TriggerNamed name -> PTrigger(skillId "a periodic trigger" name)
                    | SummonMinions _ | SummonFromCorpse _ ->
                        reject "periodic summons"
                        PHealLife(0.0, 0)
                ({ Every = pd.Every; Action = action; Key = Sim.streamHash ("P|" + player.Name + "|" + pd.Name) }: PeriodicInfo))
            |> Array.ofList

        // ---- cycles, archons ----
        let cycles =
            player.Cycles
            |> List.map (fun c ->
                if c.Variants.IsEmpty then reject "a cycle without variants"
                for v in c.Variants do
                    checkMods v
                    buffMods v
                ({ Buff = buff c.Name
                   Period = c.Period
                   Variants = Array.ofList c.Variants
                   Picked = c.Variants |> List.mapi (fun i _ -> counts.Id(sprintf "cycle:%s:%d" c.Name i)) |> Array.ofList }: CycleInfo))
            |> Array.ofList
        let archons =
            player.Archons
            |> List.map (fun a ->
                checkMods a.Mods
                buffMods a.Mods
                ({ Buff = buff a.Name
                   Stat = a.OnIgniteStat
                   Duration = a.Duration
                   Recovery = a.Recovery
                   AMods = a.Mods
                   Gained = counts.Id("archon:" + a.Name) }: ArchonInfo))
            |> Array.ofList

        // ---- enemies ----
        for (name, _, _) in fight.EnemyAuras do
            statuses.Id name |> ignore
        let exposureEffect = Mods.inc Ctx.empty [ Named "ExposureEffect" ] sheet
        let enemyInfos =
            enemies
            |> Array.map (fun d ->
                let db = ModDb.ofList d.Mods
                { Def = d
                  Db = db
                  CurseK = curseK d
                  ExposureK = Mods.more Ctx.empty [ Named "ExposureEffectOnSelf" ] db * exposureEffect
                  Evasion = Constants.monsterEvasion d.Level * Mods.mult Ctx.empty [ Evasion ] db
                  BaseArmour = max 0.0 (Mods.total Ctx.empty [ Armour ] db)
                  CullLife = d.Life * Constants.cullingThreshold (Rarity.name d.Rarity) / 100.0
                  Own = [| for m in List.rev d.Mods do
                            match Mod.eval Ctx.empty m with
                            | Some v -> yield m.Stat, m.Kind, v, (m.Source = "Shock")
                            | None -> () |]
                  // (as `Sim.runWith` makes them: of two of one name the later stays)
                  Auras =
                    fight.EnemyAuras
                    |> List.map (fun (name, stacks, mods) ->
                        let k = if name.StartsWith "Curse:" then Mods.more Ctx.empty [ Named "CurseEffectOnSelf" ] db else 1.0
                        statuses.Id name, stacks, mods |> List.map (Mod.scaled k)) })
        let enemyIds = Dictionary<string, int>()
        enemies |> Array.iteri (fun i e -> enemyIds.[e.Name] <- i)

        // ---- skills ----
        let consumed = all |> List.choose (fun s -> s.Consumes) |> List.distinct
        for charge in consumed do
            buff charge |> ignore
            consumedOf.Add charge
        // a skill's answers: its mods and the character's, newest first, by stat and kind
        let queriesOf (s: Skill) : (int * ModKind * Stat list) -> Query =
            let byKey = Dictionary<string, ResizeArray<Mod>>()
            for m in List.rev (player.Mods @ s.Mods) do
                let key = statKey m.Stat m.Kind
                if not (byKey.ContainsKey key) then byKey.[key] <- ResizeArray<Mod>()
                byKey.[key].Add m
            let dotTags = s.Tags |> Set.remove Spell |> Set.remove Attack
            let none : CC[] = [||]
            fun (mode, kind, stats) ->
                let items = ResizeArray<Item>()
                let mutable rest : Stat list = stats
                while not rest.IsEmpty do
                    let key = statKey rest.Head kind
                    rest <- rest.Tail
                    match dynKeys.TryGetValue key with
                    | true, slot -> items.Add { V = 0.0; Conds = none; Stacky = false; Slot = slot }
                    | _ -> ()
                    match byKey.TryGetValue key with
                    | true, mods ->
                        for i in 0 .. mods.Count - 1 do
                            let m = mods.[i]
                            if m.Conds.IsEmpty then items.Add { V = m.Value; Conds = none; Stacky = false; Slot = -1 }
                            elif mode = MEmpty then
                                match Mod.eval Ctx.empty m with
                                | Some v -> items.Add { V = v; Conds = none; Stacky = false; Slot = -1 }
                                | None -> ()
                            else
                                match compileConds buffs.Id statuses.Id (Some((if mode = MDot then dotTags else s.Tags), s.Name)) m.Conds with
                                | Some conds, stacky -> items.Add { V = m.Value; Conds = conds; Stacky = stacky; Slot = -1 }
                                | None, _ -> ()
                    | _ -> ()
                makeQuery kind (items.ToArray())
        let skills =
            all
            |> List.map (fun s ->
                let flag (name: string) = s.Mods |> List.exists (fun m -> m.Stat = Named name)
                let query = queriesOf s
                let own = player.Mods @ s.Mods
                let pattern = Map.tryFind s.Name player.HitPattern |> Option.defaultValue (s.Hits |> List.map (fun (f, m) -> DuringUse f, m)) |> Array.ofList
                // (as `Sim.skillStacks`: a part counts like the skill it belongs to)
                let stackCharge =
                    s.Consumes
                    |> Option.orElse (player.Skills |> List.tryPick (fun o -> if o.Parts |> List.exists (fun p -> p.Name = s.Name) then o.Consumes else None))
                let stackExtra =
                    match stackCharge with
                    | Some charge ->
                        let ctx = { Ctx.empty with SkillName = s.Name; SkillTags = s.Tags }
                        int (Mods.sumBase ctx [ Named("Extra" + charge + "sConsumed") ] (ModDb.ofList (player.Mods @ s.Mods)))
                    | None -> 0
                { Skill = s
                  Db = ModDb.ofList (player.Mods @ s.Mods)
                  Parts = (s :: s.Parts) |> List.map (fun p -> skillIds.[p.Name]) |> Array.ofList
                  Consumes = (match s.Consumes with Some c -> buffs.Id c | None -> -1)
                  StackCharge = stackCharge |> Option.defaultValue ""
                  StackBuff = (match stackCharge with Some c -> buffs.Id c | None -> -1)
                  Q = sites.ToArray() |> Array.map query
                  QArchon = player.Archons |> List.map (fun a -> query (MSkill, Base, [ Named a.OnIgniteStat ])) |> Array.ofList
                  BaseMin = DamageType.all |> List.map (fun t -> match Map.tryFind t s.BaseDamage with Some r -> r.Min | None -> 0.0) |> Array.ofList
                  BaseMax = DamageType.all |> List.map (fun t -> match Map.tryFind t s.BaseDamage with Some r -> r.Max | None -> 0.0) |> Array.ofList
                  HasGain = own |> List.exists (fun m -> match m.Stat with GainAs _ -> true | _ -> false)
                  HasConvert = own |> List.exists (fun m -> match m.Stat with SkillConvert _ | Convert _ -> true | _ -> false)
                  DotTags = s.Tags |> Set.remove Spell |> Set.remove Attack
                  SpellDot = s.Tags.Contains(Tag "SpellDot")
                  IsMelee = s.Tags.Contains Melee
                  PerChargeBuff = (match s.Delivery with Projectiles { PerCharge = Some (charge, _) } -> buffs.Id charge | _ -> -1)
                  StackExtra = stackExtra
                  PatShare = pattern |> Array.map (fun (timing, _) -> match timing with DuringUse _ -> true | After _ -> false)
                  PatAt = pattern |> Array.map (fun (timing, _) -> match timing with DuringUse f -> f | After secs -> secs)
                  PatMult = pattern |> Array.map snd
                  CostCtx = { Ctx.empty with SkillTags = s.Tags; SkillName = s.Name }
                  Source = sources.Id(s.Name + " (hit)")
                  CUses = counts.Id("uses:" + s.Name)
                  CHits = counts.Id("hits:" + s.Name)
                  CCrits = counts.Id("crits:" + s.Name)
                  CMiss = counts.Id("miss:" + s.Name)
                  CTrigger = counts.Id("trigger:" + s.Name)
                  CAftershock = counts.Id("aftershock:" + s.Name)
                  CCannotAfford = counts.Id("cannot afford:" + s.Name)
                  CKept = (match s.Consumes with Some c -> counts.Id("kept:" + c) | None -> -1)
                  CConsumed = (match s.Consumes with Some c -> counts.Id("consumed:" + c) | None -> -1)
                  StrikeKey = Sim.streamHash ("S|" + player.Name + "|" + s.Name)
                  ConsumeKey = Sim.streamHash ("C|" + player.Name + "|" + s.Name)
                  RollKey = Sim.streamHash (player.Name + "|" + s.Name)
                  Sustained = s.Tags.Contains(Tag "Type:Sustained")
                  IsAttack = s.Tags.Contains Attack
                  IsSlam = s.Tags.Contains Slam
                  Passing = (match s.Delivery with Projectiles p -> p.Passing > 0.0 && p.HitInterval <= 0.0 | _ -> false)
                  Dots =
                    s.Dots |> Map.toList |> List.map (fun (t, perSecond) ->
                        let label = sprintf "%s (%s over time)" s.Name ((DamageType.name t).ToLowerInvariant())
                        t, perSecond, label, sources.Id label) |> Array.ofList
                  OncePerUse = flag "OncePerUse"
                  OnlyTriggered = flag "OnlyTriggered"
                  NotEnraged = flag "NotEnraged"
                  NotWithCharge = flag "NotWithCharge"
                  WithCharge = s.Name.EndsWith "-Charged" || flag "OnlyWithCharge"
                  Explosion = s.Name.EndsWith ": Explosion"
                  HasChargedPart = s.Parts |> List.exists (fun p -> p.Name.EndsWith "-Charged")
                  NeedsCharge = flag "NeedsCharge"
                  SpendsEmpowered = s.Tags.Contains Attack && not (s.Tags.Contains Warcry)
                  SpendsRepeats = s.Tags.Contains Attack && s.Tags.Contains(Tag "Type:Barrageable") })
            |> Array.ofList
        let sIso, sClose, sOpen = statuses.Id "Isolated", statuses.Id "Close", statuses.Id "OpenWeakness"
        let sDerived = derivedStatuses |> List.map statuses.Id |> Array.ofList
        let rotation = player.Rotation |> List.map (fun step -> skillId "a step" step.Use, step.When) |> Array.ofList

        // ---- the names are all known now ----
        let fill (ids: int[]) = Array.init buffs.Names.Length (fun i -> if i < ids.Length then ids.[i] else -1)
        let gainedRecently = buffs.Names |> Array.map (fun n -> if n.EndsWith "Charge" || gainedOf.Contains n then buffs.Id("Gained" + n + "Recently") else -1)
        let consumedRecently = buffs.Names |> Array.map (fun n -> if consumedOf.Contains n then buffs.Id("Consumed" + n + "Recently") else -1)
        let buffNames = buffs.Names
        let gainedRecently = fill gainedRecently
        let consumedRecently = fill consumedRecently
        let cKeptOf = buffNames |> Array.map (fun n -> if consumedOf.Contains n then counts.Id("kept:" + n) else -1)
        let cLostOf = buffNames |> Array.map (fun n -> if consumedOf.Contains n then counts.Id("lost:" + n) else -1)
        let count (name: string) = counts.Id name
        let cStopped, cEvaded, cEnemyHit, cDeflected, cBlocked, cEnemyCrit = count "enemy:stopped", count "enemy:evaded", count "enemy:hit", count "enemy:deflected", count "enemy:blocked", count "enemy:crit"
        let cFlaskLife = count "flask:life"
        let cIgnited, cShocked, cChilled, cFrozen, cStunned = count "player:Ignited", count "player:Shocked", count "player:Chilled", count "player:Frozen", count "player:Stunned"
        let cGained = buffNames |> Array.map (fun n -> counts.Id("gained:" + n))
        let cExpired = buffNames |> Array.map (fun n -> if n.EndsWith "Charge" then counts.Id("expired:" + n) else -1)
        let cFlaskMana = counts.Id "flask:mana"
        let cPoolLoss = counts.Id "pool loss on use"
        let cAborted = counts.Id "aborted: too many events"
        let statusNames = statuses.Names
        let fixCurses (r: CRule) =
            let rec fix (re: Re) =
                match re with
                | RApplyCurse (id, duration, mods, _, limit, count) ->
                    let prefix = curses |> Seq.find (fun (_, i) -> i = id) |> fst
                    let others = statuses.Order |> Array.filter (fun i -> i <> id && statusNames.[i].StartsWith prefix)
                    RApplyCurse(id, duration, mods, others, limit, count)
                | RSpendStack (b, next) -> RSpendStack(b, fix next)
                | RIfEnemyHas (st, next) -> RIfEnemyHas(st, fix next)
                | RIfBuff (b, next) -> RIfBuff(b, fix next)
                | other -> other
            { r with Re = fix r.Re }
        if no.Count > 0 then Error(List.ofSeq no)
        else
            let program : Program =
                { Fight = fight
                  Vitals = player.Vitals
                  BuffNames = buffNames
                  VitalsKnown = Dictionary()
                  VitalsOn =
                    (match player.VitalsReads with
                     | Some names -> [| for i in 0 .. buffNames.Length - 1 do if names.Contains buffNames.[i] then yield i |]
                     | None -> Array.init buffNames.Length id)
                  BuffIds = buffs.Ids
                  DynKeys = dynKeys
                  DynCount = dynStats.Count
                  NoDyn = Array.create dynStats.Count [||]
                  NoItems = [||]
                  GainSlots = [| for i in 0 .. dynStats.Count - 1 do match dynStats.[i] with GainAs _ -> yield i | _ -> () |]
                  ConvSlots = [| for i in 0 .. dynStats.Count - 1 do match dynStats.[i] with SkillConvert _ | Convert _ -> yield i | _ -> () |]
                  RageSlot = rageSlot
                  QExtra =
                    (match player.Skills with
                     | first :: _ ->
                        let query = queriesOf first
                        buffNames |> Array.map (fun n -> query (MEmpty, Base, [ Named("ExtraChargeChance:" + n); Named "ExtraChargeChance:Any" ]))
                     | [] -> [||])
                  SIso = sIso
                  SClose = sClose
                  SOpen = sOpen
                  SDerived = sDerived
                  BuffOrder = buffs.Order
                  StatusNames = statusNames
                  StatusOrder = statuses.Order
                  StatusIds = statuses.Ids
                  StatusCurse = statusNames |> Array.map (fun n -> n.StartsWith "Curse:")
                  StatusMark = statusNames |> Array.map (fun n -> n.StartsWith "Mark:")
                  StatusExposure = statusNames |> Array.map (fun n -> n.StartsWith "Exposure:")
                  Skills = skills
                  FirstSkill = (if player.Skills.IsEmpty then -1 else 0)
                  Rotation = rotation
                  OnUse = onUse |> Array.map fixCurses
                  OnHit = onHit |> Array.map fixCurses
                  OnCrit = onCrit |> Array.map fixCurses
                  OnKill = onKill |> Array.map fixCurses
                  OnKillOf = onKillOf |> Array.map fixCurses
                  OnInflict = onInflict |> Array.map (Array.map fixCurses)
                  WhenHit = whenHit |> Array.map fixCurses
                  Metas = metas
                  OnBlock = onBlock |> Array.map fixCurses
                  Charms = charms
                  CPrevented = cPrevented
                  Periodics = periodics
                  RageRegen = Mods.sumBase Ctx.empty [ Named "RageRegen" ] sheet
                  RageLoss = (if Mods.flag Ctx.empty (Named "NoRageLoss") sheet then 0.0 else Constants.rageLossPerSecond * Mods.inc Ctx.empty [ Named "RageDecay" ] sheet)
                  RageDelay = Constants.rageLossDelay + Mods.sumBase Ctx.empty [ Named "RageLossDelay" ] sheet
                  RageWhenHit = Mods.sumBase Ctx.empty [ Named "RageWhenHit" ] sheet
                  RageOnKill = Mods.sumBase Ctx.empty [ Named "RageOnKill" ] sheet
                  BerserkLoss = Mods.sumBase Ctx.empty [ Named "LifeLossPercentPerMinutePerRage" ] sheet
                  MaxRage = Map.tryFind "Rage" player.MaxResources |> Option.defaultValue 0
                  Attackers =
                    (if player.Vitals.IsNone then [||]
                     else fight.Enemies |> List.filter (fun e -> e.Attack.IsSome) |> List.map (fun e -> enemyIds.[e.Name]) |> Array.ofList)
                  AttackKeys = enemies |> Array.map (fun e -> Sim.streamHash ("E|" + e.Name))
                  SheetSkip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] sheet
                  ConsumePowerMana = Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] sheet
                  Enemies = enemyInfos
                  EnemyIds = enemyIds
                  Cycles = cycles
                  Archons = archons
                  Resources = resources
                  ResourceSeconds = buffNames |> Array.map (fun n -> Map.tryFind n player.ResourceDurations |> Option.defaultValue nan)
                  ResourceMax = buffNames |> Array.map (fun n -> Map.tryFind n player.MaxResources |> Option.defaultValue System.Int32.MinValue)
                  Timed = buffs.Order |> Array.filter (fun i -> player.ResourceDurations.ContainsKey buffNames.[i])
                  Empowering = buffs.Order |> Array.filter (fun i -> buffNames.[i].StartsWith "EmpoweredAttacks")
                  Readied = buffs.Order |> Array.filter (fun i -> buffNames.[i].StartsWith "EmpoweredRepeatable:")
                  GainedRecently = gainedRecently
                  ConsumedRecently = consumedRecently
                  CKeptOf = cKeptOf
                  CLostOf = cLostOf
                  IsCharge = buffNames |> Array.map (fun n -> n.EndsWith "Charge")
                  CGained = cGained
                  CExpired = cExpired
                  AilmentSource = ailmentSource
                  AilmentStatus = ailmentStatus
                  AilmentResistant = ailmentResistant
                  CInflict = cInflict
                  ExposureStatus = exposureStatus
                  BRage = bRage
                  BHit = bRecently.[0]
                  BNonCrit = bRecently.[1]
                  BCrit = bRecently.[2]
                  BCrit8 = bRecently.[3]
                  BKilled = bRecently.[4]
                  BTriggered = bTriggered
                  BShocked = bShocked
                  BChilled = bChilled
                  BBeenHit = bBeenHit
                  CStopped = cStopped
                  CEvaded = cEvaded
                  CEnemyHit = cEnemyHit
                  CDeflected = cDeflected
                  CBlocked = cBlocked
                  CEnemyCrit = cEnemyCrit
                  CFlaskLife = cFlaskLife
                  CPlayerIgnited = cIgnited
                  CPlayerShocked = cShocked
                  CPlayerChilled = cChilled
                  CPlayerFrozen = cFrozen
                  CPlayerStunned = cStunned
                  BStates = bStates
                  BCharges = bCharges
                  BConstantMana = bConstantMana
                  BConstantLife = bConstantLife
                  SShocked = ailmentStatus.[ailmentIndex Shock]
                  SChilled = ailmentStatus.[ailmentIndex Chill]
                  SArmourBreak = sArmourBreak
                  SFullyBroken = sFullyBroken
                  CFlaskMana = cFlaskMana
                  CPoolLoss = cPoolLoss
                  CAborted = cAborted
                  HasKillOf = onKillOf.Length > 0
                  CountNames = counts.Names
                  SourceNames = sources.Names }
            Ok program

    /// Everything about a fight that this engine does not play yet (empty: it compiles).
    let unsupported (fight: Fight) : string list =
        match build fight with
        | Ok _ -> []
        | Error reasons -> reasons

    // ---- a run ----

    let private growInts (xs: int[]) : int[] =
        let ys = Array.zeroCreate (xs.Length * 2)
        for i in 0 .. xs.Length - 1 do
            ys.[i] <- xs.[i]
        ys

    let private growFloats (xs: float[]) : float[] =
        let ys = Array.zeroCreate (xs.Length * 2)
        for i in 0 .. xs.Length - 1 do
            ys.[i] <- xs.[i]
        ys

    /// The events that wait, the earliest first (of two at the same time, the one that was
    /// put in first): a binary heap of slots, an event's values in the slot's place of each array.
    type private Queue() =
        let mutable heap : int[] = Array.zeroCreate 64
        let mutable count = 0
        let mutable time : float[] = Array.zeroCreate 64
        let mutable seq : int[] = Array.zeroCreate 64
        let mutable kind : int[] = Array.zeroCreate 64
        let mutable a : int[] = Array.zeroCreate 64
        let mutable b : int[] = Array.zeroCreate 64
        let mutable x : float[] = Array.zeroCreate 64
        let mutable y : float[] = Array.zeroCreate 64
        let mutable free : int[] = Array.init 64 id
        let mutable freeCount = 64
        let before i j = time.[i] < time.[j] || (time.[i] = time.[j] && seq.[i] < seq.[j])
        // the event that was taken last
        member val Time = 0.0 with get, set
        member val Seq = 0 with get, set
        member val Kind = 0 with get, set
        member val A = 0 with get, set
        member val B = 0 with get, set
        member val X = 0.0 with get, set
        member val Y = 0.0 with get, set
        member _.Count = count
        /// When the earliest is due.
        member _.Next = time.[heap.[0]]
        member _.Push(t: float, sq: int, k: int, va: int, vb: int, vx: float, vy: float) =
            if freeCount = 0 then
                let n = time.Length
                time <- growFloats time
                seq <- growInts seq
                kind <- growInts kind
                a <- growInts a
                b <- growInts b
                x <- growFloats x
                y <- growFloats y
                heap <- growInts heap
                free <- growInts free
                for i in 0 .. n - 1 do
                    free.[i] <- n + i
                freeCount <- n
            freeCount <- freeCount - 1
            let slot = free.[freeCount]
            time.[slot] <- t
            seq.[slot] <- sq
            kind.[slot] <- k
            a.[slot] <- va
            b.[slot] <- vb
            x.[slot] <- vx
            y.[slot] <- vy
            let mutable i = count
            count <- count + 1
            heap.[i] <- slot
            let mutable go = true
            while go && i > 0 do
                let parent = (i - 1) / 2
                if before heap.[i] heap.[parent] then
                    let h = heap.[i]
                    heap.[i] <- heap.[parent]
                    heap.[parent] <- h
                    i <- parent
                else go <- false
        member this.Pop() =
            let slot = heap.[0]
            this.Time <- time.[slot]
            this.Seq <- seq.[slot]
            this.Kind <- kind.[slot]
            this.A <- a.[slot]
            this.B <- b.[slot]
            this.X <- x.[slot]
            this.Y <- y.[slot]
            free.[freeCount] <- slot
            freeCount <- freeCount + 1
            count <- count - 1
            if count > 0 then
                heap.[0] <- heap.[count]
                let mutable i = 0
                let mutable go = true
                while go do
                    let l = 2 * i + 1
                    let r = l + 1
                    let mutable least = i
                    if l < count && before heap.[l] heap.[least] then least <- l
                    if r < count && before heap.[r] heap.[least] then least <- r
                    if least <> i then
                        let h = heap.[i]
                        heap.[i] <- heap.[least]
                        heap.[least] <- h
                        i <- least
                    else go <- false

    /// Damage over time on an enemy (`DotInstance`, its label a number).
    type private FDot =
        { Ailing: Ailment option
          Label: int
          Of: DamageType
          Dps: float
          Until: float
          Most: int }

    /// What a hit asks of an enemy as a defender (see `numsFor`): NaN where not asked yet.
    type private DefNums =
        { mutable Version: int
          mutable X: float
          MaxRes: float[]
          Res: float[]
          Taken: float[]
          TakenInc: float[]
          TakenMore: float[]
          Poise: float[]
          mutable Armour: float
          mutable PhysFlat: float
          mutable Threshold: float
          mutable Flam: float }

    type private Foe =
        { Info: EnemyInfo
          Index: int
          /// The enemy as it is now (it walks).
          mutable Def: EnemyDef
          mutable Life: float
          // its debuffs, by status
          Has: bool[]
          Stacks: int[]
          Expires: float[]
          DMods: Mod list[]
          /// Nothing of its debuffs and damage over time runs out before this.
          mutable Soonest: float
          mutable Dots: FDot list
          /// The damage over time that counts (`Sim.activeDots`), and the list it was worked out from.
          mutable Active: FDot list
          mutable ActiveOf: FDot list
          /// What the damage over time that counts deals per second, with where it goes, for
          /// the list and the defender they were worked out for.
          mutable Rates: float[]
          mutable RateLabels: int[]
          mutable RateTypes: int[]
          mutable RateSum: float
          mutable RatesOf: FDot list
          mutable RatesVersion: int
          Buildup: float[]
          mutable StunBuildupAt: float
          mutable Flammability: (float * float) list
          mutable Entropy: float
          mutable EntropyAt: float
          mutable HasEntropy: bool
          /// Its statuses (`Sim.status`) with their numbers, good until a debuff or the damage
          /// over time changes, or its life crosses one of the marks that are statuses.
          mutable Status: Map<string, int>
          /// The statuses by number: whether it has each, and its stacks. Made anew when they
          /// change, never changed in place: a hit keeps the ones it began with.
          mutable SHas: bool[]
          mutable SStacks: int[]
          mutable StatusMapStale: bool
          Nums: DefNums
          /// Counts the changes to what its debuffs' mods are.
          mutable DefVersion: int
          /// Its mods as a defender, newest first as the reference's ModDb has them (its
          /// debuffs', then its own), each with its value; whether a mod is a shock's; for
          /// which count of changes they were listed.
          mutable DStat: Stat[]
          mutable DCode: int[]
          mutable DName: string[]
          mutable DKind: int[]
          mutable DVal: float[]
          mutable DShock: bool[]
          mutable DCount: int
          mutable DListVersion: int
          /// The increased damage taken that shocks give it, and whether a shock's mod is of another kind.
          mutable ShockInc: float
          mutable ShockOdd: bool
          mutable DotTakenVersion: int
          /// Time its statuses have lasted that is not yet in the results, and time its
          /// damage over time has run that is not yet (see `advance`).
          mutable UpPending: float
          mutable UpAny: bool
          mutable RatePending: float
          mutable RateAny: bool
          mutable StatusKeys: int[]
          mutable StatusStale: bool
          /// The damage over time the statuses were worked out with.
          mutable StatusDots: FDot list
          mutable WasCullable: bool
          mutable WasLow: bool
          mutable WasFull: bool
          /// The statuses as a hit sees them (with "Isolated" and "Close"), and what they were made from.
          mutable HitStatus: Map<string, int>
          mutable HitStatusOf: Map<string, int>
          mutable HitIsolated: bool
          mutable HitClose: bool
          mutable HitStatusMade: bool
          /// The enemy as a defender (`Sim.defender`), good until a debuff's mods or stacks change.
          mutable Defender: Defender
          mutable DefenderStale: bool
          /// By damage type: what it takes of damage over time (NaN: not worked out for this defender).
          DotTaken: float[] }

    type private State =
        { P: Program
          Seed: uint32
          Rng: Rng
          Queue: Queue
          Tell: (float -> string -> string -> string -> float -> unit) option
          mutable Time: float
          mutable Seq: int
          Foes: Foe[]
          mutable AliveCount: int
          /// The living enemies' definitions in the order of their names, and the one aimed at
          /// (-1: none), good until one dies.
          mutable Living: EnemyDef list
          mutable Aimed: int
          mutable AliveStale: bool
          Kills: ResizeArray<string * Rarity * float>
          // the player's buffs, by buff
          /// (Whoever asks conditions keeps these two as they are when it begins: a change
          /// while one is kept is made on a copy, see `ownBuffs`.)
          mutable BHas: bool[]
          mutable BStacks: int[]
          mutable BKept: bool
          BExpires: float[]
          BMods: Mod list[]
          BPaused: float[]
          mutable BSoonest: float
          /// The buffs' stacks as a map, good until a buff comes, goes or changes its stacks.
          mutable BuffMap: Map<string, int>
          mutable BuffMapStale: bool
          BChanged: ResizeArray<int>
          /// Counts the changes to what the buffs' mods are: a skill's mods with the buffs
          /// (`Sim.skillDb`) are good while this is what it was when they were made.
          mutable ModsVersion: int
          /// Counts the changes to which buffs there are and their stacks; the buffs as they
          /// were at the count `SnapVersion`.
          mutable BuffsVersion: int
          mutable SnapHas: bool[]
          mutable SnapStacks: int[]
          mutable SnapVersion: int
          /// The buffs' mods per stat (see `dyn`), for the mods as they were at `DynVersion`.
          mutable Dyn: Item[][]
          mutable DynVersion: int
          /// Whether a buff's mod has a condition on the skill; the mods with that decided, per
          /// skill and way of asking (as the skill, with nothing known, as damage over time).
          mutable DynOfSkill: bool
          mutable DynOnStatus: bool
          DynFor: Item[][][]
          DynForVer: int[]
          BComp: (int * Item)[][]
          BCompOf: Mod list[]
          RageMore: float[]
          RageVer: int[]
          Evs: ResizeArray<Ev>
          NoHas: bool[]
          /// Per skill: a hit's answers kept from hit to hit, with what they were worked out
          /// for (see `keepAnswers`).
          KVal: float[][]
          KBuffs: int[][]
          KStatus: int[][]
          KGBuffs: int[]
          KGStatus: int[]
          KSnap: int[]
          KDyn: int[]
          KFoe: int[]
          KHas: bool[][]
          KFlags: int[]
          mutable EvDepth: int
          Dbs: ModDb[]
          DbVersion: int[]
          Cooldowns: float[]
          Recharges: float list[]
          mutable ArchonRecovery: float
          /// By meta gem: when its invocation can be used again.
          InvocationReady: float[]
          mutable Energy: Map<string, float>
          /// The enemy that is the cause of what happens now (the one just hit, the one whose
          /// hit was blocked; -1: none), and the enemy a triggered skill is aimed at while it starts.
          mutable Cause: int
          mutable Aim: int
          mutable LastUsed: int
          CritRoll: float[]
          HasCritRoll: bool[]
          Rolled: int[]
          StrikeStream: int[]
          ConsumeStream: int[]
          // the results
          mutable Evened: float
          Damage: float[]
          DamageSeen: bool[]
          ByType: float[]
          ByTypeSeen: bool[]
          Counts: int[]
          Uptime: float[]
          UptimeSeen: bool[]
          // the player's pools (only with vitals)
          mutable Life: float
          mutable Shield: float
          mutable Mana: float
          mutable LastHit: float
          mutable Starved: float
          mutable FlaskCharges: float
          mutable FlaskUntil: float
          mutable FlaskRate: float
          mutable StrikeLeech: float
          mutable StrikeLifeLeech: float
          mutable Ward: float
          mutable LifeFlaskCharges: float
          mutable LifeFlaskUntil: float
          mutable LifeFlaskRate: float
          mutable Recoups: (float * float * string) list
          /// When the player died (NaN: it lives), what it took, its lowest life.
          mutable Died: float
          mutable Taken: float
          mutable MinLife: float
          mutable LockedUntil: float
          mutable FreezeBuildup: float
          /// Damage over time on the player: (until, per second, type).
          mutable Burning: (float * float * DamageType) list
          /// Charges per charm, by its buff.
          CharmCharges: float[]
          mutable RageAt: float
          mutable RageCarry: float
          /// The player's evasion entropy.
          mutable PEntropy: float
          mutable PEntropyAt: float
          mutable PHasEntropy: bool
          AttackStream: int[]
          PeriodicStream: int[]
          /// The defences for the sets of buffs they were asked for (`Actor.VitalsAt`), by the
          /// buffs written out; and the last set asked for, with its defences.
          VitalsSeen: Dictionary<string, PlayerStats>
          mutable VitalsOf: int
          mutable VitalsLast: PlayerStats option
          /// Mana gained: regen, leech, flask, recoup, hit, kill, consume.
          Gained: float[]
          GainedSeen: bool[] }

    let private gainedNames = [| "regen"; "leech"; "flask"; "recoup"; "hit"; "kill"; "consume" |]

    let private schedule (s: State) (t: float) kind a b x y =
        s.Queue.Push(t, s.Seq, kind, a, b, x, y)
        s.Seq <- s.Seq + 1

    let private expireAt (s: State) (t: float) = schedule s t EExpire 0 0 0.0 0.0

    let inline private bump (s: State) (count: int) = s.Counts.[count] <- s.Counts.[count] + 1

    let private addDamage (s: State) (source: int) (v: float) =
        s.Damage.[source] <- s.Damage.[source] + v
        s.DamageSeen.[source] <- true

    let private addByType (s: State) (t: int) (v: float) =
        s.ByType.[t] <- s.ByType.[t] + v
        s.ByTypeSeen.[t] <- true

    let private addGained (s: State) (slot: int) (v: float) =
        s.Gained.[slot] <- s.Gained.[slot] + v
        s.GainedSeen.[slot] <- true

    // ---- the player's buffs ----

    /// Before a buff comes, goes or changes its stacks: if a hit (or a use) still asks its
    /// conditions of the buffs as they were, they stay as they were for it.
    let private ownBuffs (s: State) =
        if s.BKept then
            if s.EvDepth > 0 then
                s.BHas <- Array.copy s.BHas
                s.BStacks <- Array.copy s.BStacks
            s.BKept <- false

    let private setBuff (s: State) (id: int) (stacks: int) (expires: float) (mods: Mod list) =
        let had = s.BHas.[id]
        if not had || s.BStacks.[id] <> stacks then
            ownBuffs s
            s.BuffMapStale <- true
            s.BChanged.Add id
            s.BuffsVersion <- s.BuffsVersion + 1
        if not (had && s.BStacks.[id] = stacks && obj.ReferenceEquals(s.BMods.[id], mods)) then
            if not mods.IsEmpty || (had && not s.BMods.[id].IsEmpty) || id = s.P.BRage then s.ModsVersion <- s.ModsVersion + 1
        s.BHas.[id] <- true
        s.BStacks.[id] <- stacks
        s.BExpires.[id] <- expires
        s.BMods.[id] <- mods
        if not (expires >= s.BSoonest) then s.BSoonest <- expires

    let private removeBuff (s: State) (id: int) =
        if s.BHas.[id] then
            ownBuffs s
            s.BuffMapStale <- true
            s.BChanged.Add id
            s.BuffsVersion <- s.BuffsVersion + 1
            if not s.BMods.[id].IsEmpty || id = s.P.BRage then s.ModsVersion <- s.ModsVersion + 1
            s.BHas.[id] <- false
            s.BStacks.[id] <- 0
            s.BMods.[id] <- []

    /// A buff's stacks, whether its time is up or not (`Sim.stacksOf`).
    let inline private stacksOf (s: State) (id: int) = if s.BHas.[id] then s.BStacks.[id] else 0

    /// Whether a buff has stacks and time left.
    let inline private held (s: State) (id: int) = s.BHas.[id] && s.BStacks.[id] > 0 && s.BExpires.[id] > s.Time

    let private buffMap (s: State) : Map<string, int> =
        if s.BuffMapStale then
            // (only the buffs that changed since it was last asked for)
            let mutable m = s.BuffMap
            for id in s.BChanged do
                m <- (if s.BHas.[id] then Map.add s.P.BuffNames.[id] s.BStacks.[id] m else Map.remove s.P.BuffNames.[id] m)
            s.BChanged.Clear()
            s.BuffMap <- m
            s.BuffMapStale <- false
        s.BuffMap

    /// Buff stacks as a skill sees them (`Sim.skillStacks`).
    let private skillStacks (s: State) (sid: int) : Map<string, int> =
        let stacks = buffMap s
        let info = s.P.Skills.[sid]
        if info.StackExtra > 0 then
            match Map.tryFind info.StackCharge stacks with
            | Some n when n > 0 -> Map.add info.StackCharge (n + info.StackExtra) stacks
            | _ -> stacks
        else stacks

    /// The player's mods while using a skill (`Sim.skillDb`): made the way the reference
    /// makes them, so that every sum over them adds in the same order.
    let private skillDb (s: State) (sid: int) : ModDb =
        if s.DbVersion.[sid] = s.ModsVersion then s.Dbs.[sid]
        else
            let p = s.P
            let buffMods =
                [ for id in p.BuffOrder do
                    if s.BHas.[id] && not s.BMods.[id].IsEmpty then
                        let k = float s.BStacks.[id]
                        for m in s.BMods.[id] -> Mod.scaled k m ]
            let db = ModDb.addList buffMods p.Skills.[sid].Db
            let db =
                if s.BHas.[p.BRage] && s.BStacks.[p.BRage] > 0 then
                    let effect = Mods.mult Ctx.empty [ Named "RageEffect" ] db
                    let more = floor (float s.BStacks.[p.BRage] * effect) * Constants.moreAttackDamagePerRage
                    ModDb.add db (Mod.more (Damage AllDamage) more |> Mod.whenever (SkillIs Attack) |> Mod.from "Rage")
                else db
            s.Dbs.[sid] <- db
            s.DbVersion.[sid] <- s.ModsVersion
            db

    // ---- enemies ----

    let private setDebuff (e: Foe) (id: int) (stacks: int) (expires: float) (mods: Mod list) =
        let had = e.Has.[id]
        if not had || e.Stacks.[id] <> stacks then e.StatusStale <- true
        if not (had && e.Stacks.[id] = stacks && (obj.ReferenceEquals(e.DMods.[id], mods) || e.DMods.[id] = mods)) then
            if not mods.IsEmpty || (had && not e.DMods.[id].IsEmpty) then
                e.DefenderStale <- true
                e.DefVersion <- e.DefVersion + 1
        e.Has.[id] <- true
        e.Stacks.[id] <- stacks
        e.Expires.[id] <- expires
        e.DMods.[id] <- mods
        if not (expires >= e.Soonest) then e.Soonest <- expires

    let private removeDebuff (e: Foe) (id: int) =
        if e.Has.[id] then
            e.StatusStale <- true
            if not e.DMods.[id].IsEmpty then
                e.DefenderStale <- true
                e.DefVersion <- e.DefVersion + 1
            e.Has.[id] <- false
            e.Stacks.[id] <- 0
            e.DMods.[id] <- []

    /// A debuff comes or stacks, for so long (`Sim.addDebuff`).
    let private addDebuff (s: State) (e: Foe) (id: int) (duration: float) (maxStacks: int) (mods: Mod list) =
        let stacks = if e.Has.[id] && e.Expires.[id] > s.Time then min maxStacks (e.Stacks.[id] + 1) else 1
        setDebuff e id stacks (s.Time + duration) mods
        expireAt s (s.Time + duration)

    /// The damage over time that counts: the strongest of each label, as many as may stack
    /// (`Sim.activeDots`, in its order).
    let private activeDots (e: Foe) : FDot list =
        if obj.ReferenceEquals(e.ActiveOf, e.Dots) then e.Active
        elif e.Dots.IsEmpty then
            e.Active <- []
            e.ActiveOf <- e.Dots
            []
        else
            // the labels in the order they first come, with the most stacks any of a label allows
            let mutable labels : int[] = Array.zeroCreate 8
            let mutable limits : int[] = Array.zeroCreate 8
            let mutable count = 0
            let dots = List.toArray e.Dots
            for d in dots do
                let mutable g = -1
                for i in 0 .. count - 1 do
                    if g < 0 && labels.[i] = d.Label then g <- i
                if g < 0 then
                    if count = labels.Length then
                        labels <- growInts labels
                        limits <- growInts limits
                    labels.[count] <- d.Label
                    limits.[count] <- d.Most
                    count <- count + 1
                elif d.Most > limits.[g] then limits.[g] <- d.Most
            // of each label the strongest so many, strongest first, of equals the one that came first
            let mutable active : FDot list = []
            for g in count - 1 .. -1 .. 0 do
                let limit = limits.[g]
                if limit > 0 then
                    let label = labels.[g]
                    let mutable top : FDot[] = Array.zeroCreate (min limit 16)
                    let mutable n = 0
                    for d in dots do
                        if d.Label = label && (n < limit || d.Dps > top.[n - 1].Dps) then
                            let mutable at = n
                            while at > 0 && top.[at - 1].Dps < d.Dps do
                                at <- at - 1
                            if n < limit then
                                if n = top.Length then
                                    let wider = Array.zeroCreate (top.Length * 2)
                                    for i in 0 .. n - 1 do
                                        wider.[i] <- top.[i]
                                    top <- wider
                                n <- n + 1
                            for i in n - 1 .. -1 .. at + 1 do
                                top.[i] <- top.[i - 1]
                            top.[at] <- d
                    for i in n - 1 .. -1 .. 0 do
                        active <- top.[i] :: active
            e.Active <- active
            e.ActiveOf <- e.Dots
            active

    /// The time an enemy's statuses have lasted goes into the results. (It is kept back while
    /// the statuses stay the same: one addition per status when they change, not one per
    /// event. The reference adds event by event; the sums are the same but for rounding.)
    let private flushUptime (s: State) (e: Foe) =
        if e.UpAny then
            for k in e.StatusKeys do
                s.Uptime.[k] <- s.Uptime.[k] + e.UpPending
                s.UptimeSeen.[k] <- true
            e.UpPending <- 0.0
            e.UpAny <- false

    /// The same for what the damage over time on an enemy has dealt.
    let private flushDots (s: State) (e: Foe) =
        if e.RateAny then
            for i in 0 .. e.Rates.Length - 1 do
                addDamage s e.RateLabels.[i] (e.Rates.[i] * e.RatePending)
            for i in 0 .. e.Rates.Length - 1 do
                addByType s e.RateTypes.[i] (e.Rates.[i] * e.RatePending)
            e.RatePending <- 0.0
            e.RateAny <- false

    /// An enemy's statuses (`Sim.statusOf`), by number.
    let private refreshStatus (s: State) (e: Foe) =
        let def = e.Def
        let cullable = e.Life <= e.Info.CullLife
        let low = e.Life <= 0.35 * def.Life
        let full = e.Life >= def.Life
        if e.StatusStale || not (obj.ReferenceEquals(e.StatusDots, e.Dots)) || cullable <> e.WasCullable || low <> e.WasLow || full <> e.WasFull then
            let p = s.P
            flushUptime s e
            let has = Array.copy e.Has
            let stacks = Array.copy e.Stacks
            let mutable cursed = false
            let mutable marked = false
            let mutable exposed = false
            for id in 0 .. has.Length - 1 do
                if has.[id] then
                    if p.StatusCurse.[id] then cursed <- true
                    if p.StatusMark.[id] then marked <- true
                    if p.StatusExposure.[id] then exposed <- true
            // the ailments that deal damage: as many stacks as count
            let mutable first = true
            for d in List.toArray (activeDots e) do
                match d.Ailing with
                | Some a ->
                    let id = p.AilmentStatus.[ailmentIndex a]
                    if first then
                        for b in Ailment.damaging do
                            if activeDots e |> List.exists (fun x -> x.Ailing = Some b) then
                                has.[p.AilmentStatus.[ailmentIndex b]] <- true
                                stacks.[p.AilmentStatus.[ailmentIndex b]] <- 0
                        first <- false
                    stacks.[id] <- stacks.[id] + 1
                | None -> ()
            let on (name: Ailment) = has.[p.AilmentStatus.[ailmentIndex name]]
            let set (slot: int) =
                has.[p.SDerived.[slot]] <- true
                stacks.[p.SDerived.[slot]] <- 1
            let ailing = allAilments |> List.exists on
            if def.Moving then set 0
            if cullable then set 1
            if ailing then set 2
            if cursed then set 3
            if marked then set 4
            if on Freeze || on HeavyStun || on Electrocute || on Pin then set 5
            (let n = (if on Ignite then 1 else 0) + (if on Chill then 1 else 0) + (if on Freeze then 1 else 0) + (if on Shock then 1 else 0) + (if on Electrocute then 1 else 0)
             if n > 0 then
                set 6
                stacks.[p.SDerived.[6]] <- n)
            if exposed then set 7
            if def.Rarity = Rare || def.Rarity = Unique then set 8
            if low then set 9
            if full then set 10
            e.SHas <- has
            e.SStacks <- stacks
            let mutable count = 0
            for id in 0 .. has.Length - 1 do
                if has.[id] then count <- count + 1
            let keys = Array.zeroCreate count
            let mutable k = 0
            for id in 0 .. has.Length - 1 do
                if has.[id] then
                    keys.[k] <- id
                    k <- k + 1
            e.StatusKeys <- keys
            e.StatusMapStale <- true
            e.StatusStale <- false
            e.StatusDots <- e.Dots
            e.WasCullable <- cullable
            e.WasLow <- low
            e.WasFull <- full

    /// An enemy's statuses as the map the reference has (for what reads them by name).
    let private status (s: State) (e: Foe) : Map<string, int> =
        refreshStatus s e
        if e.StatusMapStale then
            let mutable m = Map.empty
            for id in e.StatusKeys do
                m <- Map.add s.P.StatusNames.[id] e.SStacks.[id] m
            e.Status <- m
            e.StatusMapStale <- false
        e.Status

    /// An enemy as a defender: its own mods and its debuffs' (`Sim.defender`).
    let private defender (s: State) (e: Foe) : Defender =
        if e.DefenderStale then
            let debuffMods =
                [ for id in s.P.StatusOrder do
                    if e.Has.[id] && not e.DMods.[id].IsEmpty then
                        let k = float e.Stacks.[id]
                        for m in e.DMods.[id] -> Mod.scaled k m ]
            e.Defender <- { Level = e.Info.Def.Level; Mods = ModDb.addList debuffMods e.Info.Db }
            e.DefenderStale <- false
        e.Defender

    let private takenAll = DamageTaken AllDamage
    let private takenElemental = DamageTaken Elemental
    let private takenOf = damageTypes |> Array.map (fun t -> DamageTaken(OfType t))
    let private resistOf = damageTypes |> Array.map Resist
    let private maxResistOf = damageTypes |> Array.map MaxResist
    let private takenStats = [| for t in 0 .. 4 -> (if t >= 1 && t <= 3 then [| takenAll; takenOf.[t]; takenElemental |] else [| takenAll; takenOf.[t] |]) |]
    let private thresholdStats = [| AilmentThreshold |]
    let private dotTakenStats = [| DotTaken |]
    let private armourStats = [| Armour |]
    let private poiseStats =
        allAilments |> List.map (fun a ->
            match a with
            | Freeze | Electrocute -> [| PoiseThreshold; AilmentThreshold; Named(Ailment.name a + "Threshold") |]
            | _ -> [| PoiseThreshold; Named(Ailment.name a + "Threshold") |]) |> Array.ofList

    /// The enemy's mods as a defender, in the reference's order (see `Foe.DStat`).
    let private defList (s: State) (e: Foe) =
        if e.DListVersion <> e.DefVersion then
            let order = s.P.StatusOrder
            let own = e.Info.Own
            // how many there are, then each in its place (the arrays are kept and grow when they must)
            let mutable n = own.Length
            for id in 0 .. e.Has.Length - 1 do
                if e.Has.[id] && not e.DMods.[id].IsEmpty then n <- n + List.length e.DMods.[id]
            if e.DStat.Length < n then
                e.DStat <- Array.zeroCreate (n + 8)
                e.DCode <- Array.zeroCreate (n + 8)
                e.DName <- Array.zeroCreate (n + 8)
                e.DKind <- Array.zeroCreate (n + 8)
                e.DVal <- Array.zeroCreate (n + 8)
                e.DShock <- Array.zeroCreate (n + 8)
            let mutable at = 0
            let put (stat: Stat) (kind: ModKind) (v: float) (shock: bool) =
                e.DStat.[at] <- stat
                (let c = StatKey.code stat
                 e.DCode.[at] <- c
                 e.DName.[at] <- (if c < 0 then StatKey.name stat else ""))
                e.DKind.[at] <- ModKind.number kind
                e.DVal.[at] <- v
                e.DShock.[at] <- shock
                at <- at + 1
            for k in order.Length - 1 .. -1 .. 0 do
                let id = order.[k]
                if e.Has.[id] && not e.DMods.[id].IsEmpty then
                    let stacks = float e.Stacks.[id]
                    // (a debuff's mods, the last first)
                    let from = at
                    for m in List.toArray e.DMods.[id] do
                        if m.Conds.IsEmpty then put m.Stat m.Kind (m.Value * stacks) (m.Source = "Shock")
                        else
                            match Mod.eval Ctx.empty (Mod.scaled stacks m) with
                            | Some v -> put m.Stat m.Kind v (m.Source = "Shock")
                            | None -> ()
                    let mutable lo = from
                    let mutable hi = at - 1
                    while lo < hi do
                        let st, kd, v, sh = e.DStat.[lo], e.DKind.[lo], e.DVal.[lo], e.DShock.[lo]
                        let cd, nm = e.DCode.[lo], e.DName.[lo]
                        e.DCode.[lo] <- e.DCode.[hi]
                        e.DName.[lo] <- e.DName.[hi]
                        e.DCode.[hi] <- cd
                        e.DName.[hi] <- nm
                        e.DStat.[lo] <- e.DStat.[hi]
                        e.DKind.[lo] <- e.DKind.[hi]
                        e.DVal.[lo] <- e.DVal.[hi]
                        e.DShock.[lo] <- e.DShock.[hi]
                        e.DStat.[hi] <- st
                        e.DKind.[hi] <- kd
                        e.DVal.[hi] <- v
                        e.DShock.[hi] <- sh
                        lo <- lo + 1
                        hi <- hi - 1
            for (stat, kind, v, sh) in own do
                put stat kind v sh
            e.DCount <- at
            e.ShockInc <- 0.0
            e.ShockOdd <- false
            for i in 0 .. at - 1 do
                if e.DShock.[i] then
                    if e.DKind.[i] = 1 && e.DStat.[i] = DamageTaken AllDamage then e.ShockInc <- e.ShockInc + e.DVal.[i]
                    else e.ShockOdd <- true
            e.DListVersion <- e.DefVersion

    /// The defender's mods of a stat and a kind, added to `acc` in their order.
    let private dsum (e: Foe) (kind: ModKind) (stat: Stat) (acc: float) : float =
        let mutable acc = acc
        let c = StatKey.code stat
        let k = ModKind.number kind
        if c >= 0 then
            for i in 0 .. e.DCount - 1 do
                if e.DCode.[i] = c && e.DKind.[i] = k then acc <- acc + e.DVal.[i]
        else
            let name = StatKey.name stat
            for i in 0 .. e.DCount - 1 do
                if e.DCode.[i] < 0 && e.DKind.[i] = k && e.DName.[i] = name then acc <- acc + e.DVal.[i]
        acc

    let private dmore (e: Foe) (stat: Stat) (acc: float) : float =
        let mutable acc = acc
        let c = StatKey.code stat
        if c >= 0 then
            for i in 0 .. e.DCount - 1 do
                if e.DCode.[i] = c && e.DKind.[i] = 2 then acc <- acc * (1.0 + e.DVal.[i] / 100.0)
        else
            let name = StatKey.name stat
            for i in 0 .. e.DCount - 1 do
                if e.DCode.[i] < 0 && e.DKind.[i] = 2 && e.DName.[i] = name then acc <- acc * (1.0 + e.DVal.[i] / 100.0)
        acc

    /// increased x more of the defender's mods of these stats (`Mods.mult` with `Ctx.empty`).
    let private dmult (e: Foe) (stats: Stat[]) : float =
        let mutable inc = 0.0
        let mutable more = 1.0
        for stat in stats do
            inc <- dsum e Inc stat inc
            more <- dmore e stat more
        max 0.0 ((1.0 + inc / 100.0) * more)

    let private kill (s: State) (e: Foe) (at: float) =
        e.Life <- 0.0
        s.Kills.Add((e.Def.Name, e.Def.Rarity, at))
        s.AliveCount <- s.AliveCount - 1
        s.AliveStale <- true

    let private refreshAlive (s: State) =
        if s.AliveStale then
            let mutable living : EnemyDef list = []
            for i in s.Foes.Length - 1 .. -1 .. 0 do
                if s.Foes.[i].Life > 0.0 then living <- s.Foes.[i].Def :: living
            s.Living <- living
            s.Aimed <-
                match Geometry.aim s.P.Fight.Targeting s.Living with
                | Some d -> s.P.EnemyIds.[d.Name]
                | None -> -1
            s.AliveStale <- false

    let private living (s: State) : EnemyDef list =
        refreshAlive s
        s.Living

    /// The enemy aimed at (`Sim.aimed`): a triggered skill "usually targets the cause of the trigger".
    let private aimed (s: State) : int =
        refreshAlive s
        if s.Aim < 0 then s.Aimed
        elif s.Foes.[s.Aim].Life > 0.0 then s.Aim
        elif s.AliveCount > 0 then
            // (the cause died of the hit: the skill lands where it was, on whoever is nearest to there)
            let at = s.Foes.[s.Aim].Def.Position
            let mutable best = -1
            let mutable least = infinity
            for e in s.Foes do
                if e.Life > 0.0 then
                    let d = Vec.dist e.Def.Position at
                    if best < 0 || d < least then
                        best <- e.Index
                        least <- d
            best
        else s.Aimed

    // ---- time passing ----

    /// The clock goes to t: damage over time, and how long each status lasted (`Sim.advance`).
    let private advance (s: State) (t: float) =
        let dt = t - s.Time
        if dt > 0.0 then
            let n = float s.AliveCount
            for e in s.Foes do
                if e.Life > 0.0 then
                    if e.Dots.IsEmpty then
                        refreshStatus s e
                        e.UpPending <- e.UpPending + dt / n
                        e.UpAny <- true
                    else
                        let active = activeDots e
                        // what each deals per second, worked out once for these and this defender
                        if not (obj.ReferenceEquals(e.RatesOf, active) && e.RatesVersion = e.DefVersion) then
                            flushDots s e
                            defList s e
                            if e.DotTakenVersion <> e.DefVersion then
                                for t in 0 .. 4 do
                                    e.DotTaken.[t] <- nan
                                e.DotTakenVersion <- e.DefVersion
                            let bleedK = if e.Def.Moving then Constants.bleedMovingMultiplier else 1.0
                            let m = List.length active
                            let rates, labels, types = Array.zeroCreate m, Array.zeroCreate m, Array.zeroCreate m
                            let mutable i = 0
                            let mutable sum = 0.0
                            for d in List.toArray active do
                                let k = if d.Ailing = Some Bleed then bleedK else 1.0
                                let t = typeIndex d.Of
                                if System.Double.IsNaN e.DotTaken.[t] then
                                    // (`Ailments.dotTakenMult`: bleeding ignores armour)
                                    let resist =
                                        if t = 0 then 0.0
                                        else
                                            dsum e Base resistOf.[t] 0.0
                                            |> min (min Constants.resistCeiling (Constants.enemyBaseMaxResist + dsum e Base maxResistOf.[t] 0.0))
                                            |> max Constants.resistFloor
                                    e.DotTaken.[t] <- (1.0 - resist / 100.0) * dmult e takenStats.[t] * dmult e dotTakenStats
                                rates.[i] <- d.Dps * k * e.DotTaken.[t]
                                labels.[i] <- d.Label
                                types.[i] <- t
                                sum <- sum + rates.[i]
                                i <- i + 1
                            e.Rates <- rates
                            e.RateLabels <- labels
                            e.RateTypes <- types
                            e.RateSum <- sum
                            e.RatesOf <- active
                            e.RatesVersion <- e.DefVersion
                        let dps = e.RateSum
                        let span = if dps > 0.0 && e.Life <= dps * dt then e.Life / dps else dt
                        let life = e.Life - dps * span
                        let dead = life <= 1e-9
                        refreshStatus s e
                        e.RatePending <- e.RatePending + span
                        e.RateAny <- true
                        e.UpPending <- e.UpPending + span / n
                        e.UpAny <- true
                        if dead then kill s e (s.Time + span) else e.Life <- life
            // monsters walk towards the player until they're in attack range
            if s.P.Attackers.Length > 0 then
                for e in s.Foes do
                    match e.Def.Attack with
                    | Some atk when e.Life > 0.0 ->
                        let st = status s e
                        let stopped = [ "Frozen"; "HeavilyStunned"; "Electrocuted"; "Pinned" ] |> List.exists st.ContainsKey
                        let slow =
                            (if st.ContainsKey "Maimed" then 1.0 - Constants.maimSlow / 100.0 else 1.0)
                            * (if st.ContainsKey "Hindered" then 1.0 - Constants.hinderSlow / 100.0 else 1.0)
                            * (1.0 - (if e.Has.[s.P.SChilled] then e.DMods.[s.P.SChilled] |> List.sumBy (fun m -> m.Value) else 0.0) / 100.0)
                        let d = Vec.length e.Def.Position
                        let gap = d - e.Def.Size - atk.Range
                        if not (stopped || gap <= 0.0) then
                            let step = min gap (e.Def.Speed * slow * dt)
                            e.Def <- { e.Def with Position = Vec.scale ((d - step) / d) e.Def.Position }
                            s.AliveStale <- true
                    | _ -> ()
            s.Time <- t

    let inline private alive (s: State) = System.Double.IsNaN s.Died

    /// Mitigated damage onto the player, by type: energy shield first (chaos damage removes
    /// twice as much of it), a share of the rest from mana, life down to 1, then Runic Ward;
    /// death when nothing is left. Starts recoup, and drinks the life flask when life drops
    /// below half (`Sim.takeDamage`).
    let private takeDamage (s: State) (pools: PlayerStats) (damage: Map<DamageType, float>) =
        if alive s then
            let dmg = damage |> Map.toList |> List.sumBy snd
            if dmg > 0.0 then
                let bypass = pools.BypassES / 100.0
                let es, rest =
                    damage
                    |> Map.fold
                        (fun (es, rest) t d ->
                            let k = if t = Chaos then 2.0 else 1.0
                            let absorbed = min (d * (1.0 - bypass)) (es / k)
                            es - absorbed * k, rest + d - absorbed)
                        (s.Shield, 0.0)
                let fromMana = min s.Mana (rest * pools.ManaBeforeLife / 100.0)
                let rest = rest - fromMana
                let toLife = min (max 0.0 (s.Life - 1.0)) rest
                let rest = rest - toLife
                let toWard = min s.Ward rest
                let rest = rest - toWard
                let life = s.Life - toLife - rest
                let recoups =
                    pools.Recoup
                    |> List.choose (fun (pool, types, pct) ->
                        let rate = match pool with "Life" -> pools.LifeRecoveryRate | "Mana" -> pools.ManaRecoveryRate | _ -> pools.ESRecoveryRate
                        let amount = (types |> List.sumBy (fun t -> Map.tryFind t damage |> Option.defaultValue 0.0)) * pct / 100.0 * rate
                        if amount > 0.0 then Some(s.Time + pools.RecoupSeconds, amount / pools.RecoupSeconds, pool) else None)
                // (the lowest life counts the ward as it was before this damage)
                s.MinLife <- min s.MinLife ((max 0.0 life + s.Ward) / max 1.0 (pools.Life + pools.Ward))
                s.Mana <- s.Mana - fromMana
                s.Shield <- es
                s.Ward <- s.Ward - toWard
                s.Life <- life
                s.Taken <- s.Taken + dmg
                if not recoups.IsEmpty then s.Recoups <- recoups @ s.Recoups
                if life <= 0.0 && rest > 0.0 then s.Died <- s.Time
                match pools.LifeFlask with
                | Some f when alive s && pools.Life > 1.0 && life < 0.5 * pools.Life && s.Time >= s.LifeFlaskUntil && s.LifeFlaskCharges >= f.ChargesPerUse ->
                    s.LifeFlaskCharges <- s.LifeFlaskCharges - f.ChargesPerUse
                    s.LifeFlaskUntil <- s.Time + f.Duration
                    s.LifeFlaskRate <- f.Amount * (1.0 - f.Instant) * pools.LifeRecoveryRate / f.Duration
                    s.Life <- min (max s.Life pools.Life) (s.Life + f.Amount * f.Instant * pools.LifeRecoveryRate)
                    bump s s.P.CFlaskLife
                | _ -> ()

    /// The player's recovery and the damage over time on it during dt, ending now (`Sim.recover`).
    let private recover (s: State) (dt: float) =
        match s.P.Vitals with
        | Some _ when dt > 0.0 && s.P.Charms |> Array.exists (fun c -> c.Charm.ChargesPerSecond > 0.0) ->
            for c in s.P.Charms do
                s.CharmCharges.[c.CBuff] <- min c.Charm.MaxCharges (s.CharmCharges.[c.CBuff] + c.Charm.ChargesPerSecond * c.Charm.ChargeGainMult * dt)
        | _ -> ()
        match s.P.Vitals with
        | Some _ when dt > 0.0 ->
            match s.P.Fight.Player.ManaFlask with
            | Some f -> s.FlaskCharges <- min f.MaxCharges (s.FlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
            | None -> ()
        | _ -> ()
        match s.P.Vitals with
        | Some v when dt > 0.0 && alive s ->
            let t0 = s.Time - dt
            let overlap until = max 0.0 (min s.Time until - t0)
            let stream (pool: string) =
                if s.Recoups.IsEmpty then 0.0
                else
                    // (leech: only the instance with the highest rate; the others add up, in their order)
                    let leech = pool.StartsWith "Leech"
                    let mutable acc = 0.0
                    let mutable any = false
                    let mutable rest = s.Recoups
                    while not rest.IsEmpty do
                        let until, rate, pl = rest.Head
                        if pl = pool then
                            let part = rate * overlap until
                            acc <- (if leech then (if any then max acc part else part) else acc + part)
                            any <- true
                        rest <- rest.Tail
                    acc
            let leech = stream "LeechMana"
            let regen = v.ManaRegen * dt
            let mana = min v.Mana (s.Mana + regen + leech)
            let es =
                (if s.Time - s.LastHit >= v.ESRechargeDelay then s.Shield + v.EnergyShieldRecharge * dt else s.Shield)
                + v.ESRegen * dt + stream "ES" + stream "ShroudES"
                |> min (max s.Shield v.EnergyShield)
            let flask = s.FlaskRate * overlap s.FlaskUntil
            let recouped = stream "Mana"
            let mana = min v.Mana (mana + flask + recouped)
            addGained s 0 (min regen (v.Mana - s.Mana))
            addGained s 1 leech
            addGained s 2 flask
            if recouped > 0.0 then addGained s 3 recouped
            let lifeLeech = stream "LeechLife"
            let lifeRecharge = if v.LifeRecharge > 0.0 && s.Time - s.LastHit >= v.ESRechargeDelay then v.LifeRecharge * dt else 0.0
            let lifeGain = v.LifeRegen * dt + lifeLeech + lifeRecharge + s.LifeFlaskRate * overlap s.LifeFlaskUntil + stream "Life"
            let lifeCharges =
                match v.LifeFlask with
                | Some f -> min f.MaxCharges (s.LifeFlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
                | None -> s.LifeFlaskCharges
            s.Mana <- mana
            s.Shield <- es
            s.Life <- min (max s.Life v.Life) (s.Life + lifeGain)
            s.LifeFlaskCharges <- lifeCharges
            s.Ward <- (if s.Ward >= v.Ward then s.Ward else min v.Ward (s.Ward + v.WardRegen * dt))
            if s.Recoups |> List.exists (fun (until, _, _) -> not (until > s.Time)) then
                s.Recoups <- s.Recoups |> List.filter (fun (until, _, _) -> until > s.Time)
            if not s.Burning.IsEmpty then
                let dots =
                    s.Burning
                    |> List.fold (fun (m: Map<DamageType, float>) (until, dps, t) -> Map.add t ((Map.tryFind t m |> Option.defaultValue 0.0) + dps * overlap until) m) Map.empty
                let burning = dots |> Map.exists (fun _ d -> d > 0.0)
                s.Burning <- s.Burning |> List.filter (fun (until, _, _) -> until > s.Time)
                // damage over time keeps energy shield from recharging
                if burning then
                    s.LastHit <- s.Time
                    takeDamage s v dots
        | _ -> ()

    let private gainMana (s: State) (v: PlayerStats) (slot: int) (amount: float) =
        if amount > 0.0 then
            let m = min v.Mana (s.Mana + amount)
            addGained s slot (m - s.Mana)
            s.Mana <- m

    /// What has run out goes (`Sim.prune`).
    let private prune (s: State) =
        let p = s.P
        for e in s.Foes do
            if not (e.Soonest > s.Time) then
                let mutable soonest = infinity
                for id in 0 .. e.Has.Length - 1 do
                    if e.Has.[id] then
                        if e.Expires.[id] > s.Time then (if e.Expires.[id] < soonest then soonest <- e.Expires.[id])
                        else removeDebuff e id
                if e.Dots |> List.exists (fun d -> not (d.Until > s.Time)) then
                    e.Dots <- e.Dots |> List.filter (fun d -> d.Until > s.Time)
                for d in List.toArray e.Dots do
                    if d.Until < soonest then soonest <- d.Until
                e.Soonest <- soonest
        if not (s.BSoonest > s.Time) then
            let mutable soonest = infinity
            for id in 0 .. s.BHas.Length - 1 do
                if s.BHas.[id] then
                    if s.BExpires.[id] > s.Time then (if s.BExpires.[id] < soonest then soonest <- s.BExpires.[id])
                    else
                        if s.BExpires.[id] < infinity && p.CExpired.[id] >= 0 then bump s p.CExpired.[id]
                        removeBuff s id
            s.BSoonest <- soonest

    // ---- effects ----

    /// A buff comes or stacks, for so long (`Sim.stackEffect` on the actor's buffs).
    let private grantBuff (s: State) (id: int) (duration: float) (maxStacks: int) (mods: Mod list) =
        let stacks = if s.BHas.[id] && s.BExpires.[id] > s.Time then min maxStacks (s.BStacks.[id] + 1) else 1
        setBuff s id stacks (s.Time + duration) mods
        expireAt s (s.Time + duration)

    let private markStatus (s: State) (id: int) =
        setBuff s id 1 (s.Time + 4.0) []
        expireAt s (s.Time + 4.0)

    let private mark (s: State) (id: int) (seconds: float) =
        setBuff s id 1 (s.Time + seconds) []
        expireAt s (s.Time + seconds)

    /// A resource's stacks are set (`Sim.setStacksWith`).
    let private setStacks (s: State) (refresh: bool) (id: int) (n: int) =
        let d = s.P.ResourceSeconds.[id]
        let lasts = not (System.Double.IsNaN d)
        if n <= 0 then removeBuff s id
        else
            let expires =
                if lasts && refresh then s.Time + d
                elif s.BHas.[id] then s.BExpires.[id]
                elif lasts then s.Time + d
                else infinity
            setBuff s id n expires []
        if lasts && refresh && n > 0 then expireAt s (s.Time + d)

    /// The player gains rage, up to the maximum; the loss timer starts again (`Sim.gainRage`).
    let private gainRage (s: State) (amount: float) =
        let p = s.P
        if not (amount <= 0.0 || p.MaxRage <= 0) then
            setStacks s false p.BRage (min p.MaxRage (stacksOf s p.BRage + int amount))
            s.RageAt <- s.Time

    /// Something that uses charms happened to the player. A charm for it that is running, or
    /// has the charges to be used now, answers; whether the occurrence is prevented (`Sim.charm`).
    let private charm (s: State) (key: string) : bool =
        let p = s.P
        match p.Vitals with
        | Some pools when p.Charms.Length > 0 ->
            // (which of them run is seen before any of them is used)
            let running = p.Charms |> Array.map (fun c -> s.BHas.[c.CBuff] && s.BExpires.[c.CBuff] > s.Time)
            let mutable prevented = false
            for i in 0 .. p.Charms.Length - 1 do
                let c = p.Charms.[i]
                if c.Charm.Trigger = key then
                    let charges = s.CharmCharges.[c.CBuff]
                    if running.[i] then prevented <- prevented || c.Charm.Prevents
                    elif charges >= c.Charm.ChargesPerUse && c.Charm.ChargesPerUse > 0.0 then
                        setBuff s c.CBuff 1 (s.Time + c.Charm.Duration) []
                        expireAt s (s.Time + c.Charm.Duration)
                        s.CharmCharges.[c.CBuff] <- charges - c.Charm.ChargesPerUse
                        s.Life <- min (max s.Life pools.Life) (s.Life + (if pools.Life > 1.0 then c.Charm.LifeOnUse * pools.LifeRecoveryRate else 0.0))
                        s.Mana <- min (max s.Mana pools.Mana) (s.Mana + c.Charm.ManaOnUse * pools.ManaRecoveryRate)
                        if c.Charm.StartsRecharge then s.LastHit <- -1e9
                        bump s c.Used
                        prevented <- prevented || c.Charm.Prevents
            prevented
        | _ -> false

    /// Exposure on an enemy: the strongest applies (`Sim.expose`).
    let private expose (s: State) (target: int) (dt: DamageType) (amount: float) (duration: float) =
        let e = s.Foes.[target]
        if e.Life > 0.0 then
            let k = e.Info.ExposureK
            let id = s.P.ExposureStatus.[typeIndex dt]
            let current =
                if e.Has.[id] && e.Expires.[id] > s.Time then e.DMods.[id] |> List.sumBy (fun m -> -m.Value) else 0.0
            if amount * k >= current then addDebuff s e id duration 1 [ Mod.base' (Resist dt) (-amount * k) ]

    /// What a hit inflicts comes onto the enemy (`Sim.inflict`).
    let private inflict (s: State) (e: Foe) (i: Inflict) =
        let p = s.P
        match i with
        | Dot (a, t, dps, duration, maxStacks) ->
            let d = { Ailing = Some a; Label = p.AilmentSource.[ailmentIndex a]; Of = t; Dps = dps; Until = s.Time + duration; Most = maxStacks }
            e.Dots <- d :: e.Dots
            if not (d.Until >= e.Soonest) then e.Soonest <- d.Until
            expireAt s d.Until
        | Status (Shock, magnitude, duration) ->
            let id = p.SShocked
            let current = if e.Has.[id] && e.Expires.[id] > s.Time then e.DMods.[id] |> List.sumBy (fun m -> m.Value) else 0.0
            if magnitude >= current then
                addDebuff s e id duration 1 [ Mod.inc (DamageTaken AllDamage) magnitude |> Mod.from "Shock" ]
        | Status (Chill, magnitude, duration) ->
            addDebuff s e p.SChilled duration 1 [ Mod.base' (Named "ChillMagnitude") magnitude ]
        | Status (a, _, duration) -> addDebuff s e p.AilmentStatus.[ailmentIndex a] duration 1 []
        | AddBuildup (a, pct) ->
            let ai = ailmentIndex a
            let had =
                let had = e.Buildup.[ai]
                let idle = s.Time - e.StunBuildupAt - Constants.stunDecayDelay
                if a = HeavyStun && idle > 0.0 then max 0.0 (had - Constants.stunDecayPerSecond * idle) else had
            if a = HeavyStun then e.StunBuildupAt <- s.Time
            let total = had + pct
            if total >= 100.0 && not e.Has.[p.AilmentStatus.[ai]] then
                e.Buildup.[ai] <- 0.0
                addDebuff s e p.AilmentStatus.[ai] (Constants.buildupDuration a) 1 []
                let more, seconds = Constants.afterBuildup a
                if more > 0.0 then
                    addDebuff s e p.AilmentResistant.[ai] (Constants.buildupDuration a + seconds) 1 [ Mod.more (Named(Ailment.name a + "Threshold")) more ]
                bump s p.CInflict.[ai]
            else e.Buildup.[ai] <- min total 100.0

    // ---- using skills ----

    /// Does an attack with this chance to hit land (`Sim.lands`)?
    let private lands (s: State) (rng: Rng) (e: Foe) (hitChance: float) : bool =
        if hitChance >= 100.0 then true
        else
            let value =
                if e.HasEntropy && s.Time - e.EntropyAt <= 100.0 * Constants.serverTick then e.Entropy
                else floor (rng.Float() * 100.0)
            let value = value + hitChance
            let hit = value >= 100.0
            e.Entropy <- (if hit then value - 100.0 else value)
            e.EntropyAt <- s.Time
            e.HasEntropy <- true
            hit

    // ---- the numbers of a hit ----

    /// A buff's mods as items, each with the place it goes to.
    let private compileBuffMods (p: Program) (mods: Mod list) : (int * Item)[] =
        let id (ids: Dictionary<string, int>) (name: string) = match ids.TryGetValue name with | true, i -> i | _ -> -1
        [| for m in mods do
            let slot =
                match p.DynKeys.TryGetValue(statKey m.Stat m.Kind) with
                | true, slot -> slot
                | _ -> failwithf "Flat: a buff's mod of a stat that was not foreseen (%A)" m.Stat
            match compileConds (id p.BuffIds) (id p.StatusIds) None m.Conds with
            | Some conds, stacky -> yield slot, { V = m.Value; Conds = conds; Stacky = stacky; Slot = -1 }
            | None, _ -> () |]

    /// The mods of the buffs, per stat and kind, newest first (as they lie in `Sim.skillDb`).
    let private dyn (s: State) : Item[][] =
        if s.DynVersion <> s.ModsVersion then
            let p = s.P
            let mutable counts : int[] = [||]
            for id in 0 .. s.BHas.Length - 1 do
                if s.BHas.[id] && not s.BMods.[id].IsEmpty then
                    if not (obj.ReferenceEquals(s.BCompOf.[id], s.BMods.[id])) then
                        s.BComp.[id] <- compileBuffMods p s.BMods.[id]
                        s.BCompOf.[id] <- s.BMods.[id]
                    if counts.Length = 0 then counts <- Array.zeroCreate p.DynCount
                    for (slot, _) in s.BComp.[id] do
                        counts.[slot] <- counts.[slot] + 1
            if counts.Length = 0 then s.Dyn <- p.NoDyn
            else
                let table = counts |> Array.map (fun n -> if n = 0 then p.NoItems else Array.zeroCreate n)
                for slot in 0 .. counts.Length - 1 do
                    counts.[slot] <- 0
                for k in p.BuffOrder.Length - 1 .. -1 .. 0 do
                    let id = p.BuffOrder.[k]
                    if s.BHas.[id] && not s.BMods.[id].IsEmpty then
                        let comp = s.BComp.[id]
                        let stacks = float s.BStacks.[id]
                        for j in comp.Length - 1 .. -1 .. 0 do
                            let slot, it = comp.[j]
                            table.[slot].[counts.[slot]] <- { it with V = it.V * stacks }
                            counts.[slot] <- counts.[slot] + 1
                s.Dyn <- table
            s.DynOfSkill <- s.Dyn |> Array.exists (fun items -> items |> Array.exists (fun it -> it.Conds |> Array.exists ofSkill))
            s.DynOnStatus <- s.Dyn |> Array.exists (fun items -> items |> Array.exists (fun it -> it.Conds |> Array.exists ofEnemy))
            s.DynVersion <- s.ModsVersion
        s.Dyn

    /// The buffs' mods for a skill asked in one of the three ways (see `MSkill`).
    let private dynFor (s: State) (sid: int) (mode: int) : Item[][] =
        let table = dyn s
        if not s.DynOfSkill then table
        else
            let at = sid * 3 + mode
            if s.DynForVer.[at] <> s.DynVersion then
                let skill = s.P.Skills.[sid]
                s.DynFor.[at] <-
                    (if mode = MEmpty then resolveDyn table Set.empty ""
                     elif mode = MDot then resolveDyn table skill.DotTags skill.Skill.Name
                     else resolveDyn table skill.Skill.Tags skill.Skill.Name)
                s.DynForVer.[at] <- s.DynVersion
            s.DynFor.[at]

    let private acquireRaw (s: State) : Ev =
        if s.EvDepth = s.Evs.Count then
            let ev = Ev()
            ev.IsoId <- s.P.SIso
            ev.CloseId <- s.P.SClose
            ev.OpenId <- s.P.SOpen
            ev.RageSlot <- s.P.RageSlot
            s.Evs.Add ev
        let ev = s.Evs.[s.EvDepth]
        s.EvDepth <- s.EvDepth + 1
        ev.Crit <- false
        ev.BOn <- false
        ev.SOn <- false
        ev.Extra <- -1
        ev.SHas <- s.NoHas
        ev.Keeping <- false
        ev.AsAttack <- false
        ev.Iso <- false
        ev.Close <- false
        ev.Open <- false
        ev.RageMore <- nan
        ev

    let private release (s: State) = s.EvDepth <- s.EvDepth - 1

    /// For questions asked with nothing known but the skill's mods (`Ctx.empty`).
    let private acquireEmpty (s: State) (sid: int) : Ev =
        let ev = acquireRaw s
        ev.Q <- s.P.Skills.[sid].Q
        ev.Tags <- Set.empty
        ev.Name <- ""
        ev.Dyn <- dynFor s sid MEmpty
        ev.DynRaw <- s.Dyn
        ev

    /// For questions asked as the skill (its tags and name known; buffs and enemy to be given).
    let private acquire (s: State) (sid: int) : Ev =
        let info = s.P.Skills.[sid]
        let table = dynFor s sid MSkill
        let raw = s.Dyn
        // Rage: 1% more attack damage per Rage, times rage effect, rounded down
        if s.RageVer.[sid] <> s.DynVersion then
            let rage = s.P.BRage
            s.RageMore.[sid] <-
                (if s.BHas.[rage] && s.BStacks.[rage] > 0 then
                    let e = acquireEmpty s sid
                    let effect = multQ e qRageEffect
                    release s
                    floor (float s.BStacks.[rage] * effect) * Constants.moreAttackDamagePerRage
                 else nan)
            s.RageVer.[sid] <- s.DynVersion
        let ev = acquireRaw s
        ev.Q <- info.Q
        ev.Tags <- info.Skill.Tags
        ev.Name <- info.Skill.Name
        ev.Dyn <- table
        ev.DynRaw <- raw
        ev.AsAttack <- info.IsAttack
        ev.RageMore <- s.RageMore.[sid]
        ev

    let private useBuffs (s: State) (ev: Ev) (info: SkillInfo) (extra: bool) =
        ev.BOn <- true
        ev.BHas <- s.BHas
        ev.BStacks <- s.BStacks
        s.BKept <- true
        if extra && info.StackExtra > 0 then
            ev.Extra <- info.StackBuff
            ev.ExtraN <- info.StackExtra

    /// A hit's answers are kept for the skill's next hit: an answer is good while what it
    /// can change with is what it was (the buffs and their mods; the enemy and its statuses).
    /// Asked once the ev has its buffs and its enemy.
    let private keepAnswers (s: State) (ev: Ev) (skill: int) (foe: int) (role: int) =
        // (the answers are kept apart for each way of asking: 0 a hit, 1 how long a use takes, 2 what it costs)
        let sid = skill * 3 + role
        let snap = if ev.BOn then s.BuffsVersion else -2
        if s.KVal.[sid].Length = 0 then
            let n = ev.Q.Length * 2
            s.KVal.[sid] <- Array.zeroCreate n
            s.KBuffs.[sid] <- Array.create n -1
            s.KStatus.[sid] <- Array.create n -1
        if s.KSnap.[sid] <> snap || s.KDyn.[sid] <> s.DynVersion then
            s.KSnap.[sid] <- snap
            s.KDyn.[sid] <- s.DynVersion
            s.KGBuffs.[sid] <- s.KGBuffs.[sid] + 1
        let flags = (if ev.Iso then 1 else 0) + (if ev.Close then 2 else 0) + (if ev.Open then 4 else 0)
        if s.KFoe.[sid] <> foe || s.KFlags.[sid] <> flags || not (obj.ReferenceEquals(s.KHas.[sid], ev.SHas)) then
            s.KFoe.[sid] <- foe
            s.KFlags.[sid] <- flags
            s.KHas.[sid] <- ev.SHas
            s.KGStatus.[sid] <- s.KGStatus.[sid] + 1
        ev.KVal <- s.KVal.[sid]
        ev.KBuffs <- s.KBuffs.[sid]
        ev.KStatus <- s.KStatus.[sid]
        ev.GBuffs <- s.KGBuffs.[sid]
        ev.GStatus <- s.KGStatus.[sid]
        ev.DynOnStatus <- s.DynOnStatus
        ev.Keeping <- true

    let private useStatus (s: State) (ev: Ev) (e: Foe) =
        refreshStatus s e
        ev.SOn <- true
        ev.SHas <- e.SHas
        ev.SStacks <- e.SStacks

    let private ready (s: State) (sid: int) = s.Cooldowns.[sid] <= s.Time

    let rec private react (s: State) (rng: Rng) (rules: CRule[]) (killed: string) (sid: int) (target: int) =
        for r in rules do
            if r.Applies.[sid] && (killed = "" || r.Killed = killed) then
                if rng.Chance r.Chance then perform s rng sid target r.Re

    and private perform (s: State) (rng: Rng) (sid: int) (target: int) (re: Re) =
        let onTarget () = target >= 0 && s.Foes.[target].Life > 0.0
        match re with
        | RSpendStack (b, next) ->
            if held s b then
                if s.BStacks.[b] = 1 then removeBuff s b else setBuff s b (s.BStacks.[b] - 1) s.BExpires.[b] s.BMods.[b]
                perform s rng sid target next
        | RSetStacks (b, n) -> setBuff s b n infinity []
        | RRegenES (rate, duration, count) ->
            if s.P.Vitals.IsSome && alive s then
                s.Recoups <- (s.Time + duration, rate, "ShroudES") :: (s.Recoups |> List.filter (fun (_, _, pool) -> pool <> "ShroudES"))
                bump s count
        | RGainEnergy (meta, amount) -> if meta >= 0 then gainEnergy s rng meta amount
        | REmpower (b, perPower, cap, counts) ->
            // a warcry: one attack per so much monster power among the living, at least one
            let mutable power = 0.0
            for e in s.Foes do
                if e.Life > 0.0 then power <- power + MonsterPower.ofRarity e.Def.Rarity
            let n = max 1 (int (floor (min cap power / max 1.0 perPower)))
            bump s counts.[n]
            setBuff s b n infinity []
        | RNone -> ()
        | RIfBuff (b, next) -> if held s b then perform s rng sid target next
        | RIfEnemyHas (st, next) ->
            if onTarget () && (status s s.Foes.[target]).ContainsKey st then perform s rng sid target next
        | RGrantBuff (b, duration, maxStacks, mods) -> grantBuff s b duration maxStacks mods
        | RRemoveBuff b -> removeBuff s b
        | RApplyDebuff (id, duration, maxStacks, mods, blocked) ->
            if not blocked.[sid] && onTarget () then addDebuff s s.Foes.[target] id duration maxStacks mods
        | RApplyCurse (id, duration, mods, others, limit, count) ->
            if onTarget () then
                let e = s.Foes.[target]
                // one curse (plus "additional curses"), one mark: the oldest give way
                let there = others |> Array.filter (fun o -> e.Has.[o] && e.Expires.[o] > s.Time) |> List.ofArray |> List.sortBy (fun o -> e.Expires.[o])
                for o in there |> List.truncate (max 0 (there.Length + 1 - limit)) do
                    removeDebuff e o
                addDebuff s e id duration 1 mods.[target]
                bump s count
        | RApplyExposure (dt, amount, duration) -> if onTarget () then expose s target dt amount duration
        | RGrowExposure (dt, start, perPulse, cap, every, pulses, duration) ->
            if onTarget () then
                for k in 1 .. pulses do
                    schedule s (s.Time + every * float k) EExpose target (typeIndex dt) (min cap (start + perPulse * float k)) duration
        | RRemoveDebuff id -> if onTarget () then removeDebuff s.Foes.[target] id
        | RTrigger skill -> trigger s rng skill
        | RGainResource (b, n) -> schedule s s.Time EGainStacks b n 0.0 0.0
        | RDropRemnant (b, delay, extra, count) ->
            let n = if rng.Chance extra then 2 else 1
            bump s count
            schedule s (s.Time + delay) EGainStacks b n 0.0 0.0

    /// A meta gem gains energy, and triggers its spells if that allows (`Sim.gainEnergy`,
    /// `Sim.releaseMeta`): "Cast on" at maximum, Invocations whenever enough is banked and the
    /// invocation is off cooldown (as many times as it covers).
    and private gainEnergy (s: State) (rng: Rng) (index: int) (amount: float) =
        let info = s.P.Metas.[index]
        let meta = info.Meta
        let amount =
            if not info.HasEnergyMods then amount
            else
                let more = Mods.sumInc { Ctx.empty with Buffs = buffMap s } [ Named "EnergyGenerated" ] meta.EnergyMods / 100.0
                amount * (meta.EnergyMult + more) / meta.EnergyMult
        let energy = min meta.MaxEnergy ((Map.tryFind meta.Name s.Energy |> Option.defaultValue 0.0) + amount)
        s.Energy <- Map.add meta.Name energy s.Energy
        let triggers =
            match meta.Release with
            | AtMaximum -> if energy >= meta.MaxEnergy && meta.Cost > 0.0 then 1 else 0
            | Invocation _ -> if s.InvocationReady.[index] <= s.Time && meta.Cost > 0.0 then int (energy / meta.Cost) else 0
        if triggers <> 0 then
            let spent = float triggers * meta.Cost
            let refund = if rng.Chance meta.RefundChance then spent / 2.0 else 0.0
            s.Energy <- Map.add meta.Name (energy - spent + refund) s.Energy
            (match meta.Release with
             | Invocation cd -> s.InvocationReady.[index] <- s.Time + cd
             | AtMaximum -> ())
            bump s info.Released
            for _ in 1 .. triggers do
                for skill in info.Triggers do
                    trigger s rng skill

    /// A use of a skill starts now, aimed at the current target (`Sim.startUse`).
    and private startUse (s: State) (rng: Rng) (sid: int) (useTime: float) =
        let target = aimed s
        if target >= 0 then
            let p = s.P
            let info = p.Skills.[sid]
            let skill = info.Skill
            let foe = s.Foes.[target]
            // the use's critical hit roll, for the skill and every part of it
            let r = Rng(Sim.streamSeedOf s.Seed info.RollKey s.Rolled.[sid]).Float()
            s.Rolled.[sid] <- s.Rolled.[sid] + 1
            for part in info.Parts do
                s.CritRoll.[part] <- r
                s.HasCritRoll.[part] <- true
            // (what the use asks of the mods, with the buffs and the enemy as they are now)
            let ev = acquire s sid
            useBuffs s ev info true
            useStatus s ev foe
            let recovery = if skill.Cooldown.IsSome then multQ ev qCooldownRecovery else 1.0
            let doubled = skill.Cooldown.IsSome && sumQ ev qDoubleUses > 0.0
            let lose = if p.Vitals.IsSome then sumQ ev qLosePools / 100.0 else 0.0
            let dodge = if p.Metas.Length > 0 && skill.Tags.Contains(Tag "DodgeRoll") then Constants.dodgeRollDistance + sumQ ev qDodgeDistance else 0.0
            release s
            match skill.Cooldown with
            | Some cd ->
                let uses = skill.StoredUses * (if doubled then 2 else 1)
                let pending = s.Recharges.[sid] |> List.filter (fun t -> t > s.Time)
                let start = if p.Fight.Player.ParallelUses || pending.IsEmpty then s.Time else List.max pending
                let pending = List.sort (pending @ [ start + cd / recovery ])
                s.Recharges.[sid] <- pending
                // blocked once every stored use is recharging; free again when the first is back
                s.Cooldowns.[sid] <- (if pending.Length >= uses then pending.Head else -infinity)
            | None -> ()
            // "Lose X% of maximum Life, Mana and Energy Shield when you use"
            (match p.Vitals with
             | Some v ->
                if lose > 0.0 && useTime > 0.0 then
                    s.Life <- max (min 1.0 s.Life) (s.Life - lose * v.Life)
                    s.Shield <- max 0.0 (s.Shield - lose * v.EnergyShield)
                    s.Mana <- max 0.0 (s.Mana - lose * v.Mana)
                    bump s p.CPoolLoss
             | None -> ())
            bump s info.CUses
            if useTime > 0.0 then s.LastUsed <- sid
            let hits = info.PatAt.Length
            let mutable first = infinity
            let mutable last = s.Time
            for i in 0 .. hits - 1 do
                let at = (if info.PatShare.[i] then s.Time + info.PatAt.[i] * useTime + skill.HitDelay else s.Time + info.PatAt.[i] + skill.HitDelay)
                first <- min first at
                last <- max last at
            // (the buffs are what they were when the use began: nothing above changes them)
            let charged = info.Consumes >= 0 && stacksOf s info.Consumes > 0
            for i in 0 .. hits - 1 do
                let at = (if info.PatShare.[i] then s.Time + info.PatAt.[i] * useTime + skill.HitDelay else s.Time + info.PatAt.[i] + skill.HitDelay)
                let mult = info.PatMult.[i]
                for part in info.Parts do
                    let pi = p.Skills.[part]
                    let happens =
                        (at <= first || not pi.OncePerUse)
                        && (part = sid || not pi.OnlyTriggered)
                        && (if pi.NotEnraged then true
                            elif pi.NotWithCharge then not charged
                            elif pi.WithCharge then charged
                            else not (info.HasChargedPart && charged && pi.Explosion))
                    if happens then
                        if pi.Passing then schedule s at EPassing part target mult (at - s.Time)
                        else schedule s at EStrike part target mult 0.0
            // an attack the player makes while empowered by a warcry uses one empowerment up,
            // and a Repeatable attack one of the repeats readied for it
            if useTime > 0.0 && (info.SpendsEmpowered || info.SpendsRepeats) then
                if info.SpendsEmpowered then
                    for b in p.Empowering do
                        if s.BHas.[b] then schedule s (last + 1e-9) ESpendBuff b 0 0.0 0.0
                if info.SpendsRepeats then
                    for b in p.Readied do
                        if s.BHas.[b] then schedule s (last + 1e-9) ESpendBuff b 0 0.0 0.0
            if info.Consumes >= 0 then
                schedule s (last + 1e-9) EConsume info.Consumes sid 0.0 0.0
            if useTime > 0.0 && p.Metas.Length > 0 then
                // Cast on Dodge: energy per unit travelled by a dodge roll, there when the roll ends
                if skill.Tags.Contains(Tag "DodgeRoll") then
                    let distance = dodge
                    p.Metas |> Array.iteri (fun i meta ->
                        let amount = meta.Meta.Sources |> List.sumBy (function DodgeRolls per -> per * distance | _ -> 0.0)
                        if amount > 0.0 then schedule s (s.Time + useTime) EEnergy i 0 (amount * meta.Meta.EnergyMult) 0.0)
                // energy per 10 ms of base cast time of spells the player casts
                if skill.Tags.Contains Spell then
                    p.Metas |> Array.iteri (fun i meta ->
                        let amount = meta.Meta.Sources |> List.sumBy (function CastsSpells per -> per * skill.BaseTime * 100.0 | _ -> 0.0)
                        if amount > 0.0 then gainEnergy s rng i (amount * meta.Meta.EnergyMult))
            react s rng p.OnUse "" sid target

    and private trigger (s: State) (rng: Rng) (sid: int) =
        if ready s sid then
            bump s s.P.Skills.[sid].CTrigger
            setBuff s s.P.BTriggered 1 (s.Time + 4.0) []
            expireAt s (s.Time + 4.0)
            // (aimed at the cause of the trigger while it starts)
            s.Aim <- s.Cause
            startUse s rng sid 0.0
            s.Aim <- -1

    /// How long a use of the skill takes now (`Sim.useTime`).
    let private useTime (s: State) (sid: int) : float =
        let info = s.P.Skills.[sid]
        let ev = acquire s sid
        useBuffs s ev info false
        let t = aimed s
        if t >= 0 then useStatus s ev s.Foes.[t]
        keepAnswers s ev sid t 1
        let speed = multQ ev qSpeed
        let time = info.Skill.BaseTime / speed * (1.0 + Constants.longerPerRepeat * 0.0) + sumQ ev qFixedUse
        release s
        time

    /// What the player sees when choosing a skill (`Sim.view`).
    let private view (s: State) : View =
        let t = aimed s
        { Time = s.Time
          Buffs = buffMap s
          EnemyStatus = (if t >= 0 then status s s.Foes.[t] else Map.empty)
          EnemyLifeFraction = (if t >= 0 then s.Foes.[t].Life / s.Foes.[t].Def.Life else 0.0)
          EnemyRarity = (if t >= 0 then s.Foes.[t].Def.Rarity else Normal)
          EnemiesAlive = s.AliveCount
          Energy = s.Energy
          LastUsed = (if s.LastUsed >= 0 then s.P.Skills.[s.LastUsed].Skill.Name else "") }

    /// The player is free to act: the first step of the rotation that can be used and paid
    /// for is used (`Sim.onReadyUnlocked`).
    let private act (s: State) (rng: Rng) =
        let p = s.P
        let v = view s
        // what a use of the skill costs now: base x increased cost / (1 + cost efficiency)
        let costAt (sid: int) (seconds: unit -> float) =
            let info = p.Skills.[sid]
            let skill = info.Skill
            // (asked with the skill known and nothing else: no buffs, no enemy)
            let ev = acquire s sid
            keepAnswers s ev sid -1 2
            let extraCost =
                match p.Vitals with
                | Some pools -> pools.Mana * sumQ ev qExtraCost / 100.0
                | None -> 0.0
            let costMult = multQ ev qManaCost
            let efficiency = incQ ev qCostEfficiency
            release s
            let ownCost = skill.Cost
            let ownCost =
                if skill.CostPerSecond <= 0.0 then ownCost
                else (if s.LastUsed = sid then 0.0 else ownCost) + skill.CostPerSecond * seconds ()
            (ownCost + extraCost) * costMult / efficiency
        let costOf (sid: int) = costAt sid (fun () -> max Constants.serverTick (useTime s sid))
        let affordable (sid: int) =
            match p.Vitals with
            | Some pools when pools.CostsLife -> (let c = costOf sid in c <= 0.0 || s.Life - c >= 1.0)
            | Some _ -> s.Mana >= costOf sid
            | None -> true
        let mutable firstUsable = -1
        let mutable choice = -1
        // (every step's condition is asked, as the reference asks them all)
        for (sid, cond) in p.Rotation do
            let info = p.Skills.[sid]
            let usable = cond v && ready s sid && (not info.NeedsCharge || info.Consumes < 0 || held s info.Consumes)
            if usable then
                if firstUsable < 0 then firstUsable <- sid
                if choice < 0 && affordable sid then choice <- sid
        let choice = if choice >= 0 then choice else firstUsable
        if choice >= 0 then
            let sid = choice
            let info = p.Skills.[sid]
            // nothing is faster than a server tick (instant skills)
            let t = max Constants.serverTick (useTime s sid)
            let go () =
                startUse s rng sid t
                schedule s (s.Time + t + 0.0) EReady 0 0 0.0 0.0
            match p.Vitals with
            | None -> go ()
            | Some pools ->
                let cost = costAt sid (fun () -> t)
                // drink the mana flask when mana is getting low and it isn't already running
                (match p.Fight.Player.ManaFlask with
                 | Some f when s.Mana < 2.0 * cost && s.Time >= s.FlaskUntil && s.FlaskCharges >= f.ChargesPerUse ->
                    s.FlaskCharges <- s.FlaskCharges - f.ChargesPerUse
                    s.FlaskUntil <- s.Time + f.Duration
                    s.FlaskRate <- f.Amount * (1.0 - f.Instant) / f.Duration
                    s.Mana <- min (max s.Mana pools.Mana) (s.Mana + f.Amount * f.Instant)
                    bump s p.CFlaskMana
                 | _ -> ())
                if pools.CostsLife then
                    // Blood Magic: the cost is paid with life; a skill that would kill waits
                    if s.Life - cost >= 1.0 || cost <= 0.0 then
                        s.Life <- s.Life - cost
                        go ()
                    else
                        let wait = max Constants.serverTick 0.25
                        s.Starved <- s.Starved + wait
                        schedule s (s.Time + wait) EReady 0 0 0.0 0.0
                elif s.Mana < cost && cost > pools.Mana then
                    // costs more than the whole mana pool: this skill can never be used
                    bump s info.CCannotAfford
                    s.Cooldowns.[sid] <- infinity
                    schedule s (s.Time + Constants.serverTick) EReady 0 0 0.0 0.0
                elif s.Mana < cost then
                    // not enough mana: wait until regeneration and leech have covered it
                    let rate =
                        pools.ManaRegen + (s.Recoups |> List.sumBy (fun (until, rate, pool) -> if pool = "LeechMana" && until > s.Time then rate else 0.0))
                        + (if s.Time < s.FlaskUntil then s.FlaskRate else 0.0)
                    let wait = max Constants.serverTick (if rate > 0.0 then (cost - s.Mana) / rate else 1.0)
                    s.Starved <- s.Starved + wait
                    schedule s (s.Time + wait) EReady 0 0 0.0 0.0
                else
                    s.Mana <- s.Mana - cost
                    go ()
        else
            // Nothing usable: wait for the next cooldown, or a server tick.
            let mutable next = infinity
            for t in s.Cooldowns do
                if t > s.Time && t < infinity && t < next then next <- t
            if s.ArchonRecovery > s.Time && s.ArchonRecovery < infinity && s.ArchonRecovery < next then next <- s.ArchonRecovery
            for t in s.InvocationReady do
                if t > s.Time && t < infinity && t < next then next <- t
            schedule s (if next < infinity then next else s.Time + Constants.serverTick) EReady 0 0 0.0 0.0

    /// The player's turn comes (`Sim.onReady`): a stunned or frozen player waits.
    let private onReady (s: State) (rng: Rng) =
        if s.P.Vitals.IsSome && s.LockedUntil > s.Time then schedule s s.LockedUntil EReady 0 0 0.0 0.0
        else act s rng

    /// What a hit asks of the enemy as a defender is worked out when first asked and kept
    /// while its debuffs' mods are the same. `x`: the attacker's "enemies take increased
    /// damage", which the reference adds to the defender's mods (as the newest of them).
    let private numsFor (s: State) (e: Foe) (x: float) : Foe =
        defList s e
        let n = e.Nums
        if n.Version <> e.DefVersion || not (n.X = x) then
            n.Version <- e.DefVersion
            n.X <- x
            for i in 0 .. 4 do
                n.MaxRes.[i] <- nan
                n.Res.[i] <- nan
                n.Taken.[i] <- nan
                n.TakenInc.[i] <- nan
                n.TakenMore.[i] <- nan
            for i in 0 .. 12 do
                n.Poise.[i] <- nan
            n.Armour <- nan
            n.PhysFlat <- nan
            n.Threshold <- nan
            n.Flam <- nan
        e

    let private nMaxRes (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN n.MaxRes.[t] then
            n.MaxRes.[t] <- min Constants.resistCeiling (Constants.enemyBaseMaxResist + dsum e Base maxResistOf.[t] 0.0)
        n.MaxRes.[t]
    let private nRes (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN n.Res.[t] then n.Res.[t] <- dsum e Base resistOf.[t] 0.0
        n.Res.[t]
    /// `Hit.damageTakenMult`, with the attacker's increase first.
    let private nTaken (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN n.Taken.[t] then
            let mutable inc = if n.X <> 0.0 then 0.0 + n.X else 0.0
            let mutable more = 1.0
            inc <- dsum e Inc takenAll inc
            more <- dmore e takenAll more
            inc <- dsum e Inc takenOf.[t] inc
            more <- dmore e takenOf.[t] more
            if t >= 1 && t <= 3 then
                inc <- dsum e Inc takenElemental inc
                more <- dmore e takenElemental more
            n.TakenInc.[t] <- inc
            n.TakenMore.[t] <- more
            n.Taken.[t] <- max 0.0 ((1.0 + inc / 100.0) * more)
        n.Taken.[t]
    let private nThreshold (e: Foe) =
        let n = e.Nums
        if System.Double.IsNaN n.Threshold then n.Threshold <- Constants.ailmentThreshold e.Info.Def.Level * dmult e thresholdStats
        n.Threshold
    let private nPoise (e: Foe) (a: Ailment) =
        let n = e.Nums
        let i = ailmentIndex a
        if System.Double.IsNaN n.Poise.[i] then n.Poise.[i] <- Constants.poiseThreshold e.Info.Def.Level * dmult e poiseStats.[i]
        n.Poise.[i]

    /// Armour's part of `Hit.physicalReduction`.
    let private physicalReduction (e: Foe) (damage: float) =
        let n = e.Nums
        if System.Double.IsNaN n.Armour then
            n.Armour <- max 0.0 (dsum e Base Armour 0.0 * dmult e armourStats)
            n.PhysFlat <- dsum e Base PhysicalReduction 0.0
        let armour = n.Armour
        let fromArmour = if armour <= 0.0 && damage <= 0.0 then 0.0 else armour / (armour + Constants.armourRatio * damage) * 100.0
        min Constants.enemyMaxPhysReduction (fromArmour + n.PhysFlat)

    /// `Hit.effectiveResist`.
    let private effectiveResist (ev: Ev) (n: Foe) (t: int) =
        let maxRes = nMaxRes n t
        let res = nRes n t + sumQ ev qEnemyResist.[t] |> min maxRes |> max Constants.resistFloor
        let pen = sumQ ev qPen.[t]
        let floor' =
            if t >= 1 && t <= 3 then
                let f = qOver ev ev.Q.[qPenFloorOver]
                if ev.Found then f else min 0.0 (sumQ ev qPenFloor)
            else 0.0
        if res > floor' then max floor' (res - pen) else res

    /// The share (%) of a hit's damage of a type that the enemy does not take (`Hit.mitigateWith`).
    let private reduction (ev: Ev) (n: Foe) (negate: bool) (t: int) (v: float) =
        if t = 0 then physicalReduction n v
        else
            let r = effectiveResist ev n t
            let elemental = t >= 1 && t <= 3
            let ignored = r > 0.0 && ((elemental && flagQ ev qIgnoreElemental) || flagQ ev qIgnoreResist.[t])
            if ignored then 0.0
            elif negate && elemental && r > 0.0 then -r
            else r

    /// `Hit.mitigateWith`: what the enemy takes of `un`, into `de`.
    let private mitigate (ev: Ev) (n: Foe) (negate: bool) (has: bool[]) (un: float[]) (de: float[]) =
        for t in 0 .. 4 do
            if has.[t] then
                let v = un.[t]
                de.[t] <- v * (1.0 - reduction ev n negate t v / 100.0) * nTaken n t
            else de.[t] <- 0.0

    let private anyDyn (ev: Ev) (slots: int[]) =
        let mutable any = false
        for slot in slots do
            if ev.Dyn.[slot].Length > 0 then any <- true
        any

    /// `Hit.ranges`: the damage ranges ready to roll, into the ev's `RMin`, `RMax`, `RHas`.
    let private ranges (p: Program) (ev: Ev) (info: SkillInfo) =
        let skill = info.Skill
        let rMin, rMax, rHas = ev.RMin, ev.RMax, ev.RHas
        let oMin, oMax, oHas = ev.OMin, ev.OMax, ev.OHas
        let addedEffect = skill.AddedDamageEffectiveness * multQ ev qAddedEffect
        for t in 0 .. 4 do
            rMin.[t] <- (info.BaseMin.[t] + sumQ ev qAddedMin.[t] * addedEffect) * skill.BaseMultiplier
            rMax.[t] <- (info.BaseMax.[t] + sumQ ev qAddedMax.[t] * addedEffect) * skill.BaseMultiplier
            rHas.[t] <- rMax.[t] > 0.0
        // "gain as extra" is taken from the base damage first
        if info.HasGain || anyDyn ev p.GainSlots then
            for t in 0 .. 4 do
                oMin.[t] <- rMin.[t]
                oMax.[t] <- rMax.[t]
                oHas.[t] <- rHas.[t]
            for from in 0 .. 4 do
                if oHas.[from] then
                    for into in 0 .. 4 do
                        let pct = sumQ ev qGain.[from * 5 + into]
                        if pct > 0.0 then
                            let k = pct / 100.0
                            rMin.[into] <- (if rHas.[into] then rMin.[into] else 0.0) + oMin.[from] * k
                            rMax.[into] <- (if rHas.[into] then rMax.[into] else 0.0) + oMax.[from] * k
                            rHas.[into] <- true
        // conversion: the skill's own first, then everything else on the result
        if info.HasConvert || anyDyn ev p.ConvSlots then
            for step in 0 .. 1 do
                let q = if step = 0 then qSkillConvert else qConvert
                for t in 0 .. 4 do
                    oMin.[t] <- rMin.[t]
                    oMax.[t] <- rMax.[t]
                    oHas.[t] <- rHas.[t]
                    rHas.[t] <- false
                for from in 0 .. 4 do
                    if oHas.[from] then
                        let mutable total = 0.0
                        for into in 0 .. 4 do
                            if into <> from then total <- total + max 0.0 (sumQ ev q.[from * 5 + into]) / 100.0
                        let share (into: int) =
                            let v = max 0.0 (sumQ ev q.[from * 5 + into]) / 100.0
                            if total > 1.0 then v / total else v
                        let mutable gone = 0.0
                        for into in 0 .. 4 do
                            if into <> from then
                                let k = share into
                                if k > 0.0 then gone <- gone + k
                        let put (into: int) (k: float) =
                            if k > 0.0 then
                                rMin.[into] <- (if rHas.[into] then rMin.[into] else 0.0) + oMin.[from] * k
                                rMax.[into] <- (if rHas.[into] then rMax.[into] else 0.0) + oMax.[from] * k
                                rHas.[into] <- true
                        put from (1.0 - gone)
                        for into in 0 .. 4 do
                            if into <> from then put into (share into)
            for t in 0 .. 4 do
                if rHas.[t] && not (rMax.[t] > 0.0) then rHas.[t] <- false
        // Avatar of Fire: "Deal no Non-Fire Damage"
        if flagQ ev qNoNonFire then
            for t in 0 .. 4 do
                if t <> 3 then rHas.[t] <- false

    /// `Hit.critMultiplier`.
    let private critMultiplier (ev: Ev) =
        let bonus = Constants.playerBaseCritBonus + sumQ ev qCritBonus
        1.0 + max 0.0 (bonus * multQ ev qCritBonusMult) / 100.0

    /// `Hit.critChance` (asked before it is known whether the hit is a critical one).
    let private critChance (ev: Ev) (info: SkillInfo) =
        ev.Crit <- false
        let v = qOver ev ev.Q.[qCritOver]
        if ev.Found then v
        else
            let baseCrit = info.Skill.BaseCrit + sumQ ev qCritBase
            let chance = min 100.0 (max 0.0 (baseCrit * multQ ev qCritMult))
            let cap = sumQ ev qMaxCrit
            let chance = if cap > 0.0 then min cap chance else chance
            if flagQ ev qLuckyCrit then 100.0 - (100.0 - chance) * (100.0 - chance) / 100.0 else chance

    /// `Hit.rollUnmitigated`: one hit's damage before mitigation, into `un` (`has`: the types it has).
    let private rollUnmitigated (p: Program) (ev: Ev) (info: SkillInfo) (rng: Rng) (crit: bool) (has: bool[]) (un: float[]) =
        ev.Crit <- crit
        let lucky = flagQ ev qLucky || rng.Chance(sumQ ev qLuckyChance)
        let critMult = if crit then critMultiplier ev else 1.0
        let doubled = rng.Chance(sumQ ev qDouble)
        ranges p ev info
        for t in 0 .. 4 do
            has.[t] <- ev.RHas.[t]
            if has.[t] then
                let lo, hi = ev.RMin.[t], ev.RMax.[t]
                let rolled =
                    if lucky then
                        let a = rng.Between(lo, hi)
                        let b = rng.Between(lo, hi)
                        max a b
                    else rng.Between(lo, hi)
                un.[t] <- rolled * multQ ev qDamage.[t] * critMult * (if doubled then 2.0 else 1.0)
            else un.[t] <- 0.0

    /// `Hit.rollWith`: crit roll, damage roll, mitigation; whether it is a critical hit.
    /// `critRoll`: the use's roll (NaN: rolled for this hit alone).
    let private rollHit (p: Program) (ev: Ev) (info: SkillInfo) (rng: Rng) (critRoll: float) (n: Foe) (has: bool[]) (un: float[]) (de: float[]) : bool =
        let given = not (System.Double.IsNaN critRoll)
        let chance = if given && critRoll >= 1.0 then 0.0 else critChance ev info
        ev.Crit <- false
        let first = if given then chance >= 100.0 || (chance > 0.0 && critRoll * 100.0 < chance) else rng.Chance chance
        let rerolls =
            if first || chance <= 0.0 || not (flagQ ev qInevitable) then 0
            else
                let rec go k = if k >= 4 || rng.Chance chance then k else go (k + 1)
                go 1
        let second = rerolls = 0 && chance > 0.0 && flagQ ev qBifurcate && rng.Chance chance
        let crit = first || second || rerolls > 0
        rollUnmitigated p ev info rng crit has un
        if first && second then
            let full = critMultiplier ev
            for t in 0 .. 4 do
                if has.[t] then un.[t] <- un.[t] * (1.0 + 2.0 * (full - 1.0)) / full
        elif rerolls <> 0 then
            let full = critMultiplier ev
            let kept = max 0.0 (1.0 - Constants.inevitableCritLessPerReroll * float rerolls)
            for t in 0 .. 4 do
                if has.[t] then un.[t] <- un.[t] * (1.0 + (full - 1.0) * kept) / full
        mitigate ev n (rng.Chance(sumQ ev qNegate)) has un de
        crit

    let private flammabilityStat = Named "Flammability"

    let private total5 (xs: float[]) = 0.0 + xs.[0] + xs.[1] + xs.[2] + xs.[3] + xs.[4]

    let private allTypes = [| 0; 1; 2; 3; 4 |]
    let private damagingAilments = Array.ofList Ailment.damaging
    let private flatAilments = [| Daze; Maim; Blind; Hinder |]
    let private buildupAilments = Array.ofList Ailment.buildup
    let private scalesOf =
        allAilments |> List.map (fun a -> Ailments.scalesFrom a |> List.map typeIndex |> Array.ofList) |> Array.ofList

    /// The damage an ailment scales from (`Ailments.scalesFromWith`, summed in its order).
    let private source (ev: Ev) (a: Ailment) (un: float[]) =
        let types =
            match a with
            | Ignite when flagQ ev qAllIgnites -> allTypes
            | Chill when flagQ ev qAllChills -> allTypes
            | Shock when flagQ ev qAllShocks -> allTypes
            | _ -> scalesOf.[ailmentIndex a]
        let mutable sum = 0.0
        for t in types do
            sum <- sum + un.[t]
        sum

    let private ailmentDuration (ev: Ev) (a: Ailment) =
        let baseD =
            match a with
            | Bleed -> (let v = qOver ev ev.Q.[qBleedDuration] in if ev.Found then v else Ailments.baseDuration a)
            | Poison -> (let v = qOver ev ev.Q.[qPoisonDuration] in if ev.Found then v else Ailments.baseDuration a)
            | _ -> Ailments.baseDuration a
        baseD * multQ ev qDuration.[ailmentIndex a]

    /// `Ailments.fromHit`: everything the hit inflicts. `flammable`: the Flammability earlier
    /// hits left on the enemy.
    let private fromHit (p: Program) (ev: Ev) (info: SkillInfo) (rng: Rng) (n: Foe) (flammable: float) (crit: bool) (un: float[]) : ResizeArray<Inflict> =
        ev.Crit <- crit
        let threshold = nThreshold n
        let chance (a: Ailment) =
            let i = ailmentIndex a
            let baseChance, mult = sumQ ev qAilChance.[i], multQ ev qAilChanceMult.[i]
            let c =
                match a with
                | Ignite ->
                    // (the reference adds what earlier hits left first and the enemy's own after:
                    // the same number unless the enemy has several of its own, then the same but for rounding)
                    let left =
                        let own = n.Nums
                        if System.Double.IsNaN own.Flam then own.Flam <- dsum n Base flammabilityStat 0.0
                        if flammable > 0.0 then flammable + own.Flam else own.Flam
                    (source ev a un / threshold * Constants.igniteChanceMultiplier + baseChance) * mult + left
                | Shock -> (source ev a un / threshold * Constants.shockChanceMultiplier + baseChance) * mult
                | _ -> baseChance * mult
            min 100.0 (max 0.0 c)
        let rolled a = source ev a un > 0.0 && rng.Chance(chance a)
        let magnitude (a: Ailment) = multQ ev qMagnitude.[ailmentIndex a]
        // (written out as loops: a list made from a sequence is slow in JavaScript)
        let out = ev.Inflicted
        out.Clear()
        for a in damagingAilments do
            if rolled a then
                let faster = incQ ev (if a = Ignite then qFasterIgnite else qFaster)
                let dps = source ev a un * Ailments.percentPerSecond a / 100.0 * magnitude a * faster
                out.Add(Dot(a, Ailments.dealsType a, dps, ailmentDuration ev a / max 0.1 faster, 1 + int (sumQ ev qAilStacks.[ailmentIndex a])))
        if rolled Shock then
            out.Add(Status(Shock, Constants.baseShockMagnitude * magnitude Shock, ailmentDuration ev Shock))
        (let cold = source ev Chill un
         let effect = Constants.chillEffectMultiplier * cold / threshold * magnitude Chill
         if effect >= 1.0 then
            out.Add(Status(Chill, min Constants.chillMaxEffect (max Constants.chillMinEffect effect), ailmentDuration ev Chill)))
        for a in flatAilments do
            if rng.Chance(chance a) then out.Add(Status(a, 0.0, ailmentDuration ev a))
        for a in buildupAilments do
            let enabled =
                match a with
                | Freeze | HeavyStun -> true
                | Electrocute -> flagQ ev qCanElectrocute
                | Pin -> flagQ ev qCanPin
                | _ -> false
            let dmg =
                match a with
                | HeavyStun ->
                    (source ev a un + un.[0] * Constants.playerPhysicalStunMore / 100.0)
                    * (if info.IsMelee then 1.0 + Constants.playerMeleeStunMore / 100.0 else 1.0)
                    * (if hasStatus ev p.AilmentStatus.[ailmentIndex Daze] then 1.5 else 1.0)
                | _ -> source ev a un
            if enabled && dmg > 0.0 then
                out.Add(AddBuildup(a, dmg * Constants.buildupScale a / nPoise n a * multQ ev qBuildup.[ailmentIndex a] * 100.0))
        out

    /// The statuses in two arrays as the map the reference has.
    let private statusMap (p: Program) (has: bool[]) (stacks: int[]) : Map<string, int> =
        let mutable m = Map.empty
        for i in 0 .. has.Length - 1 do
            if has.[i] then m <- Map.add p.StatusNames.[i] stacks.[i] m
        m

    /// One hit on one enemy (`Sim.hitEnemy`, in its order).
    let private hitEnemy (s: State) (rng: Rng) (sid: int) (mult: float) (target: int) =
        let e = s.Foes.[target]
        if e.Life > 0.0 then
            let p = s.P
            let info = p.Skills.[sid]
            let skill = info.Skill
            let def0 = e.Def
            // the enemy as it is before the hit: its life, its statuses
            let lifeBefore = e.Life
            let ev = acquire s sid
            useBuffs s ev info true
            useStatus s ev e
            let beforeHas, beforeStacks = e.SHas, e.SStacks
            // Isolated: none of its allies within 6 m; Close Range: within 2 m of the player (keyword text)
            let mutable alone = true
            for o in s.Foes do
                if o.Life > 0.0 && o.Index <> e.Index && not (Vec.dist o.Def.Position def0.Position > 60.0) then alone <- false
            ev.Iso <- alone
            ev.Close <- Vec.length def0.Position - def0.Size <= 20.0
            // Revealed Weaknesses: an Open Weakness for the share of the time the enemy's life is in one
            (let evE = acquireEmpty s sid
             let more = sumQ evE qOpenMore
             if def0.Rarity <> Normal && def0.Rarity <> Magic && flagQ evE qReveal then
                let c, m = Constants.revealedWeaknessLifeShare, 1.0 + more / 100.0
                if rng.Float() < (c / m) / (1.0 - c + c / m) then ev.Open <- true
             release s)
            keepAnswers s ev sid target 0
            let hitChance =
                let v = qOver ev ev.Q.[qHitChance]
                if ev.Found then v
                elif info.IsAttack then
                    let accuracy = sumQ ev qAccuracy * multQ ev qAccuracyMult
                    if accuracy <= 0.0 then 100.0
                    else
                        let evasion =
                            e.Info.Evasion
                            * (if hasStatus ev p.AilmentStatus.[ailmentIndex Blind] then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                            * (if hasStatus ev p.AilmentStatus.[ailmentIndex Maim] then 1.0 - Constants.maimEvasionReduced / 100.0 else 1.0)
                        let distance = max 0.0 (Vec.length def0.Position - def0.Size)
                        Hit.accuracyHitChance evasion (Hit.accuracyAtDistance distance accuracy)
                else 100.0
            if not (lands s rng e hitChance) then bump s info.CMiss
            else
                // "Enemies you Curse / Mark take X% increased Damage": additive with what the enemy already takes
                let nums = numsFor s e (sumQ ev qEnemyTaken)
                let critRoll = if info.Sustained || not s.HasCritRoll.[sid] then nan else s.CritRoll.[sid]
                let critRoll = if hitChance < 100.0 && rng.Chance(100.0 - hitChance) then 1.0 else critRoll
                let has, un, de = ev.Has, ev.Un, ev.De
                let crit = rollHit p ev info rng critRoll nums has un de
                // the same hit the other way, rolled apart from the fight's stream (see `Outcome.Evened`)
                let evenly =
                    let chance = if critRoll >= 1.0 then 0.0 else min 100.0 (max 0.0 (critChance ev info))
                    let was = total5 de
                    ev.Crit <- false
                    if chance <= 0.0 || chance >= 100.0 || was <= 0.0 || flagQ ev qInevitable || flagQ ev qBifurcate then 1.0
                    else
                        let scratch = Rng(uint32 (s.Seq % 1000000) + 7919u)
                        rollUnmitigated p ev info scratch (not crit) ev.Has2 ev.Un2
                        mitigate ev nums (scratch.Chance(sumQ ev qNegate)) ev.Has2 ev.Un2 ev.De2
                        let other = total5 ev.De2
                        let pc = chance / 100.0
                        (if crit then pc * was + (1.0 - pc) * other else pc * other + (1.0 - pc) * was) / was
                // Culling Strike: an enemy at or below the threshold for its rarity dies, before the hit's damage
                ev.Crit <- crit
                if flagQ ev qCulling
                   && lifeBefore <= def0.Life * Constants.cullingThreshold (Rarity.name def0.Rarity) / 100.0 * incQ ev qCullThreshold then
                    for t in 0 .. 4 do
                        if has.[t] then de.[t] <- de.[t] + lifeBefore
                ev.Crit <- false
                let mult =
                    let near, far = sumQ ev qCloseMore, sumQ ev qFarMore
                    if near = 0.0 && far = 0.0 then mult
                    else
                        let d = max 0.0 (Vec.length def0.Position - def0.Size)
                        let closeness = max 0.0 (min 1.0 ((35.0 - d) / 25.0))
                        let farness = max 0.0 (min 1.0 ((d - 35.0) / 35.0))
                        mult * (1.0 + near * closeness / 100.0) * (1.0 + far * farness / 100.0)
                for t in 0 .. 4 do
                    if has.[t] then
                        un.[t] <- un.[t] * mult
                        de.[t] <- de.[t] * mult
                // ailments are rolled before the damage lands
                let flammable = e.Flammability |> List.filter (fun (_, until) -> until > s.Time)
                let inflicted = fromHit p ev info rng nums (flammable |> List.sumBy fst) crit un
                // the Flammability this hit leaves (`Ailments.flammability`)
                (ev.Crit <- crit
                 let fire = source ev Ignite un
                 if fire > 0.0 then
                    let f = min 100.0 (max 0.0 ((fire / nThreshold nums * Constants.igniteChanceMultiplier + sumQ ev qAilChance.[0]) * multQ ev qAilChanceMult.[0]))
                    if f > 0.0 then e.Flammability <- (f, s.Time + Constants.flammabilityDuration) :: flammable)
                ev.Crit <- false
                if flagQ ev qNoHitDamage then
                    for t in 0 .. 4 do
                        if has.[t] then
                            un.[t] <- 0.0
                            de.[t] <- 0.0
                // a shock the hit applies already counts for the hit itself
                (let currentShock =
                    if e.Has.[p.SShocked] && e.Expires.[p.SShocked] > s.Time then e.DMods.[p.SShocked] |> List.sumBy (fun m -> m.Value) else 0.0
                 let mutable newShock = nan
                 for i in 0 .. inflicted.Count - 1 do
                    match inflicted.[i] with
                    | Status (Shock, m, _) when System.Double.IsNaN newShock && m > currentShock -> newShock <- m
                    | _ -> ()
                 if not (System.Double.IsNaN newShock) then
                    let m = newShock
                    let own = e.Nums
                    if not e.ShockOdd then
                        // what the enemy takes with the new shock in place of the old: the reference
                        // makes the defender's mods anew for it, in another order (the same but for rounding)
                        for t in 0 .. 4 do
                            if has.[t] then
                                let v = un.[t]
                                let red = reduction ev nums false t v
                                nTaken nums t |> ignore
                                let taken = max 0.0 ((1.0 + (own.TakenInc.[t] - e.ShockInc + m) / 100.0) * own.TakenMore.[t])
                                de.[t] <- v * (1.0 - red / 100.0) * taken
                    else
                        // (a shock's mod of another kind: asked of the reference itself, with the mods
                        // and maps it would have; nothing of them has changed since the hit began)
                        let def = defender s e
                        let def = if own.X = 0.0 then def else { def with Mods = ModDb.add def.Mods (Mod.inc (DamageTaken AllDamage) own.X) }
                        let withoutOld = def.Mods |> ModDb.toList |> List.filter (fun x -> x.Source <> "Shock")
                        let def' = { def with Mods = ModDb.ofList ((Mod.inc (DamageTaken AllDamage) m |> Mod.from "Shock") :: withoutOld) }
                        let enemyStatus =
                            statusMap p beforeHas beforeStacks
                            |> (fun st -> if ev.Iso then Map.add "Isolated" 1 st else st)
                            |> (fun st -> if ev.Close then Map.add "Close" 1 st else st)
                            |> (fun st -> if ev.Open then Map.add "OpenWeakness" 1 st else st)
                        let ctx = Skill.ctx skill false (skillStacks s sid) enemyStatus
                        let unmitigated = [ for t in 0 .. 4 do if has.[t] then yield damageTypes.[t], un.[t] ] |> Map.ofList
                        let dealt = Hit.mitigate ctx (skillDb s sid) def' unmitigated
                        for t in 0 .. 4 do
                            if has.[t] then de.[t] <- dealt.[damageTypes.[t]])
                let dealt = total5 de
                let life = lifeBefore - dealt
                let dead = life <= 1e-9
                (match s.Tell with Some tell -> tell s.Time (if crit then "crit" else "hit") def0.Name skill.Name dealt | None -> ())
                // what the armour break below reads of the enemy as it was before the hit
                let brokenBefore =
                    if e.Has.[p.SArmourBreak] && e.Expires.[p.SArmourBreak] > s.Time then -(e.DMods.[p.SArmourBreak] |> List.sumBy (fun m -> m.Value)) else 0.0
                addDamage s info.Source (min dealt lifeBefore)
                // (only of a hit the enemy outlives: what a killing hit deals is the life that was left)
                if not dead then s.Evened <- s.Evened + dealt * (evenly - 1.0)
                // (a hit that deals more than the enemy has left: each type's share of what was left)
                (let k = if dealt > lifeBefore && dealt > 0.0 then lifeBefore / dealt else 1.0
                 for t in 0 .. 4 do
                    if has.[t] && de.[t] > 0.0 then addByType s t (de.[t] * k))
                bump s info.CHits
                if crit then bump s info.CCrits
                if dead then kill s e s.Time else e.Life <- life
                // Armour Break: the hit breaks armour by a share of the damage it dealt; armour at
                // zero is Fully Broken: the enemy takes increased physical damage from hits
                (let share =
                    de.[0] * sumQ ev qBreakPhysical / 100.0
                    + de.[3] * sumQ ev qBreakFire / 100.0
                    + dealt * sumQ ev qBreakDamage / 100.0
                    + sumQ ev qBreakFlat
                 // "Players Break 3 times Armour against Normal Monsters and 2 times Armour against Magic Monsters"
                 let broken =
                    share * multQ ev qBreakAmount
                    * (match def0.Rarity with Normal -> 3.0 | Magic -> 2.0 | _ -> 1.0)
                 if not (dead || broken <= 0.0) then
                    let baseArmour = e.Info.BaseArmour
                    let total = min baseArmour (brokenBefore + broken)
                    let seconds = Constants.armourBreakDuration * incQ ev qBreakDuration
                    addDebuff s e p.SArmourBreak seconds 1 [ Mod.base' Armour -total |> Mod.from "Armour Break" ]
                    if total >= baseArmour then
                        let taken = Constants.fullyBrokenArmourTaken * incQ ev qBrokenEffect
                        let scopes =
                            if flagQ ev qBrokenAll then [ AllDamage ]
                            else
                                [ OfType Physical ]
                                @ (if flagQ ev qBrokenFire then [ OfType Fire ] else [])
                                @ (if flagQ ev qBrokenColdLightning then [ OfType Cold; OfType Lightning ] else [])
                        addDebuff s e p.SFullyBroken seconds 1 (scopes |> List.map (fun sc -> Mod.inc (DamageTaken sc) taken |> Mod.from "Fully Broken Armour")))
                // leech, and what a hit and a kill give back
                (match p.Vitals with
                 | Some v ->
                    let leech = min de.[0] Constants.effectiveMaxDamageForLeech * sumQ ev qManaLeech / 100.0
                    // leech counts from one enemy per hit event: only the largest instance
                    let extra = max 0.0 (leech - s.StrikeLeech)
                    let charges =
                        match p.Fight.Player.ManaFlask with
                        | Some f when dead -> min f.MaxCharges (s.FlaskCharges + RarityCharges.flaskChargesPerKill def0.Rarity * f.ChargeGainMult)
                        | _ -> s.FlaskCharges
                    let leechMult = multQ ev qManaLeechAmount
                    let extra = extra * leechMult
                    let lifeLeechFull =
                        min de.[0] Constants.effectiveMaxDamageForLeech * sumQ ev qLifeLeech / 100.0
                        * multQ ev qLifeLeechAmount * v.LifeRecoveryRate
                    let lifeLeech = max 0.0 (lifeLeechFull - s.StrikeLifeLeech)
                    let onHit = sumQ ev qLifeOnHit * v.LifeRecoveryRate
                    let manaOnHit = sumQ ev qManaOnHit * v.ManaRecoveryRate
                    let lifeCharges =
                        match v.LifeFlask with
                        | Some f when dead -> min f.MaxCharges (s.LifeFlaskCharges + RarityCharges.flaskChargesPerKill def0.Rarity * f.ChargeGainMult)
                        | _ -> s.LifeFlaskCharges
                    if extra > 0.0 || (lifeLeech > 0.0 && v.Life > 1.0) then
                        s.Recoups <-
                            (if extra > 0.0 then [ s.Time + Constants.leechSeconds, extra / Constants.leechSeconds, "LeechMana" ] else [])
                            @ (if lifeLeech > 0.0 && v.Life > 1.0 then
                                   (let t = Constants.leechSeconds / max 0.05 v.LifeLeechRate
                                    [ s.Time + t, lifeLeech / t, "LeechLife" ])
                               else [])
                            @ s.Recoups
                    s.StrikeLeech <- max leech s.StrikeLeech
                    s.StrikeLifeLeech <- max lifeLeechFull s.StrikeLifeLeech
                    s.FlaskCharges <- charges
                    s.LifeFlaskCharges <- lifeCharges
                    s.Life <- min (max s.Life v.Life) (s.Life + (if v.Life > 1.0 then onHit else 0.0) + (if dead then v.LifeOnKill else 0.0))
                    s.Shield <- min (max s.Shield v.EnergyShield) (s.Shield + (if dead then v.ESOnKill else 0.0))
                    if manaOnHit > 0.0 then gainMana s v 4 manaOnHit
                    if dead then
                        let evE = acquireRaw s
                        evE.Q <- info.Q
                        evE.Tags <- Set.empty
                        evE.Name <- ""
                        evE.Dyn <- (if obj.ReferenceEquals(ev.Dyn, ev.DynRaw) then ev.DynRaw else resolveDyn ev.DynRaw Set.empty "")
                        let kill = (sumQ evE qManaOnKill + v.Mana * sumQ evE qManaPercentOnKill / 100.0) * v.ManaRecoveryRate
                        release s
                        gainMana s v 5 kill
                 | None -> ())
                // a killing hit's ailments are not inflicted; what the killed enemy was can set things off
                if dead then
                    react s rng p.OnKill "" sid -1
                    if p.HasKillOf then
                        statusMap p beforeHas beforeStacks |> Map.iter (fun k _ -> react s rng p.OnKillOf k sid -1)
                else
                    for k in 0 .. inflicted.Count - 1 do
                        let i = inflicted.[k]
                        inflict s e i
                        match i with
                        | Dot (a, _, _, _, _) | Status (a, _, _) ->
                            bump s p.CInflict.[ailmentIndex a]
                            react s rng p.OnInflict.[ailmentIndex a] "" sid target
                        | AddBuildup _ -> ()
                // the skill's own damage over time: one instance per skill and type, the strongest stays
                if not (dead || info.Dots.Length = 0) then
                    let duration = (if skill.Duration > 0.0 then skill.Duration else 1.0) * multQ ev qSkillDuration
                    // spell (or attack) damage mods only count if the skill says so
                    let asSkill = ev.Dyn
                    if not info.SpellDot then
                        ev.Tags <- info.DotTags
                        ev.AsAttack <- false
                        if not (obj.ReferenceEquals(ev.Dyn, ev.DynRaw)) then ev.Dyn <- resolveDyn ev.DynRaw info.DotTags skill.Name
                    for (t, perSecond, _, label) in info.Dots do
                        let ti = typeIndex t
                        let dps =
                            if info.SpellDot then perSecond * multQ ev qDamage.[ti] * multQ ev qSpellDotDamage
                            else perSecond * multQ ev qDotDamageAs.[ti] * multQ ev qDotDamage
                        let d = { Ailing = None; Label = label; Of = t; Dps = dps; Until = s.Time + duration; Most = 1 }
                        e.Dots <- d :: (e.Dots |> List.filter (fun x -> x.Label <> label || x.Dps > dps))
                        if not (d.Until >= e.Soonest) then e.Soonest <- d.Until
                        expireAt s d.Until
                    ev.Tags <- skill.Tags
                    ev.AsAttack <- info.IsAttack
                    ev.Dyn <- asSkill
                // meta gem energy: crits and newly inflicted ailments, scaled by monster power and by
                // the share of the ailment threshold the hit deals; none from triggered skills
                if p.Metas.Length > 0 && not (skill.Tags.Contains Triggered) then
                    let threshold = max 1.0 (nThreshold nums)
                    let power = MonsterPower.ofRarity def0.Rarity
                    if not dead then refreshStatus s e
                    let afterHas = e.SHas
                    let newly a =
                        if dead then
                            // on a killing hit: what this hit inflicted, and buildup that would complete
                            inflicted.Exists(fun i ->
                                match i with
                                | Dot (x, _, _, _, _) | Status (x, _, _) -> x = a
                                | AddBuildup (x, pct) -> x = a && e.Buildup.[ailmentIndex x] + pct >= 100.0)
                        else afterHas.[p.AilmentStatus.[ailmentIndex a]] && not beforeHas.[p.AilmentStatus.[ailmentIndex a]]
                    p.Metas |> Array.iteri (fun i meta ->
                        let amount =
                            meta.Meta.Sources
                            |> List.sumBy (fun src ->
                                match src with
                                | CritHits per when crit -> per * power * (total5 un / threshold)
                                | MeleeHits per when skill.Tags.Contains Melee -> per * power * (total5 un / threshold)
                                | MeleeKills per when dead && skill.Tags.Contains Melee -> per * power
                                | InflictsAilment (a, per) when newly a -> per * power * (source ev a un / threshold)
                                | _ -> 0.0)
                        // (what this fills up and triggers is aimed at the enemy that was hit)
                        if amount > 0.0 then
                            let cause = s.Cause
                            s.Cause <- target
                            gainEnergy s rng i (amount * meta.Meta.EnergyMult)
                            s.Cause <- cause)
                // Archon: gained on an ignite by a skill with the chance; none while one is active
                // or during the recovery period after one ends
                (let ignited = inflicted.Exists(fun i -> match i with Dot (Ignite, _, _, _, _) -> true | _ -> false)
                 if ignited && p.Archons.Length > 0 then
                    let active = p.Archons |> Array.exists (fun a -> s.BHas.[a.Buff])
                    let recovering = s.ArchonRecovery > s.Time
                    if not (active || recovering) then
                        let mutable found = -1
                        for i in 0 .. p.Archons.Length - 1 do
                            if found < 0 && rng.Chance(qSum ev info.QArchon.[i]) then found <- i
                        if found >= 0 then
                            let a = p.Archons.[found]
                            setBuff s a.Buff 1 (s.Time + a.Duration) a.AMods
                            s.ArchonRecovery <- s.Time + a.Duration + a.Recovery
                            expireAt s (s.Time + a.Duration)
                            bump s a.Gained)
                // charms: charges from kills, and the ones used by killing a rare or unique monster
                if dead && p.Charms.Length > 0 then
                    if p.Vitals.IsSome then
                        for c in p.Charms do
                            // "granting charges equal to half of the monster's Power"
                            let gain = (MonsterPower.ofRarity def0.Rarity / 2.0 + (if rng.Chance c.Charm.ChargeOnKillChance then 1.0 else 0.0)) * c.Charm.ChargeGainMult
                            s.CharmCharges.[c.CBuff] <- min c.Charm.MaxCharges (s.CharmCharges.[c.CBuff] + gain)
                    if def0.Rarity = Rare || def0.Rarity = Unique then charm s "KillRare" |> ignore
                // "Only one Hit every 0.5 seconds can cause you to gain Rage": when one last could
                // is kept, even where there is no Rage to gain
                (let fresh = s.Time - (Map.tryFind "RageHitAt" s.Energy |> Option.defaultValue -1e9) >= 0.5
                 let onHit = if fresh then sumQ ev qRageOnHit else 0.0
                 if onHit > 0.0 then s.Energy <- Map.add "RageHitAt" s.Time s.Energy
                 gainRage s (onHit + (if dead then p.RageOnKill else 0.0)))
                // what the game tracks for "recently" conditions
                mark s p.BHit 4.0
                if not crit then mark s p.BNonCrit 4.0
                if crit then
                    mark s p.BCrit 4.0
                    mark s p.BCrit8 8.0
                if dead then mark s p.BKilled 4.0
                react s rng p.OnHit "" sid target
                if crit then react s rng p.OnCrit "" sid target
            release s

    /// A strike lands: who is hit, and each hit (`Sim.onStrike`).
    let private onStrike (s: State) (rng: Rng) (sid: int) (mult: float) (aimedAt: int) (elapsed: float) (passing: bool) (direct: bool) =
        let p = s.P
        let alive = living s
        // Aim where the use was aimed; if that enemy died meanwhile, at the next one.
        let primary = if s.Foes.[aimedAt].Life > 0.0 then aimedAt else s.Aimed
        if primary >= 0 then
            let info = p.Skills.[sid]
            let skill = info.Skill
            let primaryDef = s.Foes.[primary].Def
            // (the buffs are the ones the strike began with, whatever its hits change)
            let ev = acquire s sid
            useBuffs s ev info true
            let shape () =
                let areaMult = multQ ev qArea
                let delivery =
                    match skill.Delivery with
                    | Projectiles pr ->
                        let fromCharges =
                            match pr.PerCharge with
                            | Some (_, per) -> per * buffStacksOf ev info.PerChargeBuff
                            | None -> 0
                        Projectiles
                            { pr with
                                Count =
                                    pr.Count + fromCharges
                                    + (if pr.Count + fromCharges > 0 && not (skill.Tags.Contains(Tag "Type:ProjectilesNumberModifiersNotApplied")) then
                                           int (sumQ ev qProjectiles)
                                           + (if rng.Chance(sumQ ev qExtraProjectiles2) then 2 else 0)
                                           + (let c = sumQ ev qExtraProjectiles1
                                              int (c / 100.0) + (if rng.Chance(c - 100.0 * floor (c / 100.0)) then 1 else 0))
                                       else 0)
                                Pierce = pr.Pierce + int (sumQ ev qPierce) + (if rng.Chance(sumQ ev qPierceChance) then 1 else 0)
                                Chains = pr.Chains + int (sumQ ev qChains)
                                TerrainChain = pr.TerrainChain + sumQ ev qTerrainChain }
                    | d -> d
                areaMult, delivery
            s.StrikeLeech <- 0.0
            s.StrikeLifeLeech <- 0.0
            // (the reference works the shape out twice, and each time rolls what it rolls)
            (match skill.Delivery with
             | Projectiles _ -> shape () |> ignore
             | _ -> ())
            let areaMult, delivery = shape ()
            let hitAll (names: string list) =
                let mutable rest = names
                while not rest.IsEmpty do
                    hitEnemy s rng sid mult p.EnemyIds.[rest.Head]
                    rest <- rest.Tail
            let strike () =
                if direct then
                    if primary = aimedAt then hitEnemy s rng sid mult aimedAt
                else
                    match delivery with
                    | Projectiles pr when not passing && pr.Passing > 0.0 && pr.HitInterval > 0.0 ->
                        // what is inside the projectiles as they travel: every hit at its own time
                        for (t, target) in Geometry.dwellHits rng p.Fight.Walls pr p.Fight.Reach areaMult primaryDef alive do
                            if t <= 0.0 then hitEnemy s rng sid mult p.EnemyIds.[target]
                            else schedule s (s.Time + t) EDirect sid p.EnemyIds.[target] mult 0.0
                    | Projectiles pr when passing && pr.Passing > 0.0 ->
                        // where the projectile is by now, on its way towards where it was aimed
                        let at = Vec.scale (pr.Speed * elapsed) (Vec.ofAngle (Vec.angle primaryDef.Position))
                        alive |> List.filter (Geometry.within at (Geometry.radius pr.Passing areaMult)) |> List.map (fun e -> e.Name) |> hitAll
                    | Single -> hitEnemy s rng sid mult primary
                    | _ -> Geometry.targetsFrom rng p.Fight.Walls p.Fight.Reach delivery areaMult primaryDef alive |> hitAll
            strike ()
            // Aftershock: a slam has a chance to hit its area once more
            if info.IsSlam && rng.Chance(sumQ ev qAftershock) then
                strike ()
                bump s info.CAftershock
            release s

    /// After a use's hits: the skill consumes its charges, unless they are kept (`Sim.step`).
    let private onConsume (s: State) (rng: Rng) (charge: int) (sid: int) =
        let p = s.P
        let info = p.Skills.[sid]
        let ev = acquireEmpty s sid
        let skip = sumQ ev qSkipConsume
        let every = sumQ ev qConsumeEvery / 1000.0
        let count = int (sumQ ev qConsumeCount)
        release s
        let spared = every > 0.0 && not (rng.Chance(100.0 * min 1.0 (useTime s sid / every)))
        if s.BHas.[charge] && not spared then
            // "still count as consuming": the consumed-recently status starts either way
            markStatus s p.ConsumedRecently.[charge]
            if rng.Chance skip then
                bump s info.CKept
                let pause = p.Fight.Player.KeptChargesPause
                if pause > 0.0 then
                    // freeze the timer: push expiry back by the part of the pause that
                    // doesn't overlap an earlier one
                    let pausedUntil = if System.Double.IsNaN s.BPaused.[charge] then s.Time else s.BPaused.[charge]
                    let newUntil = s.Time + pause
                    let extension = max 0.0 (newUntil - max s.Time pausedUntil)
                    if s.BHas.[charge] && s.BExpires.[charge] < infinity then
                        let expires = s.BExpires.[charge] + extension
                        setBuff s charge s.BStacks.[charge] expires s.BMods.[charge]
                        s.BPaused.[charge] <- newUntil
                        expireAt s expires
            else
                // one charge, a set number, or all of them (0)
                if s.BHas.[charge] && count > 0 && s.BStacks.[charge] > count then setBuff s charge (s.BStacks.[charge] - count) s.BExpires.[charge] s.BMods.[charge]
                else removeBuff s charge
                bump s info.CConsumed

    /// Stacks of a resource arrive (`Sim.step`, GainStacks).
    let private onGainStacks (s: State) (rng: Rng) (r: int) (n: int) =
        let p = s.P
        let maxR = if p.ResourceMax.[r] <> System.Int32.MinValue then p.ResourceMax.[r] else n
        // "N% chance to gain an additional (Power) Charge when you gain a Charge"
        let n =
            if not p.IsCharge.[r] || p.FirstSkill < 0 then n
            else
                let ev = acquireEmpty s p.FirstSkill
                let chance = qSum ev p.QExtra.[r]
                release s
                if chance > 0.0 && rng.Chance chance then n + 1 else n
        setStacks s true r (min maxR (stacksOf s r + n))
        if p.IsCharge.[r] then markStatus s p.GainedRecently.[r]
        bump s p.CGained.[r]

    /// A cycle picks its next variant (`Sim.step`, CycleTick).
    let private onCycle (s: State) (rng: Rng) (index: int) =
        let c = s.P.Cycles.[index]
        let pick = min (c.Variants.Length - 1) (int (rng.Float() * float c.Variants.Length))
        setBuff s c.Buff 1 (s.Time + c.Period) c.Variants.[pick]
        bump s c.Picked.[pick]
        schedule s (s.Time + c.Period) ECycle index 0 0.0 0.0

    /// What happens on its own every few seconds (`Sim.onPeriodic`).
    let private onPeriodic (s: State) (rng: Rng) (index: int) =
        let p = s.P
        let pd = p.Periodics.[index]
        schedule s (s.Time + pd.Every) EPeriodic index 0 0.0 0.0
        let gain (r: int) (n: int) =
            setStacks s true r n
            markStatus s p.GainedRecently.[r]
            bump s p.CGained.[r]
        match pd.Action with
        | PGainRandom resources ->
            let open' = resources |> Array.filter (fun (r, most) -> stacksOf s r < most)
            if open'.Length > 0 then
                let r, _ = open'.[min (open'.Length - 1) (int (rng.Float() * float open'.Length))]
                gain r (stacksOf s r + 1)
        | PGainWeighted weights ->
            let mutable total = 0.0
            for (_, _, w) in weights do
                total <- total + w
            let roll = rng.Float() * total
            let mutable picked = -1
            let mutable acc = 0.0
            for i in 0 .. weights.Length - 1 do
                let _, _, w = weights.[i]
                if picked < 0 then
                    if roll < acc + w then picked <- i
                    acc <- acc + w
            let r, most, _ = weights.[if picked < 0 then weights.Length - 1 else picked]
            gain r (min most (stacksOf s r + 1))
        | PGainAnyRandom resources ->
            let r, most = resources.[min (resources.Length - 1) (int (rng.Float() * float resources.Length))]
            gain r (min most (stacksOf s r + 1))
        | PTrigger skill -> trigger s rng skill
        | PHealLife (amount, count) ->
            match p.Vitals with
            | Some v when alive s && v.Life > 1.0 ->
                s.Life <- min (max s.Life v.Life) (s.Life + amount * v.LifeRecoveryRate)
                bump s count
            | _ -> ()
        | PGainTimedBuff (b, chance, duration, maxStacks, mods) ->
            // (the stacks are counted before the roll, the roll is made whatever they are)
            let have = stacksOf s b
            if not (rng.Float() >= chance || have >= maxStacks) then
                setBuff s b (have + 1) infinity mods
                schedule s (s.Time + duration) ESpendBuff b 0 0.0 0.0
                bump s p.CGained.[b]
        | PConsumeEach resources ->
            for r in resources do
                if stacksOf s r <> 0 then
                    let have = stacksOf s r
                    markStatus s p.ConsumedRecently.[r]
                    (match p.Vitals with
                     | Some v when r = p.BCharges.[0] -> gainMana s v 6 (v.Mana * p.ConsumePowerMana / 100.0)
                     | _ -> ())
                    if rng.Chance p.SheetSkip then bump s p.CKeptOf.[r]
                    else
                        setStacks s false r (have - 1)
                        bump s p.CLostOf.[r]

    /// A monster's hit at the player: evasion, deflection, armour, resistances, then energy
    /// shield and life (`Sim.attackPlayer`, in its order).
    let private attackPlayer (s: State) (rng: Rng) (index: int) =
        let p = s.P
        let e = s.Foes.[index]
        match e.Def.Attack, p.Vitals with
        | Some atk, Some pools when e.Life > 0.0 && alive s ->
            // mitigation from the player's current buffs and charges
            // (the defences are a function of the buffs: the same buffs, the same defences, and
            // working them out is most of what a monster's hit costs)
            let v =
                match p.Fight.Player.VitalsAt with
                | Some at ->
                    match s.VitalsLast with
                    | Some v when s.VitalsOf = s.BuffsVersion -> v
                    | _ ->
                        // the buffs written out: which there are, and their stacks
                        let mutable key = ""
                        for id in p.VitalsOn do
                            if s.BHas.[id] then key <- key + string id + "=" + string s.BStacks.[id] + ";"
                        let v =
                            match p.VitalsKnown.TryGetValue key with
                            | true, v -> v
                            | _ ->
                                match s.VitalsSeen.TryGetValue key with
                                | true, v -> v
                                | _ ->
                                    let v = at (buffMap s)
                                    s.VitalsSeen.[key] <- v
                                    v
                        s.VitalsOf <- s.BuffsVersion
                        s.VitalsLast <- Some v
                        v
                | None -> pools
            let st = status s e
            let stopped = [ "Frozen"; "HeavilyStunned"; "Electrocuted" ] |> List.exists st.ContainsKey
            let chill = if e.Has.[p.SChilled] then e.DMods.[p.SChilled] |> List.sumBy (fun m -> m.Value) else 0.0
            // curses on the monster: action speed (Temporal Chains), damage dealt (Enfeeble)
            let debuffs = (defender s e).Mods
            let actionSpeed = max 0.1 (Mods.more Ctx.empty [ Named "ActionSpeed" ] debuffs)
            let dealt = max 0.0 (Mods.more Ctx.empty [ Named "DamageDealt" ] debuffs)
            let atk = { atk with Damage = atk.Damage |> Map.map (fun _ d -> d * dealt) }
            let next = atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed
            schedule s (s.Time + next) EEnemyAttack index 0 0.0 0.0
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            if stopped then bump s p.CStopped
            elif inRange then
                let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                let hitChance =
                    let h = Vitals.monsterHitChance v.Evasion accuracy
                    // Glancing Blows: the evade roll is unlucky (both of two rolls must evade)
                    if v.UnluckyEvade then 100.0 - (100.0 - h) * (100.0 - h) / 100.0 else h
                let landed =
                    if hitChance >= 100.0 then true
                    else
                        let value =
                            if s.PHasEntropy && s.Time - s.PEntropyAt <= 100.0 * Constants.serverTick then s.PEntropy
                            else floor (rng.Float() * 100.0)
                        let value = value + hitChance
                        let hit = value >= 100.0
                        s.PEntropy <- (if hit then value - 100.0 else value)
                        s.PEntropyAt <- s.Time
                        s.PHasEntropy <- true
                        hit
                if not landed then bump s p.CEvaded
                else
                    let blockRoll () = rng.Chance v.Block
                    let blocked = blockRoll () || (v.LuckyBlock && blockRoll ())
                    let deflectChance =
                        let d = Vitals.deflectChance v.Deflection accuracy
                        if v.LuckyDeflect then 100.0 - (100.0 - d) * (100.0 - d) / 100.0 else d
                    let deflected = not blocked && rng.Chance deflectChance
                    // "<from> damage taken as <to>" moves part of each type before mitigation
                    let incoming =
                        v.TakenAs
                        |> List.fold
                            (fun (m: Map<DamageType, float>) (from, into, pct) ->
                                let moved = (Map.tryFind from atk.Damage |> Option.defaultValue 0.0) * pct / 100.0
                                m |> Map.add from ((Map.tryFind from m |> Option.defaultValue 0.0) - moved)
                                  |> Map.add into ((Map.tryFind into m |> Option.defaultValue 0.0) + moved))
                            atk.Damage
                    // a critical hit is tested against evasion once more: evaded, it is an ordinary hit
                    let crit = rng.Chance(atk.CritChance * v.EnemyCritChanceMult) && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
                    // "Defend with X% of Armour against Projectile Attacks / Critical Hits"
                    let armourShare = (if atk.Melee then 1.0 else v.ArmourVsProjectiles) * (if crit then v.ArmourVsCrits else 1.0)
                    let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 * (1.0 - v.LessCritDamageTaken / 100.0) else 1.0
                    let shocked =
                        if s.BHas.[p.BShocked] && s.BExpires.[p.BShocked] > s.Time then s.BMods.[p.BShocked] |> List.sumBy (fun m -> m.Value) else 0.0
                    let unmitigated = incoming |> Map.map (fun _ d -> d * critMult)
                    let taken =
                        unmitigated
                        |> Map.map (fun t d ->
                            let d = if deflected then d * (1.0 - v.DeflectEffect / 100.0) else d
                            let d =
                                match t with
                                | Physical ->
                                    let d = if atk.Melee then max 0.0 (d - v.FlatPhysicalTaken) else d
                                    d * (1.0 - Vitals.armourReduction (v.Armour * armourShare) d / 100.0)
                                | t ->
                                    let d = d * (1.0 - (Map.tryFind t v.Resist |> Option.defaultValue 0.0) / 100.0)
                                    // "X% of Armour also applies to <type> damage"
                                    match Map.tryFind t v.ArmourAppliesTo with
                                    | Some pct -> d * (1.0 - Vitals.armourReduction (v.Armour * armourShare * pct / 100.0) d / 100.0)
                                    | None -> d
                            d * (Map.tryFind t v.TakenMult |> Option.defaultValue 1.0) * (1.0 + shocked / 100.0)
                              * (if blocked then v.BlockedDamageTaken / 100.0 else 1.0))
                    takeDamage s pools taken
                    s.LastHit <- s.Time
                    gainRage s p.RageWhenHit
                    if p.FirstSkill >= 0 then react s rng p.WhenHit "" p.FirstSkill index
                    // "Rage loss is paused for 4 seconds upon gaining Rage, or after taking damage"
                    s.RageAt <- s.Time
                    // charms used when a hit of their damage type is taken (they help from the next hit on)
                    taken |> Map.iter (fun t d -> if d > 0.0 then charm s ("Hit:" + DamageType.name t) |> ignore)
                    if crit then bump s p.CEnemyCrit
                    setBuff s p.BBeenHit 1 (s.Time + 4.0) []
                    expireAt s (s.Time + 4.0)
                    // ailments and stuns the hit inflicts on the player (not when blocked)
                    if not blocked then
                        let get t = Map.tryFind t unmitigated |> Option.defaultValue 0.0
                        let avoided a = rng.Chance(Map.tryFind a v.Avoid |> Option.defaultValue 0.0)
                        let effect a = Map.tryFind a v.SelfEffect |> Option.defaultValue 1.0
                        let lasts a = Map.tryFind a v.SelfDuration |> Option.defaultValue 1.0
                        let thr = max 1.0 v.AilmentThreshold
                        let buff (id: int) (count: int) (mods: Mod list) (duration: float) () =
                            setBuff s id 1 (s.Time + duration) mods
                            expireAt s (s.Time + duration)
                            bump s count
                        let lock (seconds: float) (count: int) () =
                            s.LockedUntil <- max s.LockedUntil (s.Time + seconds)
                            bump s count
                        // a charm for this may stop it (and is used up doing so)
                        let unlessCharm (key: string) (apply: unit -> unit) =
                            if charm s key then bump s p.CPrevented.[key] else apply ()
                        // ignite: strongest only
                        (let fire = get Fire
                         if fire > 0.0 && rng.Chance(min 100.0 (fire / thr * Constants.igniteChanceMultiplier)) && not (avoided "Ignite") then
                            unlessCharm "Ignited" (fun () ->
                                let dps =
                                    fire * Constants.ignitePercentPerSecond / 100.0 * effect "Ignite"
                                    * (1.0 - (Map.tryFind Fire v.Resist |> Option.defaultValue 0.0) / 100.0) * (Map.tryFind Fire v.TakenMult |> Option.defaultValue 1.0)
                                let until = s.Time + Constants.igniteDuration * lasts "Ignite"
                                let stronger = s.Burning |> List.exists (fun (u, d, t) -> t = Fire && u > s.Time && d >= dps)
                                if not stronger then s.Burning <- (until, dps, Fire) :: (s.Burning |> List.filter (fun (_, _, t) -> t <> Fire))
                                bump s p.CPlayerIgnited))
                        (let lightning = get Lightning
                         if lightning > 0.0 && rng.Chance(min 100.0 (lightning / thr * Constants.shockChanceMultiplier)) && not (avoided "Shock") then
                            unlessCharm "Shocked" (buff p.BShocked p.CPlayerShocked [ Mod.base' (Named "ShockTaken") (Constants.baseShockMagnitude * effect "Shock") ] (Constants.playerShockDuration * lasts "Shock")))
                        (let cold = get Cold
                         let chill = Constants.chillEffectMultiplier * cold / thr * effect "Chill"
                         if chill >= 1.0 && not (avoided "Chill") then
                            unlessCharm "Slowed" (buff p.BChilled p.CPlayerChilled [ Mod.more Speed (-(min Constants.chillMaxEffect (max Constants.chillMinEffect chill))) ] (Constants.playerChillDuration * lasts "Chill"))
                         // freeze builds up with cold damage; full: frozen
                         if cold > 0.0 && not (avoided "Freeze") then
                            let buildup = s.FreezeBuildup + cold * Constants.playerFreezeScale / max 1.0 v.FreezeThreshold * 100.0
                            if buildup >= 100.0 then
                                s.FreezeBuildup <- 0.0
                                unlessCharm "Frozen" (lock (Constants.playerFreezeDuration * lasts "Freeze") p.CPlayerFrozen)
                            else s.FreezeBuildup <- buildup)
                        // light stun: chance from the hit's size against the stun threshold
                        let stunDamage =
                            unmitigated |> Map.toList
                            |> List.sumBy (fun (t, d) -> if t = Physical then d * (1.0 + Constants.monsterPhysicalStunMore / 100.0) else d)
                            |> fun d -> if atk.Melee then d * (1.0 + Constants.monsterMeleeStunMore / 100.0) else d
                        let stunChance = min 100.0 (stunDamage / max 1.0 v.StunThreshold * 100.0)
                        if stunChance >= Constants.playerLightStunMinimum && rng.Chance stunChance && not (rng.Chance v.AvoidStun) then
                            unlessCharm "Stunned" (lock Constants.playerLightStunDuration p.CPlayerStunned)
                    bump s p.CEnemyHit
                    if deflected then bump s p.CDeflected
                    if blocked then
                        bump s p.CBlocked
                        s.Ward <- (if v.WardOnBlock > 0.0 then min (max s.Ward pools.Ward) (s.Ward + v.WardOnBlock) else s.Ward)
                        s.Life <- min (max s.Life pools.Life) (s.Life + v.LifeOnBlock)
                        s.Mana <- min pools.Mana (s.Mana + v.ManaOnBlock)
                        // blocks feed meta gems (what a block triggers is aimed at the enemy whose hit was blocked)
                        p.Metas |> Array.iteri (fun i meta ->
                            let amount = meta.Meta.Sources |> List.sumBy (function Blocks energy -> energy | _ -> 0.0)
                            if amount > 0.0 then
                                let cause = s.Cause
                                s.Cause <- index
                                gainEnergy s rng i (amount * meta.Meta.EnergyMult)
                                s.Cause <- cause)
                        // blocks set off the rules for blocking
                        if p.FirstSkill >= 0 then react s rng p.OnBlock "" p.FirstSkill index
        | _ -> ()

    /// The player's life and mana state, as buffs conditions can read (`Sim.step`).
    let private playerStates (s: State) (v: PlayerStats) =
        let p = s.P
        // "Your Life cannot change while you have Energy Shield": no recovery above where it started
        if v.LifeFrozenWithES && s.Shield > 0.0 && s.Life > v.StartLife * v.Life then s.Life <- v.StartLife * v.Life
        // reserved life: what is left is the most there can be
        elif v.LifeReserved > 0.0 && s.Life > (1.0 - v.LifeReserved) * v.Life then s.Life <- (1.0 - v.LifeReserved) * v.Life
        let set (slot: int) (on: bool) =
            let id = p.BStates.[slot]
            if on then (if not s.BHas.[id] then setBuff s id 1 infinity [])
            else removeBuff s id
        let manaFlask = s.Time < s.FlaskUntil || stacksOf s p.BConstantMana > 0
        let lifeFlask = s.Time < s.LifeFlaskUntil || stacksOf s p.BConstantLife > 0
        set 0 (s.Life >= v.Life)
        set 1 (v.Life > 1.0 && s.Life <= 0.35 * v.Life)
        set 2 (s.Mana >= v.Mana)
        set 3 (s.Mana <= 0.35 * v.Mana)
        set 4 (s.Shield > 0.0)
        set 5 manaFlask
        set 6 lifeFlask
        set 7 (s.Time < s.FlaskUntil || s.Time < s.LifeFlaskUntil || stacksOf s p.BConstantMana > 0 || stacksOf s p.BConstantLife > 0)
        set 8 (s.Recoups |> List.exists (fun (until, _, pool) -> pool.StartsWith "Leech" && until > s.Time))
        // surrounded: at least 5 monsters within 3 m
        let mutable near = 0
        for e in s.Foes do
            if e.Life > 0.0 && Vec.length e.Def.Position - e.Def.Size <= 30.0 then near <- near + 1
        set 9 (near >= max 1 v.SurroundedAt)
        // charge states: "while at maximum Power Charges", "while you have no Frenzy Charges"
        for k in 0 .. 2 do
            let id = p.BCharges.[k]
            // (read before the states of this kind are set: a state's name is never a charge's)
            let n = stacksOf s id
            let maxN = if p.ResourceMax.[id] <> System.Int32.MinValue then p.ResourceMax.[id] else 0
            set (10 + 3 * k) (maxN > 0 && n >= maxN)
            set (11 + 3 * k) (n = 0)
            set (12 + 3 * k) (n = 0)

    /// The earliest event happens (`Sim.step`).
    let private step (s: State) =
        let p = s.P
        let q = s.Queue
        q.Pop()
        let t, kind, a, b, x, y = q.Time, q.Kind, q.A, q.B, q.X, q.Y
        let dt = t - s.Time
        (match s.Tell with
         | Some tell ->
            let who = p.Fight.Player.Name
            match kind with
            | EReady -> tell t "Ready" who "" (float q.Seq)
            | EStrike | EPassing | EDirect -> tell t (kindName kind) who p.Skills.[a].Skill.Name (float q.Seq)
            | EConsume | EGainStacks | ESpendBuff -> tell t (kindName kind) who p.BuffNames.[a] (float q.Seq)
            | ECycle -> tell t "CycleTick" who p.BuffNames.[p.Cycles.[a].Buff] (float q.Seq)
            | EPeriodic -> tell t "PeriodicTick" who p.Fight.Player.Periodics.[a].Name (float q.Seq)
            | EEnemyAttack -> tell t "EnemyAttack" p.Enemies.[a].Def.Name "" (float q.Seq)
            | EEnergy -> tell t "EnergyArrives" who p.Metas.[a].Meta.Name (float q.Seq)
            | EExpose -> tell t "Expose" who p.Enemies.[a].Def.Name (float q.Seq)
            | _ -> tell t "Expire" "" "" (float q.Seq)
         | None -> ())
        // a skill's hits roll from a stream of that skill alone: its first event, its second, ...
        let rng =
            match kind with
            | EStrike | EPassing | EDirect ->
                let n = s.StrikeStream.[a]
                s.StrikeStream.[a] <- n + 1
                Rng(Sim.streamSeedOf s.Seed p.Skills.[a].StrikeKey n)
            | EConsume ->
                let n = s.ConsumeStream.[b]
                s.ConsumeStream.[b] <- n + 1
                Rng(Sim.streamSeedOf s.Seed p.Skills.[b].ConsumeKey n)
            | EEnemyAttack ->
                let n = s.AttackStream.[a]
                s.AttackStream.[a] <- n + 1
                Rng(Sim.streamSeedOf s.Seed p.AttackKeys.[a] n)
            | EPeriodic ->
                let n = s.PeriodicStream.[a]
                s.PeriodicStream.[a] <- n + 1
                Rng(Sim.streamSeedOf s.Seed p.Periodics.[a].Key n)
            | _ -> s.Rng
        advance s t
        recover s dt
        if s.AliveCount > 0 && alive s then
            prune s
            // rage: regenerated all the time, lost once none has been gained for a while
            (let have = stacksOf s p.BRage
             if not (dt <= 0.0 || p.MaxRage <= 0 || (p.RageRegen <= 0.0 && have = 0)) then
                let losing = if s.Time - s.RageAt > p.RageDelay then p.RageLoss else 0.0
                // Berserk: life lost for every Rage held while Rage is not being lost (not below 1)
                (match p.Vitals with
                 | Some v when losing <= p.RageRegen && have > 0 && p.BerserkLoss > 0.0 ->
                    let life = max (min s.Life 1.0) (s.Life - p.BerserkLoss / 100.0 / 60.0 * float have * v.Life * dt)
                    s.Life <- life
                    s.MinLife <- min s.MinLife (life / max 1.0 v.Life)
                 | _ -> ())
                let carry = s.RageCarry + (p.RageRegen - losing) * dt
                let whole = int (System.Math.Truncate carry)
                let n = max 0 (min p.MaxRage (have + whole))
                if n <> have then setStacks s false p.BRage n
                s.RageCarry <- (if n = 0 && carry < 0.0 then 0.0 elif n = p.MaxRage && carry > 0.0 then 0.0 else carry - float whole))
            (match p.Vitals with
             | Some v -> playerStates s v
             | None -> ())
            match kind with
            | EReady -> onReady s rng
            | EStrike -> onStrike s rng a x b 0.0 false false
            | EPassing -> onStrike s rng a x b y true false
            | EDirect -> onStrike s rng a x b 0.0 false true
            | EConsume -> onConsume s rng a b
            | ECycle -> onCycle s rng a
            | EGainStacks -> onGainStacks s rng a b
            | EExpose -> expose s a DamageType.all.[b] x y
            | EPeriodic -> onPeriodic s rng a
            | EEnemyAttack -> attackPlayer s rng a
            | EEnergy -> gainEnergy s rng a x
            | ESpendBuff ->
                if s.BHas.[a] && s.BStacks.[a] > 1 then setBuff s a (s.BStacks.[a] - 1) s.BExpires.[a] s.BMods.[a]
                else removeBuff s a
            | _ -> ()

    let private start (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) : State =
        let fight = p.Fight
        let player = fight.Player
        let nb = p.BuffNames.Length
        let ns = p.StatusNames.Length
        let nk = p.Skills.Length
        let foes =
            p.Enemies
            |> Array.mapi (fun i info ->
                let e =
                    { Info = info
                      Index = i
                      Def = info.Def
                      Life = info.Def.Life
                      Has = Array.create ns false
                      Stacks = Array.zeroCreate ns
                      Expires = Array.zeroCreate ns
                      DMods = Array.create ns []
                      Soonest = infinity
                      Dots = []
                      Active = []
                      ActiveOf = []
                      Rates = [||]
                      RateLabels = [||]
                      RateTypes = [||]
                      RateSum = 0.0
                      RatesOf = []
                      RatesVersion = -1
                      Buildup = Array.zeroCreate 13
                      StunBuildupAt = 0.0
                      Flammability = []
                      Entropy = 0.0
                      EntropyAt = 0.0
                      HasEntropy = false
                      Status = Map.empty
                      SHas = [||]
                      SStacks = [||]
                      StatusMapStale = true
                      Nums =
                        { Version = -1; X = 0.0; MaxRes = Array.create 5 nan; Res = Array.create 5 nan; Taken = Array.create 5 nan
                          TakenInc = Array.create 5 nan; TakenMore = Array.create 5 nan
                          Poise = Array.create 13 nan; Armour = nan; PhysFlat = nan; Threshold = nan; Flam = nan }
                      DefVersion = 0
                      DStat = [||]
                      DCode = [||]
                      DName = [||]
                      DKind = [||]
                      DVal = [||]
                      DShock = [||]
                      DCount = 0
                      DListVersion = -1
                      ShockInc = 0.0
                      ShockOdd = false
                      DotTakenVersion = -1
                      UpPending = 0.0
                      UpAny = false
                      RatePending = 0.0
                      RateAny = false
                      StatusKeys = [||]
                      StatusStale = true
                      StatusDots = []
                      WasCullable = false
                      WasLow = false
                      WasFull = false
                      HitStatus = Map.empty
                      HitStatusOf = Map.empty
                      HitIsolated = false
                      HitClose = false
                      HitStatusMade = false
                      Defender = { Level = info.Def.Level; Mods = info.Db }
                      DefenderStale = true
                      DotTaken = Array.create 5 nan }
                for (id, stacks, mods) in info.Auras do
                    setDebuff e id stacks infinity mods
                e)
        let v = p.Vitals
        let s =
            { P = p
              Seed = seed
              Rng = Rng(seed)
              Queue = Queue()
              Tell = tell
              Time = 0.0
              Seq = 0
              Foes = foes
              AliveCount = foes |> Array.filter (fun e -> e.Life > 0.0) |> Array.length
              Living = []
              Aimed = -1
              AliveStale = true
              Kills = ResizeArray()
              BKept = false
              BHas = Array.create nb false
              BStacks = Array.zeroCreate nb
              BExpires = Array.zeroCreate nb
              BMods = Array.create nb []
              BPaused = Array.create nb nan
              BSoonest = infinity
              BuffMap = Map.empty
              BuffMapStale = true
              BChanged = ResizeArray()
              ModsVersion = 0
              BuffsVersion = 0
              SnapHas = [||]
              SnapStacks = [||]
              SnapVersion = -1
              Dyn = p.NoDyn
              DynVersion = -1
              DynOfSkill = false
              DynOnStatus = false
              DynFor = Array.create (nk * 3) p.NoDyn
              DynForVer = Array.create (nk * 3) -1
              BComp = Array.create nb [||]
              BCompOf = Array.create nb []
              RageMore = Array.create nk nan
              RageVer = Array.create nk -1
              Evs = ResizeArray()
              NoHas = [||]
              KVal = Array.create (nk * 3) [||]
              KBuffs = Array.create (nk * 3) [||]
              KStatus = Array.create (nk * 3) [||]
              KGBuffs = Array.zeroCreate (nk * 3)
              KGStatus = Array.zeroCreate (nk * 3)
              KSnap = Array.create (nk * 3) -1
              KDyn = Array.create (nk * 3) -1
              KFoe = Array.create (nk * 3) -1
              KHas = Array.create (nk * 3) [||]
              KFlags = Array.create (nk * 3) -1
              EvDepth = 0
              Dbs = p.Skills |> Array.map (fun k -> k.Db)
              DbVersion = Array.create nk -1
              Cooldowns = Array.create nk -infinity
              Recharges = Array.create nk []
              ArchonRecovery = -infinity
              InvocationReady = Array.create p.Metas.Length -infinity
              // invocations start with what was banked while clearing
              Energy =
                p.Metas |> Array.fold (fun (m: Map<string, float>) meta ->
                    match meta.Meta.Release with
                    | Invocation _ -> Map.add meta.Meta.Name (meta.Meta.MaxEnergy * fight.BankedInvocations) m
                    | AtMaximum -> m) Map.empty
              Cause = -1
              Aim = -1
              LastUsed = -1
              CritRoll = Array.zeroCreate nk
              HasCritRoll = Array.create nk false
              Rolled = Array.zeroCreate nk
              StrikeStream = Array.zeroCreate nk
              ConsumeStream = Array.zeroCreate nk
              Evened = 0.0
              Damage = Array.zeroCreate p.SourceNames.Length
              DamageSeen = Array.create p.SourceNames.Length false
              ByType = Array.zeroCreate 5
              ByTypeSeen = Array.create 5 false
              Counts = Array.zeroCreate p.CountNames.Length
              Uptime = Array.zeroCreate ns
              UptimeSeen = Array.create ns false
              Life = (match v with Some v -> max 1.0 (v.Life * v.StartLife) | None -> 0.0)
              Shield = (match v with Some v -> v.EnergyShield | None -> 0.0)
              Mana = (match v with Some v -> v.Mana | None -> 0.0)
              LastHit = -1e9
              Starved = 0.0
              FlaskCharges = (match v, player.ManaFlask with Some _, Some f -> f.MaxCharges | _ -> 0.0)
              FlaskUntil = 0.0
              FlaskRate = 0.0
              StrikeLeech = 0.0
              StrikeLifeLeech = 0.0
              Ward = (match v with Some v -> v.Ward | None -> 0.0)
              LifeFlaskCharges = (match v |> Option.bind (fun v -> v.LifeFlask) with Some f -> f.MaxCharges | None -> 0.0)
              LifeFlaskUntil = 0.0
              LifeFlaskRate = 0.0
              Recoups = []
              Died = nan
              Taken = 0.0
              MinLife = 1.0
              LockedUntil = 0.0
              FreezeBuildup = 0.0
              Burning = []
              CharmCharges = Array.zeroCreate nb
              RageAt = 0.0
              RageCarry = 0.0
              PEntropy = 0.0
              PEntropyAt = 0.0
              PHasEntropy = false
              AttackStream = Array.zeroCreate p.Enemies.Length
              PeriodicStream = Array.zeroCreate p.Periodics.Length
              VitalsSeen = Dictionary()
              VitalsOf = -1
              VitalsLast = None
              Gained = Array.zeroCreate gainedNames.Length
              GainedSeen = Array.create gainedNames.Length false }
        for c in p.Charms do
            s.CharmCharges.[c.CBuff] <- c.Charm.MaxCharges
        // the resources the fight starts with: a random part of their duration has already passed
        for (id, n, lasts) in p.Resources do
            let expires = match lasts with Some d -> s.Rng.Float() * d | None -> infinity
            setBuff s id n expires []
        schedule s 0.0 EReady 0 0 0.0 0.0
        p.Cycles |> Array.iteri (fun i _ -> schedule s 0.0 ECycle i 0 0.0 0.0)
        for id in p.Timed do
            if s.BHas.[id] && s.BExpires.[id] < infinity then expireAt s s.BExpires.[id]
        // periodic actions start at a random point of their first period
        p.Periodics |> Array.iteri (fun i pd -> schedule s (s.Rng.Float() * pd.Every) EPeriodic i 0 0.0 0.0)
        // monsters start attacking at random points in their first interval
        for i in p.Attackers do
            schedule s (s.Rng.Float() * p.Enemies.[i].Def.Attack.Value.Interval) EEnemyAttack i 0 0.0 0.0
        expireAt s fight.MaxTime
        s

    /// The fight played, for at most so many events.
    let private played (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) (maxEvents: int) : State =
        let s = start p seed tell
        let maxTime = p.Fight.MaxTime
        let mutable n = 0
        let mutable go = true
        while go do
            if not (alive s) || s.AliveCount = 0 || s.Queue.Count = 0 || s.Queue.Next > maxTime then go <- false
            elif n >= maxEvents then
                bump s p.CAborted
                go <- false
            else
                step s
                n <- n + 1
        s

    let private play (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) : Outcome =
        // a fight that needs more events than this is a runaway (a zero-time loop): stop it
        let s = played p seed tell 3000000
        let maxTime = p.Fight.MaxTime
        for e in s.Foes do
            flushUptime s e
            flushDots s e
        let cleared = if s.AliveCount = 0 then s.Kills |> Seq.map (fun (_, _, t) -> t) |> Seq.fold max 0.0 |> Some else None
        let floats (names: string[]) (values: float[]) (seen: bool[]) =
            let mutable m = Map.empty
            for i in 0 .. names.Length - 1 do
                if seen.[i] then m <- Map.add names.[i] values.[i] m
            m
        { Shown = []
          Ttk = cleared
          Duration = (match cleared with Some t -> t | None -> min s.Time maxTime)
          Kills = List.ofSeq s.Kills
          DamageBy = floats p.SourceNames s.Damage s.DamageSeen
          DamageByType = floats (DamageType.all |> List.map DamageType.name |> Array.ofList) s.ByType s.ByTypeSeen
          Counts =
            (let mutable m = Map.empty
             for i in 0 .. p.CountNames.Length - 1 do
                 if s.Counts.[i] > 0 then m <- Map.add p.CountNames.[i] s.Counts.[i] m
             m)
          Uptime = floats p.StatusNames s.Uptime s.UptimeSeen
          Died = (if alive s then None else Some s.Died)
          DamageTaken = s.Taken
          ManaStarved = s.Starved
          Evened = s.Evened
          MinLife = s.MinLife
          ManaGained = (if p.Vitals.IsSome then floats gainedNames s.Gained s.GainedSeen else Map.empty) }

    /// Play a program's fight with a seed: what `Sim.run` gives for the fight and the seed,
    /// but for `Shown`, which stays empty.
    let run (p: Program) (seed: uint32) : Outcome = play p seed None

    /// The same, telling `tell` what `Sim.traced` tells: every event as it is taken from the
    /// queue and every hit that lands.
    let traced (tell: float -> string -> string -> string -> float -> unit) (p: Program) (seed: uint32) : Outcome =
        play p seed (Some tell)

    /// A fight as a program for `run`, or what the fight has that is not built here. A fight
    /// that compiles gives the reference's result, but for rounding.
    ///
    /// Where monsters hit a player whose defences depend on its buffs (`Actor.VitalsAt`),
    /// the fight is played here once, briefly, to see which sets of buffs come up when
    /// a monster hits, and the defences for those are kept in the program: working them out
    /// is the costliest thing a fight asks for, and a run then asks only for sets not seen here.
    let compile (fight: Fight) : Result<Program, string> =
        match build fight with
        | Ok p ->
            if p.Attackers.Length > 0 && fight.Player.VitalsAt.IsSome then
                let known = Dictionary<string, PlayerStats>()
                for seed in 1u .. 1u do
                    for KeyValue (key, v) in (played p seed None 5000).VitalsSeen do
                        known.[key] <- v
                Ok { p with VitalsKnown = known }
            else Ok p
        | Error reasons -> Error(String.concat "; " reasons)
