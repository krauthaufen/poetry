namespace Fable.Core

/// Fable replaces a call of what has this attribute by the JavaScript given (it knows the
/// attribute by this name; the package it comes from is not needed for that). On .NET it
/// is an attribute like any other and the function's own body runs.
type EmitAttribute(macro: string) =
    inherit System.Attribute()

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
/// Played: the player and its minions and totems against any number of enemies, the dead
/// replaced where the fight says so; skills with parts, cooldowns, costs, charges and
/// ammunition; the rules whose reactions are listed in `Re`; ailments, damage over time,
/// curses, exposure, armour break; meta gems; cycles, periodic actions, archons, rage,
/// charms; infusions and remnants; Glory, Combo and banners; seals, Barrage and chained
/// attacks; what is planted in or beside an enemy (seeds, pustules, Ice Crystals); the
/// player's pools, with monsters walking up and hitting back; minions that are hurt, killed
/// and revived, temporary ones that are summoned and leave, corpses. What is left is refused
/// by `compile`, by name (`unsupported` lists it): the lists at `build` say what.
///
/// Every actor has a program of its own (`Program`: the player's, with the minions' in it)
/// and a state of its own during a run (`Agent`); the names (buffs, statuses, counts) have
/// the same numbers in all of them. One actor acts at a time (`State.P`, `State.A`, set by
/// `enter`): what is written for the player's skills, buffs and rules is a minion's the same way.
///
/// One thing is taken for granted of a fight: the defences for a set of buffs
/// (`Actor.VitalsAt`) are a function of the buffs (of those `Actor.VitalsReads` names, if
/// it names any). They are asked once per set of buffs, where the reference asks at every
/// hit of a monster; a `runner` keeps them from one run to the next.
///
/// Nothing here outlives a run: all that changes belongs to the `State` of one `run`, so
/// several runs of one program can go on at once.
module Flat =
    open System.Collections.Generic

    /// xs.[i], without Fable's check of the index in JavaScript: there every read of an array
    /// otherwise goes through one library function for arrays of all kinds, which was more
    /// than a quarter of a fight's time. (On .NET: plain indexing, checked as ever.)
    [<Fable.Core.Emit("$0[$1]")>]
    let inline private at (xs: 'a[]) (i: int) : 'a = xs.[i]

    /// xs.[i] <- v, likewise.
    [<Fable.Core.Emit("$0[$1] = $2")>]
    let inline private put (xs: 'a[]) (i: int) (v: 'a) : unit = xs.[i] <- v

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
    [<Literal>]
    let private ERevive = 13
    [<Literal>]
    let private ELeaves = 14
    [<Literal>]
    let private EWard = 15
    [<Literal>]
    let private EMana = 16
    [<Literal>]
    let private EPlant = 17

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
        | ERevive -> "ReviveMinions"
        | ELeaves -> "MinionLeaves"
        | EWard -> "WardArrives"
        | EMana -> "ManaArrives"
        | EPlant -> "PlantCrystals"
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
        /// (what of one damage type goes to each of the others, while `ranges` converts)
        member val Share : float[] = Array.zeroCreate 5 with get
        member val Has : bool[] = Array.zeroCreate 5 with get
        member val Un : float[] = Array.zeroCreate 5 with get
        member val De : float[] = Array.zeroCreate 5 with get
        member val Has2 : bool[] = Array.zeroCreate 5 with get
        member val Un2 : float[] = Array.zeroCreate 5 with get
        member val De2 : float[] = Array.zeroCreate 5 with get

    let private hasStatus (ev: Ev) (i: int) =
        i >= 0 && ((ev.SOn && (at ev.SHas i)) || (ev.Iso && i = ev.IsoId) || (ev.Close && i = ev.CloseId) || (ev.Open && i = ev.OpenId))

    let private statusStacks (ev: Ev) (i: int) =
        if i < 0 then 0
        elif (ev.Open && i = ev.OpenId) || (ev.Close && i = ev.CloseId) || (ev.Iso && i = ev.IsoId) then 1
        elif ev.SOn && (at ev.SHas i) then (at ev.SStacks i)
        else 0

    let private buffStacksOf (ev: Ev) (b: int) =
        if ev.BOn && b >= 0 && (at ev.BHas b) then
            let n = (at ev.BStacks b)
            if b = ev.Extra && n > 0 then n + ev.ExtraN else n
        else 0

    let rec private holds (ev: Ev) (c: CC) : bool =
        match c with
        | CTrue | CPerStack _ | CPerEnemy _ -> true
        | CFalse -> false
        | CCrit -> ev.Crit
        | CTag t -> ev.Tags.Contains t
        | CNamed n -> ev.Name = n
        | CBuff b -> ev.BOn && b >= 0 && (at ev.BHas b)
        | CStatus i -> hasStatus ev i
        | CNot c -> not (holds ev c)

    let private passes (ev: Ev) (it: Item) =
        let mutable ok = true
        for i__1 in 0 .. it.Conds.Length - 1 do
            let c = at it.Conds i__1
            if ok && not (holds ev c) then ok <- false
        ok

    /// The mod's value where its conditions hold (`Mod.eval`).
    let private amount (ev: Ev) (it: Item) =
        if it.Stacky then
            let mutable k = 1.0
            for i__2 in 0 .. it.Conds.Length - 1 do
                let c = at it.Conds i__2
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
            let it = (at q.Items i)
            if it.Slot < 0 then (if it.Conds.Length = 0 || passes ev it then acc <- acc + amount ev it)
            else
                for i__3 in 0 .. (at ev.Dyn it.Slot).Length - 1 do
                    let d = at (at ev.Dyn it.Slot) i__3
                    if d.Conds.Length = 0 || passes ev d then acc <- acc + amount ev d
        acc

    /// The product of the more mods (`Mods.more`).
    let private qMore (ev: Ev) (q: Query) : float =
        let mutable acc = q.Init
        for i in q.Start .. q.Items.Length - 1 do
            let it = (at q.Items i)
            if it.Slot < 0 then (if it.Conds.Length = 0 || passes ev it then acc <- acc * (1.0 + amount ev it / 100.0))
            else
                // (Rage's more attack damage is the newest mod of all)
                if it.Slot = ev.RageSlot && ev.AsAttack && not (System.Double.IsNaN ev.RageMore) then acc <- acc * (1.0 + ev.RageMore / 100.0)
                for i__4 in 0 .. (at ev.Dyn it.Slot).Length - 1 do
                    let d = at (at ev.Dyn it.Slot) i__4
                    if d.Conds.Length = 0 || passes ev d then acc <- acc * (1.0 + amount ev d / 100.0)
        acc

    let private qFlag (ev: Ev) (q: Query) : bool =
        let mutable acc = q.Init <> 0.0
        if not acc then
            for i in q.Start .. q.Items.Length - 1 do
                let it = (at q.Items i)
                if it.Slot < 0 then (if (it.Conds.Length = 0 || passes ev it) && amount ev it <> 0.0 then acc <- true)
                else
                    for i__5 in 0 .. (at ev.Dyn it.Slot).Length - 1 do
                        let d = at (at ev.Dyn it.Slot) i__5
                        if (d.Conds.Length = 0 || passes ev d) && amount ev d <> 0.0 then acc <- true
        acc

    /// The override, if there is one (`ev.Found`): the last of the mods in their order.
    let private qOver (ev: Ev) (q: Query) : float =
        let mutable v = 0.0
        ev.Found <- false
        for i in q.Start .. q.Items.Length - 1 do
            let it = (at q.Items i)
            if it.Slot < 0 then
                if it.Conds.Length = 0 || passes ev it then
                    v <- amount ev it
                    ev.Found <- true
            else
                for i__6 in 0 .. (at ev.Dyn it.Slot).Length - 1 do
                    let d = at (at ev.Dyn it.Slot) i__6
                    if d.Conds.Length = 0 || passes ev d then
                        v <- amount ev d
                        ev.Found <- true
        v

    /// The place of a kept answer, if it is still good (-1 - place if it must be worked out).
    let inline private kept (ev: Ev) (q: Query) (i: int) : int =
        let k = if ev.Crit then i + ev.Q.Length else i
        if (at ev.KBuffs k) >= 0 && (not q.OnBuffs || (at ev.KBuffs k) = ev.GBuffs) && (not (q.OnStatus || (q.HasSlot && ev.DynOnStatus)) || (at ev.KStatus k) = ev.GStatus) then k
        else
            put ev.KBuffs k (ev.GBuffs)
            put ev.KStatus k (ev.GStatus)
            -1 - k

    let private sumQ (ev: Ev) (i: int) : float =
        let q = (at ev.Q i)
        if q.Items.Length = 0 then q.Init
        elif not ev.Keeping then qSum ev q
        else
            let k = kept ev q i
            if k >= 0 then (at ev.KVal k)
            else
                let v = qSum ev q
                put ev.KVal (-1 - k) (v)
                v

    let private moreQ (ev: Ev) (i: int) : float =
        let q = (at ev.Q i)
        if q.Items.Length = 0 then q.Init
        elif not ev.Keeping then qMore ev q
        else
            let k = kept ev q i
            if k >= 0 then (at ev.KVal k)
            else
                let v = qMore ev q
                put ev.KVal (-1 - k) (v)
                v

    let private flagQ (ev: Ev) (i: int) : bool =
        let q = (at ev.Q i)
        if q.Items.Length = 0 then q.Init <> 0.0
        elif not ev.Keeping then qFlag ev q
        else
            let k = kept ev q i
            if k >= 0 then (at ev.KVal k) <> 0.0
            else
                let v = qFlag ev q
                put ev.KVal (-1 - k) ((if v then 1.0 else 0.0))
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
                if fixed' (at items i) then from <- i
            { Items = (if items.Length = 0 then items else items.[from..]); Start = 0; Init = 0.0; OnBuffs = false; OnStatus = false; HasSlot = false }
        | _ ->
            // the mods without conditions are worked out now, whatever comes between them (the
            // reference takes each in its turn: the same but for rounding where a conditional
            // one, or a buff's, counts too)
            let mutable acc = if kind = More then 1.0 else 0.0
            for i__7 in 0 .. items.Length - 1 do
                let it = at items i__7
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
    let private qKeepAmmo = site MSkill Base (named "KeepAmmoChance")
    let private qReloadSpeed = siteMult MSkill (named "ReloadSpeed")
    let private qInstantReload = site MSkill Base (named "InstantReloadChance")
    let private qTrinityGain = site MSkill Base (named "TrinityGain")
    let private qTrinityLoss = site MSkill Base (named "TrinityLoss")
    let private qCascades = site MSkill Base (named "CascadesPerSide")
    let private qCascadeArea = site MSkill More (named "CascadeArea")
    let private qInfusionKeep = site MEmpty Base (named "InfusionKeepChance")
    let private qInfusionPower = site MEmpty Base (named "PowerChargeOnInfusionConsumed")

    let private damageTypes = Array.ofList DamageType.all

    /// The questions the damage ranges of a hit are made of (`rangesAnew`).
    let private rangeSites =
        Array.concat [ [| qAddedEffect; qAddedEffect + 1; qNoNonFire |]; qAddedMin; qAddedMax; qGain; qSkillConvert; qConvert ]

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
        /// A buff scaled by the charges held (see `Reaction.ChargeBuff`).
        | RChargeBuff of buff: int * charge: int * perStack: Mod list * secondsPerStack: float * doubleChance: float * count: int
        | RSpendStack of buff: int * next: Re
        | RSetStacks of buff: int * stacks: int
        | RIfEnemyHas of status: int * next: Re
        | RIfBuff of buff: int * next: Re
        | RRegenES of perSecond: float * duration: float * count: int
        /// `meta`: -1 if the actor has no meta gem of the name (nothing happens then).
        | RGainEnergy of meta: int * amount: float
        /// `counts`: by the number of attacks empowered, where the count of it goes.
        | REmpower of buff: int * perPower: float * cap: float * counts: int[]
        /// A buff on every minion.
        | RGrantMinionBuff of buff: int * duration: float * mods: Mod list
        /// Minions of a pool appear (`members`: their places among the actors; `first`: the
        /// mods the first of them has for its time; `summoned`: where the count goes).
        | RSummon of members: int[] * count: int * duration: float * first: Mod list * summoned: int
        | RWithCorpse of next: Re * otherwise: Re
        | RSummonPerCorpse of members: int[] * duration: float * deadMinions: bool * summoned: int
        | RSummonPerWard of members: int[] * wardEach: float * duration: float * summoned: int
        | RGrantOneOf of buffs: int[] * duration: float
        /// A pustule on the enemy hit, if it has none (see `Reaction.AttachPustule`).
        | RAttachPustule of part: int * seconds: float * poisonPerPercent: float * capPercent: float * count: int
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
        /// Minions of a pool appear; `corpse`: only if there is a corpse, which it takes.
        | PSummon of members: int[] * count: int * duration: float * corpse: bool * summoned: int

    type private PeriodicInfo =
        { Every: float
          Action: PAction
          /// The name of its stream of random numbers.
          Key: uint32 }

    type private MetaInfo =
        { Meta: Meta
          /// Where its energy comes from, in the order the gem lists them, and whether any of
          /// that is a hit's doing.
          Sources: EnergySource[]
          FromHits: bool
          Triggers: int[]
          /// The character's own "increased Energy" for it, newest first, their conditions as numbers.
          EnergyMods: Item[]
          Released: int }

    /// An overkill explosion (see `OverkillBurn`).
    type private OverkillInfo =
        { Fits: bool[]
          ThresholdPercent: float
          PercentPerSecond: float
          /// The explosion among the player's skills (-1: it has none of the name), its
          /// radius before area modifiers (0: no area), its duration before modifiers.
          Burst: int
          Radius: float
          Seconds: float
          /// Where the burn's damage goes, and the count of its ignites.
          Label: int
          Ignited: int }

    /// A skill that can only be used once enough Glory or Combo has been built (`Sim.hitEnemy`).
    type private BuilderInfo =
        { /// By skill: whether a hit of it is the skill's own (which builds nothing).
          Own: bool[]
          GloryKey: string
          ComboKey: string
          Glory: float
          /// The most Glory it holds (what it needs, with "more Glory required").
          GloryCap: float
          Combo: float
          AttackHit: float
          ElementalHit: float
          ChaosHit: float
          /// Glory by newly inflicted state (heavily stunned, frozen, ignited), per monster power;
          /// and whatever the monster (ignited, shocked).
          Stunned: float
          Frozen: float
          Ignited: float
          FlatIgnited: float
          FlatShocked: float
          ComboExtraChance: float }

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
          /// What the damage ranges are made of (see `rangesAnew`): whether any of it can
          /// change with the buffs, with the enemy's statuses, and whether buffs' mods have a part in it.
          RangesOnBuffs: bool
          RangesOnStatus: bool
          RangesHasSlot: bool
          DotTags: Set<SkillTag>
          SpellDot: bool
          IsMelee: bool
          IsTriggered: bool
          /// Whether a hit of it can find an Open Weakness at all (Revealed Weaknesses).
          CanReveal: bool
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
          IsAreaAtTarget: bool
          /// An attack that uses up empowerments (a warcry's), and one that uses up readied repeats.
          SpendsEmpowered: bool
          SpendsRepeats: bool
          /// A skill that works on a corpse: whether a use takes one itself (no rule of the
          /// actor says what it does with corpses), and whether it needs one to be used
          /// (0: no; 1: yes; 2: unless the actor holds the buff `CorpseElse`).
          TakesCorpse: bool
          NeedsCorpse: int
          CorpseElse: int
          /// A part that only happens with an infusion, which it uses up: the infusion (-1: none).
          OnlyInfused: int
          /// A skill whose every impact consumes an infusion and is then its infused copy:
          /// the infusion (-1: the skill consumes none), the one that does in its place (-1:
          /// none), the copy among the actor's skills, and where the counts go.
          Infusion: int
          Wildcard: int
          Infused: int
          CInfused: int
          CUninfused: int
          /// A part that only happens against an enemy in one of some states (`OnlyVs`), and
          /// takes the first of them it finds off the enemy (`ConsumesVs`; `CConsumedVs`:
          /// where that is counted, by state); one that does not happen against some
          /// (`NotVs`); a state a hit that lands takes off the enemy (`Takes`, -1: none).
          OnlyVs: int[]
          ConsumesVs: bool
          CConsumedVs: int[]
          NotVs: int[]
          Takes: int
          CTakes: int
          /// A part that waits until another skill sets it off; the waiting parts of other
          /// skills that a use of this skill sets off, and where that is counted.
          Waits: bool
          SetsOff: int[]
          CDetonated: int
          /// The Glory and Combo the skill needs to be used (counter, amount), and the stacks
          /// of statuses the enemy aimed at must have.
          Requires: (string * float)[]
          NeedsStacks: (string * int)[]
          CReload: int
          /// A sealed skill: the most seals, what a seal takes (milliseconds, and a share of
          /// the use time in percent), the projectiles and repeats a broken seal gives, the
          /// repeats' damage (percent more), the buff's mods for the projectiles.
          MaxSeals: float
          SealMs: float
          SealPercent: float
          SealProjectiles: float
          SealRepeats: float
          SealRepeatDamage: float
          SealMods: Mod list
          CSeals: int
          /// Barrage: the repeats a use readies (and per Frenzy Charge), what the buff carries.
          BarrageRepeats: float
          BarragePerFrenzy: float
          BarrageMods: Mod list
          CBarraged: int
          /// A seed planted in the enemy that the damage the enemy takes sets off: the damage
          /// needed, percent of the enemy's ailment threshold (NaN: the skill plants none).
          Seed: float
          CPlanted: int
          CFizzled: int
          /// Ice Crystals a use leaves behind: whether the skill or part makes any, the life of
          /// one, the length of a wall of them, how many are made and the most there can be;
          /// the radius of its area (0: it has none), and whether that is around the user.
          Crystals: bool
          CrystalLife: float
          WallLength: float
          CrystalsCreated: float
          CrystalLimit: float
          AreaRadius: float
          AroundSelf: bool
          CCrystals: int
          CShattered: int
          /// Chained attacks: how many uses the chain has (0: the skill is none); of a part, the
          /// places in the chain it belongs to (`InChain`: whether it says so), and how many
          /// times it hits, spread over the use; where the skill's place in its chain is kept.
          SeqLength: int
          InChain: bool
          SeqSteps: int[]
          SeqHits: int
          SeqKey: string
          /// A form that is switched on: its buff (-1: the skill is none). One that is in it
          /// does not press the skill again.
          Form: int
          /// A warp into one's own projectile: the skills whose strikes it can warp into (empty:
          /// the skill is no warp), the buff of theirs that goes with the projectile, the buff
          /// that says the warp went into one, and where that is counted.
          WarpsInto: int[]
          Warps: bool
          WarpBuff: int
          Warped: int
          CWarped: int
          /// Whether a use asks how long the skill's effects last, and how large its area is.
          AsksDuration: bool
          AsksArea: bool }

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
              /// The actor this is the program of, whether it is the player, and the minions'
              /// programs (in the player's only). The names and their numbers are the same in
              /// all of them. An actor's place among the actors is 0 for the player, then the
              /// minions' as the fight lists them.
              Actor: Actor
              IsPlayer: bool
              Minions: Program[]
              /// The actor's own "increased Exposure effect".
              ExposureEffect: float
              /// A minion's body (None: nothing can happen to it), the buff of the player's
              /// that it only acts under (-1: it always acts), and the explosion at low life
              /// among its skills (-1: none).
              Body: MinionBody option
              ActiveBuff: int
              Explodes: int
              /// The minions that can be hurt, and those that come back when killed (by their
              /// places among the actors); whether any minion is a temporary one; whether
              /// monsters walk up (the player has pools).
              /// By ailment: whether only one of it ever counts on an enemy (no skill of any
              /// actor can have it stack, whatever the buffs).
              OneOfAilment: bool[]
              Bodies: int[]
              Reviving: int[]
              AnyPool: bool
              Walks: bool
              /// Whether monsters attack at all (the player or a minion can be hurt).
              Attacked: bool
              /// Infusion remnants: by what inflicting an ailment may drop (the ailment, the
              /// infusion, the chance, where the count goes), the chance of two, the seconds until it arrives.
              Remnants: (Ailment * int * float * int)[]
              RemnantExtra: float
              RemnantDelay: float
              BConsumedInfusion: int
              BReloaded: int
              BSealsBroken: int
              BDemonForm: int
              BDemonflame: int
              CDemonEnded: int
              BBarrage: int
              CBarrageReadied: int
              /// The actor's skills and parts by name.
              SkillIds: Dictionary<string, int>
              /// Trinity: the resonance of each element (lightning, cold, fire), and their sum.
              BResonance: int[]
              BResonanceSum: int
              /// The skills that need Glory or Combo; the banners, each with its buff and where
              /// it is counted; whether a Sigil of Power can stand, and where its stages are counted.
              Builders: BuilderInfo[]
              Banners: (Banner * int * int)[]
              HasSigil: bool
              CSigil: int
              /// Whether any meta gem gains energy from hits.
              MetaHits: bool
              /// What a full Infernal Flame burns, percent of maximum life and energy shield.
              InfernalBurn: float
              CInfernalBurn: int
              /// Runic Ward and mana remnants (the player's).
              WardRemnants: WardRemnants option
              ManaRemnants: ManaRemnants option
              CWardRemnant: int
              CManaRemnant: int
              BMinionDied: int
              BEmpowered: int
              CMinionDied: int
              CMinionRevived: int
              CMinionHit: int
              CMinionEvaded: int
              CMinionTook: int
              CMinionExploded: int
              CMinionNoRoom: int
              CMinionLeft: int
              Vitals: PlayerStats option
              BuffNames: string[]
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
              /// The charge buffs that are up when the fight starts: buff, charge, mods per
              /// stack, seconds per stack, chance (%) of twice the stacks.
              StartCharged: (int * int * Mod list * float * float) list
              Overkill: OverkillInfo[]
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
              /// The enemies that attack, in the order the fight names them.
              Attackers: int[]
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
        [ "TriskelionArms", "Triskelion Cascade"
          "DetonatesCurse", "curse detonation (Hexblast)"
          "NoCooldownWithCharge", "skills that skip their cooldown when empowered"
          "WindowSeconds", "skills with a window (Requiem)"
          "SpendsRage", "parts that spend Rage"
          "RecordsDamage", "debuffs that record damage (Inevitable Agony)" ]

    let private unbuiltPrefixes : (string * string) list = []

    /// Buffs the reference treats in a way of its own that is not built here.
    let private unbuiltBuffs : (string * string) list = []
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
        let no = ResizeArray<string>()
        let reject (what: string) = if not (no.Contains what) then no.Add what
        let buffs = Names()
        let statuses = Names()
        let counts = Names()
        let sources = Names()

        // ---- the fight ----
        // (a replacement is named after the enemy it replaces, with a + more)
        if fight.Replace && fight.Enemies |> List.exists (fun e -> e.Name.EndsWith "+") then reject "an enemy named like a replacement"
        if fight.Enemies.IsEmpty then reject "a fight without enemies"
        if (fight.Enemies |> List.map (fun e -> e.Name) |> List.distinct).Length <> fight.Enemies.Length then reject "enemies of the same name"
        // (the player's evasion entropy goes by this name in the reference, whatever the player is called)
        if fight.Enemies |> List.exists (fun e -> e.Name = "Player") then reject "an enemy called Player"
        let actors = fight.Player :: fight.Minions
        if (actors |> List.map (fun a -> a.Name) |> List.distinct).Length <> actors.Length then reject "actors of the same name"
        // (a minion's evasion entropy goes by its name, beside the enemies' and the player's)
        if fight.Minions |> List.exists (fun m -> m.Name = "Player" || fight.Enemies |> List.exists (fun e -> e.Name = m.Name)) then reject "a minion named like an enemy"

        // ---- the enemies ----
        let enemies = fight.Enemies |> List.sortBy (fun e -> e.Name) |> Array.ofList
        let curseK (e: EnemyDef) = Mods.more Ctx.empty [ Named "CurseEffectOnSelf" ] (ModDb.ofList e.Mods)
        for (name, _, _) in fight.EnemyAuras do
            statuses.Id name |> ignore
        let enemyInfos =
            enemies
            |> Array.map (fun d ->
                let db = ModDb.ofList d.Mods
                { Def = d
                  Db = db
                  CurseK = curseK d
                  ExposureK = Mods.more Ctx.empty [ Named "ExposureEffectOnSelf" ] db
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

        // ---- what the actors share ----
        // (curses name the others of their kind, which are only all known once every rule
        // has been read: they are filled in at the end)
        let curses = ResizeArray<string * int>()
        let consumedOf = ResizeArray<string>()
        let gainedOf = ResizeArray<string>()
        // the mods that a rule of any actor puts on the minions (an offering, an empowered
        // zombie): every minion has a place for them
        let onMinions =
            let rec ofReaction (r: Reaction) : Mod list =
                match r with
                | GrantMinionBuff (_, _, mods) -> mods
                | SummonEmpowered (_, _, _, first) -> first
                | WithCorpse (next, otherwise) -> ofReaction next @ ofReaction otherwise
                | SpendStack (_, next) | IfEnemyHas (_, next) | IfBuff (_, next) -> ofReaction next
                | _ -> []
            actors |> List.collect (fun a -> a.Rules |> List.collect (fun r -> ofReaction r.Reaction))
        // the minions of each pool, by their places among the actors
        let pool (name: string) : int[] =
            [| for (i, m) in List.indexed fight.Minions do
                if m.Pool = Some name then yield i + 1 |]
        let anyPool = fight.Minions |> List.exists (fun m -> m.Pool.IsSome)
        let anyBody = fight.Minions |> List.exists (fun m -> m.Body.IsSome)
        let reviving = [| for (i, m) in List.indexed fight.Minions do if m.Body |> Option.exists (fun b -> b.Revive.IsSome) then yield i + 1 |]

        // ---- an actor: the player, or a minion ----
        let one (index: int) (actor: Actor) : unit -> Program =
            let isPlayer = index = 0
            if (actor.Metas |> List.map (fun m -> m.Name) |> List.distinct).Length <> actor.Metas.Length then reject "meta gems of the same name"
            // (the reference pays a minion's remnants into the player's pools by the player's own remnants)
            if not isPlayer && actor.WardRemnants.IsSome then reject "Runic Ward remnants of a minion"
            if not isPlayer && actor.ManaRemnants.IsSome then reject "mana remnants of a minion"
            if isPlayer && (actor.ActiveWhile.IsSome || actor.Body.IsSome || actor.Pool.IsSome) then reject "a player that is a minion"
            // (the reference lets whoever has these use the player's pools)
            if not isPlayer && (actor.Vitals.IsSome || actor.ManaFlask.IsSome) then reject "a minion with pools"

            // ---- its skills ----
            let all = actor.Skills @ (actor.Skills |> List.collect (fun s -> s.Parts))
            // (the reference finds a skill by its name, the first of that name: two of one name
            // that are the same skill are that skill; of two that differ it mixes the two)
            if all |> List.groupBy (fun s -> s.Name) |> List.exists (fun (_, same) -> same |> List.exists (fun s -> s <> same.Head)) then
                reject "skills or parts of the same name that differ"
            if actor.Skills |> List.exists (fun s -> s.Parts |> List.exists (fun p -> not p.Parts.IsEmpty)) then reject "parts that have parts"
            // (the reference keeps these beside the skills' cooldowns, under these names)
            if all |> List.exists (fun s -> s.Name = "ArchonRecovery" || s.Name.StartsWith "invocation:" || s.Name.StartsWith "SealBreak:") then reject "a skill named like a cooldown of the engine's own"
            let skillIds = Dictionary<string, int>()
            all |> List.iteri (fun i s -> if not (skillIds.ContainsKey s.Name) then skillIds.[s.Name] <- i)
            let skillId (what: string) (name: string) =
                match skillIds.TryGetValue name with
                | true, i -> i
                | _ ->
                    reject (what + " of a skill its actor does not have")
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
            if not isPlayer then buffMods onMinions

            // ---- every mod there can be ----
            let sigils = ResizeArray<string>()
            let checkMods (mods: Mod list) =
                for m in mods do
                    match m.Stat with
                    | Named n ->
                        if n = "SigilMaxStages" then sigils.Add n
                        for (stat, what) in unbuiltStats do
                            if n = stat then reject what
                        for (prefix, what) in unbuiltPrefixes do
                            if n.StartsWith prefix then reject what
                    | _ -> ()
            checkMods actor.Mods
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
                actor.Resources
                |> List.filter (fun (_, n) -> n > 0)
                |> List.map (fun (k, n) -> buff k, n, Map.tryFind k actor.ResourceDurations)
            for KeyValue (k, _) in actor.ResourceDurations do
                buff k |> ignore
            for KeyValue (k, _) in actor.MaxResources do
                buffs.Id k |> ignore

            // ---- rules ----
            let sheet = ModDb.ofList actor.Mods
            let maxCurses = 1 + int (Mods.sumBase Ctx.empty [ Named "MaxCurses" ] sheet)
            let summoned (poolName: string) = counts.Id("summoned:" + poolName)
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
                | ChargeBuff (name, charge, perStack, seconds, doubleChance) ->
                    checkMods perStack
                    buffMods perStack
                    RChargeBuff(buff name, buffs.Id charge, perStack, seconds, doubleChance, counts.Id("buff:" + name))
                | SpendStack (b, next) -> RSpendStack(buffs.Id b, reaction next)
                | SetStacks (b, n) -> RSetStacks(buff b, n)
                | IfEnemyHas (st, next) -> RIfEnemyHas(statuses.Id st, reaction next)
                | IfBuff (b, next) -> RIfBuff(buffs.Id b, reaction next)
                | RegenES (rate, duration) -> RRegenES(rate, duration, counts.Id "regen:ES")
                | GainEnergy (meta, amount) -> RGainEnergy((match actor.Metas |> List.tryFindIndex (fun m -> m.Name = meta) with Some i -> i | None -> -1), amount)
                | Empower (b, perPower, cap) ->
                    // (the most there can be: with every enemy alive)
                    let power = fight.Enemies |> List.sumBy (fun e -> MonsterPower.ofRarity e.Rarity)
                    let most = max 1 (int (floor (min cap power / max 1.0 perPower)))
                    REmpower(buff b, perPower, cap, Array.init (most + 1) (fun n -> counts.Id("empowered:" + string n)))
                | NoReaction -> RNone
                | GrantMinionBuff (name, duration, mods) ->
                    checkMods mods
                    RGrantMinionBuff(buff name, duration, mods)
                | Summon (poolName, count, duration) -> RSummon(pool poolName, count, duration, [], summoned poolName)
                | SummonEmpowered (poolName, count, duration, first) ->
                    checkMods first
                    RSummon(pool poolName, count, duration, first, summoned poolName)
                | WithCorpse (next, otherwise) -> RWithCorpse(reaction next, reaction otherwise)
                | SummonPerCorpse (poolName, duration, deadMinions) -> RSummonPerCorpse(pool poolName, duration, deadMinions, summoned poolName)
                | SummonPerWard (poolName, wardEach, duration) -> RSummonPerWard(pool poolName, wardEach, duration, summoned poolName)
                | GrantOneOf (names, duration) -> RGrantOneOf(names |> List.map buff |> Array.ofList, duration)
                | AttachPustule (part, seconds, per, cap) -> RAttachPustule((match skillIds.TryGetValue part with | true, i -> i | _ -> -1), seconds, per, cap, counts.Id "pustule")
                | other ->
                    reject ("the reaction " + reactionName other)
                    RNone
            // (a charge buff is up when the fight starts, if fights start prepared, whatever its rule's hook is)
            let startCharged =
                if fight.BankedInvocations <= 0.0 then []
                else
                    actor.Rules |> List.choose (fun r ->
                        match r.Reaction with
                        | ChargeBuff (name, charge, perStack, seconds, doubleChance) ->
                            checkMods perStack
                            buffMods perStack
                            Some(buff name, buffs.Id charge, perStack, seconds, doubleChance)
                        | _ -> None)
            let rule (r: Rule) : CRule =
                { Applies = all |> List.map (matches r.Filter) |> Array.ofList
                  Chance = r.Chance
                  Re = reaction r.Reaction
                  Killed = (match r.Hook with OnKillOf st -> st | _ -> "") }
            let rulesOf (pick: Hook -> bool) = actor.Rules |> List.filter (fun r -> pick r.Hook) |> List.map rule |> Array.ofList
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
                actor.Metas
                |> List.map (fun m ->
                    ({ Meta = m
                       Sources = Array.ofList m.Sources
                       FromHits = m.Sources |> List.exists (fun src -> match src with CritHits _ | MeleeHits _ | MeleeKills _ | InflictsAilment _ -> true | _ -> false)
                       Triggers = m.Triggers |> List.map (skillId "a meta gem's trigger") |> Array.ofList
                       EnergyMods =
                        (let found = ResizeArray<Item>()
                         ModDb.forStat (Named "EnergyGenerated") Inc m.EnergyMods (fun md ->
                            match compileConds buffs.Id statuses.Id None md.Conds with
                            | Some conds, stacky -> found.Add { V = md.Value; Conds = conds; Stacky = stacky; Slot = -1 }
                            | None, _ -> ())
                         found.ToArray())
                       Released = counts.Id("meta:" + m.Name) }: MetaInfo))
                |> Array.ofList

            // ---- overkill explosions ----
            let overkill =
                actor.OverkillBurns
                |> List.map (fun b ->
                    let burst = actor.Skills |> List.tryFind (fun x -> x.Name = b.Skill)
                    let called = (let k = b.Skill.IndexOf ':' in if k > 0 then b.Skill.Substring(0, k) else b.Skill)
                    ({ Fits = all |> List.map (matches b.Filter) |> Array.ofList
                       ThresholdPercent = b.ThresholdPercent
                       PercentPerSecond = b.PercentPerSecond
                       Burst = (match burst with Some sk -> skillIds.[sk.Name] | None -> -1)
                       Radius = (match burst with Some { Delivery = AreaAtTarget r } -> r | _ -> 0.0)
                       Seconds = (match burst with Some sk -> sk.Duration | None -> 0.0)
                       Label = sources.Id called
                       Ignited = counts.Id("overkill ignite:" + b.Skill) }: OverkillInfo))
                |> Array.ofList

            // ---- charms ----
            // (the charms are the player's: the reference never looks at a minion's)
            let charms =
                (if isPlayer then actor.Charms else [])
                |> List.map (fun c -> ({ Charm = c; CBuff = buff c.Buff; Used = counts.Id("charm:" + c.Name) }: CharmInfo))
                |> Array.ofList
            let cPrevented = Dictionary<string, int>()
            for key in [ "Ignited"; "Shocked"; "Slowed"; "Frozen"; "Stunned" ] do
                cPrevented.[key] <- counts.Id("charm prevented:" + key)

            // ---- what happens on its own ----
            // (the ticks of two periodics of one name would share a stream of random numbers)
            if (actor.Periodics |> List.map (fun pd -> pd.Name) |> List.distinct).Length <> actor.Periodics.Length then reject "periodic actions of the same name"
            let maxOf (r: string) = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
            let periodics =
                actor.Periodics
                |> List.map (fun pd ->
                    // (a tick names its periodic: of two of one name the first answers both)
                    let pd = actor.Periodics |> List.find (fun o -> o.Name = pd.Name)
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
                        | SummonMinions (poolName, count, duration) -> PSummon(pool poolName, count, duration, false, summoned poolName)
                        | SummonFromCorpse (poolName, count, duration) -> PSummon(pool poolName, count, duration, true, summoned poolName)
                    ({ Every = pd.Every; Action = action; Key = Sim.streamHash ("P|" + actor.Name + "|" + pd.Name) }: PeriodicInfo))
                |> Array.ofList

            // ---- cycles, archons ----
            let cycles =
                actor.Cycles
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
                actor.Archons
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

            // (before the skills' answers are made: they have a place for every mod a buff can bring)
            let banners =
                actor.Banners
                |> List.map (fun b ->
                    checkMods b.Mods
                    buffMods b.Mods
                    b, buff ("Banner:" + b.Name), counts.Id("banner:" + b.Name))
                |> Array.ofList
            let exposureEffect = Mods.inc Ctx.empty [ Named "ExposureEffect" ] sheet

            // ---- skills ----
            let consumed = all |> List.choose (fun s -> s.Consumes) |> List.distinct
            for charge in consumed do
                buff charge |> ignore
                consumedOf.Add charge
            let anyKind (s: Skill) (name: string) = s.Mods |> List.sumBy (fun m -> if m.Stat = Named name then m.Value else 0.0)
            // (a number a skill's own mods give: `own` in the reference)
            let ownBase (s: Skill) (name: string) = s.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
            // the buffs a use of a skill itself brings: a sealed skill's broken seals, the repeats
            // Barrage readies (before the skills' answers are made, which have a place for their mods)
            let sealMods = Dictionary<string, Mod list>()
            let barrageMods = Dictionary<string, Mod list>()
            for s in all do
                if ownBase s "SealProjectiles" > 0.0 && not (sealMods.ContainsKey s.Name) then
                    let mods = [ Mod.base' AdditionalProjectiles (ownBase s "SealProjectiles") |> Mod.whenever (SkillNamed s.Name) ]
                    buffMods mods
                    sealMods.[s.Name] <- mods
                if ownBase s "BarrageRepeats" > 0.0 && not (barrageMods.ContainsKey s.Name) then
                    let mods = [ Mod.base' (Named "BarrageRepeatLess") (ownBase s "BarrageRepeatLess"); Mod.base' (Named "BarrageCooldownPct") (ownBase s "BarrageCooldownPct") |> Mod.from s.Name ]
                    buffMods mods
                    barrageMods.[s.Name] <- mods
            // a skill's answers: its mods and the character's, newest first, by stat and kind
            let queriesOf (s: Skill) : (int * ModKind * Stat list) -> Query =
                let byKey = Dictionary<string, ResizeArray<Mod>>()
                for m in List.rev (actor.Mods @ s.Mods) do
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
                    // (what the skill does with corpses, if a rule of the actor says so: `Sim.corpseRule`)
                    let onCorpse = s.Tags.Contains(Tag "Type:TargetsDestructibleCorpses")
                    let corpseRule =
                        actor.Rules |> List.tryPick (fun r ->
                            match r.Reaction with
                            | WithCorpse _ | SummonPerCorpse _ when r.Hook = OnUse && matches r.Filter s -> Some r.Reaction
                            | _ -> None)
                    // (the rest of a flag of the skill's that starts so: `Sim.partNamed`)
                    let named' (prefix: string) = s.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith prefix -> Some(n.Substring prefix.Length) | _ -> None)
                    let states (prefix: string) = match named' prefix with Some st -> st.Split '|' |> List.ofArray | None -> []
                    let onlyVs = states "OnlyVs:"
                    let consumer = actor.Infusions |> Option.bind (fun inf -> Map.tryFind s.Name inf.Consumers |> Option.map (fun c -> inf, c))
                    let query = queriesOf s
                    let answers = sites.ToArray() |> Array.map query
                    let rangeQueries = rangeSites |> Array.map (fun i -> at answers i)
                    let own = actor.Mods @ s.Mods
                    let pattern = Map.tryFind s.Name actor.HitPattern |> Option.defaultValue (s.Hits |> List.map (fun (f, m) -> DuringUse f, m)) |> Array.ofList
                    // (as `Sim.skillStacks`: a part counts like the skill it belongs to)
                    let stackCharge =
                        s.Consumes
                        |> Option.orElse (actor.Skills |> List.tryPick (fun o -> if o.Parts |> List.exists (fun p -> p.Name = s.Name) then o.Consumes else None))
                    let stackExtra =
                        match stackCharge with
                        | Some charge ->
                            let ctx = { Ctx.empty with SkillName = s.Name; SkillTags = s.Tags }
                            int (Mods.sumBase ctx [ Named("Extra" + charge + "sConsumed") ] (ModDb.ofList (actor.Mods @ s.Mods)))
                        | None -> 0
                    { Skill = s
                      Db = ModDb.ofList (actor.Mods @ s.Mods)
                      Parts = (s :: s.Parts) |> List.map (fun p -> skillIds.[p.Name]) |> Array.ofList
                      Consumes = (match s.Consumes with Some c -> buffs.Id c | None -> -1)
                      StackCharge = stackCharge |> Option.defaultValue ""
                      StackBuff = (match stackCharge with Some c -> buffs.Id c | None -> -1)
                      Q = answers
                      QArchon = actor.Archons |> List.map (fun a -> query (MSkill, Base, [ Named a.OnIgniteStat ])) |> Array.ofList
                      BaseMin = DamageType.all |> List.map (fun t -> match Map.tryFind t s.BaseDamage with Some r -> r.Min | None -> 0.0) |> Array.ofList
                      BaseMax = DamageType.all |> List.map (fun t -> match Map.tryFind t s.BaseDamage with Some r -> r.Max | None -> 0.0) |> Array.ofList
                      RangesOnBuffs = rangeQueries |> Array.exists (fun q -> q.OnBuffs)
                      RangesOnStatus = rangeQueries |> Array.exists (fun q -> q.OnStatus)
                      RangesHasSlot = rangeQueries |> Array.exists (fun q -> q.HasSlot)
                      HasGain = own |> List.exists (fun m -> match m.Stat with GainAs _ -> true | _ -> false)
                      HasConvert = own |> List.exists (fun m -> match m.Stat with SkillConvert _ | Convert _ -> true | _ -> false)
                      DotTags = s.Tags |> Set.remove Spell |> Set.remove Attack
                      SpellDot = s.Tags.Contains(Tag "SpellDot")
                      IsMelee = s.Tags.Contains Melee
                      IsTriggered = s.Tags.Contains Triggered
                      CanReveal = (let q = at answers qReveal in not (q.Items.Length = 0 && q.Init = 0.0))
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
                      StrikeKey = Sim.streamHash ("S|" + actor.Name + "|" + s.Name)
                      ConsumeKey = Sim.streamHash ("C|" + actor.Name + "|" + s.Name)
                      RollKey = Sim.streamHash (actor.Name + "|" + s.Name)
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
                      IsAreaAtTarget = (match s.Delivery with AreaAtTarget _ -> true | _ -> false)
                      SpendsEmpowered = s.Tags.Contains Attack && not (s.Tags.Contains Warcry)
                      SpendsRepeats = s.Tags.Contains Attack && s.Tags.Contains(Tag "Type:Barrageable")
                      TakesCorpse = onCorpse && corpseRule.IsNone
                      NeedsCorpse =
                        (if not onCorpse then 0
                         else
                            match corpseRule with
                            | Some (SummonPerCorpse _) -> 0
                            | Some (WithCorpse (_, SpendStack _)) -> 2
                            | Some _ -> 1
                            | None -> if s.Tags.Contains(Tag "Type:SkillConsumesPowerChargesOnUse") then 0 else 1)
                      CorpseElse = (match corpseRule with Some (WithCorpse (_, SpendStack (b, _))) -> buffs.Id b | _ -> -1)
                      OnlyInfused =
                        (match s.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "OnlyInfused:" -> Some(n.Substring 12) | _ -> None) with
                         | Some infusion -> buff infusion
                         | None -> -1)
                      Infusion = (match consumer with Some (_, (element, _)) -> buff element | None -> -1)
                      Wildcard = (match consumer with Some (inf, _) -> (match inf.Wildcard with Some w -> buff w | None -> -1) | None -> -1)
                      Infused = (match consumer with Some (_, (_, copy)) -> skillId "an infusion" copy | None -> -1)
                      CInfused = counts.Id("infused:" + s.Name)
                      CUninfused = counts.Id("uninfused:" + s.Name)
                      OnlyVs = onlyVs |> List.map statuses.Id |> Array.ofList
                      ConsumesVs = flag "ConsumesVs"
                      CConsumedVs = onlyVs |> List.map (fun st -> counts.Id("consumed:" + st)) |> Array.ofList
                      NotVs = states "NotVs:" |> List.map statuses.Id |> Array.ofList
                      Takes = (match named' "Takes:" with Some st -> statuses.Id st | None -> -1)
                      CTakes = (match named' "Takes:" with Some st -> counts.Id("consumed:" + st) | None -> -1)
                      Waits = (named' "DetonatedBy:").IsSome
                      SetsOff =
                        [| for o in actor.Skills do
                            for part in o.Parts do
                                match part.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "DetonatedBy:" -> Some(n.Substring 12) | _ -> None) with
                                | Some by when not (s.Parts |> List.exists (fun q -> q.Name = part.Name)) && ((by = "Warcry" && s.Tags.Contains Warcry) || s.Tags.Contains(Tag by)) ->
                                    yield skillIds.[part.Name]
                                | _ -> () |]
                      CDetonated = counts.Id("detonated:" + s.Name)
                      Requires =
                        [| "Glory:" + s.Name, ownBase s "Requires:Glory" * (1.0 + ownBase s "GloryRequiredMore" / 100.0)
                           "Combo:" + s.Name, ownBase s "Requires:Combo" |]
                        |> Array.filter (fun (_, n) -> n > 0.0)
                      NeedsStacks = [| for m in s.Mods do match m.Stat with Named n when n.StartsWith "NeedsEnemyStacks:" -> yield n.Substring 17, int m.Value | _ -> () |]
                      CReload = counts.Id("reload:" + s.Name)
                      MaxSeals = ownBase s "MaxSeals"
                      SealMs = ownBase s "SealIntervalMs"
                      SealPercent = ownBase s "SealIntervalPercentOfUse"
                      SealProjectiles = ownBase s "SealProjectiles"
                      SealRepeats = ownBase s "SealRepeats"
                      SealRepeatDamage = ownBase s "SealRepeatDamage"
                      SealMods = (match sealMods.TryGetValue s.Name with | true, mods -> mods | _ -> [])
                      CSeals = counts.Id("seals:" + s.Name)
                      BarrageRepeats = ownBase s "BarrageRepeats"
                      BarragePerFrenzy = ownBase s "BarrageRepeatsPerFrenzy"
                      BarrageMods = (match barrageMods.TryGetValue s.Name with | true, mods -> mods | _ -> [])
                      CBarraged = counts.Id("barraged:" + s.Name)
                      Seed = (match s.Mods |> List.tryPick (fun m -> if m.Stat = Named "DetonatedByDamage" then Some m.Value else None) with Some pct -> pct | None -> nan)
                      CPlanted = counts.Id("planted:" + s.Name)
                      CFizzled = counts.Id("fizzled:" + s.Name)
                      Crystals = flag "Crystals"
                      CrystalLife = anyKind s "CrystalLife"
                      WallLength = anyKind s "WallLength"
                      CrystalsCreated = anyKind s "CrystalsCreated"
                      CrystalLimit = anyKind s "CrystalLimit"
                      AreaRadius = (match s.Delivery with AreaAroundSelf r | AreaAtTarget r -> r | _ -> 0.0)
                      AroundSelf = (match s.Delivery with AreaAroundSelf _ -> true | _ -> false)
                      CCrystals = counts.Id("crystals:" + s.Name)
                      CShattered = counts.Id("shattered:" + s.Name)
                      SeqLength = (match s.Mods |> List.tryPick (fun m -> if m.Stat = Named "SeqLength" then Some(int m.Value) else None) with Some n -> n | None -> 0)
                      InChain = (named' "SeqSteps:").IsSome
                      SeqSteps = (match named' "SeqSteps:" with Some steps -> steps.Split ',' |> Array.map int | None -> [||])
                      SeqHits = (match s.Mods |> List.tryPick (fun m -> if m.Stat = Named "SeqHits" then Some(int m.Value) else None) with Some n -> n | None -> 1)
                      SeqKey = "Seq:" + s.Name
                      Form = (match named' "ToggleBuff:" with Some form -> buffs.Id form | None -> -1)
                      WarpsInto =
                        (match named' "WarpsInto:" with
                         | Some gem -> [| for (i, o) in List.indexed all do if skillIds.[o.Name] = i && (o.Name = gem || o.Name.StartsWith(gem + " ")) then yield i |]
                         | None -> [||])
                      Warps = (named' "WarpsInto:").IsSome
                      WarpBuff = (match named' "WarpsInto:" with Some gem -> buffs.Id(gem + " buff") | None -> -1)
                      Warped = (match named' "WarpsInto:" with Some _ -> buffs.Id("Warped:" + s.Name) | None -> -1)
                      CWarped = (match named' "WarpsInto:" with Some gem -> counts.Id("warped into:" + gem) | None -> -1)
                      AsksDuration = ownBase s "BarrageRepeats" > 0.0 || flag "DetonatedByDamage" || (s :: s.Parts) |> List.exists (fun part -> part.Mods |> List.exists (fun m -> m.Stat = Named "Crystals"))
                      AsksArea = (s :: s.Parts) |> List.exists (fun part -> part.Mods |> List.exists (fun m -> m.Stat = Named "Crystals")) })
                |> Array.ofList
            let sIso, sClose, sOpen = statuses.Id "Isolated", statuses.Id "Close", statuses.Id "OpenWeakness"
            let sDerived = derivedStatuses |> List.map statuses.Id |> Array.ofList
            let rotation = actor.Rotation |> List.map (fun step -> skillId "a step" step.Use, step.When) |> Array.ofList

            let explodes =
                match actor.Body |> Option.bind (fun b -> b.Explodes) with
                | Some name -> skillId "a minion's explosion" name
                | None -> -1
            let builders =
                actor.Skills
                |> List.filter (fun s -> s.Mods |> List.exists (fun m -> match m.Stat with Named n -> n.StartsWith "Requires:" | _ -> false))
                |> List.map (fun s ->
                    let own = ownBase s
                    ({ Own = all |> List.map (fun hit -> hit.Name = s.Name || s.Parts |> List.exists (fun part -> part.Name = hit.Name)) |> Array.ofList
                       GloryKey = "Glory:" + s.Name
                       ComboKey = "Combo:" + s.Name
                       Glory = own "Requires:Glory"
                       GloryCap = own "Requires:Glory" * (1.0 + own "GloryRequiredMore" / 100.0)
                       Combo = own "Requires:Combo"
                       AttackHit = own "Builds:Glory:AttackHit"
                       ElementalHit = own "Builds:Glory:ElementalHit"
                       ChaosHit = own "Builds:Glory:ChaosHit"
                       Stunned = own "Builds:Glory:HeavilyStunned"
                       Frozen = own "Builds:Glory:Frozen"
                       Ignited = own "Builds:Glory:Ignited"
                       FlatIgnited = own "Builds:GloryFlat:Ignited"
                       FlatShocked = own "Builds:GloryFlat:Shocked"
                       ComboExtraChance = own "ComboExtraChance" }: BuilderInfo))
                |> Array.ofList
            let remnants =
                match actor.Infusions with
                | Some inf -> inf.Remnants |> List.map (fun (a, r, chance) -> a, buff r, chance, counts.Id("remnant:" + r)) |> Array.ofList
                | None -> [||]
            // (named here, while names are still given numbers)
            buffs.Id "ConsumedInfusionRecently" |> ignore
            buffs.Id "ReloadedRecently" |> ignore
            buffs.Id "SealsBroken" |> ignore
            buffs.Id "Demon Form" |> ignore
            buffs.Id "Demonflame" |> ignore
            buffs.Id "Barrage" |> ignore
            for t in DamageType.elemental do
                buffs.Id("Resonance:" + DamageType.name t) |> ignore
            buffs.Id "Resonance" |> ignore
            buffs.Id "MinionDiedRecently" |> ignore
            buffs.Id "Empowered" |> ignore
            actor.ActiveWhile |> Option.iter (fun name -> buffs.Id name |> ignore)

            // ---- once the names are all known ----
            fun () ->
                let fill (ids: int[]) = Array.init buffs.Names.Length (fun i -> if i < ids.Length then (at ids i) else -1)
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
                            let others = statuses.Order |> Array.filter (fun i -> i <> id && (at statusNames i).StartsWith prefix)
                            RApplyCurse(id, duration, mods, others, limit, count)
                        | RSpendStack (b, next) -> RSpendStack(b, fix next)
                        | RIfEnemyHas (st, next) -> RIfEnemyHas(st, fix next)
                        | RIfBuff (b, next) -> RIfBuff(b, fix next)
                        | RWithCorpse (next, otherwise) -> RWithCorpse(fix next, fix otherwise)
                        | other -> other
                    { r with Re = fix r.Re }
                let program : Program =
                    { Fight = fight
                      Actor = actor
                      IsPlayer = isPlayer
                      Minions = [||]
                      ExposureEffect = exposureEffect
                      Body = actor.Body
                      ActiveBuff = (match actor.ActiveWhile with Some name -> buffs.Id name | None -> -1)
                      Explodes = explodes
                      OneOfAilment = [||]
                      Bodies = [| for (i, m) in List.indexed fight.Minions do if m.Body.IsSome then yield i + 1 |]
                      Reviving = reviving
                      AnyPool = anyPool
                      Walks = fight.Player.Vitals.IsSome
                      Attacked = fight.Player.Vitals.IsSome || anyBody
                      Remnants = remnants
                      RemnantExtra = (match actor.Infusions with Some inf -> inf.ExtraRemnantChance | None -> 0.0)
                      RemnantDelay = (match actor.Infusions with Some inf -> inf.Delay | None -> 0.0)
                      BConsumedInfusion = buffs.Id "ConsumedInfusionRecently"
                      BReloaded = buffs.Id "ReloadedRecently"
                      BSealsBroken = buffs.Id "SealsBroken"
                      BDemonForm = buffs.Id "Demon Form"
                      BDemonflame = buffs.Id "Demonflame"
                      CDemonEnded = count "demon form ended"
                      BBarrage = buffs.Id "Barrage"
                      CBarrageReadied = count "barrage readied"
                      SkillIds = skillIds
                      BResonance = DamageType.elemental |> List.map (fun t -> buffs.Id("Resonance:" + DamageType.name t)) |> Array.ofList
                      BResonanceSum = buffs.Id "Resonance"
                      Builders = builders
                      Banners = banners
                      HasSigil = sigils.Count > 0
                      MetaHits = metas |> Array.exists (fun m -> m.FromHits)
                      CSigil = count "sigil stage"
                      InfernalBurn = Mods.sumBase Ctx.empty [ Named "InfernalFlameBurn" ] sheet
                      CInfernalBurn = count "infernal flame burn"
                      WardRemnants = actor.WardRemnants
                      ManaRemnants = actor.ManaRemnants
                      CWardRemnant = count "ward remnant"
                      CManaRemnant = count "mana remnant"
                      BMinionDied = buffs.Id "MinionDiedRecently"
                      BEmpowered = buffs.Id "Empowered"
                      CMinionDied = count "minion:died"
                      CMinionRevived = count "minion:revived"
                      CMinionHit = count "minion:hit"
                      CMinionEvaded = count "minion:evaded"
                      CMinionTook = count "minion:took for owner"
                      CMinionExploded = count "minion:exploded"
                      CMinionNoRoom = count "minion:no room"
                      CMinionLeft = count "minion:left"
                      Vitals = actor.Vitals
                      BuffNames = buffNames
                      VitalsOn =
                        (match actor.VitalsReads with
                         | Some names -> [| for i in 0 .. buffNames.Length - 1 do if names.Contains (at buffNames i) then yield i |]
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
                        (match actor.Skills with
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
                      FirstSkill = (if actor.Skills.IsEmpty then -1 else 0)
                      Rotation = rotation
                      OnUse = onUse |> Array.map fixCurses
                      OnHit = onHit |> Array.map fixCurses
                      OnCrit = onCrit |> Array.map fixCurses
                      OnKill = onKill |> Array.map fixCurses
                      OnKillOf = onKillOf |> Array.map fixCurses
                      OnInflict = onInflict |> Array.map (Array.map fixCurses)
                      WhenHit = whenHit |> Array.map fixCurses
                      StartCharged = startCharged
                      Overkill = overkill
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
                      MaxRage = Map.tryFind "Rage" actor.MaxResources |> Option.defaultValue 0
                      Attackers =
                        (if actor.Vitals.IsNone && not anyBody then [||]
                         else fight.Enemies |> List.filter (fun e -> e.Attack.IsSome) |> List.map (fun e -> enemyIds.[e.Name]) |> Array.ofList)
                      SheetSkip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] sheet
                      ConsumePowerMana = Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] sheet
                      Enemies = enemyInfos
                      EnemyIds = enemyIds
                      Cycles = cycles
                      Archons = archons
                      Resources = resources
                      ResourceSeconds = buffNames |> Array.map (fun n -> Map.tryFind n actor.ResourceDurations |> Option.defaultValue nan)
                      ResourceMax = buffNames |> Array.map (fun n -> Map.tryFind n actor.MaxResources |> Option.defaultValue System.Int32.MinValue)
                      Timed = buffs.Order |> Array.filter (fun i -> actor.ResourceDurations.ContainsKey (at buffNames i))
                      Empowering = buffs.Order |> Array.filter (fun i -> (at buffNames i).StartsWith "EmpoweredAttacks")
                      Readied = buffs.Order |> Array.filter (fun i -> (at buffNames i).StartsWith "EmpoweredRepeatable:")
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
                      BHit = (at bRecently 0)
                      BNonCrit = (at bRecently 1)
                      BCrit = (at bRecently 2)
                      BCrit8 = (at bRecently 3)
                      BKilled = (at bRecently 4)
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
                      SShocked = (at ailmentStatus (ailmentIndex Shock))
                      SChilled = (at ailmentStatus (ailmentIndex Chill))
                      SArmourBreak = sArmourBreak
                      SFullyBroken = sFullyBroken
                      CFlaskMana = cFlaskMana
                      CPoolLoss = cPoolLoss
                      CAborted = cAborted
                      HasKillOf = onKillOf.Length > 0
                      CountNames = counts.Names
                      SourceNames = sources.Names }
                program
        let finishers = actors |> List.mapi one

        // ---- the names that come from other names ----
        // (all of them before any actor's arrays are made, which are as long as the names are many)
        let rec derive (from: int) =
            let names = buffs.Names
            for i in from .. names.Length - 1 do
                let n = names.[i]
                if n.EndsWith "Charge" || gainedOf.Contains n then buffs.Id("Gained" + n + "Recently") |> ignore
                if consumedOf.Contains n then buffs.Id("Consumed" + n + "Recently") |> ignore
            if buffs.Names.Length > names.Length then derive names.Length
        derive 0
        if no.Count > 0 then Error(List.ofSeq no)
        else
            let programs = finishers |> List.map (fun finish -> finish ()) |> Array.ofList
            // (an ailment of which one counts: no skill of any actor is ever asked for more stacks of it)
            let oneOf =
                qAilStacks |> Array.map (fun site ->
                    programs |> Array.forall (fun p -> p.Skills |> Array.forall (fun k -> (at k.Q site).Items.Length = 0 && (at k.Q site).Init = 0.0)))
            // (the counts and sources are every actor's)
            let programs = programs |> Array.map (fun p -> { p with CountNames = counts.Names; SourceNames = sources.Names; OneOfAilment = oneOf })
            Ok { programs.[0] with Minions = programs.[1..] }

    /// Everything about a fight that this engine does not play yet (empty: it compiles).
    let unsupported (fight: Fight) : string list =
        match build fight with
        | Ok _ -> []
        | Error reasons -> reasons

    // ---- a run ----

    let private growInts (xs: int[]) : int[] =
        let ys = Array.zeroCreate (xs.Length * 2)
        for i in 0 .. xs.Length - 1 do
            put ys i ((at xs i))
        ys

    let private growFloats (xs: float[]) : float[] =
        let ys = Array.zeroCreate (xs.Length * 2)
        for i in 0 .. xs.Length - 1 do
            put ys i ((at xs i))
        ys

    /// The events that wait, the earliest first (of two at the same time, the one that was
    /// put in first): a binary heap of slots, an event's values in the slot's place of each array.
    type private Queue() =
        let mutable heap : int[] = Array.zeroCreate 64
        let mutable count = 0
        let mutable time : float[] = Array.zeroCreate 64
        let mutable seq : int[] = Array.zeroCreate 64
        let mutable kind : int[] = Array.zeroCreate 64
        let mutable who : int[] = Array.zeroCreate 64
        let mutable tag : int[] = Array.zeroCreate 64
        let mutable a : int[] = Array.zeroCreate 64
        let mutable b : int[] = Array.zeroCreate 64
        let mutable x : float[] = Array.zeroCreate 64
        let mutable y : float[] = Array.zeroCreate 64
        let mutable free : int[] = Array.init 64 id
        let mutable freeCount = 64
        let before i j = (at time i) < (at time j) || ((at time i) = (at time j) && (at seq i) < (at seq j))
        // the event that was taken last
        member val Time = 0.0 with get, set
        member val Seq = 0 with get, set
        member val Kind = 0 with get, set
        /// Whose event it is (the player: 0; a minion: its place among the actors), and which
        /// of the minion's times in the fight it belongs to (see `Agent.Epoch`).
        member val Who = 0 with get, set
        member val Tag = 0 with get, set
        member val A = 0 with get, set
        member val B = 0 with get, set
        member val X = 0.0 with get, set
        member val Y = 0.0 with get, set
        member _.Count = count
        /// Nothing waits any more (the arrays are kept).
        member _.Clear() =
            count <- 0
            freeCount <- free.Length
            for i in 0 .. free.Length - 1 do
                put free i (i)
        /// When the earliest is due.
        member _.Next = (at time ((at heap 0)))
        member _.Push(t: float, sq: int, k: int, actor: int, epoch: int, va: int, vb: int, vx: float, vy: float) =
            if freeCount = 0 then
                let n = time.Length
                time <- growFloats time
                seq <- growInts seq
                kind <- growInts kind
                who <- growInts who
                tag <- growInts tag
                a <- growInts a
                b <- growInts b
                x <- growFloats x
                y <- growFloats y
                heap <- growInts heap
                free <- growInts free
                for i in 0 .. n - 1 do
                    put free i (n + i)
                freeCount <- n
            freeCount <- freeCount - 1
            let slot = (at free freeCount)
            put time slot (t)
            put seq slot (sq)
            put kind slot (k)
            put who slot (actor)
            put tag slot (epoch)
            put a slot (va)
            put b slot (vb)
            put x slot (vx)
            put y slot (vy)
            let mutable i = count
            count <- count + 1
            put heap i (slot)
            let mutable go = true
            while go && i > 0 do
                let parent = (i - 1) / 2
                if before (at heap i) (at heap parent) then
                    let h = (at heap i)
                    put heap i ((at heap parent))
                    put heap parent (h)
                    i <- parent
                else go <- false
        member this.Pop() =
            let slot = (at heap 0)
            this.Time <- (at time slot)
            this.Seq <- (at seq slot)
            this.Kind <- (at kind slot)
            this.Who <- (at who slot)
            this.Tag <- (at tag slot)
            this.A <- (at a slot)
            this.B <- (at b slot)
            this.X <- (at x slot)
            this.Y <- (at y slot)
            put free freeCount (slot)
            freeCount <- freeCount + 1
            count <- count - 1
            if count > 0 then
                put heap 0 ((at heap count))
                let mutable i = 0
                let mutable go = true
                while go do
                    let l = 2 * i + 1
                    let r = l + 1
                    let mutable least = i
                    if l < count && before (at heap l) (at heap least) then least <- l
                    if r < count && before (at heap r) (at heap least) then least <- r
                    if least <> i then
                        let h = (at heap i)
                        put heap i ((at heap least))
                        put heap least (h)
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
          /// The enemy of the fight's own that it is or stands in for (its place among them),
          /// and its stream of random numbers for its attacks, with how many it has served.
          Root: int
          AttackKey: uint32
          mutable AttackN: int
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
          /// Flammability left by fire hits, oldest first from `FlamFrom` up to `FlamTo`: how
          /// much, and when it is gone (each lasts the same, so the oldest goes first).
          mutable FlamValue: float[]
          mutable FlamUntil: float[]
          mutable FlamFrom: int
          mutable FlamTo: int
          /// What they add up to (kept as they come and go: the same as their sum but for rounding).
          mutable FlamSum: float
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
          /// The statuses the map `Status` was made for.
          mutable MapHas: bool[]
          mutable MapStacks: int[]
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

    /// The player or a minion during a run (`Sim.ActorState`): its buffs, what is kept of
    /// its skills' answers, its cooldowns and rolls, and a minion's body.
    type private Agent =
        { // its buffs, by buff
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
          /// Counts the changes to which buffs there are and their stacks.
          mutable BuffsVersion: int
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
          Dbs: ModDb[]
          DbVersion: int[]
          Cooldowns: float[]
          Recharges: float list[]
          mutable ArchonRecovery: float
          /// By meta gem: when its invocation can be used again.
          InvocationReady: float[]
          /// The counters the reference keeps by name (Glory, the place in a chain, ...), with
          /// the meta gems' energy as it was when last asked for by name (see `energyMap`).
          mutable Energy: Map<string, float>
          /// By meta gem: its energy, and whether it has had any yet.
          Held: float[]
          HeldSet: bool[]
          mutable HeldStale: bool
          mutable LastUsed: int
          CritRoll: float[]
          HasCritRoll: bool[]
          Rolled: int[]
          StrikeStream: int[]
          ConsumeStream: int[]
          PeriodicStream: int[]
          mutable RageAt: float
          mutable RageCarry: float
          /// By skill: how many of a part that waits to be set off by another skill wait.
          Waiting: int[]
          /// By crossbow skill: the bolts left (-1: a full load), and whether its load has
          /// not been paid for yet (the cost is paid with the first shot of a load).
          Ammo: int[]
          LoadDue: bool[]
          /// By sealed skill: when its seals were last broken (NaN: never).
          SealBreak: float[]
          /// By skill: how many of its strikes are on their way (in the queue), and a count of
          /// the times those were called off (a warp into the projectile: it is gone).
          InFlight: int[]
          Flight: int[]
          /// A minion's life and energy shield, when it was last hit, whether it is out of the
          /// fight (killed, or not summoned), and when a temporary one's time is up.
          mutable Health: float
          mutable Barrier: float
          mutable HitAt: float
          mutable Gone: bool
          mutable Leaves: float
          /// Counts the times it has left the fight: what it had begun before (its turn, its
          /// strikes still waiting in the queue) is lost, where the reference takes them out
          /// of the queue.
          mutable Epoch: int
          /// Whether its bones have been used since it was killed (`Sim.World.Boned`).
          mutable Boned: bool
          /// Its evasion entropy.
          mutable Dodge: float
          mutable DodgeAt: float
          mutable HasDodge: bool }

    type private State =
        { /// The actor that acts now, its program and its place; all of them.
          mutable P: Program
          mutable A: Agent
          mutable Cur: int
          Progs: Program[]
          Agents: Agent[]
          /// The player's program.
          Main: Program
          Seed: uint32
          Rng: Rng
          Queue: Queue
          Tell: (float -> string -> string -> string -> float -> unit) option
          mutable Time: float
          mutable Seq: int
          /// Every enemy there has been, by its number; those still in the fight, in the order of
          /// their names (the dead stay in it until they are replaced, if they are); the numbers by name.
          mutable Foes: Foe[]
          mutable Live: int[]
          mutable FoeIds: Dictionary<string, int>
          mutable AliveCount: int
          /// The living enemies' definitions in the order of their names, and the one aimed at
          /// (-1: none), good until one dies.
          mutable Living: EnemyDef list
          mutable Aimed: int
          mutable AliveStale: bool
          Kills: ResizeArray<string * Rarity * float>
          /// Corpses already used up by corpse skills (every kill leaves one).
          mutable CorpsesUsed: int
          /// Seeds planted in enemies: owner, skill, enemy, the enemy's life when planted, the
          /// damage it takes to set the seed off, when it fizzles. Newest first.
          mutable Seeds: (int * int * int * float * float * float) list
          /// Pustules on enemies: owner, part, enemy, the Poison damage dealt when it was
          /// attached, Poison per 1% more damage, the most (%), when it bursts at the latest.
          mutable Pustules: (int * int * int * float * float * float * float) list
          /// Ice Crystals beside an enemy: owner, explosion part, enemy, the enemy's life when
          /// they appeared, life of one crystal, how many, gone at. Newest first.
          mutable Crystals: (int * int * int * float * float * int * float) list
          /// Crystals on their way (what a `EPlant` event names by its place here): how many,
          /// the life of one, the most at once, the seconds they last.
          Plants: ResizeArray<int * float * int * float>
          /// Killed minions are revived at this time; how many have died since the last revival.
          mutable Revival: (float * int) option
          /// By actor: whether it is a melee minion that finds no room at the enemy `RoomAt`
          /// (see `crowdedOut`; -2: to be worked out anew, a minion has come or gone).
          NoRoom: bool[]
          mutable RoomAt: int
          Evs: ResizeArray<Ev>
          /// Scratch for working out an enemy's statuses (`refreshStatus`).
          StatusHas: bool[]
          StatusStacks: int[]
          AilmentSeen: bool[]
          NoHas: bool[]
          mutable EvDepth: int
          /// The enemy that is the cause of what happens now (the one just hit, the one whose
          /// hit was blocked; -1: none), and the enemy a triggered skill is aimed at while it starts.
          mutable Cause: int
          mutable Aim: int
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
          /// What a hit that takes a state off an enemy still reads of the enemy as it was:
          /// its shock, and how far its armour was broken.
          mutable ShockWas: float
          mutable BrokenWas: float
          /// Infernal Flame built by what skills cost (Pyromantic Pact).
          mutable InfernalFlame: float
          /// When a stun last made a Runic Ward remnant, and a critical hit a mana remnant.
          mutable LastStunRemnant: float
          mutable LastManaRemnant: float
          /// Damage over time on the player: (until, per second, type).
          mutable Burning: (float * float * DamageType) list
          /// Charges per charm, by its buff.
          CharmCharges: float[]
          /// The player's evasion entropy.
          mutable PEntropy: float
          mutable PEntropyAt: float
          mutable PHasEntropy: bool
          /// The defences for the sets of buffs they were asked for (`Actor.VitalsAt`), by the
          /// buffs written out; and the last set asked for, with its defences.
          VitalsSeen: Dictionary<string, PlayerStats>
          mutable VitalsOf: int
          mutable VitalsLast: PlayerStats option
          /// Mana gained: regen, leech, flask, recoup, hit, kill, consume.
          Gained: float[]
          GainedSeen: bool[] }

    let private gainedNames = [| "regen"; "leech"; "flask"; "recoup"; "hit"; "kill"; "consume" |]

    /// What tells an event that still counts from one that was lost: the time in the fight
    /// its actor is in and, of a strike on its way, which of the skill's flights it belongs
    /// to (see `Agent.Epoch`, `Agent.Flight`). A strike that is put in the queue is counted
    /// as on its way.
    let private stamp (agent: Agent) (kind: int) (skill: int) : int =
        if kind = EStrike || kind = EPassing then
            put agent.InFlight skill ((at agent.InFlight skill) + 1)
            agent.Epoch * 65536 + (at agent.Flight skill)
        else agent.Epoch * 65536

    /// An event of the actor that acts now.
    let private schedule (s: State) (t: float) kind a b x y =
        s.Queue.Push(t, s.Seq, kind, s.Cur, stamp s.A kind a, a, b, x, y)
        s.Seq <- s.Seq + 1

    /// An event of another actor.
    let private scheduleFor (s: State) (who: int) (t: float) kind a b x y =
        s.Queue.Push(t, s.Seq, kind, who, stamp (at s.Agents who) kind a, a, b, x, y)
        s.Seq <- s.Seq + 1

    /// From here on this actor acts: its buffs are the buffs, its program the program.
    let private enter (s: State) (who: int) =
        if s.Cur <> who then
            s.Cur <- who
            s.A <- at s.Agents who
            s.P <- at s.Progs who

    let private expireAt (s: State) (t: float) = schedule s t EExpire 0 0 0.0 0.0

    let inline private bump (s: State) (count: int) = s.Counts.[count] <- (at s.Counts count) + 1

    let private addDamage (s: State) (source: int) (v: float) =
        put s.Damage source ((at s.Damage source) + v)
        put s.DamageSeen source (true)

    let private addByType (s: State) (t: int) (v: float) =
        put s.ByType t ((at s.ByType t) + v)
        put s.ByTypeSeen t (true)

    let private addGained (s: State) (slot: int) (v: float) =
        put s.Gained slot ((at s.Gained slot) + v)
        put s.GainedSeen slot (true)

    // ---- the player's buffs ----

    /// Before a buff comes, goes or changes its stacks: if a hit (or a use) still asks its
    /// conditions of the buffs as they were, they stay as they were for it.
    let private ownBuffs (s: State) =
        if s.A.BKept then
            if s.EvDepth > 0 then
                s.A.BHas <- Array.copy s.A.BHas
                s.A.BStacks <- Array.copy s.A.BStacks
            s.A.BKept <- false

    let private setBuff (s: State) (id: int) (stacks: int) (expires: float) (mods: Mod list) =
        let had = (at s.A.BHas id)
        if not had || (at s.A.BStacks id) <> stacks then
            ownBuffs s
            s.A.BuffMapStale <- true
            s.A.BChanged.Add id
            s.A.BuffsVersion <- s.A.BuffsVersion + 1
        if not (had && (at s.A.BStacks id) = stacks && obj.ReferenceEquals((at s.A.BMods id), mods)) then
            if not mods.IsEmpty || (had && not (at s.A.BMods id).IsEmpty) || id = s.P.BRage then s.A.ModsVersion <- s.A.ModsVersion + 1
        put s.A.BHas id (true)
        put s.A.BStacks id (stacks)
        put s.A.BExpires id (expires)
        put s.A.BMods id (mods)
        if not (expires >= s.A.BSoonest) then s.A.BSoonest <- expires

    let private removeBuff (s: State) (id: int) =
        if (at s.A.BHas id) then
            ownBuffs s
            s.A.BuffMapStale <- true
            s.A.BChanged.Add id
            s.A.BuffsVersion <- s.A.BuffsVersion + 1
            if not (at s.A.BMods id).IsEmpty || id = s.P.BRage then s.A.ModsVersion <- s.A.ModsVersion + 1
            put s.A.BHas id (false)
            put s.A.BStacks id (0)
            put s.A.BMods id ([])

    /// A buff's stacks, whether its time is up or not (`Sim.stacksOf`).
    let inline private stacksOf (s: State) (id: int) = if (at s.A.BHas id) then (at s.A.BStacks id) else 0

    /// Whether a buff has stacks and time left.
    let inline private held (s: State) (id: int) = (at s.A.BHas id) && (at s.A.BStacks id) > 0 && (at s.A.BExpires id) > s.Time

    let private buffMap (s: State) : Map<string, int> =
        if s.A.BuffMapStale then
            // (only the buffs that changed since it was last asked for)
            let mutable m = s.A.BuffMap
            for id in s.A.BChanged do
                m <- (if (at s.A.BHas id) then Map.add (at s.P.BuffNames id) (at s.A.BStacks id) m else Map.remove (at s.P.BuffNames id) m)
            s.A.BChanged.Clear()
            s.A.BuffMap <- m
            s.A.BuffMapStale <- false
        s.A.BuffMap

    /// Buff stacks as a skill sees them (`Sim.skillStacks`).
    let private skillStacks (s: State) (sid: int) : Map<string, int> =
        let stacks = buffMap s
        let info = (at s.P.Skills sid)
        if info.StackExtra > 0 then
            match Map.tryFind info.StackCharge stacks with
            | Some n when n > 0 -> Map.add info.StackCharge (n + info.StackExtra) stacks
            | _ -> stacks
        else stacks

    /// The player's mods while using a skill (`Sim.skillDb`): made the way the reference
    /// makes them, so that every sum over them adds in the same order.
    let private skillDb (s: State) (sid: int) : ModDb =
        if (at s.A.DbVersion sid) = s.A.ModsVersion then (at s.A.Dbs sid)
        else
            let p = s.P
            let buffMods =
                [ for id in p.BuffOrder do
                    if (at s.A.BHas id) && not (at s.A.BMods id).IsEmpty then
                        let k = float (at s.A.BStacks id)
                        for m in (at s.A.BMods id) -> Mod.scaled k m ]
            let db = ModDb.addList buffMods (at p.Skills sid).Db
            let db =
                if (at s.A.BHas p.BRage) && (at s.A.BStacks p.BRage) > 0 then
                    let effect = Mods.mult Ctx.empty [ Named "RageEffect" ] db
                    let more = floor (float (at s.A.BStacks p.BRage) * effect) * Constants.moreAttackDamagePerRage
                    ModDb.add db (Mod.more (Damage AllDamage) more |> Mod.whenever (SkillIs Attack) |> Mod.from "Rage")
                else db
            put s.A.Dbs sid (db)
            put s.A.DbVersion sid (s.A.ModsVersion)
            db

    // ---- enemies ----

    let private setDebuff (e: Foe) (id: int) (stacks: int) (expires: float) (mods: Mod list) =
        let had = (at e.Has id)
        if not had || (at e.Stacks id) <> stacks then e.StatusStale <- true
        if not (had && (at e.Stacks id) = stacks && (obj.ReferenceEquals((at e.DMods id), mods) || (at e.DMods id) = mods)) then
            if not mods.IsEmpty || (had && not (at e.DMods id).IsEmpty) then
                e.DefenderStale <- true
                e.DefVersion <- e.DefVersion + 1
        put e.Has id (true)
        put e.Stacks id (stacks)
        put e.Expires id (expires)
        put e.DMods id (mods)
        if not (expires >= e.Soonest) then e.Soonest <- expires

    let private removeDebuff (e: Foe) (id: int) =
        if (at e.Has id) then
            e.StatusStale <- true
            if not (at e.DMods id).IsEmpty then
                e.DefenderStale <- true
                e.DefVersion <- e.DefVersion + 1
            put e.Has id (false)
            put e.Stacks id (0)
            put e.DMods id ([])

    /// A debuff comes or stacks, for so long (`Sim.addDebuff`).
    let private addDebuff (s: State) (e: Foe) (id: int) (duration: float) (maxStacks: int) (mods: Mod list) =
        let stacks = if (at e.Has id) && (at e.Expires id) > s.Time then min maxStacks ((at e.Stacks id) + 1) else 1
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
            for i__18 in 0 .. dots.Length - 1 do
                let d = at dots i__18
                let mutable g = -1
                for i in 0 .. count - 1 do
                    if g < 0 && (at labels i) = d.Label then g <- i
                if g < 0 then
                    if count = labels.Length then
                        labels <- growInts labels
                        limits <- growInts limits
                    put labels count (d.Label)
                    put limits count (d.Most)
                    count <- count + 1
                elif d.Most > limits.[g] then limits.[g] <- d.Most
            // of each label the strongest so many, strongest first, of equals the one that came first
            let mutable active : FDot list = []
            for g in count - 1 .. -1 .. 0 do
                let limit = (at limits g)
                if limit > 0 then
                    let label = (at labels g)
                    let mutable top : FDot[] = Array.zeroCreate (min limit 16)
                    let mutable n = 0
                    for i__19 in 0 .. dots.Length - 1 do
                        let d = at dots i__19
                        if d.Label = label && (n < limit || d.Dps > (at top (n - 1)).Dps) then
                            let mutable at = n
                            while at > 0 && top.[at - 1].Dps < d.Dps do
                                at <- at - 1
                            if n < limit then
                                if n = top.Length then
                                    let wider = Array.zeroCreate (top.Length * 2)
                                    for i in 0 .. n - 1 do
                                        put wider i (top.[i])
                                    top <- wider
                                n <- n + 1
                            for i in n - 1 .. -1 .. at + 1 do
                                put top i (top.[i - 1])
                            put top at (d)
                    for i in n - 1 .. -1 .. 0 do
                        active <- (at top i) :: active
            e.Active <- active
            e.ActiveOf <- e.Dots
            active

    /// The time an enemy's statuses have lasted goes into the results. (It is kept back while
    /// the statuses stay the same: one addition per status when they change, not one per
    /// event. The reference adds event by event; the sums are the same but for rounding.)
    let private flushUptime (s: State) (e: Foe) =
        if e.UpAny then
            for i__20 in 0 .. e.StatusKeys.Length - 1 do
                let k = at e.StatusKeys i__20
                put s.Uptime k ((at s.Uptime k) + e.UpPending)
                put s.UptimeSeen k (true)
            e.UpPending <- 0.0
            e.UpAny <- false

    /// The same for what the damage over time on an enemy has dealt.
    let private flushDots (s: State) (e: Foe) =
        if e.RateAny then
            for i in 0 .. e.Rates.Length - 1 do
                addDamage s (at e.RateLabels i) ((at e.Rates i) * e.RatePending)
            for i in 0 .. e.Rates.Length - 1 do
                addByType s (at e.RateTypes i) ((at e.Rates i) * e.RatePending)
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
            // (worked out in two arrays of the run's own, and only made anew for the enemy
            // where they say something else than it had)
            let count = e.Has.Length
            let has, stacks = s.StatusHas, s.StatusStacks
            let mutable cursed = false
            let mutable marked = false
            let mutable exposed = false
            for id in 0 .. count - 1 do
                let on = at e.Has id
                put has id on
                put stacks id (at e.Stacks id)
                if on then
                    if (at p.StatusCurse id) then cursed <- true
                    if (at p.StatusMark id) then marked <- true
                    if (at p.StatusExposure id) then exposed <- true
            // the ailments that deal damage: as many stacks as count
            let counted = s.AilmentSeen
            for ailment in 0 .. counted.Length - 1 do
                put counted ailment false
            let mutable dots = activeDots e
            while not dots.IsEmpty do
                match dots.Head.Ailing with
                | Some a ->
                    let ailment = ailmentIndex a
                    let id = at p.AilmentStatus ailment
                    if not (at counted ailment) then
                        put counted ailment true
                        put has id true
                        put stacks id 0
                    put stacks id ((at stacks id) + 1)
                | None -> ()
                dots <- dots.Tail
            let ailments = p.AilmentStatus
            // (Ignite 0, Shock 3, Chill 4, Freeze 5, Electrocute 6, Heavy Stun 7, Pin 8: see `ailmentIndex`)
            let mutable ailing = false
            for ailment in 0 .. ailments.Length - 1 do
                if (at has (at ailments ailment)) then ailing <- true
            if def.Moving then
                put has (at p.SDerived 0) true
                put stacks (at p.SDerived 0) 1
            if cullable then
                put has (at p.SDerived 1) true
                put stacks (at p.SDerived 1) 1
            if ailing then
                put has (at p.SDerived 2) true
                put stacks (at p.SDerived 2) 1
            if cursed then
                put has (at p.SDerived 3) true
                put stacks (at p.SDerived 3) 1
            if marked then
                put has (at p.SDerived 4) true
                put stacks (at p.SDerived 4) 1
            if (at has (at ailments 5)) || (at has (at ailments 7)) || (at has (at ailments 6)) || (at has (at ailments 8)) then
                put has (at p.SDerived 5) true
                put stacks (at p.SDerived 5) 1
            (let n =
                (if (at has (at ailments 0)) then 1 else 0) + (if (at has (at ailments 4)) then 1 else 0) + (if (at has (at ailments 5)) then 1 else 0)
                + (if (at has (at ailments 3)) then 1 else 0) + (if (at has (at ailments 6)) then 1 else 0)
             if n > 0 then
                put has (at p.SDerived 6) true
                put stacks (at p.SDerived 6) n)
            if exposed then
                put has (at p.SDerived 7) true
                put stacks (at p.SDerived 7) 1
            if def.Rarity = Rare || def.Rarity = Unique then
                put has (at p.SDerived 8) true
                put stacks (at p.SDerived 8) 1
            if low then
                put has (at p.SDerived 9) true
                put stacks (at p.SDerived 9) 1
            if full then
                put has (at p.SDerived 10) true
                put stacks (at p.SDerived 10) 1
            // (the same statuses as before: the enemy's arrays stay, and with them what was worked out for them)
            let mutable same = e.SHas.Length = count
            if same then
                for id in 0 .. count - 1 do
                    if (at has id) <> (at e.SHas id) || (at stacks id) <> (at e.SStacks id) then same <- false
            if not same then
                flushUptime s e
                let mutable held = 0
                for id in 0 .. count - 1 do
                    if (at has id) then held <- held + 1
                let keys = Array.zeroCreate held
                let mutable k = 0
                for id in 0 .. count - 1 do
                    if (at has id) then
                        put keys k (id)
                        k <- k + 1
                e.SHas <- Array.sub has 0 count
                e.SStacks <- Array.sub stacks 0 count
                e.StatusKeys <- keys
                e.StatusMapStale <- true
            e.StatusStale <- false
            e.StatusDots <- e.Dots
            e.WasCullable <- cullable
            e.WasLow <- low
            e.WasFull <- full

    /// An enemy's statuses as the map the reference has (for what reads them by name).
    /// Kept from one asking to the next: only what has changed since is put in or taken out.
    let private status (s: State) (e: Foe) : Map<string, int> =
        refreshStatus s e
        if e.StatusMapStale then
            let names = s.P.StatusNames
            let was, wasStacks = e.MapHas, e.MapStacks
            let mutable m = e.Status
            if was.Length <> e.SHas.Length then
                m <- Map.empty
                for k in 0 .. e.StatusKeys.Length - 1 do
                    let id = at e.StatusKeys k
                    m <- Map.add (at names id) (at e.SStacks id) m
            else
                for id in 0 .. was.Length - 1 do
                    if (at e.SHas id) then
                        if not (at was id) || (at wasStacks id) <> (at e.SStacks id) then m <- Map.add (at names id) (at e.SStacks id) m
                    elif (at was id) then m <- Map.remove (at names id) m
            e.Status <- m
            e.MapHas <- e.SHas
            e.MapStacks <- e.SStacks
            e.StatusMapStale <- false
        e.Status

    /// An enemy as a defender: its own mods and its debuffs' (`Sim.defender`).
    let private defender (s: State) (e: Foe) : Defender =
        if e.DefenderStale then
            let debuffMods =
                [ for id in s.P.StatusOrder do
                    if (at e.Has id) && not (at e.DMods id).IsEmpty then
                        let k = float (at e.Stacks id)
                        for m in (at e.DMods id) -> Mod.scaled k m ]
            e.Defender <- { Level = e.Info.Def.Level; Mods = ModDb.addList debuffMods e.Info.Db }
            e.DefenderStale <- false
        e.Defender

    let private takenAll = DamageTaken AllDamage
    let private takenElemental = DamageTaken Elemental
    let private takenOf = damageTypes |> Array.map (fun t -> DamageTaken(OfType t))
    let private resistOf = damageTypes |> Array.map Resist
    let private maxResistOf = damageTypes |> Array.map MaxResist
    let private takenStats = [| for t in 0 .. 4 -> (if t >= 1 && t <= 3 then [| takenAll; (at takenOf t); takenElemental |] else [| takenAll; (at takenOf t) |]) |]
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
                if (at e.Has id) && not (at e.DMods id).IsEmpty then n <- n + List.length (at e.DMods id)
            if e.DStat.Length < n then
                e.DStat <- Array.zeroCreate (n + 8)
                e.DCode <- Array.zeroCreate (n + 8)
                e.DName <- Array.zeroCreate (n + 8)
                e.DKind <- Array.zeroCreate (n + 8)
                e.DVal <- Array.zeroCreate (n + 8)
                e.DShock <- Array.zeroCreate (n + 8)
            let mutable at = 0
            let put (stat: Stat) (kind: ModKind) (v: float) (shock: bool) =
                put e.DStat at (stat)
                (let c = StatKey.code stat
                 put e.DCode at (c)
                 put e.DName at ((if c < 0 then StatKey.name stat else "")))
                put e.DKind at (ModKind.number kind)
                put e.DVal at (v)
                put e.DShock at (shock)
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
                if (at e.DCode i) = c && (at e.DKind i) = k then acc <- acc + (at e.DVal i)
        else
            let name = StatKey.name stat
            for i in 0 .. e.DCount - 1 do
                if (at e.DCode i) < 0 && (at e.DKind i) = k && (at e.DName i) = name then acc <- acc + (at e.DVal i)
        acc

    let private dmore (e: Foe) (stat: Stat) (acc: float) : float =
        let mutable acc = acc
        let c = StatKey.code stat
        if c >= 0 then
            for i in 0 .. e.DCount - 1 do
                if (at e.DCode i) = c && (at e.DKind i) = 2 then acc <- acc * (1.0 + (at e.DVal i) / 100.0)
        else
            let name = StatKey.name stat
            for i in 0 .. e.DCount - 1 do
                if (at e.DCode i) < 0 && (at e.DKind i) = 2 && (at e.DName i) = name then acc <- acc * (1.0 + (at e.DVal i) / 100.0)
        acc

    /// increased x more of the defender's mods of these stats (`Mods.mult` with `Ctx.empty`).
    let private dmult (e: Foe) (stats: Stat[]) : float =
        let mutable inc = 0.0
        let mutable more = 1.0
        for i__24 in 0 .. stats.Length - 1 do
            let stat = at stats i__24
            inc <- dsum e Inc stat inc
            more <- dmore e stat more
        max 0.0 ((1.0 + inc / 100.0) * more)

    let private actionSpeedStat = Named "ActionSpeed"
    let private damageDealtStat = Named "DamageDealt"

    let private kill (s: State) (e: Foe) (at: float) =
        e.Life <- 0.0
        s.Kills.Add((e.Def.Name, e.Def.Rarity, at))
        s.AliveCount <- s.AliveCount - 1
        s.AliveStale <- true

    let private refreshAlive (s: State) =
        if s.AliveStale then
            let mutable living : EnemyDef list = []
            for k in s.Live.Length - 1 .. -1 .. 0 do
                let e = at s.Foes (at s.Live k)
                if e.Life > 0.0 then living <- e.Def :: living
            s.Living <- living
            s.Aimed <-
                match Geometry.aim s.P.Fight.Targeting s.Living with
                | Some d -> s.FoeIds.[d.Name]
                | None -> -1
            s.AliveStale <- false

    let private living (s: State) : EnemyDef list =
        refreshAlive s
        s.Living

    /// The enemy aimed at (`Sim.aimed`): a triggered skill "usually targets the cause of the trigger".
    let private aimed (s: State) : int =
        refreshAlive s
        if s.Aim < 0 then s.Aimed
        elif (at s.Foes s.Aim).Life > 0.0 then s.Aim
        elif s.AliveCount > 0 then
            // (the cause died of the hit: the skill lands where it was, on whoever is nearest to there)
            let spot = (at s.Foes s.Aim).Def.Position
            let mutable best = -1
            let mutable least = infinity
            for k in 0 .. s.Live.Length - 1 do
                let e = at s.Foes (at s.Live k)
                if e.Life > 0.0 then
                    let d = Vec.dist e.Def.Position spot
                    if best < 0 || d < least then
                        best <- e.Index
                        least <- d
            best
        else s.Aimed

    /// Whether an enemy cannot act: frozen, heavily stunned or electrocuted (`pinned`: or pinned).
    let private stopped (s: State) (e: Foe) (pinned: bool) : bool =
        refreshStatus s e
        let ailments = s.P.AilmentStatus
        (at e.SHas (at ailments 5)) || (at e.SHas (at ailments 7)) || (at e.SHas (at ailments 6)) || (pinned && (at e.SHas (at ailments 8)))

    // ---- time passing ----

    /// The clock goes to t: damage over time, and how long each status lasted (`Sim.advance`).
    let private advance (s: State) (t: float) =
        let dt = t - s.Time
        if dt > 0.0 then
            let n = float s.AliveCount
            for i__26 in 0 .. s.Live.Length - 1 do
                let e = at s.Foes (at s.Live i__26)
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
                                    put e.DotTaken t (nan)
                                e.DotTakenVersion <- e.DefVersion
                            let bleedK = if e.Def.Moving then Constants.bleedMovingMultiplier else 1.0
                            let m = List.length active
                            let rates, labels, types = Array.zeroCreate m, Array.zeroCreate m, Array.zeroCreate m
                            let mutable i = 0
                            let mutable sum = 0.0
                            for d in List.toArray active do
                                let k = if d.Ailing = Some Bleed then bleedK else 1.0
                                let t = typeIndex d.Of
                                if System.Double.IsNaN (at e.DotTaken t) then
                                    // (`Ailments.dotTakenMult`: bleeding ignores armour)
                                    let resist =
                                        if t = 0 then 0.0
                                        else
                                            dsum e Base (at resistOf t) 0.0
                                            |> min (min Constants.resistCeiling (Constants.enemyBaseMaxResist + dsum e Base (at maxResistOf t) 0.0))
                                            |> max Constants.resistFloor
                                    put e.DotTaken t ((1.0 - resist / 100.0) * dmult e (at takenStats t) * dmult e dotTakenStats)
                                put rates i (d.Dps * k * (at e.DotTaken t))
                                put labels i (d.Label)
                                put types i (t)
                                sum <- sum + (at rates i)
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
            if s.P.Walks then
                for i__27 in 0 .. s.Live.Length - 1 do
                    let e = at s.Foes (at s.Live i__27)
                    match e.Def.Attack with
                    | Some atk when e.Life > 0.0 ->
                        let d = Vec.length e.Def.Position
                        let gap = d - e.Def.Size - atk.Range
                        // (one that has arrived walks no further: its statuses are not asked)
                        if not (gap <= 0.0) && not (stopped s e true) then
                            let ailments = s.P.AilmentStatus
                            let slow =
                                (if (at e.SHas (at ailments 10)) then 1.0 - Constants.maimSlow / 100.0 else 1.0)
                                * (if (at e.SHas (at ailments 12)) then 1.0 - Constants.hinderSlow / 100.0 else 1.0)
                                * (1.0 - (if (at e.Has s.P.SChilled) then (at e.DMods s.P.SChilled) |> List.sumBy (fun m -> m.Value) else 0.0) / 100.0)
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

    /// What the recovery streams of a pool give from `t0` to now (recoup, leech: see `recover`).
    let private recouped (s: State) (pool: string) (t0: float) : float =
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
                    let part = rate * max 0.0 (min s.Time until - t0)
                    acc <- (if leech then (if any then max acc part else part) else acc + part)
                    any <- true
                rest <- rest.Tail
            acc

    /// The player's recovery and the damage over time on it during dt, ending now (`Sim.recover`).
    let private recover (s: State) (dt: float) =
        match s.P.Vitals with
        | Some _ when dt > 0.0 && s.P.Charms |> Array.exists (fun c -> c.Charm.ChargesPerSecond > 0.0) ->
            for i__28 in 0 .. s.P.Charms.Length - 1 do
                let c = at s.P.Charms i__28
                put s.CharmCharges c.CBuff (min c.Charm.MaxCharges ((at s.CharmCharges c.CBuff) + c.Charm.ChargesPerSecond * c.Charm.ChargeGainMult * dt))
        | _ -> ()
        match s.P.Vitals with
        | Some _ when dt > 0.0 ->
            match s.P.Actor.ManaFlask with
            | Some f -> s.FlaskCharges <- min f.MaxCharges (s.FlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
            | None -> ()
        | _ -> ()
        match s.P.Vitals with
        | Some v when dt > 0.0 && alive s ->
            let t0 = s.Time - dt
            let stream (pool: string) = recouped s pool t0
            let leech = stream "LeechMana"
            let regen = v.ManaRegen * dt
            let mana = min v.Mana (s.Mana + regen + leech)
            let es =
                (if s.Time - s.LastHit >= v.ESRechargeDelay then s.Shield + v.EnergyShieldRecharge * dt else s.Shield)
                + v.ESRegen * dt + stream "ES" + stream "ShroudES"
                |> min (max s.Shield v.EnergyShield)
            let flask = s.FlaskRate * max 0.0 (min s.Time s.FlaskUntil - t0)
            let recouped = stream "Mana"
            let mana = min v.Mana (mana + flask + recouped)
            addGained s 0 (min regen (v.Mana - s.Mana))
            addGained s 1 leech
            addGained s 2 flask
            if recouped > 0.0 then addGained s 3 recouped
            let lifeLeech = stream "LeechLife"
            let lifeRecharge = if v.LifeRecharge > 0.0 && s.Time - s.LastHit >= v.ESRechargeDelay then v.LifeRecharge * dt else 0.0
            let lifeGain = v.LifeRegen * dt + lifeLeech + lifeRecharge + s.LifeFlaskRate * max 0.0 (min s.Time s.LifeFlaskUntil - t0) + stream "Life"
            let lifeCharges =
                match v.LifeFlask with
                | Some f -> min f.MaxCharges (s.LifeFlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
                | None -> s.LifeFlaskCharges
            s.Mana <- mana
            s.Shield <- es
            s.Life <- min (max s.Life v.Life) (s.Life + lifeGain)
            s.LifeFlaskCharges <- lifeCharges
            s.Ward <- (if s.Ward >= v.Ward then s.Ward else min v.Ward (s.Ward + v.WardRegen * dt))
            if not s.Recoups.IsEmpty && s.Recoups |> List.exists (fun (until, _, _) -> not (until > s.Time)) then
                s.Recoups <- s.Recoups |> List.filter (fun (until, _, _) -> until > s.Time)
            if not s.Burning.IsEmpty then
                let dots =
                    s.Burning
                    |> List.fold (fun (m: Map<DamageType, float>) (until, dps, t) -> Map.add t ((Map.tryFind t m |> Option.defaultValue 0.0) + dps * max 0.0 (min s.Time until - t0)) m) Map.empty
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

    /// What has run out of the enemies' debuffs and damage over time goes (`Sim.prune`).
    let private pruneFoes (s: State) =
        for i__29 in 0 .. s.Live.Length - 1 do
            let e = at s.Foes (at s.Live i__29)
            if not (e.Soonest > s.Time) then
                let mutable soonest = infinity
                for id in 0 .. e.Has.Length - 1 do
                    if (at e.Has id) then
                        if (at e.Expires id) > s.Time then (if (at e.Expires id) < soonest then soonest <- (at e.Expires id))
                        else removeDebuff e id
                if e.Dots |> List.exists (fun d -> not (d.Until > s.Time)) then
                    e.Dots <- e.Dots |> List.filter (fun d -> d.Until > s.Time)
                for d in List.toArray e.Dots do
                    if d.Until < soonest then soonest <- d.Until
                e.Soonest <- soonest

    /// What has run out of the buffs of the actor that acts now goes (`Sim.prune`).
    let private prune (s: State) =
        let p = s.P
        if not (s.A.BSoonest > s.Time) then
            let mutable soonest = infinity
            for id in 0 .. s.A.BHas.Length - 1 do
                if (at s.A.BHas id) then
                    if (at s.A.BExpires id) > s.Time then (if (at s.A.BExpires id) < soonest then soonest <- (at s.A.BExpires id))
                    else
                        if (at s.A.BExpires id) < infinity && (at p.CExpired id) >= 0 then bump s (at p.CExpired id)
                        removeBuff s id
            s.A.BSoonest <- soonest

    // ---- effects ----

    /// A buff comes or stacks, for so long (`Sim.stackEffect` on the actor's buffs).
    let private grantBuff (s: State) (id: int) (duration: float) (maxStacks: int) (mods: Mod list) =
        let stacks = if (at s.A.BHas id) && (at s.A.BExpires id) > s.Time then min maxStacks ((at s.A.BStacks id) + 1) else 1
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
        let d = (at s.P.ResourceSeconds id)
        let lasts = not (System.Double.IsNaN d)
        if n <= 0 then removeBuff s id
        else
            let expires =
                if lasts && refresh then s.Time + d
                elif (at s.A.BHas id) then (at s.A.BExpires id)
                elif lasts then s.Time + d
                else infinity
            setBuff s id n expires []
        if lasts && refresh && n > 0 then expireAt s (s.Time + d)

    /// The player gains rage, up to the maximum; the loss timer starts again (`Sim.gainRage`).
    let private gainRage (s: State) (amount: float) =
        let p = s.P
        if not (amount <= 0.0 || p.MaxRage <= 0) then
            setStacks s false p.BRage (min p.MaxRage (stacksOf s p.BRage + int amount))
            s.A.RageAt <- s.Time

    /// Something that uses charms happened to the player. A charm for it that is running, or
    /// has the charges to be used now, answers; whether the occurrence is prevented (`Sim.charm`).
    let private charm (s: State) (key: string) : bool =
        let p = s.Main
        match p.Vitals with
        | Some pools when p.Charms.Length > 0 ->
            // (the charms and their buffs are the player's, whoever it is that acts)
            let was = s.Cur
            enter s 0
            // (which of them run is seen before any of them is used)
            let running = p.Charms |> Array.map (fun c -> (at s.A.BHas c.CBuff) && (at s.A.BExpires c.CBuff) > s.Time)
            let mutable prevented = false
            for i in 0 .. p.Charms.Length - 1 do
                let c = (at p.Charms i)
                if c.Charm.Trigger = key then
                    let charges = (at s.CharmCharges c.CBuff)
                    if (at running i) then prevented <- prevented || c.Charm.Prevents
                    elif charges >= c.Charm.ChargesPerUse && c.Charm.ChargesPerUse > 0.0 then
                        setBuff s c.CBuff 1 (s.Time + c.Charm.Duration) []
                        expireAt s (s.Time + c.Charm.Duration)
                        put s.CharmCharges c.CBuff (charges - c.Charm.ChargesPerUse)
                        s.Life <- min (max s.Life pools.Life) (s.Life + (if pools.Life > 1.0 then c.Charm.LifeOnUse * pools.LifeRecoveryRate else 0.0))
                        s.Mana <- min (max s.Mana pools.Mana) (s.Mana + c.Charm.ManaOnUse * pools.ManaRecoveryRate)
                        if c.Charm.StartsRecharge then s.LastHit <- -1e9
                        bump s c.Used
                        prevented <- prevented || c.Charm.Prevents
            enter s was
            prevented
        | _ -> false

    /// Exposure on an enemy: the strongest applies (`Sim.expose`).
    let private expose (s: State) (target: int) (dt: DamageType) (amount: float) (duration: float) =
        let e = (at s.Foes target)
        if e.Life > 0.0 then
            let k = e.Info.ExposureK * s.P.ExposureEffect
            let id = (at s.P.ExposureStatus (typeIndex dt))
            let current =
                if (at e.Has id) && (at e.Expires id) > s.Time then (at e.DMods id) |> List.sumBy (fun m -> -m.Value) else 0.0
            if amount * k >= current then addDebuff s e id duration 1 [ Mod.base' (Resist dt) (-amount * k) ]

    /// Damage over time comes onto an enemy. Where only one of a label counts (an Ignite),
    /// those that can never count are not kept: an older one that is weaker and gone no
    /// later than the new one, and a new one that an older one outlasts and is no weaker
    /// than. (The reference keeps them all and finds the strongest each time; what counts
    /// at any time is the same.) `single`: no hit of any actor lets more than one of the
    /// label count, now or later; where one may, all are kept, as one dropped now could
    /// count beside a later one.
    let private addDot (e: Foe) (single: bool) (d: FDot) =
        let mutable alone = single && d.Most = 1
        let mutable beaten = false
        let mutable beats = false
        for x in List.toArray e.Dots do
            if x.Label = d.Label then
                if x.Most <> 1 then alone <- false
                elif x.Dps >= d.Dps && x.Until >= d.Until then beaten <- true
                elif x.Dps < d.Dps && x.Until <= d.Until then beats <- true
        if not alone then e.Dots <- d :: e.Dots
        elif not beaten then
            e.Dots <- d :: (if beats then e.Dots |> List.filter (fun x -> x.Label <> d.Label || not (x.Dps < d.Dps && x.Until <= d.Until)) else e.Dots)

    /// What a hit inflicts comes onto the enemy (`Sim.inflict`).
    let private inflict (s: State) (e: Foe) (i: Inflict) =
        let p = s.P
        match i with
        | Dot (a, t, dps, duration, maxStacks) ->
            let d = { Ailing = Some a; Label = (at p.AilmentSource (ailmentIndex a)); Of = t; Dps = dps; Until = s.Time + duration; Most = maxStacks }
            addDot e (at p.OneOfAilment (ailmentIndex a)) d
            if not (d.Until >= e.Soonest) then e.Soonest <- d.Until
            expireAt s d.Until
        | Status (Shock, magnitude, duration) ->
            let id = p.SShocked
            let current = if (at e.Has id) && (at e.Expires id) > s.Time then (at e.DMods id) |> List.sumBy (fun m -> m.Value) else 0.0
            if magnitude >= current then
                addDebuff s e id duration 1 [ Mod.inc (DamageTaken AllDamage) magnitude |> Mod.from "Shock" ]
        | Status (Chill, magnitude, duration) ->
            addDebuff s e p.SChilled duration 1 [ Mod.base' (Named "ChillMagnitude") magnitude ]
        | Status (a, _, duration) -> addDebuff s e (at p.AilmentStatus (ailmentIndex a)) duration 1 []
        | AddBuildup (a, pct) ->
            let ai = ailmentIndex a
            let had =
                let had = (at e.Buildup ai)
                let idle = s.Time - e.StunBuildupAt - Constants.stunDecayDelay
                if a = HeavyStun && idle > 0.0 then max 0.0 (had - Constants.stunDecayPerSecond * idle) else had
            if a = HeavyStun then e.StunBuildupAt <- s.Time
            let total = had + pct
            if total >= 100.0 && not (at e.Has ((at p.AilmentStatus ai))) then
                put e.Buildup ai (0.0)
                addDebuff s e (at p.AilmentStatus ai) (Constants.buildupDuration a) 1 []
                let more, seconds = Constants.afterBuildup a
                if more > 0.0 then
                    addDebuff s e (at p.AilmentResistant ai) (Constants.buildupDuration a + seconds) 1 [ Mod.more (Named(Ailment.name a + "Threshold")) more ]
                bump s (at p.CInflict ai)
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
        if s.A.DynVersion <> s.A.ModsVersion then
            let p = s.P
            let mutable counts : int[] = [||]
            for id in 0 .. s.A.BHas.Length - 1 do
                if (at s.A.BHas id) && not (at s.A.BMods id).IsEmpty then
                    if not (obj.ReferenceEquals((at s.A.BCompOf id), (at s.A.BMods id))) then
                        put s.A.BComp id (compileBuffMods p (at s.A.BMods id))
                        put s.A.BCompOf id ((at s.A.BMods id))
                    if counts.Length = 0 then counts <- Array.zeroCreate p.DynCount
                    for i__30 in 0 .. (at s.A.BComp id).Length - 1 do
                        let (slot, _) = at (at s.A.BComp id) i__30
                        put counts slot ((at counts slot) + 1)
            if counts.Length = 0 then s.A.Dyn <- p.NoDyn
            else
                let table = counts |> Array.map (fun n -> if n = 0 then p.NoItems else Array.zeroCreate n)
                for slot in 0 .. counts.Length - 1 do
                    put counts slot (0)
                for k in p.BuffOrder.Length - 1 .. -1 .. 0 do
                    let id = (at p.BuffOrder k)
                    if (at s.A.BHas id) && not (at s.A.BMods id).IsEmpty then
                        let comp = (at s.A.BComp id)
                        let stacks = float (at s.A.BStacks id)
                        for j in comp.Length - 1 .. -1 .. 0 do
                            let slot, it = (at comp j)
                            (at table slot).[(at counts slot)] <- { it with V = it.V * stacks }
                            put counts slot ((at counts slot) + 1)
                s.A.Dyn <- table
            s.A.DynOfSkill <- s.A.Dyn |> Array.exists (fun items -> items |> Array.exists (fun it -> it.Conds |> Array.exists ofSkill))
            s.A.DynOnStatus <- s.A.Dyn |> Array.exists (fun items -> items |> Array.exists (fun it -> it.Conds |> Array.exists ofEnemy))
            s.A.DynVersion <- s.A.ModsVersion
        s.A.Dyn

    /// The buffs' mods for a skill asked in one of the three ways (see `MSkill`).
    let private dynFor (s: State) (sid: int) (mode: int) : Item[][] =
        let table = dyn s
        if not s.A.DynOfSkill then table
        else
            let at = sid * 3 + mode
            if s.A.DynForVer.[at] <> s.A.DynVersion then
                let skill = s.P.Skills.[sid]
                s.A.DynFor.[at] <-
                    (if mode = MEmpty then resolveDyn table Set.empty ""
                     elif mode = MDot then resolveDyn table skill.DotTags skill.Skill.Name
                     else resolveDyn table skill.Skill.Tags skill.Skill.Name)
                put s.A.DynForVer at (s.A.DynVersion)
            s.A.DynFor.[at]

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
        ev.Q <- (at s.P.Skills sid).Q
        ev.Tags <- Set.empty
        ev.Name <- ""
        ev.Dyn <- dynFor s sid MEmpty
        ev.DynRaw <- s.A.Dyn
        ev

    /// For questions asked as the skill (its tags and name known; buffs and enemy to be given).
    let private acquire (s: State) (sid: int) : Ev =
        let info = (at s.P.Skills sid)
        let table = dynFor s sid MSkill
        let raw = s.A.Dyn
        // Rage: 1% more attack damage per Rage, times rage effect, rounded down
        if (at s.A.RageVer sid) <> s.A.DynVersion then
            let rage = s.P.BRage
            s.A.RageMore.[sid] <-
                (if (at s.A.BHas rage) && (at s.A.BStacks rage) > 0 then
                    let e = acquireEmpty s sid
                    let effect = multQ e qRageEffect
                    release s
                    floor (float (at s.A.BStacks rage) * effect) * Constants.moreAttackDamagePerRage
                 else nan)
            put s.A.RageVer sid (s.A.DynVersion)
        let ev = acquireRaw s
        ev.Q <- info.Q
        ev.Tags <- info.Skill.Tags
        ev.Name <- info.Skill.Name
        ev.Dyn <- table
        ev.DynRaw <- raw
        ev.AsAttack <- info.IsAttack
        ev.RageMore <- (at s.A.RageMore sid)
        ev

    let private useBuffs (s: State) (ev: Ev) (info: SkillInfo) (extra: bool) =
        ev.BOn <- true
        ev.BHas <- s.A.BHas
        ev.BStacks <- s.A.BStacks
        s.A.BKept <- true
        if extra && info.StackExtra > 0 then
            ev.Extra <- info.StackBuff
            ev.ExtraN <- info.StackExtra

    /// A hit's answers are kept for the skill's next hit: an answer is good while what it
    /// can change with is what it was (the buffs and their mods; the enemy and its statuses).
    /// Asked once the ev has its buffs and its enemy.
    let private keepAnswers (s: State) (ev: Ev) (skill: int) (foe: int) (role: int) =
        // (the answers are kept apart for each way of asking: 0 a hit, 1 how long a use takes, 2 what it costs)
        let sid = skill * 3 + role
        let snap = if ev.BOn then s.A.BuffsVersion else -2
        if (at s.A.KVal sid).Length = 0 then
            // (after the answers: the damage ranges `ranges` worked out, and again for critical hits)
            let n = ev.Q.Length * 2
            put s.A.KVal sid (Array.zeroCreate (n + 30))
            put s.A.KBuffs sid (Array.create (n + 2) -1)
            put s.A.KStatus sid (Array.create (n + 2) -1)
        if (at s.A.KSnap sid) <> snap || (at s.A.KDyn sid) <> s.A.DynVersion then
            put s.A.KSnap sid (snap)
            put s.A.KDyn sid (s.A.DynVersion)
            put s.A.KGBuffs sid ((at s.A.KGBuffs sid) + 1)
        let flags = (if ev.Iso then 1 else 0) + (if ev.Close then 2 else 0) + (if ev.Open then 4 else 0)
        if (at s.A.KFoe sid) <> foe || (at s.A.KFlags sid) <> flags || not (obj.ReferenceEquals((at s.A.KHas sid), ev.SHas)) then
            put s.A.KFoe sid (foe)
            put s.A.KFlags sid (flags)
            put s.A.KHas sid (ev.SHas)
            put s.A.KGStatus sid ((at s.A.KGStatus sid) + 1)
        ev.KVal <- (at s.A.KVal sid)
        ev.KBuffs <- (at s.A.KBuffs sid)
        ev.KStatus <- (at s.A.KStatus sid)
        ev.GBuffs <- (at s.A.KGBuffs sid)
        ev.GStatus <- (at s.A.KGStatus sid)
        ev.DynOnStatus <- s.A.DynOnStatus
        ev.Keeping <- true

    let private useStatus (s: State) (ev: Ev) (e: Foe) =
        refreshStatus s e
        ev.SOn <- true
        ev.SHas <- e.SHas
        ev.SStacks <- e.SStacks

    let private ready (s: State) (sid: int) = (at s.A.Cooldowns sid) <= s.Time

    // ---- minions ----

    /// A minion goes out of the fight (killed, or its time is up): what it had begun is
    /// lost (`Sim.leave`; see `Agent.Epoch`).
    let private leave (s: State) (who: int) =
        let a = at s.Agents who
        s.RoomAt <- -2
        a.Epoch <- a.Epoch + 1
        for skill in 0 .. a.InFlight.Length - 1 do
            put a.InFlight skill 0
        a.Gone <- true
        a.Health <- 0.0
        a.Barrier <- 0.0

    /// Temporary minions of a pool appear: those that are out first; when all are in the
    /// fight, the ones whose time would be up soonest are summoned anew (`Sim.summonWith`).
    let private summon (s: State) (members: int[]) (count: int) (duration: float) (first: Mod list) (summoned: int) =
        if members.Length > 0 && count > 0 then
            let was = s.Cur
            s.RoomAt <- -2
            let mutable rest =
                List.ofArray members
                |> List.sortBy (fun m -> let a = at s.Agents m in (if a.Gone then 0 else 1), a.Leaves)
                |> List.truncate count
            let mutable place = 0
            while not rest.IsEmpty do
                let m = rest.Head
                let a = at s.Agents m
                let gone = a.Gone
                let leaves = s.Time + duration
                enter s m
                if place = 0 && not first.IsEmpty then setBuff s s.P.BEmpowered 1 leaves first else removeBuff s s.P.BEmpowered
                a.Gone <- false
                a.Leaves <- leaves
                (match s.P.Body with
                 | Some body ->
                    a.Health <- body.Life
                    a.Barrier <- body.EnergyShield
                 | None ->
                    a.Health <- 0.0
                    a.Barrier <- 0.0)
                a.HitAt <- s.Time
                if gone then schedule s s.Time EReady 0 0 0.0 0.0
                if leaves < infinity then schedule s leaves ELeaves 0 0 leaves 0.0
                bump s summoned
                rest <- rest.Tail
                place <- place + 1
            enter s was

    /// Chained attacks: which use of the chain comes next (-1: the skill is no chain). Using
    /// another skill in between starts the chain again (`Sim.seqStep`).
    let private seqStep (s: State) (sid: int) (info: SkillInfo) : int =
        if info.SeqLength <= 0 then -1
        elif s.A.LastUsed <> sid then 0
        else int (Map.tryFind info.SeqKey s.A.Energy |> Option.defaultValue 0.0) % info.SeqLength

    /// Whether a part belongs to this place of the chain (`Sim.inStep`).
    let private inStep (step: int) (part: SkillInfo) : bool =
        step < 0 || not part.InChain || Array.contains step part.SeqSteps

    /// The corpses that lie about: every kill leaves one, a corpse skill takes one.
    let inline private corpses (s: State) = s.Kills.Count - s.CorpsesUsed

    let rec private react (s: State) (rng: Rng) (rules: CRule[]) (killed: string) (sid: int) (target: int) =
        for i__31 in 0 .. rules.Length - 1 do
            let r = at rules i__31
            if (at r.Applies sid) && (killed = "" || r.Killed = killed) then
                if rng.Chance r.Chance then perform s rng sid target r.Re

    and private perform (s: State) (rng: Rng) (sid: int) (target: int) (re: Re) =
        let onTarget () = target >= 0 && (at s.Foes target).Life > 0.0
        match re with
        | RSpendStack (b, next) ->
            if held s b then
                if (at s.A.BStacks b) = 1 then removeBuff s b else setBuff s b ((at s.A.BStacks b) - 1) (at s.A.BExpires b) (at s.A.BMods b)
                perform s rng sid target next
        | RSetStacks (b, n) -> setBuff s b n infinity []
        | RRegenES (rate, duration, count) ->
            // (the player's, whoever the rule is of)
            if s.Main.Vitals.IsSome && alive s then
                s.Recoups <- (s.Time + duration, rate, "ShroudES") :: (s.Recoups |> List.filter (fun (_, _, pool) -> pool <> "ShroudES"))
                bump s count
        | RGainEnergy (meta, amount) -> if meta >= 0 then gainEnergy s rng meta amount
        | REmpower (b, perPower, cap, counts) ->
            // a warcry: one attack per so much monster power among the living, at least one
            let mutable power = 0.0
            for i__32 in 0 .. s.Live.Length - 1 do
                let e = at s.Foes (at s.Live i__32)
                if e.Life > 0.0 then power <- power + MonsterPower.ofRarity e.Def.Rarity
            let n = max 1 (int (floor (min cap power / max 1.0 perPower)))
            bump s (at counts n)
            setBuff s b n infinity []
        | RNone -> ()
        | RWithCorpse (next, otherwise) ->
            if corpses s > 0 then
                s.CorpsesUsed <- s.CorpsesUsed + 1
                perform s rng sid target next
            else perform s rng sid target otherwise
        | RSummon (members, count, duration, first, summoned) -> summon s members count duration first summoned
        | RSummonPerCorpse (members, duration, deadMinions, summoned) ->
            // one for every corpse, and for every killed minion that waits to be revived, each once per death
            let taken = min members.Length (corpses s)
            let mutable bones = 0
            if deadMinions then
                let reviving = s.P.Reviving
                for i in 0 .. reviving.Length - 1 do
                    let a = at s.Agents (at reviving i)
                    if a.Gone && not a.Boned && bones < members.Length - taken then
                        a.Boned <- true
                        bones <- bones + 1
            if taken + bones > 0 then
                s.CorpsesUsed <- s.CorpsesUsed + taken
                summon s members (taken + bones) duration [] summoned
        | RSummonPerWard (members, wardEach, duration, summoned) ->
            if s.Main.Vitals.IsSome && wardEach > 0.0 && s.Ward >= wardEach then
                let count = int (s.Ward / wardEach)
                s.Ward <- 0.0
                summon s members count duration [] summoned
        | RGrantMinionBuff (b, duration, mods) ->
            let was = s.Cur
            for m in 1 .. s.Agents.Length - 1 do
                enter s m
                setBuff s b 1 (s.Time + duration) mods
            enter s was
            expireAt s (s.Time + duration)
        | RAttachPustule (part, seconds, per, cap, count) ->
            if onTarget () && not (s.Pustules |> List.exists (fun (_, _, enemy, _, _, _, _) -> enemy = target)) then
                // (the Poison damage dealt so far: what is still kept back goes into the results first)
                for i in 0 .. s.Foes.Length - 1 do
                    flushDots s (at s.Foes i)
                let dealt = at s.Damage (at s.P.AilmentSource (ailmentIndex Poison))
                s.Pustules <- (s.Cur, part, target, dealt, per, cap, s.Time + seconds) :: s.Pustules
                bump s count
                expireAt s (s.Time + seconds)
        | RGrantOneOf (names, duration) ->
            if names.Length > 0 then
                let pick = at names (min (names.Length - 1) (int (rng.Float() * float names.Length)))
                for i in 0 .. names.Length - 1 do
                    removeBuff s (at names i)
                setBuff s pick 1 (s.Time + duration) []
                expireAt s (s.Time + duration)
        | RIfBuff (b, next) -> if held s b then perform s rng sid target next
        | RIfEnemyHas (st, next) ->
            if onTarget () && (let e = at s.Foes target in refreshStatus s e; at e.SHas st) then perform s rng sid target next
        | RGrantBuff (b, duration, maxStacks, mods) -> grantBuff s b duration maxStacks mods
        | RRemoveBuff b -> removeBuff s b
        | RApplyDebuff (id, duration, maxStacks, mods, blocked) ->
            if not (at blocked sid) && onTarget () then addDebuff s (at s.Foes target) id duration maxStacks mods
        | RApplyCurse (id, duration, mods, others, limit, count) ->
            if onTarget () then
                let e = (at s.Foes target)
                // one curse (plus "additional curses"), one mark: the oldest give way
                let there = others |> Array.filter (fun o -> (at e.Has o) && (at e.Expires o) > s.Time) |> List.ofArray |> List.sortBy (fun o -> (at e.Expires o))
                for o in there |> List.truncate (max 0 (there.Length + 1 - limit)) do
                    removeDebuff e o
                addDebuff s e id duration 1 (at mods e.Root)
                bump s count
        | RApplyExposure (dt, amount, duration) -> if onTarget () then expose s target dt amount duration
        | RGrowExposure (dt, start, perPulse, cap, every, pulses, duration) ->
            if onTarget () then
                for k in 1 .. pulses do
                    schedule s (s.Time + every * float k) EExpose target (typeIndex dt) (min cap (start + perPulse * float k)) duration
        | RRemoveDebuff id -> if onTarget () then removeDebuff (at s.Foes target) id
        | RTrigger skill -> trigger s rng skill
        | RGainResource (b, n) -> schedule s s.Time EGainStacks b n 0.0 0.0
        | RDropRemnant (b, delay, extra, count) ->
            let n = if rng.Chance extra then 2 else 1
            bump s count
            schedule s (s.Time + delay) EGainStacks b n 0.0 0.0
        | RChargeBuff (b, charge, perStack, seconds, doubleChance, count) ->
            let held = stacksOf s charge
            if held <> 0 then
                let stacks = if rng.Chance doubleChance then 2 * held else held
                // mods are per stack: the buff's stacks scale them
                let expires = s.Time + seconds * float stacks
                setBuff s b stacks expires perStack
                expireAt s expires
                bump s count

    /// A meta gem gains energy, and triggers its spells if that allows (`Sim.gainEnergy`,
    /// `Sim.releaseMeta`): "Cast on" at maximum, Invocations whenever enough is banked and the
    /// invocation is off cooldown (as many times as it covers).
    and private gainEnergy (s: State) (rng: Rng) (index: int) (gained: float) =
        let info = (at s.P.Metas index)
        let meta = info.Meta
        let gained =
            if info.EnergyMods.Length = 0 then gained
            else
                // (asked with the buffs known and nothing else: no skill, no enemy)
                let ev = acquireRaw s
                ev.Tags <- Set.empty
                ev.Name <- ""
                ev.BOn <- true
                ev.BHas <- s.A.BHas
                ev.BStacks <- s.A.BStacks
                s.A.BKept <- true
                let mutable sum = 0.0
                for k in 0 .. info.EnergyMods.Length - 1 do
                    let it = at info.EnergyMods k
                    if it.Conds.Length = 0 || passes ev it then sum <- sum + amount ev it
                release s
                let more = sum / 100.0
                gained * (meta.EnergyMult + more) / meta.EnergyMult
        let energy = min meta.MaxEnergy ((if (at s.A.HeldSet index) then (at s.A.Held index) else 0.0) + gained)
        put s.A.Held index energy
        put s.A.HeldSet index true
        s.A.HeldStale <- true
        let triggers =
            match meta.Release with
            | AtMaximum -> if energy >= meta.MaxEnergy && meta.Cost > 0.0 then 1 else 0
            | Invocation _ -> if (at s.A.InvocationReady index) <= s.Time && meta.Cost > 0.0 then int (energy / meta.Cost) else 0
        if triggers <> 0 then
            let spent = float triggers * meta.Cost
            let refund = if rng.Chance meta.RefundChance then spent / 2.0 else 0.0
            put s.A.Held index (energy - spent + refund)
            (match meta.Release with
             | Invocation cd -> s.A.InvocationReady.[index] <- s.Time + cd
             | AtMaximum -> ())
            bump s info.Released
            for _ in 1 .. triggers do
                for i__33 in 0 .. info.Triggers.Length - 1 do
                    let skill = at info.Triggers i__33
                    trigger s rng skill

    /// A use of a skill starts now, aimed at the current target (`Sim.startUse`).
    and private startUse (s: State) (rng: Rng) (sid: int) (useTime: float) =
        let target = aimed s
        if target >= 0 then
            let p = s.P
            let info = (at p.Skills sid)
            let skill = info.Skill
            let foe = (at s.Foes target)
            // the use's critical hit roll, for the skill and every part of it
            let r = Rng(Sim.streamSeedOf s.Seed info.RollKey (at s.A.Rolled sid)).Float()
            put s.A.Rolled sid ((at s.A.Rolled sid) + 1)
            for i__34 in 0 .. info.Parts.Length - 1 do
                let part = at info.Parts i__34
                put s.A.CritRoll part (r)
                put s.A.HasCritRoll part (true)
            // (what the use asks of the mods, with the buffs and the enemy as they are now)
            let ev = acquire s sid
            useBuffs s ev info true
            useStatus s ev foe
            let recovery = if skill.Cooldown.IsSome then multQ ev qCooldownRecovery else 1.0
            let doubled = skill.Cooldown.IsSome && sumQ ev qDoubleUses > 0.0
            let lose = if p.Vitals.IsSome then sumQ ev qLosePools / 100.0 else 0.0
            let dodge = if p.Metas.Length > 0 && skill.Tags.Contains(Tag "DodgeRoll") then Constants.dodgeRollDistance + sumQ ev qDodgeDistance else 0.0
            let lasting = if info.AsksDuration then multQ ev qSkillDuration else 1.0
            let areaMult = if info.AsksArea then multQ ev qArea else 1.0
            release s
            match skill.Cooldown with
            | Some cd ->
                let uses = skill.StoredUses * (if doubled then 2 else 1)
                let pending = (at s.A.Recharges sid) |> List.filter (fun t -> t > s.Time)
                let start = if p.Actor.ParallelUses || pending.IsEmpty then s.Time else List.max pending
                let pending = List.sort (pending @ [ start + cd / recovery ])
                put s.A.Recharges sid (pending)
                // blocked once every stored use is recharging; free again when the first is back
                put s.A.Cooldowns sid ((if pending.Length >= uses then pending.Head else -infinity))
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
            // (what the use reads of the buffs as they were when it began)
            let charged = info.Consumes >= 0 && stacksOf s info.Consumes > 0
            // chained attacks: this use's place in the chain, and the next
            let step = seqStep s sid info
            let barrage = p.BBarrage
            let repeats = if info.SpendsRepeats && (at s.A.BHas barrage) && (at s.A.BStacks barrage) > 0 then (at s.A.BStacks barrage) else 0
            let repeatsUntil = if repeats > 0 then (at s.A.BExpires barrage) else 0.0
            let repeatsMods = if repeats > 0 then (at s.A.BMods barrage) else []
            // when the use's hits land (see `SkillInfo.PatShare`): the skill's own pattern,
            // unless seals or readied repeats add to it
            let mutable patShare = info.PatShare
            let mutable patAt = info.PatAt
            let mutable patMult = info.PatMult
            // Sealed skills: a seal every so often (a fixed time, or a share of the use time), up to
            // a maximum; the use breaks them all. A fight starts with the seals full.
            if info.MaxSeals > 0.0 then
                let interval = info.SealMs / 1000.0 + info.SealPercent / 100.0 * useTime
                let broken = at s.A.SealBreak sid
                let seals =
                    if interval <= 0.0 || useTime <= 0.0 then 0
                    elif System.Double.IsNaN broken then int info.MaxSeals
                    else int (min info.MaxSeals (floor ((s.Time - broken) / interval)))
                if seals > 0 then
                    put s.A.SealBreak sid s.Time
                    // "additional Projectiles are fired": for this use
                    if info.SealProjectiles > 0.0 then setBuff s p.BSealsBroken seals (s.Time + useTime + skill.HitDelay + 1e-6) info.SealMods
                    bump s info.CSeals
                // (a use without seals still restarts the count)
                elif System.Double.IsNaN broken then put s.A.SealBreak sid s.Time
                // "each causing the Spell to Repeat": the hits again per seal, with the repeats' own damage
                let again = seals * int info.SealRepeats
                if again > 0 then
                    let k = 1.0 + info.SealRepeatDamage / 100.0
                    let n = patAt.Length
                    patShare <- Array.append patShare (Array.create (again * n) false)
                    patAt <- Array.append patAt (Array.init (again * n) (fun j -> useTime + 0.1 * float (j / n + 1)))
                    patMult <- Array.append patMult (Array.init (again * n) (fun j -> (at patMult (j % n)) * k))
            // Barrage readies repeats for the next Repeatable attack: its base number and one per
            // Frenzy Charge, all of which it consumes
            if info.BarrageRepeats > 0.0 then
                let frenzy = at p.BCharges 1
                let n = int (info.BarrageRepeats + info.BarragePerFrenzy * float (stacksOf s frenzy))
                let lasts = useTime + (if skill.Duration > 0.0 then skill.Duration * lasting else 5.0)
                removeBuff s frenzy
                setBuff s barrage n (s.Time + lasts) info.BarrageMods
                expireAt s (s.Time + lasts)
                bump s p.CBarrageReadied
            // a Repeatable attack with Barrage readied fires again for every repeat, within the
            // same (longer) use; the repeats deal less. Barrage's cooldown grows by a share of
            // the attack's time
            if repeats > 0 && s.Time < repeatsUntil && useTime > 0.0 then
                let value (name: string) = repeatsMods |> List.sumBy (fun m -> if m.Stat = Named name then m.Value else 0.0)
                let k = 1.0 - value "BarrageRepeatLess" / 100.0
                let n = patAt.Length
                let share, time, mult = patShare, patAt, patMult
                patShare <- Array.init ((repeats + 1) * n) (fun j -> at share (j % n))
                patAt <-
                    Array.init ((repeats + 1) * n) (fun j ->
                        let i = j / n
                        if (at share (j % n)) then (float i + (at time (j % n))) / float (repeats + 1)
                        else (at time (j % n)) + useTime * float i / float (repeats + 1))
                patMult <- Array.init ((repeats + 1) * n) (fun j -> if j / n = 0 then (at mult (j % n)) else (at mult (j % n)) * k)
                removeBuff s barrage
                let extra = value "BarrageCooldownPct" / 100.0 * useTime
                (match repeatsMods |> List.tryPick (fun m -> if m.Stat = Named "BarrageCooldownPct" && m.Source <> "" then Some m.Source else None) with
                 | Some name when extra > 0.0 ->
                    match p.SkillIds.TryGetValue name with
                    | true, source ->
                        // "to a maximum of 30 seconds"
                        let ready = min (s.Time + 30.0) (max s.Time (at s.A.Cooldowns source) + extra)
                        put s.A.Cooldowns source ready
                        put s.A.Recharges source [ ready ]
                    | _ -> ()
                 | _ -> ())
                bump s info.CBarraged
            bump s info.CUses
            // the Glory or Combo the skill needed is spent
            for k in 0 .. info.Requires.Length - 1 do
                s.A.Energy <- Map.remove (fst (at info.Requires k)) s.A.Energy
            // (a skill whose rule says what it does with corpses takes them there)
            if info.TakesCorpse && corpses s > 0 then s.CorpsesUsed <- s.CorpsesUsed + 1
            // a warp into one's own projectile: the projectile is gone (what it had still to do
            // with it), and the warp's explosion happens whatever state the enemy is in
            if info.Warps && info.WarpsInto |> Array.exists (fun into -> (at s.A.InFlight into) > 0) then
                for k in 0 .. info.WarpsInto.Length - 1 do
                    let into = at info.WarpsInto k
                    put s.A.Flight into ((at s.A.Flight into) + 1)
                    put s.A.InFlight into 0
                bump s info.CWarped
                removeBuff s info.WarpBuff
                setBuff s info.Warped 1 infinity []
            if step >= 0 then s.A.Energy <- Map.add info.SeqKey (float (step + 1)) s.A.Energy
            if useTime > 0.0 then s.A.LastUsed <- sid
            let hits = patAt.Length
            let mutable first = infinity
            let mutable last = s.Time
            for i in 0 .. hits - 1 do
                let at = (if (at patShare i) then s.Time + (at patAt i) * useTime + skill.HitDelay else s.Time + (at patAt i) + skill.HitDelay)
                first <- min first at
                last <- max last at
            for i in 0 .. hits - 1 do
                let at = (if (at patShare i) then s.Time + (at patAt i) * useTime + skill.HitDelay else s.Time + (at patAt i) + skill.HitDelay)
                let mult = patMult.[i]
                for part in info.Parts do
                    let pi = p.Skills.[part]
                    let happens =
                        System.Double.IsNaN pi.Seed
                        && (at <= first || not pi.OncePerUse)
                        && (part = sid || not pi.OnlyTriggered)
                        && (if pi.NotEnraged then true
                            elif pi.NotWithCharge then not charged
                            elif pi.WithCharge then charged
                            elif pi.Waits then false
                            elif pi.OnlyInfused >= 0 then stacksOf s pi.OnlyInfused > 0
                            else not (info.HasChargedPart && charged && pi.Explosion))
                        && inStep step pi
                    if happens then
                        // (a strike of the chain that hits several times, spread over the use)
                        let n = pi.SeqHits
                        for i in 1 .. n do
                            if pi.Passing then schedule s at EPassing part target mult (at - s.Time)
                            else schedule s (if n = 1 then at else s.Time + (at - s.Time) * float i / float n) EStrike part target mult 0.0
            // Ice Crystals the use leaves behind: each stands at a random spot of the skill's
            // area (around the user, or around the target), and those whose explosion reaches
            // the target count. They appear after the use's own hits, which do not damage them.
            if info.AsksArea then
                let targetDef = foe.Def
                for k in 0 .. info.Parts.Length - 1 do
                    let part = at info.Parts k
                    let pi = at p.Skills part
                    if pi.Crystals then
                        let radius (q: SkillInfo) = if q.AreaRadius > 0.0 || q.IsAreaAtTarget || q.AroundSelf then Geometry.radius q.AreaRadius areaMult else 0.0
                        let centre = if info.AroundSelf then Vec.zero else targetDef.Position
                        let spread, reach = radius info, radius pi
                        let stands () =
                            if spread <= 0.0 || reach <= 0.0 then true
                            else
                                let r, a = spread * sqrt (rng.Float()), 2.0 * System.Math.PI * rng.Float()
                                Vec.dist (Vec.add centre (Vec.scale r (Vec.ofAngle a))) targetDef.Position <= reach + targetDef.Size
                        let life = pi.CrystalLife
                        // a wall: a row of crystals through the target, across the line to it. Those
                        // that come up inside the monster are broken by it at once; the others
                        // stand, and count if their explosion reaches it
                        let wall = if part = sid then pi.WallLength else 0.0
                        let row =
                            if wall <= 0.0 then []
                            else
                                let n = max 1 (int (wall / Constants.iceCrystalWidth))
                                [ for i in 0 .. n - 1 -> abs ((float i - float (n - 1) / 2.0) * Constants.iceCrystalWidth) ]
                        let broken = row |> List.filter (fun d -> d <= targetDef.Size) |> List.length
                        if life > 0.0 && broken > 0 then
                            for _ in 1 .. broken do
                                schedule s (last + 1e-6) EStrike part target Constants.freshCrystalDamage 0.0
                        let created = if wall > 0.0 then max 1 row.Length else max 1 (int pi.CrystalsCreated)
                        let near =
                            if wall > 0.0 then row |> List.filter (fun d -> d > targetDef.Size && d <= reach + targetDef.Size) |> List.length
                            else
                                let mutable standing = 0
                                for _ in 1 .. created do
                                    if stands () then standing <- standing + 1
                                standing
                        if life > 0.0 && near > 0 then
                            let lasts = if pi.Skill.Duration > 0.0 then pi.Skill.Duration * lasting else 1e9
                            // the limit counts every crystal; of those, the share that stands near
                            let limit = max near (int (System.Math.Round(max (float created) pi.CrystalLimit * float near / float created)))
                            s.Plants.Add((near, life, limit, lasts))
                            schedule s (last + 1e-6) EPlant part target (float (s.Plants.Count - 1)) 0.0
            // a seed planted in the target: it explodes once the enemy has taken enough damage
            // from then on; a new seed replaces the old one
            if not (System.Double.IsNaN info.Seed) then
                defList s foe
                let needed = info.Seed / 100.0 * max 1.0 (Constants.ailmentThreshold foe.Info.Def.Level * dmult foe thresholdStats)
                let lasts = if skill.Duration > 0.0 then skill.Duration * lasting else 1e9
                let owner = s.Cur
                s.Seeds <- (owner, sid, target, foe.Life, needed, s.Time + useTime + lasts) :: (s.Seeds |> List.filter (fun (a, b, _, _, _, _) -> not (a = owner && b = sid)))
                bump s info.CPlanted
            // an infusion a part needed is used up; a part that waits is put down
            // (which parts had their infusion is seen before any is used up)
            if info.Parts.Length > 1 then
                let had = info.Parts |> Array.map (fun part -> let infusion = (at p.Skills part).OnlyInfused in infusion >= 0 && stacksOf s infusion > 0)
                for k in 1 .. info.Parts.Length - 1 do
                    let part = at info.Parts k
                    let pi = at p.Skills part
                    if at had k then
                        let infusion = pi.OnlyInfused
                        if (at s.A.BHas infusion) && (at s.A.BStacks infusion) > 1 then setBuff s infusion ((at s.A.BStacks infusion) - 1) (at s.A.BExpires infusion) (at s.A.BMods infusion)
                        else removeBuff s infusion
                    elif pi.Waits then put s.A.Waiting part (min 20 ((at s.A.Waiting part) + 1))
            // a detonator (or a warcry) sets off everything of that kind that waits
            for k in 0 .. info.SetsOff.Length - 1 do
                let part = at info.SetsOff k
                let n = at s.A.Waiting part
                if n > 0 then
                    put s.A.Waiting part 0
                    for _ in 1 .. n do
                        schedule s (last + 1e-6) EStrike part target 1.0 0.0
                    bump s (at p.Skills part).CDetonated
            // an attack the player makes while empowered by a warcry uses one empowerment up,
            // and a Repeatable attack one of the repeats readied for it
            if useTime > 0.0 && (info.SpendsEmpowered || info.SpendsRepeats) then
                if info.SpendsEmpowered then
                    for i__36 in 0 .. p.Empowering.Length - 1 do
                        let b = at p.Empowering i__36
                        if (at s.A.BHas b) then schedule s (last + 1e-9) ESpendBuff b 0 0.0 0.0
                if info.SpendsRepeats then
                    for i__37 in 0 .. p.Readied.Length - 1 do
                        let b = at p.Readied i__37
                        if (at s.A.BHas b) then schedule s (last + 1e-9) ESpendBuff b 0 0.0 0.0
            if info.Consumes >= 0 then
                schedule s (last + 1e-9) EConsume info.Consumes sid 0.0 0.0
            if useTime > 0.0 && p.Metas.Length > 0 then
                // Cast on Dodge: energy per unit travelled by a dodge roll, there when the roll ends
                if skill.Tags.Contains(Tag "DodgeRoll") then
                    let distance = dodge
                    for i in 0 .. p.Metas.Length - 1 do
                        let meta = at p.Metas i
                        let mutable amount = 0.0
                        for k in 0 .. meta.Sources.Length - 1 do
                            match at meta.Sources k with
                            | DodgeRolls per -> amount <- amount + per * distance
                            | _ -> ()
                        if amount > 0.0 then schedule s (s.Time + useTime) EEnergy i 0 (amount * meta.Meta.EnergyMult) 0.0
                // energy per 10 ms of base cast time of spells the player casts
                if skill.Tags.Contains Spell then
                    for i in 0 .. p.Metas.Length - 1 do
                        let meta = at p.Metas i
                        let mutable amount = 0.0
                        for k in 0 .. meta.Sources.Length - 1 do
                            match at meta.Sources k with
                            | CastsSpells per -> amount <- amount + per * skill.BaseTime * 100.0
                            | _ -> ()
                        if amount > 0.0 then gainEnergy s rng i (amount * meta.Meta.EnergyMult)
            react s rng p.OnUse "" sid target

    and private trigger (s: State) (rng: Rng) (sid: int) =
        if ready s sid then
            bump s (at s.P.Skills sid).CTrigger
            setBuff s s.P.BTriggered 1 (s.Time + 4.0) []
            expireAt s (s.Time + 4.0)
            // (aimed at the cause of the trigger while it starts)
            s.Aim <- s.Cause
            startUse s rng sid 0.0
            s.Aim <- -1

    /// How long a use of the skill takes now (`Sim.useTime`).
    let private useTime (s: State) (sid: int) : float =
        let info = (at s.P.Skills sid)
        let t = aimed s
        // a chained attack takes the time of the strike that is due (its own speed)
        let step = seqStep s sid info
        let mutable own = sid
        if step >= 0 then
            let mutable found = false
            for k in 0 .. info.Parts.Length - 1 do
                let part = at s.P.Skills (at info.Parts k)
                if not found && part.InChain && inStep step part then
                    own <- at info.Parts k
                    found <- true
        let ownSpeed =
            if own = sid then nan
            else
                let ownInfo = at s.P.Skills own
                let evO = acquire s own
                useBuffs s evO ownInfo false
                if t >= 0 then useStatus s evO (at s.Foes t)
                keepAnswers s evO own t 1
                let v = multQ evO qSpeed
                release s
                v
        let ev = acquire s sid
        useBuffs s ev info false
        if t >= 0 then useStatus s ev (at s.Foes t)
        keepAnswers s ev sid t 1
        let speed = if own = sid then multQ ev qSpeed else ownSpeed
        let info' = at s.P.Skills own
        // "Skills take 5% longer to perform for each time they Repeat"
        let barrage = s.P.BBarrage
        let repeats = if info.SpendsRepeats && (at s.A.BHas barrage) && (at s.A.BStacks barrage) > 0 && s.Time < (at s.A.BExpires barrage) then float (at s.A.BStacks barrage) else 0.0
        let time = info'.Skill.BaseTime / speed * (1.0 + Constants.longerPerRepeat * repeats) + sumQ ev qFixedUse
        release s
        time

    /// Mana spent while a Sigil of Power stands: a Stage for every share of maximum mana, at
    /// most one a second, up to its maximum (`Sim.sigilSpend`).
    let private sigilSpend (s: State) (spent: float) (maxMana: float) =
        let p = s.P
        // the first of the buffs, in the order of their names, that is a sigil
        let mutable sigil = -1
        for k in 0 .. p.BuffOrder.Length - 1 do
            let id = at p.BuffOrder k
            if sigil < 0 && (at s.A.BHas id) && (at s.A.BMods id) |> List.exists (fun m -> m.Stat = Named "SigilMaxStages") then sigil <- id
        if sigil >= 0 && (at s.A.BExpires sigil) > s.Time && spent > 0.0 && maxMana > 0.0 then
            let value (stat: string) = (at s.A.BMods sigil) |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
            let need = value "SigilManaPercentPerStage" / 100.0 * maxMana
            let total = (Map.tryFind "SigilSpent" s.A.Energy |> Option.defaultValue 0.0) + spent
            let last = Map.tryFind "SigilStageAt" s.A.Energy |> Option.defaultValue -1.0
            if need > 0.0 && total >= need && (at s.A.BStacks sigil) < int (value "SigilMaxStages") && s.Time - last >= 1.0 then
                bump s p.CSigil
                setBuff s sigil ((at s.A.BStacks sigil) + 1) (at s.A.BExpires sigil) (at s.A.BMods sigil)
                s.A.Energy <- s.A.Energy |> Map.add "SigilSpent" (total - need) |> Map.add "SigilStageAt" s.Time
            else s.A.Energy <- Map.add "SigilSpent" total s.A.Energy

    /// The actor's energies and counters by name, as the reference has them: the meta gems'
    /// energy is kept by number, and put under the gems' names here when it has changed.
    let private energyMap (s: State) : Map<string, float> =
        if s.A.HeldStale then
            let metas = s.P.Metas
            for i in 0 .. metas.Length - 1 do
                if (at s.A.HeldSet i) then s.A.Energy <- Map.add (at metas i).Meta.Name (at s.A.Held i) s.A.Energy
            s.A.HeldStale <- false
        s.A.Energy

    /// What the player sees when choosing a skill (`Sim.view`).
    let private view (s: State) : View =
        let t = aimed s
        // what is seen of the minions: how many of each pool of temporary ones are out
        // ("Out:<pool>"), and how many corpses and dead minions lie about ("Corpses")
        let seen =
            if not s.P.AnyPool then buffMap s
            else
                let mutable m = buffMap s
                let mutable bones = 0
                for i in 0 .. s.P.Reviving.Length - 1 do
                    let a = at s.Agents (at s.P.Reviving i)
                    if a.Gone && not a.Boned then bones <- bones + 1
                let lying = corpses s + bones
                if lying > 0 then m <- Map.add "Corpses" lying m
                let mutable out : Map<string, int> = Map.empty
                for i in 1 .. s.Agents.Length - 1 do
                    match (at s.Progs i).Actor.Pool with
                    | Some pool when not (at s.Agents i).Gone ->
                        let key = "Out:" + pool
                        out <- Map.add key ((Map.tryFind key out |> Option.defaultValue 0) + 1) out
                    | _ -> ()
                out |> Map.fold (fun m key n -> Map.add key n m) m
        { Time = s.Time
          Buffs = seen
          EnemyStatus = (if t >= 0 then status s (at s.Foes t) else Map.empty)
          EnemyLifeFraction = (if t >= 0 then (at s.Foes t).Life / (at s.Foes t).Def.Life else 0.0)
          EnemyRarity = (if t >= 0 then (at s.Foes t).Def.Rarity else Normal)
          EnemiesAlive = s.AliveCount
          Energy = energyMap s
          LastUsed = (if s.A.LastUsed >= 0 then (at s.P.Skills s.A.LastUsed).Skill.Name else "") }

    /// The player is free to act: the first step of the rotation that can be used and paid
    /// for is used (`Sim.onReadyUnlocked`).
    let private act (s: State) (rng: Rng) =
        let p = s.P
        let v = view s
        let t = aimed s
        // what a use of the skill costs now: base x increased cost / (1 + cost efficiency)
        let costAt (sid: int) (seconds: unit -> float) =
            let info = (at p.Skills sid)
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
            // crossbow ammunition: the cost is for loading, paid with the first shot of a new load
            let ownCost = if skill.Clip > 0 && not (at s.A.LoadDue sid) then 0.0 else skill.Cost
            let ownCost =
                if skill.CostPerSecond <= 0.0 then ownCost
                else (if s.A.LastUsed = sid then 0.0 else ownCost) + skill.CostPerSecond * seconds ()
            (ownCost + extraCost) * costMult / efficiency
        let costOf (sid: int) = costAt sid (fun () -> max Constants.serverTick (useTime s sid))
        let affordable (sid: int) =
            match p.Vitals with
            | Some pools when pools.CostsInfernalFlame -> true
            | Some pools when pools.CostsLife -> (let c = costOf sid in c <= 0.0 || s.Life - c >= 1.0)
            | Some _ -> s.Mana >= costOf sid
            | None -> true
        let mutable firstUsable = -1
        let mutable choice = -1
        // (every step's condition is asked, as the reference asks them all)
        for i__38 in 0 .. p.Rotation.Length - 1 do
            let (sid, cond) = at p.Rotation i__38
            let info = (at p.Skills sid)
            let usable =
                cond v && ready s sid
                // a skill that works on a corpse needs one, unless it does without
                && (info.NeedsCorpse = 0 || corpses s > 0 || (info.NeedsCorpse = 2 && held s info.CorpseElse))
                && (not info.NeedsCharge || info.Consumes < 0 || held s info.Consumes)
                // a warp needs something to warp into: an enemy that can be culled, or a projectile
                && (not info.Warps || info.WarpsInto |> Array.exists (fun skill -> (at s.A.InFlight skill) > 0) || (t >= 0 && v.EnemyStatus.ContainsKey "Cullable"))
                // a form one is in is not pressed again (that would end it)
                && (info.Form < 0 || not (at s.A.BHas info.Form))
                // a seed still in an enemy is not planted over
                && (s.Seeds.IsEmpty || not (s.Seeds |> List.exists (fun (a, b, _, _, _, _) -> a = s.Cur && b = sid)))
                // the Glory or Combo it needs has been built
                && (info.Requires.Length = 0 || info.Requires |> Array.forall (fun (key, n) -> (Map.tryFind key s.A.Energy |> Option.defaultValue 0.0) >= n - 1e-9))
                // a skill that spends what has piled up on the enemy waits for the pile to be full
                && (info.NeedsStacks.Length = 0 || info.NeedsStacks |> Array.forall (fun (status, n) -> t >= 0 && (Map.tryFind status v.EnemyStatus |> Option.defaultValue 0) >= n))
            if usable then
                if firstUsable < 0 then firstUsable <- sid
                if choice < 0 && affordable sid then choice <- sid
        let choice = if choice >= 0 then choice else firstUsable
        if choice >= 0 then
            let sid = choice
            let info = (at p.Skills sid)
            // nothing is faster than a server tick (instant skills)
            let t = max Constants.serverTick (useTime s sid)
            let go () =
                let clip = info.Skill.Clip
                if clip <= 0 then
                    startUse s rng sid t
                    schedule s (s.Time + t + 0.0) EReady 0 0 0.0 0.0
                else
                    // after the last bolt of a load: reload (attack speed and reload speed shorten
                    // it), unless the reload is instant
                    // (asked of the mods as they are before the use, with the buffs as they are after it)
                    let ev = acquire s sid
                    startUse s rng sid t
                    ev.BOn <- true
                    ev.BHas <- s.A.BHas
                    ev.BStacks <- s.A.BStacks
                    s.A.BKept <- true
                    let left = if (at s.A.Ammo sid) < 0 then clip else (at s.A.Ammo sid)
                    let left = if rng.Chance(sumQ ev qKeepAmmo) then left else left - 1
                    put s.A.Ammo sid left
                    put s.A.LoadDue sid false
                    let extra =
                        if left > 0 then 0.0
                        else
                            let speed = (1.0 + (sumQ ev qSpeed + sumQ ev qReloadSpeed) / 100.0) * moreQ ev (qSpeed + 1) * moreQ ev (qReloadSpeed + 1)
                            let seconds = if rng.Chance(sumQ ev qInstantReload) then 0.0 else info.Skill.ReloadTime / max 0.1 speed
                            put s.A.Ammo sid clip
                            put s.A.LoadDue sid true
                            setBuff s p.BReloaded 1 (s.Time + t + seconds + 4.0) []
                            expireAt s (s.Time + t + seconds + 4.0)
                            bump s info.CReload
                            seconds
                    release s
                    schedule s (s.Time + t + extra) EReady 0 0 0.0 0.0
            match p.Vitals with
            | None -> go ()
            | Some pools ->
                let cost = costAt sid (fun () -> t)
                // drink the mana flask when mana is getting low and it isn't already running
                (match p.Actor.ManaFlask with
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
                elif pools.CostsInfernalFlame then
                    // Pyromantic Pact: the cost builds Infernal Flame (max: twice max mana); reaching the
                    // maximum empties it and burns maximum life + energy shield as fire damage
                    let flame = s.InfernalFlame + cost
                    if flame >= 2.0 * pools.Mana then
                        // a fire hit on yourself: fire resistance applies
                        let fireRes = Map.tryFind Fire pools.Resist |> Option.defaultValue 0.0
                        let burn = (pools.Life + pools.EnergyShield) * p.InfernalBurn / 100.0 * (1.0 - fireRes / 100.0)
                        s.InfernalFlame <- 0.0
                        bump s p.CInfernalBurn
                        takeDamage s pools (Map.ofList [ Fire, burn ])
                    else s.InfernalFlame <- flame
                    go ()
                elif s.Mana < cost && cost > pools.Mana then
                    // costs more than the whole mana pool: this skill can never be used
                    bump s info.CCannotAfford
                    put s.A.Cooldowns sid (infinity)
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
                    if p.HasSigil then sigilSpend s cost pools.Mana
                    go ()
        else
            // Nothing usable: wait for the next cooldown, or a server tick.
            let mutable next = infinity
            for i__39 in 0 .. s.A.Cooldowns.Length - 1 do
                let t = at s.A.Cooldowns i__39
                if t > s.Time && t < infinity && t < next then next <- t
            if s.A.ArchonRecovery > s.Time && s.A.ArchonRecovery < infinity && s.A.ArchonRecovery < next then next <- s.A.ArchonRecovery
            for i__40 in 0 .. s.A.InvocationReady.Length - 1 do
                let t = at s.A.InvocationReady i__40
                if t > s.Time && t < infinity && t < next then next <- t
            schedule s (if next < infinity then next else s.Time + Constants.serverTick) EReady 0 0 0.0 0.0

    /// Has this melee minion no room at the enemy aimed at? The melee minions in the fight
    /// take their places in order (`Sim.crowdedOut`). Who has room is worked out when the
    /// enemy aimed at is another, or a minion has come or gone, not at every turn.
    let private crowdedOut (s: State) : bool =
        match s.P.Body with
        | Some body when body.Melee ->
            let t = aimed s
            if t < 0 then false
            else
                if s.RoomAt <> t then
                    let melee =
                        [ for i in s.P.Bodies do
                            let b = (at s.Progs i).Body.Value
                            if b.Melee && not (at s.Agents i).Gone then yield i, (b.Size, b.Reach) ]
                    let places = Geometry.meleePlaces (at s.Foes t).Def.Size (melee |> List.map snd)
                    for i in 0 .. s.NoRoom.Length - 1 do
                        put s.NoRoom i false
                    List.iter2 (fun (i, _) has -> put s.NoRoom i (not has)) melee places
                    s.RoomAt <- t
                at s.NoRoom s.Cur
        | _ -> false

    /// An actor's turn comes (`Sim.onReady`): a stunned or frozen player waits, a totem
    /// that is not placed waits, a melee minion that finds no room at the enemy waits.
    let private onReady (s: State) (rng: Rng) =
        let p = s.P
        let standing =
            p.ActiveBuff < 0
            || (let player = at s.Agents 0 in (at player.BHas p.ActiveBuff) && (at player.BExpires p.ActiveBuff) > s.Time)
        if p.IsPlayer && p.Vitals.IsSome && s.LockedUntil > s.Time then schedule s s.LockedUntil EReady 0 0 0.0 0.0
        elif not standing then schedule s (s.Time + 0.1) EReady 0 0 0.0 0.0
        elif crowdedOut s then
            bump s p.CMinionNoRoom
            schedule s (s.Time + Constants.crowdedWait) EReady 0 0 0.0 0.0
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
                put n.MaxRes i (nan)
                put n.Res i (nan)
                put n.Taken i (nan)
                put n.TakenInc i (nan)
                put n.TakenMore i (nan)
            for i in 0 .. 12 do
                put n.Poise i (nan)
            n.Armour <- nan
            n.PhysFlat <- nan
            n.Threshold <- nan
            n.Flam <- nan
        e

    let private nMaxRes (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN (at n.MaxRes t) then
            put n.MaxRes t (min Constants.resistCeiling (Constants.enemyBaseMaxResist + dsum e Base (at maxResistOf t) 0.0))
        (at n.MaxRes t)
    let private nRes (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN n.Res.[t] then n.Res.[t] <- dsum e Base (at resistOf t) 0.0
        (at n.Res t)
    /// `Hit.damageTakenMult`, with the attacker's increase first.
    let private nTaken (e: Foe) (t: int) =
        let n = e.Nums
        if System.Double.IsNaN (at n.Taken t) then
            let mutable inc = if n.X <> 0.0 then 0.0 + n.X else 0.0
            let mutable more = 1.0
            inc <- dsum e Inc takenAll inc
            more <- dmore e takenAll more
            inc <- dsum e Inc (at takenOf t) inc
            more <- dmore e (at takenOf t) more
            if t >= 1 && t <= 3 then
                inc <- dsum e Inc takenElemental inc
                more <- dmore e takenElemental more
            put n.TakenInc t (inc)
            put n.TakenMore t (more)
            put n.Taken t (max 0.0 ((1.0 + inc / 100.0) * more))
        (at n.Taken t)
    let private nThreshold (e: Foe) =
        let n = e.Nums
        if System.Double.IsNaN n.Threshold then n.Threshold <- Constants.ailmentThreshold e.Info.Def.Level * dmult e thresholdStats
        n.Threshold
    let private nPoise (e: Foe) (a: Ailment) =
        let n = e.Nums
        let i = ailmentIndex a
        if System.Double.IsNaN n.Poise.[i] then n.Poise.[i] <- Constants.poiseThreshold e.Info.Def.Level * dmult e (at poiseStats i)
        (at n.Poise i)

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
        let res = nRes n t + sumQ ev (at qEnemyResist t) |> min maxRes |> max Constants.resistFloor
        let pen = sumQ ev (at qPen t)
        let floor' =
            if t >= 1 && t <= 3 then
                let f = qOver ev (at ev.Q qPenFloorOver)
                if ev.Found then f else min 0.0 (sumQ ev qPenFloor)
            else 0.0
        if res > floor' then max floor' (res - pen) else res

    /// The share (%) of a hit's damage of a type that the enemy does not take (`Hit.mitigateWith`).
    let private reduction (ev: Ev) (n: Foe) (negate: bool) (t: int) (v: float) =
        if t = 0 then physicalReduction n v
        else
            let r = effectiveResist ev n t
            let elemental = t >= 1 && t <= 3
            let ignored = r > 0.0 && ((elemental && flagQ ev qIgnoreElemental) || flagQ ev (at qIgnoreResist t))
            if ignored then 0.0
            elif negate && elemental && r > 0.0 then -r
            else r

    /// `Hit.mitigateWith`: what the enemy takes of `un`, into `de`.
    let private mitigate (ev: Ev) (n: Foe) (negate: bool) (has: bool[]) (un: float[]) (de: float[]) =
        for t in 0 .. 4 do
            if (at has t) then
                let v = (at un t)
                put de t (v * (1.0 - reduction ev n negate t v / 100.0) * nTaken n t)
            else de.[t] <- 0.0

    let private anyDyn (ev: Ev) (slots: int[]) =
        let mutable any = false
        for i__41 in 0 .. slots.Length - 1 do
            let slot = at slots i__41
            if (at ev.Dyn slot).Length > 0 then any <- true
        any

    /// `Hit.ranges`: the damage ranges ready to roll, into the ev's `RMin`, `RMax`, `RHas`.
    let private rangesAnew (p: Program) (ev: Ev) (info: SkillInfo) =
        let skill = info.Skill
        let rMin, rMax, rHas = ev.RMin, ev.RMax, ev.RHas
        let oMin, oMax, oHas = ev.OMin, ev.OMax, ev.OHas
        let addedEffect = skill.AddedDamageEffectiveness * multQ ev qAddedEffect
        for t in 0 .. 4 do
            put rMin t (((at info.BaseMin t) + sumQ ev (at qAddedMin t) * addedEffect) * skill.BaseMultiplier)
            put rMax t (((at info.BaseMax t) + sumQ ev (at qAddedMax t) * addedEffect) * skill.BaseMultiplier)
            put rHas t ((at rMax t) > 0.0)
        // "gain as extra" is taken from the base damage first
        if info.HasGain || anyDyn ev p.GainSlots then
            for t in 0 .. 4 do
                put oMin t ((at rMin t))
                put oMax t ((at rMax t))
                put oHas t ((at rHas t))
            for from in 0 .. 4 do
                if (at oHas from) then
                    for into in 0 .. 4 do
                        let pct = sumQ ev (at qGain (from * 5 + into))
                        if pct > 0.0 then
                            let k = pct / 100.0
                            put rMin into ((if (at rHas into) then (at rMin into) else 0.0) + (at oMin from) * k)
                            put rMax into ((if (at rHas into) then (at rMax into) else 0.0) + (at oMax from) * k)
                            put rHas into (true)
        // conversion: the skill's own first, then everything else on the result
        if info.HasConvert || anyDyn ev p.ConvSlots then
            for step in 0 .. 1 do
                let q = if step = 0 then qSkillConvert else qConvert
                for t in 0 .. 4 do
                    put oMin t ((at rMin t))
                    put oMax t ((at rMax t))
                    put oHas t ((at rHas t))
                    put rHas t (false)
                for from in 0 .. 4 do
                    if (at oHas from) then
                        let shares = ev.Share
                        let mutable total = 0.0
                        for into in 0 .. 4 do
                            if into <> from then
                                let v = max 0.0 (sumQ ev (at q (from * 5 + into))) / 100.0
                                put shares into v
                                total <- total + v
                        let mutable gone = 0.0
                        for into in 0 .. 4 do
                            if into <> from then
                                let k = if total > 1.0 then (at shares into) / total else (at shares into)
                                put shares into k
                                if k > 0.0 then gone <- gone + k
                        let put (into: int) (k: float) =
                            if k > 0.0 then
                                put rMin into ((if (at rHas into) then (at rMin into) else 0.0) + (at oMin from) * k)
                                put rMax into ((if (at rHas into) then (at rMax into) else 0.0) + (at oMax from) * k)
                                put rHas into (true)
                        put from (1.0 - gone)
                        for into in 0 .. 4 do
                            if into <> from then put into (at shares into)
            for t in 0 .. 4 do
                if rHas.[t] && not (rMax.[t] > 0.0) then rHas.[t] <- false
        // Avatar of Fire: "Deal no Non-Fire Damage"
        if flagQ ev qNoNonFire then
            for t in 0 .. 4 do
                if t <> 3 then rHas.[t] <- false

    /// The same, kept like the answers it is made of: good while the buffs and the enemy's
    /// statuses are what they were, where any of those answers can change with them (see
    /// `keepAnswers`; the ranges stand after the answers).
    let private ranges (p: Program) (ev: Ev) (info: SkillInfo) =
        if not ev.Keeping then rangesAnew p ev info
        else
            let rMin, rMax, rHas = ev.RMin, ev.RMax, ev.RHas
            let stamp = ev.Q.Length * 2 + (if ev.Crit then 1 else 0)
            let from = ev.Q.Length * 2 + (if ev.Crit then 15 else 0)
            if (at ev.KBuffs stamp) >= 0
               && (not info.RangesOnBuffs || (at ev.KBuffs stamp) = ev.GBuffs)
               && (not (info.RangesOnStatus || (info.RangesHasSlot && ev.DynOnStatus)) || (at ev.KStatus stamp) = ev.GStatus) then
                for t in 0 .. 4 do
                    put rMin t (at ev.KVal (from + t))
                    put rMax t (at ev.KVal (from + 5 + t))
                    put rHas t ((at ev.KVal (from + 10 + t)) <> 0.0)
            else
                rangesAnew p ev info
                for t in 0 .. 4 do
                    put ev.KVal (from + t) (at rMin t)
                    put ev.KVal (from + 5 + t) (at rMax t)
                    put ev.KVal (from + 10 + t) (if (at rHas t) then 1.0 else 0.0)
                put ev.KBuffs stamp ev.GBuffs
                put ev.KStatus stamp ev.GStatus

    /// `Hit.critMultiplier`.
    let private critMultiplier (ev: Ev) =
        let bonus = Constants.playerBaseCritBonus + sumQ ev qCritBonus
        1.0 + max 0.0 (bonus * multQ ev qCritBonusMult) / 100.0

    /// `Hit.critChance` (asked before it is known whether the hit is a critical one).
    let private critChance (ev: Ev) (info: SkillInfo) =
        ev.Crit <- false
        let v = qOver ev (at ev.Q qCritOver)
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
            put has t ((at ev.RHas t))
            if (at has t) then
                let lo, hi = (at ev.RMin t), (at ev.RMax t)
                let rolled =
                    if lucky then
                        let a = rng.Between(lo, hi)
                        let b = rng.Between(lo, hi)
                        max a b
                    else rng.Between(lo, hi)
                put un t (rolled * multQ ev (at qDamage t) * critMult * (if doubled then 2.0 else 1.0))
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
                if has.[t] then un.[t] <- (at un t) * (1.0 + 2.0 * (full - 1.0)) / full
        elif rerolls <> 0 then
            let full = critMultiplier ev
            let kept = max 0.0 (1.0 - Constants.inevitableCritLessPerReroll * float rerolls)
            for t in 0 .. 4 do
                if has.[t] then un.[t] <- (at un t) * (1.0 + (full - 1.0) * kept) / full
        mitigate ev n (rng.Chance(sumQ ev qNegate)) has un de
        crit

    let private flammabilityStat = Named "Flammability"

    let private total5 (xs: float[]) = 0.0 + (at xs 0) + (at xs 1) + (at xs 2) + (at xs 3) + (at xs 4)

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
            | _ -> (at scalesOf (ailmentIndex a))
        let mutable sum = 0.0
        for i__42 in 0 .. types.Length - 1 do
            let t = at types i__42
            sum <- sum + (at un t)
        sum

    let private ailmentDuration (ev: Ev) (a: Ailment) =
        let baseD =
            match a with
            | Bleed -> (let v = qOver ev (at ev.Q qBleedDuration) in if ev.Found then v else Ailments.baseDuration a)
            | Poison -> (let v = qOver ev (at ev.Q qPoisonDuration) in if ev.Found then v else Ailments.baseDuration a)
            | _ -> Ailments.baseDuration a
        baseD * multQ ev (at qDuration (ailmentIndex a))

    /// The chance (%) that a hit inflicts the ailment (see `fromHit`).
    let private ailmentChance (ev: Ev) (n: Foe) (flammable: float) (threshold: float) (a: Ailment) (un: float[]) : float =
        let i = ailmentIndex a
        let baseChance, mult = sumQ ev (at qAilChance i), multQ ev (at qAilChanceMult i)
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

    /// `Ailments.fromHit`: everything the hit inflicts. `flammable`: the Flammability earlier
    /// hits left on the enemy.
    let private fromHit (p: Program) (ev: Ev) (info: SkillInfo) (rng: Rng) (n: Foe) (flammable: float) (crit: bool) (un: float[]) : ResizeArray<Inflict> =
        ev.Crit <- crit
        let threshold = nThreshold n
        // (written out as loops: a list made from a sequence is slow in JavaScript)
        let out = ev.Inflicted
        out.Clear()
        for i__43 in 0 .. damagingAilments.Length - 1 do
            let a = at damagingAilments i__43
            if source ev a un > 0.0 && rng.Chance(ailmentChance ev n flammable threshold a un) then
                let faster = incQ ev (if a = Ignite then qFasterIgnite else qFaster)
                let dps = source ev a un * Ailments.percentPerSecond a / 100.0 * multQ ev (at qMagnitude (ailmentIndex a)) * faster
                out.Add(Dot(a, Ailments.dealsType a, dps, ailmentDuration ev a / max 0.1 faster, 1 + int (sumQ ev (at qAilStacks (ailmentIndex a)))))
        if source ev Shock un > 0.0 && rng.Chance(ailmentChance ev n flammable threshold Shock un) then
            out.Add(Status(Shock, Constants.baseShockMagnitude * multQ ev (at qMagnitude (ailmentIndex Shock)), ailmentDuration ev Shock))
        (let cold = source ev Chill un
         let effect = Constants.chillEffectMultiplier * cold / threshold * multQ ev (at qMagnitude (ailmentIndex Chill))
         if effect >= 1.0 then
            out.Add(Status(Chill, min Constants.chillMaxEffect (max Constants.chillMinEffect effect), ailmentDuration ev Chill)))
        for i__44 in 0 .. flatAilments.Length - 1 do
            let a = at flatAilments i__44
            if rng.Chance(ailmentChance ev n flammable threshold a un) then out.Add(Status(a, 0.0, ailmentDuration ev a))
        for i__45 in 0 .. buildupAilments.Length - 1 do
            let a = at buildupAilments i__45
            let enabled =
                match a with
                | Freeze | HeavyStun -> true
                | Electrocute -> flagQ ev qCanElectrocute
                | Pin -> flagQ ev qCanPin
                | _ -> false
            let dmg =
                match a with
                | HeavyStun ->
                    (source ev a un + (at un 0) * Constants.playerPhysicalStunMore / 100.0)
                    * (if info.IsMelee then 1.0 + Constants.playerMeleeStunMore / 100.0 else 1.0)
                    * (if hasStatus ev (at p.AilmentStatus (ailmentIndex Daze)) then 1.5 else 1.0)
                | _ -> source ev a un
            if enabled && dmg > 0.0 then
                out.Add(AddBuildup(a, dmg * Constants.buildupScale a / nPoise n a * multQ ev (at qBuildup (ailmentIndex a)) * 100.0))
        out

    /// Whether a hit inflicted the ailment, or (on a killing hit) built it up to the full.
    let private inflictedNow (inflicted: ResizeArray<Inflict>) (e: Foe) (a: Ailment) : bool =
        let mutable found = false
        for k in 0 .. inflicted.Count - 1 do
            match inflicted.[k] with
            | Dot (x, _, _, _, _) | Status (x, _, _) -> if x = a then found <- true
            | AddBuildup (x, pct) -> if x = a && (at e.Buildup (ailmentIndex x)) + pct >= 100.0 then found <- true
        found

    /// Whether a hit ignited.
    let private ignitedBy (inflicted: ResizeArray<Inflict>) : bool =
        let mutable ignited = false
        for k in 0 .. inflicted.Count - 1 do
            match inflicted.[k] with
            | Dot (Ignite, _, _, _, _) -> ignited <- true
            | _ -> ()
        ignited

    /// Whether a hit inflicted the ailment as damage over time or as a status.
    let private inflictedPlain (inflicted: ResizeArray<Inflict>) (a: Ailment) : bool =
        let mutable found = false
        for k in 0 .. inflicted.Count - 1 do
            match inflicted.[k] with
            | Dot (x, _, _, _, _) | Status (x, _, _) -> if x = a then found <- true
            | AddBuildup _ -> ()
        found

    /// A hit takes a state off the enemy (see `hitEnemy`): what the hit still has to read of
    /// the enemy as it was is kept (in `State.ShockWas`, `State.BrokenWas`, and the enemy's
    /// mods as a defender stay listed as they are until the hit is over). `already`: whether
    /// the hit has taken a state before. Whether the enemy had the state as a debuff.
    let private takeState (s: State) (e: Foe) (id: int) (already: bool) : bool =
        if id >= 0 && (at e.Has id) then
            if not already then
                let p = s.P
                defList s e
                // (the enemy's mods as the reference has them are only asked where a shock is of an odd kind)
                if e.ShockOdd then defender s e |> ignore
                s.ShockWas <- (if (at e.Has p.SShocked) && (at e.Expires p.SShocked) > s.Time then (at e.DMods p.SShocked) |> List.sumBy (fun m -> m.Value) else 0.0)
                s.BrokenWas <- (if (at e.Has p.SArmourBreak) && (at e.Expires p.SArmourBreak) > s.Time then -((at e.DMods p.SArmourBreak) |> List.sumBy (fun m -> m.Value)) else 0.0)
            put e.Has id false
            put e.Stacks id 0
            put e.DMods id []
            e.StatusStale <- true
            true
        else false

    /// The statuses in two arrays as the map the reference has.
    let private statusMap (p: Program) (has: bool[]) (stacks: int[]) : Map<string, int> =
        let mutable m = Map.empty
        for i in 0 .. has.Length - 1 do
            if (at has i) then m <- Map.add (at p.StatusNames i) (at stacks i) m
        m

    /// One hit on one enemy (`Sim.hitEnemy`, in its order).
    let private hitEnemy (s: State) (rng: Rng) (sid: int) (mult: float) (target: int) =
        let e = (at s.Foes target)
        if e.Life > 0.0 then
            let p = s.P
            let info = (at p.Skills sid)
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
            for i__46 in 0 .. s.Live.Length - 1 do
                let o = at s.Foes (at s.Live i__46)
                if o.Life > 0.0 && o.Index <> e.Index && not (Vec.dist o.Def.Position def0.Position > 60.0) then alone <- false
            ev.Iso <- alone
            ev.Close <- Vec.length def0.Position - def0.Size <= 20.0
            // Revealed Weaknesses: an Open Weakness for the share of the time the enemy's life is in one
            if info.CanReveal then
                let evE = acquireEmpty s sid
                let more = sumQ evE qOpenMore
                if def0.Rarity <> Normal && def0.Rarity <> Magic && flagQ evE qReveal then
                    let c, m = Constants.revealedWeaknessLifeShare, 1.0 + more / 100.0
                    if rng.Float() < (c / m) / (1.0 - c + c / m) then ev.Open <- true
                release s
            keepAnswers s ev sid target 0
            let hitChance =
                let v = qOver ev (at ev.Q qHitChance)
                if ev.Found then v
                elif info.IsAttack then
                    let accuracy = sumQ ev qAccuracy * multQ ev qAccuracyMult
                    if accuracy <= 0.0 then 100.0
                    else
                        let evasion =
                            e.Info.Evasion
                            * (if hasStatus ev (at p.AilmentStatus (ailmentIndex Blind)) then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                            * (if hasStatus ev (at p.AilmentStatus (ailmentIndex Maim)) then 1.0 - Constants.maimEvasionReduced / 100.0 else 1.0)
                        let distance = max 0.0 (Vec.length def0.Position - def0.Size)
                        Hit.accuracyHitChance evasion (Hit.accuracyAtDistance distance accuracy)
                else 100.0
            // a part that only happens against an enemy in some state: nothing otherwise; and one
            // that gives way to another part against a state
            // (a warp into a projectile explodes there, whatever the enemy's state)
            let warped = info.Warped >= 0 && (at s.A.BHas info.Warped)
            if warped then removeBuff s info.Warped
            let mutable met = warped || info.OnlyVs.Length = 0
            for k in 0 .. info.OnlyVs.Length - 1 do
                if hasStatus ev (at info.OnlyVs k) then met <- true
            for k in 0 .. info.NotVs.Length - 1 do
                if hasStatus ev (at info.NotVs k) then met <- false
            // a state the hit takes off the enemy: the hit itself still meets the enemy as it
            // was (its mods as a defender, its shock, its broken armour), what the hit leaves
            // on it finds the state gone
            let mutable taken = false
            if met && not warped && info.ConsumesVs && info.OnlyVs.Length > 0 then
                let mutable first = -1
                for k in 0 .. info.OnlyVs.Length - 1 do
                    if first < 0 && hasStatus ev (at info.OnlyVs k) then first <- k
                if takeState s e (at info.OnlyVs first) taken then taken <- true
                bump s (at info.CConsumedVs first)
            if not met then ()
            elif not (lands s rng e hitChance) then bump s info.CMiss
            else
                // a hit that takes a state off the enemy without needing it ("Consumes Shock")
                if info.Takes >= 0 && hasStatus ev info.Takes then
                    if takeState s e info.Takes taken then taken <- true
                    bump s info.CTakes
                // "Enemies you Curse / Mark take X% increased Damage": additive with what the enemy already takes
                let nums = numsFor s e (sumQ ev qEnemyTaken)
                let critRoll = if info.Sustained || not (at s.A.HasCritRoll sid) then nan else (at s.A.CritRoll sid)
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
                        if has.[t] then de.[t] <- (at de t) + lifeBefore
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
                    if (at has t) then
                        put un t ((at un t) * mult)
                        put de t ((at de t) * mult)
                // ailments are rolled before the damage lands
                while e.FlamFrom < e.FlamTo && not ((at e.FlamUntil e.FlamFrom) > s.Time) do
                    e.FlamSum <- e.FlamSum - (at e.FlamValue e.FlamFrom)
                    e.FlamFrom <- e.FlamFrom + 1
                if e.FlamFrom = e.FlamTo then e.FlamSum <- 0.0
                let flammable = e.FlamSum
                let inflicted = fromHit p ev info rng nums flammable crit un
                // the Flammability this hit leaves (`Ailments.flammability`)
                (ev.Crit <- crit
                 let fire = source ev Ignite un
                 if fire > 0.0 then
                    let f = min 100.0 (max 0.0 ((fire / nThreshold nums * Constants.igniteChanceMultiplier + sumQ ev (at qAilChance 0)) * multQ ev (at qAilChanceMult 0)))
                    if f > 0.0 then
                        if e.FlamTo = e.FlamValue.Length then
                            // (what is gone makes room, or the arrays grow)
                            let live = e.FlamTo - e.FlamFrom
                            if live * 2 > e.FlamValue.Length then
                                let values, untils = Array.zeroCreate (live * 2), Array.zeroCreate (live * 2)
                                for i in 0 .. live - 1 do
                                    put values i (at e.FlamValue (e.FlamFrom + i))
                                    put untils i (at e.FlamUntil (e.FlamFrom + i))
                                e.FlamValue <- values
                                e.FlamUntil <- untils
                            else
                                for i in 0 .. live - 1 do
                                    put e.FlamValue i (at e.FlamValue (e.FlamFrom + i))
                                    put e.FlamUntil i (at e.FlamUntil (e.FlamFrom + i))
                            e.FlamFrom <- 0
                            e.FlamTo <- live
                        put e.FlamValue e.FlamTo f
                        put e.FlamUntil e.FlamTo (s.Time + Constants.flammabilityDuration)
                        e.FlamTo <- e.FlamTo + 1
                        e.FlamSum <- e.FlamSum + f)
                ev.Crit <- false
                if flagQ ev qNoHitDamage then
                    for t in 0 .. 4 do
                        if (at has t) then
                            put un t (0.0)
                            put de t (0.0)
                // a shock the hit applies already counts for the hit itself
                (let currentShock =
                    if taken then s.ShockWas
                    elif (at e.Has p.SShocked) && (at e.Expires p.SShocked) > s.Time then (at e.DMods p.SShocked) |> List.sumBy (fun m -> m.Value) else 0.0
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
                            if (at has t) then
                                let v = (at un t)
                                let red = reduction ev nums false t v
                                nTaken nums t |> ignore
                                let taken = max 0.0 ((1.0 + ((at own.TakenInc t) - e.ShockInc + m) / 100.0) * (at own.TakenMore t))
                                put de t (v * (1.0 - red / 100.0) * taken)
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
                        let unmitigated = [ for t in 0 .. 4 do if (at has t) then yield (at damageTypes t), (at un t) ] |> Map.ofList
                        let dealt = Hit.mitigate ctx (skillDb s sid) def' unmitigated
                        for t in 0 .. 4 do
                            if has.[t] then de.[t] <- dealt.[damageTypes.[t]])
                let dealt = total5 de
                let life = lifeBefore - dealt
                let dead = life <= 1e-9
                (match s.Tell with Some tell -> tell s.Time (if crit then "crit" else "hit") def0.Name skill.Name dealt | None -> ())
                // what the armour break below reads of the enemy as it was before the hit
                let brokenBefore =
                    if taken then s.BrokenWas
                    elif (at e.Has p.SArmourBreak) && (at e.Expires p.SArmourBreak) > s.Time then -((at e.DMods p.SArmourBreak) |> List.sumBy (fun m -> m.Value)) else 0.0
                addDamage s info.Source (min dealt lifeBefore)
                // (only of a hit the enemy outlives: what a killing hit deals is the life that was left)
                if not dead then s.Evened <- s.Evened + dealt * (evenly - 1.0)
                // (a hit that deals more than the enemy has left: each type's share of what was left)
                (let k = if dealt > lifeBefore && dealt > 0.0 then lifeBefore / dealt else 1.0
                 for t in 0 .. 4 do
                    if (at has t) && (at de t) > 0.0 then addByType s t ((at de t) * k))
                bump s info.CHits
                if crit then bump s info.CCrits
                if dead then kill s e s.Time else e.Life <- life
                // overkill explosions: nearby enemies are ignited for a share of the overkill
                (let overkill = dealt - lifeBefore
                 if dead && overkill > 0.0 && p.Overkill.Length > 0 then
                    for b in p.Overkill do
                        if b.Fits.[sid] && overkill >= def0.Life * b.ThresholdPercent / 100.0 && b.Burst >= 0 then
                            // (the explosion's area and the burn's time are the explosion skill's, with the buffs as they are)
                            let bev = acquire s b.Burst
                            useBuffs s bev p.Skills.[b.Burst] true
                            let radius = if b.Radius > 0.0 || p.Skills.[b.Burst].IsAreaAtTarget then Geometry.radius b.Radius (multQ bev qArea) else 0.0
                            let seconds = b.Seconds * multQ bev qSkillDuration
                            release s
                            let dps = overkill * b.PercentPerSecond / 100.0
                            for k in 0 .. s.Live.Length - 1 do
                                let o = at s.Foes (at s.Live k)
                                if o.Life > 0.0 && o.Index <> e.Index && Vec.dist o.Def.Position def0.Position <= radius + o.Def.Size then
                                    bump s b.Ignited
                                    // (an ignite, counted as the explosion's damage)
                                    let d = { Ailing = Some Ignite; Label = b.Label; Of = Fire; Dps = dps; Until = s.Time + seconds; Most = 1 }
                                    o.Dots <- d :: o.Dots
                                    if not (d.Until >= o.Soonest) then o.Soonest <- d.Until
                                    expireAt s d.Until)
                // Armour Break: the hit breaks armour by a share of the damage it dealt; armour at
                // zero is Fully Broken: the enemy takes increased physical damage from hits
                (let share =
                    (at de 0) * sumQ ev qBreakPhysical / 100.0
                    + (at de 3) * sumQ ev qBreakFire / 100.0
                    + dealt * sumQ ev qBreakDamage / 100.0
                    + sumQ ev qBreakFlat
                 // "Players Break 3 times Armour against Normal Monsters and 2 times Armour against Magic Monsters"
                 let broken =
                    share * multQ ev qBreakAmount
                    * (if not p.IsPlayer then 1.0 else match def0.Rarity with Normal -> 3.0 | Magic -> 2.0 | _ -> 1.0)
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
                    let leech = min (at de 0) Constants.effectiveMaxDamageForLeech * sumQ ev qManaLeech / 100.0
                    // leech counts from one enemy per hit event: only the largest instance
                    let extra = max 0.0 (leech - s.StrikeLeech)
                    let charges =
                        match p.Actor.ManaFlask with
                        | Some f when dead -> min f.MaxCharges (s.FlaskCharges + RarityCharges.flaskChargesPerKill def0.Rarity * f.ChargeGainMult)
                        | _ -> s.FlaskCharges
                    let leechMult = multQ ev qManaLeechAmount
                    let extra = extra * leechMult
                    let lifeLeechFull =
                        min (at de 0) Constants.effectiveMaxDamageForLeech * sumQ ev qLifeLeech / 100.0
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
                            bump s (at p.CInflict (ailmentIndex a))
                            react s rng (at p.OnInflict (ailmentIndex a)) "" sid target
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
                    for i__47 in 0 .. info.Dots.Length - 1 do
                        let (t, perSecond, _, label) = at info.Dots i__47
                        let ti = typeIndex t
                        let dps =
                            if info.SpellDot then perSecond * multQ ev (at qDamage ti) * multQ ev qSpellDotDamage
                            else perSecond * multQ ev (at qDotDamageAs ti) * multQ ev qDotDamage
                        let d = { Ailing = None; Label = label; Of = t; Dps = dps; Until = s.Time + duration; Most = 1 }
                        e.Dots <- d :: (e.Dots |> List.filter (fun x -> x.Label <> label || x.Dps > dps))
                        if not (d.Until >= e.Soonest) then e.Soonest <- d.Until
                        expireAt s d.Until
                    ev.Tags <- skill.Tags
                    ev.AsAttack <- info.IsAttack
                    ev.Dyn <- asSkill
                // Glory and Combo: skills that can only be used once enough has been built count up here
                if p.Builders.Length > 0 then
                    let power = MonsterPower.ofRarity def0.Rarity
                    if not dead then refreshStatus s e
                    let afterHas = e.SHas
                    let newly (a: Ailment) = (let id = at p.AilmentStatus (ailmentIndex a) in not dead && not (at beforeHas id) && (at afterHas id))
                    // (what the gain is limited by: once every so often per monster, or per strike;
                    // and what is lost when nothing was gained for a while)
                    let add (key: string) (amount: float) (cap: float) (per: string) (every: float) (fade: float -> float -> float) =
                        if amount > 0.0 then
                            let stamp, lastKey = key + "@" + per, key + "@last"
                            let since (k: string) = s.Time - (Map.tryFind k s.A.Energy |> Option.defaultValue -1e9)
                            if not (since stamp < every) then
                                let have = fade (Map.tryFind key s.A.Energy |> Option.defaultValue 0.0) (since lastKey)
                                s.A.Energy <- s.A.Energy |> Map.add key (min cap (have + amount)) |> Map.add stamp s.Time |> Map.add lastKey s.Time
                    for k in 0 .. p.Builders.Length - 1 do
                        let b = at p.Builders k
                        let ownHit = at b.Own sid
                        let glory =
                            if b.Glory <= 0.0 || ownHit then 0.0
                            else
                                power / 100.0
                                * ((if info.IsAttack then b.AttackHit else 0.0)
                                   + (if 0.0 + (at de 1) + (at de 2) + (at de 3) > 0.0 then b.ElementalHit else 0.0)
                                   + (if (at de 4) > 0.0 then b.ChaosHit else 0.0)
                                   + (0.0 + (if newly HeavyStun then b.Stunned else 0.0) + (if newly Freeze then b.Frozen else 0.0) + (if newly Ignite then b.Ignited else 0.0)))
                                + (0.0 + (if newly Ignite then b.FlatIgnited else 0.0) + (if newly Shock then b.FlatShocked else 0.0))
                        // Combo: from melee attack hits with other skills
                        let combo =
                            if b.Combo <= 0.0 || ownHit || not info.IsAttack || not (skill.Tags.Contains(Tag "Type:Melee")) then 0.0
                            else 1.0 + (if rng.Chance b.ComboExtraChance then 1.0 else 0.0)
                        // "Skills lose 2 Glory per second if they haven't gained Glory in the past 15 seconds"
                        add b.GloryKey glory b.GloryCap def0.Name 0.5 (fun have idle -> if idle > 15.0 && idle < 1e8 then max 0.0 (have - 2.0 * (idle - 15.0)) else have)
                        // Combo counts the strikes "in the last 8 seconds"
                        add b.ComboKey combo b.Combo "strike" 1e-9 (fun have idle -> if idle > 8.0 && idle < 1e8 then 0.0 else have)
                // Banners: attack hits build Glory; a full banner is placed at once
                if p.Banners.Length > 0 && info.IsAttack then
                    let power = MonsterPower.ofRarity def0.Rarity
                    for k in 0 .. p.Banners.Length - 1 do
                        let b, banner, placed = at p.Banners k
                        let key = "Glory:" + b.Name
                        // once every 0.5 seconds per monster
                        let stamp = key + "@" + def0.Name
                        let fresh = s.Time - (Map.tryFind stamp s.A.Energy |> Option.defaultValue -1e9) >= 0.5
                        if fresh then s.A.Energy <- Map.add stamp s.Time s.A.Energy
                        let have = (Map.tryFind key s.A.Energy |> Option.defaultValue 0.0) + (if fresh then power * b.PerAttackHit / 100.0 else 0.0)
                        if (at s.A.BHas banner) && (at s.A.BExpires banner) > s.Time then s.A.Energy <- Map.add key (min b.Glory have) s.A.Energy
                        elif have >= b.Glory then
                            s.A.Energy <- Map.add key 0.0 s.A.Energy
                            setBuff s banner 1 (s.Time + b.Duration) b.Mods
                            expireAt s (s.Time + b.Duration)
                            bump s placed
                        else s.A.Energy <- Map.add key have s.A.Energy
                // meta gem energy: crits and newly inflicted ailments, scaled by monster power and by
                // the share of the ailment threshold the hit deals; none from triggered skills
                if p.MetaHits && not info.IsTriggered then
                    let threshold = max 1.0 (nThreshold nums)
                    let power = MonsterPower.ofRarity def0.Rarity
                    if not dead then refreshStatus s e
                    let afterHas = e.SHas
                    for i in 0 .. p.Metas.Length - 1 do
                        let meta = at p.Metas i
                        if meta.FromHits then
                            let mutable amount = 0.0
                            for k in 0 .. meta.Sources.Length - 1 do
                                match at meta.Sources k with
                                | CritHits per when crit -> amount <- amount + per * power * (total5 un / threshold)
                                | MeleeHits per when info.IsMelee -> amount <- amount + per * power * (total5 un / threshold)
                                | MeleeKills per when dead && info.IsMelee -> amount <- amount + per * power
                                | InflictsAilment (a, per) ->
                                    let newly =
                                        // on a killing hit: what this hit inflicted, and buildup that would complete
                                        if dead then inflictedNow inflicted e a
                                        else (let id = at p.AilmentStatus (ailmentIndex a) in (at afterHas id) && not (at beforeHas id))
                                    if newly then amount <- amount + per * power * (source ev a un / threshold)
                                | _ -> ()
                            // (what this fills up and triggers is aimed at the enemy that was hit)
                            if amount > 0.0 then
                                let cause = s.Cause
                                s.Cause <- target
                                gainEnergy s rng i (amount * meta.Meta.EnergyMult)
                                s.Cause <- cause
                // infusion remnants: each freeze, shock or ignite inflicted may drop one
                if p.Remnants.Length > 0 then
                    if not dead then refreshStatus s e
                    let frozen = at p.AilmentStatus (ailmentIndex Freeze)
                    for k in 0 .. p.Remnants.Length - 1 do
                        let a, resource, chance, dropped = at p.Remnants k
                        let newly =
                            match a with
                            | Freeze ->
                                if dead then
                                    let mutable full = false
                                    for j in 0 .. inflicted.Count - 1 do
                                        match inflicted.[j] with
                                        | AddBuildup (Freeze, pct) -> if (at e.Buildup (ailmentIndex Freeze)) + pct >= 100.0 then full <- true
                                        | _ -> ()
                                    full
                                else (at e.SHas frozen) && not (at beforeHas frozen)
                            | _ -> inflictedPlain inflicted a
                        if newly && rng.Chance chance then
                            let n = if rng.Chance p.RemnantExtra then 2 else 1
                            bump s dropped
                            schedule s (s.Time + p.RemnantDelay) EGainStacks resource n 0.0 0.0
                // Archon: gained on an ignite by a skill with the chance; none while one is active
                // or during the recovery period after one ends
                (if p.Archons.Length > 0 && ignitedBy inflicted then
                    let active = p.Archons |> Array.exists (fun a -> (at s.A.BHas a.Buff))
                    let recovering = s.A.ArchonRecovery > s.Time
                    if not (active || recovering) then
                        let mutable found = -1
                        for i in 0 .. p.Archons.Length - 1 do
                            if found < 0 && rng.Chance(qSum ev (at info.QArchon i)) then found <- i
                        if found >= 0 then
                            let a = (at p.Archons found)
                            setBuff s a.Buff 1 (s.Time + a.Duration) a.AMods
                            s.A.ArchonRecovery <- s.Time + a.Duration + a.Recovery
                            expireAt s (s.Time + a.Duration)
                            bump s a.Gained)
                // charms: charges from kills, and the ones used by killing a rare or unique monster
                // Runic Ward remnants from kills and stuns
                (match p.WardRemnants with
                 | Some r when s.Main.Vitals.IsSome ->
                    let stunned = at p.AilmentStatus (ailmentIndex HeavyStun)
                    let stunnedNow = not dead && (refreshStatus s e; at e.SHas stunned) && not (at beforeHas stunned)
                    let fromKill = dead && rng.Chance r.OnKillChance
                    let fromStun = stunnedNow && s.Time - s.LastStunRemnant >= r.StunCooldown && rng.Chance r.OnStunChance
                    if fromKill || fromStun then
                        let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                        if fromStun then s.LastStunRemnant <- s.Time
                        bump s p.CWardRemnant
                        schedule s (s.Time + r.Delay) EWard 0 0 (r.WardPerRemnant * count) 0.0
                 | _ -> ())
                // mana remnants: from a critical hit on, or the kill of, an enemy with an elemental ailment
                (match p.ManaRemnants with
                 | Some r when s.Main.Vitals.IsSome ->
                    let had (a: Ailment) = at beforeHas (at p.AilmentStatus (ailmentIndex a))
                    let ailing = had Ignite || had Chill || had Freeze || had Shock || had Electrocute
                    let fromKill = dead && ailing && rng.Chance r.OnKillChance
                    let fromCrit = crit && ailing && s.Time - s.LastManaRemnant >= r.CritCooldown
                    if fromKill || fromCrit then
                        let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                        if fromCrit then s.LastManaRemnant <- s.Time
                        bump s p.CManaRemnant
                        let most = match p.Vitals with Some v -> v.Mana | None -> 0.0
                        schedule s (s.Time + r.Delay) EMana 0 0 ((r.ManaPerRemnant + r.PercentOfMaxMana / 100.0 * most) * count) 0.0
                 | _ -> ())
                // Trinity: the hit's strongest element gains resonance (up to 100), the others lose some
                if p.IsPlayer then
                    ev.Crit <- false
                    let gain = sumQ ev qTrinityGain
                    if gain > 0.0 && ((at un 1) > 0.0 || (at un 2) > 0.0 || (at un 3) > 0.0) then
                        // (lightning, cold, fire: of equals the first)
                        let mutable top = -1
                        for t in 1 .. 3 do
                            if (at un t) > 0.0 && (top < 0 || (at un t) > (at un top)) then top <- t
                        let loss = sumQ ev qTrinityLoss
                        let mutable sum = 0
                        for t in 1 .. 3 do
                            let b = at p.BResonance (t - 1)
                            let n = if t = top then min 100 (stacksOf s b + int gain) else max 0 (stacksOf s b - int loss)
                            if n > 0 then setBuff s b n infinity [] else removeBuff s b
                            sum <- sum + n
                        if sum > 0 then setBuff s p.BResonanceSum sum infinity [] else removeBuff s p.BResonanceSum
                // (the player's charms, whoever made the kill)
                if dead && s.Main.Charms.Length > 0 then
                    if s.Main.Vitals.IsSome then
                        for i__48 in 0 .. s.Main.Charms.Length - 1 do
                            let c = at s.Main.Charms i__48
                            // "granting charges equal to half of the monster's Power"
                            let gain = (MonsterPower.ofRarity def0.Rarity / 2.0 + (if rng.Chance c.Charm.ChargeOnKillChance then 1.0 else 0.0)) * c.Charm.ChargeGainMult
                            put s.CharmCharges c.CBuff (min c.Charm.MaxCharges ((at s.CharmCharges c.CBuff) + gain))
                    if def0.Rarity = Rare || def0.Rarity = Unique then charm s "KillRare" |> ignore
                // "Only one Hit every 0.5 seconds can cause you to gain Rage": when one last could
                // is kept, even where there is no Rage to gain
                // (Rage is the player's alone)
                if p.IsPlayer then
                    let fresh = s.Time - (Map.tryFind "RageHitAt" s.A.Energy |> Option.defaultValue -1e9) >= 0.5
                    let onHit = if fresh then sumQ ev qRageOnHit else 0.0
                    if onHit > 0.0 then s.A.Energy <- Map.add "RageHitAt" s.Time s.A.Energy
                    gainRage s (onHit + (if dead then p.RageOnKill else 0.0))
                // what the game tracks for "recently" conditions
                mark s p.BHit 4.0
                if not crit then mark s p.BNonCrit 4.0
                if crit then
                    mark s p.BCrit 4.0
                    mark s p.BCrit8 8.0
                if dead then mark s p.BKilled 4.0
                react s rng p.OnHit "" sid target
                if crit then react s rng p.OnCrit "" sid target
            // (from now on the enemy is met without what was taken off it)
            if taken then
                e.DefenderStale <- true
                e.DefVersion <- e.DefVersion + 1
            release s

    /// A strike lands: who is hit, and each hit (`Sim.onStrike`).
    let private onStrike (s: State) (rng: Rng) (sid: int) (mult: float) (aimedAt: int) (elapsed: float) (passing: bool) (direct: bool) =
        let p = s.P
        let alive = living s
        // Aim where the use was aimed; if that enemy died meanwhile, at the next one.
        let primary = if (at s.Foes aimedAt).Life > 0.0 then aimedAt else s.Aimed
        if primary >= 0 then
            let plain = (at p.Skills sid)
            let primaryDef = (at s.Foes primary).Def
            let shape (ev: Ev) (info: SkillInfo) =
                let skill = info.Skill
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
            // (the buffs are the ones the strike began with, whatever it and its hits change)
            let began = acquire s sid
            useBuffs s began plain true
            // what is asked of a skill for an impact of this strike: with the buffs, and their
            // mods, as they were when the strike began
            let ask (skill: int) : Ev =
                let info = at p.Skills skill
                let ev = acquire s skill
                ev.DynRaw <- began.DynRaw
                ev.Dyn <-
                    (if skill = sid then began.Dyn
                     elif obj.ReferenceEquals(began.Dyn, began.DynRaw) then began.DynRaw
                     else resolveDyn began.DynRaw info.Skill.Tags info.Skill.Name)
                ev.BOn <- true
                ev.BHas <- began.BHas
                ev.BStacks <- began.BStacks
                if info.StackExtra > 0 then
                    ev.Extra <- info.StackBuff
                    ev.ExtraN <- info.StackExtra
                ev
            // (the reference works the shape out twice, and each time rolls what it rolls)
            (match plain.Skill.Delivery with
             | Projectiles _ -> shape began plain |> ignore
             | _ -> ())
            // Infusions: each impact consumes one of the skill's element (or the one that does
            // in its place) and is the infused copy; without one, the plain skill
            let variant () : int =
                if plain.Infusion < 0 then sid
                else
                    let source =
                        if stacksOf s plain.Infusion > 0 then plain.Infusion
                        elif plain.Wildcard >= 0 && stacksOf s plain.Wildcard > 0 then plain.Wildcard
                        else -1
                    if source < 0 then
                        bump s plain.CUninfused
                        sid
                    else
                        let evE = acquireEmpty s plain.Infused
                        let keep = sumQ evE qInfusionKeep
                        let charges = int (sumQ evE qInfusionPower)
                        release s
                        bump s plain.CInfused
                        markStatus s p.BConsumedInfusion
                        // "Gain a Power Charge when you consume an Elemental Infusion"
                        if charges > 0 then schedule s s.Time EGainStacks (at p.BCharges 0) charges 0.0 0.0
                        if not (rng.Chance keep) then setStacks s false source (stacksOf s source - 1)
                        plain.Infused
            // Spell Cascade: extra impacts on either side of the target, along an axis chosen at random
            let cascades =
                match plain.Skill.Delivery with
                | AreaAtTarget _ -> int (sumQ began qCascades)
                | _ -> 0
            if cascades > 0 then
                let axis = Vec.ofAngle (rng.Float() * 2.0 * System.Math.PI)
                let mutable step = 0.0
                // the centre impact first (it takes the first infusion); its radius sets the spacing
                for place in 0 .. 2 * cascades do
                    let k = if place = 0 then 0 elif place % 2 = 1 then -((place + 1) / 2) else place / 2
                    let impact = variant ()
                    let info = at p.Skills impact
                    let ev = ask impact
                    let areaMult, delivery = shape ev info
                    let r = match delivery with AreaAtTarget r -> r | _ -> 0.0
                    let centreR = Geometry.radius r areaMult
                    if k = 0 then step <- centreR * p.Fight.CascadeSpacing
                    let reach = if k = 0 then centreR else Geometry.radius r (areaMult * moreQ ev qCascadeArea)
                    release s
                    let centre = Vec.add primaryDef.Position (Vec.scale (float k * step) axis)
                    let mutable rest = alive
                    while not rest.IsEmpty do
                        let d = rest.Head
                        if Vec.dist centre d.Position <= reach + d.Size then hitEnemy s rng impact mult s.FoeIds.[d.Name]
                        rest <- rest.Tail
            else
                let impact = variant ()
                let info = at p.Skills impact
                let ev = ask impact
                let areaMult, delivery = shape ev info
                let hitAll (names: string list) =
                    let mutable rest = names
                    while not rest.IsEmpty do
                        hitEnemy s rng impact mult s.FoeIds.[rest.Head]
                        rest <- rest.Tail
                let strike () =
                    if direct then
                        if primary = aimedAt then hitEnemy s rng impact mult aimedAt
                    else
                        match delivery with
                        | Projectiles pr when not passing && pr.Passing > 0.0 && pr.HitInterval > 0.0 ->
                            // what is inside the projectiles as they travel: every hit at its own time
                            for (t, target) in Geometry.dwellHits rng p.Fight.Walls pr p.Fight.Reach areaMult primaryDef alive do
                                if t <= 0.0 then hitEnemy s rng impact mult s.FoeIds.[target]
                                else schedule s (s.Time + t) EDirect sid s.FoeIds.[target] mult 0.0
                        | Projectiles pr when passing && pr.Passing > 0.0 ->
                            // where the projectile is by now, on its way towards where it was aimed
                            let spot = Vec.scale (pr.Speed * elapsed) (Vec.ofAngle (Vec.angle primaryDef.Position))
                            alive |> List.filter (Geometry.within spot (Geometry.radius pr.Passing areaMult)) |> List.map (fun e -> e.Name) |> hitAll
                        | Single -> hitEnemy s rng impact mult primary
                        | _ -> Geometry.targetsFrom rng p.Fight.Walls p.Fight.Reach delivery areaMult primaryDef alive |> hitAll
                strike ()
                // Aftershock: a slam has a chance to hit its area once more
                if info.IsSlam && rng.Chance(sumQ ev qAftershock) then
                    strike ()
                    bump s info.CAftershock
                release s
            release s

    /// A minion is killed: it leaves the fight; a reviving one comes back with the others
    /// once none has died for its revive time (`Sim.killMinion`).
    let private killMinion (s: State) (rng: Rng) (who: int) =
        let body = (at s.Progs who).Body.Value
        leave s who
        let was = s.Cur
        enter s 0
        let p = s.P
        bump s p.CMinionDied
        markStatus s p.BMinionDied
        (match body.Revive with
         | None -> ()
         | Some seconds ->
            // "gains Energy when one of your Persistent Minions is Killed"
            for i in 0 .. p.Metas.Length - 1 do
                let meta = (at p.Metas i).Meta
                let amount = meta.Sources |> List.sumBy (fun src -> match src with MinionDeaths per -> per * body.Power | _ -> 0.0)
                if amount > 0.0 then gainEnergy s rng i (amount * meta.EnergyMult)
            let due, n = Sim.nextRevival s.Time seconds s.Revival
            s.Revival <- Some(due, n)
            schedule s due ERevive 0 0 due 0.0)
        enter s was

    /// Killed minions come back, if no other has died since this was set (`Sim.reviveMinions`).
    let private reviveMinions (s: State) (due: float) =
        match s.Revival with
        | Some (t, _) when abs (t - due) < 1e-9 ->
            s.Revival <- None
            s.RoomAt <- -2
            let p = s.Main
            for i in 1 .. s.Agents.Length - 1 do
                (at s.Agents i).Boned <- false
            for i in 0 .. p.Reviving.Length - 1 do
                let m = at p.Reviving i
                let a = at s.Agents m
                if a.Gone then
                    let body = (at s.Progs m).Body.Value
                    a.Gone <- false
                    a.Health <- body.Life
                    a.Barrier <- body.EnergyShield
                    a.HitAt <- s.Time
                    scheduleFor s m s.Time EReady 0 0 0.0 0.0
                    bump s p.CMinionRevived
        | _ -> ()

    /// Mitigated damage onto a minion: energy shield first (chaos removes twice as much),
    /// then life. Before that it has recovered since it was last hit: its life regeneration,
    /// and its energy shield recharge once the delay has passed (`Sim.hurtMinion`).
    let private hurtMinion (s: State) (rng: Rng) (who: int) (damage: Map<DamageType, float>) =
        let p = at s.Progs who
        let body = p.Body.Value
        let a = at s.Agents who
        let since = s.Time - max 0.0 a.HitAt
        let life = min body.Life (a.Health + body.LifeRegen * since)
        let shield =
            let recharging = since - Constants.energyShieldRechargeDelay
            if recharging > 0.0 then min body.EnergyShield (a.Barrier + body.EnergyShield * Constants.energyShieldRechargePercent / 100.0 * recharging) else a.Barrier
        let shield, rest =
            damage
            |> Map.fold
                (fun (es, rest) t d ->
                    let k = if t = Chaos then 2.0 else 1.0
                    let absorbed = min d (es / k)
                    es - absorbed * k, rest + d - absorbed)
                (shield, 0.0)
        let life = life - rest
        a.Health <- life
        a.Barrier <- shield
        a.HitAt <- s.Time
        let target = aimed s
        // "explode when reduced to Low Life" (35%): its explosion strikes, and it is dead
        if p.Explodes >= 0 && target >= 0 && life <= 0.35 * body.Life then
            bump s p.CMinionExploded
            let was = s.Cur
            enter s who
            onStrike s rng p.Explodes 1.0 target 0.0 false false
            enter s was
            killMinion s rng who
        elif life <= 0.0 then killMinion s rng who

    /// A monster's hit at a minion: evasion, armour against physical damage, resistances
    /// (`Sim.attackMinion`).
    let private attackMinion (s: State) (rng: Rng) (index: int) (who: int) =
        let p = s.Main
        let e = at s.Foes index
        let atk = e.Def.Attack.Value
        let body = (at s.Progs who).Body.Value
        let a = at s.Agents who
        let stopped = stopped s e false
        let blinded = at e.SHas (at p.AilmentStatus 11)
        let chill = if (at e.Has p.SChilled) then (at e.DMods p.SChilled) |> List.sumBy (fun m -> m.Value) else 0.0
        defList s e
        let actionSpeed = max 0.1 (dmore e actionSpeedStat 1.0)
        let dealt = max 0.0 (dmore e damageDealtStat 1.0)
        let next = atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed
        schedule s (s.Time + next) EEnemyAttack index 0 0.0 0.0
        if stopped then bump s p.CStopped
        else
            let accuracy = atk.Accuracy * (if blinded then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
            let hitChance = Vitals.monsterHitChance body.Evasion accuracy
            let landed =
                if hitChance >= 100.0 then true
                else
                    let value =
                        if a.HasDodge && s.Time - a.DodgeAt <= 100.0 * Constants.serverTick then a.Dodge
                        else floor (rng.Float() * 100.0)
                    let value = value + hitChance
                    let hit = value >= 100.0
                    a.Dodge <- (if hit then value - 100.0 else value)
                    a.DodgeAt <- s.Time
                    a.HasDodge <- true
                    hit
            if not landed then bump s p.CMinionEvaded
            else
                let crit = rng.Chance atk.CritChance && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
                let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 else 1.0
                let taken =
                    atk.Damage
                    |> Map.map (fun t d ->
                        let d = d * dealt * critMult
                        match t with
                        | Physical -> d * (1.0 - Vitals.armourReduction body.Armour d / 100.0)
                        | t -> d * (1.0 - (Map.tryFind t body.Resist |> Option.defaultValue 0.0) / 100.0))
                bump s p.CMinionHit
                hurtMinion s rng who taken

    /// After a use's hits: the skill consumes its charges, unless they are kept (`Sim.step`).
    let private onConsume (s: State) (rng: Rng) (charge: int) (sid: int) =
        let p = s.P
        let info = (at p.Skills sid)
        let ev = acquireEmpty s sid
        let skip = sumQ ev qSkipConsume
        let every = sumQ ev qConsumeEvery / 1000.0
        let count = int (sumQ ev qConsumeCount)
        release s
        let spared = every > 0.0 && not (rng.Chance(100.0 * min 1.0 (useTime s sid / every)))
        if (at s.A.BHas charge) && not spared then
            // "still count as consuming": the consumed-recently status starts either way
            markStatus s (at p.ConsumedRecently charge)
            if rng.Chance skip then
                bump s info.CKept
                let pause = p.Actor.KeptChargesPause
                if pause > 0.0 then
                    // freeze the timer: push expiry back by the part of the pause that
                    // doesn't overlap an earlier one
                    let pausedUntil = if System.Double.IsNaN (at s.A.BPaused charge) then s.Time else (at s.A.BPaused charge)
                    let newUntil = s.Time + pause
                    let extension = max 0.0 (newUntil - max s.Time pausedUntil)
                    if (at s.A.BHas charge) && (at s.A.BExpires charge) < infinity then
                        let expires = (at s.A.BExpires charge) + extension
                        setBuff s charge (at s.A.BStacks charge) expires (at s.A.BMods charge)
                        put s.A.BPaused charge (newUntil)
                        expireAt s expires
            else
                // one charge, a set number, or all of them (0)
                if (at s.A.BHas charge) && count > 0 && (at s.A.BStacks charge) > count then setBuff s charge ((at s.A.BStacks charge) - count) (at s.A.BExpires charge) (at s.A.BMods charge)
                else removeBuff s charge
                bump s info.CConsumed

    /// Stacks of a resource arrive (`Sim.step`, GainStacks).
    let private onGainStacks (s: State) (rng: Rng) (r: int) (n: int) =
        let p = s.P
        let maxR = if (at p.ResourceMax r) <> System.Int32.MinValue then (at p.ResourceMax r) else n
        // "N% chance to gain an additional (Power) Charge when you gain a Charge"
        let n =
            if not (at p.IsCharge r) || p.FirstSkill < 0 then n
            else
                let ev = acquireEmpty s p.FirstSkill
                let chance = qSum ev (at p.QExtra r)
                release s
                if chance > 0.0 && rng.Chance chance then n + 1 else n
        setStacks s true r (min maxR (stacksOf s r + n))
        if (at p.IsCharge r) then markStatus s (at p.GainedRecently r)
        bump s (at p.CGained r)

    /// A cycle picks its next variant (`Sim.step`, CycleTick).
    let private onCycle (s: State) (rng: Rng) (index: int) =
        let c = (at s.P.Cycles index)
        let pick = min (c.Variants.Length - 1) (int (rng.Float() * float c.Variants.Length))
        setBuff s c.Buff 1 (s.Time + c.Period) (at c.Variants pick)
        bump s (at c.Picked pick)
        schedule s (s.Time + c.Period) ECycle index 0 0.0 0.0

    /// What happens on its own every few seconds (`Sim.onPeriodic`).
    let private onPeriodic (s: State) (rng: Rng) (index: int) =
        let p = s.P
        let pd = (at p.Periodics index)
        schedule s (s.Time + pd.Every) EPeriodic index 0 0.0 0.0
        let gain (r: int) (n: int) =
            setStacks s true r n
            markStatus s (at p.GainedRecently r)
            bump s (at p.CGained r)
        match pd.Action with
        | PGainRandom resources ->
            let open' = resources |> Array.filter (fun (r, most) -> stacksOf s r < most)
            if open'.Length > 0 then
                let r, _ = (at open' (min (open'.Length - 1) (int (rng.Float() * float open'.Length))))
                gain r (stacksOf s r + 1)
        | PGainWeighted weights ->
            let mutable total = 0.0
            for i__49 in 0 .. weights.Length - 1 do
                let (_, _, w) = at weights i__49
                total <- total + w
            let roll = rng.Float() * total
            let mutable picked = -1
            let mutable acc = 0.0
            for i in 0 .. weights.Length - 1 do
                let _, _, w = (at weights i)
                if picked < 0 then
                    if roll < acc + w then picked <- i
                    acc <- acc + w
            let r, most, _ = (at weights (if picked < 0 then weights.Length - 1 else picked))
            gain r (min most (stacksOf s r + 1))
        | PGainAnyRandom resources ->
            let r, most = (at resources (min (resources.Length - 1) (int (rng.Float() * float resources.Length))))
            gain r (min most (stacksOf s r + 1))
        | PTrigger skill -> trigger s rng skill
        | PSummon (members, count, duration, corpse, summoned) ->
            if not corpse then summon s members count duration [] summoned
            elif corpses s > 0 then
                s.CorpsesUsed <- s.CorpsesUsed + 1
                summon s members count duration [] summoned
        | PHealLife (amount, count) ->
            // (the player's life, whoever the action is of)
            match s.Main.Vitals with
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
                bump s (at p.CGained b)
        | PConsumeEach resources ->
            for i__50 in 0 .. resources.Length - 1 do
                let r = at resources i__50
                if stacksOf s r <> 0 then
                    let have = stacksOf s r
                    markStatus s (at p.ConsumedRecently r)
                    (match p.Vitals with
                     | Some v when r = (at p.BCharges 0) -> gainMana s v 6 (v.Mana * p.ConsumePowerMana / 100.0)
                     | _ -> ())
                    if rng.Chance p.SheetSkip then bump s (at p.CKeptOf r)
                    else
                        setStacks s false r (have - 1)
                        bump s (at p.CLostOf r)

    /// A monster's hit at the player: evasion, deflection, armour, resistances, then energy
    /// shield and life (`Sim.attackPlayer`, in its order).
    let private attackPlayer (s: State) (rng: Rng) (index: int) =
        let p = s.P
        let e = (at s.Foes index)
        match e.Def.Attack, p.Vitals with
        | Some atk, Some pools when e.Life > 0.0 && alive s ->
            // mitigation from the player's current buffs and charges
            // (the defences are a function of the buffs: the same buffs, the same defences, and
            // working them out is most of what a monster's hit costs)
            let v =
                match p.Fight.Player.VitalsAt with
                | Some at ->
                    match s.VitalsLast with
                    | Some v when s.VitalsOf = s.A.BuffsVersion -> v
                    | _ ->
                        // the buffs written out: which there are, and their stacks
                        let mutable key = ""
                        for id in p.VitalsOn do
                            if s.A.BHas.[id] then key <- key + string id + "=" + string s.A.BStacks.[id] + ";"
                        let v =
                            match s.VitalsSeen.TryGetValue key with
                            | true, v -> v
                            | _ ->
                                let v = at (buffMap s)
                                s.VitalsSeen.[key] <- v
                                v
                        s.VitalsOf <- s.A.BuffsVersion
                        s.VitalsLast <- Some v
                        v
                | None -> pools
            let stopped = stopped s e false
            let blinded = at e.SHas (at p.AilmentStatus 11)
            let chill = if (at e.Has p.SChilled) then (at e.DMods p.SChilled) |> List.sumBy (fun m -> m.Value) else 0.0
            // curses on the monster: action speed (Temporal Chains), damage dealt (Enfeeble)
            defList s e
            let actionSpeed = max 0.1 (dmore e actionSpeedStat 1.0)
            let dealt = max 0.0 (dmore e damageDealtStat 1.0)
            let atk = { atk with Damage = atk.Damage |> Map.map (fun _ d -> d * dealt) }
            let next = atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed
            schedule s (s.Time + next) EEnemyAttack index 0 0.0 0.0
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            if stopped then bump s p.CStopped
            elif inRange then
                let accuracy = atk.Accuracy * (if blinded then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
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
                        if (at s.A.BHas p.BShocked) && (at s.A.BExpires p.BShocked) > s.Time then (at s.A.BMods p.BShocked) |> List.sumBy (fun m -> m.Value) else 0.0
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
                    // "x% of Damage from Hits is taken from your Companion's Life before you": the
                    // first minion that does so and is alive takes its share, as much as it has
                    let taken =
                        let mutable guard = -1
                        for i in 0 .. p.Bodies.Length - 1 do
                            let m = at p.Bodies i
                            if guard < 0 && (at s.Progs m).Body.Value.TakesForOwner > 0.0 && not (at s.Agents m).Gone then guard <- m
                        if guard < 0 then taken
                        else
                            let share = (at s.Progs guard).Body.Value.TakesForOwner / 100.0
                            let its = at s.Agents guard
                            let total = taken |> Map.toList |> List.sumBy snd
                            let k = if total <= 0.0 then 0.0 else min share ((its.Health + its.Barrier) / total)
                            bump s p.CMinionTook
                            hurtMinion s rng guard (taken |> Map.map (fun _ d -> d * k))
                            taken |> Map.map (fun _ d -> d * (1.0 - k))
                    takeDamage s pools taken
                    s.LastHit <- s.Time
                    gainRage s p.RageWhenHit
                    if p.FirstSkill >= 0 then react s rng p.WhenHit "" p.FirstSkill index
                    // "Rage loss is paused for 4 seconds upon gaining Rage, or after taking damage"
                    s.A.RageAt <- s.Time
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
                        for i in 0 .. p.Metas.Length - 1 do
                            let meta = at p.Metas i
                            let mutable amount = 0.0
                            for k in 0 .. meta.Sources.Length - 1 do
                                match at meta.Sources k with
                                | Blocks energy -> amount <- amount + energy
                                | _ -> ()
                            if amount > 0.0 then
                                let cause = s.Cause
                                s.Cause <- index
                                gainEnergy s rng i (amount * meta.Meta.EnergyMult)
                                s.Cause <- cause
                        // blocks set off the rules for blocking
                        if p.FirstSkill >= 0 then react s rng p.OnBlock "" p.FirstSkill index
        | _ -> ()

    /// A monster's attack goes at the player or at one of the minions in its reach, each as
    /// likely as the other. A melee minion fights at the monster; the others stand where the
    /// player does (`Sim.enemyAttack`).
    let private enemyAttack (s: State) (rng: Rng) (index: int) =
        let p = s.P
        let e = at s.Foes index
        match e.Def.Attack with
        | Some atk when e.Life > 0.0 && p.Bodies.Length > 0 ->
            let playerUp = p.Vitals.IsSome && alive s
            let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
            let mutable reachable = 0
            for i in 0 .. p.Bodies.Length - 1 do
                let m = at p.Bodies i
                if not (at s.Agents m).Gone && ((at s.Progs m).Body.Value.Melee || inRange) then reachable <- reachable + 1
            let n = reachable + (if playerUp && inRange then 1 else 0)
            let pick = if n > 0 then int (rng.Float() * float n) else 0
            if pick < reachable then
                // the pick-th of those in reach
                let mutable left = pick
                let mutable who = -1
                for i in 0 .. p.Bodies.Length - 1 do
                    let m = at p.Bodies i
                    if who < 0 && not (at s.Agents m).Gone && ((at s.Progs m).Body.Value.Melee || inRange) then
                        if left = 0 then who <- m else left <- left - 1
                attackMinion s rng index who
            elif playerUp then attackPlayer s rng index
            else schedule s (s.Time + atk.Interval) EEnemyAttack index 0 0.0 0.0
        | _ -> attackPlayer s rng index

    /// One of the player's states (see `playerStates`) is a buff while it holds.
    let private setState (s: State) (slot: int) (on: bool) =
        let id = (at s.P.BStates slot)
        if on then (if not (at s.A.BHas id) then setBuff s id 1 infinity [])
        elif (at s.A.BHas id) then removeBuff s id

    /// The player's life and mana state, as buffs conditions can read (`Sim.step`).
    let private playerStates (s: State) (v: PlayerStats) =
        let p = s.P
        // "Your Life cannot change while you have Energy Shield": no recovery above where it started
        if v.LifeFrozenWithES && s.Shield > 0.0 && s.Life > v.StartLife * v.Life then s.Life <- v.StartLife * v.Life
        // reserved life: what is left is the most there can be
        elif v.LifeReserved > 0.0 && s.Life > (1.0 - v.LifeReserved) * v.Life then s.Life <- (1.0 - v.LifeReserved) * v.Life
        let manaFlask = s.Time < s.FlaskUntil || stacksOf s p.BConstantMana > 0
        let lifeFlask = s.Time < s.LifeFlaskUntil || stacksOf s p.BConstantLife > 0
        setState s 0 (s.Life >= v.Life)
        setState s 1 (v.Life > 1.0 && s.Life <= 0.35 * v.Life)
        setState s 2 (s.Mana >= v.Mana)
        setState s 3 (s.Mana <= 0.35 * v.Mana)
        setState s 4 (s.Shield > 0.0)
        setState s 5 manaFlask
        setState s 6 lifeFlask
        setState s 7 (s.Time < s.FlaskUntil || s.Time < s.LifeFlaskUntil || stacksOf s p.BConstantMana > 0 || stacksOf s p.BConstantLife > 0)
        (let mutable leeching = false
         let mutable rest = s.Recoups
         while not rest.IsEmpty do
            let until, _, pool = rest.Head
            if pool.StartsWith "Leech" && until > s.Time then leeching <- true
            rest <- rest.Tail
         setState s 8 leeching)
        // surrounded: at least 5 monsters within 3 m
        let mutable near = 0
        for i__52 in 0 .. s.Live.Length - 1 do
            let e = at s.Foes (at s.Live i__52)
            if e.Life > 0.0 && Vec.length e.Def.Position - e.Def.Size <= 30.0 then near <- near + 1
        setState s 9 (near >= max 1 v.SurroundedAt)
        // charge states: "while at maximum Power Charges", "while you have no Frenzy Charges"
        for k in 0 .. 2 do
            let id = (at p.BCharges k)
            // (read before the states of this kind are set: a state's name is never a charge's)
            let n = stacksOf s id
            let maxN = if (at p.ResourceMax id) <> System.Int32.MinValue then (at p.ResourceMax id) else 0
            setState s (10 + 3 * k) (maxN > 0 && n >= maxN)
            setState s (11 + 3 * k) (n = 0)
            setState s (12 + 3 * k) (n = 0)

    /// What is planted in or beside the enemies (`Sim.step`): seeds set off by the damage
    /// their enemy has taken (or its death: the blast then strikes the enemy aimed at), gone
    /// when their time is up; pustules that burst when their time is up or they hold all the
    /// Poison they can use; Ice Crystals destroyed once the enemy beside them has lost as much
    /// life as one withstands, each of which then explodes.
    let private planted (s: State) =
        if not s.Seeds.IsEmpty then
            let mutable kept = []
            for ((owner, skill, enemy, mark, needed, until) as seed) in s.Seeds do
                let e = at s.Foes enemy
                if e.Life > 0.0 && mark - e.Life >= needed then scheduleFor s owner s.Time EStrike skill enemy 1.0 0.0
                elif e.Life > 0.0 then
                    if s.Time >= until then bump s (at (at s.Progs owner).Skills skill).CFizzled else kept <- seed :: kept
                else
                    let other = aimed s
                    if other >= 0 then scheduleFor s owner s.Time EStrike skill other 1.0 0.0
            s.Seeds <- List.rev kept
        if not s.Pustules.IsEmpty then
            // (the Poison damage dealt so far: what is still kept back goes into the results first)
            for i in 0 .. s.Foes.Length - 1 do
                flushDots s (at s.Foes i)
            let poison = at s.Damage (at s.P.AilmentSource (ailmentIndex Poison))
            let mutable kept = []
            for ((owner, part, enemy, mark, per, cap, until) as pustule) in s.Pustules do
                if (at s.Foes enemy).Life > 0.0 then
                    let stored = poison - mark
                    let more = if per > 0.0 then min cap (stored / per) else 0.0
                    if s.Time >= until || (cap > 0.0 && more >= cap) then
                        // (a part the actor does not have: the reference fails when it is to burst)
                        if part < 0 then failwith "a pustule bursts with a part its actor does not have"
                        scheduleFor s owner s.Time EStrike part enemy (1.0 + more / 100.0) 0.0
                    else kept <- pustule :: kept
            s.Pustules <- List.rev kept
        if not s.Crystals.IsEmpty then
            let mutable kept = []
            for ((owner, part, enemy, mark, life, count, until) as crystal) in s.Crystals do
                let e = at s.Foes enemy
                if e.Life > 0.0 && mark - e.Life >= life then
                    for _ in 1 .. count do
                        scheduleFor s owner s.Time EStrike part enemy 1.0 0.0
                    bump s (at (at s.Progs owner).Skills part).CShattered
                elif e.Life > 0.0 && s.Time < until then kept <- crystal :: kept
            s.Crystals <- List.rev kept

    /// What has run out of every actor's buffs goes (`Sim.prune`).
    let private pruneAll (s: State) =
        pruneFoes s
        prune s
        if s.Agents.Length > 1 then
            for i in 1 .. s.Agents.Length - 1 do
                if not ((at s.Agents i).BSoonest > s.Time) then
                    enter s i
                    prune s
            enter s 0

    /// The earliest event happens (`Sim.step`).
    let private step (s: State) =
        let q = s.Queue
        q.Pop()
        // what a minion had begun when it left the fight is lost (the reference takes it out of the queue)
        let lost =
            let agent = at s.Agents q.Who
            if q.Kind = EStrike || q.Kind = EPassing then
                let still = q.Tag = agent.Epoch * 65536 + (at agent.Flight q.A)
                if still then put agent.InFlight q.A ((at agent.InFlight q.A) - 1)
                not still
            else (q.Kind = EReady || q.Kind = EDirect) && q.Tag <> agent.Epoch * 65536
        if not lost then
            let t, kind, who, a, b, x, y = q.Time, q.Kind, q.Who, q.A, q.B, q.X, q.Y
            let dt = t - s.Time
            enter s who
            let p = s.P
            (match s.Tell with
             | Some tell ->
                let name = p.Actor.Name
                match kind with
                | EReady -> tell t "Ready" name "" (float q.Seq)
                | EStrike | EPassing | EDirect -> tell t (kindName kind) name (at p.Skills a).Skill.Name (float q.Seq)
                | EConsume | EGainStacks | ESpendBuff -> tell t (kindName kind) name (at p.BuffNames a) (float q.Seq)
                | ECycle -> tell t "CycleTick" name (at p.BuffNames ((at p.Cycles a).Buff)) (float q.Seq)
                | EPeriodic -> tell t "PeriodicTick" name p.Actor.Periodics.[a].Name (float q.Seq)
                | EEnemyAttack -> tell t "EnemyAttack" (at s.Foes a).Def.Name "" (float q.Seq)
                | EEnergy -> tell t "EnergyArrives" name (at p.Metas a).Meta.Name (float q.Seq)
                | EExpose -> tell t "Expose" name (at s.Foes a).Def.Name (float q.Seq)
                | ERevive -> tell t "ReviveMinions" "" "" (float q.Seq)
                | ELeaves -> tell t "MinionLeaves" name "" (float q.Seq)
                | EWard -> tell t "WardArrives" "" "" (float q.Seq)
                | EMana -> tell t "ManaArrives" "" "" (float q.Seq)
                | EPlant -> tell t "PlantCrystals" name (at p.Skills a).Skill.Name (float q.Seq)
                | _ -> tell t "Expire" "" "" (float q.Seq)
             | None -> ())
            // a skill's hits roll from a stream of that skill alone: its first event, its second, ...
            let rng =
                match kind with
                | EStrike | EPassing | EDirect ->
                    let n = (at s.A.StrikeStream a)
                    put s.A.StrikeStream a (n + 1)
                    Rng(Sim.streamSeedOf s.Seed (at p.Skills a).StrikeKey n)
                | EConsume ->
                    let n = (at s.A.ConsumeStream b)
                    put s.A.ConsumeStream b (n + 1)
                    Rng(Sim.streamSeedOf s.Seed (at p.Skills b).ConsumeKey n)
                | EEnemyAttack ->
                    let e = at s.Foes a
                    let n = e.AttackN
                    e.AttackN <- n + 1
                    Rng(Sim.streamSeedOf s.Seed e.AttackKey n)
                | EPeriodic ->
                    let n = (at s.A.PeriodicStream a)
                    put s.A.PeriodicStream a (n + 1)
                    Rng(Sim.streamSeedOf s.Seed (at p.Periodics a).Key n)
                | _ -> s.Rng
            // (time passes for the player, whoever the event is of)
            enter s 0
            let p = s.P
            advance s t
            recover s dt
            if s.AliveCount > 0 && alive s then
                pruneAll s
                if not (s.Seeds.IsEmpty && s.Pustules.IsEmpty && s.Crystals.IsEmpty) then planted s
                // Demon Form: a Demonflame for every second in the form (at most 10), life lost for
                // each; at 1 life the form ends and its cooldown starts
                if (at s.A.BHas p.BDemonForm) && p.Vitals.IsSome then
                    let form = p.BDemonForm
                    let mods = at s.A.BMods form
                    let value (stat: string) = mods |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
                    let since = (at s.A.BExpires form) - 1e9
                    let flames = min 10 (int (s.Time - since))
                    // the life lost since the last event, at the Demonflame held then
                    let before = max 0 (min 10 (int (s.Time - dt - since)))
                    let life = s.Life - value "DemonLifeLossPerStack" * float before * max 0.0 dt
                    if life <= 1.0 then
                        let back = s.Time + value "DemonCooldown"
                        s.Life <- min s.Life 1.0
                        bump s p.CDemonEnded
                        removeBuff s form
                        removeBuff s p.BDemonflame
                        // (the form's skill is named by where its mods are from)
                        match mods |> List.tryPick (fun m -> if m.Source <> "" then Some m.Source else None) with
                        | Some name ->
                            match p.SkillIds.TryGetValue name with
                            | true, skill ->
                                put s.A.Cooldowns skill back
                                put s.A.Recharges skill [ back ]
                            | _ -> ()
                        | None -> ()
                    else
                        s.Life <- life
                        if flames > 0 then setBuff s p.BDemonflame flames infinity []
                // rage: regenerated all the time, lost once none has been gained for a while
                (let have = stacksOf s p.BRage
                 if not (dt <= 0.0 || p.MaxRage <= 0 || (p.RageRegen <= 0.0 && have = 0)) then
                    let losing = if s.Time - s.A.RageAt > p.RageDelay then p.RageLoss else 0.0
                    // Berserk: life lost for every Rage held while Rage is not being lost (not below 1)
                    (match p.Vitals with
                     | Some v when losing <= p.RageRegen && have > 0 && p.BerserkLoss > 0.0 ->
                        let life = max (min s.Life 1.0) (s.Life - p.BerserkLoss / 100.0 / 60.0 * float have * v.Life * dt)
                        s.Life <- life
                        s.MinLife <- min s.MinLife (life / max 1.0 v.Life)
                     | _ -> ())
                    let carry = s.A.RageCarry + (p.RageRegen - losing) * dt
                    let whole = int (System.Math.Truncate carry)
                    let n = max 0 (min p.MaxRage (have + whole))
                    if n <> have then setStacks s false p.BRage n
                    s.A.RageCarry <- (if n = 0 && carry < 0.0 then 0.0 elif n = p.MaxRage && carry > 0.0 then 0.0 else carry - float whole))
                (match p.Vitals with
                 | Some v -> playerStates s v
                 | None -> ())
                // a minion that is out of the fight does nothing
                if (kind = EReady || kind = EStrike || kind = EPassing || kind = EDirect) && (at s.Agents who).Gone then ()
                else
                    match kind with
                    | EEnemyAttack -> enemyAttack s rng a
                    | ERevive -> reviveMinions s x
                    | ELeaves ->
                        let minion = at s.Agents who
                        if not (minion.Gone || minion.Leaves <> x) then
                            leave s who
                            bump s p.CMinionLeft
                    | EPlant ->
                        let e = at s.Foes b
                        if e.Life > 0.0 then
                            let count, life, limit, lasts = s.Plants.[int x]
                            // more than the limit: the oldest go (without exploding)
                            let mutable kept = []
                            let mutable n = 0
                            for ((o, part, _, _, _, c, _) as crystal) in (who, a, b, e.Life, life, count, s.Time + lasts) :: s.Crystals do
                                if o = who && part = a then
                                    if n + c <= max count limit then kept <- crystal :: kept
                                    n <- n + c
                                else kept <- crystal :: kept
                            s.Crystals <- List.rev kept
                            bump s (at (at s.Progs who).Skills a).CCrystals
                    | EWard ->
                        (match p.Vitals, p.WardRemnants with
                         | Some v, Some r -> s.Ward <- min (v.Ward * r.Overflow) (s.Ward + x)
                         | _ -> ())
                    | EMana ->
                        (match p.Vitals, p.ManaRemnants with
                         | Some v, Some r -> s.Mana <- min (max s.Mana (v.Mana * r.Overflow)) (s.Mana + x * v.ManaRecoveryRate)
                         | _ -> ())
                    | EExpire -> ()
                    | _ ->
                        // (what an actor does, it does in its own place)
                        enter s who
                        (match kind with
                         | EReady -> onReady s rng
                         | EStrike -> onStrike s rng a x b 0.0 false false
                         | EPassing -> onStrike s rng a x b y true false
                         | EDirect -> onStrike s rng a x b 0.0 false true
                         | EConsume -> onConsume s rng a b
                         | ECycle -> onCycle s rng a
                         | EGainStacks -> onGainStacks s rng a b
                         | EExpose -> expose s a DamageType.all.[b] x y
                         | EPeriodic -> onPeriodic s rng a
                         | EEnergy -> gainEnergy s rng a x
                         | ESpendBuff ->
                            if (at s.A.BHas a) && (at s.A.BStacks a) > 1 then setBuff s a ((at s.A.BStacks a) - 1) (at s.A.BExpires a) (at s.A.BMods a)
                            else removeBuff s a
                         | _ -> ())
                        enter s 0

    /// What a runner keeps from one run to the next (see `runner`): the queue's arrays and
    /// the scratch of the hits, emptied for every run, and the defences per set of buffs
    /// that the runs so far have asked for (a function of the buffs: knowing them changes no result).
    type private Kept =
        { Queue: Queue
          Evs: ResizeArray<Ev>
          Vitals: Dictionary<string, PlayerStats> }

    let private newKept () : Kept = { Queue = Queue(); Evs = ResizeArray(); Vitals = Dictionary() }

    /// An enemy as it comes into the fight (`fresh` in `Sim.runWith`): `root` is the enemy
    /// the fight names that it is, or that it replaces.
    let private makeFoe (ns: int) (info: EnemyInfo) (index: int) (root: int) (def: EnemyDef) : Foe =
        let e =
            { Info = info
              Index = index
              Root = root
              AttackKey = Sim.streamHash ("E|" + def.Name)
              AttackN = 0
              Def = def
              Life = def.Life
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
              FlamValue = Array.zeroCreate 16
              FlamUntil = Array.zeroCreate 16
              FlamFrom = 0
              FlamTo = 0
              FlamSum = 0.0
              Entropy = 0.0
              EntropyAt = 0.0
              HasEntropy = false
              Status = Map.empty
              SHas = [||]
              SStacks = [||]
              StatusMapStale = true
              MapHas = [||]
              MapStacks = [||]
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
        e

    let private start (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) (kept: Kept) : State =
        kept.Queue.Clear()
        let fight = p.Fight
        let player = fight.Player
        let nb = p.BuffNames.Length
        let ns = p.StatusNames.Length
        let foes = p.Enemies |> Array.mapi (fun i info -> makeFoe ns info i i info.Def)
        let v = p.Vitals
        let progs = Array.append [| p |] p.Minions
        let agents =
            progs
            |> Array.map (fun (pr: Program) ->
                let nk = pr.Skills.Length
                { BKept = false
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
                  Dyn = pr.NoDyn
                  DynVersion = -1
                  DynOfSkill = false
                  DynOnStatus = false
                  DynFor = Array.create (nk * 3) pr.NoDyn
                  DynForVer = Array.create (nk * 3) -1
                  BComp = Array.create nb [||]
                  BCompOf = Array.create nb []
                  RageMore = Array.create nk nan
                  RageVer = Array.create nk -1
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
                  Dbs = pr.Skills |> Array.map (fun k -> k.Db)
                  DbVersion = Array.create nk -1
                  Cooldowns = Array.create nk -infinity
                  Recharges = Array.create nk []
                  ArchonRecovery = -infinity
                  InvocationReady = Array.create pr.Metas.Length -infinity
                  Energy = Map.empty
                  // invocations start with what was banked while clearing
                  Held = pr.Metas |> Array.map (fun meta -> match meta.Meta.Release with Invocation _ -> meta.Meta.MaxEnergy * fight.BankedInvocations | AtMaximum -> 0.0)
                  HeldSet = pr.Metas |> Array.map (fun meta -> match meta.Meta.Release with Invocation _ -> true | AtMaximum -> false)
                  HeldStale = true
                  LastUsed = -1
                  CritRoll = Array.zeroCreate nk
                  HasCritRoll = Array.create nk false
                  Rolled = Array.zeroCreate nk
                  StrikeStream = Array.zeroCreate nk
                  ConsumeStream = Array.zeroCreate nk
                  PeriodicStream = Array.zeroCreate pr.Periodics.Length
                  RageAt = 0.0
                  RageCarry = 0.0
                  Waiting = Array.zeroCreate nk
                  Ammo = Array.create nk -1
                  LoadDue = Array.create nk false
                  SealBreak = Array.create nk nan
                  InFlight = Array.zeroCreate nk
                  Flight = Array.zeroCreate nk
                  Health = (match pr.Body with Some body -> body.Life | None -> 0.0)
                  Barrier = (match pr.Body with Some body -> body.EnergyShield | None -> 0.0)
                  HitAt = -1e9
                  // (a temporary minion is out of the fight until it is summoned)
                  Gone = pr.Actor.Pool.IsSome
                  Leaves = infinity
                  Epoch = 0
                  Boned = false
                  Dodge = 0.0
                  DodgeAt = 0.0
                  HasDodge = false })
        let s =
            { P = p
              A = agents.[0]
              Cur = 0
              Progs = progs
              Agents = agents
              Main = p
              Seed = seed
              Rng = Rng(seed)
              Queue = kept.Queue
              Tell = tell
              Time = 0.0
              Seq = 0
              Foes = foes
              Live = Array.init foes.Length id
              FoeIds = (if fight.Replace then Dictionary<string, int>(p.EnemyIds) else p.EnemyIds)
              AliveCount = foes |> Array.filter (fun e -> e.Life > 0.0) |> Array.length
              Living = []
              Aimed = -1
              AliveStale = true
              Kills = ResizeArray()
              CorpsesUsed = 0
              Seeds = []
              Pustules = []
              Crystals = []
              Plants = ResizeArray()
              Revival = None
              NoRoom = Array.create progs.Length false
              RoomAt = -2
              Evs = kept.Evs
              StatusHas = Array.zeroCreate ns
              StatusStacks = Array.zeroCreate ns
              AilmentSeen = Array.zeroCreate 13
              NoHas = [||]
              EvDepth = 0
              Cause = -1
              Aim = -1
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
              ShockWas = 0.0
              BrokenWas = 0.0
              InfernalFlame = 0.0
              LastStunRemnant = -1e9
              LastManaRemnant = -1e9
              Burning = []
              CharmCharges = Array.zeroCreate nb
              PEntropy = 0.0
              PEntropyAt = 0.0
              PHasEntropy = false
              VitalsSeen = kept.Vitals
              VitalsOf = -1
              VitalsLast = None
              Gained = Array.zeroCreate gainedNames.Length
              GainedSeen = Array.create gainedNames.Length false }
        for i__54 in 0 .. p.Charms.Length - 1 do
            let c = at p.Charms i__54
            put s.CharmCharges c.CBuff (c.Charm.MaxCharges)
        // every actor in its turn, the player first (the reference's order, and with it the
        // order in which the fight's random numbers are drawn)
        for who in 0 .. progs.Length - 1 do
            enter s who
            // the resources the fight starts with: a random part of their duration has already passed
            for (id, n, lasts) in s.P.Resources do
                let expires = match lasts with Some d -> s.Rng.Float() * d | None -> infinity
                setBuff s id n expires []
            // charge buffs are up when the fight starts: a random part of their time has already passed
            for (b, charge, perStack, seconds, doubleChance) in s.P.StartCharged do
                if s.A.BHas.[charge] && s.A.BStacks.[charge] > 0 then
                    let stacks = if s.Rng.Chance doubleChance then 2 * s.A.BStacks.[charge] else s.A.BStacks.[charge]
                    let left = seconds * float stacks * (0.5 + 0.5 * s.Rng.Float())
                    setBuff s b stacks left perStack
        // (a temporary minion starts acting when it is summoned)
        for who in 0 .. progs.Length - 1 do
            enter s who
            if not s.A.Gone then schedule s 0.0 EReady 0 0 0.0 0.0
        for who in 0 .. progs.Length - 1 do
            enter s who
            s.P.Cycles |> Array.iteri (fun i _ -> schedule s 0.0 ECycle i 0 0.0 0.0)
        for who in 0 .. progs.Length - 1 do
            enter s who
            for i__56 in 0 .. s.P.Timed.Length - 1 do
                let id = at s.P.Timed i__56
                if (at s.A.BHas id) && (at s.A.BExpires id) < infinity then expireAt s (at s.A.BExpires id)
        // periodic actions start at a random point of their first period
        for who in 0 .. progs.Length - 1 do
            enter s who
            s.P.Periodics |> Array.iteri (fun i pd -> schedule s (s.Rng.Float() * pd.Every) EPeriodic i 0 0.0 0.0)
        enter s 0
        // monsters start attacking at random points in their first interval
        for i__57 in 0 .. p.Attackers.Length - 1 do
            let i = at p.Attackers i__57
            schedule s (s.Rng.Float() * (at p.Enemies i).Def.Attack.Value.Interval) EEnemyAttack i 0 0.0 0.0
        expireAt s fight.MaxTime
        s

    /// Those that died in the last step are replaced: one of the same kind comes from where
    /// the first of them began, further out and somewhat to the side, and walks in
    /// (`Sim.runWith`). `from`: the first of the kills that are new.
    let private replace (s: State) (from: int) =
        let p = s.Main
        let ns = p.StatusNames.Length
        for k in from .. s.Kills.Count - 1 do
            let name, _, _ = s.Kills.[k]
            let root = (at s.Foes s.FoeIds.[name]).Root
            let d = (at p.Enemies root).Def
            let position = Vec.scale (Vec.length d.Position * 1.25 + 10.0) (Vec.ofAngle (Vec.angle d.Position + (s.Rng.Float() - 0.5) * 1.2))
            let def = { d with Name = name + "+"; Position = position }
            let index = s.Foes.Length
            s.Foes <- Array.append s.Foes [| makeFoe ns (at p.Enemies root) index root def |]
            s.FoeIds.[def.Name] <- index
            s.AliveCount <- s.AliveCount + 1
            if p.Attacked then
                match d.Attack with
                | Some attack -> schedule s (s.Time + s.Rng.Float() * attack.Interval) EEnemyAttack index 0 0.0 0.0
                | None -> ()
        // those in the fight now, in the order of their names: the dead are out of it for good
        s.Live <-
            [| for e in s.Foes do if e.Life > 0.0 then yield e |]
            |> Array.sortWith (fun a b -> compare a.Def.Name b.Def.Name)
            |> Array.map (fun e -> e.Index)
        s.AliveStale <- true

    /// The fight played, for at most so many events.
    let private played (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) (maxEvents: int) (kept: Kept) : State =
        let s = start p seed tell kept
        let maxTime = p.Fight.MaxTime
        let mutable n = 0
        let mutable go = true
        while go do
            if not (alive s) || s.AliveCount = 0 || s.Queue.Count = 0 || s.Queue.Next > maxTime then go <- false
            elif n >= maxEvents then
                bump s p.CAborted
                go <- false
            else
                let before = s.Kills.Count
                step s
                if p.Fight.Replace && s.Kills.Count > before then replace s before
                n <- n + 1
        s

    let private play (p: Program) (seed: uint32) (tell: (float -> string -> string -> string -> float -> unit) option) (kept: Kept) : Outcome =
        // a fight that needs more events than this is a runaway (a zero-time loop): stop it
        let s = played p seed tell 3000000 kept
        let maxTime = p.Fight.MaxTime
        for i__58 in 0 .. s.Foes.Length - 1 do
            let e = at s.Foes i__58
            flushUptime s e
            flushDots s e
        let cleared = if s.AliveCount = 0 then s.Kills |> Seq.map (fun (_, _, t) -> t) |> Seq.fold max 0.0 |> Some else None
        let floats (names: string[]) (values: float[]) (seen: bool[]) =
            let mutable m = Map.empty
            for i in 0 .. names.Length - 1 do
                if (at seen i) then m <- Map.add (at names i) (at values i) m
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
                 if (at s.Counts i) > 0 then m <- Map.add (at p.CountNames i) (at s.Counts i) m
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
    let run (p: Program) (seed: uint32) : Outcome = play p seed None (newKept ())

    /// The program's fight, to be played with one seed after another: what `run` gives for
    /// each, faster from the second on. A runner keeps its working memory between runs and
    /// what it has learned of the player's defences (see `Kept`). One runner is for one
    /// thread at a time; who plays in parallel makes a runner for each thread.
    let runner (p: Program) : uint32 -> Outcome =
        let kept = newKept ()
        fun seed -> play p seed None kept

    /// The same, telling `tell` what `Sim.traced` tells: every event as it is taken from the
    /// queue and every hit that lands.
    let traced (tell: float -> string -> string -> string -> float -> unit) (p: Program) (seed: uint32) : Outcome =
        play p seed (Some tell) (newKept ())

    /// A fight as a program for `run` and `runner`, or what the fight has that is not built
    /// here. A fight that compiles gives the reference's result, but for rounding. (Nothing
    /// of the fight is played here: what can only be learned by playing, a runner learns.)
    let compile (fight: Fight) : Result<Program, string> =
        match build fight with
        | Ok p -> Ok p
        | Error reasons -> Error(String.concat "; " reasons)
