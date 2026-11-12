namespace PoEtry

/// Which damage a "X damage" modifier applies to.
type DamageScope =
    | AllDamage
    | OfType of DamageType
    | Elemental

/// A modifiable quantity. Who owns a mod decides who it affects: mods on the
/// attacker describe the attacker, mods on the enemy (from debuffs) describe
/// the enemy.
type Stat =
    // ---- hit damage ----
    | Damage of DamageScope
    | AddedMin of DamageType
    | AddedMax of DamageType
    | AddedDamageEffect
    | SkillConvert of from: DamageType * into: DamageType
    | Convert of from: DamageType * into: DamageType
    | GainAs of from: DamageType * into: DamageType
    | LuckyDamage
    | CritChance
    | CritBonus
    | Penetration of DamageType
    | HitChance
    | DoubleDamageChance
    // ---- delivery ----
    | AreaOfEffect
    | AdditionalProjectiles
    | Pierce
    | Chains
    // ---- speed / timing ----
    | Speed
    | CooldownRecovery
    | SkillDuration
    // ---- ailments inflicted ----
    | AilmentChance of Ailment
    | AilmentMagnitude of Ailment option // None: all ailments
    | AilmentDuration of Ailment option
    | AilmentStacks of Ailment
    | Buildup of Ailment
    // ---- defensive side (used on the enemy) ----
    | Resist of DamageType
    | MaxResist of DamageType
    | DamageTaken of DamageScope
    | DotTaken
    | Armour
    | PhysicalReduction
    | AilmentThreshold
    | PoiseThreshold
    // ---- the player's own pools and defences ----
    | Attribute of string // "Str" | "Dex" | "Int"
    | MaxLife
    | MaxMana
    | MaxEnergyShield
    | Evasion
    | Deflection
    | LifeRegen // flat per second
    | LifeRegenPercent // % of max life per second
    | ManaRegen // increased mana regeneration rate
    | ManaCostEfficiency
    | ManaCost // increased / reduced mana cost
    | ManaOnKill
    | ManaLeech // % of physical attack damage leeched as mana
    // ---- minions ----
    | ForMinions of Stat
    // ---- anything else, by name ----
    | Named of string

type ModKind =
    | Base
    | Inc
    | More
    | Flag
    | Override

/// When a mod applies. Evaluated against a Ctx at the moment it matters.
type Cond =
    | IsCrit
    | SkillIs of SkillTag
    | SkillNamed of string
    | WhileBuff of string
    | EnemyHas of string
    /// Value is multiplied by floor(stacks / every) of the named buff or charge.
    | PerStack of buff: string * every: float
    /// Same, for a status/debuff on the enemy.
    | PerEnemyStack of status: string * every: float
    | Not of Cond

type Mod =
    { Stat: Stat
      Kind: ModKind
      Value: float
      Conds: Cond list
      Source: string }

/// The situation a mod is evaluated in.
type Ctx =
    { Crit: bool
      SkillName: string
      SkillTags: Set<SkillTag>
      /// Buffs and charges on the actor: name -> stacks.
      Buffs: Map<string, int>
      /// Statuses on the enemy: name -> stacks.
      EnemyStatus: Map<string, int> }

module Ctx =
    let empty =
        { Crit = false
          SkillName = ""
          SkillTags = Set.empty
          Buffs = Map.empty
          EnemyStatus = Map.empty }

/// Shorthand constructors so mod lists read like item text.
module Mod =
    let make kind stat value =
        { Stat = stat
          Kind = kind
          Value = value
          Conds = []
          Source = "" }

    let base' stat v = make Base stat v
    let inc stat v = make Inc stat v
    let more stat v = make More stat v
    let flag stat = make Flag stat 1.0
    let override' stat v = make Override stat v

    let added t lo hi = [ make Base (AddedMin t) lo; make Base (AddedMax t) hi ]

    let resistAllElemental v =
        DamageType.elemental |> List.map (fun t -> make Base (Resist t) v)

    let whenever cond (m: Mod) = { m with Conds = cond :: m.Conds }
    let from source (m: Mod) = { m with Source = source }
    let scaled k (m: Mod) = { m with Value = m.Value * k }

    let rec private holds ctx cond =
        match cond with
        | IsCrit -> ctx.Crit
        | SkillIs tag -> Set.contains tag ctx.SkillTags
        | SkillNamed n -> ctx.SkillName = n
        | WhileBuff b -> Map.containsKey b ctx.Buffs
        | EnemyHas s -> Map.containsKey s ctx.EnemyStatus
        | PerStack _
        | PerEnemyStack _ -> true
        | Not c -> not (holds ctx c)

    let private stackFactor ctx cond =
        let per stacks every = floor (float stacks / every)
        match cond with
        | PerStack (b, every) -> per (Map.tryFind b ctx.Buffs |> Option.defaultValue 0) every
        | PerEnemyStack (s, every) -> per (Map.tryFind s ctx.EnemyStatus |> Option.defaultValue 0) every
        | _ -> 1.0

    /// The mod's value in this context, or None if a condition fails.
    let eval ctx (m: Mod) =
        if m.Conds.IsEmpty then Some m.Value
        elif List.forall (holds ctx) m.Conds then
            Some(m.Value * List.fold (fun k c -> k * stackFactor ctx c) 1.0 m.Conds)
        else
            None

/// A stat as a key: a small number for the stats that are cases of their own, a text for the
/// ones named by a string. (An ordered map keyed by the stat itself compares stats
/// structurally at every step of every lookup, which was a quarter of a fight's time.)
module StatKey =
    let private damageType (t: DamageType) =
        match t with
        | Physical -> 0
        | Lightning -> 1
        | Cold -> 2
        | Fire -> 3
        | Chaos -> 4

    let private scope (s: DamageScope) =
        match s with
        | AllDamage -> 0
        | Elemental -> 1
        | OfType t -> 2 + damageType t

    let private ailment (a: Ailment) =
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

    let private ailmentOption (a: Ailment option) =
        match a with
        | None -> 0
        | Some a -> 1 + ailment a

    /// The stat's number, or -1 for one that is known by its text (`name`).
    let rec code (s: Stat) : int =
        match s with
        | Named _ | Attribute _ -> -1
        | ForMinions inner -> (let c = code inner in if c < 0 then -1 else 1000 + c)
        | Damage sc -> scope sc
        | AddedMin t -> 10 + damageType t
        | AddedMax t -> 20 + damageType t
        | AddedDamageEffect -> 30
        | LuckyDamage -> 31
        | CritChance -> 32
        | CritBonus -> 33
        | HitChance -> 34
        | DoubleDamageChance -> 35
        | AreaOfEffect -> 36
        | AdditionalProjectiles -> 37
        | Pierce -> 38
        | Chains -> 39
        | Penetration t -> 40 + damageType t
        | Speed -> 50
        | CooldownRecovery -> 51
        | SkillDuration -> 52
        | DotTaken -> 53
        | Armour -> 54
        | PhysicalReduction -> 55
        | AilmentThreshold -> 56
        | PoiseThreshold -> 57
        | MaxLife -> 58
        | MaxMana -> 59
        | Resist t -> 60 + damageType t
        | MaxResist t -> 70 + damageType t
        | DamageTaken sc -> 80 + scope sc
        | MaxEnergyShield -> 90
        | Evasion -> 91
        | Deflection -> 92
        | LifeRegen -> 93
        | LifeRegenPercent -> 94
        | ManaRegen -> 95
        | ManaCostEfficiency -> 96
        | ManaCost -> 97
        | ManaOnKill -> 98
        | ManaLeech -> 99
        | SkillConvert (f, i) -> 100 + damageType f * 5 + damageType i
        | Convert (f, i) -> 130 + damageType f * 5 + damageType i
        | GainAs (f, i) -> 160 + damageType f * 5 + damageType i
        | AilmentChance a -> 200 + ailment a
        | AilmentMagnitude a -> 220 + ailmentOption a
        | AilmentDuration a -> 240 + ailmentOption a
        | AilmentStacks a -> 260 + ailment a
        | Buildup a -> 280 + ailment a

    /// The text of a stat without a number.
    let rec name (s: Stat) : string =
        match s with
        | Named n -> n
        | Attribute a -> "\u0001" + a
        | ForMinions inner -> "\u0002" + name inner
        | _ -> ""

module ModKind =
    let number (k: ModKind) =
        match k with
        | Base -> 0
        | Inc -> 1
        | More -> 2
        | Flag -> 3
        | Override -> 4

/// A mod with its stat's key (in the part of a db that is not indexed yet).
type KeyedMod = { Code: int; Name: string; Mod: Mod }

/// Mods by their stats' keys and their kinds, newest first: a numbered stat's under its
/// number x 8 + its kind's, a named one's under its name, one list per kind.
type ModIndex =
    { Coded: System.Collections.Generic.Dictionary<int, Mod list>
      Names: System.Collections.Generic.Dictionary<string, Mod list[]> }

/// Mods indexed by stat, so a query only looks at the mods that can matter. An index is
/// built once and never changed; mods added later lie in a short list before the indexes
/// (`Recent`, newest first), which becomes an index of its own once it grows (`Layers`,
/// newest first; folded into one when there are many). `Order` is every mod, newest first.
/// Two dbs are equal when they hold the same mods in the same order.
[<CustomEquality; NoComparison>]
type ModDb =
    { Layers: ModIndex list
      Recent: KeyedMod list
      RecentCount: int
      Order: Mod list }
    override this.Equals(other: obj) =
        match other with
        | :? ModDb as o -> this.Order = o.Order
        | _ -> false
    override this.GetHashCode() = List.length this.Order

module ModDb =
    /// The index of these mods (newest first).
    let private index (order: Mod list) : ModIndex =
        let coded = System.Collections.Generic.Dictionary<int, Mod list>()
        let names = System.Collections.Generic.Dictionary<string, Mod list[]>()
        // oldest first, each put before the ones of its stat: newest first per stat
        for m in List.rev order do
            let c = StatKey.code m.Stat
            let k = ModKind.number m.Kind
            if c >= 0 then
                let key = c * 8 + k
                match coded.TryGetValue key with
                | true, ms -> coded.[key] <- m :: ms
                | _ -> coded.[key] <- [ m ]
            else
                let n = StatKey.name m.Stat
                match names.TryGetValue n with
                | true, kinds -> kinds.[k] <- m :: kinds.[k]
                | _ ->
                    let kinds = Array.create 5 []
                    kinds.[k] <- [ m ]
                    names.[n] <- kinds
        { Coded = coded; Names = names }

    let empty: ModDb = { Layers = []; Recent = []; RecentCount = 0; Order = [] }

    /// With the recent mods as an index of their own (all in one index once there are many).
    let private settle (db: ModDb) : ModDb =
        if db.RecentCount = 0 then db
        elif db.Layers.Length >= 3 then { db with Layers = [ index db.Order ]; Recent = []; RecentCount = 0 }
        else { db with Layers = index (db.Recent |> List.map (fun k -> k.Mod)) :: db.Layers; Recent = []; RecentCount = 0 }

    let add (db: ModDb) (m: Mod) : ModDb =
        let db = if db.RecentCount >= 12 then settle db else db
        let c = StatKey.code m.Stat
        { db with
            Recent = { Code = c; Name = (if c < 0 then StatKey.name m.Stat else ""); Mod = m } :: db.Recent
            RecentCount = db.RecentCount + 1
            Order = m :: db.Order }

    let addList (mods: Mod list) (db: ModDb) =
        match mods with
        | [] -> db
        | _ when List.length mods > 12 ->
            // many at once: an index of their own, over what was there
            let db = settle db
            let order = List.fold (fun order m -> m :: order) db.Order mods
            if db.Layers.Length >= 3 then { db with Layers = [ index order ]; Order = order }
            else { db with Layers = index (List.rev mods) :: db.Layers; Order = order }
        | _ -> List.fold add db mods

    let ofList (mods: Mod list) = addList mods empty

    /// Every mod, stat by stat (in the stats' own order), the newest of a stat first.
    let toList (db: ModDb) = db.Order |> List.sortWith (fun a b -> compare a.Stat b.Stat)

    /// Whether any mod is of a stat like this.
    let hasStat (pred: Stat -> bool) (db: ModDb) = db.Order |> List.exists (fun m -> pred m.Stat)

    /// The mods of a stat and a kind, newest first, each given to `f`.
    let inline forStat (stat: Stat) (kind: ModKind) (db: ModDb) ([<InlineIfLambda>] f: Mod -> unit) =
        let c = StatKey.code stat
        let k = ModKind.number kind
        if c >= 0 then
            for r in db.Recent do
                if r.Code = c && r.Mod.Kind = kind then f r.Mod
            let key = c * 8 + k
            for layer in db.Layers do
                match layer.Coded.TryGetValue key with
                | true, ms -> for m in ms do f m
                | _ -> ()
        else
            let n = StatKey.name stat
            for r in db.Recent do
                if r.Code < 0 && r.Mod.Kind = kind && r.Name = n then f r.Mod
            for layer in db.Layers do
                match layer.Names.TryGetValue n with
                | true, kinds -> for m in kinds.[k] do f m
                | _ -> ()

/// Queries over a mod db, PoE style:
///   total = base * (1 + sum increased / 100) * product (1 + more / 100)
module Mods =
    let private fold ctx kind stats (db: ModDb) f init =
        let mutable acc = init
        for stat in stats do
            ModDb.forStat stat kind db (fun m ->
                match Mod.eval ctx m with
                | Some v -> acc <- f acc v
                | None -> ())
        acc

    let sumBase ctx stats db = fold ctx Base stats db (+) 0.0
    let sumInc ctx stats db = fold ctx Inc stats db (+) 0.0
    let inc ctx stats db = 1.0 + sumInc ctx stats db / 100.0
    let more ctx stats db = fold ctx More stats db (fun acc v -> acc * (1.0 + v / 100.0)) 1.0

    /// increased * more, the usual multiplier applied to a base value.
    let mult ctx stats db = max 0.0 (inc ctx stats db * more ctx stats db)
    let total ctx stats db = sumBase ctx stats db * mult ctx stats db
    let flag ctx stat db = fold ctx Flag [ stat ] db (fun acc v -> acc || v <> 0.0) false
    let override' ctx stat db = fold ctx Override [ stat ] db (fun _ v -> Some v) None

    /// Mods granted to minions by "Minions deal X" style mods on the owner.
    let forMinions (mods: Mod list) =
        mods
        |> List.choose (fun m ->
            match m.Stat with
            | ForMinions s -> Some { m with Stat = s }
            | _ -> None)
