namespace PoEsie

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
        if List.forall (holds ctx) m.Conds then
            Some(m.Value * List.fold (fun k c -> k * stackFactor ctx c) 1.0 m.Conds)
        else
            None

/// Mods indexed by stat, so a query only looks at the mods that can matter.
type ModDb = Map<Stat, Mod list>

module ModDb =
    let empty: ModDb = Map.empty

    let add (db: ModDb) (m: Mod) : ModDb =
        Map.add m.Stat (m :: (Map.tryFind m.Stat db |> Option.defaultValue [])) db

    let addList (mods: Mod list) (db: ModDb) = List.fold add db mods
    let ofList (mods: Mod list) = addList mods empty
    let toList (db: ModDb) = db |> Map.toList |> List.collect snd

/// Queries over a mod db, PoE style:
///   total = base * (1 + sum increased / 100) * product (1 + more / 100)
module Mods =
    let private fold ctx kind stats (db: ModDb) f init =
        let mutable acc = init
        for stat in stats do
            match Map.tryFind stat db with
            | Some ms ->
                for m in ms do
                    if m.Kind = kind then
                        match Mod.eval ctx m with
                        | Some v -> acc <- f acc v
                        | None -> ()
            | None -> ()
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
