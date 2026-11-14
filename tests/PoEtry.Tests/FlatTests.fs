/// The fast engine (`Flat`) against the reference (`Sim`): the same fight with the same seed
/// must give the same outcome, bit for bit. Fights built by hand, and fights made at random
/// inside (and a little outside) what `Flat.compile` accepts.
module PoEtry.FlatTests

open PoEtry

/// Numbers are the same within this: relative, and absolute near zero. (The fast engine may
/// add and multiply in another order than the reference where that is faster.)
let tolerance = 1e-9

let private exactFloat (a: float) (b: float) = a = b || (System.Double.IsNaN a && System.Double.IsNaN b)
let private nearFloat (a: float) (b: float) =
    exactFloat a b || abs (a - b) <= tolerance || abs (a - b) <= tolerance * max (abs a) (abs b)

let private mapDifference (field: string) (show: 'v -> string) (same: 'v -> 'v -> bool) (a: Map<string, 'v>) (b: Map<string, 'v>) : string option =
    let keys = (a |> Map.toList |> List.map fst) @ (b |> Map.toList |> List.map fst) |> List.distinct |> List.sort
    keys |> List.tryPick (fun k ->
        match Map.tryFind k a, Map.tryFind k b with
        | Some x, Some y when same x y -> None
        | x, y ->
            let say v = match v with Some v -> show v | None -> "nothing"
            Some(sprintf "%s[%s]: reference %s, flat %s" field k (say x) (say y)))

/// The first field in which two outcomes differ (every field `Summary` and `Report` read),
/// with both values; None if they are the same. `same`: when two numbers are the same;
/// what is counted or named must be identical either way.
let differenceWith (same: float -> float -> bool) (reference: Outcome) (flat: Outcome) : string option =
    let float' (field: string) (a: float) (b: float) =
        if same a b then None else Some(sprintf "%s: reference %.17g, flat %.17g" field a b)
    let option' (field: string) (a: float option) (b: float option) =
        match a, b with
        | None, None -> None
        | Some x, Some y when same x y -> None
        | _ -> Some(sprintf "%s: reference %A, flat %A" field a b)
    let showFloat (v: float) = sprintf "%.17g" v
    let kills () =
        let alike = reference.Kills.Length = flat.Kills.Length && List.forall2 (fun (n, r, t) (n', r', t') -> n = n' && r = r' && same t t') reference.Kills flat.Kills
        if alike then None else Some(sprintf "Kills: reference %A, flat %A" reference.Kills flat.Kills)
    [ (fun () -> float' "Duration" reference.Duration flat.Duration)
      (fun () -> option' "Ttk" reference.Ttk flat.Ttk)
      kills
      (fun () -> mapDifference "Counts" string (=) reference.Counts flat.Counts)
      (fun () -> mapDifference "DamageBy" showFloat same reference.DamageBy flat.DamageBy)
      (fun () -> mapDifference "DamageByType" showFloat same reference.DamageByType flat.DamageByType)
      (fun () -> mapDifference "Uptime" showFloat same reference.Uptime flat.Uptime)
      (fun () -> option' "Died" reference.Died flat.Died)
      (fun () -> float' "DamageTaken" reference.DamageTaken flat.DamageTaken)
      (fun () -> float' "ManaStarved" reference.ManaStarved flat.ManaStarved)
      (fun () -> float' "Evened" reference.Evened flat.Evened)
      (fun () -> float' "MinLife" reference.MinLife flat.MinLife)
      (fun () -> mapDifference "ManaGained" showFloat same reference.ManaGained flat.ManaGained) ]
    |> List.tryPick (fun check -> check ())

/// The first difference beyond the tolerance.
let difference (reference: Outcome) (flat: Outcome) : string option = differenceWith nearFloat reference flat

/// Where the two engines first part, by what they tell of every event and hit: the place,
/// and whether up to there all their numbers were the same within the tolerance and what
/// differs there is a decision (which event comes next, a hit or none, a critical hit or
/// not), not a number. That is what a rounding difference flipping a threshold looks like;
/// a number that is off beyond the tolerance while the events still agree is a mistake.
let divergence (fight: Fight) (program: Flat.Program) (seed: uint32) : bool * string =
    let record () =
        let told = ResizeArray<float * string * float>()
        told, (fun (t: float) (kind: string) (who: string) (what: string) (v: float) -> told.Add((t, kind + " " + who + " " + what, v)))
    let a, tellA = record ()
    let b, tellB = record ()
    Sim.traced tellA fight seed |> ignore
    Flat.traced tellB program seed |> ignore
    let say (xs: ResizeArray<float * string * float>) (i: int) =
        if i < xs.Count then (let t, what, v = xs.[i] in sprintf "%.17g %s %.17g" t what v) else "(nothing more)"
    let n = min a.Count b.Count
    let mutable at = 0
    let mutable numbersOff = false
    let mutable go = true
    while go && at < n do
        let ta, wa, va = a.[at]
        let tb, wb, vb = b.[at]
        if wa <> wb then go <- false
        elif not (nearFloat ta tb && nearFloat va vb) then
            numbersOff <- true
            go <- false
        else at <- at + 1
    if at = n && a.Count = b.Count then false, sprintf "the %d events and hits are told the same" n
    else
        let before = [ for i in max 0 (at - 3) .. at - 1 -> "    " + say a i ] |> String.concat "\n"
        not numbersOff, sprintf "event %d differs\n%s\n  reference: %s\n  flat:      %s" at before (say a at) (say b at)

/// What comparing the two engines on a fight and a seed gave.
type Verdict =
    | NotCompiled
    /// The same in every bit.
    | Exact
    /// The same within the tolerance.
    | Within
    /// The two went different ways at a decision, their numbers the same within the tolerance until then.
    | AtThreshold of string
    /// Different, and not for rounding.
    | Wrong of string

let verdict (fight: Fight) (seed: uint32) : Verdict =
    match Flat.compile fight with
    | Error _ -> NotCompiled
    | Ok program ->
        let reference, flat = Sim.run fight seed, Flat.run program seed
        match differenceWith exactFloat reference flat with
        | None -> Exact
        | Some _ ->
            match difference reference flat with
            | None -> Within
            | Some d ->
                let threshold, where = divergence fight program seed
                let text = sprintf "seed %d: %s\n  %s" seed d where
                if threshold then AtThreshold text else Wrong text

/// Both engines on one fight and seed: None if the fight does not compile; else whether
/// the outcomes are the same (or differ only as rounding can make them), and if not, what
/// differs and where it starts.
let check (fight: Fight) (seed: uint32) : Result<unit, string> option =
    match verdict fight seed with
    | NotCompiled -> None
    | Wrong d -> Some(Error d)
    | _ -> Some(Ok())

// ---- fights made at random ----

/// A fight made from a number: the same number, the same fight. Mostly inside what the fast
/// engine plays; now and then with something it does not (it must then refuse, or be right).
let randomFight (case: uint32) : Fight * string =
    let rng = Rng(case * 2654435761u + 12345u)
    let chance (p: float) = rng.Float() < p
    let pick (xs: 'a list) = xs.[min (xs.Length - 1) (int (rng.Float() * float xs.Length))]
    let between (lo: float) (hi: float) = lo + (hi - lo) * rng.Float()
    let whole (lo: int) (hi: int) = lo + int (rng.Float() * float (hi - lo + 1)) |> min hi
    let types = [ Physical; Lightning; Cold; Fire; Chaos ]
    let notes = ResizeArray<string>()
    let note (x: string) = notes.Add x
    // the player's mods
    let conds () =
        if chance 0.7 then []
        else
            [ pick
                  [ IsCrit; SkillIs Spell; SkillIs Attack; WhileBuff "Buffed"; WhileBuff "HitRecently"; WhileBuff "CritRecently"; EnemyHas "Shocked"
                    EnemyHas "Ignited"; EnemyHas "Cursed"; EnemyHas "Isolated"; EnemyHas "LowLife"; PerStack("PowerCharge", 1.0); PerStack("Buffed", 1.0)
                    WhileBuff "EmpoweredAttacks"; WhileBuff "EmpoweredAttacks:Temper"; WhileBuff "EmpoweredRepeatable:Tips"; WhileBuff "EmpoweredAttacks:Cry"
                    PerEnemyStack("Poisoned", 2.0); PerEnemyStack("Marked by pain", 1.0); Not(WhileBuff "FullMana"); WhileBuff "LowMana"; WhileBuff "KilledRecently"
                    WhileBuff "ConsumedPowerChargeRecently"; WhileBuff "TriggeredRecently"; WhileBuff "Tide"; EnemyHas "Exposed"; EnemyHas "FullyBrokenArmour" ] ]
    let withConds (m: Mod) = { m with Conds = conds () }
    let modPool : (unit -> Mod list) list =
        [ (fun () -> [ Mod.inc (Damage AllDamage) (between 5.0 150.0) |> withConds ])
          (fun () -> [ Mod.more (Damage AllDamage) (between -20.0 40.0) |> withConds ])
          (fun () -> [ Mod.inc (Damage(OfType(pick types))) (between 5.0 150.0) |> withConds ])
          (fun () -> [ Mod.inc (Damage Elemental) (between 5.0 80.0) |> withConds ])
          (fun () -> [ Mod.inc CritChance (between 10.0 400.0) |> withConds ])
          (fun () -> [ Mod.base' CritChance (between 1.0 10.0) |> withConds ])
          (fun () -> [ Mod.base' CritBonus (between 10.0 150.0) |> withConds ])
          (fun () -> [ Mod.inc Speed (between -10.0 60.0) |> withConds ])
          (fun () -> [ Mod.more Speed (between -10.0 20.0) |> withConds ])
          (fun () -> (let t = pick types in Mod.added t (between 1.0 20.0) (between 20.0 90.0)))
          (fun () -> [ Mod.base' (Penetration(pick types)) (between 5.0 40.0) |> withConds ])
          (fun () -> [ Mod.base' (AilmentChance(pick [ Bleed; Poison; Ignite; Shock; Daze; Maim; Blind; Hinder ])) (between 5.0 100.0) |> withConds ])
          (fun () -> [ Mod.inc (AilmentMagnitude(pick [ None; Some Ignite; Some Shock; Some Poison; Some Chill ])) (between 10.0 80.0) ])
          (fun () -> [ Mod.inc (AilmentDuration(pick [ None; Some Ignite; Some Shock; Some Poison ])) (between -30.0 80.0) ])
          (fun () -> [ Mod.base' (AilmentStacks(pick [ Poison; Bleed; Ignite ])) (float (whole 1 4)) ])
          (fun () -> [ Mod.inc (Buildup(pick [ Freeze; HeavyStun; Electrocute; Pin ])) (between 50.0 4000.0) ])
          (fun () -> [ Mod.flag (Named(pick [ "CanElectrocute"; "CanPin"; "AllDamageIgnites"; "AllDamageShocks"; "AllDamageChills" ])) ])
          (fun () -> [ Mod.base' (Named "Accuracy") (between 200.0 6000.0) ])
          (fun () -> [ Mod.base' ManaLeech (between 1.0 12.0) ])
          (fun () -> [ Mod.base' (Named "LifeLeech") (between 1.0 12.0) ])
          (fun () -> [ Mod.base' (Named(pick [ "LifeOnHit"; "ManaOnHit" ])) (between 1.0 9.0) ])
          (fun () -> [ Mod.base' ManaOnKill (between 5.0 30.0) ])
          (fun () -> [ Mod.flag (Named "CullingStrike") |> withConds ])
          (fun () -> [ Mod.base' (Named(pick [ "ArmourBreakOfPhysical"; "ArmourBreakOfDamage"; "ArmourBreakFlat" ])) (between 5.0 400.0) ])
          (fun () -> [ Mod.flag (Named(pick [ "BrokenArmourAllDamage"; "BrokenArmourFire"; "BrokenArmourColdLightning" ])) ])
          (fun () -> [ Mod.base' (Named(pick [ "CloseCombatMore"; "FarCombatMore" ])) (between 5.0 40.0) ])
          (fun () -> [ Mod.inc (Named "EnemyDamageTaken") (between 3.0 20.0) |> Mod.whenever (EnemyHas(pick [ "Cursed"; "Marked"; "Shocked" ])) ])
          (fun () -> [ Mod.base' (Named "ChargeSkipConsume") (between 10.0 70.0) ])
          (fun () -> [ Mod.base' (Named "ExtraChargeChance:Any") (between 10.0 70.0) ])
          (fun () -> [ Mod.base' (Named "AftershockChance") (between 10.0 60.0) ])
          (fun () -> [ Mod.base' (Named "RageOnHit") (between 1.0 5.0) ])
          (fun () -> [ Mod.base' (Named(pick [ "RageRegen"; "RageWhenHit"; "RageOnKill"; "RageLossDelay" ])) (between 1.0 6.0); Mod.more (Damage AllDamage) 1.0 |> Mod.whenever (PerStack("Rage", 3.0)) ])
          (fun () -> [ Mod.base' (Named "LifeLossPercentPerMinutePerRage") (between 1.0 20.0); Mod.base' (Named "RageRegen") 3.0 ])
          (fun () -> [ Mod.flag (Named "NoRageLoss") ])
          (fun () -> [ Mod.base' (Named "ManaPercentOnConsumePower") (between 1.0 5.0) ])
          (fun () -> [ Mod.flag LuckyDamage |> withConds ])
          (fun () -> [ Mod.base' (Named "LuckyDamageChance") (between 10.0 60.0) ])
          (fun () -> [ Mod.base' DoubleDamageChance (between 5.0 40.0) ])
          (fun () -> (let a = pick types in let b = pick types in if a = b then [] else [ Mod.base' (Convert(a, b)) (between 10.0 100.0) ]))
          (fun () -> (let a = pick types in let b = pick types in [ Mod.base' (GainAs(a, b)) (between 5.0 50.0) |> withConds ]))
          (fun () -> [ Mod.inc CooldownRecovery (between 5.0 80.0) ])
          (fun () -> [ Mod.inc ManaCost (between -30.0 40.0) ])
          (fun () -> [ Mod.inc ManaCostEfficiency (between 5.0 40.0) ])
          (fun () -> [ Mod.inc SkillDuration (between -20.0 60.0) ])
          (fun () -> [ Mod.inc AreaOfEffect (between -20.0 150.0) ])
          (fun () -> [ Mod.base' AdditionalProjectiles (float (whole 1 3)) ])
          (fun () -> [ Mod.base' Pierce (float (whole 1 2)); Mod.base' Chains (float (whole 0 2)) ])
          (fun () -> [ Mod.base' (Named(pick [ "ExtraProjectileChance1"; "ExtraProjectileChance2"; "PierceChance" ])) (between 10.0 180.0) ])
          (fun () -> [ Mod.flag (Named(pick [ "LuckyCrit"; "InevitableCrits"; "BifurcateCrits" ])) ])
          (fun () -> [ Mod.base' (Named "NegateEnemyResistChance") (between 10.0 60.0) ])
          (fun () -> [ Mod.flag (Named "RevealWeakness"); Mod.base' (Named "OpenWeaknessMore") 30.0; Mod.more (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "OpenWeakness") ])
          (fun () -> [ Mod.base' (Named "MaxCurses") 1.0 ])
          (fun () -> [ Mod.inc (Named "ExposureEffect") (between 10.0 50.0) ])
          (fun () -> [ Mod.base' (Named "FixedUseTime") (between 0.05 0.3) |> withConds ])
          (fun () -> [ Mod.base' (Named "ExtraCostPercentOfMaxMana") (between 1.0 6.0) |> Mod.whenever (SkillIs Spell) ])
          (fun () -> [ Mod.base' (Named "LosePoolsOnUse") (between 1.0 5.0) ])
          (fun () -> [ Mod.base' (Named "ManaPercentOnKill") (between 1.0 5.0) ])
          (fun () -> [ Mod.base' (Named "DotDamage") 0.0; Mod.inc (Named "DotDamage") (between 10.0 80.0) ])
          (fun () -> [ Mod.inc (Named "FasterAilments") (between 10.0 50.0) ])
          (fun () -> [ Mod.base' (Named "RageEffect") 0.0; Mod.inc (Named "RageEffect") (between 10.0 50.0) ])
          (fun () -> [ Mod.base' (Named "ConsumeChargeCount") (float (whole 1 2)) ])
          (fun () -> [ Mod.flag (Named "NoHitDamage") |> Mod.whenever (SkillNamed "Skill 2") ])
          (fun () -> [ Mod.override' HitChance (between 40.0 100.0) |> Mod.whenever (SkillNamed "Skill 1") ]) ]
    // (now and then a build around something: ailments that deal damage, charges, leech)
    let theme =
        match whole 0 9 with
        | 0 -> [ Mod.base' (AilmentChance Bleed) (between 40.0 100.0); Mod.base' (AilmentChance Poison) (between 40.0 100.0); Mod.base' (AilmentStacks Poison) (float (whole 1 6)); Mod.base' (GainAs(Fire, Chaos)) 50.0; Mod.base' (GainAs(Cold, Physical)) 50.0; Mod.base' (GainAs(Lightning, Physical)) 50.0 ]
        | 1 -> [ Mod.base' (Named "ChargeSkipConsume") (between 20.0 80.0); Mod.base' (Named "ExtraChargeChance:PowerCharge") 40.0; Mod.more (Damage AllDamage) 10.0 |> Mod.whenever (PerStack("PowerCharge", 1.0)) ]
        | 2 -> [ Mod.base' ManaLeech 8.0; Mod.base' (Named "LifeLeech") 8.0; Mod.base' (GainAs(Fire, Physical)) 60.0; Mod.base' (GainAs(Cold, Physical)) 60.0; Mod.inc (Named "ManaLeechAmount") 30.0 ]
        | 3 -> [ Mod.inc (Buildup Freeze) 2000.0; Mod.inc (Buildup HeavyStun) 2000.0; Mod.flag (Named "CanElectrocute"); Mod.inc (Buildup Electrocute) 3000.0; Mod.flag (Named "CanPin"); Mod.inc (Buildup Pin) 3000.0 ]
        | _ -> []
    let mods = theme @ [ for _ in 1 .. whole 0 14 do yield! (pick modPool) () ]
    // its skills
    let damage () =
        [ for _ in 1 .. whole 1 2 -> pick types, (let lo = between 5.0 200.0 in { Min = lo; Max = lo + between 0.0 300.0 }) ] |> Map.ofList
    let delivery () =
        match whole 0 7 with
        | 0 | 1 | 2 -> Single
        | 3 -> AreaAtTarget(between 8.0 40.0)
        | 4 -> AreaAroundSelf(between 15.0 60.0)
        | 5 -> Projectiles { ProjectileShape.make (whole 1 5) with Pierce = whole 0 2; Chains = whole 0 2; CanShotgun = chance 0.5; Spread = between 10.0 90.0 }
        | 6 -> Projectiles { ProjectileShape.make (whole 1 4) with Explosion = Some(between 8.0 20.0); PerCharge = (if chance 0.5 then Some("PowerCharge", 1) else None) }
        | _ -> Projectiles { ProjectileShape.make (whole 2 6) with Rain = Some(between 15.0 30.0, between 5.0 12.0) }
    let skill (i: int) (spam: bool) : Skill =
        let attack = chance 0.4
        let tags =
            (if attack then [ Attack ] else [ Spell ])
            @ (if chance 0.3 then [ Melee ] else []) @ (if chance 0.2 then [ Slam ] else []) @ (if chance 0.2 then [ Area ] else [])
            @ (if chance 0.1 then [ Tag "Type:Sustained" ] else []) @ (if chance 0.1 then [ Tag "SpellDot" ] else [])
            @ (if chance 0.1 then [ Warcry ] else []) @ (if chance 0.2 then [ Tag "Type:Barrageable" ] else [])
        { Skill.make (sprintf "Skill %d" i) tags with
            BaseDamage = (if chance 0.08 then Map.empty else damage ())
            BaseMultiplier = (if chance 0.3 then between 0.5 2.5 else 1.0)
            BaseCrit = (if chance 0.2 then 0.0 else between 3.0 30.0)
            BaseTime = (if chance 0.05 then 0.0 else between 0.25 1.4)
            Cooldown = (if not spam && chance 0.7 then Some(between 0.5 6.0) else None)
            StoredUses = (if chance 0.3 then whole 2 3 else 1)
            Hits =
                (match whole 0 5 with
                 | 0 -> [ 0.3, 0.5; 0.6, 0.5; 1.0, 1.0 ]
                 | 1 -> [ 0.5, 1.0 ]
                 | 2 -> []
                 | _ -> [ 1.0, 1.0 ])
            Delivery = delivery ()
            Mods =
                [ if chance 0.3 then yield! (pick modPool) ()
                  if chance 0.15 then yield Mod.base' (Named "ConsumeChargeEveryMs") (between 300.0 3000.0)
                  if chance 0.1 then yield Mod.flag (Named "NeedsCharge")
                  if chance 0.1 then yield Mod.flag (Named "CannotInflict:Marked by pain")
                  if chance 0.1 then yield Mod.base' (Named "DoubleCooldownUses") 1.0 ]
            Consumes = (if chance 0.25 then Some(pick [ "PowerCharge"; "FrenzyCharge" ]) else None)
            Cost = (if chance 0.5 then between 3.0 45.0 else 0.0)
            CostPerSecond = (if chance 0.1 then between 3.0 30.0 else 0.0)
            HitDelay = (if chance 0.15 then between 0.1 1.0 else 0.0)
            Dots = (if chance 0.2 then Map.ofList [ pick types, between 20.0 300.0 ] else Map.empty)
            Duration = (if chance 0.4 then between 0.5 5.0 else 0.0) }
    let withParts (s: Skill) : Skill =
        if chance 0.75 then s
        else
            let part (suffix: string) (flags: string list) =
                { skill 0 true with
                    Name = s.Name + suffix
                    Mods = flags |> List.map (fun f -> Mod.flag (Named f)) }
            let parts =
                match whole 0 4 with
                | 0 -> [ part ": Wave" [] ]
                | 1 -> [ part ": Explosion" []; part "-Charged" [] ]
                | 2 -> [ part ": Echo" [ "OncePerUse" ]; part ": Spark" [ "OnlyWithCharge" ] ]
                | 3 -> [ part ": Plain" [ "NotWithCharge" ]; part ": Rest" [ "NotEnraged" ] ]
                | _ -> [ part ": Hidden" [ "OnlyTriggered" ] ]
            { s with Parts = parts }
    let count = whole 1 4
    let skills = [ for i in 1 .. count -> withParts (skill i (i = count)) ]
    // (with a cooldown: a skill that triggers itself without one never ends)
    let triggered = if chance 0.3 then [ { skill 9 false with Name = "Echo"; Tags = Set.ofList [ Spell; Triggered ]; Cooldown = Some(between 0.2 2.0) } ] else []
    let names = skills |> List.map (fun s -> s.Name)
    // what it does when
    let buffMods () = if chance 0.6 then (pick modPool) () else []
    let reaction () : Reaction =
        let simple () =
            match whole 0 13 with
            | 0 | 1 -> GrantBuff(pick [ "Buffed"; "Tide"; "Onslaught" ], between 0.5 6.0, whole 1 5, buffMods ())
            | 2 -> ApplyDebuff(pick [ "Marked by pain"; "Weakened"; "Mark:Sniper" ], between 0.5 6.0, whole 1 6, [ Mod.inc (DamageTaken(pick [ AllDamage; Elemental; OfType Fire ])) (between 2.0 15.0) ])
            | 3 -> ApplyCurse(pick [ "Hex"; "Despair"; "Mark:Sniper"; "Curse:Frailty" ], between 1.0 8.0, [ Mod.base' (Resist(pick types)) (between -40.0 -5.0); Mod.inc (DamageTaken AllDamage) (between 1.0 20.0) ])
            | 4 -> ApplyExposure(pick types, between 5.0 40.0, between 1.0 8.0)
            | 5 -> GrowExposure(pick types, 10.0, 5.0, 30.0, 0.25, whole 1 5, between 1.0 6.0)
            | 6 -> RemoveBuff(pick [ "Buffed"; "Tide"; "PowerCharge" ])
            | 7 -> RemoveDebuff(pick [ "Marked by pain"; "Shocked"; "Curse:Hex" ])
            | 8 | 9 -> GainResource(pick [ "PowerCharge"; "FrenzyCharge"; "Mote" ], whole 1 2)
            | 10 -> DropRemnant(pick [ "PowerCharge"; "Mote" ], between 0.1 1.5, between 0.0 60.0)
            | 11 -> SetStacks(pick [ "Buffed"; "Readied"; "EmpoweredAttacks:Temper"; "EmpoweredRepeatable:Tips" ], whole 1 4)
            | 12 -> (if triggered.IsEmpty then GainEnergy(pick [ "Cast on Crit"; "Invocation"; "No such gem" ], between 5.0 40.0) else TriggerSkill "Echo")
            | _ -> NoReaction
        match whole 0 11 with
        | 11 -> ChargeBuff(pick [ "Charged"; "Tide" ], pick [ "PowerCharge"; "FrenzyCharge" ], buffMods (), between 0.5 3.0, pick [ 0.0; 30.0; 100.0 ])
        | 10 -> Empower(pick [ "EmpoweredAttacks"; "EmpoweredAttacks:Cry"; "Buffed" ], pick [ 1.0; 5.0; 0.5 ], pick [ 10.0; 20.0; 3.0 ])
        | 0 -> SpendStack(pick [ "Buffed"; "Readied"; "PowerCharge" ], simple ())
        | 1 -> IfBuff(pick [ "Buffed"; "Tide"; "CritRecently" ], simple ())
        | 2 -> IfEnemyHas(pick [ "Shocked"; "Ignited"; "Cursed"; "LowLife"; "Marked by pain"; "FullLife" ], simple ())
        | _ -> simple ()
    let rules : Rule list =
        [ for _ in 1 .. whole 0 6 ->
            { Hook = pick [ OnUse; OnHit; OnHit; OnCrit; OnKill; OnKillOf(pick [ "Shocked"; "RareOrUnique"; "LowLife" ]); OnInflict(pick [ Ignite; Shock; Poison; Bleed; Chill ]); WhenHit ]
              Filter = pick [ AnySkill; AnySkill; WithTag Spell; WithTag Attack; WithTagNot(Spell, Triggered); Called(pick names); WithTags([ Spell ], [ Slam ]) ]
              Chance = pick [ 100.0; 100.0; between 5.0 95.0; 0.0 ]
              Reaction = reaction () } ]
    let condition () : View -> bool =
        match whole 0 8 with
        | 0 -> fun v -> not (v.Buffs.ContainsKey "Buffed")
        | 1 -> fun v -> not (v.EnemyStatus.ContainsKey "Cursed")
        | 2 -> fun v -> v.EnemyLifeFraction < 0.6
        | 3 -> fun v -> (Map.tryFind "PowerCharge" v.Buffs |> Option.defaultValue 0) >= 2
        | 4 -> fun v -> v.LastUsed <> "Skill 1"
        | 5 -> fun v -> v.EnemiesAlive > 1 || v.Time > 2.0
        | 6 -> fun v -> v.Energy.IsEmpty || v.EnemyRarity = Unique
        | _ -> fun _ -> true
    let rotation = [ for s in skills -> { Use = s.Name; When = (if s.Name = List.last names then (fun _ -> true) else condition ()) } ]
    let vitals =
        if chance 0.45 then None
        else
            Some
                { PlayerStats.basic (between 500.0 3000.0) (between 60.0 600.0) with
                    ManaRegen = between 2.0 40.0
                    LifeRegen = (if chance 0.5 then between 1.0 50.0 else 0.0)
                    EnergyShield = (if chance 0.4 then between 50.0 800.0 else 0.0)
                    EnergyShieldRecharge = between 10.0 80.0
                    ESRegen = (if chance 0.2 then between 1.0 10.0 else 0.0)
                    StartLife = (if chance 0.2 then between 0.2 0.9 else 1.0)
                    LifeReserved = (if chance 0.1 then between 0.1 0.5 else 0.0)
                    LifeFrozenWithES = chance 0.05
                    CostsLife = chance 0.12
                    LifeOnKill = (if chance 0.2 then between 5.0 50.0 else 0.0)
                    ESOnKill = (if chance 0.2 then between 5.0 50.0 else 0.0)
                    LifeRecoveryRate = (if chance 0.2 then between 0.5 1.5 else 1.0)
                    ManaRecoveryRate = (if chance 0.2 then between 0.5 1.5 else 1.0)
                    LifeLeechRate = (if chance 0.2 then between 0.5 2.0 else 1.0)
                    SurroundedAt = whole 1 5
                    Ward = (if chance 0.1 then between 10.0 200.0 else 0.0)
                    WardRegen = 5.0
                    LifeRecharge = (if chance 0.1 then between 5.0 50.0 else 0.0)
                    LifeFlask = (if chance 0.3 then Some { Amount = 300.0; Duration = 3.0; ChargesPerUse = 10.0; MaxCharges = 60.0; ChargesPerSecond = 0.2; ChargeGainMult = 1.0; Instant = (if chance 0.5 then 0.4 else 0.0) } else None)
                    Armour = (if chance 0.5 then between 100.0 8000.0 else 0.0)
                    Evasion = (if chance 0.5 then between 100.0 8000.0 else 0.0)
                    Deflection = (if chance 0.3 then between 100.0 8000.0 else 0.0)
                    Resist = Map.ofList [ Fire, between -20.0 75.0; Cold, between -20.0 75.0; Lightning, between -20.0 75.0; Chaos, between -60.0 40.0 ]
                    Block = (if chance 0.4 then between 5.0 75.0 else 0.0)
                    LuckyBlock = chance 0.1
                    BlockedDamageTaken = (if chance 0.3 then between 5.0 50.0 else 0.0)
                    WardOnBlock = (if chance 0.1 then 20.0 else 0.0)
                    LifeOnBlock = (if chance 0.2 then 15.0 else 0.0)
                    ManaOnBlock = (if chance 0.2 then 9.0 else 0.0)
                    TakenAs = (if chance 0.15 then [ Physical, pick [ Fire; Chaos ], between 5.0 40.0 ] else [])
                    ManaBeforeLife = (if chance 0.15 then between 5.0 30.0 else 0.0)
                    BypassES = (if chance 0.1 then between 5.0 50.0 else 0.0)
                    ArmourAppliesTo = (if chance 0.15 then Map.ofList [ pick [ Fire; Cold; Lightning ], between 10.0 100.0 ] else Map.empty)
                    TakenMult = (if chance 0.2 then Map.ofList [ pick types, between 0.6 1.3 ] else Map.empty)
                    FlatPhysicalTaken = (if chance 0.1 then 12.0 else 0.0)
                    LessCritDamageTaken = (if chance 0.1 then 30.0 else 0.0)
                    EnemyCritChanceMult = (if chance 0.1 then 0.5 else 1.0)
                    ArmourVsProjectiles = (if chance 0.1 then 1.5 else 1.0)
                    ArmourVsCrits = (if chance 0.1 then 0.5 else 1.0)
                    StunThreshold = between 50.0 3000.0
                    AilmentThreshold = between 50.0 2000.0
                    FreezeThreshold = between 50.0 2000.0
                    AvoidStun = (if chance 0.2 then between 10.0 80.0 else 0.0)
                    Avoid = (if chance 0.3 then Map.ofList [ pick [ "Ignite"; "Shock"; "Chill"; "Freeze" ], between 10.0 90.0 ] else Map.empty)
                    SelfEffect = (if chance 0.2 then Map.ofList [ pick [ "Ignite"; "Shock"; "Chill" ], between 0.5 1.5 ] else Map.empty)
                    SelfDuration = (if chance 0.2 then Map.ofList [ pick [ "Ignite"; "Shock"; "Chill"; "Freeze" ], between 0.5 1.5 ] else Map.empty)
                    Recoup = (if chance 0.25 then [ pick [ "Life"; "Mana"; "ES" ], [ pick types; pick types ], between 5.0 40.0 ] else [])
                    UnluckyEvade = chance 0.05
                    LuckyDeflect = chance 0.05 }
    let player =
        { Actor.make "Player" mods (skills @ triggered) with
            Rotation = rotation
            Rules = rules
            Resources =
                [ if chance 0.5 then yield "PowerCharge", whole 0 4
                  if chance 0.2 then yield "FrenzyCharge", whole 1 3
                  if chance 0.1 then yield "Rage", whole 1 30
                  if chance 0.1 then yield "ConstantManaFlask", 1
                  if chance 0.1 then yield "Buffed", whole 1 3 ]
            MaxResources =
                [ if chance 0.8 then yield "PowerCharge", whole 2 6
                  if chance 0.35 then yield "Rage", whole 5 30
                  if chance 0.5 then yield "FrenzyCharge", whole 1 4
                  if chance 0.3 then yield "Mote", whole 1 9 ]
                |> Map.ofList
            ResourceDurations =
                [ if chance 0.5 then yield "PowerCharge", between 2.0 12.0
                  if chance 0.3 then yield "FrenzyCharge", between 2.0 12.0
                  if chance 0.2 then yield "Mote", between 1.0 4.0 ]
                |> Map.ofList
            KeptChargesPause = (if chance 0.3 then between 0.5 3.0 else 0.0)
            ParallelUses = chance 0.5
            HitPattern = (if chance 0.15 then Map.ofList [ "Skill 1", [ DuringUse 0.4, 1.0; After 0.8, 0.6 ] ] else Map.empty)
            Cycles =
                (if chance 0.15 then
                    [ { Name = "Conflux"; Period = between 0.7 3.0; Variants = [ for t in DamageType.elemental -> [ Mod.more (Damage(OfType t)) 40.0 ] ] } ]
                 else [])
            Archons =
                (if chance 0.4 then
                    [ { Name = "Archon"; OnIgniteStat = "ArchonOnIgnite"; Duration = between 1.0 5.0; Recovery = between 0.5 4.0; Mods = [ Mod.more (Damage Elemental) 30.0 ] } ]
                 else [])
            Vitals = vitals
            // (a meta gem's spell is a triggered one, which gives no energy: a skill that fed the
            // gem that triggers it would go on for ever)
            Metas =
                [ if not triggered.IsEmpty && chance 0.6 then
                    yield
                        { Meta.simple "Cast on Crit" (between 100.0 300.0) "Echo" with
                            Sources = [ for _ in 1 .. whole 0 3 -> pick [ CritHits(between 20.0 400.0); MeleeHits(between 20.0 200.0); MeleeKills(between 5.0 60.0); InflictsAilment(pick [ Ignite; Shock; Freeze; Chill; Poison ], between 50.0 800.0); CastsSpells(between 0.2 0.9); Blocks(between 20.0 60.0); DodgeRolls 1.0 ] ]
                            EnergyMult = between 0.8 1.6
                            EnergyMods = ModDb.ofList (if chance 0.4 then [ Mod.inc (Named "EnergyGenerated") (between 10.0 50.0) |> Mod.whenever (WhileBuff "CritRecently") ] else [])
                            RefundChance = (if chance 0.3 then between 10.0 60.0 else 0.0) }
                  if not triggered.IsEmpty && chance 0.4 then
                    let most = between 200.0 600.0
                    yield
                        { Meta.simple "Invocation" most "Echo" with
                            Release = Invocation(between 0.5 4.0)
                            Cost = most / float (whole 1 4)
                            Triggers = [ "Echo"; "Echo" ]
                            Sources = [ CritHits(between 50.0 400.0); CastsSpells(between 0.3 0.9) ] } ]
            // (what the buffs do to the defences: more armour while buffed)
            VitalsAt = (if chance 0.3 then vitals |> Option.map (fun v -> fun (buffs: Map<string, int>) -> { v with Armour = v.Armour + 500.0 * float (Map.tryFind "Buffed" buffs |> Option.defaultValue 0); Block = (if buffs.ContainsKey "Tide" then min 75.0 (v.Block + 20.0) else v.Block) }) else None)
            Charms =
                [ for i in 1 .. (if chance 0.35 then whole 1 3 else 0) ->
                    { Name = sprintf "charm %d" i
                      Trigger = pick [ "Frozen"; "Stunned"; "Ignited"; "Shocked"; "Slowed"; "KillRare"; "Hit:Fire"; "Hit:Cold"; "Hit:Lightning"; "Hit:Chaos"; "Hit:Physical" ]
                      Buff = sprintf "Charm%d" (whole 1 2)
                      Prevents = chance 0.6
                      Duration = between 0.5 4.0
                      ChargesPerUse = pick [ 20.0; 40.0; 0.0 ]
                      MaxCharges = 80.0
                      ChargesPerSecond = (if chance 0.5 then between 0.5 8.0 else 0.0)
                      ChargeOnKillChance = between 0.0 60.0
                      ChargeGainMult = between 0.8 1.4
                      LifeOnUse = (if chance 0.3 then 50.0 else 0.0)
                      ManaOnUse = (if chance 0.3 then 30.0 else 0.0)
                      StartsRecharge = chance 0.2 } ]
            Periodics =
                [ for i in 1 .. (if chance 0.35 then whole 1 2 else 0) ->
                    { Name = sprintf "periodic %d" i
                      Every = between 0.4 3.0
                      Action =
                        (match whole 0 6 with
                         | 0 -> GainRandom [ "PowerCharge"; "FrenzyCharge"; "Mote" ]
                         | 1 -> GainAnyRandom [ "PowerCharge"; "Mote" ]
                         | 2 -> GainWeighted [ "PowerCharge", 2.0; "FrenzyCharge", 1.0; "EnduranceCharge", 0.5 ]
                         | 3 -> ConsumeEach [ "PowerCharge"; "FrenzyCharge" ]
                         | 4 -> GainTimedBuff("Flames", between 0.3 1.0, between 1.0 5.0, whole 1 4, [ Mod.inc (Damage(OfType Chaos)) 10.0 ])
                         | 5 -> HealLife(between 10.0 200.0)
                         | _ -> TriggerNamed(if triggered.IsEmpty then names.Head else "Echo")) } ]
            ManaFlask =
                (if chance 0.4 then Some { Amount = between 50.0 300.0; Duration = between 1.0 4.0; ChargesPerUse = 20.0; MaxCharges = 60.0; ChargesPerSecond = between 0.0 3.0; ChargeGainMult = 1.0; Instant = (if chance 0.3 then 0.5 else 0.0) }
                 else None) }
    // an overkill explosion: its skill is one of the player's, never pressed
    let player =
        if chance 0.8 then player
        else
            let burst = { Skill.make (pick [ "Ash: Burst"; "Burst" ]) [ Spell; Area ] with Delivery = (if chance 0.8 then AreaAtTarget(between 15.0 60.0) else Single); Duration = between 1.0 5.0; Hits = [] }
            { player with
                Skills = player.Skills @ [ burst ]
                OverkillBurns = [ { Skill = burst.Name; Filter = pick [ AnySkill; WithTag Attack; WithTag Spell ]; ThresholdPercent = pick [ 0.0; 5.0; 30.0 ]; PercentPerSecond = between 20.0 90.0 } ] }
    // infusions: remnants from ailments, a skill whose impacts consume one and are its infused
    // copy, a part that needs one; Runic Ward and mana remnants
    let player =
        if chance 0.75 then player
        else
            let infusions = [ "FireInfusion"; "ColdInfusion"; "LightningInfusion" ]
            let copy = { skill 8 true with Name = names.Head + " infused" }
            let needy = { skill 0 true with Name = names.Head + ": Infused"; Mods = [ Mod.flag (Named("OnlyInfused:" + pick infusions)) ] }
            let withPart = chance 0.4
            { player with
                Skills = (player.Skills |> List.mapi (fun i s -> if i = 0 && withPart then { s with Parts = s.Parts @ [ needy ] } else s)) @ [ copy ]
                Mods = [ if chance 0.4 then yield Mod.base' (Named "InfusionKeepChance") (between 10.0 80.0)
                         if chance 0.3 then yield Mod.base' (Named "PowerChargeOnInfusionConsumed") 1.0 ] @ player.Mods
                Resources = player.Resources @ [ for r in infusions do if chance 0.4 then yield r, whole 1 3 ] @ [ if chance 0.2 then yield "VerisiumInfusion", whole 1 2 ]
                MaxResources = infusions |> List.fold (fun m r -> Map.add r 3 m) player.MaxResources
                Infusions =
                    Some
                        { Remnants = [ for a, r in [ Ignite, "FireInfusion"; Freeze, "ColdInfusion"; Shock, "LightningInfusion"; Chill, "ColdInfusion" ] do if chance 0.7 then yield a, r, pick [ 100.0; between 10.0 90.0 ] ]
                          ExtraRemnantChance = pick [ 0.0; between 10.0 60.0 ]
                          Delay = between 0.05 1.0
                          Consumers = (if chance 0.7 then Map.ofList [ names.Head, (pick infusions, copy.Name) ] else Map.empty)
                          Wildcard = (if chance 0.5 then Some "VerisiumInfusion" else None) } }
    let player =
        if player.Vitals.IsNone || chance 0.75 then player
        else
            { player with
                WardRemnants = (if chance 0.6 then Some { OnKillChance = between 0.0 100.0; OnStunChance = between 20.0 100.0; StunCooldown = between 0.0 3.0; WardPerRemnant = between 5.0 80.0; ExtraChance = between 0.0 50.0; Delay = between 0.1 1.5; Overflow = pick [ 1.0; 2.0 ] } else None)
                ManaRemnants = (if chance 0.6 then Some { OnKillChance = between 0.0 100.0; CritCooldown = between 0.0 3.0; ManaPerRemnant = between 5.0 60.0; PercentOfMaxMana = pick [ 0.0; 5.0 ]; ExtraChance = between 0.0 50.0; Delay = between 0.1 1.5; Overflow = pick [ 1.0; 2.0 ] } else None) }
    let player = if player.Archons.IsEmpty then player else { player with Mods = Mod.base' (Named "ArchonOnIgnite") (between 20.0 100.0) :: player.Mods }
    // what it fights
    let enemies =
        [ for i in 1 .. (if chance 0.5 then 1 else whole 2 5) do
            let rarity = pick [ Normal; Magic; Rare; Unique ]
            let d = EnemyDef.make Rarity.defaults (whole 60 83) rarity (sprintf "%s %d" (Rarity.name rarity) i) (Vec.make (between 5.0 60.0) (between -30.0 30.0))
            yield
                { d with
                    Life = (if chance 0.35 then 1e12 elif chance 0.4 then between 50.0 2500.0 else between 300.0 60000.0)
                    Moving = chance 0.2
                    Mods = (if chance 0.3 then [ Mod.base' Armour (between 0.0 3000.0) ] else d.Mods @ (if chance 0.3 then [ Mod.inc Evasion (between 50.0 300.0) ] else []))
                    Speed = between 5.0 60.0
                    Attack =
                        (if chance 0.35 then None
                         elif chance 0.5 then d.Attack
                         else
                            Some
                                { Interval = between 0.3 2.0
                                  Damage = [ for _ in 1 .. whole 1 3 -> pick types, between 20.0 (if chance 0.2 then 3000.0 else 400.0) ] |> Map.ofList
                                  Accuracy = between 100.0 5000.0
                                  Range = between 5.0 40.0
                                  CritChance = between 0.0 40.0
                                  Melee = chance 0.5 }) } ]
    let fight =
        { Fight.single enemies.Head player (between 3.0 12.0) with
            Enemies = enemies
            Targeting = pick [ Strongest; Nearest ]
            Reach = (if chance 0.3 then between 20.0 50.0 else infinity)
            BankedInvocations = pick [ 1.0; 0.0; 0.5 ]
            EnemyAuras =
                (if chance 0.2 then [ pick [ "Curse:Aura"; "Withered"; "Mark:Aura" ], whole 1 4, [ Mod.inc (DamageTaken AllDamage) 5.0; Mod.base' (Resist Chaos) -3.0 ] ] else []) }
    // minions and totems beside the player, in about a third of the fights: some that can be
    // hurt and come back, some that cannot, totems that act under a buff of the player's, and
    // a pool of temporary ones with what summons them
    let fight =
        if not (chance 0.35) then fight
        else
            note "minions"
            let minionSkill (name: string) (spam: bool) = { skill 0 spam with Name = name }
            let minionMods () = [ if chance 0.6 then yield Mod.base' (Named "Accuracy") 1e9 ] @ [ for _ in 1 .. whole 0 3 do yield! (pick modPool) () ]
            let minionRules (own: string) : Rule list =
                [ for _ in 1 .. (if chance 0.4 then whole 1 2 else 0) ->
                    { Hook = pick [ OnHit; OnHit; OnUse; OnCrit; OnKill ]
                      Filter = pick [ AnySkill; Called own; WithTag Attack ]
                      Chance = pick [ 100.0; between 10.0 90.0 ]
                      Reaction =
                        (match whole 0 6 with
                         | 0 | 1 -> ApplyDebuff(pick [ "Marked by pain"; "Gnawed" ], between 0.5 6.0, whole 1 4, [ Mod.inc (DamageTaken AllDamage) (between 2.0 15.0) ])
                         | 2 -> GrantBuff(pick [ "Buffed"; "Frenzied" ], between 0.5 5.0, whole 1 4, buffMods ())
                         | 3 -> ApplyExposure(pick types, between 5.0 40.0, between 1.0 8.0)
                         | 4 -> GainResource(pick [ "PowerCharge"; "FrenzyCharge" ], 1)
                         | 5 -> ApplyCurse(pick [ "Hex"; "Mark:Prey" ], between 1.0 6.0, [ Mod.base' (Resist(pick types)) (between -30.0 -5.0) ])
                         | _ -> GrantMinionBuff("Pack howl", between 0.5 4.0, [ Mod.inc Speed (between 5.0 30.0) ])) } ]
            let body () : MinionBody =
                { Life = (if chance 0.6 then between 40.0 600.0 else between 600.0 20000.0)
                  EnergyShield = (if chance 0.3 then between 20.0 300.0 else 0.0)
                  Armour = (if chance 0.4 then between 100.0 3000.0 else 0.0)
                  Evasion = (if chance 0.4 then between 100.0 3000.0 else 0.0)
                  Resist = [ for t in types do if chance 0.3 then yield t, between -20.0 75.0 ] |> Map.ofList
                  Melee = chance 0.6
                  Size = pick [ 1.0; 2.0; 6.0 ]
                  Reach = pick [ 8.0; 12.0; 16.0 ]
                  Revive = (if chance 0.7 then Some(between 1.5 7.0) else None)
                  Power = between 0.3 2.0
                  LifeRegen = (if chance 0.3 then between 1.0 60.0 else 0.0)
                  TakesForOwner = (if chance 0.25 then between 10.0 70.0 else 0.0)
                  Explodes = None }
            let persistent =
                [ for i in 1 .. whole 0 4 do
                    let name = sprintf "Minion %d" i
                    let own = minionSkill (sprintf "Bite %d" (whole 1 2)) true
                    let extra = if chance 0.3 then [ minionSkill "Howl" false ] else []
                    let blast = { minionSkill "Instability" true with Delivery = AreaAroundSelf(between 15.0 40.0) }
                    let b = if chance 0.75 then Some(body ()) else None
                    let explodes = b.IsSome && chance 0.25
                    yield
                        { Actor.make name (minionMods ()) (extra @ [ own ] @ (if explodes then [ blast ] else [])) with
                            Rotation = [ for s in extra @ [ own ] -> Actor.always s.Name ]
                            Rules = minionRules own.Name
                            Body = b |> Option.map (fun b -> if explodes then { b with Explodes = Some blast.Name } else b)
                            Resources = [ if chance 0.3 then yield "PowerCharge", whole 1 3 ]
                            MaxResources = Map.ofList [ "PowerCharge", 3; "FrenzyCharge", 2 ]
                            ResourceDurations = (if chance 0.3 then Map.ofList [ "PowerCharge", between 2.0 8.0 ] else Map.empty)
                            Cycles = (if chance 0.1 then [ { Name = "Mood"; Period = between 1.0 4.0; Variants = [ [ Mod.more (Damage AllDamage) 20.0 ]; [ Mod.inc Speed 20.0 ]; [] ] } ] else [])
                            Periodics = (if chance 0.1 then [ { Name = "stir"; Every = between 0.5 3.0; Action = GainRandom [ "PowerCharge"; "FrenzyCharge" ] } ] else []) } ]
            let totems =
                [ for i in 1 .. (if chance 0.4 then whole 1 2 else 0) ->
                    let own = minionSkill "Totem shot" true
                    { Actor.make (sprintf "Totem %d" i) (minionMods ()) [ own ] with ActiveWhile = Some(pick [ "Totem buff"; "Spirit A"; "Spirit B"; "Buffed" ]) } ]
            let pack =
                [ for i in 1 .. (if chance 0.5 then whole 1 5 else 0) ->
                    let own = minionSkill "Snap" true
                    { Actor.make (sprintf "Whelp %d" i) (minionMods ()) [ own ] with Pool = Some "Pack"; Body = (if chance 0.5 then Some { body () with Revive = None } else None); Rules = minionRules own.Name } ]
            let first = fight.Player.Skills.Head.Name
            let summons () : Reaction =
                match whole 0 7 with
                | 0 -> Summon("Pack", whole 1 3, pick [ between 0.5 6.0; infinity ])
                | 1 -> SummonEmpowered("Pack", whole 1 3, between 0.5 6.0, [ Mod.more (Damage AllDamage) (between 20.0 100.0) ])
                | 2 -> WithCorpse(Summon("Pack", 1, between 1.0 6.0), pick [ NoReaction; SpendStack("PowerCharge", Summon("Pack", 2, between 1.0 6.0)); GrantBuff("Buffed", 2.0, 2, []) ])
                | 3 -> SummonPerCorpse("Pack", between 1.0 6.0, chance 0.6)
                | 4 -> SummonPerWard("Pack", between 10.0 60.0, between 1.0 6.0)
                | 5 -> GrantOneOf(pick [ [ "Spirit A"; "Spirit B" ]; [ "Totem buff" ]; [] ], between 1.0 6.0)
                | 6 -> GrantBuff("Totem buff", between 1.0 6.0, 1, [])
                | _ -> GrantMinionBuff(pick [ "Offering"; "Buffed" ], between 0.5 5.0, buffMods ())
            let more : Rule list =
                [ for _ in 1 .. whole 1 4 ->
                    { Hook = pick [ OnUse; OnUse; OnHit; OnKill ]; Filter = pick [ AnySkill; Called first ]; Chance = pick [ 100.0; 100.0; between 10.0 90.0 ]; Reaction = summons () } ]
            let corpseSkill = chance 0.3
            let onDeath = if not triggered.IsEmpty && fight.Player.Metas.IsEmpty && chance 0.5 then [ { Meta.simple "On Death" (between 20.0 80.0) "Echo" with Sources = [ MinionDeaths(between 20.0 60.0) ] } ] else []
            let watchful = chance 0.3
            let player' =
                { fight.Player with
                    Rules = fight.Player.Rules @ more
                    Metas = fight.Player.Metas @ onDeath
                    Skills =
                        fight.Player.Skills |> List.mapi (fun i s ->
                            if i = 0 && corpseSkill then { s with Tags = s.Tags |> Set.add (Tag "Type:TargetsDestructibleCorpses") |> (if chance 0.3 then Set.add (Tag "Type:SkillConsumesPowerChargesOnUse") else id) } else s)
                    Rotation =
                        fight.Player.Rotation |> List.mapi (fun i step ->
                            if i = 0 && watchful then { step with When = fun v -> step.When v && ((Map.tryFind "Out:Pack" v.Buffs |> Option.defaultValue 0) < 2 || v.Buffs.ContainsKey "Corpses") } else step)
                    Periodics =
                        fight.Player.Periodics
                        @ [ if chance 0.3 then
                                yield { Name = "pack"; Every = between 0.5 3.0; Action = (if chance 0.5 then SummonMinions("Pack", whole 1 2, between 0.5 4.0) else SummonFromCorpse("Pack", whole 1 2, between 0.5 4.0)) } ] }
            // (monsters that hit, so that minions die)
            let enemies' =
                fight.Enemies |> List.map (fun e ->
                    if e.Attack.IsSome || chance 0.3 then e
                    else
                        { e with
                            Attack =
                                Some
                                    { Interval = between 0.3 1.5
                                      Damage = [ for _ in 1 .. whole 1 2 -> pick types, between 20.0 400.0 ] |> Map.ofList
                                      Accuracy = between 100.0 5000.0
                                      Range = between 5.0 40.0
                                      CritChance = between 0.0 40.0
                                      Melee = chance 0.5 } })
            { fight with Player = player'; Enemies = enemies'; Minions = persistent @ totems @ pack }
    // now and then something the fast engine does not play: it must say so
    let fight =
        match (if chance 0.12 then pick [ 1; 3 ] else -1) with
        | 1 ->
            note "pustule"
            { fight with Player = { fight.Player with Rules = { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = AttachPustule(fight.Player.Skills.Head.Name, 3.0, 0.0, 0.0) } :: fight.Player.Rules } }
        | 3 ->
            note "seals"
            { fight with Player = { fight.Player with Skills = fight.Player.Skills |> List.map (fun s -> { s with Mods = Mod.base' (Named "MaxSeals") 2.0 :: Mod.base' (Named "SealIntervalMs") 900.0 :: Mod.base' (Named "SealRepeats") 1.0 :: s.Mods }) } }
        | _ -> fight
    // a pack that never ends: the dead are replaced (several enemies, most of them weak enough to die)
    let fight =
        if not (chance 0.2) then fight
        else
            note "replaced"
            let extra =
                [ for i in 1 .. whole 0 3 ->
                    let d = EnemyDef.make Rarity.defaults (whole 60 83) (pick [ Normal; Magic ]) (sprintf "Pack %d" i) (Vec.make (between 5.0 50.0) (between -30.0 30.0))
                    { d with Life = between 40.0 1500.0; Speed = between 5.0 60.0; Attack = (if chance 0.5 then d.Attack else None) } ]
            { fight with
                Replace = true
                Enemies = (fight.Enemies |> List.map (fun e -> if chance 0.6 then { e with Life = between 40.0 3000.0 } else e)) @ extra }
    let described =
        sprintf "case %d: %d skills (%s), %d rules (%s), %d mods, %d enemies, vitals %b, %.2f s%s"
            case fight.Player.Skills.Length
            (fight.Player.Skills |> List.map (fun s -> sprintf "%s%s%s" s.Name (if s.Cooldown.IsSome then " cd" else "") (if s.Parts.IsEmpty then "" else sprintf " +%d" s.Parts.Length)) |> String.concat ", ")
            fight.Player.Rules.Length
            (fight.Player.Rules |> List.map (fun r -> match r.Reaction with GrantBuff _ -> "buff" | ApplyDebuff _ -> "debuff" | ApplyCurse _ -> "curse" | TriggerSkill _ -> "trigger" | GainResource _ -> "resource" | _ -> "other") |> String.concat " ")
            fight.Player.Mods.Length fight.Enemies.Length fight.Player.Vitals.IsSome fight.MaxTime
            ((if fight.Minions.IsEmpty then "" else sprintf ", %d minions" fight.Minions.Length) + (if notes.Count = 0 then "" else " [" + String.concat ", " notes + "]"))
    fight, described

let run (test: string -> (unit -> unit) -> unit) =
    let noIgnite = Mod.more (AilmentChance Ignite) -100.0
    let boss life = { EnemyDef.atLevel 83 life with Mods = [] }
    let spell name t lo hi = { Skill.make name [ Spell ] with BaseDamage = Map.ofList [ t, { Min = lo; Max = hi } ]; BaseCrit = 9.0 }
    let strike name lo hi = { Skill.make name [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, { Min = lo; Max = hi } ]; BaseCrit = 7.0; BaseTime = 0.7 }
    /// The same outcome from both engines, for these seeds; the fight must compile.
    let same (fight: Fight) (seeds: uint32 list) =
        match Flat.compile fight with
        | Error why -> failwithf "does not compile: %s" why
        | Ok program ->
            for seed in seeds do
                match check fight seed with
                | Some (Error d) -> failwith d
                | _ -> ()
            // and what the two tell of the fight as it goes: every event and every hit
            match seeds with
            | seed :: _ ->
                let parted, where = divergence fight program seed
                if parted || not (where.EndsWith "told the same") then failwithf "seed %d: %s" seed where
            | [] -> ()
    let seeds = [ 1u .. 25u ]

    test "flat: a spell with critical hits against a boss that does not die" (fun () ->
        let player = Actor.make "Player" [ Mod.inc CritChance 150.0; Mod.base' CritBonus 60.0; Mod.inc (Damage AllDamage) 40.0; Mod.inc Speed 25.0 ] [ spell "Arc Bolt" Lightning 20.0 380.0 ]
        same (Fight.single (boss 1e12) player 20.0) seeds)

    test "flat: a fight that ends when the enemy dies, with ignite, shock and chill on the way" (fun () ->
        let player =
            Actor.make "Player" [ Mod.inc CritChance 200.0; Mod.inc (AilmentMagnitude None) 50.0; Mod.inc (Damage AllDamage) 30.0 |> Mod.whenever (EnemyHas "Shocked") ]
                [ spell "Fire Bolt" Fire 200.0 900.0; { spell "Arc" Lightning 50.0 1500.0 with Cooldown = Some 2.0 }; { spell "Frost" Cold 400.0 700.0 with Cooldown = Some 3.0; StoredUses = 2 } ]
        let player = { player with Rotation = [ Actor.always "Arc"; Actor.always "Frost"; Actor.always "Fire Bolt" ] }
        let enemy = EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 30.0 0.0)
        same (Fight.single { enemy with Life = 150000.0 } player 60.0) seeds)

    test "flat: attacks that can miss, bleed, poison stacks, armour break and stun buildup" (fun () ->
        let player =
            Actor.make "Player"
                [ Mod.base' (Named "Accuracy") 900.0; Mod.base' (AilmentChance Bleed) 40.0; Mod.base' (AilmentChance Poison) 60.0; Mod.base' (AilmentStacks Poison) 3.0
                  Mod.base' (Named "ArmourBreakOfPhysical") 150.0; Mod.inc (Buildup HeavyStun) 3000.0; Mod.base' (AilmentChance Maim) 30.0; noIgnite ]
                [ strike "Jab" 80.0 160.0; { strike "Smash" 300.0 500.0 with Cooldown = Some 2.5; Tags = Set.ofList [ Attack; Melee; Slam ]; Delivery = AreaAtTarget 20.0 } ]
        let player = { player with Rotation = [ Actor.always "Smash"; Actor.always "Jab" ] }
        let enemy = { EnemyDef.make Rarity.defaults 80 Rare "Brute" (Vec.make 12.0 0.0) with Life = 1e12; Moving = true }
        same (Fight.single enemy player 25.0) seeds)

    test "flat: mana, costs, a flask and leech; a skill that waits for mana" (fun () ->
        let vitals = { PlayerStats.basic 1000.0 120.0 with ManaRegen = 8.0; EnergyShield = 200.0; EnergyShieldRecharge = 40.0; LifeRegen = 5.0; StartLife = 0.6 }
        let flask = { Amount = 90.0; Duration = 3.0; ChargesPerUse = 20.0; MaxCharges = 60.0; ChargesPerSecond = 1.0; ChargeGainMult = 1.0; Instant = 0.3 }
        let player =
            { Actor.make "Player" [ Mod.base' ManaLeech 6.0; Mod.base' (Named "LifeLeech") 4.0; Mod.inc ManaCost 20.0; Mod.inc (Damage AllDamage) 25.0 |> Mod.whenever (WhileBuff "LowMana"); Mod.base' (Named "Accuracy") 5000.0 ]
                  [ { spell "Costly" Fire 100.0 200.0 with Cost = 45.0; BaseTime = 0.6 }; { strike "Drain" 100.0 200.0 with Cost = 5.0 } ] with
                Vitals = Some vitals
                ManaFlask = Some flask }
        same (Fight.single { boss 1e12 with Mods = [ Mod.base' Armour 500.0 ] } player 30.0) seeds
        // Blood Magic: the cost is paid with life
        same (Fight.single (boss 1e12) { player with Vitals = Some { vitals with CostsLife = true; StartLife = 1.0 } } 20.0) seeds)

    test "flat: buffs and debuffs from rules, with stacks and expiry; a curse, exposure, a triggered skill" (fun () ->
        let echo = { spell "Echo" Cold 300.0 400.0 with Tags = Set.ofList [ Spell; Triggered ]; Cooldown = Some 0.4 }
        let player =
            { Actor.make "Player"
                  [ Mod.inc (Damage AllDamage) 12.0 |> Mod.whenever (PerStack("Momentum", 1.0)); Mod.inc CritChance 300.0; Mod.more (Damage AllDamage) 20.0 |> Mod.whenever (EnemyHas "Cursed") ]
                  [ { spell "Hex" Chaos 1.0 2.0 with Cooldown = Some 5.0; Hits = []; BaseTime = 0.5 }; spell "Bolt" Fire 100.0 300.0; echo ] with
                Rotation = [ { Use = "Hex"; When = fun v -> not (v.EnemyStatus.ContainsKey "Curse:Hex") }; Actor.always "Bolt" ]
                Rules =
                    [ { Hook = OnHit; Filter = Called "Bolt"; Chance = 100.0; Reaction = GrantBuff("Momentum", 2.5, 5, []) }
                      { Hook = OnUse; Filter = Called "Hex"; Chance = 100.0; Reaction = ApplyCurse("Hex", 4.0, [ Mod.base' (Resist Fire) -25.0 ]) }
                      { Hook = OnHit; Filter = AnySkill; Chance = 50.0; Reaction = ApplyDebuff("Sundered", 3.0, 4, [ Mod.inc (DamageTaken AllDamage) 4.0 ]) }
                      { Hook = OnCrit; Filter = Called "Bolt"; Chance = 100.0; Reaction = TriggerSkill "Echo" }
                      { Hook = OnInflict Ignite; Filter = AnySkill; Chance = 100.0; Reaction = ApplyExposure(Fire, 20.0, 4.0) }
                      { Hook = OnUse; Filter = Called "Hex"; Chance = 100.0; Reaction = GrowExposure(Cold, 10.0, 5.0, 40.0, 0.25, 6, 5.0) } ] }
        same (Fight.single (EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 30.0 0.0) |> fun e -> { e with Life = 1e12; Attack = None }) player 30.0) seeds)

    test "flat: charges gained and consumed, parts of a skill, damage over time of a skill" (fun () ->
        let burst = { spell "Thunder: Burst" Lightning 500.0 900.0 with Mods = [ Mod.flag (Named "OnlyWithCharge") ]; Delivery = AreaAtTarget 25.0 }
        let thunder = { strike "Thunder" 200.0 300.0 with Consumes = Some "PowerCharge"; Parts = [ burst ]; Cooldown = Some 1.5 }
        let rot = { spell "Rot" Chaos 10.0 20.0 with Dots = Map.ofList [ Chaos, 400.0 ]; Duration = 3.0; Cooldown = Some 4.0 }
        let player =
            { Actor.make "Player" [ Mod.inc CritChance 400.0; Mod.more (Damage AllDamage) 15.0 |> Mod.whenever (PerStack("PowerCharge", 1.0)); Mod.base' (Named "ChargeSkipConsume") 30.0 ]
                  [ thunder; rot; strike "Jab" 50.0 90.0 ] with
                Rotation = [ { Use = "Thunder"; When = fun v -> v.Buffs.ContainsKey "PowerCharge" }; Actor.always "Rot"; Actor.always "Jab" ]
                Rules = [ { Hook = OnCrit; Filter = Called "Jab"; Chance = 100.0; Reaction = GainResource("PowerCharge", 1) } ]
                Resources = [ "PowerCharge", 2 ]
                MaxResources = Map.ofList [ "PowerCharge", 3 ]
                ResourceDurations = Map.ofList [ "PowerCharge", 6.0 ]
                KeptChargesPause = 1.0 }
        same (Fight.single (boss 1e12) player 30.0) seeds)

    test "flat: several enemies, areas and projectiles, kills on the way" (fun () ->
        let pack = Encounter.pack 80 Rarity.defaults [ Normal, 6; Magic, 2; Rare, 1 ] 35.0 25.0 false 7u
        let shot = { spell "Shards" Cold 300.0 600.0 with Delivery = Projectiles { ProjectileShape.make 5 with Pierce = 1; Chains = 1 }; BaseTime = 0.5 }
        let nova = { spell "Nova" Fire 500.0 800.0 with Delivery = AreaAroundSelf 45.0; Cooldown = Some 2.0 }
        let player =
            { Actor.make "Player" [ Mod.inc AreaOfEffect 30.0; Mod.flag (Named "CullingStrike"); Mod.inc (Damage AllDamage) 40.0 |> Mod.whenever (WhileBuff "KilledRecently") ] [ nova; shot ] with
                Rules = [ { Hook = OnKill; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("Frenzied", 4.0, 3, [ Mod.inc Speed 10.0 ]) } ] }
        same { Fight.single pack.Head player 30.0 with Enemies = pack } seeds)

    test "flat: monsters walk up and hit back; block, ailments on the player, a charm, death" (fun () ->
        let attack t dmg = { Interval = 0.8; Damage = Map.ofList [ t, dmg; Physical, dmg / 2.0 ]; Accuracy = 800.0; Range = 6.0; CritChance = 15.0; Melee = true }
        let brutes =
            [ for i, t in [ 1, Fire; 2, Cold; 3, Lightning ] ->
                { EnemyDef.make Rarity.defaults 70 Magic (sprintf "brute %d" i) (Vec.make (20.0 + 10.0 * float i) (float i * 4.0)) with Life = 1e9; Attack = Some(attack t 60.0); Speed = 20.0 } ]
        let vitals =
            { PlayerStats.basic 1500.0 200.0 with
                ManaRegen = 10.0; EnergyShield = 300.0; EnergyShieldRecharge = 60.0; Evasion = 1500.0; Armour = 2000.0; Block = 30.0; BlockedDamageTaken = 20.0
                AilmentThreshold = 200.0; FreezeThreshold = 150.0; StunThreshold = 400.0; LifeRegen = 20.0
                Recoup = [ "Life", [ Fire; Cold ], 20.0 ]
                LifeFlask = Some { Amount = 400.0; Duration = 3.0; ChargesPerUse = 20.0; MaxCharges = 60.0; ChargesPerSecond = 0.0; ChargeGainMult = 1.0; Instant = 0.25 } }
        let charm = { Name = "thaw"; Trigger = "Frozen"; Buff = "Charm1"; Prevents = true; Duration = 3.0; ChargesPerUse = 40.0; MaxCharges = 80.0; ChargesPerSecond = 2.0; ChargeOnKillChance = 0.0; ChargeGainMult = 1.0; LifeOnUse = 30.0; ManaOnUse = 0.0; StartsRecharge = true }
        let player =
            { Actor.make "Player" [ Mod.inc (Damage AllDamage) 30.0 |> Mod.whenever (WhileBuff "BeenHitRecently") ] [ { spell "Nova" Fire 50.0 90.0 with Delivery = AreaAroundSelf 30.0; Cost = 12.0 } ] with
                Vitals = Some vitals
                Charms = [ charm ]
                Rules = [ { Hook = WhenHit; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("Riposte", 2.0, 3, [ Mod.more (Damage AllDamage) 10.0 ]) }
                          { Hook = OnBlock; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("EnduranceCharge", 1) } ]
                MaxResources = Map.ofList [ "EnduranceCharge", 3; "Rage", 30 ]
                Mods = [ Mod.base' (Named "RageWhenHit") 4.0; Mod.more (Damage AllDamage) 1.0 |> Mod.whenever (PerStack("Rage", 1.0)) ] }
        same { Fight.single brutes.Head player 25.0 with Enemies = brutes } seeds
        // harder hits: the player dies
        let killers = brutes |> List.map (fun b -> { b with Attack = Some(attack Physical 500.0) })
        same { Fight.single killers.Head player 25.0 with Enemies = killers } seeds
        match Sim.run { Fight.single killers.Head player 25.0 with Enemies = killers } 1u with
        | { Died = Some _ } -> ()
        | _ -> failwith "expected the player to die")

    test "flat: a meta gem gains energy from critical hits and triggers its spell at the enemy that was hit" (fun () ->
        let flare = { spell "Flare" Fire 900.0 1300.0 with Tags = Set.ofList [ Spell; Area; Triggered ]; Delivery = AreaAtTarget 15.0 }
        let meta = { Meta.simple "Cast on Crit" 100.0 "Flare" with Sources = [ CritHits 300.0; InflictsAilment(Shock, 200.0) ]; EnergyMult = 1.3; RefundChance = 25.0 }
        let invocation = { Meta.simple "Invocation" 400.0 "Flare" with Release = Invocation 2.0; Cost = 100.0; Sources = [ CastsSpells 0.6 ] }
        let player =
            { Actor.make "Player" [ Mod.inc CritChance 250.0; Mod.inc (Damage AllDamage) 60.0 ] [ { spell "Arc Bolt" Lightning 20.0 380.0 with Delivery = Projectiles(ProjectileShape.make 3); BaseTime = 0.6 }; flare ] with
                Rotation = [ { Use = "Arc Bolt"; When = fun v -> (Map.tryFind "Invocation" v.Energy |> Option.defaultValue 0.0) >= 0.0 } ]
                Metas = [ meta; invocation ] }
        let pack = Encounter.pack 80 Rarity.defaults [ Magic, 3; Rare, 2 ] 30.0 15.0 false 3u |> List.map (fun e -> { e with Life = e.Life * 30.0 })
        same { Fight.single pack.Head player 20.0 with Enemies = pack; BankedInvocations = 0.5 } seeds)

    test "flat: an overkill explosion ignites the enemies near the one killed; a buff scaled by the charges held" (fun () ->
        let pack = Encounter.pack 80 Rarity.defaults [ Normal, 8; Magic, 3 ] 30.0 20.0 false 5u |> List.map (fun e -> { e with Life = e.Life * 0.3 })
        let burst = { Skill.make "Ash: Burst" [ Spell; Area ] with Delivery = AreaAtTarget 40.0; Duration = 4.0; Hits = [] }
        let staff = { Skill.make "Charged Staff" [ Spell ] with Hits = []; BaseTime = 0.5; Cooldown = Some 6.0 }
        let player =
            { Actor.make "Player" [ Mod.inc AreaOfEffect 40.0; Mod.inc SkillDuration 25.0; Mod.inc CritChance 300.0 ]
                  [ staff; { strike "Swing" 4000.0 9000.0 with Delivery = AreaAtTarget 12.0 }; burst ] with
                Rotation = [ { Use = "Charged Staff"; When = fun v -> not (v.Buffs.ContainsKey "Charged") }; Actor.always "Swing" ]
                OverkillBurns = [ { Skill = "Ash: Burst"; Filter = WithTag Attack; ThresholdPercent = 5.0; PercentPerSecond = 60.0 } ]
                Rules =
                    [ { Hook = OnUse; Filter = Called "Charged Staff"; Chance = 100.0; Reaction = ChargeBuff("Charged", "PowerCharge", [ Mod.more (Damage(OfType Lightning)) 8.0; Mod.base' (AddedMax Lightning) 50.0 ], 3.0, 40.0) }
                      { Hook = OnCrit; Filter = AnySkill; Chance = 100.0; Reaction = GainResource("PowerCharge", 1) } ]
                Resources = [ "PowerCharge", 2 ]
                MaxResources = Map.ofList [ "PowerCharge", 4 ] }
        let fight = { Fight.single pack.Head player 25.0 with Enemies = pack }
        same fight seeds
        same { fight with BankedInvocations = 0.0 } seeds
        let o = Sim.run fight 1u
        if not (o.Counts |> Map.exists (fun k _ -> k.StartsWith "overkill ignite:")) then failwith "expected overkill ignites"
        // (with nothing prepared the buff is not up at the start: the staff is used for it)
        let unprepared = Sim.run { fight with BankedInvocations = 0.0; Enemies = pack |> List.map (fun e -> { e with Life = 1e9 }) } 1u
        if not (unprepared.Counts.ContainsKey "buff:Charged") then failwith "expected the charge buff")

    // ---- minions and totems ----
    let bite name dmg = { Skill.make name [ Attack; Melee ] with BaseDamage = Map.ofList [ Physical, { Min = dmg * 0.8; Max = dmg * 1.2 } ]; BaseCrit = 6.0 }
    let sure = Mod.base' (Named "Accuracy") 1e12
    let idle = { Skill.make "Wait" [ Spell ] with Hits = [] }
    let body life revive =
        { Life = life; EnergyShield = 0.0; Armour = 0.0; Evasion = 0.0; Resist = Map.empty; Melee = true; Size = 1.0; Reach = 12.0; Revive = revive; Power = 0.55; LifeRegen = 0.0; TakesForOwner = 0.0; Explodes = None }
    let brute t dmg interval =
        { EnemyDef.atLevel 83 1e9 with
            Name = "brute"; Position = Vec.make 8.0 0.0; Size = 4.0; Mods = []
            Attack = Some { Interval = interval; Damage = Map.ofList [ t, dmg ]; Accuracy = 1500.0; Range = 5.0; CritChance = 10.0; Melee = true } }
    let minionSeeds = [ 1u .. 12u ]

    test "flat: a minion that cannot be hurt fights beside the player; an offering buffs the minions" (fun () ->
        let offering = { Skill.make "Offering" [ Spell ] with Hits = []; Cooldown = Some 4.0 }
        let player =
            { Actor.make "Player" [ noIgnite; Mod.inc CritChance 100.0 ] [ offering; spell "Bolt" Fire 40.0 90.0 ] with
                Rules = [ { Hook = OnUse; Filter = Called "Offering"; Chance = 100.0; Reaction = GrantMinionBuff("Offering", 3.0, [ Mod.inc (Damage AllDamage) 50.0; Mod.more Speed 20.0 ]) } ] }
        let hound =
            { Actor.make "Hound" [ sure; Mod.base' (AilmentChance Bleed) 40.0; Mod.inc (Damage AllDamage) 20.0 |> Mod.whenever (WhileBuff "HitRecently") ] [ bite "Bite" 100.0; { bite "Howl" 30.0 with Cooldown = Some 2.5 } ] with
                Rotation = [ Actor.always "Howl"; Actor.always "Bite" ]
                Rules = [ { Hook = OnHit; Filter = Called "Howl"; Chance = 100.0; Reaction = ApplyDebuff("Marked", 3.0, 2, [ Mod.inc (DamageTaken AllDamage) 10.0 ]) } ] }
        let crow = Actor.make "Crow" [ sure ] [ { spell "Peck" Cold 20.0 60.0 with BaseTime = 0.45 } ]
        let fight = { Fight.single (boss 1e12) player 12.0 with Minions = [ hound; crow ] }
        same fight minionSeeds
        let o = Sim.run fight 1u
        if not (o.DamageBy.ContainsKey "Bite (hit)" && o.DamageBy.ContainsKey "Peck (hit)" && o.DamageBy.ContainsKey "Bolt (hit)") then failwith "expected every actor to hit"
        // and a fight that ends when the enemies die
        same { fight with Enemies = [ boss 3000.0; { boss 5000.0 with Name = "other"; Position = Vec.make 25.0 10.0 } ] } minionSeeds)

    test "flat: poisons of an actor that lets several count beside those of one that lets one count" (fun () ->
        // (the weaker poisons of the one are kept: they count once the other's hit lets more stack)
        let venom = Mod.base' (AilmentChance Poison) 100.0
        let player = Actor.make "Player" [ noIgnite; venom ] [ { spell "Dart" Chaos 20.0 200.0 with BaseTime = 0.35 } ]
        let spider = Actor.make "Spider" [ sure; venom; Mod.base' (AilmentStacks Poison) 3.0 ] [ { bite "Fang" 60.0 with BaseTime = 0.8 } ]
        same { Fight.single (boss 1e12) player 12.0 with Minions = [ spider ] } minionSeeds)

    test "flat: monsters kill minions, which are revived together; one that does not revive is gone" (fun () ->
        let skeleton i = { Actor.make (sprintf "Skeleton %d" i) [ sure ] [ bite "Bite" 100.0 ] with Body = Some { body 150.0 (Some 5.0) with Evasion = 800.0; Armour = 300.0; LifeRegen = 5.0 } }
        let spirit = { Actor.make "Spirit" [ sure ] [ spell "Wail" Chaos 10.0 30.0 ] with Body = Some { body 90.0 None with Melee = false; EnergyShield = 60.0; Resist = Map.ofList [ Fire, 50.0 ] } }
        let player = Actor.make "Player" [ noIgnite ] [ idle ]
        let brutes = [ brute Physical 60.0 0.7; { brute Fire 45.0 1.1 with Name = "burner"; Position = Vec.make -6.0 3.0 }; { brute Chaos 30.0 0.9 with Name = "rot"; Position = Vec.make 3.0 -7.0 } ]
        let fight = { Fight.single brutes.Head player 30.0 with Enemies = brutes; Minions = [ skeleton 1; skeleton 2; skeleton 3; spirit ] }
        same fight minionSeeds
        let o = Sim.run fight 1u
        if not (o.Counts.ContainsKey "minion:died" && o.Counts.ContainsKey "minion:revived") then failwithf "expected deaths and revivals: %A" o.Counts
        // with a player that can be hurt the attacks go at it too; a companion takes its share
        let wolf = { Actor.make "Wolf" [ sure ] [ bite "Maul" 80.0 ] with Body = Some { body 2000.0 (Some 4.0) with TakesForOwner = 40.0 } }
        let hurt = { player with Vitals = Some { PlayerStats.basic 4000.0 100.0 with Evasion = 500.0; LifeRegen = 30.0; Block = 20.0 } }
        let fight = { fight with Player = hurt; Minions = wolf :: fight.Minions }
        same fight minionSeeds
        if not ((Sim.run fight 1u).Counts.ContainsKey "minion:took for owner") then failwith "expected the companion to take hits")

    test "flat: energy from minions' deaths, a minion that explodes at low life, melee minions that find no room" (fun () ->
        let nova = { spell "Nova" Fire 80.0 120.0 with Tags = Set.ofList [ Spell; Triggered ] }
        let meta = { Meta.simple "On Death" 55.0 "Nova" with Sources = [ MinionDeaths 50.0 ] }
        let player = { Actor.make "Player" [ noIgnite ] [ idle; nova ] with Rotation = [ Actor.always idle.Name ]; Metas = [ meta ] }
        let blast = { Skill.make "Instability" [ Area; Tag "Fire" ] with BaseDamage = Map.ofList [ Fire, { Min = 100.0; Max = 200.0 } ]; BaseCrit = 5.0; Delivery = AreaAroundSelf 30.0 }
        let skeleton i explodes =
            { Actor.make (sprintf "Skeleton %d" i) [ sure ] [ bite "Bite" 60.0; blast ] with
                Rotation = [ Actor.always "Bite" ]
                Body = Some { body 400.0 (Some 3.0) with Explodes = (if explodes then Some blast.Name else None) } }
        let fight = { Fight.single (brute Physical 110.0 0.5) player 25.0 with Minions = [ skeleton 1 true; skeleton 2 false; skeleton 3 true ] }
        same fight minionSeeds
        let o = Sim.run fight 1u
        if not (o.Counts.ContainsKey "minion:exploded" && o.Counts.ContainsKey "trigger:Nova") then failwithf "expected explosions and triggers: %A" o.Counts
        // big beasts around a small monster: four find room, the others wait
        let beast i = { Actor.make (sprintf "Beast %d" i) [ sure ] [ bite "Gore" 100.0 ] with Body = Some { body 1e9 None with Size = 6.0; Reach = 16.0 } }
        let small = { boss 1e12 with Size = 4.0 }
        let crowded = { Fight.single small (Actor.make "Player" [ noIgnite ] [ idle ]) 6.0 with Minions = [ for i in 1 .. 6 -> beast i ] }
        same crowded minionSeeds
        if not ((Sim.run crowded 1u).Counts.ContainsKey "minion:no room") then failwith "expected minions without room")

    test "flat: totems act while their buff is up; one spirit of several is called at random" (fun () ->
        let place = { Skill.make "Totem" [ Spell ] with Hits = []; Cooldown = Some 6.0 }
        let spirit n = { Actor.make n [ sure ] [ bite ("Bite " + n) 100.0 ] with ActiveWhile = Some n }
        let turret = { Actor.make "Turret" [ sure ] [ { spell "Shot" Lightning 30.0 70.0 with BaseTime = 0.3; Delivery = Projectiles(ProjectileShape.make 2) } ] with ActiveWhile = Some "Turret buff" }
        let player =
            { Actor.make "Player" [ noIgnite ] [ place; spell "Bolt" Fire 10.0 20.0 ] with
                Rules =
                    [ { Hook = OnUse; Filter = Called "Totem"; Chance = 100.0; Reaction = GrantOneOf([ "A"; "B"; "C" ], 3.5) }
                      { Hook = OnUse; Filter = Called "Totem"; Chance = 100.0; Reaction = GrantBuff("Turret buff", 4.0, 1, []) } ] }
        let fight = { Fight.single (boss 1e12) player 20.0 with Minions = [ spirit "A"; spirit "B"; spirit "C"; turret ] }
        same fight minionSeeds
        if not ((Sim.run fight 1u).DamageBy.ContainsKey "Shot (hit)") then failwith "expected the turret to shoot")

    test "flat: temporary minions are summoned, last their time and leave; corpses, bones and Runic Ward raise them" (fun () ->
        let call = { Skill.make "Call" [ Spell ] with Hits = []; Cooldown = Some 2.0 }
        let spirits = [ for i in 1 .. 3 -> { Actor.make (sprintf "Spirit %d" i) [ sure ] [ bite "Bite" 100.0 ] with Pool = Some "Spirits" } ]
        let player =
            { Actor.make "Player" [ noIgnite ] [ call; spell "Bolt" Fire 10.0 20.0 ] with
                Rotation = [ { Use = "Call"; When = fun v -> (Map.tryFind "Out:Spirits" v.Buffs |> Option.defaultValue 0) < 3 }; Actor.always "Bolt" ]
                Rules = [ { Hook = OnUse; Filter = Called "Call"; Chance = 100.0; Reaction = Summon("Spirits", 2, 3.0) } ] }
        let fight = { Fight.single (boss 1e12) player 15.0 with Minions = spirits }
        same fight minionSeeds
        let o = Sim.run fight 1u
        if not (o.Counts.ContainsKey "summoned:Spirits" && o.Counts.ContainsKey "minion:left") then failwithf "expected summons that leave: %A" o.Counts
        // a zombie from a corpse, or three for a power charge, the first of them empowered
        let raise = { Skill.make "Raise" [ Spell; Tag "Type:TargetsDestructibleCorpses" ] with Hits = [] }
        let zombies = [ for i in 1 .. 10 -> { Actor.make (sprintf "Zombie %d" i) [ sure ] [ bite "Chew" 100.0 ] with Pool = Some "Zombies"; Body = Some(body 300.0 None) } ]
        let rule = WithCorpse(Summon("Zombies", 1, 6.0), SpendStack("PowerCharge", SummonEmpowered("Zombies", 3, 8.0, [ Mod.more (Damage AllDamage) 100.0 ])))
        let raiser =
            { Actor.make "Player" [ noIgnite ] [ raise; { spell "Nova" Fire 200.0 300.0 with Delivery = AreaAroundSelf 80.0 } ] with
                Resources = [ "PowerCharge", 2 ]
                Rules = [ { Hook = OnUse; Filter = Called "Raise"; Chance = 100.0; Reaction = rule } ] }
        let weak i = { EnemyDef.atLevel 70 600.0 with Name = sprintf "weak %d" i; Position = Vec.make (10.0 + float i) (float i); Mods = [] }
        let fight = { Fight.single (brute Physical 90.0 0.8) raiser 20.0 with Enemies = brute Physical 90.0 0.8 :: [ for i in 1 .. 4 -> weak i ]; Minions = zombies }
        same fight minionSeeds
        // Unearth raises a construct from a dead reviving minion, once per death; beetles from corpses now and then
        let dig = { Skill.make "Dig" [ Spell; Tag "Type:TargetsDestructibleCorpses" ] with Hits = [] }
        let skeleton = { Actor.make "Skeleton" [ sure ] [ bite "Bite" 100.0 ] with Body = Some(body 100.0 (Some 5.0)) }
        let constructs = [ for i in 1 .. 4 -> { Actor.make (sprintf "Construct %d" i) [ sure ] [ bite "Claw" 10.0 ] with Pool = Some "Constructs" } ]
        let beetles = [ for i in 1 .. 4 -> { Actor.make (sprintf "Beetle %d" i) [ sure ] [ bite "Burst" 100.0 ] with Pool = Some "Beetles" } ]
        let digger =
            { Actor.make "Player" [ noIgnite ] [ dig; { spell "Nova" Fire 200.0 300.0 with Delivery = AreaAroundSelf 80.0 } ] with
                Periodics = [ { Name = "beetles"; Every = 1.0; Action = SummonFromCorpse("Beetles", 2, 1.5) }; { Name = "more"; Every = 2.3; Action = SummonMinions("Beetles", 1, 0.9) } ]
                Rules = [ { Hook = OnUse; Filter = Called "Dig"; Chance = 100.0; Reaction = SummonPerCorpse("Constructs", 2.5, true) } ] }
        let fight = { fight with Player = digger; Minions = skeleton :: constructs @ beetles }
        same fight minionSeeds
        if not ((Sim.run fight 1u).Counts.ContainsKey "summoned:Constructs") then failwith "expected constructs"
        // Wardbound casters, one per so much Runic Ward, all of it spent
        let bind = { Skill.make "Bind" [ Spell ] with Hits = []; Cooldown = Some 3.0 }
        let casters = [ for i in 1 .. 5 -> { Actor.make (sprintf "Caster %d" i) [ sure ] [ spell "Blast" Cold 50.0 100.0 ] with Pool = Some "Casters" } ]
        let binder =
            { Actor.make "Player" [ noIgnite ] [ bind; spell "Bolt" Fire 10.0 20.0 ] with
                Vitals = Some { PlayerStats.basic 100.0 100.0 with Ward = 100.0; WardRegen = 20.0 }
                Rules = [ { Hook = OnUse; Filter = Called "Bind"; Chance = 100.0; Reaction = SummonPerWard("Casters", 30.0, 2.5) } ] }
        same { Fight.single (boss 1e12) binder 12.0 with Minions = casters } minionSeeds)

    test "flat: a pack whose dead are replaced; minions beside the player in it" (fun () ->
        let weak i = { EnemyDef.make Rarity.defaults 75 (if i % 3 = 0 then Magic else Normal) (sprintf "pack %d" i) (Vec.make (12.0 + 4.0 * float i) (float (i % 4) * 6.0 - 9.0)) with Life = 400.0 + 150.0 * float i; Speed = 25.0 }
        let pack = [ for i in 1 .. 11 -> weak i ]
        let player =
            { Actor.make "Player" [ Mod.inc CritChance 80.0; Mod.base' (AilmentChance Poison) 60.0 ] [ { spell "Nova" Fire 80.0 160.0 with Delivery = AreaAroundSelf 35.0; BaseTime = 0.6 }; { spell "Hex" Chaos 1.0 2.0 with Cooldown = Some 3.0; Hits = [ 1.0, 1.0 ] } ] with
                Vitals = Some { PlayerStats.basic 6000.0 500.0 with Evasion = 900.0; LifeRegen = 80.0; ManaRegen = 40.0 }
                Rules =
                    [ { Hook = OnHit; Filter = Called "Hex"; Chance = 100.0; Reaction = ApplyCurse("Hex", 5.0, [ Mod.base' (Resist Fire) -20.0 ]) }
                      { Hook = OnKill; Filter = AnySkill; Chance = 100.0; Reaction = GrantBuff("Rampage", 3.0, 5, [ Mod.inc (Damage AllDamage) 8.0 ]) } ] }
        let fight = { Fight.single pack.Head player 14.0 with Enemies = pack; Replace = true; Targeting = Nearest }
        same fight minionSeeds
        let o = Sim.run fight 1u
        if o.Kills.Length <= pack.Length then failwithf "expected replacements to be killed too: %d kills" o.Kills.Length
        let hound = { Actor.make "Hound" [ sure ] [ bite "Bite" 150.0 ] with Body = Some(body 500.0 (Some 3.0)) }
        same { fight with Minions = [ hound; { hound with Name = "Hound 2" } ] } minionSeeds)

    test "flat: infusions dropped by ailments and consumed by impacts; Runic Ward and mana remnants" (fun () ->
        let bolt = { spell "Bolt" Lightning 60.0 240.0 with BaseTime = 0.5; Cost = 8.0 }
        let burst = { spell "Bolt: Burst" Cold 100.0 150.0 with Mods = [ Mod.flag (Named "OnlyInfused:ColdInfusion") ]; Delivery = AreaAtTarget 20.0 }
        let infused = { spell "Bolt infused" Lightning 200.0 500.0 with Delivery = AreaAtTarget 25.0; Mods = [ Mod.base' (Named "InfusionKeepChance") 25.0; Mod.base' (Named "PowerChargeOnInfusionConsumed") 1.0 ] }
        let frost = { spell "Frost" Cold 150.0 300.0 with Cooldown = Some 2.0; BaseTime = 0.4 }
        let player =
            { Actor.make "Player" [ Mod.inc CritChance 200.0; Mod.inc (Buildup Freeze) 4000.0; Mod.base' (AilmentChance Shock) 60.0; Mod.inc (Buildup HeavyStun) 3000.0; Mod.more (Damage AllDamage) 15.0 |> Mod.whenever (WhileBuff "ConsumedInfusionRecently") ]
                  [ frost; { bolt with Parts = [ burst ] }; infused ] with
                Rotation = [ Actor.always "Frost"; Actor.always "Bolt" ]
                Vitals = Some { PlayerStats.basic 2000.0 300.0 with ManaRegen = 6.0; Ward = 150.0; WardRegen = 0.0 }
                MaxResources = Map.ofList [ "LightningInfusion", 3; "ColdInfusion", 3; "PowerCharge", 3 ]
                Resources = [ "LightningInfusion", 1 ]
                Infusions =
                    Some
                        { Remnants = [ Shock, "LightningInfusion", 70.0; Freeze, "ColdInfusion", 100.0; Chill, "ColdInfusion", 60.0 ]
                          ExtraRemnantChance = 30.0
                          Delay = 0.4
                          Consumers = Map.ofList [ "Bolt", ("LightningInfusion", "Bolt infused") ]
                          Wildcard = Some "VerisiumInfusion" }
                WardRemnants = Some { OnKillChance = 60.0; OnStunChance = 100.0; StunCooldown = 1.0; WardPerRemnant = 40.0; ExtraChance = 30.0; Delay = 0.5; Overflow = 2.0 }
                ManaRemnants = Some { OnKillChance = 80.0; CritCooldown = 0.6; ManaPerRemnant = 25.0; PercentOfMaxMana = 5.0; ExtraChance = 20.0; Delay = 0.3; Overflow = 2.0 } }
        let weak i = { EnemyDef.atLevel 70 9000.0 with Name = sprintf "weak %d" i; Position = Vec.make (14.0 + float i) (float i - 2.0); Mods = [] }
        let fight = { Fight.single (boss 1e12) player 20.0 with Enemies = boss 1e12 :: [ for i in 1 .. 4 -> weak i ]; Targeting = Nearest }
        same fight seeds
        let o = Sim.run fight 1u
        for key in [ "infused:Bolt"; "remnant:LightningInfusion"; "remnant:ColdInfusion"; "hits:Bolt: Burst"; "mana remnant"; "ward remnant" ] do
            if not (o.Counts.ContainsKey key) then failwithf "expected %s: %A" key o.Counts
        // with remnants that are rare, most impacts are the plain skill's
        let scarce = { fight with Player = { player with Infusions = player.Infusions |> Option.map (fun inf -> { inf with Remnants = [ Shock, "LightningInfusion", 15.0 ] }) } }
        same scarce seeds
        if not ((Sim.run scarce 1u).Counts.ContainsKey "uninfused:Bolt") then failwith "expected impacts without an infusion")

    test "flat: what is not built is refused, with every reason" (fun () ->
        let player = Actor.make "Player" [] [ spell "Bolt" Fire 1.0 2.0 ]
        let fine = Fight.single (boss 1e9) player 5.0
        let refused (fight: Fight) (reason: string) =
            match Flat.compile fight with
            | Ok _ -> failwithf "compiled, but has %s" reason
            | Error why -> if not (why.Contains reason) then failwithf "refused for '%s', expected '%s'" why reason
        if not (Flat.unsupported fine).IsEmpty then failwithf "a plain fight is refused: %A" (Flat.unsupported fine)
        refused { fine with Minions = [ { Actor.make "Hound" [] [ strike "Bite" 1.0 2.0 ] with Vitals = Some(PlayerStats.basic 100.0 0.0) } ] } "a minion with pools"
        refused { fine with Minions = [ Actor.make "Player" [] [ strike "Bite" 1.0 2.0 ] ] } "actors of the same name"
        refused { fine with Player = { player with Metas = [ Meta.simple "Cast on Crit" 100.0 "Bolt"; Meta.simple "Cast on Crit" 50.0 "Bolt" ] } } "meta gems of the same name"
        refused { fine with Player = { player with Infusions = Some { Remnants = []; ExtraRemnantChance = 0.0; Delay = 0.5; Consumers = Map.ofList [ "Bolt", ("FireInfusion", "No such skill") ]; Wildcard = None } } } "an infusion of a skill"
        refused { fine with Player = { player with Skills = [ { spell "Bolt" Fire 1.0 2.0 with Mods = [ Mod.base' (Named "MaxSeals") 2.0 ] } ] } } "sealed skills"
        refused { fine with Player = { player with Rules = [ { Hook = OnHit; Filter = AnySkill; Chance = 100.0; Reaction = AttachPustule("Bolt", 3.0, 0.0, 0.0) } ] } } "the reaction AttachPustule"
        // both reasons of a fight with two
        let two = { fine with Enemies = [ { boss 1e9 with Name = "boss+" } ]; Replace = true; Player = { player with Banners = [ { Name = "War Banner"; Glory = 100.0; PerAttackHit = 5.0; Duration = 10.0; Mods = [] } ] } }
        if (Flat.unsupported two).Length <> 2 then failwithf "expected two reasons, got %A" (Flat.unsupported two))

    test "flat: random fights give the reference's outcome, or are refused" (fun () ->
        let mutable exact = 0
        let mutable within = 0
        let mutable threshold = 0
        let mutable example = ""
        let mutable compiled = 0
        let mutable refused = 0
        for case in 1u .. 400u do
            let fight, described = randomFight case
            match Flat.compile fight with
            | Error _ -> refused <- refused + 1
            | Ok _ ->
                compiled <- compiled + 1
                for seed in [ case; case * 7919u + 1u; 4000000000u - case ] do
                    match verdict fight seed with
                    | Exact -> exact <- exact + 1
                    | Within -> within <- within + 1
                    | AtThreshold d ->
                        threshold <- threshold + 1
                        if example = "" then example <- described + "\n  " + d
                    | Wrong d -> failwithf "%s\n  %s" described d
                    | NotCompiled -> ()
        let runs = exact + within + threshold
        printfn "     flat: %d random fights compiled (%d refused); of %d runs %d the same in every bit, %d the same within %g, %d parted at a threshold after a rounding difference" compiled refused runs exact within tolerance threshold
        if example <> "" then printfn "     for example %s" example
        if compiled < 250 then failwithf "only %d of 400 random fights compile" compiled
        if threshold * 1000 > runs then failwithf "%d of %d runs parted at a threshold: more than 1 in 1000" threshold runs)

    test "flat: a runner gives what a fresh run gives, whatever it has played before" (fun () ->
        let mutable compared = 0
        for case in 1u .. 60u do
            let fight, described = randomFight case
            match Flat.compile fight with
            | Error _ -> ()
            | Ok p ->
                let play = Flat.runner p
                for seed in [ 3u; 1u; 2u; 1u; 3u ] do
                    compared <- compared + 1
                    match differenceWith (fun a b -> a = b || (System.Double.IsNaN a && System.Double.IsNaN b)) (Flat.run p seed) (play seed) with
                    | Some d -> failwithf "%s\n  seed %d: %s (fresh run first, runner second)" described seed d
                    | None -> ()
        if compared < 100 then failwithf "only %d runs compared" compared)

    test "flat: how fast (rough; printed, not judged)" (fun () ->
        // a sheet like a character's: many mods of many stats, some conditional
        let sheet =
            [ for i in 1 .. 40 do
                yield Mod.inc (Damage AllDamage) (float i)
                yield Mod.inc (Damage(OfType Fire)) 5.0 |> Mod.whenever (WhileBuff "HitRecently")
                yield Mod.inc CritChance 10.0
                yield Mod.base' (Named(sprintf "Other%d" i)) 1.0
                yield Mod.inc Speed 1.0 |> Mod.whenever (EnemyHas "Shocked")
                yield Mod.base' (Resist Fire) 1.0 ]
        let echo = { spell "Echo" Cold 300.0 400.0 with Tags = Set.ofList [ Spell; Triggered ]; Cooldown = Some 0.4 }
        let player =
            { Actor.make "Player"
                  (sheet @ [ Mod.inc (Damage AllDamage) 12.0 |> Mod.whenever (PerStack("Momentum", 1.0)); Mod.inc CritChance 300.0; Mod.more (Damage AllDamage) 20.0 |> Mod.whenever (EnemyHas "Cursed"); Mod.base' (AilmentChance Poison) 40.0 ])
                  [ { spell "Hex" Chaos 1.0 2.0 with Cooldown = Some 5.0; Hits = []; BaseTime = 0.5 }; { spell "Bolt" Fire 100.0 300.0 with BaseTime = 0.5; Cost = 10.0 }; echo ] with
                Rotation = [ { Use = "Hex"; When = fun v -> not (v.EnemyStatus.ContainsKey "Curse:Hex") }; Actor.always "Bolt" ]
                Vitals = Some { PlayerStats.basic 1000.0 300.0 with ManaRegen = 30.0 }
                Rules =
                    [ { Hook = OnHit; Filter = Called "Bolt"; Chance = 100.0; Reaction = GrantBuff("Momentum", 2.5, 5, []) }
                      { Hook = OnUse; Filter = Called "Hex"; Chance = 100.0; Reaction = ApplyCurse("Hex", 4.0, [ Mod.base' (Resist Fire) -25.0 ]) }
                      { Hook = OnHit; Filter = AnySkill; Chance = 50.0; Reaction = ApplyDebuff("Sundered", 3.0, 4, [ Mod.inc (DamageTaken AllDamage) 4.0 ]) }
                      { Hook = OnCrit; Filter = Called "Bolt"; Chance = 100.0; Reaction = TriggerSkill "Echo" } ] }
        let big = Fight.single (EnemyDef.make Rarity.defaults 83 Unique "Boss" (Vec.make 30.0 0.0) |> fun e -> { e with Life = 1e12; Attack = None }) player 30.0
        let ms (f: unit -> unit) =
            let t0 = System.DateTime.UtcNow
            f ()
            (System.DateTime.UtcNow - t0).TotalMilliseconds
        let events (p: Flat.Program) =
            let mutable n = 0
            Flat.traced (fun _ kind _ _ _ -> if kind <> "hit" && kind <> "crit" then n <- n + 1) p 1u |> ignore
            n
        let measure (what: string) (fights: (Fight * Flat.Program) list) (simRuns: int) (flatRuns: int) =
            let programs = fights
            // (both engines run first, so the runtime has compiled them)
            for (f, p) in fights do
                Sim.run f 1u |> ignore
                for seed in 1u .. 20u do
                    Flat.run p seed |> ignore
            let fights = fights |> List.map (fun (f, p) -> f, Flat.runner p)
            let sim = ms (fun () -> for (f, _) in fights do (for seed in 1 .. simRuns do Sim.run f (uint32 seed) |> ignore)) / float (simRuns * fights.Length)
            let flat = ms (fun () -> for (_, play) in fights do (for seed in 1 .. flatRuns do play (uint32 seed) |> ignore)) / float (flatRuns * fights.Length)
            let perFight = float (programs |> List.sumBy (fun (_, p) -> events p)) / float fights.Length
            printfn "     flat: %s: reference %.3f ms a fight (%.1f us an event), flat %.4f ms (%.2f us an event), %.1f times as fast" what sim (sim * 1000.0 / perFight) flat (flat * 1000.0 / perFight) (sim / flat)
        (match Flat.compile big with
         | Ok p -> measure "a 240-mod sheet, 30 s" [ big, p ] 10 200
         | Error why -> failwith why)
        let random =
            [ 1u .. 120u ] |> List.choose (fun case ->
                let f, _ = randomFight case
                match Flat.compile f with
                | Ok p -> Some(f, p)
                | Error _ -> None)
        measure (sprintf "%d random fights" random.Length) random 2 20)
