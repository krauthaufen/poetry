/// What the player could take at one moment of a fight: a hit that is not really dealt is put
/// through the same steps as a monster's (`Monsters`), on the world of that moment, and thrown
/// away. Nothing here changes the fight and nothing here rolls.
[<AutoOpen>]
module internal PoEtry.Engine.Probe

open PoEtry
open PoEtry.Engine

/// The world with the player unhurt: life, energy shield and Runic Ward as a fight begins
/// with them, mana as it is now (what the skills have spent of it is the build's own doing).
let unhurt (start: PlayerState) (w: World) : World =
    { w with Player = w.Player |> Option.map (fun p -> { p with Life = start.Life; EnergyShield = start.EnergyShield; Ward = start.Ward; Darkness = []; Dots = [] }) }

/// What a hit of `raw` damage of one type leaves to be taken, after the defences of the moment.
let leftOf (env: Env) (v: PlayerStats) (deflected: bool) (blocked: bool) (t: DamageType) (raw: float) (w: World) : Map<DamageType, float> =
    let taken = mitigatedBy env v false false deflected blocked (takenAs v (Map.ofList [ t, raw ])) w
    match companionGuard env taken w with
    | Some (_, k) -> taken |> Map.map (fun _ d -> d * (1.0 - k))
    | None -> taken

/// Whether the player lives through this much damage, already mitigated.
let livesThrough (pools: PlayerStats) (v: PlayerStats) (taken: Map<DamageType, float>) (w: World) : bool =
    (takeDamage true pools taken (recoverBeforeHit v w)).Player |> Option.exists (fun p -> p.Died.IsNone)

/// The largest `x` in [0, top] for which `lives x` holds (it holds less the larger `x` is);
/// infinity where it holds at the top.
let private largest (top: float) (lives: float -> bool) =
    if lives top then infinity
    else
        let rec go lo hi n = if n = 0 then lo else (let mid = (lo + hi) / 2.0 in if lives mid then go mid hi (n - 1) else go lo mid (n - 1))
        go 0.0 top 50

/// The defences at this moment of the fight, from unhurt pools: the largest hit of each type
/// lived through (a hit that lands: not evaded, not blocked, not deflected, not critical).
let probe (env: Env) (start: PlayerState) (w: World) : DefenceSample =
    let pools = env.Fight.Player.Vitals.Value
    let v = vitalsNow env w |> Option.defaultValue pools
    let w = unhurt start w
    { Time = w.Time
      MaxHit = DamageType.all |> List.map (fun t -> t, largest 1e9 (fun raw -> livesThrough pools v (leftOf env v false false t raw w) w)) |> Map.ofList }
