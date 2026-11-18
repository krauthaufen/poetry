/// Time passing between two events: damage over time, recovery, what runs out.
[<AutoOpen>]
module internal PoEtry.Engine.Clock

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// Damage over time on one enemy for the time that passes (`dt`; `n` enemies share the
/// count of how long each status was up): its life, the books, and its death if it comes.
let sufferOverTime (dt: float) (n: float) (w: World) (e: EnemyState) : World =
    // (nothing over time on it: only its statuses' time is counted, and it stays the
    // very value it was, so what is remembered about it stays good)
    if e.Dots.IsEmpty then
      { w with Uptime = status e |> Map.fold (fun m k _ -> addf k (dt / n) m) w.Uptime }
    else
      let def = defender e
      let bleedK = if e.Def.Moving then Constants.bleedMovingMultiplier else 1.0
      let active = activeDots e
      let parts =
          active
          |> List.map (fun d ->
              let k = if d.Ailment = Some Bleed then bleedK else 1.0
              d.Label, d.Dps * k * Ailments.dotTakenMult def d.Type)
      let types =
          active
          |> List.map (fun d ->
              let k = if d.Ailment = Some Bleed then bleedK else 1.0
              DamageType.name d.Type, d.Dps * k * Ailments.dotTakenMult def d.Type)
      let dps = parts |> List.sumBy snd
      let span = if dps > 0.0 && e.Life <= dps * dt then e.Life / dps else dt
      let life = e.Life - dps * span
      let dead = life <= 1e-9
      { w with
          Enemies = Map.add e.Def.Name { e with Life = (if dead then 0.0 else life) } w.Enemies
          DamageBy = parts |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageBy
          DamageByType = types |> List.fold (fun m (k, v) -> addf k (v * span) m) w.DamageByType
          Uptime = status e |> Map.fold (fun m k _ -> addf k (span / n) m) w.Uptime
          CorpsesUsed = corpsesAfter dead e.Def.Name (w.Time + span) w
          Kills = if dead then (e.Def.Name, e.Def.Rarity, w.Time + span) :: w.Kills else w.Kills }

/// On a march a monster the player is not near enough to sleeps, and wakes for good once
/// the player is, or when there is nothing ahead of the player any more. Whether it
/// sleeps still (`d`: how far it is from the player).
let wakes (env: Env) (e: EnemyState) (d: float) (w: World) : bool * World =
    match env.Fight.March with
    | Some m when not (w.Woken.Contains e.Def.Name) ->
        // (the room of the pack it came with: the player is in it once the pack's middle is half a spacing ahead or less)
        let near =
            if not m.Rooms then d - e.Def.Size <= m.Aggro
            else
                let pack = int (e.Def.Name.Substring(e.Def.Name.LastIndexOf '#' + 1).TrimEnd('+'))
                w.Front - float (w.Packs - 1 - pack) * m.Spacing <= m.Spacing / 2.0
        if near || w.Packs > m.Packs && not (w.Enemies |> Map.exists (fun _ x -> x.Life > 0.0 && x.Def.Position.X > 0.0))
        then false, { w with Woken = w.Woken.Add e.Def.Name }
        else true, w
    | _ -> false, w

/// A monster walks towards the player until it is in attack range (stopped, asleep or
/// there already: it stands), slowed by what is on it.
let walk (env: Env) (dt: float) (w: World) (e: EnemyState) : World =
    match e.Def.Attack with
    | Some atk when e.Life > 0.0 && w.Player.IsSome ->
        let st = status e
        let stopped = e.StunnedUntil > w.Time || [ "Frozen"; "HeavilyStunned"; "Electrocuted"; "Pinned" ] |> List.exists st.ContainsKey
        // (a slow weakens with rarity, and never brings a monster below a quarter of its speed)
        let potency = Rarity.slowPotency e.Def.Rarity
        let slow =
            (if st.ContainsKey "Maimed" then max 0.0 (1.0 - Constants.maimSlow * env.SlowMagnitude * potency / 100.0) else 1.0)
            * (if st.ContainsKey "Hindered" then max 0.0 (1.0 - Constants.hinderSlow * env.SlowMagnitude * potency / 100.0) else 1.0)
            * (1.0 - (e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0) / 100.0)
        // what its debuffs make of its movement speed (Jagged Ground, "Hindered, with 15% reduced Movement Speed")
        let slow = if e.Debuffs.IsEmpty then slow else slow * Rarity.slowed e.Def.Rarity (max 0.0 (Mods.more Ctx.empty [ Named "MovementSpeed" ] (defender e).Mods))
        let slow = max Constants.slowFloor slow
        let d = Vec.length e.Def.Position
        let gap = d - e.Def.Size - atk.Range
        let asleep, w = wakes env e d w
        // (one that walks is moving: its Bleeding deals twice the damage)
        if stopped || asleep || gap <= 0.0 then
            if e.Def.Moving then { w with Enemies = w.Enemies |> Map.change e.Def.Name (Option.map (fun e -> { e with Def = { e.Def with Moving = false } })) } else w
        else
            let step = min gap (e.Def.Speed * slow * dt)
            let pos = Vec.scale ((d - step) / d) e.Def.Position
            { w with Enemies = w.Enemies |> Map.change e.Def.Name (Option.map (fun e -> { e with Def = { e.Def with Position = pos; Moving = true } })) }
    | _ -> w

/// A march: the player walks on until `t`, less so while a skill it uses holds it. Reckoned
/// around the player, that is every enemy (and the packs to come) coming nearer.
let playerWalksOn (env: Env) (t: float) (w: World) : World =
    let dt = t - w.Time
    match env.Fight.March with
    | Some m when w.Player |> Option.forall (fun p -> p.Died.IsNone) ->
        let held = max 0.0 (min t w.WalkHeldUntil - w.Time)
        let met = m.Hold && (alive w |> List.exists (fun e -> Vec.length e.Def.Position - e.Def.Size <= m.Engage))
        let ahead = w.Packs <= m.Packs || (w.Enemies |> Map.exists (fun _ e -> e.Life > 0.0 && e.Def.Position.X > 0.0))
        let way = if met then 0.0 else m.Speed * (held * w.WalkKept + (dt - held))
        if way <= 0.0 then w
        elif ahead then
            { w with
                Walked = w.Walked + way
                Front = w.Front - way
                Enemies = w.Enemies |> Map.map (fun _ e -> if e.Life > 0.0 then { e with Def = { e.Def with Position = { e.Def.Position with X = e.Def.Position.X - way } } } else e) }
        else
            // nothing ahead any more: it walks to the nearest of what is left (one that
            // shoots from where it stands does not come), until that is near enough
            match alive w with
            | [] -> w
            | left ->
                let near = left |> List.minBy (fun e -> Vec.length e.Def.Position - e.Def.Size)
                let d = Vec.length near.Def.Position
                let step = min way (d - near.Def.Size - m.Engage)
                if step <= 0.0 || d <= 1e-9 then w
                else
                    let by = Vec.scale (step / d) near.Def.Position
                    { w with
                        Walked = w.Walked + step
                        Enemies = w.Enemies |> Map.map (fun _ e -> if e.Life > 0.0 then { e with Def = { e.Def with Position = Vec.sub e.Def.Position by } } else e) }
    | _ -> w

let advance env t (w: World) =
    let dt = t - w.Time
    if dt <= 0.0 then
        w
    else
        let living = alive w
        let n = float living.Length
        let w = living |> List.fold (sufferOverTime dt n) w
        // monsters walk towards the player until they're in attack range
        let w = alive w |> List.fold (walk env dt) w
        let w = playerWalksOn env t w
        { w with Time = t }

/// Auras on enemies in the player's Presence: an enemy has them from when its body reaches
/// into the Presence, and loses them when it is out of it again. (One that is put on an
/// enemy by something else under the same name and runs out is the aura's again.)
let syncPresence (fight: Fight) (w: World) =
    if fight.PresenceAuras.IsEmpty && fight.PresenceEntry.IsEmpty then w
    else
        alive w
        |> List.fold
            (fun (w: World) e ->
                let inside = Vec.length e.Def.Position - e.Def.Size <= fight.Presence
                let debuffs =
                    fight.PresenceAuras
                    |> List.fold
                        (fun (ds: Map<string, Effect>) (name, stacks, mods) ->
                            match Map.tryFind name ds |> Option.filter (fun x -> x.Expires > w.Time) with
                            | None when inside ->
                                // (an exposure is as strong as the enemy lets exposure be)
                                let mods = if name.StartsWith "Exposure:" then mods |> List.map (Mod.scaled (Mods.more Ctx.empty [ Named "ExposureEffectOnSelf" ] e.Db)) else mods
                                Map.add name { Name = name; Mods = mods; Stacks = stacks; Expires = infinity } ds
                            | Some x when not inside && System.Double.IsPositiveInfinity x.Expires -> Map.remove name ds
                            | _ -> ds)
                        e.Debuffs
                // what an enemy gets once as it comes in: a mark says it is inside, and is
                // taken off when it is out again
                let debuffs, expiries =
                    fight.PresenceEntry
                    |> List.fold
                        (fun (ds: Map<string, Effect>, ex: float list) (name, stacks, mods, seconds) ->
                            let mark = "Entered: " + name
                            match Map.containsKey mark ds with
                            | false when inside ->
                                let ds = Map.add mark { Name = mark; Mods = []; Stacks = 1; Expires = infinity } ds
                                if Map.tryFind name ds |> Option.exists (fun x -> x.Expires > w.Time) then ds, ex
                                else Map.add name { Name = name; Mods = mods; Stacks = stacks; Expires = w.Time + seconds } ds, (w.Time + seconds) :: ex
                            | true when not inside -> Map.remove mark ds, ex
                            | _ -> ds, ex)
                        (debuffs, [])
                let w = if obj.ReferenceEquals(debuffs, e.Debuffs) then w else { w with Enemies = Map.add e.Def.Name { e with Debuffs = debuffs } w.Enemies }
                List.foldBack (fun t w -> schedule t Expire w) expiries w)
            w

/// Mana recovery while time passes: regeneration, leech (rate-capped), ES recharge, life regen.
/// Mitigated damage onto the player, by type: energy shield first (chaos damage removes
/// twice as much of it), a share of the rest from mana ("damage taken from mana before
/// life"), life down to 1, then Runic Ward; death when nothing is left. Starts recoup,
/// and drinks the life flask when life drops below half.
let takeDamage (fromHit: bool) (pools: PlayerStats) (damage: Map<DamageType, float>) (w: World) =
    match w.Player with
    | Some p when p.Died.IsNone ->
        let dmg = damage |> Map.toList |> List.sumBy snd
        if dmg <= 0.0 then w
        else
        // Darkness: damage is reserved from what is not reserved of it, before anything else;
        // a reservation is over after its time
        let damage, darkness =
            if pools.Darkness <= 0.0 then damage, p.Darkness
            else
                let live = p.Darkness |> List.filter (fun (until, _) -> until > w.Time)
                let taken = min (max 0.0 (pools.Darkness - (live |> List.sumBy snd))) dmg
                if taken <= 0.0 then damage, live
                else damage |> Map.map (fun _ d -> d * (1.0 - taken / dmg)), (w.Time + pools.DarknessSeconds, taken) :: live
        let bypass = pools.BypassES / 100.0
        let es, rest =
            damage
            |> Map.fold
                (fun (es, rest) t d ->
                    let k = if t = Chaos then 2.0 else 1.0
                    let absorbed = min (d * (1.0 - bypass)) (es / k)
                    es - absorbed * k, rest + d - absorbed)
                (p.EnergyShield, 0.0)
        let fromMana = min (max 0.0 p.Mana) (rest * pools.ManaBeforeLife / 100.0)
        let rest = rest - fromMana
        let toLife = min (max 0.0 (p.Life - 1.0)) rest
        let rest = rest - toLife
        let toWard = min p.Ward rest
        let rest = rest - toWard
        // life loss from a hit put off: a share prevented, a share lost over 4 s (a negative recoup)
        let deferred = fromHit && pools.DeferPrevent > 0.0
        let life = p.Life - toLife * (if deferred then 1.0 - pools.DeferPrevent / 100.0 else 1.0) - rest
        let later = if deferred then toLife * pools.DeferLose / 100.0 else 0.0
        let recoups =
            pools.Recoup
            |> List.choose (fun (pool, types, pct) ->
                let rate = match pool with "Life" -> pools.LifeRecoveryRate | "Mana" -> pools.ManaRecoveryRate | _ -> pools.ESRecoveryRate
                let amount = (types |> List.sumBy (fun t -> Map.tryFind t damage |> Option.defaultValue 0.0)) * pct / 100.0 * rate
                if amount > 0.0 then Some { Until = w.Time + pools.RecoupSeconds; Rate = amount / pools.RecoupSeconds; Pool = pool } else None)
        let recoups = (if later > 0.0 then [ { Until = w.Time + Constants.deferredLossSeconds; Rate = -later / Constants.deferredLossSeconds; Pool = "Life" } ] else []) @ recoups
        let p =
            { p with
                Mana = p.Mana - fromMana
                EnergyShield = es
                Ward = p.Ward - toWard
                Darkness = darkness
                Life = life
                Taken = p.Taken + dmg
                Recoups = recoups @ p.Recoups
                MinLife = min p.MinLife ((max 0.0 life + p.Ward) / max 1.0 (pools.Life + pools.Ward))
                Died = if life <= 0.0 && rest > 0.0 then Some w.Time else None }
        match pools.LifeFlask with
        | Some f when p.Died.IsNone && pools.Life > 1.0 && life < 0.5 * pools.Life && w.Time >= p.LifeFlaskUntil && p.LifeFlaskCharges >= f.ChargesPerUse ->
            { w with
                Player =
                    Some
                        { p with
                            LifeFlaskCharges = p.LifeFlaskCharges - f.ChargesPerUse
                            LifeFlaskUntil = w.Time + f.Duration
                            LifeFlaskRate = f.Amount * (1.0 - f.Instant) * pools.LifeRecoveryRate / f.Duration
                            Life = min (max p.Life pools.Life) (p.Life + f.Amount * f.Instant * pools.LifeRecoveryRate)
                            EnergyShield =
                                if pools.LifeFlaskAlsoES then min (max p.EnergyShield (min pools.EnergyShield pools.ESRecoverCap)) (p.EnergyShield + f.Amount * f.Instant * pools.LifeRecoveryRate)
                                else p.EnergyShield }
                Counts = bump "flask:life" w.Counts }
        | _ -> { w with Player = Some p }
    | _ -> w

/// Recovery and damage over time on the player during dt (ending now). The pools are the
/// sheet's (`stats`); how fast they recover is of the moment (`rates`: the defences with
/// the buffs and states that held during dt).
let recover (stats: PlayerStats option) (rates: PlayerStats option) (flask: ManaFlask option) (charms: Charm list) dt (w: World) =
    let w =
        match w.Player with
        | Some p when dt > 0.0 && charms |> List.exists (fun c -> c.ChargesPerSecond > 0.0) ->
            { w with
                Player =
                    Some
                        { p with
                            CharmCharges =
                                charms |> List.fold (fun (m: Map<string, float>) c ->
                                    Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + c.ChargesPerSecond * c.ChargeGainMult * dt)) m) p.CharmCharges } }
        | _ -> w
    let w =
        match flask, w.Player with
        | Some f, Some p when dt > 0.0 ->
            { w with Player = Some { p with FlaskCharges = min f.MaxCharges (p.FlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt) } }
        | _ -> w
    match stats, w.Player with
    | Some v, Some p when dt > 0.0 && p.Died.IsNone ->
        let r = rates |> Option.defaultValue v
        let t0 = w.Time - dt
        let overlap until = max 0.0 (min w.Time until - t0)
        let stream (pool: string) =
            let parts = p.Recoups |> List.filter (fun r -> r.Pool = pool) |> List.map (fun r -> r.Rate * overlap r.Until)
            // leech: "only the instance with the highest recovery rate will be active" (wiki, Leech)
            if pool.StartsWith "Leech" then (if parts.IsEmpty then 0.0 else List.max parts) else List.sum parts
        // leech: each instance is recovered over one second (keyword text)
        let leech = stream "LeechMana"
        let regen = r.ManaRegen * dt
        // (mana below nothing: a channel's cost that regeneration is still to cover, `manaNeeded`)
        let mana = min v.Mana (max (min 0.0 p.Mana) (p.Mana + regen + leech))
        let es =
            (if w.Time - p.LastHit >= r.ESRechargeDelay then p.EnergyShield + r.EnergyShieldRecharge * dt else p.EnergyShield)
            + r.ESRegen * dt + stream "ES" + stream "ShroudES"
            + (if v.LifeFlaskAlsoES then p.LifeFlaskRate * overlap p.LifeFlaskUntil else 0.0)
            |> min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap))
        let flask = p.FlaskRate * overlap p.FlaskUntil
        let recouped = stream "Mana"
        let mana = min v.Mana (mana + flask + recouped)
        let gained =
            p.ManaGained |> addf "regen" (min regen (v.Mana - p.Mana)) |> addf "leech" leech |> addf "flask" flask
            |> (if recouped > 0.0 then addf "recoup" recouped else id)
        let lifeLeech = stream "LeechLife"
        let lifeRecharge = if r.LifeRecharge > 0.0 && w.Time - p.LastHit >= r.ESRechargeDelay then r.LifeRecharge * dt else 0.0
        let lifeGain = r.LifeRegen * dt + lifeLeech + lifeRecharge + p.LifeFlaskRate * overlap p.LifeFlaskUntil + stream "Life"
        let lifeCharges =
            match v.LifeFlask with
            | Some f -> min f.MaxCharges (p.LifeFlaskCharges + f.ChargesPerSecond * f.ChargeGainMult * dt)
            | None -> p.LifeFlaskCharges
        let dots =
            p.Dots
            |> List.fold (fun (m: Map<DamageType, float>) (d: PlayerDot) -> Map.add d.Damage ((Map.tryFind d.Damage m |> Option.defaultValue 0.0) + d.PerSecond * overlap d.Until) m) Map.empty
        let burning = dots |> Map.exists (fun _ d -> d > 0.0)
        let w =
            { w with
                Player =
                    Some
                        { p with
                            Mana = mana
                            EnergyShield = es
                            Life = min (max p.Life v.Life) (p.Life + lifeGain)
                            LifeFlaskCharges = lifeCharges
                            Ward = (if p.Ward >= v.Ward then p.Ward else min v.Ward (p.Ward + r.WardRegen * dt))
                            Recoups = p.Recoups |> List.filter (fun r -> r.Until > w.Time)
                            Dots = p.Dots |> List.filter (fun d -> d.Until > w.Time)
                            // damage over time keeps energy shield from recharging
                            LastHit = (if burning then w.Time else p.LastHit)
                            ManaGained = gained } }
        // (life lost over time down to nothing: dead)
        let w =
            match w.Player with
            | Some p when p.Life <= 0.0 && p.Died.IsNone -> { w with Player = Some { p with Died = Some w.Time } }
            | _ -> w
        if burning then takeDamage false v dots w else w
    | _ -> w

let gainMana (v: PlayerStats) source amount (w: World) =
    match w.Player with
    | Some p when amount > 0.0 ->
        let m = min v.Mana (p.Mana + amount)
        { w with Player = Some { p with Mana = m; ManaGained = addf source (m - p.Mana) p.ManaGained } }
    | _ -> w

let prune (w: World) =
    let live (x: Effect) = x.Expires > w.Time
    // (what has nothing to lose stays the very value it was: nothing is built anew, and
    // what is remembered for an actor's buffs stays good)
    let enemyStale (e: EnemyState) = (e.Debuffs |> Map.exists (fun _ x -> not (live x))) || (e.Dots |> List.exists (fun d -> not (d.Expires > w.Time)))
    let actorStale (st: ActorState) = st.Buffs |> Map.exists (fun _ x -> not (live x))
    { w with
        Enemies =
            (if w.Enemies |> Map.exists (fun _ e -> enemyStale e) then
                w.Enemies
                |> Map.map (fun _ e ->
                    if enemyStale e then
                        { e with
                            Debuffs = e.Debuffs |> Map.filter (fun _ x -> live x)
                            Dots = e.Dots |> List.filter (fun d -> d.Expires > w.Time) }
                    else e)
             else w.Enemies)
        Actors =
            (if w.Actors |> Map.exists (fun _ st -> actorStale st) then
                w.Actors |> Map.map (fun _ st -> if actorStale st then { st with Buffs = st.Buffs |> Map.filter (fun _ x -> live x) } else st)
             else w.Actors)
        Counts =
            (if w.Actors |> Map.exists (fun _ st -> actorStale st) then
                w.Actors
                |> Map.fold (fun c _ st -> st.Buffs |> Map.fold (fun c k x -> if not (live x) && x.Expires < infinity && k.EndsWith "Charge" then bump ("expired:" + k) c else c) c) w.Counts
             else w.Counts) }

// ---- effects ----

let stackEffect now (existing: Effect option) name duration maxStacks mods =
    let stacks =
        match existing with
        | Some e when e.Expires > now -> min maxStacks (e.Stacks + 1)
        | _ -> 1
    { Name = name
      Mods = mods
      Stacks = stacks
      Expires = now + duration }

let updateActor name f (w: World) =
    { w with Actors = Map.add name (f (Map.find name w.Actors)) w.Actors }

let updateEnemy name f (w: World) =
    { w with Enemies = Map.add name (f (Map.find name w.Enemies)) w.Enemies }
