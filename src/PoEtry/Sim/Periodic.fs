/// What happens on a timer, and kills by damage over time.
[<AutoOpen>]
module internal PoEtry.Engine.Periodic

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

let onPeriodic env (rng: Rng) actorName periodicName (w: World) =
    let actor = actorDef env actorName
    match actor.Periodics |> List.tryFind (fun p -> p.Name = periodicName) with
    | None -> w
    | Some p ->
        let w = schedule (w.Time + p.Every) (PeriodicTick(actorName, periodicName)) w
        let st = Map.find actorName w.Actors
        let gain r n w =
            if conduit actor r then { w with Counts = bump ("conduit:" + r) w.Counts }
            else
                setStacksWith env actorName true r n w
                |> markStatus actorName ("Gained" + r + "Recently")
                |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
        match p.Action with
        | GainRandom resources ->
            let open' = resources |> List.filter (fun r -> stacksOf st r < (Map.tryFind r actor.MaxResources |> Option.defaultValue 0))
            if open'.IsEmpty then w
            else
                let r = open'.[min (open'.Length - 1) (int (rng.Float() * float open'.Length))]
                gain r (stacksOf st r + 1) w
        | GainWeighted weights ->
            let total = weights |> List.sumBy snd
            let roll = rng.Float() * total
            let r =
                weights
                |> List.fold (fun (picked, acc) (k, wgt) -> match picked with Some _ -> picked, acc | None -> (if roll < acc + wgt then Some k else None), acc + wgt) (None, 0.0)
                |> fst
                |> Option.defaultValue (fst (List.last weights))
            let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
            gain r (min maxR (stacksOf st r + 1)) w
        | GainAnyRandom resources ->
            let r = resources.[min (resources.Length - 1) (int (rng.Float() * float resources.Length))]
            let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue 0
            gain r (min maxR (stacksOf st r + 1)) w
        | SummonMinions (pool, count, duration) -> summon env pool count duration w
        | SummonFromCorpse (pool, count, duration) ->
            if w.Kills.Length > w.CorpsesUsed then summon env pool count duration { w with CorpsesUsed = w.CorpsesUsed + 1 } else w
        | TriggerNamed skill -> trigger env rng (actorDef env actorName) skill w
        | HealLife amount ->
            match env.Fight.Player.Vitals, w.Player with
            | Some v, Some p when p.Died.IsNone && v.Life > 1.0 ->
                { w with Player = Some { p with Life = min (max p.Life v.Life) (p.Life + amount * v.LifeRecoveryRate) }; Counts = bump "healed" w.Counts }
            | _ -> w
        | GainTimedBuff (buff, chance, duration, maxStacks, mods) ->
            if rng.Float() >= chance || stacksOf st buff >= maxStacks then w
            else
                updateActor actorName (fun st ->
                    let n = stacksOf st buff + 1
                    { st with Buffs = Map.add buff { Name = buff; Mods = mods; Stacks = n; Expires = infinity } st.Buffs }) w
                |> schedule (w.Time + duration) (SpendBuff(actorName, buff))
                |> fun w -> { w with Counts = bump ("gained:" + buff) w.Counts }
        | ConsumeEach resources ->
            let skip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] (ModDb.ofList actor.Mods)
            resources
            |> List.fold
                (fun w r ->
                    let st = Map.find actorName w.Actors
                    if stacksOf st r = 0 then w
                    else
                        let w = markStatus actorName ("Consumed" + r + "Recently") w
                        // (mana for consuming power charges: once, kept or not)
                        let w =
                            match actor.Vitals with
                            | Some v when r = "PowerCharge" ->
                                gainMana v "consume" (v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] (ModDb.ofList actor.Mods) / 100.0) w
                            | _ -> w
                        if rng.Chance skip then { w with Counts = bump ("kept:" + r) w.Counts }
                        else
                            setStacksWith env actorName false r (stacksOf st r - 1) w |> fun w -> { w with Counts = bump ("lost:" + r) w.Counts }
                            |> fun w -> if r = "EnduranceCharge" then enduranceLife actor 1 w else w)
                w

/// Whether an enemy that died of damage over time at `at` is the player's kill: some damage
/// over time of the player's was on it then (one only of minions is theirs, as their hits are).
let playersDotKill env (e: EnemyState) at =
    e.Dots |> List.exists (fun d -> d.Owner = env.Fight.Player.Name && d.Expires >= at)

/// What a kill by damage over time gives besides the rules for kills: what a killing hit gives
/// where the game does not tie it to a hit. The player's kill: flask charges, life, energy
/// shield and mana on kill (the character's own, no skill's), Runic Ward and Mana Remnants, Rage
/// on kill, killed recently; any kill: charm charges and what kills of rare monsters set off.
/// Meta energy from kills is from melee attack kills only (Reaper's Invocation), never these.
let dotKillGains env (rng: Rng) (e: EnemyState) mine (w: World) =
    let player = env.Fight.Player
    let w =
        match player.Vitals, w.Player with
        | Some v, Some p when mine ->
            let per = RarityCharges.flaskChargesPerKill e.Def.Rarity
            let w =
                { w with
                    Player =
                        Some
                            { p with
                                FlaskCharges = (match player.ManaFlask with Some f -> min f.MaxCharges (p.FlaskCharges + per * f.ChargeGainMult) | None -> p.FlaskCharges)
                                LifeFlaskCharges = (match v.LifeFlask with Some f -> min f.MaxCharges (p.LifeFlaskCharges + per * f.ChargeGainMult) | None -> p.LifeFlaskCharges)
                                Life = min (max p.Life v.Life) (p.Life + v.LifeOnKill)
                                EnergyShield = min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap)) (p.EnergyShield + v.ESOnKill) } }
            let sheet = ModDb.ofList player.Mods
            gainMana v "kill" ((Mods.sumBase Ctx.empty [ ManaOnKill ] sheet + v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnKill" ] sheet / 100.0) * v.ManaRecoveryRate) w
        | _ -> w
    let w =
        match player.WardRemnants, w.Player with
        | Some r, Some _ when mine && rng.Chance r.OnKillChance ->
            let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
            { w with Counts = bump "ward remnant" w.Counts } |> schedule (w.Time + r.Delay) (WardArrives(r.WardPerRemnant * count))
        | _ -> w
    let w =
        match player.ManaRemnants, w.Player with
        | Some r, Some _ when mine ->
            let ailing = [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.exists (status e).ContainsKey
            if ailing && rng.Chance r.OnKillChance then
                let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                { w with Counts = bump "mana remnant" w.Counts } |> schedule (w.Time + r.Delay) (ManaArrives((r.ManaPerRemnant + r.PercentOfMaxMana / 100.0 * (player.Vitals |> Option.map (fun v -> v.Mana) |> Option.defaultValue 0.0)) * count))
            else w
        | _ -> w
    let w =
        match player.LifeRemnants, player.Vitals, w.Player with
        | Some r, Some v, Some p when mine ->
            if rng.Chance r.OnKillChance then
                let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                let rate = (r.LifePerRemnant + r.PercentOfMaxLife / 100.0 * v.Life) * count * v.LifeRecoveryRate / r.Delay
                { w with Player = Some { p with Recoups = { Until = w.Time + r.Delay; Rate = rate; Pool = "Life" } :: p.Recoups }; Counts = bump "life remnant" w.Counts }
            else w
        | _ -> w
    let w =
        if player.Charms.IsEmpty then w
        else
            let w =
                { w with
                    Player =
                        w.Player |> Option.map (fun p ->
                            { p with
                                CharmCharges =
                                    player.Charms |> List.fold (fun (m: Map<string, float>) c ->
                                        let gain = (MonsterPower.ofRarity e.Def.Rarity / 2.0 + (if rng.Chance c.ChargeOnKillChance then 1.0 else 0.0)) * c.ChargeGainMult
                                        Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + gain)) m) p.CharmCharges }) }
            if e.Def.Rarity = Rare || e.Def.Rarity = Unique then fst (charm env rng "KillRare" w) else w
    if not mine then w
    else
        let onKill = env.Rage.OnKill
        gainRage env onKill w |> markRecently player.Name false false true

/// An enemy killed by damage over time or recorded damage coming due: the player's kill
/// (`mine`) sets off the player's rules for kills; what a kill gives follows.
let killedOverTime env rng name mine (w: World) =
    let player = env.Fight.Player
    let w =
        if not mine then w
        else
            let w = react env rng player OnKill dotKiller (Some name) w
            status w.Enemies.[name] |> Map.fold (fun w k _ -> react env rng player (OnKillOf k) dotKiller (Some name) w) w
    dotKillGains env rng w.Enemies.[name] mine w
