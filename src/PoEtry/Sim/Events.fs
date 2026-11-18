/// The event loop's one step: take the next event from the queue and let it happen.
[<AutoOpen>]
module internal PoEtry.Engine.Events

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

let stepEvent env rng (w: World) =
    let (KeyValue ((t, sq), ev)) = Seq.head w.Queue
    let dt = t - w.Time
    (match env.Trace with
     | Some tell ->
         match ev with
         | Ready a -> tell t "Ready" a "" (float sq)
         | Strike (a, s, _, _) -> tell t "Strike" a s (float sq)
         | PassingStrike (a, s, _, _, _) -> tell t "PassingStrike" a s (float sq)
         | DirectHit (a, s, _, _) -> tell t "DirectHit" a s (float sq)
         | ConsumeCharges (a, c, _) -> tell t "ConsumeCharges" a c (float sq)
         | CycleTick (a, c) -> tell t "CycleTick" a c (float sq)
         | EnemyAttack e -> tell t "EnemyAttack" e "" (float sq)
         | ReviveMinions _ -> tell t "ReviveMinions" "" "" (float sq)
         | MinionLeaves (a, _) -> tell t "MinionLeaves" a "" (float sq)
         | Landed (a, s, _) -> tell t "Landed" a s (float sq)
         | PeriodicTick (a, p) -> tell t "PeriodicTick" a p (float sq)
         | WardArrives _ -> tell t "WardArrives" "" "" (float sq)
         | ManaArrives _ -> tell t "ManaArrives" "" "" (float sq)
         | GainStacks (a, r, _) -> tell t "GainStacks" a r (float sq)
         | Expose (a, e, _, _, _) -> tell t "Expose" a e (float sq)
         | Explosion (a, e, _, _, _) -> tell t "Explosion" a e (float sq)
         | PlantCrystals (a, p, _, _, _, _, _) -> tell t "PlantCrystals" a p (float sq)
         | SpendBuff (a, b) -> tell t "SpendBuff" a b (float sq)
         | EnergyArrives (a, m, _) -> tell t "EnergyArrives" a m (float sq)
         | Expire -> tell t "Expire" "" "" (float sq)
     | None -> ())
    // What a skill's hits roll comes from a stream of that skill alone (who strikes with
    // what: its first event, its second, ...), not from the one stream of the fight: its
    // tenth strike rolls the same whatever else happened before. Two fights of one seed
    // that differ a little (a passive more) then differ by what that does, not by luck.
    let rng, w =
        let key =
            match ev with
            | Strike (a, s, _, _) | PassingStrike (a, s, _, _, _) | DirectHit (a, s, _, _) -> "S|" + a + "|" + s
            | ConsumeCharges (a, _, s) -> "C|" + a + "|" + s
            | EnemyAttack e -> "E|" + e
            | PeriodicTick (a, p) -> "P|" + a + "|" + p
            | _ -> ""
        if key = "" then rng, w
        else
            let n = Map.tryFind key w.Streams |> Option.defaultValue 0
            Rng(streamSeed w.Seed key n), { w with Streams = Map.add key (n + 1) w.Streams }
    let w = { w with Queue = Map.remove (t, sq) w.Queue }
    let killsBefore = w.Kills.Length
    let w = advance env t w
    // the player's kills by damage over time set off the player's rules for kills, as by a
    // skill that is neither attack nor spell, and give what a kill gives (`dotKillGains`)
    let w =
        let n = w.Kills.Length - killsBefore
        if n <= 0 then w
        else
            w.Kills |> List.truncate n |> List.rev
            |> List.fold (fun w (name, _, at) -> killedOverTime env rng name (playersDotKill env w.Enemies.[name] at) w) w
    let w = w |> syncPresence env.Fight |> syncGround env
    let rates = match env.Fight.Player.Vitals with Some v when not v.RatesVary -> Some v | _ -> vitalsNow env w
    let w = w |> recover env.Fight.Player.Vitals rates env.Fight.Player.ManaFlask env.Fight.Player.Charms dt
    if allDead w || (w.Player |> Option.exists (fun p -> p.Died.IsSome)) then
        // (on a march the field is empty between a pack and the next: the fight is not
        // over, and the player's turn comes again; what else was due is lost as ever)
        match ev, env.Fight.March with
        | Ready name, Some m when name = env.Fight.Player.Name && w.Packs <= m.Packs && (w.Player |> Option.forall (fun p -> p.Died.IsNone)) ->
            schedule (w.Time + Constants.serverTick) (Ready name) w
        | _ -> w
    else
        let w = prune w
        // seeds: set off by the damage their enemy has taken (or its death: the blast then
        // strikes the enemy aimed at), gone when their time is up
        let w =
            if w.Bombs.IsEmpty then w
            else
                w.Bombs |> List.fold (fun w ((owner, skill, enemy, mark, needed, until) as bomb) ->
                    let drop w = { w with Bombs = w.Bombs |> List.filter (fun b -> b <> bomb) }
                    match Map.tryFind enemy w.Enemies with
                    | Some e when e.Life > 0.0 && mark - e.Life >= needed ->
                        drop w |> schedule w.Time (Strike(owner, skill, 1.0, enemy))
                    | Some e when e.Life > 0.0 ->
                        if w.Time >= until then { drop w with Counts = bump ("fizzled:" + skill) w.Counts } else w
                    | _ ->
                        match aimed env w with
                        | Some other -> drop w |> schedule w.Time (Strike(owner, skill, 1.0, other.Def.Name))
                        | None -> drop w) w
        // pustules: burst when their time is up or they hold all the Poison they can use
        let w =
            if w.Pustules.IsEmpty then w
            else
                w.Pustules |> List.fold (fun w ((owner, part, enemy, mark, per, cap, until) as pustule) ->
                    let drop w = { w with Pustules = w.Pustules |> List.filter (fun x -> x <> pustule) }
                    match Map.tryFind enemy w.Enemies with
                    | Some e when e.Life > 0.0 ->
                        let stored = (Map.tryFind "Poison" w.DamageBy |> Option.defaultValue 0.0) - mark
                        let more = if per > 0.0 then min cap (stored / per) else 0.0
                        if w.Time >= until || (cap > 0.0 && more >= cap) then drop w |> schedule w.Time (Strike(owner, part, 1.0 + more / 100.0, enemy)) else w
                    | _ -> drop w) w
        // Ice Crystals: destroyed once the enemy beside them has lost as much life as one
        // withstands (the damage dealt there since they appeared); each then explodes
        let w =
            if w.Crystals.IsEmpty then w
            else
                w.Crystals |> List.fold (fun w ((owner, part, enemy, mark, life, count, until) as c) ->
                    let drop w = { w with Crystals = w.Crystals |> List.filter (fun x -> x <> c) }
                    match Map.tryFind enemy w.Enemies with
                    | Some e when e.Life > 0.0 && mark - e.Life >= life ->
                        [ 1 .. count ] |> List.fold (fun w _ -> schedule w.Time (Strike(owner, part, 1.0, enemy)) w) (drop w)
                        |> fun w -> { w with Counts = bump ("shattered:" + part) w.Counts }
                    | Some e when e.Life > 0.0 && w.Time < until -> w
                    | _ -> drop w) w
        // recorded damage is taken again as life when the debuff ends, or as soon as that kills
        let w =
            if w.Agonies.IsEmpty then w
            else
                // (whose debuff killed which enemy, in order)
                let killed = ResizeArray<string * string>()
                w.Agonies |> List.fold (fun w ((owner, skill, enemy, mark, share, until) as a) ->
                    let drop w = { w with Agonies = w.Agonies |> List.filter (fun x -> x <> a) }
                    match Map.tryFind enemy w.Enemies with
                    | Some e when e.Life > 0.0 ->
                        let owed = share * mark
                        if w.Time >= until || owed >= e.Life then
                            let dealt = min owed e.Life
                            let dead = owed >= e.Life
                            if dead then killed.Add((enemy, owner))
                            { drop w with
                                Enemies = Map.add enemy { e with Life = (if dead then 0.0 else e.Life - dealt) } w.Enemies
                                DamageBy = addf skill dealt w.DamageBy
                                DamageByType = addf "Other" dealt w.DamageByType
                                CorpsesUsed = corpsesAfter dead enemy w.Time w
                                Kills = if dead then (enemy, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
                        else w
                    | _ -> drop w) w
                // what they killed, in order: the player's kill when the debuff was the player's
                |> fun w ->
                    killed |> Seq.fold (fun w (name, owner) -> killedOverTime env rng name (owner = env.Fight.Player.Name) w) w
        // Demon Form: a Demonflame for every second in the form (at most 10), life lost for
        // each; at 1 life the form ends and its cooldown starts
        let w =
            let name = env.Fight.Player.Name
            let st = Map.find name w.Actors
            match Map.tryFind "Demon Form" st.Buffs, w.Player with
            | Some form, Some p ->
                let value stat = form.Mods |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
                let flames = min 10 (int (w.Time - (form.Expires - 1e9)))
                // the life lost since the last event, at the Demonflame held then
                let before = max 0 (min 10 (int (w.Time - dt - (form.Expires - 1e9))))
                let life = p.Life - value "DemonLifeLossPerStack" * float before * max 0.0 dt
                if life <= 1.0 then
                    let skill = form.Mods |> List.tryPick (fun m -> if m.Source <> "" then Some m.Source else None) |> Option.defaultValue ""
                    let back = w.Time + value "DemonCooldown"
                    { w with Player = Some { p with Life = min p.Life 1.0 }; Counts = bump "demon form ended" w.Counts }
                    |> updateActor name (fun st ->
                        { st with
                            Buffs = st.Buffs |> Map.remove "Demon Form" |> Map.remove "Demonflame"
                            Cooldowns = Map.add skill back st.Cooldowns
                            Recharges = Map.add skill [ back ] st.Recharges })
                else
                    { w with Player = Some { p with Life = life } }
                    |> updateActor name (fun st ->
                        if flames > 0 then { st with Buffs = Map.add "Demonflame" { Name = "Demonflame"; Mods = []; Stacks = flames; Expires = infinity } st.Buffs } else st)
            | _ -> w
        // rage: regenerated all the time, lost once none has been gained for a while
        let w =
            let regen, loss, delay, _, _ = env.Rage
            let name = env.Fight.Player.Name
            let st = Map.find name w.Actors
            let have = stacksOf st "Rage"
            let maxRage = Map.tryFind "Rage" env.Fight.Player.MaxResources |> Option.defaultValue 0
            if dt <= 0.0 || maxRage <= 0 || (regen <= 0.0 && have = 0) then w
            else
                // Rage is lost from when the delay is over: of dt, the part after it
                let losingFor = max 0.0 (w.Time - max (w.Time - dt) (st.RageAt + delay))
                // Berserk: life lost for every Rage held while Rage is not being lost (not below 1)
                let berserkFor = if loss <= regen then dt else dt - losingFor
                let w =
                    match env.Fight.Player.Vitals, w.Player with
                    | Some v, Some p when berserkFor > 0.0 && have > 0 && env.BerserkLoss > 0.0 ->
                        let life = max (min p.Life 1.0) (p.Life - env.BerserkLoss / 100.0 / 60.0 * float have * v.Life * berserkFor)
                        { w with Player = Some { p with Life = life; MinLife = min p.MinLife (life / max 1.0 v.Life) } }
                    | _ -> w
                let carry = st.RageCarry + regen * dt - loss * losingFor
                let whole = int (System.Math.Truncate carry)
                let n = max 0 (min maxRage (have + whole))
                let w = if n <> have then setStacksWith env name false "Rage" n w else w
                updateActor name (fun st -> { st with RageCarry = (if n = 0 && carry < 0.0 then 0.0 elif n = maxRage && carry > 0.0 then 0.0 else carry - float whole) }) w
        // the player's life and mana state, as buffs conditions can read
        // "Your Life cannot change while you have Energy Shield": no recovery above where it started
        let w =
            match env.Fight.Player.Vitals, w.Player with
            | Some v, Some p when v.LifeFrozenWithES && p.EnergyShield > 0.0 && p.Life > v.StartLife * v.Life ->
                { w with Player = Some { p with Life = v.StartLife * v.Life } }
            // reserved life: what is left is the most there can be
            | Some v, Some p when v.LifeReserved > 0.0 && p.Life > (1.0 - v.LifeReserved) * v.Life ->
                { w with Player = Some { p with Life = (1.0 - v.LifeReserved) * v.Life } }
            | _ -> w
        let w =
            match env.Fight.Player.Vitals, w.Player with
            | Some v, Some p ->
                let states =
                    [ "FullLife", p.Life >= v.Life
                      "LowLife", v.Life > 1.0 && p.Life <= 0.35 * v.Life
                      "FullMana", p.Mana >= v.Mana
                      "LowMana", p.Mana <= 0.35 * v.Mana
                      "HasEnergyShield", p.EnergyShield > 0.0
                      "FullEnergyShield", v.EnergyShield > 0.0 && p.EnergyShield >= v.EnergyShield
                      // a flask drunk and still working (a constant one is a resource the fight starts with)
                      "ManaFlaskEffect", (w.Time < p.FlaskUntil || stacksOf (Map.find env.Fight.Player.Name w.Actors) "ConstantManaFlask" > 0)
                      "LifeFlaskEffect", (w.Time < p.LifeFlaskUntil || stacksOf (Map.find env.Fight.Player.Name w.Actors) "ConstantLifeFlask" > 0)
                      "FlaskEffect",
                      (w.Time < p.FlaskUntil || w.Time < p.LifeFlaskUntil
                       || (let st = Map.find env.Fight.Player.Name w.Actors in stacksOf st "ConstantManaFlask" > 0 || stacksOf st "ConstantLifeFlask" > 0))
                      "Leeching", p.Recoups |> List.exists (fun (until, _, pool) -> pool.StartsWith "Leech" && until > w.Time)
                      // surrounded: at least 5 monsters within 3 m (PoB SurroundedRadiusBase, game constant)
                      "Surrounded", (alive w |> List.filter (fun e -> Vec.length e.Def.Position - e.Def.Size <= v.SurroundedRadius) |> List.length) >= max 1 v.SurroundedAt
                      // the player's own ailments: Ignite is the only damaging one a monster's hit or the
                      // character itself puts on it (Shocked, Chilled, Frozen, Stunned are buffs of their own)
                      "Ignited", p.Dots |> List.exists (fun (until, _, t) -> t = Fire && until > w.Time)
                      "DamagingAilment", p.Dots |> List.exists (fun (until, _, t) -> t = Fire && until > w.Time)
                      // on Low Energy Shield: at most 35% of its maximum (DefaultLowStatusThresholdPercent); none without any
                      "LowEnergyShield", v.EnergyShield > 0.0 && p.EnergyShield <= Constants.lowStatusShare * v.EnergyShield
                      // energy shield recharge started in the past 2 seconds: its delay since the last
                      // hit (or damage over time) is over, by no more than 2 s
                      "ESRechargeStartedRecently", env.AsksESRecharge && v.EnergyShield > 0.0 && w.Time - p.LastHit >= v.ESRechargeDelay && w.Time - p.LastHit < v.ESRechargeDelay + Constants.esRechargeStartedRecently ]
                // charge states: "while at maximum Power Charges", "while you have no Frenzy Charges"
                let playerSt = Map.find env.Fight.Player.Name w.Actors
                let states =
                    states
                    @ [ for kind in [ "Power"; "Frenzy"; "Endurance" ] do
                            let n = Map.tryFind (kind + "Charge") playerSt.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                            let maxN = Map.tryFind (kind + "Charge") env.Fight.Player.MaxResources |> Option.defaultValue 0
                            yield "AtMaximum" + kind + "Charges", maxN > 0 && n >= maxN
                            yield "No" + kind + "Charges", n = 0
                            yield "YouHaveNo" + kind + "Charges", n = 0 ]
                updateActor env.Fight.Player.Name (fun st ->
                    { st with
                        Buffs =
                            states |> List.fold (fun (b: Map<string, Effect>) (k, on) ->
                                if on then (if b.ContainsKey k then b else Map.add k { Name = k; Mods = []; Stacks = 1; Expires = infinity } b)
                                else Map.remove k b) st.Buffs }) w
            | _ -> w
        let gone name = Map.tryFind name w.Actors |> Option.exists (fun st -> st.Gone)
        match ev with
        // a minion that is out of the fight does nothing, and what it had begun is lost
        // (but what it has shot or thrown still arrives: `outliving`)
        | Ready name when gone name -> w
        | Strike (name, skill, _, _) | PassingStrike (name, skill, _, _, _) | DirectHit (name, skill, _, _) when gone name && not ((outliving (actorDef env name)).Contains skill) -> w
        | Ready name -> onReady env rng name w
        | Strike (name, skill, mult, aimedAt) -> onStrike env rng name skill mult aimedAt None false w
        | PassingStrike (name, skill, mult, aimedAt, elapsed) -> onStrike env rng name skill mult aimedAt (Some elapsed) false w
        | DirectHit (name, skill, mult, enemy) -> onStrike env rng name skill mult enemy None true w
        | EnemyAttack name -> enemyAttack env rng name w
        | ReviveMinions at -> reviveMinions env at w
        | MinionLeaves (name, at) ->
            let st = Map.find name w.Actors
            if st.Gone || st.Leaves <> at then w else { leave env name w with Counts = bump "minion:left" w.Counts }
        | Landed (name, skill, aimedAt) ->
            let actor = actorDef env name
            react env rng actor OnLand (skillOf actor skill) (Some aimedAt) w
        | PeriodicTick (name, periodic) -> onPeriodic env rng name periodic w
        | ManaArrives amount ->
            match env.Fight.Player.Vitals, env.Fight.Player.ManaRemnants, w.Player with
            | Some v, Some r, Some p -> { w with Player = Some { p with Mana = min (max p.Mana (v.Mana * r.Overflow)) (p.Mana + amount * v.ManaRecoveryRate) } }
            | _ -> w
        | WardArrives amount ->
            match env.Fight.Player.Vitals, env.Fight.Player.WardRemnants, w.Player with
            | Some v, Some r, Some p -> { w with Player = Some { p with Ward = min (v.Ward * r.Overflow) (p.Ward + amount) } }
            | _ -> w
        | CycleTick (name, cycleName) ->
            let actor = actorDef env name
            match actor.Cycles |> List.tryFind (fun c -> c.Name = cycleName) with
            | None -> w
            | Some c ->
                let pick = min (c.Variants.Length - 1) (int (rng.Float() * float c.Variants.Length))
                let e = { Name = c.Name; Mods = c.Variants.[pick]; Stacks = 1; Expires = w.Time + c.Period }
                updateActor name (fun st -> { st with Buffs = Map.add c.Name e st.Buffs }) w
                |> fun w -> { w with Counts = bump (sprintf "cycle:%s:%d" c.Name pick) w.Counts }
                |> schedule (w.Time + c.Period) (CycleTick(name, cycleName))
        | ConsumeCharges (name, charge, skillName) ->
            let actor = actorDef env name
            let st = Map.find name w.Actors
            // character + skill (gem quality, supports): the skill's own db
            let skip = Mods.sumBase Ctx.empty [ Named "ChargeSkipConsume" ] (skillDb env actor (skillOf actor skillName) st)
            let skip = if (skillOf actor skillName).Mods |> List.exists (fun m -> m.Stat = Named "FlickersPerPowerCharge") then min Constants.flickerKeepCap skip else skip
            // a channel that takes a charge for so long of it: each use takes one with the
            // chance its own time is of that (the same charges per second, at random moments)
            let every = Mods.sumBase Ctx.empty [ Named "ConsumeChargeEveryMs" ] (skillDb env actor (skillOf actor skillName) st) / 1000.0
            let spared = every > 0.0 && not (rng.Chance(100.0 * min 1.0 (useTime env actor (skillOf actor skillName) w / every)))
            if not (Map.containsKey charge st.Buffs) || spared then w
            else
                // "still count as consuming": the consumed-recently status starts either way
                let recent = "Consumed" + charge + "Recently"
                let w =
                    updateActor name (fun st -> { st with Buffs = Map.add recent { Name = recent; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
                    |> schedule (w.Time + 4.0) Expire
                // "Recover N% of maximum Mana when you consume a Power Charge": once for a use
                // that consumes them, however many, and whether they are kept or not (user,
                // in game: "happens but only once per Falling Thunder or Charged Staff")
                let w =
                    match actor.Vitals with
                    | Some v when charge = "PowerCharge" ->
                        gainMana v "consume" (v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnConsumePower" ] (ModDb.ofList actor.Mods) / 100.0) w
                    | _ -> w
                if rng.Chance skip then
                    let w = { w with Counts = bump ("kept:" + charge) w.Counts }
                    if actor.KeptChargesPause > 0.0 then
                        // freeze the timer: push expiry back by the part of the pause that
                        // doesn't overlap an earlier one
                        let st = Map.find name w.Actors
                        let pausedUntil = Map.tryFind charge st.Paused |> Option.defaultValue w.Time
                        let newUntil = w.Time + actor.KeptChargesPause
                        let extension = max 0.0 (newUntil - max w.Time pausedUntil)
                        match Map.tryFind charge st.Buffs with
                        | Some e when e.Expires < infinity ->
                            let e = { e with Expires = e.Expires + extension }
                            updateActor name (fun st -> { st with Buffs = Map.add charge e st.Buffs; Paused = Map.add charge newUntil st.Paused }) w
                            |> schedule e.Expires Expire
                        | _ -> w
                    else w
                else
                    // one charge, a set number, or all of them (0)
                    let count = int (Mods.sumBase Ctx.empty [ Named "ConsumeChargeCount" ] (skillDb env actor (skillOf actor skillName) st))
                    let had = match Map.tryFind charge st.Buffs with Some e -> e.Stacks | None -> 0
                    let w = if charge = "EnduranceCharge" then enduranceLife actor (if count > 0 && had > count then count else had) w else w
                    updateActor name (fun st ->
                        match Map.tryFind charge st.Buffs with
                        | Some e when count > 0 && e.Stacks > count -> { st with Buffs = Map.add charge { e with Stacks = e.Stacks - count } st.Buffs }
                        | _ -> { st with Buffs = Map.remove charge st.Buffs }) w
                    |> fun w -> { w with Counts = bump ("consumed:" + charge) w.Counts }
        | PlantCrystals (owner, part, enemy, count, life, limit, lasts) ->
            match Map.tryFind enemy w.Enemies with
            | Some e when e.Life > 0.0 ->
                // more than the limit: the oldest go (without exploding)
                let all = (owner, part, enemy, e.Life, life, count, w.Time + lasts) :: w.Crystals
                let kept, _ =
                    all |> List.fold (fun (kept, n) ((o, p, _, _, _, c, _) as x) ->
                        if o = owner && p = part then (if n + c <= max count limit then x :: kept, n + c else kept, n + c) else x :: kept, n) ([], 0)
                { w with Crystals = List.rev kept; Counts = bump ("crystals:" + part) w.Counts }
            | _ -> w
        | Expose (name, target, dt, amount, duration) -> expose (actorDef env name) target dt amount duration w
        | Explosion (name, dead, percent, dt, radius) ->
            let d = w.Enemies.[dead]
            let amount = d.Def.Life * percent / 100.0
            let killed = ResizeArray<string>()
            let w =
                alive w
                |> List.filter (fun o -> o.Def.Name <> dead && Vec.dist o.Def.Position d.Def.Position <= radius + o.Def.Size)
                |> List.fold (fun (w: World) o ->
                    let e = w.Enemies.[o.Def.Name]
                    let dealt = Hit.mitigate Ctx.empty ModDb.empty (defender e) (Map.ofList [ dt, amount ]) |> Map.find dt
                    let took = min dealt e.Life
                    let dies = dealt >= e.Life
                    if dies then killed.Add e.Def.Name
                    { w with
                        Enemies = Map.add e.Def.Name { e with Life = (if dies then 0.0 else e.Life - took) } w.Enemies
                        DamageBy = addf "Explosion" took w.DamageBy
                        DamageByType = addf (DamageType.name dt) took w.DamageByType
                        Counts = bump "hits:Explosion" w.Counts
                        CorpsesUsed = corpsesAfter dies e.Def.Name w.Time w
                        Kills = if dies then (e.Def.Name, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }) { w with Counts = bump "explosion" w.Counts }
            killed |> Seq.fold (fun w k -> killedOverTime env rng k (name = env.Fight.Player.Name) w) w
        // Conduit: "If you would gain a Charge, Allies in your Presence gain that Charge
        // instead": the actor gains nothing (what an ally holds, no minion reads)
        | GainStacks (name, r, _) when r = "PowerCharge" && ((Map.find name w.Actors).Energy |> Map.tryFind "PowerChargeLock" |> Option.exists (fun t -> w.Time <= t)) ->
            { w with Counts = bump "barred:PowerCharge" w.Counts }
        | GainStacks (name, r, _) when conduit (actorDef env name) r ->
            { w with Counts = bump ("conduit:" + r) w.Counts }
        | GainStacks (name, r, n) ->
            let actor = actorDef env name
            let st = Map.find name w.Actors
            let maxR = Map.tryFind r actor.MaxResources |> Option.defaultValue n
            // "N% chance to gain an additional (Power) Charge when you gain a Charge"
            let n =
                if not (r.EndsWith "Charge") then n
                else
                    match actor.Skills |> List.tryHead with
                    | Some sk ->
                        let db = skillDb env actor sk st
                        let chance = Mods.sumBase Ctx.empty [ Named("ExtraChargeChance:" + r); Named "ExtraChargeChance:Any" ] db
                        if chance > 0.0 && rng.Chance chance then n + 1 else n
                    | None -> n
            setStacksWith env name true r (min maxR (stacksOf st r + n)) w
            |> (if r.EndsWith "Charge" then markStatus name ("Gained" + r + "Recently") else id)
            |> fun w -> { w with Counts = bump ("gained:" + r) w.Counts }
        | EnergyArrives (name, meta, amount) -> gainEnergy env rng (actorDef env name) meta amount w
        | SpendBuff (name, buff) ->
            updateActor name (fun st ->
                match Map.tryFind buff st.Buffs with
                | Some e when e.Stacks > 1 -> { st with Buffs = Map.add buff { e with Stacks = e.Stacks - 1 } st.Buffs }
                | _ -> { st with Buffs = Map.remove buff st.Buffs }) w
        | Expire -> w

/// The next event, and then the ground again: what the event left or lit is there at once.
let step env rng (w: World) =
    let w = stepEvent env rng w
    if w.Grounds.IsEmpty then w else syncGround env w
