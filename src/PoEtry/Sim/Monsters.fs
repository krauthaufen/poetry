/// Monsters hitting back, at the player or the minions; minions dying and coming back.
[<AutoOpen>]
module internal PoEtry.Engine.Monsters

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// When killed minions come back: all together, `seconds` after the first death. Each
/// further death while they wait adds less: the n-th adds a n-th of the time, and the wait
/// is never more than the full time from now ("each successive minion that dies increases
/// it by less and less (still capped to a max of 7.5 seconds)", 0.2.0e patch notes; the
/// n-th share is the wiki's). Before 0.2.0e every death started the full wait anew.
/// `waiting`: the time set so far and the deaths counted.
let nextRevival (now: float) (seconds: float) (waiting: (float * int) option) : float * int =
    match waiting with
    | Some (at, n) when at > now -> min (now + seconds) (at + seconds / float (n + 1)), n + 1
    | _ -> now + seconds, 1

/// A minion is killed: it leaves the fight; a reviving one comes back with the others
/// once none has died for its revive time.
let killMinion env (rng: Rng) (minion: Actor) (w: World) =
    let body = minion.Body.Value
    let player = env.Fight.Player
    let w = leave env minion.Name w
    let w = { w with Counts = bump "minion:died" w.Counts } |> markStatus player.Name "MinionDiedRecently"
    match body.Revive with
    | None -> w
    | Some seconds ->
        // "gains Energy when one of your Persistent Minions is Killed"
        let w =
            player.Metas
            |> List.fold
                (fun w meta ->
                    let amount = meta.Sources |> List.sumBy (fun src -> match src with MinionDeaths per -> per * body.Power | _ -> 0.0)
                    if amount > 0.0 then gainEnergy env rng player meta.Name (amount * meta.EnergyMult) w else w)
                w
        let at, n = nextRevival w.Time seconds w.Revival
        { w with Revival = Some(at, n) } |> schedule at (ReviveMinions at)

let reviveMinions env (at: float) (w: World) =
    match w.Revival with
    | Some (t, _) when abs (t - at) < 1e-9 ->
        env.Fight.Minions
        |> List.fold
            (fun w m ->
                match m.Body with
                | Some b when b.Revive.IsSome && (Map.find m.Name w.Actors).Gone ->
                    updateActor m.Name (fun st -> { st with Gone = false; Life = b.Life; Shield = b.EnergyShield; HitAt = w.Time }) w
                    |> schedule w.Time (Ready m.Name)
                    |> fun w -> { w with Counts = bump "minion:revived" w.Counts }
                | _ -> w)
            { w with Revival = None; Boned = Set.empty }
    | _ -> w

/// Mitigated damage onto a minion: energy shield first (chaos removes twice as much),
/// then life. Before that it has recovered since it was last hit: its life regeneration,
/// and its energy shield recharge once the delay has passed.
let hurtMinion env (rng: Rng) (minion: Actor) (damage: Map<DamageType, float>) (w: World) =
    let body = minion.Body.Value
    let st = Map.find minion.Name w.Actors
    let since = w.Time - max 0.0 st.HitAt
    let life = min body.Life (st.Life + body.LifeRegen * since)
    let shield =
        let recharging = since - Constants.energyShieldRechargeDelay
        if recharging > 0.0 then min body.EnergyShield (st.Shield + body.EnergyShield * Constants.energyShieldRechargePercent / 100.0 * recharging) else st.Shield
    let shield, rest =
        damage
        |> Map.fold
            (fun (es, rest) t d ->
                let k = if t = Chaos then 2.0 else 1.0
                let absorbed = min d (es / k)
                es - absorbed * k, rest + d - absorbed)
            (shield, 0.0)
    let life = life - rest
    let w = updateActor minion.Name (fun st -> { st with Life = life; Shield = shield; HitAt = w.Time }) w
    match body.Explodes, aimed env w with
    // "explode when reduced to Low Life" (35%): its explosion strikes, and it is dead
    | Some skill, Some enemy when life <= 0.35 * body.Life ->
        onStrike env rng minion.Name skill 1.0 enemy.Def.Name None false { w with Counts = bump "minion:exploded" w.Counts }
        |> killMinion env rng minion
    | _ -> if life <= 0.0 then killMinion env rng minion w else w

/// A monster's hit at the player: evasion, deflection, armour, resistances, then
/// energy shield and life. Frozen / stunned / electrocuted monsters don't act; chill
/// slows them; blind lowers their accuracy.
let attackPlayer env (rng: Rng) name (w: World) =
    let e = w.Enemies.[name]
    match e.Def.Attack, env.Fight.Player.Vitals, w.Player with
    | Some atk, Some pools, Some p when e.Life > 0.0 && p.Died.IsNone ->
        // mitigation from the player's current buffs and charges
        let v =
            match env.Fight.Player.VitalsAt with
            | Some at -> at (buffStacks (Map.find env.Fight.Player.Name w.Actors))
            | None -> pools
        let st = status e
        let stopped = e.StunnedUntil > w.Time || [ "Frozen"; "HeavilyStunned"; "Electrocuted" ] |> List.exists st.ContainsKey
        let chill = e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
        // curses on the monster: action speed (Temporal Chains), damage dealt (Enfeeble)
        let debuffs = (defender e).Mods
        let actionSpeed = max 0.1 (Mods.more Ctx.empty [ Named "ActionSpeed" ] debuffs)
        let dealt = max 0.0 (Mods.more Ctx.empty [ Named "DamageDealt" ] debuffs)
        let atk = { atk with Damage = atk.Damage |> Map.map (fun _ d -> d * dealt) |> Vitals.withExtraChaos (Mods.sumBase Ctx.empty [ Named "GainAsChaos" ] debuffs) }
        let next = (if (1.0 - chill / 100.0) * actionSpeed < Constants.slowFloor then atk.Interval / Constants.slowFloor else atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed)
        let w = schedule (w.Time + next) (EnemyAttack name) w
        let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
        if stopped then
            { w with Counts = bump "enemy:stopped" w.Counts }
        elif not inRange then
            w
        else
            let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0) * (if st.ContainsKey "Marked" then v.MarkedEnemyAccuracyMult else 1.0)
            let hitChance =
                let h = Vitals.monsterHitChance v.Evasion accuracy
                // Glancing Blows: the evade roll is unlucky (both of two rolls must evade)
                if v.UnluckyEvade then 100.0 - (100.0 - h) * (100.0 - h) / 100.0 else h
            let landed, w = lands rng "Player" hitChance w
            if not landed then
                { w with Counts = bump "enemy:evaded" w.Counts }
            else
                // "Recover X% of Missing Life before being Hit by an Enemy"
                let w =
                    match w.Player with
                    | Some p when v.MissingLifeBeforeHit > 0.0 && p.Life < v.Life -> { w with Player = Some { p with Life = p.Life + (v.Life - p.Life) * v.MissingLifeBeforeHit / 100.0 } }
                    | _ -> w
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
                let crit = rng.Chance(atk.CritChance * v.EnemyCritChanceMult * (if st.ContainsKey "Blinded" then v.BlindedEnemyCritMult else 1.0) * (if st.ContainsKey "Marked" then v.MarkedEnemyCritMult else 1.0)) && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
                // "Defend with X% of Armour against Projectile Attacks / Critical Hits"
                let armourShare = (if atk.Melee then 1.0 else v.ArmourVsProjectiles) * (if crit then v.ArmourVsCrits else 1.0) * v.ArmourDefend
                let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 * (1.0 - v.LessCritDamageTaken / 100.0) * (if st.ContainsKey "Blinded" then v.BlindedEnemyCritBonusMult else 1.0) else 1.0
                let playerSt = Map.find env.Fight.Player.Name w.Actors
                let shocked =
                    Map.tryFind "Shocked" playerSt.Buffs |> Option.filter (fun x -> x.Expires > w.Time)
                    |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
                let unmitigated = incoming |> Map.map (fun _ d -> d * critMult)
                let taken =
                    unmitigated
                    |> Map.map (fun t d ->
                        let d = if deflected then d * (1.0 - v.DeflectEffect / 100.0) else d
                        let d =
                            match t with
                            | Physical ->
                                let d = if atk.Melee then max 0.0 (d - v.FlatPhysicalTaken) else d
                                Vitals.physicalLeft v (v.Armour * armourShare) (match w.Player with Some p -> p.EnergyShield | None -> 0.0) d
                            | t ->
                                let d = d * (1.0 - (Map.tryFind t v.Resist |> Option.defaultValue 0.0) / 100.0)
                                // "X% of Armour also applies to <type> damage"
                                match Map.tryFind t v.ArmourAppliesTo with
                                | Some pct -> d * (1.0 - Vitals.armourReduction (v.Armour * armourShare * pct / 100.0) d / 100.0)
                                | None -> d
                        d * (Map.tryFind t v.TakenMult |> Option.defaultValue 1.0) * (1.0 + shocked / 100.0)
                          * (if blocked then v.BlockedDamageTaken / 100.0 else 1.0))
                // Scarred Faith: "5% of Physical Damage prevented Recouped as Energy Shield per
                // enemy Power": what mitigation kept of the hit's physical damage
                let preventedRecoup =
                    if v.RecoupPreventedPerPower <= 0.0 then 0.0
                    else
                        let prevented = max 0.0 ((Map.tryFind Physical unmitigated |> Option.defaultValue 0.0) - (Map.tryFind Physical taken |> Option.defaultValue 0.0))
                        prevented * v.RecoupPreventedPerPower / 100.0 * MonsterPower.ofRarity e.Def.Rarity * v.ESRecoveryRate
                // "X% of Physical Damage prevented Recouped as Life"
                let preventedLife =
                    if v.RecoupPreventedLife <= 0.0 then 0.0
                    else
                        let prevented = max 0.0 ((Map.tryFind Physical unmitigated |> Option.defaultValue 0.0) - (Map.tryFind Physical taken |> Option.defaultValue 0.0))
                        prevented * v.RecoupPreventedLife / 100.0 * v.LifeRecoveryRate
                // "x% of Damage from Hits is taken from your Companion's Life before you": the
                // first minion that does so and is alive takes its share, as much as it has
                let taken, w =
                    let guard =
                        env.Fight.Minions |> List.tryFind (fun m ->
                            (m.Body |> Option.exists (fun b -> b.TakesForOwner > 0.0)) && not (Map.find m.Name w.Actors).Gone)
                    match guard with
                    | Some m ->
                        let share = m.Body.Value.TakesForOwner / 100.0
                        let its = Map.find m.Name w.Actors
                        let total = taken |> Map.toList |> List.sumBy snd
                        let k = if total <= 0.0 then 0.0 else min share ((its.Life + its.Shield) / total)
                        taken |> Map.map (fun _ d -> d * (1.0 - k)),
                        hurtMinion env rng m (taken |> Map.map (fun _ d -> d * k)) { w with Counts = bump "minion:took for owner" w.Counts }
                    | None -> taken, w
                let p = w.Player |> Option.defaultValue p
                let w = if w.Tracing then { w with Shown = Hurt(w.Time, name, taken |> Map.fold (fun s _ v -> s + v) 0.0) :: w.Shown } else w
                let w = takeDamage true pools taken { w with Player = Some p } |> fun w -> { w with Player = w.Player |> Option.map (fun p -> { p with LastHit = w.Time }) }
                let w =
                    if preventedRecoup > 0.0 then
                        { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + v.RecoupSeconds, preventedRecoup / v.RecoupSeconds, "ES") :: p.Recoups }) }
                    else w
                let w =
                    if preventedLife > 0.0 then
                        { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + v.RecoupSeconds, preventedLife / v.RecoupSeconds, "Life") :: p.Recoups }) }
                    else w
                let w = (let _, _, _, whenHit, _ = env.Rage in gainRage env whenHit w)
                let w = (match env.Fight.Player.Skills |> List.tryHead with Some any -> react env rng env.Fight.Player WhenHit any (Some name) w | None -> w)
                let w = (match env.Fight.Player.Skills |> List.tryHead with Some any when atk.Melee -> react env rng env.Fight.Player WhenHitMelee any (Some name) w | _ -> w)
                // "Rage loss is paused for 4 seconds upon gaining Rage, or after taking damage"
                let w = updateActor env.Fight.Player.Name (fun st -> { st with RageAt = w.Time }) w
                // charms used when a hit of their damage type is taken (they help from the next hit on)
                let w = taken |> Map.fold (fun w t d -> if d > 0.0 then fst (charm env rng ("Hit:" + DamageType.name t) w) else w) w
                let w = if crit then { w with Counts = bump "enemy:crit" w.Counts } else w
                let w =
                    updateActor env.Fight.Player.Name (fun st ->
                        { st with Buffs = Map.add "BeenHitRecently" { Name = "BeenHitRecently"; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
                    |> schedule (w.Time + 4.0) Expire
                // ailments and stuns the hit inflicts on the player (not when blocked)
                let w =
                    if blocked then w
                    else
                        let get t = Map.tryFind t unmitigated |> Option.defaultValue 0.0
                        let avoided a = rng.Chance(Map.tryFind a v.Avoid |> Option.defaultValue 0.0)
                        let effect a = Map.tryFind a v.SelfEffect |> Option.defaultValue 1.0
                        let lasts a = Map.tryFind a v.SelfDuration |> Option.defaultValue 1.0
                        let thr = max 1.0 v.AilmentThreshold
                        let playerName = env.Fight.Player.Name
                        let buff name mods duration (w: World) =
                            updateActor playerName (fun st -> { st with Buffs = Map.add name { Name = name; Mods = mods; Stacks = 1; Expires = w.Time + duration } st.Buffs }) w
                            |> schedule (w.Time + duration) Expire
                            |> fun w -> { w with Counts = bump ("player:" + name) w.Counts }
                        let lock seconds what (w: World) =
                            { w with
                                Player = w.Player |> Option.map (fun p -> { p with LockedUntil = max p.LockedUntil (w.Time + seconds) })
                                Counts = bump ("player:" + what) w.Counts }
                        // a charm for this may stop it (and is used up doing so)
                        let unlessCharm key (apply: World -> World) (w: World) =
                            let w, prevented = charm env rng key w
                            if prevented then { w with Counts = bump ("charm prevented:" + key) w.Counts } else apply w
                        // ignite: strongest only
                        let w =
                            let fire = get Fire
                            if fire > 0.0 && rng.Chance(min 100.0 (fire / thr * Constants.igniteChanceMultiplier)) && not (avoided "Ignite") then
                              w |> unlessCharm "Ignited" (fun w ->
                                let dps =
                                    fire * Constants.ignitePercentPerSecond / 100.0 * effect "Ignite"
                                    * (1.0 - (Map.tryFind Fire v.Resist |> Option.defaultValue 0.0) / 100.0) * (Map.tryFind Fire v.TakenMult |> Option.defaultValue 1.0)
                                let until = w.Time + Constants.igniteDuration * lasts "Ignite"
                                { w with
                                    Player =
                                        w.Player |> Option.map (fun p ->
                                            let stronger = p.Dots |> List.exists (fun (u, d, t) -> t = Fire && u > w.Time && d >= dps)
                                            if stronger then p else { p with Dots = (until, dps, Fire) :: (p.Dots |> List.filter (fun (_, _, t) -> t <> Fire)) })
                                    Counts = bump "player:Ignited" w.Counts })
                            else w
                        let w =
                            let lightning = get Lightning
                            if lightning > 0.0 && rng.Chance(min 100.0 (lightning / thr * Constants.shockChanceMultiplier)) && not (avoided "Shock") then
                                w |> unlessCharm "Shocked" (buff "Shocked" [ Mod.base' (Named "ShockTaken") (Constants.baseShockMagnitude * effect "Shock") ] (Constants.playerShockDuration * lasts "Shock"))
                            else w
                        let w =
                            let cold = get Cold
                            // (a chill below 30% is discarded; "effect of Chill on you" then scales what is left)
                            let chill = Constants.chillEffectMultiplier * cold / thr
                            let w =
                                if chill >= Constants.chillMinEffect && effect "Chill" > 0.0 && not (avoided "Chill") then
                                    w |> unlessCharm "Slowed" (buff "Chilled" [ Mod.more Speed (-(min Constants.chillMaxEffect chill * effect "Chill")) ] (Constants.playerChillDuration * lasts "Chill"))
                                else w
                            // freeze builds up with cold damage; full: frozen
                            match w.Player with
                            | Some p when cold > 0.0 && not (avoided "Freeze") ->
                                let buildup = p.FreezeBuildup + cold * Constants.playerFreezeScale / max 1.0 v.FreezeThreshold * 100.0
                                if buildup >= 100.0 then
                                    { w with Player = Some { p with FreezeBuildup = 0.0 } } |> unlessCharm "Frozen" (lock (Constants.playerFreezeDuration * lasts "Freeze") "Frozen")
                                else { w with Player = Some { p with FreezeBuildup = buildup } }
                            | _ -> w
                        // light stun: chance from the hit's size against the stun threshold
                        let stunDamage =
                            unmitigated |> Map.toList
                            |> List.sumBy (fun (t, d) -> if t = Physical then d * (1.0 + Constants.monsterPhysicalStunMore / 100.0) else d)
                            |> fun d -> if atk.Melee then d * (1.0 + Constants.monsterMeleeStunMore / 100.0) else d
                        // (50% more threshold for each light stun of the past 4 s)
                        let stuns = match w.Player with Some p -> p.Stuns |> List.filter (fun t -> t > w.Time - Constants.recentlySeconds) | None -> []
                        let stunChance = min 100.0 (stunDamage / (max 1.0 v.StunThreshold * (1.0 + Constants.lightStunThresholdMore / 100.0 * float stuns.Length)) * 100.0)
                        // players are not heavily stunned by ordinary hits (only while raising a shield,
                        // parrying or mounted: not modelled)
                        if stunChance >= Constants.playerLightStunMinimum && rng.Chance stunChance && not (rng.Chance v.AvoidStun) then
                            w |> unlessCharm "Stunned" (fun w ->
                                lock (Constants.playerLightStunDuration / v.StunRecovery) "Stunned" w
                                |> fun w -> { w with Player = w.Player |> Option.map (fun p -> { p with Stuns = w.Time :: stuns }) }
                                // "if you've been Stunned Recently"; "Regenerate X% of maximum Life over 1 second when Stunned"
                                |> markStatus env.Fight.Player.Name "StunnedRecently"
                                |> fun w ->
                                    if v.RegenOnStun <= 0.0 then w
                                    else
                                        let rate = v.RegenOnStun / 100.0 * pools.Life / Constants.stunRegenSeconds
                                        { w with Player = w.Player |> Option.map (fun p -> { p with Recoups = (w.Time + Constants.stunRegenSeconds, rate, "Life") :: p.Recoups }) })
                        else w
                let w =
                    { w with
                        Counts = w.Counts |> bump "enemy:hit" |> (if deflected then bump "enemy:deflected" else id) |> (if blocked then bump "enemy:blocked" else id) }
                let w =
                    match w.Player with
                    | Some p when blocked ->
                        { w with
                            Player =
                                Some
                                    { p with
                                        Ward = (if v.WardOnBlock > 0.0 then min (max p.Ward pools.Ward) (p.Ward + v.WardOnBlock) else p.Ward)
                                        Life = min (max p.Life pools.Life) (p.Life + v.LifeOnBlock)
                                        EnergyShield =
                                            (if v.ESOnBlock = 0.0 then p.EnergyShield
                                             else max 0.0 (min (max p.EnergyShield (min pools.EnergyShield v.ESRecoverCap)) (p.EnergyShield + v.ESOnBlock)))
                                        Mana = min pools.Mana (p.Mana + v.ManaOnBlock) } }
                    | _ -> w
                if blocked then
                    // blocks feed meta gems (Cast on Block) and OnBlock rules
                    let player = env.Fight.Player
                    let w =
                        player.Metas
                        |> List.fold
                            (fun w meta ->
                                let amount = meta.Sources |> List.sumBy (function Blocks e -> e | _ -> 0.0)
                                // (what a block triggers is aimed at the enemy whose hit was blocked)
                                if amount > 0.0 then { gainEnergy env rng player meta.Name (amount * meta.EnergyMult) { w with Cause = name } with Cause = w.Cause } else w)
                            w
                    match player.Skills |> List.tryHead with
                    | Some anySkill -> react env rng player OnBlock anySkill (Some name) w
                    | None -> w
                else w
    | _ -> w

/// Mark "<what>Recently" for 4 s (gained / consumed charges...).
/// A monster's hit at a minion: evasion, armour against physical damage, resistances.
let attackMinion env (rng: Rng) name (minion: Actor) (w: World) =
    let e = w.Enemies.[name]
    let atk = e.Def.Attack.Value
    let body = minion.Body.Value
    let st = status e
    let stopped = e.StunnedUntil > w.Time || [ "Frozen"; "HeavilyStunned"; "Electrocuted" ] |> List.exists st.ContainsKey
    let chill = e.Debuffs |> Map.tryFind "Chilled" |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value)) |> Option.defaultValue 0.0
    let debuffs = (defender e).Mods
    let actionSpeed = max 0.1 (Mods.more Ctx.empty [ Named "ActionSpeed" ] debuffs)
    let dealt = max 0.0 (Mods.more Ctx.empty [ Named "DamageDealt" ] debuffs)
    let atk = { atk with Damage = Vitals.withExtraChaos (Mods.sumBase Ctx.empty [ Named "GainAsChaos" ] debuffs) atk.Damage }
    let next = (if (1.0 - chill / 100.0) * actionSpeed < Constants.slowFloor then atk.Interval / Constants.slowFloor else atk.Interval / max 0.1 (1.0 - chill / 100.0) / actionSpeed)
    let w = schedule (w.Time + next) (EnemyAttack name) w
    if stopped then { w with Counts = bump "enemy:stopped" w.Counts }
    else
        let accuracy = atk.Accuracy * (if st.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
        let hitChance = Vitals.monsterHitChance body.Evasion accuracy
        let landed, w = lands rng minion.Name hitChance w
        if not landed then { w with Counts = bump "minion:evaded" w.Counts }
        else
            let crit = rng.Chance atk.CritChance && not (hitChance < 100.0 && rng.Chance(100.0 - hitChance))
            let critMult = if crit then 1.0 + Constants.monsterBaseCritBonus / 100.0 else 1.0
            let taken =
                atk.Damage
                |> Map.map (fun t d ->
                    let d = d * dealt * critMult
                    match t with
                    | Physical -> d * body.Taken * (1.0 - (if body.PhysicalReduction = 0.0 then Vitals.armourReduction body.Armour d else min 90.0 (Vitals.armourReduction body.Armour d + body.PhysicalReduction)) / 100.0)
                    | t -> d * body.Taken * (1.0 - (Map.tryFind t body.Resist |> Option.defaultValue 0.0) / 100.0))
            hurtMinion env rng minion taken { w with Counts = bump "minion:hit" w.Counts }

/// A monster's attack goes at the player or at one of the minions in its reach, each as
/// likely as the other. A melee minion fights at the monster; the others stand where the
/// player does.
let enemyAttack env (rng: Rng) name (w: World) =
    let e = w.Enemies.[name]
    let minions = env.Fight.Minions |> List.filter (fun m -> m.Body.IsSome && not (Map.find m.Name w.Actors).Gone)
    let asleep = env.Fight.March.IsSome && not (w.Woken.Contains name)
    match e.Def.Attack with
    | Some atk when asleep && e.Life > 0.0 -> schedule (w.Time + atk.Interval) (EnemyAttack name) w
    | Some atk when e.Life > 0.0 && env.Fight.Minions |> List.exists (fun m -> m.Body.IsSome) ->
        let playerUp = env.Fight.Player.Vitals.IsSome && (w.Player |> Option.exists (fun p -> p.Died.IsNone))
        let inRange = Vec.length e.Def.Position - e.Def.Size <= atk.Range + 1e-6
        let reachable = minions |> List.filter (fun m -> m.Body.Value.Melee || inRange)
        let n = reachable.Length + (if playerUp && inRange then 1 else 0)
        let pick = if n > 0 then int (rng.Float() * float n) else 0
        if pick < reachable.Length then attackMinion env rng name reachable.[pick] w
        elif playerUp then attackPlayer env rng name w
        else schedule (w.Time + atk.Interval) (EnemyAttack name) w
    | _ -> attackPlayer env rng name w

/// Conduit: "If you would gain a Charge, Allies in your Presence gain that Charge instead".
let conduit (actor: Actor) (r: string) =
    r.EndsWith "Charge" && Mods.flag Ctx.empty (Named("Conduit:" + r)) (ModDb.ofList actor.Mods)

/// "Recover X% of maximum Life for each Endurance Charge consumed": the player's, for so many.
let enduranceLife (actor: Actor) (n: int) (w: World) =
    match actor.Vitals, w.Player with
    | Some v, Some p when n > 0 && v.LifePerEnduranceConsumed > 0.0 && v.Life > 1.0 ->
        { w with Player = Some { p with Life = min (max p.Life v.Life) (p.Life + v.LifePerEnduranceConsumed * float n) } }
    | _ -> w
