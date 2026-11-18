/// One hit on one enemy: the roll, the damage, and everything the hit leaves behind.
[<AutoOpen>]
module internal PoEtry.Engine.Hitting

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// Statuses the game tracks for "recently" conditions: (name, seconds).
let recently hit crit killed =
    [ if hit then yield "HitRecently", 4.0
      if hit && not crit then yield "NonCritRecently", 4.0
      if crit then
          yield "CritRecently", 4.0
          yield "CritInPast8Seconds", 8.0
      if killed then yield "KilledRecently", 4.0 ]

let markRecently (actor: string) hit crit killed (w: World) =
    recently hit crit killed
    |> List.fold
        (fun w (name, secs) ->
            updateActor actor (fun st -> { st with Buffs = Map.add name { Name = name; Mods = []; Stacks = 1; Expires = w.Time + secs } st.Buffs }) w
            |> schedule (w.Time + secs) Expire)
        w

let markStatus actorName (name: string) (w: World) =
    updateActor actorName (fun st -> { st with Buffs = Map.add name { Name = name; Mods = []; Stacks = 1; Expires = w.Time + 4.0 } st.Buffs }) w
    |> schedule (w.Time + 4.0) Expire

let stacksOf (st: ActorState) r = Map.tryFind r st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0

/// Set a resource's stacks. `refresh`: a gain restarts its duration; a loss keeps it.
let setStacksWith (env: Env) actorName refresh r n (w: World) =
    let actor = actorDef env actorName
    let duration = Map.tryFind r actor.ResourceDurations
    let w =
        updateActor actorName (fun st ->
            if n <= 0 then { st with Buffs = Map.remove r st.Buffs }
            else
                let expires =
                    match duration, Map.tryFind r st.Buffs with
                    | Some d, _ when refresh -> w.Time + d
                    | _, Some old -> old.Expires
                    | Some d, None -> w.Time + d
                    | None, None -> infinity
                { st with Buffs = Map.add r { Name = r; Mods = []; Stacks = n; Expires = expires } st.Buffs }) w
    match duration with
    | Some d when refresh && n > 0 -> schedule (w.Time + d) Expire w
    | _ -> w

/// The player gains rage, up to the maximum; the loss timer starts again.
let gainRage (env: Env) (amount: float) (w: World) =
    let name = env.Fight.Player.Name
    let maxRage = Map.tryFind "Rage" env.Fight.Player.MaxResources |> Option.defaultValue 0
    if amount <= 0.0 || maxRage <= 0 then w
    else
        let st = Map.find name w.Actors
        setStacksWith env name false "Rage" (min maxRage (stacksOf st "Rage" + int amount)) w
        |> updateActor name (fun st -> { st with RageAt = w.Time })

/// One hit on one enemy.
/// Trinity: "Resonance of an element not gained for 8 seconds drains away", 10 a second
/// (the gem's data). Reckoned whenever a hit of the player's is worked out (it drains at no
/// event); the clock of an element says from when on its drain is not reckoned yet.
let syncResonance (env: Env) (actor: Actor) (w: World) =
    if actor.Name <> env.Fight.Player.Name then w
    else
        let st = Map.find actor.Name w.Actors
        if not (st.Buffs.ContainsKey "Resonance") then w
        else
            let clock t = "ResonanceClock:" + DamageType.name t
            let drained t =
                match Map.tryFind (clock t) st.Energy with
                | Some c when w.Time > c -> int ((w.Time - c) * Constants.trinityDecayPerSecond)
                | _ -> 0
            if DamageType.elemental |> List.forall (fun t -> drained t = 0) then w
            else
                updateActor actor.Name (fun st ->
                    let key t = "Resonance:" + DamageType.name t
                    let put k n (b: Map<string, Effect>) = if n > 0 then Map.add k { Name = k; Mods = []; Stacks = n; Expires = infinity } b else Map.remove k b
                    let values = DamageType.elemental |> List.map (fun t -> t, max 0 ((Map.tryFind (key t) st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0) - drained t))
                    let energy =
                        DamageType.elemental |> List.fold (fun (m: Map<string, float>) t ->
                            let d = drained t
                            if d > 0 then Map.add (clock t) (Map.find (clock t) m + float d / Constants.trinityDecayPerSecond) m else m) st.Energy
                    let buffs = values |> List.fold (fun b (t, n) -> put (key t) n b) st.Buffs
                    { st with Energy = energy; Buffs = put "Resonance" (values |> List.sumBy snd) buffs }) w

let hitEnemy env (rng: Rng) (actor: Actor) (skill: Skill) mult (target: string) (w: World) =
    let w = syncResonance env actor w
    let e = w.Enemies.[target]
    if e.Life <= 0.0 then
        w
    else
        let st = Map.find actor.Name w.Actors
        let mods = skillDb env actor skill st
        // Isolated: none of its allies within 6 m (keyword text)
        let isolated = alive w |> List.forall (fun o -> o.Def.Name = e.Def.Name || Vec.dist o.Def.Position e.Def.Position > 60.0)
        // Close Range: within 2 m of the player (keyword text)
        let close = Vec.length e.Def.Position - e.Def.Size <= 20.0
        let enemyStatus = status e |> (fun m -> if isolated then Map.add "Isolated" 1 m else m) |> (fun m -> if close then Map.add "Close" 1 m else m)
        // Revealed Weaknesses cover 45% of a rare or unique enemy's life in random segments, and
        // it has an Open Weakness while its life is inside one. Over a kill that is the share of
        // the time spent in those segments, where damage is higher: (c / m) / (1 - c + c / m).
        let enemyStatus =
            let more = Mods.sumBase Ctx.empty [ Named "OpenWeaknessMore" ] mods
            if e.Def.Rarity <> Normal && e.Def.Rarity <> Magic && Mods.flag Ctx.empty (Named "RevealWeakness") mods then
                let c, m = Constants.revealedWeaknessLifeShare, 1.0 + more / 100.0
                if rng.Float() < (c / m) / (1.0 - c + c / m) then Map.add "OpenWeakness" 1 enemyStatus else enemyStatus
            else enemyStatus
        let ctx = Skill.ctx skill false (skillStacks actor skill st) enemyStatus |> owned env w
        let hitChance =
            match Mods.override' ctx HitChance mods with
            | Some v -> v
            | None when skill.Tags.Contains Attack ->
                // attacks roll accuracy against the monster's evasion; spells always hit.
                // Actors without any accuracy (hand-built) always hit.
                let accuracy = Mods.total ctx [ Named "Accuracy" ] mods
                if accuracy <= 0.0 then 100.0
                else
                    // Blind: 20% less evasion (and accuracy); Maim: 15% reduced evasion (wiki, Blind, Maim)
                    let evasion =
                        Constants.monsterEvasion e.Def.Level * Mods.mult Ctx.empty [ Evasion ] e.Db
                        * (if enemyStatus.ContainsKey "Blinded" then 1.0 - Constants.blindAccuracyLess / 100.0 else 1.0)
                        * (if enemyStatus.ContainsKey "Maimed" then 1.0 - Constants.maimEvasionReduced / 100.0 else 1.0)
                    let distance = max 0.0 (Vec.length e.Def.Position - e.Def.Size)
                    Hit.accuracyHitChance evasion (Hit.accuracyAtDistance distance accuracy)
            | None -> 100.0
        // a part that only happens against an enemy in some state ("Vs Frozen"): nothing otherwise;
        // one that consumes the state takes it off
        let onlyVs = skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "OnlyVs:" -> Some(n.Substring 7) | _ -> None)
        let consumesVs = skill.Mods |> List.exists (fun m -> m.Stat = Named "ConsumesVs")
        let states = onlyVs |> Option.map (fun (st: string) -> st.Split '|' |> List.ofArray) |> Option.defaultValue []
        // and one that gives way to another part against that state ("NotVs")
        let notVs =
            skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "NotVs:" -> Some(n.Substring 6) | _ -> None)
            |> Option.map (fun (st: string) -> st.Split '|' |> List.ofArray) |> Option.defaultValue []
        let takes = skill.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith "Takes:" -> Some(n.Substring 6) | _ -> None)
        // Hexblast: nothing without a curse that is ripe; the curses it detonates are gone
        let ripe = if partFlag skill "DetonatesCurse" then ripeCurses actor e w.Time else []
        if partFlag skill "DetonatesCurse" && ripe.IsEmpty then w
        else
        let w =
            if ripe.IsEmpty then w
            else
                updateEnemy target (fun en -> { en with Debuffs = ripe |> List.fold (fun m k -> Map.remove k m) en.Debuffs }) w
                |> fun w -> { w with Counts = bump "curse detonated" w.Counts }
        // a warp into a projectile explodes there, whatever the enemy's state
        let warped = (Map.find actor.Name w.Actors).Buffs.ContainsKey("Warped:" + skill.Name)
        let w = if warped then updateActor actor.Name (fun st -> { st with Buffs = Map.remove ("Warped:" + skill.Name) st.Buffs }) w else w
        let states, consumesVs = if warped then [], false else states, consumesVs
        if not states.IsEmpty && not (states |> List.exists enemyStatus.ContainsKey) then w
        elif notVs |> List.exists enemyStatus.ContainsKey then w
        else
        let w =
            if consumesVs && not states.IsEmpty then
                let st = states |> List.find enemyStatus.ContainsKey
                updateEnemy target (fun en -> { en with Debuffs = Map.remove st en.Debuffs; Immune = Map.add st (w.Time + Constants.consumedImmunity) en.Immune }) w |> fun w -> { w with Counts = bump ("consumed:" + st) w.Counts }
            else w
        let landed, w = lands rng target hitChance w
        if not landed then
            { w with Counts = bump ("miss:" + skill.Name) w.Counts }
        else
        // a hit that takes a state off the enemy without needing it ("Consumes Shock")
        let w =
            match takes with
            | Some st when enemyStatus.ContainsKey st ->
                updateEnemy target (fun en -> { en with Debuffs = Map.remove st en.Debuffs; Immune = Map.add st (w.Time + Constants.consumedImmunity) en.Immune }) w |> fun w -> { w with Counts = bump ("consumed:" + st) w.Counts }
            | _ -> w
        if false then w
        else
            let def = defender e
            // "Enemies you Curse / Mark take X% increased Damage": additive with what the enemy already takes
            let def =
                match Mods.sumInc ctx [ Named "EnemyDamageTaken" ] mods with
                | 0.0 -> def
                | x -> { def with Mods = ModDb.add def.Mods (Mod.inc (DamageTaken AllDamage) x) }
            // one critical hit roll per use, shared by all its hits; skills that keep hitting
            // on their own (Sustained) roll for every hit
            let critRoll = if skill.Tags.Contains(Tag "Type:Sustained") then None else Map.tryFind skill.Name (Map.find actor.Name w.Actors).CritRolls
            // "If ... that hit would deal a critical hit, the chance to evade is tested again
            // independently. If this check is passed, the critical hit is downgraded to a
            // non-critical hit" (wiki, Evasion)
            let critRoll = if hitChance < 100.0 && rng.Chance(100.0 - hitChance) then Some 1.0 else critRoll
            let hit = Hit.rollWith critRoll rng ctx mods skill def
            // The same hit the other way (a critical hit if it was none, none if it was one),
            // rolled apart from the fight's stream, and with it what the hit deals on average
            // over the two, as a factor on what it did deal. (Not where a critical hit is
            // rolled in a way of its own.)
            let evenly =
                let chance = if critRoll |> Option.exists (fun r -> r >= 1.0) then 0.0 else min 100.0 (max 0.0 (Hit.critChance ctx mods skill))
                let was = Dmg.total hit.Dealt
                if chance <= 0.0 || chance >= 100.0 || was <= 0.0 || Mods.flag ctx (Named "InevitableCrits") mods || Mods.flag ctx (Named "BifurcateCrits") mods then 1.0
                else
                    let other = Dmg.total (Hit.rollWith (Some(if hit.Crit then 1.0 else 0.0)) (Rng(uint32 (w.Seq % 1000000) + 7919u)) ctx mods skill def).Dealt
                    let p = chance / 100.0
                    (if hit.Crit then p * was + (1.0 - p) * other else p * other + (1.0 - p) * was) / was
            // Culling Strike: an enemy at or below the threshold for its rarity dies, before the hit's damage
            let hit =
                let cctx = { ctx with Crit = hit.Crit }
                if Mods.flag cctx (Named "CullingStrike") mods
                   && e.Life <= e.Def.Life * Constants.cullingThreshold (Rarity.name e.Def.Rarity) / 100.0 * Mods.inc cctx [ Named "CullingThreshold" ] mods then
                    { hit with Dealt = Dmg.map (fun _ v -> v + e.Life) hit.Dealt }
                else hit
            // more attack damage by distance: Close Combat, full within 1 m, none from 3.5 m;
            // Longshot, none within 3.5 m, full from 7 m
            let mult =
                let near, far = Mods.sumBase ctx [ Named "CloseCombatMore" ] mods, Mods.sumBase ctx [ Named "FarCombatMore" ] mods
                if near = 0.0 && far = 0.0 then mult
                else
                    let d = max 0.0 (Vec.length e.Def.Position - e.Def.Size)
                    let closeness = max 0.0 (min 1.0 ((35.0 - d) / 25.0))
                    let farness = max 0.0 (min 1.0 ((d - 35.0) / 35.0))
                    mult * (1.0 + near * closeness / 100.0) * (1.0 + far * farness / 100.0)
            let hit =
                { hit with
                    Unmitigated = Dmg.map (fun _ v -> v * mult) hit.Unmitigated
                    Dealt = Dmg.map (fun _ v -> v * mult) hit.Dealt }
            // ailments are rolled before the damage lands; a shock the hit applies already
            // counts for the hit itself (user observation)
            // Flammability earlier hits left on the enemy adds to this hit's chance to Ignite,
            // and this hit leaves its own
            let flammable = e.Flammability |> List.filter (fun (_, until) -> until > w.Time)
            let inflicted =
                let total = flammable |> List.sumBy fst
                Ailments.fromHit rng ctx mods (if total > 0.0 then { def with Mods = ModDb.add def.Mods (Mod.base' (Named "Flammability") total) } else def) hit
            let w =
                match Ailments.flammability ctx mods def hit with
                | f when f > 0.0 -> updateEnemy target (fun en -> { en with Flammability = (f, w.Time + Constants.flammabilityDuration) :: flammable }) w
                | _ -> w
            // a part that only inflicts (a poison burst): what it would have dealt counts
            // for the ailment, the hit itself deals nothing
            let hit = if Mods.flag ctx (Named "NoHitDamage") mods then { hit with Dealt = Dmg.map (fun _ _ -> 0.0) hit.Dealt; Unmitigated = Dmg.map (fun _ _ -> 0.0) hit.Unmitigated } else hit
            let hit =
                let currentShock =
                    Map.tryFind "Shocked" e.Debuffs
                    |> Option.filter (fun x -> x.Expires > w.Time)
                    |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value))
                    |> Option.defaultValue 0.0
                let newShock = inflicted |> List.tryPick (function Status (Shock, m, _) when m > currentShock -> Some m | _ -> None)
                match newShock with
                | Some m ->
                    let withoutOld = def.Mods |> ModDb.toList |> List.filter (fun x -> x.Source <> "Shock")
                    let def' = { def with Mods = ModDb.ofList ((Mod.inc (DamageTaken AllDamage) m |> Mod.from "Shock") :: withoutOld) }
                    { hit with Dealt = Hit.mitigate ctx mods def' hit.Unmitigated }
                | None -> hit
            let dealt = Dmg.total hit.Dealt
            let life = e.Life - dealt
            let dead = life <= 1e-9
            (match env.Trace with Some tell -> tell w.Time (if hit.Crit then "crit" else "hit") target skill.Name dealt | None -> ())
            let w =
                { w with
                    // the enemy as it is now: what the hit consumed stays consumed
                    Enemies = Map.add target { (Map.find target w.Enemies) with Life = (if dead then 0.0 else life) } w.Enemies
                    DamageBy = addf (skill.Name + " (hit)") (min dealt e.Life) w.DamageBy
                    // (only of a hit the enemy outlives: what a killing hit deals is the life that was left)
                    Evened = (if dead then w.Evened else w.Evened + dealt * (evenly - 1.0))
                    Overkill = w.Overkill + max 0.0 (dealt - e.Life)
                    // (a hit that deals more than the enemy has left: each type's share of what was left)
                    DamageByType = (let k = if dealt > e.Life && dealt > 0.0 then e.Life / dealt else 1.0 in hit.Dealt |> Map.fold (fun m t v -> if v > 0.0 then addf (DamageType.name t) (v * k) m else m) w.DamageByType)
                    // "25% of Hit Damage dealt to them while Debuffed": hits are what is recorded
                    Agonies = w.Agonies |> List.map (fun ((o, s, en, recorded, share, until) as a) -> if en = target && until > w.Time then (o, s, en, recorded + min dealt e.Life, share, until) else a)
                    Counts = w.Counts |> bump ("hits:" + skill.Name) |> (if hit.Crit then bump ("crits:" + skill.Name) else id)
                    Shown = (if w.Tracing then Struck(w.Time, target, min dealt e.Life, hit.Crit) :: w.Shown else w.Shown)
                    // (on a march a hit wakes who it lands on, and in rooms the whole of its pack)
                    Woken =
                        (match env.Fight.March with
                         | Some m when not (w.Woken.Contains target) ->
                             if not m.Rooms then w.Woken.Add target
                             else
                                 let pack = target.Substring(target.LastIndexOf '#')
                                 w.Enemies |> Map.fold (fun (woken: Set<string>) name x -> if x.Life > 0.0 && name.TrimEnd('+').EndsWith pack then woken.Add name else woken) w.Woken
                         | _ -> w.Woken)
                    CorpsesUsed = corpsesAfter dead target w.Time w
                    Kills = if dead then (target, e.Def.Rarity, w.Time) :: w.Kills else w.Kills }
            // overkill explosions: nearby enemies are ignited for a share of the overkill
            let w =
                let overkill = dealt - e.Life
                if not dead || overkill <= 0.0 || actor.OverkillBurns.IsEmpty then w
                else
                    actor.OverkillBurns
                    |> List.filter (fun b -> matches b.Filter skill && overkill >= e.Def.Life * b.ThresholdPercent / 100.0)
                    |> List.fold (fun (w: World) b ->
                        match actor.Skills |> List.tryFind (fun x -> x.Name = b.Skill) with
                        | Some sk ->
                            let st = Map.find actor.Name w.Actors
                            let bctx = Skill.ctx sk false (skillStacks actor sk st) Map.empty |> owned env w
                            let bmods = skillDb env actor sk st
                            let radius = match sk.Delivery with | AreaAtTarget r -> Geometry.radius r (Mods.mult bctx [ AreaOfEffect ] bmods) | _ -> 0.0
                            let seconds = sk.Duration * Mods.mult bctx [ SkillDuration ] bmods
                            let dps = overkill * b.PercentPerSecond / 100.0
                            // (the explosion is shown, and its burn is the herald's in the damage shares, not any Ignite)
                            let called = (let k = b.Skill.IndexOf ':' in if k > 0 then b.Skill.Substring(0, k) else b.Skill)
                            let w = if w.Tracing then { w with Shown = Used(w.Time, actor.Name, called, [ Ring(e.Def.Position, radius, 0.0) ]) :: w.Shown } else w
                            alive w
                            |> List.filter (fun o -> o.Def.Name <> e.Def.Name && Vec.dist o.Def.Position e.Def.Position <= radius + o.Def.Size)
                            |> List.fold (fun w o ->
                                inflict o.Def.Name actor.Name (Dot(Ignite, Fire, dps, seconds, 1)) { w with Counts = bump ("overkill ignite:" + b.Skill) w.Counts }
                                |> updateEnemy o.Def.Name (fun x -> match x.Dots with d :: rest when d.Label = Ailment.name Ignite -> { x with Dots = { d with Label = called } :: rest } | _ -> x)) w
                        | None -> w) w
            // Armour Break: the hit breaks armour by a share of the damage it dealt; armour at
            // zero is Fully Broken: the enemy takes increased physical damage from hits
            let w =
                let share =
                    Dmg.get Physical hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfPhysical" ] mods / 100.0
                    + Dmg.get Fire hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfFire" ] mods / 100.0
                    + Dmg.get Chaos hit.Dealt * Mods.sumBase ctx [ Named "ArmourBreakOfChaos" ] mods / 100.0
                    + dealt * Mods.sumBase ctx [ Named "ArmourBreakOfDamage" ] mods / 100.0
                    + Mods.sumBase ctx [ Named "ArmourBreakFlat" ] mods
                // "Players Break 3 times Armour against Normal Monsters and 2 times Armour against Magic Monsters"
                let broken =
                    share * Mods.mult ctx [ Named "ArmourBreakAmount" ] mods
                    * (if actor.Name <> env.Fight.Player.Name then 1.0 else match e.Def.Rarity with Normal -> 3.0 | Magic -> 2.0 | _ -> 1.0)
                if dead || broken <= 0.0 then w
                else
                    let baseArmour = max 0.0 (Mods.total Ctx.empty [ Armour ] e.Db)
                    let before = Map.tryFind "ArmourBreak" e.Debuffs |> Option.filter (fun x -> x.Expires > w.Time) |> Option.map (fun x -> -(x.Mods |> List.sumBy (fun m -> m.Value))) |> Option.defaultValue 0.0
                    let total = min baseArmour (before + broken)
                    let seconds = Constants.armourBreakDuration * Mods.inc ctx [ Named "ArmourBreakDuration" ] mods
                    let w = addDebuff target "ArmourBreak" seconds 1 [ Mod.base' Armour -total |> Mod.from "Armour Break" ] w
                    if total >= baseArmour then
                        let taken = Constants.fullyBrokenArmourTaken * Mods.inc ctx [ Named "FullyBrokenArmourEffect" ] mods
                        let scopes =
                            if Mods.flag ctx (Named "BrokenArmourAllDamage") mods then [ AllDamage ]
                            else
                                [ OfType Physical ]
                                @ (if Mods.flag ctx (Named "BrokenArmourFire") mods then [ OfType Fire ] else [])
                                @ (if Mods.flag ctx (Named "BrokenArmourColdLightning") mods then [ OfType Cold; OfType Lightning ] else [])
                        let w = addDebuff target "FullyBrokenArmour" seconds 1 (scopes |> List.map (fun sc -> Mod.inc (DamageTaken sc) taken |> Mod.from "Fully Broken Armour")) w
                        // (a break of armour that was not fully broken already)
                        if before < baseArmour then react env rng actor OnFullBreak skill (Some target) w else w
                    else w
            // a light stun: the chance is the hit against the monster's life (physical damage and a
            // melee hit 50% more each), nothing below 15%; it stops the monster for half a second
            let w =
                let chance = min 100.0 ((dealt + Dmg.get Physical hit.Dealt * Constants.playerPhysicalStunMore / 100.0) * (if skill.Tags.Contains Melee then 1.0 + Constants.playerMeleeStunMore / 100.0 else 1.0) / max 1.0 e.Def.Life * 100.0)
                if not dead && chance >= Constants.lightStunMinimum && rng.Chance chance then
                    updateEnemy target (fun en -> { en with StunnedUntil = w.Time + Constants.monsterLightStunDuration }) w |> fun w -> { w with Counts = bump "light stun" w.Counts }
                else w
            // mana leech from physical attack damage dealt, recovered over time
            let w =
                match actor.Vitals, w.Player with
                | Some v, Some p ->
                    // (monsters resist leech, the more the higher their level)
                    let kept = Rarity.leechKept e.Def.Level
                    let leech = min (Dmg.get Physical hit.Dealt) Constants.effectiveMaxDamageForLeech * Mods.sumBase ctx [ ManaLeech ] mods / 100.0 * kept
                    // leech counts from one enemy per hit event: only the largest instance
                    let extra = max 0.0 (leech - p.StrikeLeech)
                    let charges =
                        match actor.ManaFlask with
                        | Some f when dead -> min f.MaxCharges (p.FlaskCharges + RarityCharges.flaskChargesPerKill e.Def.Rarity * f.ChargeGainMult)
                        | _ -> p.FlaskCharges
                    let leechMult = Mods.mult ctx [ Named "ManaLeechAmount" ] mods
                    let extra = extra * leechMult
                    // life: leech from physical attack damage (same one-enemy rule), gain per hit
                    // (of physical damage; "Life Leech recovers based on your Lightning / Elemental
                    // damage as well as Physical damage", "... on your Chaos damage instead of Physical damage")
                    let leechBase =
                        let also t = if Mods.flag ctx (Named("LeechFrom:" + DamageType.name t)) mods then Dmg.get t hit.Dealt else 0.0
                        (if Mods.flag ctx (Named "LeechNotFromPhysical") mods then 0.0 else Dmg.get Physical hit.Dealt) + also Lightning + also Cold + also Fire + also Chaos
                    let lifeLeechFull =
                        min leechBase Constants.effectiveMaxDamageForLeech * Mods.sumBase ctx [ Named "LifeLeech" ] mods / 100.0
                        * Mods.mult ctx [ Named "LifeLeechAmount" ] mods * v.LifeRecoveryRate * kept
                    let lifeLeech = max 0.0 (lifeLeechFull - p.StrikeLifeLeech)
                    let onHit = Mods.sumBase ctx [ Named "LifeOnHit" ] mods * v.LifeRecoveryRate
                    let manaOnHit = Mods.sumBase ctx [ Named "ManaOnHit" ] mods * v.ManaRecoveryRate
                    let lifeCharges =
                        match v.LifeFlask with
                        | Some f when dead -> min f.MaxCharges (p.LifeFlaskCharges + RarityCharges.flaskChargesPerKill e.Def.Rarity * f.ChargeGainMult)
                        | _ -> p.LifeFlaskCharges
                    let w =
                        { w with
                            Player =
                                Some
                                    { p with
                                        Recoups =
                                            // a leech instance comes back over one second ("Leech Life X% less quickly" stretches it)
                                            (if extra > 0.0 then [ w.Time + Constants.leechSeconds, extra / Constants.leechSeconds, "LeechMana" ] else [])
                                            @ (if lifeLeech > 0.0 && v.Life > 1.0 then
                                                   (let t = Constants.leechSeconds / max 0.05 v.LifeLeechRate
                                                    [ w.Time + t, lifeLeech / t, "LeechLife" ])
                                               else [])
                                            @ p.Recoups
                                        StrikeLeech = max leech p.StrikeLeech
                                        StrikeLifeLeech = max lifeLeechFull p.StrikeLifeLeech
                                        FlaskCharges = charges
                                        LifeFlaskCharges = lifeCharges
                                        Life = min (max p.Life v.Life) (p.Life + (if v.Life > 1.0 then onHit else 0.0) + (if dead then v.LifeOnKill else 0.0))
                                        EnergyShield = min (max p.EnergyShield (min v.EnergyShield v.ESRecoverCap)) (p.EnergyShield + (if dead then v.ESOnKill else 0.0)) } }
                    let w = if manaOnHit > 0.0 then gainMana v "hit" manaOnHit w else w
                    if dead then
                        let kill = (Mods.sumBase Ctx.empty [ ManaOnKill ] mods + v.Mana * Mods.sumBase Ctx.empty [ Named "ManaPercentOnKill" ] mods / 100.0) * v.ManaRecoveryRate
                        gainMana v "kill" kill w
                    else w
                | _ -> w
            // a killing hit still inflicts its ailments (they count for meta energy and Archon);
            // only their damage over time on the dead monster is dropped
            let w =
                if dead then
                    let w = react env rng actor OnKill skill (Some target) w
                    // what the killed enemy was (frozen, shocked...) can set things off
                    status e |> Map.fold (fun w k _ -> react env rng actor (OnKillOf k) skill (Some target) w) w
                else
                    inflicted
                    |> List.fold
                        (fun w i ->
                            // (a bar that this hit fills: the enemy is Frozen, Electrocuted,
                            // Heavily Stunned or Pinned by it, and was not before)
                            let had = match i with AddBuildup (a, _, _) -> Map.containsKey (Ailment.status a) w.Enemies.[target].Debuffs | _ -> true
                            let w = inflict target actor.Name i w
                            match i with
                            | Dot (a, _, _, _, _) | Status (a, _, _) ->
                                react env rng actor (OnInflict a) skill (Some target) { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
                            | AddBuildup (a, _, _) when not had && Map.containsKey (Ailment.status a) w.Enemies.[target].Debuffs ->
                                react env rng actor (OnInflict a) skill (Some target) w
                            | AddBuildup _ -> w)
                        w
            // the skill's own damage over time: one instance per skill and type, the strongest stays
            let w =
                if dead || skill.Dots.IsEmpty then w
                else
                    // spell (or attack) damage mods only count if the skill says so
                    let dotCtx = if skill.Tags.Contains(Tag "SpellDot") then ctx else { ctx with SkillTags = ctx.SkillTags |> Set.remove Spell |> Set.remove Attack }
                    let duration = (if skill.Duration > 0.0 then skill.Duration else 1.0) * Mods.mult ctx [ SkillDuration ] mods
                    skill.Dots
                    |> Map.fold
                        (fun w t perSecond ->
                            let label = sprintf "%s (%s over time)" skill.Name ((DamageType.name t).ToLowerInvariant())
                            let dps = perSecond * Hit.damageMult dotCtx mods t * Mods.mult dotCtx [ Named "DotDamage" ] mods
                            let d = { Ailment = None; Label = label; Type = t; Dps = dps; Expires = w.Time + duration; MaxStacks = 1; Owner = actor.Name; Carried = 0.0 }
                            updateEnemy target (fun e -> { e with Dots = d :: (e.Dots |> List.filter (fun x -> x.Label <> label || x.Dps > dps)) }) w
                            |> schedule d.Expires Expire)
                        w
            // meta gem energy: crits and newly inflicted freeze / shock / ignite, scaled by
            // monster power and by the share of the ailment threshold the hit deals
            // Glory and Combo: skills that can only be used once enough has been built, count up here
            let w =
                let needs (s: Skill) = s.Mods |> List.exists (fun m -> match m.Stat with Named n -> n.StartsWith "Requires:" | _ -> false)
                match actor.Skills |> List.filter needs with
                | [] -> w
                | builders ->
                    let power = MonsterPower.ofRarity e.Def.Rarity
                    let before = status e
                    let after = if dead then Map.empty else status w.Enemies.[target]
                    let newly (st: string) = not dead && not (before.ContainsKey st) && after.ContainsKey st
                    let dealt types = Dmg.sumOf types hit.Dealt > 0.0
                    builders |> List.fold (fun w (s: Skill) ->
                        let own name = s.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
                        let ownHit = skill.Name = s.Name || s.Parts |> List.exists (fun p -> p.Name = skill.Name)
                        let glory =
                            if own "Requires:Glory" <= 0.0 || ownHit then 0.0
                            else
                                power / 100.0
                                * ((if skill.Tags.Contains Attack then own "Builds:Glory:AttackHit" else 0.0)
                                   + (if dealt DamageType.elemental then own "Builds:Glory:ElementalHit" else 0.0)
                                   + (if dealt [ Chaos ] then own "Builds:Glory:ChaosHit" else 0.0)
                                   + ([ "HeavilyStunned"; "Frozen"; "Ignited" ] |> List.sumBy (fun st -> if newly st then own ("Builds:Glory:" + st) else 0.0)))
                                + ([ "Ignited"; "Shocked" ] |> List.sumBy (fun st -> if newly st then own ("Builds:GloryFlat:" + st) else 0.0))
                        // Combo: from melee attack hits with other skills
                        let combo =
                            if own "Requires:Combo" <= 0.0 || ownHit || not (skill.Tags.Contains Attack) || not (skill.Tags.Contains(Tag "Type:Melee")) then 0.0
                            else 1.0 + (if rng.Chance(own "ComboExtraChance") then 1.0 else 0.0)
                        // `per`: what the gain is limited by ("each Skill can only generate Glory once
                        // every 0.5 seconds per monster"; Combo "once per strike"); `fade`: what is
                        // lost when nothing was gained for a while
                        let add key amount cap (per: string) every (fade: float -> float -> float) (w: World) =
                            if amount <= 0.0 then w
                            else
                                let st = Map.find actor.Name w.Actors
                                let stamp, lastKey = key + "@" + per, key + "@last"
                                let since k = w.Time - (Map.tryFind k st.Energy |> Option.defaultValue -1e9)
                                if since stamp < every then w
                                else
                                    let have = fade (Map.tryFind key st.Energy |> Option.defaultValue 0.0) (since lastKey)
                                    updateActor actor.Name (fun st ->
                                        { st with Energy = st.Energy |> Map.add key (min cap (have + amount)) |> Map.add stamp w.Time |> Map.add lastKey w.Time }) w
                        w
                        // "Skills lose 2 Glory per second if they haven't gained Glory in the past 15 seconds"
                        |> add ("Glory:" + s.Name) glory (own "Requires:Glory" * (1.0 + own "GloryRequiredMore" / 100.0)) target 0.5
                               (fun have idle -> if idle > 15.0 && idle < 1e8 then max 0.0 (have - 2.0 * (idle - 15.0)) else have)
                        // Combo counts the strikes "in the last 8 seconds" (less, where Combo is lost faster)
                        |> add ("Combo:" + s.Name) combo (own "Requires:Combo") "strike" 1e-9
                               (let window = match own "ComboWindow" with 0.0 -> Constants.comboWindow | w -> w
                                fun have idle -> if idle > window && idle < 1e8 then 0.0 else have)) w
            // Banners: attack hits build Glory; a full banner is placed at once
            let w =
                if actor.Banners.IsEmpty || not (skill.Tags.Contains Attack) then w
                else
                    let power = MonsterPower.ofRarity e.Def.Rarity
                    actor.Banners |> List.fold (fun w b ->
                        let key = "Glory:" + b.Name
                        // once every 0.5 seconds per monster
                        let stamp = key + "@" + target
                        let fresh = w.Time - (Map.tryFind stamp (Map.find actor.Name w.Actors).Energy |> Option.defaultValue -1e9) >= 0.5
                        let w = if fresh then updateActor actor.Name (fun st -> { st with Energy = Map.add stamp w.Time st.Energy }) w else w
                        let have = (Map.tryFind key (Map.find actor.Name w.Actors).Energy |> Option.defaultValue 0.0) + (if fresh then power * b.PerAttackHit / 100.0 else 0.0)
                        let up = (Map.find actor.Name w.Actors).Buffs |> Map.tryFind ("Banner:" + b.Name) |> Option.exists (fun x -> x.Expires > w.Time)
                        if up then updateActor actor.Name (fun st -> { st with Energy = Map.add key (min b.Glory have) st.Energy }) w
                        elif have >= b.Glory then
                            updateActor actor.Name (fun st ->
                                { st with
                                    Energy = Map.add key 0.0 st.Energy
                                    Buffs = Map.add ("Banner:" + b.Name) { Name = "Banner:" + b.Name; Mods = b.Mods; Stacks = 1; Expires = w.Time + b.Duration } st.Buffs }) w
                            |> schedule (w.Time + b.Duration) Expire
                            |> fun w -> { w with Counts = bump ("banner:" + b.Name) w.Counts }
                        else updateActor actor.Name (fun st -> { st with Energy = Map.add key have st.Energy }) w) w
            // "Energy cannot be gained from direct effects of Triggered Skills" (poe2wiki, poe2db)
            let w =
                if actor.Metas.IsEmpty || skill.Tags.Contains Triggered then w
                else
                    let threshold = max 1.0 (Ailments.threshold def)
                    let power = MonsterPower.ofRarity e.Def.Rarity
                    let ratio types = Dmg.sumOf types hit.Unmitigated / threshold
                    let before = status e
                    let after = if dead then Map.empty else status w.Enemies.[target]
                    let newly a =
                        if dead then
                            // on a killing hit: what this hit inflicted, and buildup that would complete
                            inflicted
                            |> List.exists (fun i ->
                                match i with
                                | Dot (x, _, _, _, _) | Status (x, _, _) -> x = a
                                | AddBuildup (x, pct, _) -> x = a && (Map.tryFind x e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0)
                        else after.ContainsKey (Ailment.status a) && not (before.ContainsKey (Ailment.status a))
                    actor.Metas
                    |> List.fold
                        (fun w meta ->
                            let amount =
                                meta.Sources
                                |> List.sumBy (fun src ->
                                    match src with
                                    | CritHits per when hit.Crit -> per * power * ratio DamageType.all
                                    | MeleeHits per when skill.Tags.Contains Melee -> per * power * ratio DamageType.all
                                    | MeleeKills per when dead && skill.Tags.Contains Melee -> per * power
                                    | InflictsAilment (a, per) when newly a -> per * power * ratio (Ailments.scalesFromWith ctx mods a)
                                    | _ -> 0.0)
                            // (what this fills up and triggers is aimed at the enemy that was hit)
                            if amount > 0.0 then { gainEnergy env rng actor meta.Name (amount * meta.EnergyMult) { w with Cause = target } with Cause = w.Cause } else w)
                        w
            // infusion remnants (Siphon Elements): each freeze / shock / ignite inflicted
            let w =
                match actor.Infusions with
                | Some inf when not inf.Remnants.IsEmpty ->
                    let newly a =
                        match a with
                        | Freeze ->
                            if dead then
                                inflicted |> List.exists (function AddBuildup (Freeze, pct, _) -> (Map.tryFind Freeze e.Buildup |> Option.defaultValue 0.0) + pct >= 100.0 | _ -> false)
                            else (status w.Enemies.[target]).ContainsKey (Ailment.status Freeze) && not ((status e).ContainsKey (Ailment.status Freeze))
                        | _ -> inflicted |> List.exists (function Dot (x, _, _, _, _) | Status (x, _, _) -> x = a | _ -> false)
                    inf.Remnants
                    |> List.fold
                        (fun w (a, r, chance) ->
                            if newly a && rng.Chance chance then
                                let n = if rng.Chance inf.ExtraRemnantChance then 2 else 1
                                { w with Counts = bump ("remnant:" + r) w.Counts } |> schedule (w.Time + inf.Delay) (GainStacks(actor.Name, r, n))
                            else w)
                        w
                | _ -> w
            // Archon: gained on a new ignite by a skill with the chance; none while one is active or
            // during the recovery period after one ends
            let w =
                // every ignite applied by this hit counts, not only a first one
                let ignited = inflicted |> List.exists (function Dot (Ignite, _, _, _, _) -> true | _ -> false)
                if not ignited || actor.Archons.IsEmpty then w
                else
                    let st = Map.find actor.Name w.Actors
                    let active = actor.Archons |> List.exists (fun a -> Map.containsKey a.Name st.Buffs)
                    let recovering = Map.tryFind "ArchonRecovery" st.Cooldowns |> Option.exists (fun t -> t > w.Time)
                    if active || recovering then w
                    else
                        actor.Archons
                        |> List.tryFind (fun a -> rng.Chance(Mods.sumBase ctx [ Named a.OnIgniteStat ] mods))
                        |> function
                            | Some a ->
                                updateActor actor.Name (fun st ->
                                    { st with
                                        Buffs = Map.add a.Name { Name = a.Name; Mods = a.Mods; Stacks = 1; Expires = w.Time + a.Duration } st.Buffs
                                        Cooldowns = Map.add "ArchonRecovery" (w.Time + a.Duration + a.Recovery) st.Cooldowns }) w
                                |> schedule (w.Time + a.Duration) Expire
                                |> fun w -> { w with Counts = bump ("archon:" + a.Name) w.Counts }
                            | None -> w
            // Runic Ward remnants from kills and stuns
            let w =
                match actor.WardRemnants, w.Player with
                | Some r, Some p ->
                    let stunnedNow = not dead && (status w.Enemies.[target]).ContainsKey "HeavilyStunned" && not ((status e).ContainsKey "HeavilyStunned")
                    let fromKill = dead && rng.Chance r.OnKillChance
                    let fromStun = stunnedNow && w.Time - p.LastStunRemnant >= r.StunCooldown && rng.Chance r.OnStunChance
                    if fromKill || fromStun then
                        let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                        let w = if fromStun then { w with Player = Some { p with LastStunRemnant = w.Time } } else w
                        { w with Counts = bump "ward remnant" w.Counts } |> schedule (w.Time + r.Delay) (WardArrives(r.WardPerRemnant * count))
                    else w
                | _ -> w
            // Mana Remnants: from a critical hit on, or the kill of, an enemy with an elemental ailment
            let w =
                match actor.ManaRemnants, w.Player with
                | Some r, Some p ->
                    let ailing = [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.exists (status e).ContainsKey
                    let fromKill = dead && ailing && rng.Chance r.OnKillChance
                    let fromCrit = hit.Crit && ailing && w.Time - p.LastManaRemnant >= r.CritCooldown
                    if fromKill || fromCrit then
                        let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                        let w = if fromCrit then { w with Player = Some { p with LastManaRemnant = w.Time } } else w
                        { w with Counts = bump "mana remnant" w.Counts } |> schedule (w.Time + r.Delay) (ManaArrives((r.ManaPerRemnant + r.PercentOfMaxMana / 100.0 * (actor.Vitals |> Option.map (fun v -> v.Mana) |> Option.defaultValue 0.0)) * count))
                    else w
                | _ -> w
            // Life Remnants: from a kill, or from a hit no more than once every so often
            let w =
                match actor.LifeRemnants, actor.Vitals, w.Player with
                | Some r, Some v, Some p ->
                    let fromKill = dead && rng.Chance r.OnKillChance
                    let fromHit = w.Time - p.LastLifeRemnant >= r.HitCooldown
                    if fromKill || fromHit then
                        let count = if rng.Chance r.ExtraChance then 2.0 else 1.0
                        let rate = (r.LifePerRemnant + r.PercentOfMaxLife / 100.0 * v.Life) * count * v.LifeRecoveryRate / r.Delay
                        { w with
                            Player = Some { p with LastLifeRemnant = (if fromHit then w.Time else p.LastLifeRemnant); Recoups = (w.Time + r.Delay, rate, "Life") :: p.Recoups }
                            Counts = bump "life remnant" w.Counts }
                    else w
                | _ -> w
            // Trinity: the hit's strongest element gains resonance (up to 100), the others lose some
            let w =
                let gain = Mods.sumBase ctx [ Named "TrinityGain" ] mods
                let elemental = DamageType.elemental |> List.map (fun t -> t, Dmg.get t hit.Unmitigated) |> List.filter (fun (_, d) -> d > 0.0)
                if gain <= 0.0 || elemental.IsEmpty || actor.Name <> env.Fight.Player.Name then w
                else
                    let top = elemental |> List.maxBy snd |> fst
                    let loss = Mods.sumBase ctx [ Named "TrinityLoss" ] mods
                    updateActor actor.Name (fun st ->
                        let key t = "Resonance:" + DamageType.name t
                        // (the element gained does not drain for 8 s: `syncResonance`)
                        let get t = Map.tryFind (key t) st.Buffs |> Option.map (fun e -> e.Stacks) |> Option.defaultValue 0
                        let st = { st with Energy = Map.add ("ResonanceClock:" + DamageType.name top) (w.Time + Constants.trinityDecayDelay) st.Energy }
                        let values = DamageType.elemental |> List.map (fun t -> t, (if t = top then min 100 (get t + int gain) else max 0 (get t - int loss)))
                        let put k n (b: Map<string, Effect>) = if n > 0 then Map.add k { Name = k; Mods = []; Stacks = n; Expires = infinity } b else Map.remove k b
                        let buffs = values |> List.fold (fun b (t, n) -> put (key t) n b) st.Buffs
                        { st with Buffs = put "Resonance" (values |> List.sumBy snd) buffs }) w
            // charms: charges from kills, and the ones used by killing a rare or unique monster
            let w =
                if not dead || env.Fight.Player.Charms.IsEmpty then w
                else
                    let w =
                        { w with
                            Player =
                                w.Player |> Option.map (fun p ->
                                    { p with
                                        CharmCharges =
                                            env.Fight.Player.Charms |> List.fold (fun (m: Map<string, float>) c ->
                                                // "granting charges equal to half of the monster's Power"
                                                let gain = (MonsterPower.ofRarity e.Def.Rarity / 2.0 + (if rng.Chance c.ChargeOnKillChance then 1.0 else 0.0)) * c.ChargeGainMult
                                                Map.add c.Buff (min c.MaxCharges ((Map.tryFind c.Buff m |> Option.defaultValue 0.0) + gain)) m) p.CharmCharges }) }
                    if e.Def.Rarity = Rare || e.Def.Rarity = Unique then fst (charm env rng "KillRare" w) else w
            // rage from hitting and from kills
            let w =
                if actor.Name <> env.Fight.Player.Name then w
                else
                    let st = Map.find actor.Name w.Actors
                    // "Only one Hit every 0.5 seconds can cause you to gain Rage" (wiki, Rage)
                    let fresh = w.Time - (Map.tryFind "RageHitAt" st.Energy |> Option.defaultValue -1e9) >= 0.5
                    let onHit = if fresh then Mods.sumBase ctx [ Named "RageOnHit" ] mods else 0.0
                    let w = if onHit > 0.0 then updateActor actor.Name (fun st -> { st with Energy = Map.add "RageHitAt" w.Time st.Energy }) w else w
                    let _, _, _, _, onKill = env.Rage
                    gainRage env (onHit + (if dead then onKill else 0.0)) w
            // Knockback: a hit that knocks back pushes the monster straight away from the player
            // (a boss is not moved). How far: the skill's own distance, or a metre where it names
            // none, x "increased Knockback Distance"; skills that knock back "based on how close
            // they are to you" push more from close by and less towards their limit. It walks
            // back like any monster that is out of its reach. Walls are not in its way.
            let w =
                let kctx = { ctx with Crit = hit.Crit }
                let chance = if dead || e.Def.Rarity = Unique then 0.0 else Mods.sumBase kctx [ Named "KnockbackChance" ] mods
                if chance > 0.0 && not (Mods.flag kctx (Named "CannotKnockback") mods) && rng.Chance chance then
                    let pos = e.Def.Position
                    let d = Vec.length pos
                    let limit = Mods.sumBase kctx [ Named "KnockbackLimit" ] mods
                    let byDistance =
                        if limit <= 0.0 then 1.0
                        else
                            let k = min 1.0 (max 0.0 (d - e.Def.Size) / limit)
                            1.0 + (Mods.sumBase kctx [ Named "KnockbackAtMin" ] mods * (1.0 - k) + Mods.sumBase kctx [ Named "KnockbackAtMax" ] mods * k) / 100.0
                    let own = Mods.sumBase kctx [ Named "KnockbackBase" ] mods
                    let far = (if own > 0.0 then own / Constants.knockbackUnitsPerDistance else Constants.knockbackDefault) * Mods.mult kctx [ Named "KnockbackDistance" ] mods * byDistance
                    if far <= 0.0 then w
                    else
                        let pushed = if d > 1e-9 then Vec.scale ((d + far) / d) pos else Vec.make far 0.0
                        { updateEnemyDef target pushed w with Counts = bump "knockback" w.Counts }
                else w
            let w = markRecently actor.Name true hit.Crit dead w
            let w = react env rng actor OnHit skill (Some target) w
            if hit.Crit then react env rng actor OnCrit skill (Some target) w else w
