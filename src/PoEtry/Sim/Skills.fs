/// Questions about a skill before it is used (may it, is it ready, what does it need),
/// and summoning.
[<AutoOpen>]
module internal PoEtry.Engine.Skills

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// "Can also be used on Ball Lightning Projectiles": the skill a warp can go into, and
/// whether one of its projectiles is still on its way (it has strikes to come).
let warpsInto (skill: Skill) = partNamed skill "WarpsInto:"
let ballInFlight (actorName: string) (gem: string) (w: World) =
    w.Queue |> Map.exists (fun _ ev ->
        match ev with
        | Strike (a, name, _, _) | PassingStrike (a, name, _, _, _) -> a = actorName && (name = gem || name.StartsWith(gem + " "))
        | _ -> false)

/// Curses on an enemy that Hexblast can detonate: those at least half expired (their
/// duration is the one the actor's own curse rule gives; unknown: any).
let ripeCurses (actor: Actor) (e: EnemyState) now =
    e.Debuffs |> Map.toList |> List.filter (fun (k, x) ->
        k.StartsWith "Curse:" && x.Expires > now
        && (actor.Rules |> List.tryPick (fun r ->
                let rec find reaction =
                    match reaction with
                    | ApplyCurse (n, d, _) when n = k || "Curse:" + n = k -> Some d
                    | IfEnemyHas (_, next) | IfBuff (_, next) | SpendStack (_, next) | Every (_, _, next) -> find next
                    | _ -> None
                find r.Reaction)
            |> Option.forall (fun d -> x.Expires - now <= d / 2.0)))
    |> List.map fst

/// Flicker Strike's additional strikes: so many per Power Charge there is as it is used.
let flickers (skill: Skill) (st: ActorState) =
    let per = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named "FlickersPerPowerCharge" && m.Kind = Base then m.Value else 0.0)
    if per <= 0.0 then 0.0 else per * float (match Map.tryFind "PowerCharge" st.Buffs with Some e -> e.Stacks | None -> 0)

/// Barrage: the readied repeats waiting for the next Repeatable bow or spear attack.
let barraged (skill: Skill) (st: ActorState) =
    if skill.Tags.Contains(Tag "Type:Barrageable") && skill.Tags.Contains Attack then
        Map.tryFind "Barrage" st.Buffs |> Option.filter (fun b -> b.Stacks > 0)
    else None

/// Chained attacks (Ice Strike, Tempest Flurry): which use of the chain comes next. Using
/// another skill in between starts the chain again.
let seqLength (skill: Skill) =
    skill.Mods |> List.tryPick (fun m -> match m.Stat, m.Kind with Named "SeqLength", _ -> Some(int m.Value) | _ -> None) |> Option.defaultValue 0
let seqStep (skill: Skill) (st: ActorState) =
    let n = seqLength skill
    if n <= 0 then None
    elif st.LastUsed <> skill.Name then Some 0
    else Some(int (Map.tryFind ("Seq:" + skill.Name) st.Energy |> Option.defaultValue 0.0) % n)
let inStep (step: int option) (p: Skill) =
    match step, partNamed p "SeqSteps:" with
    | Some k, Some steps -> (steps: string).Split ',' |> Array.exists (fun x -> int x = k)
    | _ -> true
/// The stat set a chained skill uses this time (the skill itself, or one of its parts).
let stepSkill (skill: Skill) (st: ActorState) =
    match seqStep skill st with
    | None -> skill
    | step -> (skill :: skill.Parts) |> List.tryFind (fun p -> (partNamed p "SeqSteps:").IsSome && inStep step p) |> Option.defaultValue skill

/// Does an attack with this chance to hit land? Evasion is not rolled for each hit: the
/// defender has an entropy value, random at the first attack and again after 100 server ticks
/// (3.33 s) without one; each attempt adds the chance to hit, and at 100 or more it hits and
/// 100 is taken off (wiki, Evasion). All attackers share the defender's value.
let lands (rng: Rng) (defender: string) (hitChance: float) (w: World) : bool * World =
    if hitChance >= 100.0 then true, w
    else
        let value =
            match Map.tryFind defender w.Entropy with
            | Some (v, at) when w.Time - at <= 100.0 * Constants.serverTick -> v
            | _ -> floor (rng.Float() * 100.0)
        let value = value + hitChance
        let hit = value >= 100.0
        hit, { w with Entropy = Map.add defender ((if hit then value - 100.0 else value), w.Time) w.Entropy }

/// Glory and Combo a skill needs before it can be used: (counter, amount).
let requirements (skill: Skill) =
    let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
    [ "Glory:" + skill.Name, own "Requires:Glory" * (1.0 + own "GloryRequiredMore" / 100.0)
      "Combo:" + skill.Name, own "Requires:Combo" ]
    |> List.filter (fun (_, n) -> n > 0.0)
let built (st: ActorState) (skill: Skill) =
    requirements skill |> List.forall (fun (key, n) -> (Map.tryFind key st.Energy |> Option.defaultValue 0.0) >= n - 1e-9)

/// A skill that works for a fixed time once what it needs has built up (Requiem: 12 s,
/// then 30 s until it is charged again): (seconds it lasts, seconds to recharge).
let window (skill: Skill) =
    let own name = skill.Mods |> List.sumBy (fun m -> if m.Stat = Named name && m.Kind = Base then m.Value else 0.0)
    let secs = own "WindowSeconds"
    if secs > 0.0 then Some(secs, own "WindowRecharge") else None
/// Is its window shut (the next use opens one)?
let windowShut (st: ActorState) now (skill: Skill) =
    (window skill).IsSome && now >= (Map.tryFind ("Window:" + skill.Name) st.Energy |> Option.defaultValue -1.0)
/// Can it be used: inside its window, or charged for the next.
let windowOk (st: ActorState) now (skill: Skill) =
    not (windowShut st now skill) || now >= (Map.tryFind ("WindowNext:" + skill.Name) st.Energy |> Option.defaultValue 0.0)

let ready (st: ActorState) now (skill: Skill) =
    Map.tryFind skill.Name st.Cooldowns |> Option.forall (fun t -> t <= now)

/// A minion goes out of the fight (killed, or its time is up): what it had begun is lost.
/// The skills (and parts) of an actor whose strikes go on without it: what a minion has
/// shot or thrown still arrives when the minion is gone (a Mirage's grenades in the air).
/// A projectile or grenade skill, and its parts. Guess: the game has no text for it.
let outliving (actor: Actor) : Set<string> =
    let rec go inherited (s: Skill) =
        let o = inherited || s.Tags.Contains(Tag "Projectile") || s.Tags.Contains(Tag "Grenade")
        (if o then [ s.Name ] else []) @ (s.Parts |> List.collect (go o))
    actor.Skills |> List.collect (go false) |> Set.ofList

let leave (env: Env) name (w: World) =
    let stays = outliving (actorDef env name)
    let its ev =
        match ev with
        | Ready n -> n = name
        | Strike (n, s, _, _) | PassingStrike (n, s, _, _, _) | DirectHit (n, s, _, _) | Landed (n, s, _) -> n = name && not (stays.Contains s)
        | _ -> false
    { w with Queue = w.Queue |> Map.filter (fun _ ev -> not (its ev)) }
    |> updateActor name (fun st -> { st with Gone = true; Life = 0.0; Shield = 0.0 })

/// Temporary minions of a pool appear: those that are out first; when all are in the
/// fight, the ones whose time would be up soonest are summoned anew.
let summonWith (env: Env) (pool: string) (count: int) (duration: float) (first: Mod list) (w: World) =
    env.Fight.Minions
    |> List.filter (fun m -> m.Pool = Some pool)
    |> List.sortBy (fun m -> let st = Map.find m.Name w.Actors in (if st.Gone then 0 else 1), st.Leaves)
    |> List.truncate count
    |> List.indexed
    |> List.fold
        (fun w (i, m) ->
            let was = Map.find m.Name w.Actors
            let leaves = w.Time + duration
            let life, shield = match m.Body with Some b -> b.Life, b.EnergyShield | None -> 0.0, 0.0
            let buffs (b: Map<string, Effect>) =
                if i = 0 && not first.IsEmpty then Map.add "Empowered" { Name = "Empowered"; Mods = first; Stacks = 1; Expires = leaves } b
                else Map.remove "Empowered" b
            // (one that was out is a new one: none of its skills is on cooldown)
            let w =
                updateActor m.Name (fun st ->
                    { st with
                        Gone = false; Leaves = leaves; Life = life; Shield = shield; HitAt = w.Time; Buffs = buffs st.Buffs
                        Cooldowns = (if was.Gone then Map.empty else st.Cooldowns)
                        Recharges = (if was.Gone then Map.empty else st.Recharges) }) w
            let w = if was.Gone then schedule w.Time (Ready m.Name) w else w
            let w = if leaves < infinity then schedule leaves (MinionLeaves(m.Name, leaves)) w else w
            { w with Counts = bump ("summoned:" + pool) w.Counts })
        w

let summon env pool count duration w = summonWith env pool count duration [] w

/// Something that uses charms happened to the player ("Frozen", "Hit:Fire", "KillRare"...).
/// A charm for it that is running, or has the charges to be used now, answers; returns
/// whether the occurrence is prevented (immunity charms). "X% chance for Charms you use to
/// not consume Charges": rolled for each use.
let charm (env: Env) (rng: Rng) (key: string) (w: World) : World * bool =
    let player = env.Fight.Player
    match player.Vitals, w.Player with
    | Some pools, Some p when not player.Charms.IsEmpty ->
        let st = Map.find player.Name w.Actors
        player.Charms
        |> List.filter (fun c -> c.Trigger = key)
        |> List.fold
            (fun (w: World, prevented) c ->
                let running = Map.tryFind c.Buff st.Buffs |> Option.exists (fun e -> e.Expires > w.Time)
                let charges = match w.Player with Some p -> Map.tryFind c.Buff p.CharmCharges |> Option.defaultValue 0.0 | None -> 0.0
                if running then w, prevented || c.Prevents
                elif charges >= c.ChargesPerUse && c.ChargesPerUse > 0.0 then
                    let kept = rng.Chance env.CharmKeep
                    let w =
                        updateActor player.Name (fun st -> { st with Buffs = Map.add c.Buff { Name = c.Buff; Mods = []; Stacks = 1; Expires = w.Time + c.Duration } st.Buffs }) w
                        |> schedule (w.Time + c.Duration) Expire
                    { w with
                        Player =
                            w.Player |> Option.map (fun p ->
                                { p with
                                    CharmCharges = (if kept then p.CharmCharges else Map.add c.Buff (charges - c.ChargesPerUse) p.CharmCharges)
                                    Life = min (max p.Life pools.Life) (p.Life + (if pools.Life > 1.0 then c.LifeOnUse * pools.LifeRecoveryRate else 0.0))
                                    Mana = min (max p.Mana pools.Mana) (p.Mana + c.ManaOnUse * pools.ManaRecoveryRate)
                                    LastHit = (if c.StartsRecharge then -1e9 else p.LastHit) })
                        Counts = bump ("charm:" + c.Name) w.Counts |> (if kept then bump "charm:kept charges" else id) },
                    prevented || c.Prevents
                else w, prevented)
            (w, false)
    | _ -> w, false

/// What a skill does with corpses, if a rule of the actor says so.
let corpseRule (actor: Actor) (skill: Skill) =
    actor.Rules
    |> List.tryPick (fun r ->
        match r.Reaction with
        | WithCorpse _ | SummonPerCorpse _ when r.Hook = OnUse && matches r.Filter skill -> Some r.Reaction
        | _ -> None)
