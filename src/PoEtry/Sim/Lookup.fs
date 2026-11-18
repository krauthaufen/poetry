/// Reading the world: what an enemy suffers from, what an actor's buffs make of its modifiers,
/// the player's defences at this moment, what the player can see.
[<AutoOpen>]
module internal PoEtry.Engine.Lookup

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// Damaging ailments that actually deal damage: the strongest N of each kind.
/// (Remembered for the last few lists of them, by the very list: sorting hundreds of
/// stacks is asked for several times between two changes of them.)
let dotsSeen : (obj * DotInstance list) option[] = Array.create 32 None
let mutable dotsNext = 0

let activeDots (e: EnemyState) =
    if e.Dots.IsEmpty then []
    else
        let mutable found = None
        for i in 0 .. dotsSeen.Length - 1 do
            match dotsSeen.[i] with
            | Some (dots, active) when obj.ReferenceEquals(dots, e.Dots) -> found <- Some active
            | _ -> ()
        match found with
        | Some active -> active
        | None ->
            // (as `groupBy label |> collect (sortByDescending dps |> truncate limit)` would give
            // them: the groups in the order their labels first come, in a group the strongest
            // first and of equals the one that came first; without sorting where only the
            // few strongest of many are wanted)
            let labels = ResizeArray<string>()
            let groups = ResizeArray<ResizeArray<DotInstance>>()
            for d in e.Dots do
                let mutable g = -1
                for i in 0 .. labels.Count - 1 do
                    if g < 0 && labels.[i] = d.Label then g <- i
                if g < 0 then
                    labels.Add d.Label
                    groups.Add(ResizeArray<DotInstance>())
                    g <- labels.Count - 1
                groups.[g].Add d
            let active =
                [ for ds in groups do
                    let mutable limit = ds.[0].MaxStacks
                    for d in ds do
                        if d.MaxStacks > limit then limit <- d.MaxStacks
                    if limit > 0 then
                        // the strongest `limit` so far, strongest first
                        let top = ResizeArray<DotInstance>()
                        for d in ds do
                            if top.Count < limit || d.Dps > top.[top.Count - 1].Dps then
                                // after every one at least as strong
                                let mutable at = top.Count
                                while at > 0 && top.[at - 1].Dps < d.Dps do
                                    at <- at - 1
                                top.Insert(at, d)
                                if top.Count > limit then top.RemoveAt(top.Count - 1)
                        yield! top ]
            let at = dotsNext % dotsSeen.Length
            dotsNext <- at + 1
            dotsSeen.[at] <- Some (box e.Dots, active)
            active

let statusOf (e: EnemyState) =
    let fromDebuffs = e.Debuffs |> Map.map (fun _ x -> x.Stacks)
    let withDots =
        activeDots e
        |> List.choose (fun d -> d.Ailment |> Option.map Ailment.status)
        |> List.countBy id
        |> List.fold (fun m (k, n) -> Map.add k n m) fromDebuffs
    let ailing = withDots |> Map.exists (fun k _ -> allAilments |> List.exists (fun a -> Ailment.status a = k))
    withDots
    |> fun m -> if e.Def.Moving then Map.add "Moving" 1 m else m
    // within Culling Strike range for its rarity
    |> fun m -> if e.Life <= e.Def.Life * Constants.cullingThreshold (Rarity.name e.Def.Rarity) / 100.0 then Map.add "Cullable" 1 m else m
    |> fun m -> if ailing then Map.add "AnyAilment" 1 m else m
    |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Curse:") then Map.add "Cursed" 1 m else m
    |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Mark:") then Map.add "Marked" 1 m else m
    |> fun m -> if [ "Frozen"; "HeavilyStunned"; "Electrocuted"; "Pinned" ] |> List.exists m.ContainsKey then Map.add "Immobilised" 1 m else m
    |> fun m ->
        match [ "Ignited"; "Chilled"; "Frozen"; "Shocked"; "Electrocuted" ] |> List.filter m.ContainsKey |> List.length with
        | 0 -> m
        | n -> Map.add "ElementalAilments" n m
    |> fun m -> if e.Debuffs |> Map.exists (fun k _ -> k.StartsWith "Exposure:") then Map.add "Exposed" 1 m else m
    |> fun m -> if e.Def.Rarity = Rare || e.Def.Rarity = Unique then Map.add "RareOrUnique" 1 m else m
    |> fun m -> if e.Def.Rarity = Unique then Map.add "Unique" 1 m else m
    |> fun m -> if e.Def.Rarity = Magic then Map.add "Magic" 1 m else m
    // low life: at or below 35% (PoE2); full life: untouched
    |> fun m -> if e.Life <= 0.35 * e.Def.Life then Map.add "LowLife" 1 m else m
    |> fun m -> if e.Life >= e.Def.Life then Map.add "FullLife" 1 m else m

/// The last few enemies asked about with their statuses: one hit asks several times about
/// the very same enemy value. (A pair is put in whole, so a reader on another thread sees
/// an old pair or a new one, never a mixed one.)
let statusSeen : (EnemyState * Map<string, int>) option[] = Array.create 4 None
let mutable statusNext = 0

let status (e: EnemyState) : Map<string, int> =
    let mutable found = None
    for i in 0 .. statusSeen.Length - 1 do
        match statusSeen.[i] with
        | Some (seen, s) when obj.ReferenceEquals(seen, e) -> found <- Some s
        | _ -> ()
    match found with
    | Some s -> s
    | None ->
        let s = statusOf e
        let at = statusNext % statusSeen.Length
        statusNext <- at + 1
        statusSeen.[at] <- Some (e, s)
        s

/// The last few enemies' own mods and debuffs with what they make together: a hit on an
/// enemy changes its life, and a debuff put on it again its time, far more often than
/// what the debuffs do; and every hit asks. Remembered by the enemy's own mods (the very
/// value) and what each debuff is (its stacks and mods). (Put in whole, as above.)
let defenderSeen : (obj * int * (int * Mod list) list * Defender) option[] = Array.create 32 None
let mutable defenderNext = 0

let defender (e: EnemyState) : Defender =
    let now = e.Debuffs |> Map.fold (fun acc _ x -> (x.Stacks, x.Mods) :: acc) []
    let rec same (a: (int * Mod list) list) (b: (int * Mod list) list) =
        match a, b with
        | [], [] -> true
        | (n, ms) :: ra, (k, ks) :: rb -> n = k && (obj.ReferenceEquals(ms, ks) || ms = ks) && same ra rb
        | _ -> false
    let mutable found = None
    for i in 0 .. defenderSeen.Length - 1 do
        match defenderSeen.[i] with
        | Some (db, level, debuffs, d) when found.IsNone && obj.ReferenceEquals(db, e.Db) && level = e.Def.Level && same debuffs now -> found <- Some d
        | _ -> ()
    match found with
    | Some d -> d
    | None ->
        let debuffMods =
            e.Debuffs
            |> Map.toList
            |> List.collect (fun (_, x) -> x.Mods |> List.map (Mod.scaled (float x.Stacks)))
        let d = { Level = e.Def.Level; Mods = ModDb.addList debuffMods e.Db }
        let at = defenderNext % defenderSeen.Length
        defenderNext <- at + 1
        defenderSeen.[at] <- Some (box e.Db, e.Def.Level, now, d)
        d

let actorDef (env: Env) name =
    if name = env.Fight.Player.Name then env.Fight.Player
    else env.Fight.Minions |> List.find (fun m -> m.Name = name)

let buffStacks (st: ActorState) = st.Buffs |> Map.map (fun _ e -> e.Stacks)

/// The player's buffs as the last few asked about them saw them (one hit of a minion asks
/// several times about the same buffs). (Put in whole, as above.)
let ownerSeen : (Map<string, Effect> * Map<string, int>) option[] = Array.create 2 None
let mutable ownerNext = 0

/// The buffs and charges of the player, as conditions on the owner read them (`OfOwner`).
let ownerStacks (env: Env) (w: World) : Map<string, int> =
    let buffs = (Map.find env.Fight.Player.Name w.Actors).Buffs
    let mutable found = None
    for i in 0 .. ownerSeen.Length - 1 do
        match ownerSeen.[i] with
        | Some (seen, s) when obj.ReferenceEquals(seen, buffs) -> found <- Some s
        | _ -> ()
    match found with
    | Some s -> s
    | None ->
        let s = buffs |> Map.map (fun _ e -> e.Stacks)
        let at = ownerNext % ownerSeen.Length
        ownerNext <- at + 1
        ownerSeen.[at] <- Some (buffs, s)
        s

/// A context with the owner's buffs in it.
let owned (env: Env) (w: World) (ctx: Ctx) = { ctx with Owner = ownerStacks env w }

/// The player's defences and recovery with its buffs, charges and states of the moment
/// (`Actor.VitalsAt`; None: the player has no pools, or none that change). Remembered by
/// all its buffs and their stacks: the same buffs, the same answer.
let vitalsNow (env: Env) (w: World) : PlayerStats option =
    match env.Fight.Player.Vitals, env.Fight.Player.VitalsAt with
    | Some _, Some at ->
        let st = Map.find env.Fight.Player.Name w.Actors
        match env.VitalsLast.[0] with
        | Some (asked, v) when obj.ReferenceEquals(asked, st.Buffs) -> Some v
        | _ ->
            // (written out: the buffs the defences read, where the actor says which those are)
            let key =
                match env.Fight.Player.VitalsReads with
                | Some names ->
                    names
                    |> Set.fold (fun (acc: System.Text.StringBuilder) k ->
                        match Map.tryFind k st.Buffs with
                        | Some e -> acc.Append(k).Append('=').Append(e.Stacks).Append(';')
                        | None -> acc) (System.Text.StringBuilder())
                    |> string
                | None ->
                    st.Buffs |> Map.fold (fun (acc: System.Text.StringBuilder) k e -> acc.Append(k).Append('=').Append(e.Stacks).Append(';')) (System.Text.StringBuilder()) |> string
            // (what any run of the fight has worked out is kept with the fight)
            let seen = env.VitalsSeen
            let v =
                lock seen (fun () ->
                    match seen.TryGetValue key with
                    | true, v -> v
                    | _ ->
                        let v = at (buffStacks st)
                        seen.[key] <- v
                        v)
            env.VitalsLast.[0] <- Some(st.Buffs, v)
            Some v
    | v, _ -> v

/// Buff stacks as a skill sees them: a skill that consumes charges counts as consuming
/// "N additional" ones too ("50% more damage per Power Charge Consumed").
let skillStacks (actor: Actor) (skill: Skill) (st: ActorState) =
    let stacks = buffStacks st
    // parts (Falling Thunder's projectiles) count like the skill they belong to
    let consumes =
        skill.Consumes
        |> Option.orElse (actor.Skills |> List.tryPick (fun s -> if s.Parts |> List.exists (fun p -> p.Name = skill.Name) then s.Consumes else None))
    match consumes with
    | Some charge when Map.tryFind charge stacks |> Option.exists (fun n -> n > 0) ->
        let ctx = { Ctx.empty with SkillName = skill.Name; SkillTags = skill.Tags }
        let extra = int (Mods.sumBase ctx [ Named("Extra" + charge + "sConsumed") ] (ModDb.ofList (actor.Mods @ skill.Mods)))
        if extra > 0 then Map.add charge (stacks.[charge] + extra) stacks else stacks
    | _ -> stacks

/// The actor's mods while using a skill: sheet + skill mods + active buffs.
let skillDb env (actor: Actor) (skill: Skill) (st: ActorState) =
    let build () =
        let buffMods = st.Buffs |> Map.toList |> List.collect (fun (_, e) -> e.Mods |> List.map (Mod.scaled (float e.Stacks)))
        let db = ModDb.addList buffMods (Map.find (actor.Name, skill.Name) env.SkillDbs)
        // Rage: 1% more attack damage per Rage, times rage effect, rounded down.
        match Map.tryFind "Rage" st.Buffs with
        | Some r when r.Stacks > 0 ->
            let effect = Mods.mult Ctx.empty [ Named "RageEffect" ] db
            let more = floor (float r.Stacks * effect) * Constants.moreAttackDamagePerRage
            ModDb.add db (Mod.more (Damage AllDamage) more |> Mod.whenever (SkillIs Attack) |> Mod.from "Rage")
        | _ -> db
    let ofActor =
        match env.WithBuffs.TryGetValue actor.Name with
        | true, d -> d
        | _ ->
            let d = System.Collections.Generic.Dictionary<string, Map<string, Effect> * ModDb>()
            env.WithBuffs.[actor.Name] <- d
            d
    match ofActor.TryGetValue skill.Name with
    | true, (buffs, db) when obj.ReferenceEquals(buffs, st.Buffs) -> db
    | _ ->
        let db = build ()
        ofActor.[skill.Name] <- (st.Buffs, db)
        db

let aimed (env: Env) (w: World) =
    // (a triggered skill "usually targets the cause of the trigger", wiki: Trigger)
    match (if w.Aim = "" then None else Map.tryFind w.Aim w.Enemies) with
    | Some e when e.Life > 0.0 -> Some e
    // (the cause died of the hit: the skill lands where it was, on whoever is nearest to there)
    | Some e when not (allDead w) -> alive w |> List.minBy (fun x -> Vec.dist x.Def.Position e.Def.Position) |> Some
    | _ ->
        Geometry.aim env.Fight.Targeting (alive w |> List.map (fun e -> e.Def))
        |> Option.map (fun d -> w.Enemies.[d.Name])

let view env (w: World) (st: ActorState) =
    let target = aimed env w
    // what the player sees of the minions: how many of each pool of temporary ones are out
    // ("Out:<pool>"), and how many corpses and dead minions lie about ("Corpses")
    let seen =
        if env.Fight.Minions |> List.forall (fun m -> m.Pool.IsNone) then buffStacks st
        else
            let out =
                env.Fight.Minions
                |> List.filter (fun m -> m.Pool.IsSome && not (Map.find m.Name w.Actors).Gone)
                |> List.countBy (fun m -> "Out:" + m.Pool.Value)
            let bones =
                env.Fight.Minions
                |> List.filter (fun m -> (m.Body |> Option.exists (fun b -> b.Revive.IsSome)) && (Map.find m.Name w.Actors).Gone && not (w.Boned.Contains m.Name))
                |> List.length
            ("Corpses", w.Kills.Length - w.CorpsesUsed + bones) :: out
            |> List.fold (fun m (k, n) -> if n > 0 then Map.add k n m else m) (buffStacks st)
    { Time = w.Time
      Buffs = seen
      EnemyStatus = target |> Option.map status |> Option.defaultValue Map.empty
      EnemyLifeFraction = target |> Option.map (fun e -> e.Life / e.Def.Life) |> Option.defaultValue 0.0
      EnemyRarity = target |> Option.map (fun e -> e.Def.Rarity) |> Option.defaultValue Normal
      EnemiesAlive = (alive w).Length
      Energy = st.Energy
      LastUsed = st.LastUsed
      Standing =
        (if w.Grounds.IsEmpty then Map.empty
         else w.Grounds |> List.filter (fun g -> g.Until > w.Time) |> List.countBy (fun g -> g.Spec.Skill) |> Map.ofList) }

let matches filter (skill: Skill) =
    match filter with
    | AnySkill -> true
    | WithTag t -> Set.contains t skill.Tags
    | WithTagNot (t, n) -> Set.contains t skill.Tags && not (Set.contains n skill.Tags)
    | WithTags (has, hasNot) -> has |> List.forall skill.Tags.Contains && not (hasNot |> List.exists skill.Tags.Contains)
    | Called n -> skill.Name = n

/// What kills by damage over time count as for a rule's filter: no skill types.
let dotKiller = Skill.make "damage over time" []

// ---- time passing ----

/// Advance the clock to t, applying damage over time and recording uptime.
/// DoT dps is constant between events (every change is an event), so this is
/// exact; an enemy dying from DoT mid-interval dies at the exact time.
/// Whether a condition of the fight's (the player's mods and skills', its minions', its
/// defences', its rules') asks about a buff of this name.
let asksBuff (fight: Fight) (name: string) =
    let rec asks (c: Cond) =
        match c with
        | WhileBuff b | PerStack (b, _) -> b = name
        | Not inner | OfOwner inner -> asks inner
        | _ -> false
    let inMods (ms: Mod list) = ms |> List.exists (fun m -> m.Conds |> List.exists asks)
    let rec inReaction (r: Reaction) =
        match r with
        | IfBuff (b, next) -> b = name || inReaction next
        | SpendStack (_, next) | IfEnemyHas (_, next) | Every (_, _, next) -> inReaction next
        | WithCorpse (a, b) -> inReaction a || inReaction b
        | _ -> false
    let rec skill (s: Skill) = inMods s.Mods || s.Parts |> List.exists skill
    let actor (a: Actor) =
        inMods a.Mods
        || a.Skills |> List.exists skill
        || a.Rules |> List.exists (fun r -> inReaction r.Reaction)
        || (match a.VitalsReads with Some names -> names.Contains name | None -> a.VitalsAt.IsSome)
    actor fight.Player || fight.Minions |> List.exists actor
