/// Putting things on enemies and on the ground: debuffs, ailments, exposure, ground surfaces.
[<AutoOpen>]
module internal PoEtry.Engine.Effects

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

let addDebuff target name duration maxStacks mods (w: World) =
    // (what a skill has just consumed off the enemy cannot be put on it for a second)
    if (match Map.tryFind target w.Enemies with Some e -> e.Immune |> Map.tryFind name |> Option.exists (fun t -> t > w.Time) | None -> false) then w else
    let w =
        updateEnemy target (fun e ->
            let x = stackEffect w.Time (Map.tryFind name e.Debuffs) name duration maxStacks mods
            { e with Debuffs = Map.add name x e.Debuffs }) w
    schedule (w.Time + duration) Expire w

/// Effect multiplier of a curse / exposure on this enemy (bosses: 50% less).
let effectOnSelf (e: EnemyState) stat =
    Mods.more Ctx.empty [ Named stat ] e.Db

let inflict target owner (i: Inflict) (w: World) =
    match i with
    | Dot (a, t, dps, duration, maxStacks) ->
        let d =
            { Ailment = Some a
              Label = Ailment.name a
              Type = t
              Dps = dps
              Expires = w.Time + duration
              MaxStacks = maxStacks
              Owner = owner
              Carried = dps }
        updateEnemy target (fun e -> { e with Dots = d :: e.Dots }) w |> schedule d.Expires Expire
    | Status (Shock, magnitude, duration) ->
        // Shocks don't stack: keep the strongest.
        let e = w.Enemies.[target]
        let current =
            Map.tryFind Statuses.Shocked e.Debuffs
            |> Option.filter (fun x -> x.Expires > w.Time)
            |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> m.Value))
            |> Option.defaultValue 0.0
        if magnitude >= current then
            addDebuff target "Shocked" duration 1 [ Mod.inc (DamageTaken AllDamage) magnitude |> Mod.from "Shock" ] w
        else
            w
    | Status (Chill, magnitude, duration) ->
        // the slow is read back by the monster's attack timing
        // (a slow: what a rarer monster takes of it is less, after the cap)
        addDebuff target "Chilled" duration 1 [ Mod.base' (Named "ChillMagnitude") (magnitude * Rarity.slowPotency w.Enemies.[target].Def.Rarity) ] w
    | Status (a, _, duration) -> addDebuff target (Ailment.status a) duration 1 [] w
    | AddBuildup (a, pct, seconds) ->
        let e = w.Enemies.[target]
        // the Heavy Stun bar drains once it has gone a while without any buildup
        let had =
            let had = Map.tryFind a e.Buildup |> Option.defaultValue 0.0
            let idle = w.Time - e.StunBuildupAt - Constants.stunDecayDelay
            if a = HeavyStun && idle > 0.0 then max 0.0 (had - Constants.stunDecayPerSecond * idle) else had
        let w = if a = HeavyStun then updateEnemy target (fun e -> { e with StunBuildupAt = w.Time }) w else w
        let total = had + pct
        if total >= 100.0 && not (Map.containsKey (Ailment.status a) e.Debuffs) then
            updateEnemy target (fun e -> { e with Buildup = Map.add a 0.0 e.Buildup }) w
            |> addDebuff target (Ailment.status a) seconds 1 []
            |> fun w ->
                // afterwards the monster is much harder to freeze / electrocute / stun / pin again for a while
                let more, after = Constants.afterBuildup a
                if more > 0.0 then
                    addDebuff target (Ailment.name a + "Resistant") (seconds + after) 1
                        [ Mod.more (Named(Ailment.name a + "Threshold")) more ] w
                else w
            |> fun w -> { w with Counts = bump ("inflict:" + Ailment.name a) w.Counts }
        else
            updateEnemy target (fun e -> { e with Buildup = Map.add a (min total 100.0) e.Buildup }) w

// ---- using skills ----

let allSkills (actor: Actor) = actor.Skills @ (actor.Skills |> List.collect (fun s -> s.Parts))

let skillOf (actor: Actor) name =
    allSkills actor |> List.find (fun s -> s.Name = name)

let partFlag (p: Skill) (name: string) = p.Mods |> List.exists (fun m -> m.Stat = Named name)

/// How many of a part that waits to be set off can wait at once: the skill's own limit
/// ("Maximum of 30 active Bolts", "Limit 6 Spikes": `WaitLimit`), 20 where it has none.
let waitLimit (p: Skill) =
    match p.Mods |> List.filter (fun m -> m.Stat = Named "WaitLimit" && m.Kind = Base) with
    | [] -> 20
    | ms -> max 1 (int (ms |> List.sumBy (fun m -> m.Value)))

/// Seconds a part of a skill strikes after the skill's own hit (a lodged spear's explosion
/// after its detonation time: `PartDelay`).
let partDelay (p: Skill) = p.Mods |> List.sumBy (fun m -> if m.Stat = Named "PartDelay" && m.Kind = Base then m.Value else 0.0)
/// The rest of a part's flag that starts with this ("OnlyInfused:ColdInfusion" -> "ColdInfusion").
let partNamed (p: Skill) (prefix: string) =
    p.Mods |> List.tryPick (fun m -> match m.Stat with Named n when n.StartsWith prefix -> Some(n.Substring prefix.Length) | _ -> None)

/// Exposure on an enemy: the strongest applies, scaled by exposure effect.
let expose (actor: Actor) (t: string) (dt: DamageType) amount duration (w: World) =
    match Map.tryFind t w.Enemies with
    | Some e when e.Life > 0.0 ->
        let k = effectOnSelf e "ExposureEffectOnSelf" * Mods.inc Ctx.empty [ Named "ExposureEffect" ] (ModDb.ofList actor.Mods)
        let name = "Exposure:" + DamageType.name dt
        let current =
            Map.tryFind name e.Debuffs
            |> Option.filter (fun x -> x.Expires > w.Time)
            |> Option.map (fun x -> x.Mods |> List.sumBy (fun m -> -m.Value))
            |> Option.defaultValue 0.0
        if amount * k >= current then addDebuff t name duration 1 [ Mod.base' (Resist dt) (-amount * k) ] w else w
    | _ -> w

/// How far a ground surface reaches now ("gain X% increased Area of Effect per second, up to
/// a maximum of Y%": the radius grows with the root of the area), and where it is.
let groundRadius (g: Ground) (age: float) = g.Radius * sqrt (1.0 + min g.GrowMax (g.GrowPerSecond * age) / 100.0)
let groundCentre (g: Ground) (from: Vec) (towards: Vec) (age: float) =
    if g.Speed <= 0.0 then towards
    else
        let d = Vec.dist from towards
        let gone = g.Speed * age
        if gone >= d || d <= 0.0 then towards else Vec.add from (Vec.scale (gone / d) (Vec.sub towards from))

/// The Ignite an enemy has, as Oil would burn with it (-1: it is not Ignited): its strongest Ignite's.
let burnsWith (now: float) (e: EnemyState) : float =
    e.Dots
    |> List.fold (fun (dps, carried) d ->
        if d.Ailment = Some Ignite && d.Expires > now && (d.Dps > dps || (d.Dps = dps && d.Carried > carried)) then d.Dps, d.Carried else dps, carried) (-1.0, -1.0)
    |> snd

/// Oil catches fire: round after round, each from what burned in the round before (from
/// burning ground that touches it, or an Ignited enemy standing in it). The grounds after, and those lit.
let lightOil (now: float) (living: EnemyState list) (gs: GroundState list) : GroundState list * GroundState list =
    let radius (g: GroundState) = groundRadius g.Spec (now - g.Born)
    let centre (g: GroundState) = groundCentre g.Spec g.From g.To (now - g.Born)
    let inside (g: GroundState) (e: EnemyState) = Vec.dist e.Def.Position (centre g) - e.Def.Size <= radius g
    let touch (a: GroundState) (b: GroundState) = Vec.dist (centre a) (centre b) <= radius a + radius b
    let burns = burnsWith now
    let rec light (gs: GroundState list) (lit: GroundState list) =
        let burning = gs |> List.filter (fun g -> g.Kind = Burning)
        let gs', lit' =
            gs |> List.fold (fun (out, lit) g ->
                if g.Kind <> Oil then g :: out, lit
                else
                    let byGround = burning |> List.fold (fun best b -> if touch g b && b.Carried > best then b.Carried else best) -1.0
                    let by =
                        if byGround >= 0.0 then byGround
                        else living |> List.fold (fun best e -> if inside g e then max best (burns e) else best) -1.0
                    if by < 0.0 then g :: out, lit
                    else
                        let g' = { g with Kind = Burning; Carried = by; Ignite = by + g.Spec.Ignite; Until = now + g.Spec.Seconds }
                        g' :: out, g' :: lit) ([], lit)
        let gs' = List.rev gs'
        if lit'.Length > lit.Length then light gs' lit' else gs', lit'
    light gs []

/// A storm takes up the Ignited Ground it overlaps (the strongest, once).
let stormsAbsorb (env: Env) (now: float) (live: GroundState list) (w: World) : GroundState list * World =
    let tell kind skill x = match env.Trace with Some tell -> tell now "Ground" skill kind x | None -> ()
    let radius (g: GroundState) = groundRadius g.Spec (now - g.Born)
    let centre (g: GroundState) = groundCentre g.Spec g.From g.To (now - g.Born)
    let inside (g: GroundState) (e: EnemyState) = Vec.dist e.Def.Position (centre g) - e.Def.Size <= radius g
    let touch (a: GroundState) (b: GroundState) = Vec.dist (centre a) (centre b) <= radius a + radius b
    let live, w =
        if not (live |> List.exists (fun g -> g.Kind = Storm && g.Ignite <= 0.0 && not g.Spec.Absorbs.IsEmpty)) then live, w
        else
            let burning = live |> List.filter (fun g -> g.Kind = Burning)
            live |> List.fold (fun (out, w: World) g ->
                if g.Kind <> Storm || g.Ignite > 0.0 || g.Spec.Absorbs.IsEmpty then g :: out, w
                else
                    match burning |> List.filter (touch g) with
                    | [] -> g :: out, w
                    | touched ->
                        let b = touched |> List.fold (fun (best: GroundState) x -> if x.Ignite > best.Ignite then x else best) touched.Head
                        if b.Ignite <= 0.0 then g :: out, w
                        else
                            tell "absorbed" g.Spec.Skill b.Ignite
                            { g with Ignite = b.Ignite; Carried = b.Carried } :: out, { w with Counts = bump ("absorbed:" + g.Spec.Skill) w.Counts }) ([], w)
            |> fun (out, w) -> List.rev out, w
    live, w

/// What an enemy has of where it stands: covered in Oil, on Ignited Ground, the Exposure of
/// either, a storm's damage of the element it took up, and the ground's Ignite.
let standsOnGround (now: float) (live: GroundState list) (w: World) (e: EnemyState) : World =
    let radius (g: GroundState) = groundRadius g.Spec (now - g.Born)
    let centre (g: GroundState) = groundCentre g.Spec g.From g.To (now - g.Born)
    let inside (g: GroundState) (e: EnemyState) = Vec.dist e.Def.Position (centre g) - e.Def.Size <= radius g
    let touch (a: GroundState) (b: GroundState) = Vec.dist (centre a) (centre b) <= radius a + radius b
    let mutable oil : GroundState option = None
    let mutable onFire = false
    let mutable exposure = 0.0
    let mutable ignite = 0.0
    let mutable carried = 0.0
    let mutable seconds = 0.0
    let mutable owner = ""
    let mutable dots : (string * DamageType * float * string) list = []
    for g in live do
        if inside g e then
            match g.Kind with
            | Oil ->
                if oil.IsNone then oil <- Some g
                exposure <- max exposure (g.Spec.Exposure * g.ExposureEffect)
            | Burning ->
                onFire <- true
                exposure <- max exposure (g.Spec.Exposure * g.ExposureEffect)
                if g.Ignite > ignite then
                    ignite <- g.Ignite
                    carried <- g.Carried
                    seconds <- g.Spec.IgniteSeconds
                    owner <- g.Owner
            | Storm ->
                if g.Ignite > 0.0 then
                    if g.Ignite > ignite then
                        ignite <- g.Ignite
                        carried <- g.Carried
                        seconds <- g.Spec.IgniteSeconds
                        owner <- g.Owner
                    for (t, dps) in g.Spec.Absorbs do
                        if t = Fire && dps > 0.0 then
                            let label = Ground.dotLabel g.Spec.Shared t
                            if not (dots |> List.exists (fun (l, _, d, _) -> l = label && d >= dps)) then
                                dots <- (label, t, dps, g.Owner) :: (dots |> List.filter (fun (l, _, _, _) -> l <> label))
    let oil, onFire, exposure, ignite, carried, seconds, owner, dots = oil, onFire, exposure, ignite, carried, seconds, owner, dots
    // what is on it while it stands there, and goes when it does not
    let held (name: string) (ds: Map<string, Effect>) = Map.tryFind name ds |> Option.filter (fun x -> System.Double.IsPositiveInfinity x.Expires)
    let set (name: string) (mods: Mod list option) (ds: Map<string, Effect>) =
        match mods, held name ds with
        | Some ms, Some x when x.Mods = ms -> ds
        | Some ms, _ -> Map.add name { Name = name; Mods = ms; Stacks = 1; Expires = infinity } ds
        | None, Some _ -> Map.remove name ds
        | None, None -> ds
    let debuffs =
        e.Debuffs
        |> set Ground.oiled (oil |> Option.map (fun g -> g.Spec.Mods))
        |> set Ground.ignited (if onFire then Some [] else None)
        |> set Ground.exposed (if exposure > 0.0 then Some [ Mod.base' (Resist Fire) (-exposure * effectOnSelf e "ExposureEffectOnSelf") ] else None)
    // the storm's damage of the element it took up
    let stays (d: DotInstance) = not (d.Ailment.IsNone && System.Double.IsPositiveInfinity d.Expires) || dots |> List.exists (fun (l, _, dps, _) -> l = d.Label && dps = d.Dps)
    let kept = if e.Dots |> List.forall stays then e.Dots else e.Dots |> List.filter stays
    let fresh =
        dots |> List.rev |> List.filter (fun (l, _, _, _) -> not (kept |> List.exists (fun d -> d.Ailment.IsNone && d.Label = l && System.Double.IsPositiveInfinity d.Expires)))
        |> List.map (fun (l, t, dps, by) -> { Ailment = None; Label = l; Type = t; Dps = dps; Expires = infinity; MaxStacks = 1; Owner = by; Carried = 0.0 })
    let kept = if fresh.IsEmpty then kept else fresh @ kept
    // the Ignite of the ground, where the enemy has none as strong
    let burnsNow = ignite > 0.0 && not (kept |> List.exists (fun d -> d.Ailment = Some Ignite && d.Label = Ailment.name Ignite && d.Expires > now && d.Dps >= ignite))
    let kept =
        if not burnsNow then kept
        else { Ailment = Some Ignite; Label = Ailment.name Ignite; Type = Fire; Dps = ignite; Expires = now + seconds; MaxStacks = 1; Owner = owner; Carried = carried } :: kept
    if obj.ReferenceEquals(debuffs, e.Debuffs) && obj.ReferenceEquals(kept, e.Dots) then w
    else
        let w = { w with Enemies = Map.add e.Def.Name { e with Debuffs = debuffs; Dots = kept } w.Enemies }
        if burnsNow then { w with Counts = bump "ground ignite" w.Counts } |> schedule (now + seconds) Expire else w

/// What lies on the ground and the storms, looked at whenever something happens in the
/// fight (as the Presence is):
/// Oil catches fire where Ignited Ground touches it, or an Ignited enemy stands in it: it
/// is Ignited Ground from then, for its whole time again, and burns with the Ignite that
/// lit it (the ground's, if ground lit it; else the strongest of the enemies in it) plus
/// what the skill adds. A storm that overlaps Ignited Ground takes that ground's Ignite
/// (the strongest it overlaps then) and keeps it. An enemy in Oil is covered in it and
/// has its Exposure; one on Ignited Ground, or in a storm that has taken some up, is
/// Ignited with that Ignite (again as soon as it runs out), and in such a storm takes
/// the storm's damage of that element. Nothing here is rolled.
let syncGround (env: Env) (w: World) =
    if w.Grounds.IsEmpty then w
    else
        let now = w.Time
        let tell kind skill x = match env.Trace with Some tell -> tell now "Ground" skill kind x | None -> ()
        let live = w.Grounds |> List.filter (fun g -> g.Until > now)
        let living = alive w
        let live, lit = if live |> List.exists (fun g -> g.Kind = Oil) then lightOil now living live else live, []
        let w =
            List.rev lit |> List.fold (fun (w: World) g ->
                tell "lit" g.Spec.Skill g.Ignite
                { w with Counts = bump ("lit:" + g.Spec.Skill) w.Counts } |> schedule g.Until Expire) w
        let live, w = stormsAbsorb env now live w
        let w = { w with Grounds = live }
        // the enemies: what each has of where it stands
        living |> List.fold (standsOnGround now live) w

/// Mana spent while a Sigil of Power stands: a Stage for every share of maximum mana, at
/// most one a second, up to its maximum.
let sigilSpend (name: string) (spent: float) (maxMana: float) (w: World) =
    let st = Map.find name w.Actors
    match st.Buffs |> Map.toList |> List.tryFind (fun (_, e) -> e.Mods |> List.exists (fun m -> m.Stat = Named "SigilMaxStages")) with
    | Some (key, sigil) when sigil.Expires > w.Time && spent > 0.0 && maxMana > 0.0 ->
        let value stat = sigil.Mods |> List.sumBy (fun m -> if m.Stat = Named stat then m.Value else 0.0)
        let need = value "SigilManaPercentPerStage" / 100.0 * maxMana
        let total = (Map.tryFind "SigilSpent" st.Energy |> Option.defaultValue 0.0) + spent
        let last = Map.tryFind "SigilStageAt" st.Energy |> Option.defaultValue -1.0
        if need > 0.0 && total >= need && sigil.Stacks < int (value "SigilMaxStages") && w.Time - last >= 1.0 then
            { w with Counts = bump "sigil stage" w.Counts }
            |> updateActor name (fun st ->
                { st with
                    Buffs = Map.add key { sigil with Stacks = sigil.Stacks + 1 } st.Buffs
                    Energy = st.Energy |> Map.add "SigilSpent" (total - need) |> Map.add "SigilStageAt" w.Time })
        else updateActor name (fun st -> { st with Energy = Map.add "SigilSpent" total st.Energy }) w
    | _ -> w
