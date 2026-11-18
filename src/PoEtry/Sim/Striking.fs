/// A use of a skill lands: which enemies it reaches, and each of its hits.
[<AutoOpen>]
module internal PoEtry.Engine.Striking

open PoEtry
// (again, after PoEtry: where both have a record with a field of one name, the engine's is meant)
open PoEtry.Engine

/// "Gain X% of Damage as Extra Chaos Damage per 20 Unreserved Darkness": the Darkness that
/// is not reserved now, as the stacks of a buff the player's hits ask about. Looked at when
/// a strike of the player's is due (a reservation ends by itself, at no event), where the
/// character has the line (the resource "UnreservedDarkness" is among its maxima then).
let syncDarkness (env: Env) (actor: Actor) (w: World) =
    match actor.Vitals, w.Player with
    | Some pools, Some p when pools.Darkness > 0.0 && actor.MaxResources.ContainsKey "UnreservedDarkness" ->
        let live = p.Darkness |> List.filter (fun (until, _) -> until > w.Time)
        let n = int (floor (max 0.0 (pools.Darkness - (live |> List.sumBy snd))))
        match Map.tryFind "UnreservedDarkness" (Map.find actor.Name w.Actors).Buffs with
        | Some e when e.Stacks = n -> w
        | _ -> updateActor actor.Name (fun st -> { st with Buffs = Map.add "UnreservedDarkness" { Name = "UnreservedDarkness"; Mods = []; Stacks = n; Expires = infinity } st.Buffs }) w
    | _ -> w

/// Whom a strike lands on: the enemy the use was aimed at, or another if that one has died
/// meanwhile (and whether it springs from where a dead one lies).
let strikeAim (env: Env) (name: string) (skill: Skill) (aimedAt: string) (w: World) : bool * EnemyDef option =
    let living = alive w |> List.map (fun e -> e.Def)
    // Aim where the use was aimed; if that enemy died meanwhile, at the next one.
    // (On a map the player's skill lands where it was aimed: on someone near enough to where
    // the dead one was, or on the next one that is within the reach the map is played with;
    // a skill around the player itself needs no one in particular.)
    // (what springs from the impact of the same use, Falling Thunder's projectiles, springs
    // from there though the slam has killed whom it was aimed at)
    let fromCorpse =
        env.Fight.March.IsSome && name = env.Fight.Player.Name
        && (match skill.Delivery with Projectiles p -> p.FromImpact | _ -> false)
        && not (living |> List.exists (fun e -> e.Name = aimedAt))
    let primary =
        living |> List.tryFind (fun e -> e.Name = aimedAt)
        |> Option.orElse (
            match env.Fight.March with
            | Some _ when fromCorpse -> Map.tryFind aimedAt w.Enemies |> Option.map (fun dead -> dead.Def)
            | Some m when name = env.Fight.Player.Name && not (match skill.Delivery with AreaAroundSelf _ -> true | _ -> false) ->
                let near =
                    match Map.tryFind aimedAt w.Enemies, living with
                    | Some dead, _ :: _ ->
                        let near = living |> List.minBy (fun x -> Vec.dist x.Position dead.Def.Position)
                        if Vec.dist near.Position dead.Def.Position <= Constants.triggerStray then Some near else None
                    | _ -> None
                near |> Option.orElse (Geometry.aim env.Fight.Targeting (living |> List.filter (fun e -> Vec.length e.Position - e.Size <= m.Engage)))
            | _ -> Geometry.aim env.Fight.Targeting living)
    fromCorpse, primary

/// On a march a strike that teleports (Flicker Strike) takes the player to whom it
/// strikes: reckoned around the player, everything comes that much nearer.
let flickerTo (env: Env) (name: string) (skill: Skill) (primary: EnemyDef option) (w: World) : World =
    match env.Fight.March, primary with
    | Some _, Some e when name = "Player" && skill.Mods |> List.exists (fun m -> m.Stat = Named "FlickersPerPowerCharge") ->
        let d = Vec.length e.Position
        let step = d - e.Size
        if step <= 0.0 then w
        else
            let by = Vec.scale (step / d) e.Position
            { w with
                Walked = w.Walked + step
                Front = w.Front - by.X
                Enemies = w.Enemies |> Map.map (fun _ e -> if e.Life > 0.0 then { e with Def = { e.Def with Position = Vec.sub e.Def.Position by } } else e) }
    | _ -> w

/// The shape of a strike with a skill: the modifiers it is asked with, the area's size, and
/// how it is delivered (projectiles: how many, what they do), with what is rolled for it.
let strikeShape (env: Env) (rng: Rng) (actor: Actor) (st: ActorState) (w: World) (skill: Skill) =
    let ctx = Skill.ctx skill false (skillStacks actor skill st) Map.empty |> owned env w
    let mods = skillDb env actor skill st
    let areaMult = Mods.mult ctx [ AreaOfEffect ] mods
    let delivery =
        match skill.Delivery with
        | Projectiles p ->
            let fromCharges =
                match p.PerCharge with
                | Some (charge, per) -> per * (Map.tryFind charge (skillStacks actor skill st) |> Option.defaultValue 0)
                | None -> 0
            Projectiles
                { p with
                    Count =
                        p.Count + fromCharges
                        + (if p.Count + fromCharges > 0 && not (skill.Tags.Contains(Tag "Type:ProjectilesNumberModifiersNotApplied")) then
                               int (Mods.sumBase ctx [ AdditionalProjectiles ] mods)
                               + (if rng.Chance(Mods.sumBase ctx [ Named "ExtraProjectileChance2" ] mods) then 2 else 0)
                               + (let c = Mods.sumBase ctx [ Named "ExtraProjectileChance1" ] mods
                                  int (c / 100.0) + (if rng.Chance(c - 100.0 * floor (c / 100.0)) then 1 else 0))
                           else 0)
                    Pierce = p.Pierce + int (Mods.sumBase ctx [ Pierce ] mods) + (if rng.Chance(Mods.sumBase ctx [ Named "PierceChance" ] mods) then 1 else 0)
                    Chains = (if Mods.flag ctx (Named "CannotChain") mods then 0 else p.Chains + int (Mods.sumBase ctx [ Chains ] mods))
                    TerrainChain = p.TerrainChain + Mods.sumBase ctx [ Named "TerrainChain" ] mods
                    // "Projectiles Fork" (a chance of 100%), "Fork an additional time"
                    Forks =
                        (let chance = Mods.sumBase ctx [ Named "ForkChance" ] mods
                         if Mods.flag ctx (Named "CannotFork") mods || not (rng.Chance chance) then 0
                         else p.Forks + 1 + int (Mods.sumBase ctx [ Named "AdditionalForks" ] mods))
                    ForkExtraChance = p.ForkExtraChance + Mods.sumBase ctx [ Named "ForkExtraChance" ] mods
                    ForkAngle = p.ForkAngle * Mods.inc ctx [ Named "ForkAngle" ] mods }
        | d -> d
    ctx, mods, areaMult, delivery

/// Infusions: each impact consumes one of the skill's element (or the wildcard) and
/// uses the infused copy; without one, the plain skill.
let infusedVariant (env: Env) (rng: Rng) (actor: Actor) (skill: Skill) (w: World) : Skill * World =
    let consumer = actor.Infusions |> Option.bind (fun inf -> Map.tryFind skill.Name inf.Consumers |> Option.map (fun c -> inf, c))
    match consumer with
    | None -> skill, w
    | Some (inf, (element, infusedName)) ->
        let st = Map.find actor.Name w.Actors
        let have r = stacksOf st r > 0
        let source = if have element then Some element else inf.Wildcard |> Option.filter have
        match source with
        | None -> skill, { w with Counts = bump ("uninfused:" + skill.Name) w.Counts }
        | Some r ->
            let infused = skillOf actor infusedName
            let keep = Mods.sumBase Ctx.empty [ Named "InfusionKeepChance" ] (skillDb env actor infused st)
            let w = { w with Counts = bump ("infused:" + skill.Name) w.Counts }
            let w = markStatus actor.Name "ConsumedInfusionRecently" w
            // "Gain a Power Charge when you consume an Elemental Infusion"
            let w =
                let n = int (Mods.sumBase Ctx.empty [ Named "PowerChargeOnInfusionConsumed" ] (skillDb env actor infused st))
                if n > 0 then schedule w.Time (GainStacks(actor.Name, "PowerCharge", n)) w else w
            if rng.Chance keep then infused, w
            else infused, setStacksWith env actor.Name false r (stacksOf st r - 1) w

/// A strike lands (an event): whom it reaches, and each of its hits (`hitEnemy`).
/// `elapsed`: a projectile in flight, so long after it was launched; `direct`: one hit on
/// the one enemy, worked out when the skill was used.
let onStrike env (rng: Rng) name skillName mult aimedAt (elapsed: float option) (direct: bool) (w: World) =
    let actor = actorDef env name
    let skill = skillOf actor skillName
    let w = syncDarkness env actor w
    let fromCorpse, primary = strikeAim env name skill aimedAt w
    let w = flickerTo env name skill primary w
    let living = alive w |> List.map (fun e -> e.Def)
    let primary = if fromCorpse then primary else primary |> Option.bind (fun e -> living |> List.tryFind (fun x -> x.Name = e.Name))
    match primary with
    | None -> w
    | Some primary ->
        let st = Map.find name w.Actors
        // (with the actor and the world as they are here: the strike's own hits do not change its shape)
        let shape (sk: Skill) = strikeShape env rng actor st w sk
        let variant (w: World) = infusedVariant env rng actor skill w
        let w = { w with Player = w.Player |> Option.map (fun p -> { p with StrikeLeech = 0.0; StrikeLifeLeech = 0.0 }) }
        // Spell Cascade: extra impacts on either side of the target "along a randomly chosen
        // axis"; side impacts have their own less area; spacing: user observation
        let ctx0, mods0, _, _ = shape skill
        let cascades = int (Mods.sumBase ctx0 [ Named "CascadesPerSide" ] mods0)
        // Triskelion Cascade: the empowered spell "becomes centred on your location, and
        // Cascades to six areas around you in a Triskelion pattern": three arms a third of a
        // turn apart, so many impacts along each, spaced like Spell Cascade's (the pattern's
        // measures are not in the data). The empowerment is used up.
        let arms = int (Mods.sumBase ctx0 [ Named "TriskelionArms" ] mods0)
        // ---- the strike itself, by how the skill is delivered ----
        match skill.Delivery with
        | AreaAtTarget _ | AreaAroundSelf _ when arms > 0 ->
            let sk, w = variant w
            let _, _, areaMult, delivery = shape sk
            let r = match delivery with AreaAtTarget r | AreaAroundSelf r -> Geometry.radius r areaMult | _ -> 0.0
            let turn = rng.Float() * 2.0 * System.Math.PI
            let centres =
                Vec.make 0.0 0.0
                :: [ for a in 0 .. 2 do
                        for k in 1 .. arms ->
                            Vec.scale (float k * r * env.Fight.CascadeSpacing) (Vec.ofAngle (turn + float a * 2.0 * System.Math.PI / 3.0)) ]
            let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, centres |> List.map (fun c -> Ring(c, r, 0.0))) :: w.Shown } else w
            let w =
                centres
                |> List.fold
                    (fun w centre ->
                        living
                        |> List.filter (fun e -> Vec.dist centre e.Position <= r + e.Size)
                        |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w)
                    w
            updateActor actor.Name (fun st ->
                { st with Buffs = st.Buffs |> Map.filter (fun _ e -> not (e.Mods |> List.exists (fun m -> m.Stat = Named "TriskelionArms"))) }) w
        | AreaAtTarget _ when cascades > 0 ->
            let axis = Vec.ofAngle (rng.Float() * 2.0 * System.Math.PI)
            // the centre impact first (it takes the first infusion); its radius sets the spacing
            let order = 0 :: [ for i in 1 .. cascades do yield -i; yield i ]
            order
            |> List.fold
                (fun (w, step) k ->
                    let sk, w = variant w
                    let ctx, mods, areaMult, delivery = shape sk
                    let r = match delivery with AreaAtTarget r -> r | _ -> 0.0
                    let centreR = Geometry.radius r areaMult
                    let step = if k = 0 then centreR * env.Fight.CascadeSpacing else step
                    let rr = if k = 0 then centreR else Geometry.radius r (areaMult * Mods.more ctx [ Named "CascadeArea" ] mods)
                    let centre = Vec.add primary.Position (Vec.scale (float k * step) axis)
                    let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(centre, rr, 0.0) ]) :: w.Shown } else w
                    let w =
                        living
                        |> List.filter (fun e -> Vec.dist centre e.Position <= rr + e.Size)
                        |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w
                    w, step)
                (w, 0.0)
            |> fst
        | _ ->
            let sk, w = variant w
            let ctx, mods, areaMult, delivery = shape sk
            // a bolt that leaves now (Frostbolt): when, and towards where it was sent
            let w =
                if Mods.flag ctx (Named "LaunchesBolt") mods && not direct && elapsed.IsNone then
                    updateActor name (fun st -> { st with Energy = st.Energy |> Map.add "BoltAt" w.Time |> Map.add "BoltAngle" (Vec.angle primary.Position) }) w
                else w
            // "Casting Ice Nova targeting near a Frostbolt Projectile will cause it to originate
            // from the Frostbolt instead of you": the bolt sent last, while it flies, if it is
            // nearer the enemy aimed at than the player is
            let novaAt =
                match delivery with
                | AreaAroundSelf _ ->
                    let speed = Mods.sumBase ctx [ Named "NovaOnBoltSpeed" ] mods
                    let st = Map.find name w.Actors
                    match Map.tryFind "BoltAt" st.Energy, Map.tryFind "BoltAngle" st.Energy with
                    | Some t0, Some angle when speed > 0.0 && w.Time - t0 >= 0.0 && w.Time - t0 < Mods.sumBase ctx [ Named "NovaOnBoltLife" ] mods ->
                        let at = Vec.scale (speed * (w.Time - t0)) (Vec.ofAngle angle)
                        if Vec.dist at primary.Position < Vec.length primary.Position then Some at else None
                    | _ -> None
                | _ -> None
            let w = if novaAt.IsSome then { w with Counts = bump ("on bolt:" + sk.Name) w.Counts } else w
            let strike (w: World) =
                match delivery, elapsed with
                | AreaAroundSelf r, _ when novaAt.IsSome ->
                    let at, rr = novaAt.Value, Geometry.radius r areaMult
                    let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(at, rr, 0.0) ]) :: w.Shown } else w
                    living |> List.filter (Geometry.within at rr) |> List.fold (fun w e -> hitEnemy env rng actor sk mult e.Name w) w
                | _ when direct ->
                    let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Line(Vec.zero, primary.Position) ]) :: w.Shown } else w
                    (if primary.Name = aimedAt then hitEnemy env rng actor sk mult aimedAt w else w)
                | Projectiles p, None when p.Passing > 0.0 && p.HitInterval > 0.0 ->
                    // what is inside the projectiles as they travel: every hit at its own time
                    let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Flight [ Vec.zero; primary.Position ] ]) :: w.Shown } else w
                    Geometry.dwellHits rng env.Fight.Walls p env.Fight.Reach areaMult primary living
                    |> List.fold (fun w (t, target) ->
                        if t <= 0.0 then hitEnemy env rng actor sk mult target w
                        else schedule (w.Time + t) (DirectHit(name, skillName, mult, target)) w) w
                | Projectiles p, Some t when p.Passing > 0.0 ->
                    // where the projectile is by now, on its way towards where it was aimed
                    let at = Vec.scale (p.Speed * t) (Vec.ofAngle (Vec.angle primary.Position))
                    let w = if w.Tracing then { w with Shown = Used(w.Time, name, sk.Name, [ Ring(at, Geometry.radius p.Passing areaMult, 0.0) ]) :: w.Shown } else w
                    living |> List.filter (Geometry.within at (Geometry.radius p.Passing areaMult)) |> List.map (fun e -> e.Name)
                    |> List.fold (fun w target -> hitEnemy env rng actor sk mult target w) w
                | _ ->
                    // "X% chance to deal Splash Damage": a strike that does also hits the
                    // others around the one it hit, as one with Splash
                    let delivery =
                        match delivery with
                        | Single when (let c = Mods.sumBase ctx [ Named "SplashChance" ] mods in c > 0.0 && rng.Chance c) -> AreaAtTarget Constants.splashRadius
                        | d -> d
                    // forked projectiles: "Forked Projectiles deal N% less damage" (Fork)
                    let forkMult = mult * Mods.mult ctx [ Named "ForkedDamage" ] mods
                    let hit, forked, w =
                        if w.Tracing then
                            let hit, forked, marks = Geometry.targetsShown rng env.Fight.Walls env.Fight.Reach delivery areaMult primary living
                            hit, forked, { w with Shown = Used(w.Time, name, sk.Name, marks) :: w.Shown }
                        else
                            let hit, forked = Geometry.targetsParted rng env.Fight.Walls env.Fight.Reach delivery areaMult primary living
                            hit, forked, w
                    let w = hit |> List.fold (fun w target -> hitEnemy env rng actor sk mult target w) w
                    let w = forked |> List.fold (fun w target -> hitEnemy env rng actor sk forkMult target w) w
                    // a grenade that activates a second time (Payload) hits once more: rolled
                    // for each of its hits (a grenade's second blast hits all it hit together:
                    // the same on average)
                    let twice = Mods.sumBase ctx [ Named "ExplodeTwice" ] mods
                    if twice <= 0.0 then w
                    else hit |> List.fold (fun w target -> if rng.Chance twice then hitEnemy env rng actor sk mult target { w with Counts = bump ("twice:" + sk.Name) w.Counts } else w) w
            let w = strike w
            // Aftershock: a slam has a chance to hit its area once more
            if sk.Tags.Contains Slam && rng.Chance(Mods.sumBase ctx [ Named "AftershockChance" ] mods) then
                // (while it strikes the actor has "Aftershock": for what only an Aftershock's hits do)
                let again =
                    updateActor actor.Name (fun st -> { st with Buffs = Map.add "Aftershock" { Name = "Aftershock"; Mods = []; Stacks = 1; Expires = infinity } st.Buffs }) w
                    |> strike
                    |> updateActor actor.Name (fun st -> { st with Buffs = Map.remove "Aftershock" st.Buffs })
                { again with Counts = bump ("aftershock:" + sk.Name) again.Counts }
            else w
