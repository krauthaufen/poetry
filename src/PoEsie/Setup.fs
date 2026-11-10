namespace PoEsie

/// How the player plays: a priority list. The first step whose skill is ready and whose
/// condition holds is used. Skills are named by gem; each brings its own socket group's
/// supports. A play style is plain F#: `When` takes any condition on what the player sees.
type Play =
    /// Always, when nothing before it applies (the main skill).
    | Spam of gem: string
    /// These skills in turn.
    | Cycle of gems: string list
    /// Whenever ready. A skill that only grants Verisium infusions waits until they aren't full.
    | OnCooldown of gem: string
    /// When its buff is down; a charge-consuming buff (Charged Staff) also waits for full charges.
    | KeepBuff of gem: string
    /// While the player has fewer than `count` stacks of the resource ("PowerCharge").
    | WhileBelow of resource: string * count: int * gem: string
    /// A curse or mark: cast when the enemy aimed at doesn't have it.
    | KeepCurse of gem: string
    /// Any condition.
    | When of condition: (View -> bool) * gem: string

/// Knobs of a simulation that aren't the build (formerly CLI flags).
type Options =
    { /// Drop gems before the character is built: "Name" everywhere, "10:Name" from group 10 only.
      Without: string list
      /// Count bonded lines (idols) as active.
      Bonded: bool
      /// Seconds between Thaumaturgical Dynamism charges.
      ChargeGen: float
      /// Observed resource maxima override computed ones.
      MaxOverrides: (string * int) list
      /// Start with this many power charges instead of the maximum.
      StartPowerCharges: int option
      /// Start with no resources except power charges and rage.
      StartEmpty: bool
      /// Replace the character's chance to not consume charges.
      ChargeSkip: float option
      /// Unlimited infusions: infusing skills always use their "-Infused" stat sets.
      Infused: bool
      /// Seconds to collect a remnant.
      Pickup: float
      /// Invocations and infusions start at this fraction of their maximum; charge buffs start up if > 0.
      Banked: float
      KeptChargesPause: float
      SequentialUses: bool
      /// No incoming damage and no costs.
      Immortal: bool
      NoFlask: bool
      /// Ground skills land on the player (Cast on Self).
      CastOnSelf: bool
      Reach: float
      CascadeSpacing: float
      Targeting: Targeting
      MaxTime: float
      /// Projectile overrides for the main skill: several may hit the same enemy; spread in degrees.
      Shotgun: bool
      Spread: float option }

module Options =
    let defaults =
        { Without = []
          Bonded = false
          ChargeGen = 5.0
          MaxOverrides = []
          StartPowerCharges = None
          StartEmpty = false
          ChargeSkip = None
          Infused = false
          Pickup = 1.0
          Banked = 1.0
          KeptChargesPause = 1.0
          SequentialUses = false
          Immortal = false
          NoFlask = false
          CastOnSelf = false
          Reach = 15.0
          CascadeSpacing = 1.5
          Targeting = Strongest
          MaxTime = 60.0
          Shotgun = false
          Spread = None }

/// A loaded character: everything about the build that doesn't depend on how it's played.
type Loaded =
    { Db: GameDb
      Build: PobBuild
      Sheet: CharacterSheet
      Bases: Map<string, float * float * float>
      MaxResources: Map<string, int>
      Resources: (string * int) list
      Vitals: PlayerStats
      Periodics: Periodic list
      Cycles: Cycle list
      MoteMaxima: (string * int) list
      /// Persistent minions, and notes on minion skills left out.
      Minions: Actor list
      MinionNotes: string list }

module Setup =
    /// A build from its text: official character API JSON or PoB XML.
    /// (PoB build codes are zlib-compressed; decode them before.)
    let parseBuild (text: string) =
        if text.TrimStart().StartsWith "{" then Api.read text
        else Pob.read text, []

    let private dropGems (without: string list) (build: PobBuild) =
        let drops gi (name: string) =
            without |> List.exists (fun d ->
                match d.IndexOf ':' with
                | k when k > 0 && fst (System.Int32.TryParse(d.Substring(0, k))) -> int (d.Substring(0, k)) = gi + 1 && d.Substring(k + 1) = name
                | _ -> d = name)
        { build with Groups = build.Groups |> List.mapi (fun gi g -> { g with Gems = g.Gems |> List.filter (fun x -> not (drops gi x.Name)) }) }

    /// Build the character. `read` reads a game data file by name ("tree.json").
    let rec load (read: string -> string) (build: PobBuild) (o: Options) : Loaded =
        loadWith (GameDb.load read) (read "tree.json") build o

    /// The same with game data already loaded (many builds, one database).
    and loadWith (db: GameDb) (treeJson: string) (build: PobBuild) (o: Options) : Loaded =
        let tree = Tree.parseNodes treeJson
        let build = if o.Bonded then { build with Config = Map.add "bonded" "1" build.Config } else build
        let build = dropGems o.Without build
        let sheet = Pob.character db tree (Tree.jewelSlots treeJson) build |> fun s -> Pob.withPersistentBuffs db s build
        let bases = Defence.classBases treeJson
        // motes: base maximum from the mote-granting gem's description stat
        let moteBase =
            build.Groups
            |> List.tryPick (fun g ->
                g.Gems
                |> List.tryPick (fun gem ->
                    if gem.IsSupport then None
                    else
                        let raw = Gems.rawStats db (ModDb.ofList sheet.Mods) { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                        if raw |> List.exists (fun (k, _) -> k = "base_active_skill_buff_stack_gain_frequency_ms") then
                            raw |> List.tryPick (fun (k, v) -> if k = "skill_specific_stat_description_mode" then Some(int v) else None)
                        else None))
        let moteMax = match moteBase with Some b -> Behaviours.moteMaxima db build b | None -> []
        // what is held, for "while wielding ..." / "while Off Hand is empty" conditions
        let equipmentFlags =
            let offhand = build.Items |> List.tryFind (fun (slot, _) -> slot = "Weapon 2") |> Option.map snd
            let offhandWeapon = offhand |> Option.exists (fun t -> t.Split('\n') |> Array.exists (fun l -> Map.containsKey (l.Trim()) db.Weapons))
            let kinds = sheet.MainHand |> Option.map (fun w -> w.Tags |> List.choose (function Tag _ -> None | t -> Some(sprintf "Wielding%A" t))) |> Option.defaultValue []
            let oneHanded = sheet.MainHand |> Option.exists (fun w -> List.contains (Tag "OneHanded") w.Tags)
            kinds
            @ (if oneHanded && offhand.IsNone then [ "OneHandedEmptyOffHand" ] else [])
            @ (if offhandWeapon then [ "DualWielding" ] else [])
            @ (if offhand.IsSome && not offhandWeapon then [ "WieldingShield" ] else [])
            @ (if not (build.Items |> List.exists (fun (slot, _) -> slot = "Weapon 1")) then [ "Unarmed" ] else [])
        // socketed support gems by colour ("if you have at least 10 Red Support Gems socketed")
        let supportFlags =
            let colours =
                build.Groups |> List.filter (fun g -> g.Enabled)
                |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.IsSupport && x.Enabled))
                |> List.choose (fun x -> Gems.data db x.Name |> Option.map (fun d -> d.Color))
                |> List.countBy id
            let count c = colours |> List.tryPick (fun (k, n) -> if k = c then Some n else None) |> Option.defaultValue 0
            let named = [ 1, "red"; 2, "green"; 3, "blue" ]
            let most = named |> List.map (fun (c, _) -> count c) |> List.max
            [ for c, name in named do
                if count c >= 10 then yield "Supports10:" + name
                if most > 0 && count c = most then yield "SupportsMajority:" + name ]
        let maxResources = (Pob.maxResources sheet @ moteMax @ o.MaxOverrides @ ((equipmentFlags @ supportFlags) |> List.map (fun f -> f, 1))) |> Map.ofList
        let startResources =
            maxResources
            |> Map.map (fun k n ->
                match k, o.StartPowerCharges with
                | "PowerCharge", Some c -> c
                | _ when o.StartEmpty && k <> "PowerCharge" && k <> "Rage" -> 0
                | _ -> n)
        let vitals = Defence.computeWith startResources bases sheet
        // accuracy: 6 per level above the first and 6 per Dexterity (Character.ot, PoB AccuracyPerDexBase)
        let sheet =
            { sheet with Mods = sheet.Mods @ [ Mod.base' (Named "Accuracy") (6.0 * float (sheet.Level - 1) + 6.0 * vitals.Dex) |> Mod.from "level and Dexterity" ] }
        let periodics = Behaviours.periodics db sheet build o.ChargeGen
        let sheet =
            match o.ChargeSkip with
            | Some p -> { sheet with Mods = (sheet.Mods |> List.filter (fun m -> m.Stat <> Named "ChargeSkipConsume")) @ [ Mod.base' (Named "ChargeSkipConsume") p ] }
            | None -> sheet
        let cycles =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.choose (fun g ->
                match g.Gems |> List.tryFind (fun gem -> not gem.IsSupport && Gems.isPersistentBuff db gem.Name) with
                | Some a ->
                    Some
                        { Gem = a.Name; Level = a.Level; Quality = a.Quality; StatSet = a.StatSet
                          Supports = g.Gems |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level) }
                | None -> None)
            |> Behaviours.cycles db (ModDb.ofList sheet.Mods)
        let minionActors = Behaviours.minions db sheet build
        { Db = db
          Build = build
          Sheet = sheet
          Bases = bases
          // "per 100 maximum mana" (Archmage) reads this permanent stack count
          MaxResources = Map.add "MaxMana100" (int (vitals.Mana / 100.0)) maxResources
          Resources = (startResources |> Map.toList) @ [ "MaxMana100", int (vitals.Mana / 100.0) ]
          Vitals = vitals
          Periodics = periodics
          Cycles = cycles
          MoteMaxima = moteMax
          Minions = fst minionActors
          MinionNotes = snd minionActors }

    /// A skill the player uses, by gem name, with its group's supports: the skill, its
    /// infused copy (skill name, infusion, copy), Verisium infusions it grants, and the rules
    /// its use sets off (exposure, remnants, charge buffs).
    type Pressed =
        { Skill: Skill
          Infused: (string * string * Skill) option
          GrantsVerisium: int
          Rules: Rule list
          /// Mods the skill gives the character while it is in the build (a mark's bonuses
          /// against marked enemies).
          Grants: Mod list
          /// The debuff a curse or mark applies ("Curse:Despair").
          Curse: string option }

    let pressed (c: Loaded) (o: Options) (gemName: string) : Pressed option =
        let db, sheet = c.Db, c.Sheet
        let charMods = ModDb.ofList sheet.Mods
        c.Build.Groups |> List.tryPick (fun g ->
            match g.Gems |> List.tryFind (fun x -> x.Name = gemName && not x.IsSupport && x.Enabled) with
            | None -> None
            | Some gem ->
                let pick (x: PobGem) =
                    if o.Infused && x.Name = gemName then { x with StatSet = Gems.infusedSet db gemName |> Option.defaultValue x.StatSet } else x
                let g' = { g with Gems = g.Gems |> List.filter (fun x -> x.IsSupport || x.Name = gemName) |> List.map pick }
                match Pob.groupSkill db sheet g' |> List.tryHead with
                | Some (Ok (s, _)) ->
                    let supports = g.Gems |> List.filter (fun x -> x.IsSupport && x.Enabled) |> List.map (fun x -> x.Name, x.Level)
                    let setup = { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = supports }
                    let copy =
                        if o.Infused then None
                        else Behaviours.infusedCopy db charMods (Gems.chooseWeapon db setup.Gem sheet.Weapons) setup |> Option.map (fun (el, cp) -> s.Name, el, { (Behaviours.applyObserved cp) with Name = s.Name + " infused" })
                    let curse = Behaviours.curseOf db charMods setup s
                    let curseRules =
                        match curse with
                        | Some (name, duration, monster, _) -> [ { Hook = OnHit; Filter = Called s.Name; Chance = 100.0; Reaction = ApplyCurse(name, duration, monster) } ]
                        | None -> []
                    let rules = Behaviours.useRules db charMods setup s.Name o.Pickup @ Behaviours.buffRules db charMods setup s @ curseRules
                    let grants = Behaviours.verisiumGrant db charMods setup
                    let s = Behaviours.applyObserved s
                    // skills that deal no damage (buffs, infusion grants) don't hit
                    // (curses and marks do "hit": that is how they reach their targets)
                    let s = if grants > 0 || (curse.IsNone && s.BaseDamage.IsEmpty && s.Dots.IsEmpty && not (s.Tags.Contains Attack) && s.Parts.IsEmpty) then { s with Hits = [] } else s
                    Some
                        { Skill = s
                          Infused = copy
                          GrantsVerisium = grants
                          Rules = rules
                          Grants = (match curse with Some (_, _, _, player) -> player | None -> [])
                          Curse = curse |> Option.map (fun (name, _, _, _) -> name) }
                | _ -> None)

    let private gemOf (p: Play) =
        match p with
        | Spam g | OnCooldown g | KeepBuff g | KeepCurse g | WhileBelow (_, _, g) | When (_, g) -> [ g ]
        | Cycle gs -> gs

    /// A play style from skill types, for builds without one. Only what the simulator models is
    /// played: buffs it can grant are kept up, cooldown skills that hit or leave something
    /// (remnants, exposure, infusions) are used on cooldown, the main skill is spammed.
    /// Marks, warcries, travel skills and anything else stay unpressed.
    let auto (c: Loaded) (o: Options) (mainGem: string) : Play list =
        let others =
            c.Build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport))
            |> List.filter (fun x -> x.Name <> mainGem && not (Gems.isMeta c.Db x.Name) && not (Gems.isPersistentBuff c.Db x.Name))
            |> List.distinctBy (fun x -> x.Name)
            |> List.choose (fun x ->
                match Gems.data c.Db x.Name, pressed c o x.Name with
                | Some d, Some p ->
                    let skip = [ "Warcry"; "Travel"; "Minion"; "ModifiesNextSkill"; "Persistent"; "SummonsTotem" ] |> List.exists d.Types.Contains
                    let grantsBuff = p.Rules |> List.exists (fun r -> match r.Reaction with ChargeBuff _ | GrantBuff _ -> true | _ -> false)
                    let cooldown = p.Skill.Cooldown |> Option.defaultValue 0.0
                    if skip then None
                    elif p.Curse.IsSome then (if p.Rules |> List.exists (fun r -> match r.Reaction with ApplyCurse (_, _, ms) -> not ms.IsEmpty | _ -> false) || not p.Grants.IsEmpty then Some(KeepCurse x.Name) else None)
                    elif grantsBuff then Some(KeepBuff x.Name)
                    elif cooldown >= 1.0 && (not p.Skill.Hits.IsEmpty || not p.Rules.IsEmpty || p.GrantsVerisium > 0) then Some(OnCooldown x.Name)
                    else None
                | _ -> None)
        others @ [ Spam mainGem ]

    /// The player actor: the play style's skills, meta gems and their triggered spells,
    /// infusions, charges, defences. `main` replaces the play style's first skill (CLI
    /// overrides like --shotgun apply to it).
    let player (c: Loaded) (o: Options) (play: Play list) (main: (string * Skill) option) : Actor =
        let db, sheet = c.Db, c.Sheet
        let infusionMaxima = Behaviours.infusionMaxima sheet
        let metas = Behaviours.metas db sheet c.Build o.Infused
        let resolve g =
            match main with
            | Some (name, s) when name = g ->
                Some { (pressed c o g |> Option.defaultValue { Skill = s; Infused = None; GrantsVerisium = 0; Rules = []; Grants = []; Curse = None }) with Skill = s }
            | _ -> pressed c o g
        let used = play |> List.collect gemOf |> List.distinct |> List.choose (fun g -> resolve g |> Option.map (fun p -> g, p))
        let byGem g = used |> List.tryFind (fun (n, _) -> n = g) |> Option.map snd
        let nameOf g = byGem g |> Option.map (fun p -> p.Skill.Name) |> Option.defaultValue g
        let steps =
            play
            |> List.collect (fun p ->
                match p with
                | Spam g -> [ Actor.always (nameOf g) ]
                | Cycle gs ->
                    let names = gs |> List.map nameOf
                    names
                    |> List.mapi (fun i n ->
                        let previous = names.[(i + names.Length - 1) % names.Length]
                        { Use = n; When = fun v -> v.LastUsed = previous || (v.LastUsed = "" && i = 0) })
                    |> fun s -> s @ [ Actor.always names.Head ]
                | OnCooldown g ->
                    match byGem g with
                    | Some p when p.GrantsVerisium > 0 ->
                        let cap = Map.find "VerisiumInfusion" infusionMaxima
                        [ { Use = p.Skill.Name; When = fun v -> (Map.tryFind "VerisiumInfusion" v.Buffs |> Option.defaultValue 0) < cap } ]
                    | _ -> [ Actor.always (nameOf g) ]
                | KeepBuff g ->
                    match byGem g with
                    | Some p ->
                        let full =
                            p.Rules |> List.tryPick (fun r -> match r.Reaction with ChargeBuff (_, ch, _, _, _) -> Some ch | _ -> None)
                            |> Option.map (fun ch -> fun (v: View) -> (Map.tryFind ch v.Buffs |> Option.defaultValue 0) >= (Map.tryFind ch c.MaxResources |> Option.defaultValue 0))
                            |> Option.defaultValue (fun _ -> true)
                        [ { Use = p.Skill.Name; When = fun v -> not (v.Buffs.ContainsKey(p.Skill.Name + " buff")) && full v } ]
                    | None -> []
                | KeepCurse g ->
                    match byGem g with
                    | Some { Curse = Some name; Skill = sk } -> [ { Use = sk.Name; When = fun v -> not (v.EnemyStatus.ContainsKey name) } ]
                    | _ -> []
                | WhileBelow (r, n, g) -> [ { Use = nameOf g; When = fun v -> (Map.tryFind r v.Buffs |> Option.defaultValue 0) < n } ]
                | When (cond, g) -> [ { Use = nameOf g; When = cond } ])
        let pressedSkills = used |> List.map (fun (_, p) -> p.Skill)
        // a step whose skill couldn't be built is left out (the fight goes on without it)
        let steps =
            let known = pressedSkills |> List.map (fun s -> s.Name) |> Set.ofList
            steps |> List.filter (fun st -> known.Contains st.Use)
        let consumers =
            (used |> List.choose (fun (_, p) -> p.Infused)) @ (metas |> List.collect (fun (_, _, cs) -> cs))
        let grantRules =
            used
            |> List.filter (fun (_, p) -> p.GrantsVerisium > 0)
            |> List.map (fun (_, p) -> { Hook = OnUse; Filter = Called p.Skill.Name; Chance = 100.0; Reaction = GainResource("VerisiumInfusion", p.GrantsVerisium) })
        let infusionSetup =
            if o.Infused then None
            else Behaviours.infusions db sheet c.Build o.Pickup (consumers |> List.map (fun (n, el, cp) -> n, (el, cp.Name)) |> Map.ofList)
        { Actor.make "Player" (sheet.Mods @ (used |> List.collect (fun (_, p) -> p.Grants))) (pressedSkills @ (metas |> List.collect (fun (_, s, _) -> s)) @ (consumers |> List.map (fun (_, _, cp) -> cp))) with
            Rotation = steps
            Rules = grantRules @ (used |> List.collect (fun (_, p) -> p.Rules))
            Metas = metas |> List.map (fun (m, _, _) -> m)
            Infusions = infusionSetup
            // infusions start full (kept up while clearing), scaled like invocations
            Resources =
                c.Resources
                @ (match infusionSetup with
                   | Some inf ->
                       let usedInf = (consumers |> List.map (fun (_, el, _) -> el)) @ Option.toList inf.Wildcard |> List.distinct
                       usedInf |> List.map (fun r -> r, int (float (Map.find r infusionMaxima) * o.Banked))
                   | None -> [])
            Cycles = c.Cycles
            Vitals = (if o.Immortal then None else Some c.Vitals)
            VitalsAt = Some(fun current -> Defence.computeWith current c.Bases sheet)
            Periodics = c.Periodics
            MaxResources = Map.fold (fun m k v -> Map.add k v m) c.MaxResources infusionMaxima
            ResourceDurations = Map.fold (fun m k v -> Map.add k v m) (Behaviours.chargeDurations sheet) (Behaviours.infusionDurations db sheet c.Build)
            KeptChargesPause = o.KeptChargesPause
            // user: stored uses are "just two cooldowns"
            ParallelUses = not o.SequentialUses
            WardRemnants = Behaviours.wardRemnants db sheet c.Build o.Pickup
            Archons = [ Behaviours.elementalArchon sheet ]
            ManaFlask =
                if o.Immortal || o.NoFlask then None
                else
                    Defence.flask "Mana" (ModDb.ofList sheet.Mods) sheet.ManaFlask }

    /// A fight against these enemies.
    let fight (c: Loaded) (o: Options) (player: Actor) (enemies: EnemyDef list) : Fight =
        // auras that weaken monsters near you (Overwhelming Presence); presence range is ignored
        let db = ModDb.ofList c.Sheet.Mods
        let aura stat name =
            let k = Mods.more Ctx.empty [ Named name ] db
            if k <> 1.0 then [ Mod.more stat ((k - 1.0) * 100.0) |> Mod.from "aura" ] else []
        let auraMods = aura AilmentThreshold "EnemyAura:AilmentThreshold" @ aura PoiseThreshold "EnemyAura:PoiseThreshold"
        let enemies = if auraMods.IsEmpty then enemies else enemies |> List.map (fun e -> { e with Mods = e.Mods @ auraMods })
        { Enemies = enemies
          Player = player
          Minions = c.Minions
          Targeting = o.Targeting
          Reach = (if o.CastOnSelf then 0.0 else o.Reach)
          CascadeSpacing = o.CascadeSpacing
          BankedInvocations = o.Banked
          Walls = []
          MaxTime = o.MaxTime }
