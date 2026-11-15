namespace PoEtry

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
      /// The character is kept on low life (35% of the maximum), whatever its recovery says.
      LowLife: bool
      /// The weapon set the character sheet (pools, defences) is for: the second, the first, or
      /// (None) the one whose weapons are stronger.
      SheetSetTwo: bool option
      /// Start with this many power charges instead of the maximum.
      StartPowerCharges: int option
      /// Start with no resources except power charges and rage.
      StartEmpty: bool
      /// Resources that begin the fight at these counts, whatever else is assumed about them.
      Start: (string * int) list
      /// Dead enemies are replaced (see `Fight.Replace`).
      Replace: bool
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
          LowLife = false
          SheetSetTwo = None
          StartPowerCharges = None
          StartEmpty = false
          Start = []
          Replace = false
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
      Charms: Charm list
      /// Spirit: the total, and what each persistent gem reserves (minion gems: per minion).
      Spirit: float
      SpiritUses: Behaviours.SpiritUse list
      /// Persistent minions, and notes on minion skills left out.
      Minions: Actor list
      MinionNotes: string list
      /// The character fights with weapon set two in hand: its defences in the fight are that
      /// set's (`Vitals` is already).
      SheetSetTwo: bool }

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

    /// The game's own character data gives a skill gem's level with what its supports add
    /// (a Mastery's level, Dialla's Desire, and so its quality): here a gem has its own, and the supports'
    /// levels and quality are added where the skill is built (so a support turned off takes its level along).
    let ownLevels (db: GameDb) (build: PobBuild) =
        if not (Map.containsKey "levelsWithSupports" build.Config) then build
        else
            { build with
                Config = Map.remove "levelsWithSupports" build.Config
                Groups =
                    build.Groups |> List.map (fun g ->
                        let supports = g.Gems |> List.filter (fun x -> x.Enabled && x.IsSupport) |> List.map (fun x -> x.Name, x.Level)
                        { g with
                            Gems =
                                g.Gems |> List.map (fun x ->
                                    if x.IsSupport then x
                                    else
                                        let setup = { Gem = x.Name; Level = x.Level; Quality = x.Quality; StatSet = 1; Supports = supports }
                                        { x with Level = max 1 (x.Level - Gems.supportLevels db setup); Quality = max 0 (x.Quality - Gems.supportQuality db setup) }) }) }

    /// Build the character. `read` reads a game data file by name ("tree.json").
    let rec load (read: string -> string) (build: PobBuild) (o: Options) : Loaded =
        loadWith (GameDb.load read) (read "tree.json") build o

    /// The same with game data already loaded (many builds, one database).
    and loadWith (db: GameDb) (treeJson: string) (build: PobBuild) (o: Options) : Loaded =
        loadParsed db (Tree.parseNodes treeJson) (Tree.jewelSlots treeJson) (Defence.classBases treeJson) build o

    /// The same with the tree read already (one character loaded again and again: a front end).
    and loadParsed (db: GameDb) (tree: Map<int, TreeNode>) (jewelSlots: int list) (classBases: Map<string, float * float * float>) (build: PobBuild) (o: Options) : Loaded =
        let build = if o.Bonded then { build with Config = Map.add "bonded" "1" build.Config } else build
        let build = ownLevels db build
        let build = dropGems o.Without build
        let sheet = Pob.character db tree jewelSlots build |> fun s -> Pob.withPersistentBuffs db s build
        // charms: their effects are sheet mods that hold while the charm lasts
        let charms, charmMods = Behaviours.charms db sheet build
        let sheet = { sheet with Mods = sheet.Mods @ charmMods }
        // named effects the character has outright, and what their stacks add
        let sheet = { sheet with Mods = sheet.Mods @ Behaviours.keywordMods sheet }
        let sheet = { sheet with Mods = sheet.Mods @ Behaviours.minionIncreasesOnYou sheet }
        let bases = classBases
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
            // "while wielding a Two Handed Weapon"
            @ (if sheet.MainHand |> Option.exists (fun w -> List.contains (Tag "TwoHanded") w.Tags) then [ "WieldingTwoHandedWeapon" ] else [])
            @ (if offhand.IsNone then [ "OffHandIsEmpty" ] else [])
            // "while affected by Herald of Ash": every persistent buff gem in the build
            @ (build.Groups |> List.filter (fun g -> g.Enabled)
               |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport && Gems.isPersistentBuff db x.Name))
               |> List.map (fun x -> "AffectedBy" + (x.Name.Split(' ') |> Array.map (fun w -> if w = "" then w else string (System.Char.ToUpperInvariant w.[0]) + w.Substring 1) |> String.concat "")))
            // "while affected by a Herald": a herald among them
            @ (if build.Groups |> List.exists (fun g -> g.Enabled && g.Gems |> List.exists (fun x -> x.Enabled && not x.IsSupport && Gems.isPersistentBuff db x.Name && (Gems.data db x.Name |> Option.exists (fun d -> d.Types.Contains "Herald")))) then [ "AffectedByAHerald" ] else [])
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
                if count c >= 5 then yield "Supports5:" + name
                if most > 0 && count c = most then yield "SupportsMajority:" + name ]
        // "for each Corrupted Item Equipped"
        let corrupted = Behaviours.corruptedItems build
        let spectrums = build.Items |> List.filter (fun (slot, text) -> slot.StartsWith "Jewel" && text.Contains "Grand Spectrum") |> List.length
        // what sits in the items' sockets: idols, and the other augments (runes, soul cores)
        let augments, idols =
            let count (prefix: string) =
                build.Items |> List.sumBy (fun (_, text) ->
                    text.Split('\n') |> Array.sumBy (fun l -> let l = l.Trim() in if l.StartsWith prefix then (match System.Int32.TryParse(l.Substring(prefix.Length).Trim()) with | true, n -> n | _ -> 0) else 0))
            count "Sockets filled:", count "Idols socketed:"
        // a flask whose effect is constant: "during any Flask effect" always holds
        let flaskFlags =
            let sdb = ModDb.ofList sheet.Mods
            [ for kind in [ "Mana"; "Life" ] do
                if Mods.flag Ctx.empty (Named("ConstantFlask:" + kind)) sdb then
                    yield "Constant" + kind + "Flask", 1 ]
        let maxResources =
            (Pob.maxResources sheet @ flaskFlags @ moteMax @ o.MaxOverrides @ ((equipmentFlags @ supportFlags) |> List.map (fun f -> f, 1)) @ (if corrupted > 0 then [ "CorruptedItems", corrupted ] else [])
             @ (if spectrums > 0 then [ "GrandSpectrum", spectrums ] else [])
             @ (if idols > 0 then [ "SocketedIdols", idols ] else []) @ (if augments - idols > 0 then [ "NonIdolAugments", augments - idols ] else []))
            |> Map.ofList
        let startResources =
            maxResources
            |> Map.map (fun k n ->
                match k, o.StartPowerCharges with
                | _ when o.Start |> List.exists (fun (x, _) -> x = k) -> max 0 (min n (o.Start |> List.find (fun (x, _) -> x = k) |> snd))
                | "PowerCharge", Some c -> c
                | _ when o.StartEmpty && k <> "PowerCharge" && k <> "Rage" -> 0
                | _ -> n)
        // the sheet's own life and attributes, for Spirit that depends on them
        let sheet =
            let v = Defence.computeWith startResources bases sheet
            { sheet with Mods = sheet.Mods @ ([ "Life", v.Life; "Str", v.Str; "Dex", v.Dex; "Int", v.Int ] |> List.map (fun (k, x) -> Mod.base' (Named("Sheet:" + k)) x |> Mod.from "sheet")) }
        // support gems that pay off on low life ("... while on Low Life") show the build means to be there
        let startResources =
            let lowLifeGem =
                build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems) |> List.filter (fun x -> x.Enabled)
                |> List.exists (fun x ->
                    Gems.data db x.Name |> Option.exists (fun d ->
                        d.StatSets |> List.exists (fun st -> st.Stats |> List.exists (fun id -> id.Contains "on_low_life" && not (id.Contains "enem")))))
            if lowLifeGem then Map.add "LowLifeIntent" 1 startResources else startResources
        // total Spirit as a stack count, for "per 1 Spirit"
        let spirit = int (Behaviours.spiritTotal sheet build)
        let maxResources, startResources = Map.add "Spirit" spirit maxResources, Map.add "Spirit" spirit startResources
        // The character sheet is the one of the weapon set the character fights with: set two when
        // its weapon is the stronger one (the API does not say which set is in hand).
        let sheetSetTwo =
            match o.SheetSetTwo with
            | Some chosen -> chosen
            | None ->
               (let strength (w: Weapon) = (w.Damage |> Map.toList |> List.sumBy (fun (_, r) -> (r.Min + r.Max) / 2.0)) * w.AttacksPerSecond
                let one = sheet.Weapons |> List.filter (fun w -> not (List.contains (Tag "WeaponSet2") w.Tags)) |> List.sumBy strength
                let two = sheet.Weapons |> List.filter (fun w -> List.contains (Tag "WeaponSet2") w.Tags) |> List.sumBy strength
                two > one)
        // Wind Dancer's stages build up by themselves: full on the sheet and when a fight starts
        let maxResources, startResources =
            match Behaviours.windDancerStages db sheet build with
            | Some n -> Map.add "WindDancerStage" n maxResources, Map.add "WindDancerStage" (if o.StartEmpty then 0 else n) startResources
            | None -> maxResources, startResources
        // "while your Companion is in your Presence": a character with a companion gem has it
        // beside it for the whole fight
        let startResources =
            let companion =
                build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems)
                |> List.exists (fun x -> x.Enabled && not x.IsSupport && (x.Name.StartsWith "Companion: " || (Gems.data db x.Name |> Option.exists (fun d -> d.Types.Contains "Companion"))))
            if companion then Map.add "CompanionInPresence" 1 startResources else startResources
        // and so do Arctic Armour's
        let maxResources, startResources =
            match Behaviours.arcticArmour db sheet build with
            | Some (_, _, _, n) -> Map.add "ArcticArmourStage" n maxResources, Map.add "ArcticArmourStage" (if o.StartEmpty then 0 else n) startResources
            | None -> maxResources, startResources
        let vitals = Defence.computeWith (if sheetSetTwo then Map.add "WeaponSetTwo" 1 startResources else startResources) bases sheet
        let vitals = if o.LowLife && vitals.Life > 1.0 then { vitals with StartLife = 0.35 } else vitals
        // life reserved by skills ("Reserve Life instead of Spirit") and by stats: what is left is all
        // the life there is, so a character with 65% or more reserved is on low life for good
        let vitals =
            let reserved = min 0.99 (vitals.LifeReserved + (Behaviours.spiritUses db sheet build |> List.sumBy (fun u -> u.LifePercent)) / 100.0)
            if reserved > 0.0 && vitals.Life > 1.0 then { vitals with LifeReserved = reserved; StartLife = min vitals.StartLife (1.0 - reserved) } else vitals
        // accuracy: 6 per level above the first and 6 per Dexterity (Character.ot, PoB AccuracyPerDexBase)
        let sheet =
            { sheet with
                Mods =
                    sheet.Mods
                    @ [ Mod.base' (Named "Accuracy") (6.0 * float (sheet.Level - 1) + 6.0 * vitals.Dex) |> Mod.from "level and Dexterity" ]
                    // "Gain Accuracy Rating equal to your Intelligence"
                    @ (if Mods.flag Ctx.empty (Named "AccuracyFromIntelligence") (ModDb.ofList sheet.Mods) then [ Mod.base' (Named "Accuracy") vitals.Int |> Mod.from "Intelligence" ] else []) }
        let periodics = Behaviours.periodics db sheet build o.ChargeGen
        // a Fragment of Divinity lies ready (one at a time: how many can lie about is not in the data)
        let maxResources, startResources =
            if Mods.sumBase Ctx.empty [ Named "DivineFragmentEvery" ] (ModDb.ofList sheet.Mods) > 0.0 then Map.add "DivineFragment" 1 maxResources, Map.add "DivineFragment" 1 startResources
            else maxResources, startResources
        // bolts that build up by themselves: full at the start
        let maxResources, startResources =
            match Behaviours.storedBolts db sheet build with
            | Some (n, _) -> Map.add "StoredBolts" n maxResources, Map.add "StoredBolts" n startResources
            | None -> maxResources, startResources
        // Ghost Dance: shrouds as stacks (full at the start), spent when hit
        let ghost = Behaviours.ghostDance db sheet build
        let maxResources, startResources =
            match ghost with
            | Some (n, _) -> Map.add "GhostShroud" n maxResources, Map.add "GhostShroud" n startResources
            | None -> maxResources, startResources
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
        let minionActors =
            let ms, mNotes = Behaviours.minions db sheet build
            let ts, tNotes = Behaviours.totems db sheet build
            let spirits, _ = Behaviours.ancestralSpirits db sheet build vitals.Str vitals.Dex ts
            ms @ ts @ spirits, mNotes @ tNotes
        { Db = db
          Build = build
          Sheet = sheet
          Bases = bases
          // "per 100 maximum mana" (Archmage) reads this permanent stack count
          // the character's own numbers as stacks, for "per 100 maximum Mana", "per 15 Strength"
          MaxResources = maxResources |> Map.add "MaxMana100" (int (vitals.Mana / 100.0)) |> Map.add "BlockChance" (int vitals.Block) |> Map.add "LowestAttribute" (int (min vitals.Str (min vitals.Dex vitals.Int))) |> Map.add "Str" (int vitals.Str) |> Map.add "Dex" (int vitals.Dex) |> Map.add "Int" (int vitals.Int)
          Resources = (startResources |> Map.toList) @ [ "MaxMana100", int (vitals.Mana / 100.0); "BlockChance", int vitals.Block; "LowestAttribute", int (min vitals.Str (min vitals.Dex vitals.Int)); "Str", int vitals.Str; "Dex", int vitals.Dex; "Int", int vitals.Int ]
          Vitals = vitals
          Periodics = periodics
          Cycles = cycles
          MoteMaxima = moteMax
          Charms = charms
          Spirit = Behaviours.spiritTotal sheet build
          SpiritUses = Behaviours.spiritUses db sheet build
          Minions = fst minionActors
          MinionNotes = snd minionActors
          SheetSetTwo = sheetSetTwo }

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
          Curse: string option
          /// Timed hits of one use (skills that keep hitting), instead of the skill's own.
          Pattern: (HitTiming * float) list option
          /// Skills its supports grant and trigger (Impending Doom's Doom Blast).
          Extra: Skill list
          /// What the skill sets going on a clock of its own (Entangle's vines).
          Periodics: Periodic list }

    /// The dodge roll as something the player can press (`Spam Setup.dodgeRoll`): it deals
    /// nothing and takes its time; Cast on Dodge gains energy from the distance rolled.
    let dodgeRoll = "Dodge Roll"

    let rec pressed (c: Loaded) (o: Options) (gemName: string) : Pressed option =
        if gemName = dodgeRoll then
            Some
                { Skill = { Skill.make dodgeRoll [ Tag "DodgeRoll" ] with BaseTime = Constants.dodgeRollSeconds; Hits = [] }
                  Infused = None; GrantsVerisium = 0; Rules = []; Grants = []; Curse = None; Pattern = None; Extra = []; Periodics = [] }
        else pressedGem c o gemName

    and private pressedGem (c: Loaded) (o: Options) (gemName: string) : Pressed option =
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
                    // "Curse zones erupt after a delay": the curse lands that much after the cast
                    let curseDelay = if curse.IsSome then Behaviours.curseDelay db charMods setup s else 0.0
                    let s = if curseDelay > 0.0 then { s with HitDelay = s.HitDelay + curseDelay } else s
                    // a curse that explodes when replaced: the check comes before the new curse lands
                    let doom = curse |> Option.bind (fun (name, _, _, _) -> Behaviours.impendingDoom db charMods setup s name)
                    let curseRules =
                        match curse with
                        | Some (name, duration, monster, _) ->
                            (doom |> Option.map snd |> Option.toList)
                            @ [ { Hook = OnHit; Filter = Called s.Name; Chance = 100.0; Reaction = ApplyCurse(name, duration, monster) } ]
                            // on its way: not cast again before it lands
                            @ (if curseDelay > 0.0 then [ { Hook = OnUse; Filter = Called s.Name; Chance = 100.0; Reaction = ApplyDebuff("Pending " + name, s.BaseTime + curseDelay, 1, []) } ] else [])
                        | None -> []
                    let lasting = Behaviours.sustained db charMods setup s
                    let cryRules, cryGrants = Behaviours.warcry db charMods setup s
                    let s, cryRules =
                        match Behaviours.tempestBell db charMods setup s with
                        | Some (s', rs) -> s', cryRules @ rs
                        | None -> s, cryRules
                    let s, cryRules, cryGrants =
                        match Behaviours.demonForm db charMods setup s with
                        | Some (s', rs, ms) -> s', cryRules @ rs, cryGrants @ ms
                        | None -> s, cryRules, cryGrants
                    let cryRules = cryRules @ Behaviours.sigilOfPower db charMods setup s @ Behaviours.markBuff db charMods setup s @ Behaviours.offering db charMods setup s @ Behaviours.summoner db charMods setup s @ Behaviours.triskelion db charMods setup s @ Behaviours.toxicDomain db charMods setup s
                    // parts that happen when the skill kills (Crushing Fear's shockwave)
                    let cryRules =
                        cryRules
                        @ (s.Parts |> List.filter (fun p -> p.Mods |> List.exists (fun m -> m.Stat = Named "TriggeredOnKill"))
                           |> List.map (fun p -> { Hook = OnKill; Filter = Called s.Name; Chance = 100.0; Reaction = TriggerSkill p.Name }))
                    let empowered, empRules, empGrants = Behaviours.empowerNext db charMods (Gems.chooseWeapon db setup.Gem sheet.Weapons) setup s
                    let cryRules, cryGrants = cryRules @ empRules, cryGrants @ empGrants
                    let tipped = Behaviours.iceTipped db charMods (Gems.chooseWeapon db setup.Gem sheet.Weapons) setup s
                    let cryRules = cryRules @ (match tipped with Some (_, rs, _) -> rs | None -> [])
                    let cryGrants = cryGrants @ (match tipped with Some (_, _, ms) -> ms | None -> [])
                    let wave = Behaviours.chargedShockwave db charMods (Gems.chooseWeapon db setup.Gem sheet.Weapons) setup s
                    let cryRules = cryRules @ (match wave with Some (_, rs) -> rs | None -> [])
                    let biteRules =
                        let withParts (x: Skill) = x.Name :: (x.Parts |> List.map (fun p -> p.Name))
                        let names = withParts s @ (match copy with Some (_, _, cp) -> withParts cp | None -> [])
                        Behaviours.iceBite db charMods setup s names @ Behaviours.blazingCritical db charMods setup s names
                    let biteRules =
                        let withParts (x: Skill) = x.Name :: (x.Parts |> List.map (fun p -> p.Name))
                        biteRules @ Behaviours.jaggedGround db charMods setup s (withParts s @ (match copy with Some (_, _, cp) -> withParts cp | None -> []))
                        @ Behaviours.oilGround db charMods setup s @ Behaviours.whirlwind db setup s
                    let rules = cryRules @ biteRules @ (match lasting with Some (_, rs) -> rs | None -> []) @ Behaviours.useRules db charMods setup s.Name o.Pickup @ Behaviours.buffRules db charMods setup s @ Behaviours.totemPlacement db charMods setup s @ curseRules
                    let grants = Behaviours.verisiumGrant db charMods setup
                    let s = Behaviours.applyObserved s
                    // Entangle: the vines are a skill of their own, on a clock, against what a fissure hit
                    let vines = Behaviours.entangle db charMods setup s
                    let s = match vines with Some (main, _, _, _) -> main | None -> s
                    let rules = rules @ (match vines with Some (_, _, r, _) -> [ r ] | None -> [])
                    // skills that deal no damage (buffs, infusion grants) don't hit
                    // (curses and marks do "hit": that is how they reach their targets)
                    let s = if grants > 0 || (curse.IsNone && s.BaseDamage.IsEmpty && s.Dots.IsEmpty && not (s.Tags.Contains Attack) && s.Parts.IsEmpty) then { s with Hits = [] } else s
                    Some
                        { Skill = s
                          Infused = copy
                          GrantsVerisium = grants
                          Rules = rules
                          Grants = (match curse with Some (_, _, _, player) -> player | None -> []) @ cryGrants
                          Curse = curse |> Option.map (fun (name, _, _, _) -> name)
                          Pattern = lasting |> Option.map fst
                          Extra = (doom |> Option.map fst |> Option.toList) @ (match tipped with Some (f, _, _) -> [ f ] | None -> []) @ (match wave with Some (f, _) -> [ f ] | None -> []) @ empowered @ (match vines with Some (_, v, _, _) -> [ v ] | None -> [])
                          Periodics = (match vines with Some (_, _, _, p) -> [ p ] | None -> []) }
                | _ -> None)

    let gemOf (p: Play) =
        match p with
        | Spam g | OnCooldown g | KeepBuff g | KeepCurse g | WhileBelow (_, _, g) | When (_, g) -> [ g ]
        | Cycle gs -> gs

    /// A play style from skill types, for builds without one. Only what the simulator models is
    /// played: buffs it can grant are kept up, cooldown skills that hit or leave something
    /// (remnants, exposure, infusions) are used on cooldown, the main skill is spammed.
    /// Marks, warcries, travel skills and anything else stay unpressed.
    let rec auto (c: Loaded) (o: Options) (mainGem: string) : Play list =
        let isTotem (name: string) = Gems.data c.Db name |> Option.exists (fun d -> d.Types.Contains "SummonsTotem")
        let inOwnGroup =
            c.Build.Groups
            |> List.filter (fun g -> g.Enabled && not (g.Gems |> List.exists (fun x -> x.Enabled && not x.IsSupport && Gems.isMeta c.Db x.Name)))
            |> List.collect (fun g -> g.Gems |> List.map (fun x -> x.Name))
        let others =
            c.Build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport))
            |> List.filter (fun x -> x.Name <> mainGem && (not (Gems.isMeta c.Db x.Name) || isTotem x.Name) && not (Gems.isPersistentBuff c.Db x.Name))
            // a gem socketed in a trigger or aura gem is used by that gem, not pressed
            // (a totem that uses socketed skills is itself placed by pressing it)
            |> List.filter (fun x -> List.contains x.Name inOwnGroup || isTotem x.Name)
            |> List.distinctBy (fun x -> x.Name)
            |> List.choose (fun x ->
                match Gems.data c.Db x.Name, pressed c o x.Name with
                | Some d, Some p ->
                    // a weapon's default attack (Grenade, Bow Shot) is what one presses for want of a
                    // skill, never something kept on cooldown beside the main skill
                    let isDefaultAttack =
                        Map.tryFind x.Name c.Db.GemVariants |> Option.defaultValue [] |> List.exists (fun g -> g.GameId.Contains "PlayerDefault")
                    // (an offering is a minion skill, but one to press)
                    let offering = d.Types.Contains "Offering" && not p.Rules.IsEmpty
                    let skip = isDefaultAttack || (not offering && ([ "Travel"; "Minion"; "ModifiesNextSkill"; "Persistent" ] |> List.exists d.Types.Contains))
                    let empowers = p.Rules |> List.exists (fun r -> match r.Reaction with Empower _ -> true | _ -> false)
                    // (a buff the skill's use gives: not one that comes of what its hits inflict, like Ice Bite's)
                    let grantsBuff = p.Rules |> List.exists (fun r -> match r.Hook, r.Reaction with OnInflict _, _ -> false | _, (ChargeBuff _ | GrantBuff _) -> true | _ -> false)
                    let cooldown = p.Skill.Cooldown |> Option.defaultValue 0.0
                    let needsBuilding = p.Skill.Mods |> List.exists (fun m -> match m.Stat with Named n -> (n.StartsWith "Requires:" || n.StartsWith "NeedsEnemyStacks:") && m.Value > 0.0 | _ -> false)
                    // Barrage is readied whenever it can be, for a main skill it can repeat
                    let barrage = p.Skill.Mods |> List.exists (fun m -> m.Stat = Named "BarrageRepeats" && m.Value > 0.0)
                    let mainRepeats = Gems.data c.Db mainGem |> Option.exists (fun m -> m.Types.Contains "Barrageable")
                    let tipped = p.Rules |> List.exists (fun r -> match r.Reaction with SetStacks (b, _) -> b.StartsWith "EmpoweredRepeatable:" | _ -> false)
                    // a warp is used into the main skill's projectile whenever there is one
                    let warps = p.Skill.Mods |> List.exists (fun m -> m.Stat = Named("WarpsInto:" + mainGem))
                    // a skill that summons temporary minions is used while fewer than can be are out
                    // (Unearth: and there is something to raise them from)
                    let summons =
                        p.Rules |> List.tryPick (fun r ->
                            match r.Reaction with
                            | WithCorpse _ -> Some false
                            | SummonPerCorpse _ -> Some true
                            | _ -> None)
                    let most = c.Minions |> List.filter (fun m -> m.Pool = Some x.Name) |> List.length
                    let count key (v: View) = Map.tryFind key v.Buffs |> Option.defaultValue 0
                    if summons.IsSome && most > 0 then
                        let needsCorpses = summons.Value
                        Some(When((fun v -> count ("Out:" + x.Name) v < most && (not needsCorpses || count "Corpses" v > 0)), x.Name))
                    elif warps then Some(OnCooldown x.Name)
                    elif barrage || tipped then (if mainRepeats then Some(OnCooldown x.Name) else None)
                    elif skip then None
                    // a skill that needs Glory or Combo is used whenever it has enough
                    elif needsBuilding then Some(OnCooldown x.Name)
                    // a warcry is worth pressing when it empowers attacks
                    elif d.Types.Contains "Warcry" then (if empowers then Some(OnCooldown x.Name) else None)
                    elif p.Curse.IsSome then (if p.Rules |> List.exists (fun r -> match r.Reaction with ApplyCurse (_, _, ms) -> not ms.IsEmpty | _ -> false) || not p.Grants.IsEmpty then Some(KeepCurse x.Name) else None)
                    elif grantsBuff then Some(KeepBuff x.Name)
                    elif cooldown >= 1.0 && (not p.Skill.Hits.IsEmpty || not p.Rules.IsEmpty || p.GrantsVerisium > 0) then Some(OnCooldown x.Name)
                    else None
                | _ -> None)
        // only as many curses as can be on a monster at once (more would overwrite each other
        // for ever), and one mark
        let others =
            let curseLimit = 1 + int (Mods.sumBase Ctx.empty [ Named "MaxCurses" ] (ModDb.ofList c.Sheet.Mods))
            let kind g = pressed c o g |> Option.bind (fun p -> p.Curse) |> Option.map (fun (n: string) -> n.Substring(0, n.IndexOf ':'))
            others
            |> List.fold
                (fun (kept, curses, marks) p ->
                    match p with
                    | KeepCurse g ->
                        match kind g with
                        | Some "Curse" when curses < curseLimit -> p :: kept, curses + 1, marks
                        | Some "Mark" when marks < 1 -> p :: kept, curses, marks + 1
                        | _ -> kept, curses, marks
                    | _ -> p :: kept, curses, marks)
                ([], 0, 0)
            |> fun (kept, _, _) -> List.rev kept
        // a main "skill" that only places a totem or grants a buff is kept up, not spammed
        let main =
            match pressed c o mainGem with
            | Some p when (p.Skill.Hits.IsEmpty || p.Pattern.IsSome) && p.Rules |> List.exists (fun r -> match r.Hook, r.Reaction with OnInflict _, _ -> false | _, (ChargeBuff _ | GrantBuff _) -> true | _ -> false) -> KeepBuff mainGem
            | _ -> Spam mainGem
        let plain =
            // a main skill with a cooldown of its own goes first whenever it is ready: otherwise the
            // other cooldown skills would fill every gap and it would never be used
            match pressed c o mainGem with
            | Some p when p.Skill.Mods |> List.exists (fun m -> match m.Stat with Named n -> (n.StartsWith "Requires:" || n.StartsWith "NeedsEnemyStacks:") && m.Value > 0.0 | _ -> false) ->
                // the main skill needs Glory, Combo or a pile on the enemy: it is used when there is
                // enough, and the weapon's plain attack (if the character has it) builds in between
                let filler =
                    c.Build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems) |> List.filter (fun x -> x.Enabled && not x.IsSupport && x.Name <> mainGem)
                    |> List.tryFind (fun x -> (Map.tryFind x.Name c.Db.GemVariants |> Option.defaultValue [] |> List.exists (fun g -> g.GameId.Contains "PlayerDefault")) && (pressed c o x.Name).IsSome)
                // a pile of lances grows with every spear hit: the spear skill to build it with is
                // the one that does best in a short fight beside the main skill
                let stacker =
                    if not (p.Skill.Mods |> List.exists (fun m -> match m.Stat with Named n -> n.StartsWith "NeedsEnemyStacks:" | _ -> false)) then None
                    else
                        c.Build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems) |> List.filter (fun x -> x.Enabled && not x.IsSupport && x.Name <> mainGem && not (Gems.isPersistentBuff c.Db x.Name))
                        |> List.filter (fun x -> Gems.data c.Db x.Name |> Option.exists (fun d -> not (d.Types.Contains "Persistent") && not (d.Types.Contains "Triggered")))
                        |> List.choose (fun x -> pressed c o x.Name |> Option.map (fun q -> x.Name, q))
                        |> List.filter (fun (_, q) -> q.Skill.Tags.Contains Spear && q.Skill.Tags.Contains Attack && not q.Skill.Hits.IsEmpty && q.Skill.Cooldown.IsNone)
                        |> List.map (fun (g, _) ->
                            let short = { o with MaxTime = min o.MaxTime 10.0 }
                            let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
                            let out = Sim.run (fight c short (player c short [ OnCooldown mainGem; Spam g ] None) boss) 1u
                            g, out.DamageBy |> Map.toList |> List.sumBy snd)
                        |> List.sortByDescending snd
                        |> List.tryHead |> Option.map fst
                match stacker with
                | Some g -> OnCooldown mainGem :: (others |> List.filter (function Spam x | OnCooldown x -> x <> g | _ -> true)) @ [ Spam g ]
                | None -> OnCooldown mainGem :: others @ (filler |> Option.map (fun x -> Spam x.Name) |> Option.toList)
            | Some p when p.Skill.Cooldown.IsSome && (match main with Spam _ -> true | _ -> false) -> OnCooldown mainGem :: others @ [ main ]
            | _ -> others @ [ main ]

        // a main skill that consumes a Whirlwind (Twister): the whirlwind is built to the
        // number of stages that does best in a short fight (none, if that is best)
        let builder =
            c.Build.Groups |> List.filter (fun g -> g.Enabled) |> List.collect (fun g -> g.Gems)
            |> List.filter (fun x -> x.Enabled && not x.IsSupport && x.Name <> mainGem)
            |> List.tryFind (fun x -> pressed c o x.Name |> Option.exists (fun q -> q.Rules |> List.exists (fun r -> r.Reaction = GainResource("Whirlwind", 1))))
        let stages = Behaviours.whirlwindStages c.Db c.Build
        match builder, pressed c o mainGem with
        | Some b, Some p when p.Skill.Consumes = Some "Whirlwind" && stages > 0 ->
            let before (k: int) =
                let rec go (steps: Play list) =
                    match steps with
                    | (Spam g as s) :: rest when g = mainGem -> WhileBelow("Whirlwind", k, b.Name) :: s :: rest
                    | s :: rest -> s :: go rest
                    | [] -> []
                go (plain |> List.filter (function Spam g | OnCooldown g -> g <> b.Name | _ -> true))
            let short = { o with MaxTime = min o.MaxTime 10.0 }
            let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
            let dealt (play: Play list) = (Sim.run (fight c short (player c short play None) boss) 1u).DamageBy |> Map.toList |> List.sumBy snd
            (plain, dealt plain) :: [ for k in 1 .. stages -> let pl = before k in pl, dealt pl ]
            |> List.fold (fun (best, most) (pl, d) -> if d > most then pl, d else best, most) (plain, -infinity)
            |> fst
        | _ -> plain

    /// The player actor: the play style's skills, meta gems and their triggered spells,
    /// infusions, charges, defences. `main` replaces the play style's first skill (CLI
    /// overrides like --shotgun apply to it).
    and player (c: Loaded) (o: Options) (play: Play list) (main: (string * Skill) option) : Actor =
        let db, sheet = c.Db, c.Sheet
        let infusionMaxima = Behaviours.infusionMaxima sheet
        let metas = Behaviours.metas db sheet c.Build o.Infused
        let resolve g =
            match main with
            | Some (name, s) when name = g ->
                Some { (pressed c o g |> Option.defaultValue { Skill = s; Infused = None; GrantsVerisium = 0; Rules = []; Grants = []; Curse = None; Pattern = None; Extra = []; Periodics = [] }) with Skill = s }
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
                    | Some { Curse = Some name; Skill = sk } -> [ { Use = sk.Name; When = fun v -> not (v.EnemyStatus.ContainsKey name || v.EnemyStatus.ContainsKey("Pending " + name)) } ]
                    | _ -> []
                | WhileBelow (r, n, g) -> [ { Use = nameOf g; When = fun v -> (Map.tryFind r v.Buffs |> Option.defaultValue 0) < n } ]
                | When (cond, g) -> [ { Use = nameOf g; When = cond } ])
        let pressedSkills = used |> List.map (fun (_, p) -> p.Skill)
        // a step whose skill couldn't be built is left out (the fight goes on without it)
        let steps =
            let known = pressedSkills |> List.map (fun s -> s.Name) |> Set.ofList
            steps |> List.filter (fun st -> known.Contains st.Use)
        let heraldSkills, heraldRules = Behaviours.heralds db sheet c.Build
        let starSkills, starPeriodics = Behaviours.starsAnswer db sheet c.Build c.Vitals.Mana
        let heraldSkills = heraldSkills @ starSkills
        let ashSkills, overkillBurns = Behaviours.heraldOfAsh db sheet c.Build
        let heraldSkills = heraldSkills @ ashSkills
        let windDancer = Behaviours.windDancer db sheet c.Build
        let heraldSkills = heraldSkills @ (match windDancer with Some (s, _, _, _) -> s | None -> [])
        // skills an item triggers on hit: built like a pressed gem, used only by their rule
        let itemTriggered =
            Behaviours.itemTriggers sheet (c.Vitals.Str, c.Vitals.Dex, c.Vitals.Int)
            |> List.choose (fun (name, rule) ->
                pressed c o name |> Option.map (fun p ->
                    { p.Skill with Tags = p.Skill.Tags.Add Triggered; Cost = 0.0 }, { rule with Reaction = TriggerSkill p.Skill.Name }))
        let heraldSkills = heraldSkills @ (itemTriggered |> List.map fst)
        let heraldRules = heraldRules @ (itemTriggered |> List.map snd)
        // Thorns: the hit that retaliates when a monster hits the player
        let thorns = Behaviours.thorns sheet
        let heraldSkills = heraldSkills @ (thorns |> Option.map fst |> Option.toList)
        // Arctic Armour: the burst a melee hit on the player sets off
        let arctic = Behaviours.arcticArmour db sheet c.Build
        let heraldSkills = heraldSkills @ (match arctic with Some (s, _, _, _) -> [ s ] | None -> [])
        let consumers =
            (used |> List.choose (fun (_, p) -> p.Infused)) @ (metas |> List.collect (fun (_, _, cs) -> cs))
        let grantRules =
            used
            |> List.filter (fun (_, p) -> p.GrantsVerisium > 0)
            |> List.map (fun (_, p) -> { Hook = OnUse; Filter = Called p.Skill.Name; Chance = 100.0; Reaction = GainResource("VerisiumInfusion", p.GrantsVerisium) })
        let infusionSetup =
            if o.Infused then None
            else Behaviours.infusions db sheet c.Build o.Pickup (consumers |> List.map (fun (n, el, cp) -> n, (el, cp.Name)) |> Map.ofList)
        // Ice Nova cast on a Frostbolt: when the play style uses Frostbolt, its bolt is
        // followed (it flies at the speed of the game's Projectiles table, x the skill's
        // projectile speed, for the skill's duration), and a nova of a gem that "casts on" one
        // can be cast on it (`Sim.onStrike`)
        let onBolts (skills: Skill list) =
            match used |> List.tryFind (fun (g, _) -> g = "Frostbolt") with
            | None -> skills
            | Some (_, fb) ->
                let fctx = { Ctx.empty with SkillName = fb.Skill.Name; SkillTags = fb.Skill.Tags }
                let fdb = ModDb.ofList (sheet.Mods @ fb.Skill.Mods)
                let speed = Constants.frostboltSpeed * Mods.mult fctx [ Named "ProjectileSpeed" ] fdb
                let life = (if fb.Skill.Duration > 0.0 then fb.Skill.Duration else Constants.frostboltLife) * Mods.mult fctx [ SkillDuration ] fdb
                let castsOnBolt (gem: string) =
                    Gems.rawStats db (ModDb.ofList sheet.Mods) { Gem = gem; Level = 1; Quality = 0; StatSet = 1; Supports = [] }
                    |> List.exists (fun (k, v) -> k = "ice_nova_number_of_frost_bolts_to_cast_on" && v > 0.0)
                let novas = used |> List.map fst |> List.filter castsOnBolt |> List.map (fun g -> Tag("Gem:" + g)) |> Set.ofList
                skills |> List.map (fun (sk: Skill) ->
                    if sk.Name = fb.Skill.Name then { sk with Mods = sk.Mods @ [ Mod.flag (Named "LaunchesBolt") ] }
                    elif sk.Tags |> Set.exists novas.Contains then
                        { sk with Mods = sk.Mods @ [ Mod.base' (Named "NovaOnBoltSpeed") speed; Mod.base' (Named "NovaOnBoltLife") life ] }
                    else sk)
        // Combat Frenzy: a Frenzy Charge for a freeze, electrocute or pin, once every few seconds
        let frenzy = Behaviours.combatFrenzy db sheet c.Build
        // "X% increased Glory generation": every amount a skill builds
        let gloryGeneration (skills: Skill list) =
            skills |> List.map (fun (sk: Skill) ->
                let ctx = { Ctx.empty with SkillName = sk.Name; SkillTags = sk.Tags }
                let k = Mods.inc ctx [ Named "GloryGeneration" ] (ModDb.ofList (sheet.Mods @ sk.Mods))
                if k = 1.0 then sk
                else { sk with Mods = sk.Mods |> List.map (fun m -> match m.Stat with Named n when n.StartsWith "Builds:Glory" -> { m with Value = m.Value * k } | _ -> m) })
        // "Skills deal X% increased Damage per Combo consumed, up to 40%": a skill that requires
        // Combo consumes that much at every use
        let comboConsumed (skills: Skill list) =
            skills |> List.map (fun (sk: Skill) ->
                let ctx = { Ctx.empty with SkillName = sk.Name; SkillTags = sk.Tags }
                let db = ModDb.ofList (sheet.Mods @ sk.Mods)
                let per = Mods.sumBase ctx [ Named "DamagePerComboConsumed" ] db
                let combo = sk.Mods |> List.sumBy (fun m -> if m.Stat = Named "Requires:Combo" && m.Kind = Base then m.Value else 0.0)
                let falloff = Mods.inc ctx [ Named "ComboFalloff" ] db
                let sk =
                    if combo > 0.0 && falloff <> 1.0 then { sk with Mods = sk.Mods @ [ Mod.base' (Named "ComboWindow") (Constants.comboWindow / max 0.01 falloff) |> Mod.from "Combo lost faster" ] }
                    else sk
                if per = 0.0 || combo <= 0.0 then sk
                else
                    let inc = if per > 0.0 then min 40.0 (per * combo) else max -40.0 (per * combo)
                    { sk with Mods = sk.Mods @ [ Mod.inc (Damage AllDamage) inc |> Mod.from "Combo consumed" ] })
        { Actor.make "Player" (sheet.Mods @ (used |> List.collect (fun (_, p) -> p.Grants))) (comboConsumed << gloryGeneration << onBolts <| (pressedSkills @ (metas |> List.collect (fun (_, s, _) -> s)) @ (consumers |> List.map (fun (_, _, cp) -> cp)) @ heraldSkills @ (used |> List.collect (fun (_, p) -> p.Extra)))) with
            Rotation = steps
            OverkillBurns = overkillBurns
            HitPattern = used |> List.choose (fun (_, p) -> p.Pattern |> Option.map (fun pat -> p.Skill.Name, pat)) |> Map.ofList
            Rules =
                (frenzy |> Option.map snd |> Option.defaultValue [])
                @
                // ("Buffs on you expire X% faster": the buffs these give the character)
                Behaviours.buffsExpire sheet (grantRules @ (used |> List.collect (fun (_, p) -> p.Rules)) @ heraldRules @ Behaviours.keywordRules sheet pressedSkills)
                @ Behaviours.selfIgnite sheet
                @ Behaviours.rareModsOnKill sheet
                @ Behaviours.totemRecently sheet pressedSkills
                @ Behaviours.recentStates sheet pressedSkills
                @ (thorns |> Option.map snd |> Option.toList)
                @ (match arctic with Some (_, r, _, _) -> r | None -> [])
                @ Behaviours.scavengedPlating db sheet c.Build
                @ Behaviours.fullBreakRules pressedSkills
                @ (match windDancer with Some (_, r, _, _) -> r | None -> [])
                @ Behaviours.shockedGround sheet
                @ fst (Behaviours.summons db sheet c.Build pressedSkills)
                @ snd (Behaviours.ancestralSpirits db sheet c.Build c.Vitals.Str c.Vitals.Dex (c.Minions |> List.filter (fun m -> m.ActiveWhile |> Option.exists (fun b -> b.EndsWith " buff"))))
                @ (match Behaviours.ghostDance db sheet c.Build with
                   | Some (_, percent) when percent > 0.0 ->
                       let rate = c.Vitals.Evasion * percent / 100.0 * Mods.mult Ctx.empty [ Named "ESRecoveryRate" ] (ModDb.ofList sheet.Mods)
                       [ { Hook = WhenHit; Filter = AnySkill; Chance = 100.0; Reaction = SpendStack("GhostShroud", RegenES(rate, 4.0)) } ]
                   | _ -> [])
                // (an effect the character has all the time is not given again for a while)
                |> (let permanent = Behaviours.permanentEffects sheet
                    if permanent.IsEmpty then id
                    else List.filter (fun r -> match r.Reaction with GrantBuff (n, _, _, _) -> not (List.contains n permanent) | _ -> true))
            Metas = metas |> List.map (fun (m, _, _) -> m)
            Infusions = infusionSetup
            // infusions start full (kept up while clearing), scaled like invocations
            Resources =
                // rage is only there if something on the character or its skills generates it
                (let sources = [ Named "RageOnHit"; Named "RageWhenHit"; Named "RageOnKill"; Named "RageRegen" ]
                 let has (ms: Mod list) = ms |> List.exists (fun m -> List.contains m.Stat sources && m.Value > 0.0)
                 if has sheet.Mods || (used |> List.exists (fun (_, p) -> has p.Skill.Mods)) then c.Resources
                 else c.Resources |> List.map (fun (k, n) -> if k = "Rage" then k, 0 else k, n))
                // "Onslaught", "You have Arcane Surge": the state for the whole fight
                @ (Behaviours.permanentEffects sheet |> List.map (fun name -> name, 1))
                // shapeshift skills are used in animal form: "while Shapeshifted" holds
                @ (if used |> List.exists (fun (g, _) -> Gems.data db g |> Option.exists (fun d -> d.Types.Contains "Shapeshift")) then [ "Shapeshifted", 1 ] else [])
                // "while moving": a character that gains from moving moves, where the skills it
                // keeps using can be used on the move
                @ (let gains (ms: Mod list) = ms |> List.exists (fun m -> m.Value > 0.0 && m.Kind <> Flag && List.contains (WhileBuff "Moving") m.Conds)
                   let spammed = play |> List.collect (function Spam g -> [ g ] | Cycle gs -> gs | _ -> [])
                   let onTheMove (g: string) = Gems.data db g |> Option.exists (fun d -> d.Types.Contains "UsableWhileMoving")
                   if not spammed.IsEmpty && spammed |> List.forall onTheMove && (gains sheet.Mods || used |> List.exists (fun (_, p) -> gains p.Skill.Mods)) then [ "Moving", 1 ]
                   else [])
                @ (if frenzy.IsSome then [ "CombatFrenzyReady", 1 ] else [])
                // a Whirlwind the character builds is built up before the fight (it lasts while
                // the character stands in it), like a charge that needs no kill
                @ (let stages = Behaviours.whirlwindStages db c.Build
                   if stages > 0 && not o.StartEmpty && used |> List.exists (fun (_, p) -> p.Rules |> List.exists (fun r -> r.Reaction = GainResource("Whirlwind", 1))) then [ "Whirlwind", stages ] else [])
                // wind skills that count as boosted by elemental ground: by the first of them
                // only, unless they can be boosted by several
                @ (let flag n = Mods.flag Ctx.empty (Named n) (ModDb.ofList sheet.Mods)
                   let boosts = DamageType.elemental |> List.filter (fun e -> flag ("WindBoost:" + DamageType.name e))
                   (if flag "WindBoostMultiple" then boosts else List.truncate 1 boosts) |> List.map (fun e -> "BoostedBy:" + DamageType.name e, 1))
                @ (match infusionSetup with
                   | Some inf ->
                       let usedInf = (consumers |> List.map (fun (_, el, _) -> el)) @ Option.toList inf.Wildcard |> List.distinct
                       usedInf |> List.map (fun r -> r, int (float (Map.find r infusionMaxima) * o.Banked))
                   | None -> [])
            Cycles = c.Cycles
            Vitals = (if o.Immortal then None else Some c.Vitals)
            // (with the weapon set in hand: the buffs of the moment don't say which it is)
            VitalsAt = Some(fun current -> Defence.computeWith (if c.SheetSetTwo then Map.add "WeaponSetTwo" 1 current else current) c.Bases sheet)
            VitalsReads = Some(Defence.reads sheet)
            Periodics = c.Periodics @ (used |> List.collect (fun (_, p) -> p.Periodics)) @ (match arctic with Some (_, _, p, _) -> [ p ] | None -> []) @ snd (Behaviours.summons db sheet c.Build pressedSkills) @ starPeriodics @ (frenzy |> Option.map fst |> Option.toList) @ (match windDancer with Some (_, _, p, _) -> p | None -> [])
            MaxResources =
                Map.fold (fun m k v -> Map.add k v m) c.MaxResources infusionMaxima |> (if frenzy.IsSome then Map.add "CombatFrenzyReady" 1 else id)
                |> (let stages = Behaviours.whirlwindStages db c.Build
                    if stages > 0 then Map.add "Whirlwind" stages else id)

            ResourceDurations = Map.fold (fun m k v -> Map.add k v m) (Behaviours.chargeDurations sheet) (Behaviours.infusionDurations db sheet c.Build)
            KeptChargesPause = o.KeptChargesPause
            // user: stored uses are "just two cooldowns"
            ParallelUses = not o.SequentialUses
            WardRemnants = Behaviours.wardRemnants db sheet c.Build o.Pickup
            ManaRemnants = Behaviours.manaRemnants db sheet c.Build o.Pickup
            Banners = Behaviours.banners db sheet c.Build
            Archons = [ (let a = Behaviours.elementalArchon sheet in { a with Duration = a.Duration / Behaviours.buffExpiry sheet }) ]
            Charms = (if o.Immortal || o.NoFlask then [] else c.Charms)
            ManaFlask =
                if o.Immortal || o.NoFlask then None
                else
                    Defence.flask "Mana" (ModDb.ofList sheet.Mods) sheet.ManaFlask }
        // Affinity: what the rules and periodic effects gain is the other charge
        |> fun (a: Actor) ->
            let rules, periodics = Behaviours.affinity sheet a.Rules a.Periodics
            { a with Rules = rules; Periodics = periodics }

    /// A fight against these enemies.
    and fight (c: Loaded) (o: Options) (player: Actor) (enemies: EnemyDef list) : Fight =
        // auras that weaken monsters in your Presence (Overwhelming Presence)
        let db = ModDb.ofList c.Sheet.Mods
        let aura stat name =
            let k = Mods.more Ctx.empty [ Named name ] db
            if k <> 1.0 then [ Mod.more stat ((k - 1.0) * 100.0) |> Mod.from "aura" ] else []
        let auraMods = aura AilmentThreshold "EnemyAura:AilmentThreshold" @ aura PoiseThreshold "EnemyAura:PoiseThreshold"
        // a condition on the enemy being in the Presence ("against targets in your Presence")
        // is the status "InPresence": there only for a character that asks
        let asksPresence =
            let asks (ms: Mod list) = ms |> List.exists (fun m -> m.Conds |> List.exists (function EnemyHas "InPresence" | Not(EnemyHas "InPresence") -> true | _ -> false))
            asks player.Mods || player.Skills |> List.exists (fun s -> asks s.Mods)
        let curses, curseGrants = Behaviours.curseAuras c.Db c.Sheet c.Build
        let player = if curseGrants.IsEmpty then player else { player with Mods = player.Mods @ curseGrants }
        // what a killed rare's modifiers give (Headhunter): nothing in a fight where no enemy
        // carries any, so the modifiers that wait on their buffs are left out there (they
        // would only be asked about at every turn)
        let player =
            let rec rare (c: Cond) =
                match c with
                | WhileBuff b -> b.StartsWith(RareMods.buffName "")
                | Not inner | OfOwner inner -> rare inner
                | _ -> false
            let waits (m: Mod) = m.Conds |> List.exists rare
            if enemies |> List.exists (fun e -> not e.Affixes.IsEmpty) || not (player.Mods |> List.exists waits) then player
            else
                let sheet = { c.Sheet with Mods = c.Sheet.Mods |> List.filter (waits >> not) }
                { player with
                    Mods = player.Mods |> List.filter (waits >> not)
                    // (whether the recovery rates vary in the fight is asked of the modifiers that are left:
                    // with those that wait on a rare's modifiers, both engines would work the defences out
                    // anew at every recovery)
                    Vitals = player.Vitals |> Option.map (fun v -> { v with RatesVary = (Defence.computeWith (if c.SheetSetTwo then Map.ofList [ "WeaponSetTwo", 1 ] else Map.empty) c.Bases sheet).RatesVary })
                    VitalsAt = player.VitalsAt |> Option.map (fun _ -> fun current -> Defence.computeWith (if c.SheetSetTwo then Map.add "WeaponSetTwo" 1 current else current) c.Bases sheet)
                    VitalsReads = player.VitalsReads |> Option.map (fun _ -> Defence.reads sheet) }
        // What the character brings into the fight. Charges: only those the build can generate,
        // and against a lone boss (its introduction outlasts anything gained from the last kill)
        // only those that need no kill. Rage runs out during such an introduction unless it
        // regenerates by itself.
        let player =
            let lone = enemies.Length <= 1
            let sources = Behaviours.chargeSources c.Db c.Sheet c.Build
            // (with Affinity a source of one charge is a source of the one it becomes)
            let sources =
                if Mods.flag Ctx.empty (Named "ChargeCycle") db then sources |> Map.toList |> List.map (fun (k, v) -> Behaviours.affinityCharge k, v) |> Map.ofList
                else sources
            let rageRegen = Mods.sumBase Ctx.empty [ Named "RageRegen" ] db > 0.0
            { player with
                Resources =
                    player.Resources |> List.map (fun (k, n) ->
                        match k with
                        | _ when o.Start |> List.exists (fun (x, _) -> x = k) ->
                            k, max 0 (min (Map.tryFind k player.MaxResources |> Option.defaultValue n) (o.Start |> List.find (fun (x, _) -> x = k) |> snd))
                        // Conduit: the character's charges are its allies'
                        | "PowerCharge" | "FrenzyCharge" | "EnduranceCharge" when Mods.flag Ctx.empty (Named("Conduit:" + k)) db -> k, 0
                        | "PowerCharge" when o.StartPowerCharges.IsSome -> k, n
                        | "PowerCharge" | "FrenzyCharge" | "EnduranceCharge" ->
                            (match Map.tryFind k sources with
                             | Some free when free || not lone -> k, n
                             | _ -> k, 0)
                        | "Rage" when lone && not rageRegen -> k, 0
                        | _ -> k, n) }
        // how many persistent minions there are, and of how many different kinds ("per Minion",
        // "for each different type of Persistent Minion in your Presence"); a dead one still counts
        let persistent = c.Minions |> List.filter (fun m -> m.Pool.IsNone && m.ActiveWhile.IsNone)
        let kind (m: Actor) = m.Name.TrimEnd([| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]).TrimEnd()
        let isCompanion (m: Actor) = m.Skills |> List.exists (fun s -> s.Tags.Contains(Tag "Companion"))
        let minionCounts =
            [ "ActiveMinions", persistent.Length
              "PersistentMinionTypes", persistent |> List.map kind |> List.distinct |> List.length
              "CompanionTypes", persistent |> List.filter isCompanion |> List.map kind |> List.distinct |> List.length ]
            |> List.filter (fun (_, n) -> n > 0)
        let counted (a: Actor) =
            { a with Resources = a.Resources @ minionCounts; MaxResources = minionCounts |> List.fold (fun acc (k, n) -> Map.add k n acc) a.MaxResources }
        let player = counted player
        { Enemies = enemies
          // (Blasphemy's curses are an aura of their own size, which is not looked at)
          EnemyAuras = curses
          Presence = Behaviours.presenceRadius c.Sheet
          PresenceAuras =
            // (of two sources of Critical Weakness the one with more stacks)
            ((Behaviours.criticalWeaknessAura c.Sheet @ Behaviours.malice c.Db c.Sheet c.Build) |> List.sortByDescending (fun (_, n, _) -> n) |> List.truncate 1)
            @ Behaviours.witheringPresence c.Db c.Sheet c.Build
            @ (if auraMods.IsEmpty then [] else [ "Overwhelming Presence", 1, auraMods ])
            @ Behaviours.presenceAuras c.Sheet
            @ (if asksPresence then [ "InPresence", 1, [] ] else [])
          Player = player
          // a totem uses its skill with the character's modifiers, so it also counts with the
          // character's numbers ("per 100 maximum Mana", "per 15 Strength", socketed gem colours)
          Minions =
            (let carried =
                player.Resources |> List.filter (fun (k, _) ->
                    List.contains k [ "MaxMana100"; "Str"; "Dex"; "Int"; "Spirit"; "BlockChance"; "LowestAttribute"; "GrandSpectrum"; "CorruptedItems" ]
                    || k.StartsWith "Supports" || k.StartsWith "AffectedBy" || k.StartsWith "Constant")
             c.Minions |> List.map (fun m ->
                 if m.ActiveWhile.IsSome && m.Resources.IsEmpty then
                     { m with Resources = carried; MaxResources = carried |> List.fold (fun acc (k, n) -> Map.add k n acc) m.MaxResources }
                 else counted m))
          Targeting = o.Targeting
          Reach = (if o.CastOnSelf then 0.0 else o.Reach)
          CascadeSpacing = o.CascadeSpacing
          BankedInvocations = o.Banked
          Replace = o.Replace
          Walls = []
          MaxTime = o.MaxTime }

    /// The skill gems the player could press: enabled active gems that aren't triggers,
    /// persistent buffs or socketed in a trigger gem.
    and pressable (c: Loaded) : string list =
        c.Build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.filter (fun g -> not (g.Gems |> List.exists (fun x -> x.Enabled && not x.IsSupport && Gems.isMeta c.Db x.Name)))
        |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport && not (Gems.isPersistentBuff c.Db x.Name)))
        // a skill that is only ever triggered (by an item, a support, another skill) can't be pressed
        |> List.filter (fun x -> not (Gems.data c.Db x.Name |> Option.exists (fun d -> d.Types.Contains "Triggered")))
        // nor can a persistent minion gem: its minions are simply there
        |> List.filter (fun x ->
            not (x.Name.StartsWith "Spectre: " || x.Name.StartsWith "Companion: ")
            && not (Gems.data c.Db x.Name |> Option.exists (fun d -> d.Types.Contains "Persistent" && d.Types.Contains "Minion")))
        |> List.map (fun x -> x.Name)
        // and totems that use socketed skills: pressing them places the totem
        |> fun own ->
            own
            @ (c.Build.Groups |> List.filter (fun g -> g.Enabled)
               |> List.collect (fun g -> g.Gems |> List.filter (fun x -> x.Enabled && not x.IsSupport && Gems.isMeta c.Db x.Name && (Gems.data c.Db x.Name |> Option.exists (fun d -> d.Types.Contains "SummonsTotem"))))
               |> List.map (fun x -> x.Name))
        |> List.distinct

    /// The gem that deals the most damage when it is the one pressed (everything else
    /// played by `auto`): a short fight per candidate against these enemies. For builds
    /// whose damage comes from triggers this finds the skill that feeds them best.
    and strongest (c: Loaded) (o: Options) (enemies: EnemyDef list) : string option =
        let o = { o with MaxTime = min o.MaxTime 10.0 }
        // something that deals damage itself comes first; curses, marks and buffs are only
        // candidates when nothing else is
        let candidates =
            let all = pressable c |> List.choose (fun g -> pressed c o g |> Option.map (fun p -> g, p))
            let damaging =
                all |> List.filter (fun (_, p) ->
                    not p.Extra.IsEmpty
                    || p.Curse.IsNone && not p.Skill.Hits.IsEmpty
                    && (not p.Skill.BaseDamage.IsEmpty || p.Skill.Tags.Contains Attack || not p.Skill.Dots.IsEmpty || not p.Skill.Parts.IsEmpty))
            let rolls = Behaviours.metas c.Db c.Sheet c.Build o.Infused |> List.exists (fun (m, _, _) -> m.Sources |> List.exists (function DodgeRolls _ -> true | _ -> false))
            ((if damaging.IsEmpty then all else damaging) |> List.map fst) @ (if rolls then [ dodgeRoll ] else [])
        candidates
        |> List.choose (fun g ->
            match pressed c o g with
            | Some _ ->
                let f = fight c o (player c o (auto c o g) None) enemies
                let out = Sim.run f 1u
                Some(g, (out.DamageBy |> Map.toList |> List.sumBy snd) / max 0.1 out.Duration)
            | None -> None)
        |> List.sortByDescending snd
        |> List.tryHead
        |> Option.map fst
