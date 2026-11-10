namespace PoEsie

/// A gem in a PoB socket group.
type PobGem =
    { Name: string
      Level: int
      Quality: int
      Enabled: bool
      IsSupport: bool
      StatSet: int }

type PobGroup =
    { Label: string
      Slot: string
      Enabled: bool
      Gems: PobGem list }

/// The parts of a PoB build PoEsie uses. Only PoB's *inputs* are read (items, tree,
/// gems, level); none of PoB's computed numbers.
type PobBuild =
    { Level: int
      ClassName: string
      Ascendancy: string
      /// Slot name -> raw item text.
      Items: (string * string) list
      TreeNodes: int list
      Groups: PobGroup list
      MainGroup: int
      /// PoB configuration inputs, name -> value.
      Config: Map<string, string>
      /// Passive nodes whose stats differ from the tree data (chosen attributes).
      TreeOverrides: Map<int, string list>
      /// Character-wide lines from elsewhere (quest rewards).
      ExtraLines: string list }

/// A character ready to simulate: global mods, main-hand weapon, how well we understood it.
type CharacterSheet =
    { Mods: Mod list
      MainHand: Weapon option
      /// Main-hand weapons of weapon set one and two; a skill uses the first that fits it.
      /// The second carries the tag "WeaponSet2".
      Weapons: Weapon list
      /// Armour / evasion / energy shield shown on the equipped items: (slot, stat, value).
      ItemDefences: (string * Stat * float) list
      Level: int
      ClassName: string
      /// Mana flask: (amount, duration, charges per use, max charges) and its "increased charges gained".
      ManaFlask: ((float * float * float * float) * float) option
      /// Life flask: values and its own "increased Charges gained".
      LifeFlask: ((float * float * float * float) * float) option
      Coverage: Coverage }

module Pob =
    let private int' (s: string option) d =
        match s with
        | Some v ->
            match System.Int32.TryParse v with
            | true, x -> x
            | _ -> d
        | None -> d

    let read (xmlText: string) : PobBuild =
        let root = Xml.parse xmlText
        let build = Xml.child "Build" root
        let items =
            match Xml.child "Items" root with
            | None -> []
            | Some itemsNode ->
                let byId =
                    Xml.children "Item" itemsNode
                    |> List.choose (fun it -> Xml.attr "id" it |> Option.map (fun id -> id, it.Text))
                    |> Map.ofList
                let activeSet = Xml.attr "activeItemSet" itemsNode
                let sets = Xml.children "ItemSet" itemsNode
                let set =
                    sets
                    |> List.tryFind (fun s -> Xml.attr "id" s = activeSet)
                    |> Option.orElse (List.tryHead sets)
                // Older files keep slots directly under <Items>.
                let slots = match set with Some s -> Xml.children "Slot" s | None -> Xml.children "Slot" itemsNode
                slots
                |> List.choose (fun sl ->
                    match Xml.attr "name" sl, Xml.attr "itemId" sl with
                    | Some name, Some id when id <> "0" -> Map.tryFind id byId |> Option.map (fun t -> name, t)
                    | _ -> None)
        let spec =
            Xml.child "Tree" root
            |> Option.bind (fun t ->
                let active = Xml.attr "activeSpec" t
                let specs = Xml.children "Spec" t
                match active with
                | Some a -> specs |> List.tryItem (int' (Some a) 1 - 1)
                | None -> List.tryHead specs)
        let nodes =
            spec
            |> Option.bind (Xml.attr "nodes")
            |> Option.map (fun s -> s.Split(',') |> Array.choose (fun x -> match System.Int32.TryParse(x.Trim()) with | true, v -> Some v | _ -> None) |> List.ofArray)
            |> Option.defaultValue []
        let skills = Xml.child "Skills" root
        let skillSet =
            skills
            |> Option.bind (fun s ->
                let active = Xml.attr "activeSkillSet" s
                let sets = Xml.children "SkillSet" s
                sets |> List.tryFind (fun x -> Xml.attr "id" x = active) |> Option.orElse (List.tryHead sets))
        let groupNodes =
            match skillSet, skills with
            | Some s, _ -> Xml.children "Skill" s
            | None, Some s -> Xml.children "Skill" s
            | _ -> []
        let groups =
            groupNodes
            |> List.map (fun g ->
                { Label = Xml.attr "label" g |> Option.defaultValue ""
                  Slot = Xml.attr "slot" g |> Option.defaultValue ""
                  Enabled = Xml.attr "enabled" g <> Some "false"
                  Gems =
                    Xml.children "Gem" g
                    |> List.map (fun gem ->
                        let skillId = Xml.attr "skillId" gem |> Option.defaultValue ""
                        let gemId = Xml.attr "gemId" gem |> Option.defaultValue ""
                        { Name = Xml.attr "nameSpec" gem |> Option.defaultValue ""
                          Level = int' (Xml.attr "level" gem) 20
                          Quality = int' (Xml.attr "quality" gem) 20
                          Enabled = Xml.attr "enabled" gem <> Some "false"
                          IsSupport = gemId.Contains "SupportGem" || skillId.StartsWith "Support" || skillId.Contains "SupportPlayer"
                          StatSet = int' (Xml.attr "statSetIndex" gem) 1 }) })
        let config =
            Xml.descendants "Input" root
            |> List.choose (fun i ->
                match Xml.attr "name" i with
                | Some name ->
                    let v =
                        Xml.attr "boolean" i
                        |> Option.orElse (Xml.attr "number" i)
                        |> Option.orElse (Xml.attr "string" i)
                        |> Option.defaultValue ""
                    Some(name, v)
                | None -> None)
            |> Map.ofList
        { Level = build |> Option.bind (Xml.attr "level") |> fun l -> int' l 1
          ClassName = build |> Option.bind (Xml.attr "className") |> Option.defaultValue ""
          Ascendancy = build |> Option.bind (Xml.attr "ascendClassName") |> Option.defaultValue ""
          Items = items
          TreeNodes = nodes
          Groups = groups
          MainGroup = build |> Option.bind (Xml.attr "mainSocketGroup") |> fun m -> int' m 1
          Config = config
          TreeOverrides = Map.empty
          ExtraLines = [] }

    /// Slots whose items always apply. Flasks and charms only apply while active.
    let private alwaysOn (slot: string) =
        not (slot.StartsWith "Flask" || slot.StartsWith "Charm")

    /// Weapon-set items only count while a skill uses their set: set two's skills carry the
    /// tag "WeaponSet2" (from their weapon); everything else runs on set one.
    let private setTwo = SkillIs(Tag "WeaponSet2")
    let private slotCondition (slot: string) (m: Mod) =
        if slot.Contains "Swap" then Mod.whenever setTwo m
        elif slot.StartsWith "Weapon" then Mod.whenever (Not setTwo) m
        else m

    /// Rune / idol lines prefixed "Bonded:" only work for Shamans, or on an item that says
    /// "Idols socketed in this item gain the benefits of their Bonded modifiers"
    /// (Darkness Enthroned). The values the game reports already include the item's
    /// increased effect of socketed items.
    let private resolveBonded (build: PobBuild) (slot: string) (text: string) =
        let slotIsBody = slot = "Body Armour"
        let lines = text.Split('\n')
        let enabled =
            (build.ClassName + build.Ascendancy).Contains "Shaman"
            || Map.containsKey "bonded" build.Config
            || (lines |> Array.exists (fun l -> l.Contains "gain the benefits of their Bonded modifiers")
                && (slotIsBody || lines |> Array.exists (fun l -> l.Contains "as though it was a Body Armour")))
        lines
        |> Array.choose (fun l ->
            let t = l.Trim()
            if t.StartsWith "Bonded: " then (if enabled then Some(t.Substring 8) else None)
            elif t.Contains "gain the benefits of their Bonded modifiers" then None
            else Some l)
        |> String.concat "\n"

    /// "Allocates <notable>" on an item (anoints, jewels) allocates that passive.
    let private allocatedByItems (tree: Map<int, TreeNode>) (texts: string list) =
        texts
        |> List.collect (fun t -> t.Split('\n') |> List.ofArray)
        |> List.choose (fun l ->
            let l = l.Trim()
            if l.StartsWith "Allocates " then
                let name = l.Substring 10
                tree |> Map.tryPick (fun id n -> if n.Name = name && (n.IsNotable || n.IsKeystone) then Some id else None)
            else None)

    /// Character mods from equipped items and allocated passives.
    let character (db: GameDb) (tree: Map<int, TreeNode>) (build: PobBuild) : CharacterSheet =
        let items = build.Items |> List.filter (fst >> alwaysOn) |> List.map (fun (slot, text) -> slot, resolveBonded build slot text)
        let extraNodes = allocatedByItems tree (List.map snd items)
        let build = { build with TreeNodes = build.TreeNodes @ extraNodes }
        let parsed = items |> List.map (fun (slot, text) -> slot, Items.parse db text)
        let mainHand = parsed |> List.tryFind (fun (slot, _) -> slot = "Weapon 1") |> Option.bind (fun (_, p) -> p.Weapon)
        let swapHand =
            parsed |> List.tryFind (fun (slot, _) -> slot = "Weapon 1 Swap") |> Option.bind (fun (_, p) -> p.Weapon)
            |> Option.map (fun w -> { w with Tags = Tag "WeaponSet2" :: w.Tags })
        // A weapon's local mods are already in its Weapon; only its global mods go to the sheet.
        let itemMods = parsed |> List.collect (fun (slot, p) -> p.Mods |> List.map (slotCondition slot))
        let itemCov =
            parsed
            |> List.map (fun (slot, p) -> { p.Coverage with UnknownText = p.Coverage.UnknownText |> List.map (fun l -> slot + ": " + l) })
            |> List.fold Coverage.merge Coverage.empty
        let tree = build.TreeOverrides |> Map.fold (fun t id stats -> match Map.tryFind id t with Some n -> Map.add id { n with Stats = stats } t | None -> t) tree
        let treeMods, treeCov = Tree.mods db tree build.TreeNodes
        let extraStats, extraUnknown = StatText.parseLines db.Text build.ExtraLines
        let extraMods, _, extraCov = Items.interpret extraStats
        let flaskOf (pick: ParsedItem -> (float * float * float * float) option) =
            build.Items
            |> List.filter (fun (slot, _) -> slot.StartsWith "Flask")
            |> List.tryPick (fun (_, text) ->
                let p = Items.parse db text
                pick p |> Option.map (fun f ->
                    let inc = text.Split('\n') |> Array.tryPick (fun l -> if l.Contains "increased Charges gained" then (match System.Double.TryParse(l.Trim().Split('%').[0]) with | true, v -> Some v | _ -> None) else None)
                    f, inc |> Option.defaultValue 0.0))
        { Mods = itemMods @ treeMods @ extraMods
          MainHand = mainHand
          Weapons = Option.toList mainHand @ Option.toList swapHand
          ItemDefences = parsed |> List.filter (fun (slot, _) -> not (slot.Contains "Swap")) |> List.collect (fun (slot, p) -> p.Defences |> List.map (fun (st, v) -> slot, st, v))
          ManaFlask = flaskOf (fun p -> p.ManaFlask)
          LifeFlask = flaskOf (fun p -> p.LifeFlask)
          Level = build.Level
          ClassName = build.ClassName
          Coverage = Coverage.merge (Coverage.merge itemCov treeCov) { extraCov with UnknownText = extraUnknown |> List.map (fun l -> "quest: " + l) } }

    /// The skill of a socket group: its first enabled active gem with the group's supports.
    let groupSkill (db: GameDb) (sheet: CharacterSheet) (group: PobGroup) =
        let enabled = group.Gems |> List.filter (fun g -> g.Enabled)
        let supports = enabled |> List.filter (fun g -> g.IsSupport) |> List.map (fun g -> g.Name, g.Level)
        enabled
        |> List.filter (fun g -> not g.IsSupport)
        |> List.map (fun active ->
            Gems.skill
                db
                (ModDb.ofList sheet.Mods)
                (Gems.chooseWeapon db active.Name sheet.Weapons)
                { Gem = active.Name
                  Level = active.Level
                  Quality = active.Quality
                  StatSet = active.StatSet
                  Supports = supports })

    /// Add the effects of persistent buff gems (Berserk, Eternal Rage, Charge Regulation...)
    /// to the character. Their gem stats apply to everything the character does.
    let withPersistentBuffs (db: GameDb) (sheet: CharacterSheet) (build: PobBuild) =
        // meta gems are persistent buffs too, but their group's supports belong to the triggered
        // spells; only the meta gem's own "granted" stats reach the character
        let metaGranted =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.choose (fun g -> g.Gems |> List.tryFind (fun gem -> gem.Enabled && not gem.IsSupport && Gems.isMeta db gem.Name))
            |> List.collect (fun gem ->
                Gems.rawStats db (ModDb.ofList sheet.Mods) { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                |> List.filter (fun (id, _) -> Gems.grantsToCharacter db id)
                |> List.collect (fun (id, v) -> match StatMap.meaning id v with Mods ms -> ms | _ -> []))
        let sheet = { sheet with Mods = sheet.Mods @ metaGranted }
        build.Groups
        |> List.filter (fun g -> g.Enabled)
        |> List.choose (fun g ->
            g.Gems
            |> List.tryFind (fun gem -> gem.Enabled && not gem.IsSupport && Gems.isPersistentBuff db gem.Name && not (Gems.isMeta db gem.Name))
            |> Option.map (fun _ -> g))
        // only the buff gem's own stats reach the character: its group's supports modify the
        // buff skill (a herald's explosion), not everything the character does
        |> List.map (fun g -> { g with Gems = g.Gems |> List.filter (fun x -> not x.IsSupport) })
        |> List.collect (groupSkill db sheet)
        |> List.fold
            (fun (sheet: CharacterSheet) r ->
                match r with
                | Ok (skill, cov) ->
                    // Skill-named stats apply to the character only if the game describes them as
                    // granted ("Buff grants 46% increased Fire damage"); otherwise they belong to
                    // the skill itself (a herald's explosion, ...).
                    let characterWide (m: Mod) =
                        not (m.Source.StartsWith "skill-specific:") || Gems.grantsToCharacter db (m.Source.Substring 15)
                    { sheet with
                        Mods = sheet.Mods @ (skill.Mods |> List.filter characterWide |> List.map (Mod.from ("buff:" + skill.Name)))
                        Coverage = Coverage.merge sheet.Coverage { cov with Unmapped = cov.Unmapped |> List.map (fun (s, v) -> skill.Name + ": " + s, v) } }
                | Error _ -> sheet)
            sheet

    /// Charges and rage the character can hold.
    let maxResources (sheet: CharacterSheet) =
        let db = ModDb.ofList sheet.Mods
        let get stat = Mods.sumBase Ctx.empty [ Named stat ] db
        [ "PowerCharge", int (Constants.baseMaxCharges + get "MaxPowerCharge")
          "FrenzyCharge", int (Constants.baseMaxCharges + get "MaxFrenzyCharge")
          "EnduranceCharge", int (Constants.baseMaxCharges + get "MaxEnduranceCharge")
          "Rage", int (Constants.baseMaxRage + get "MaxRage") ]
