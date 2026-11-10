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
      Config: Map<string, string> }

/// A character ready to simulate: global mods, main-hand weapon, how well we understood it.
type CharacterSheet =
    { Mods: Mod list
      MainHand: Weapon option
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
                          Quality = int' (Xml.attr "quality" gem) 0
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
          Config = config }

    /// Slots whose items always apply. Flasks and charms only apply while active.
    let private alwaysOn (slot: string) =
        not (slot.StartsWith "Flask" || slot.StartsWith "Charm" || slot.Contains "Swap")

    /// Character mods from equipped items and allocated passives.
    let character (db: GameDb) (tree: Map<int, TreeNode>) (build: PobBuild) : CharacterSheet =
        let parsed = build.Items |> List.filter (fst >> alwaysOn) |> List.map (fun (slot, text) -> slot, Items.parse db text)
        let mainHand = parsed |> List.tryFind (fun (slot, _) -> slot = "Weapon 1") |> Option.bind (fun (_, p) -> p.Weapon)
        // A weapon's local mods are already in its Weapon; only its global mods go to the sheet.
        let itemMods = parsed |> List.collect (fun (_, p) -> p.Mods)
        let itemCov =
            parsed
            |> List.map (fun (slot, p) -> { p.Coverage with UnknownText = p.Coverage.UnknownText |> List.map (fun l -> slot + ": " + l) })
            |> List.fold Coverage.merge Coverage.empty
        let treeMods, treeCov = Tree.mods db tree build.TreeNodes
        { Mods = itemMods @ treeMods
          MainHand = mainHand
          Coverage = Coverage.merge itemCov treeCov }

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
                sheet.MainHand
                { Gem = active.Name
                  Level = active.Level
                  Quality = active.Quality
                  StatSet = active.StatSet
                  Supports = supports })
