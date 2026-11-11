namespace PoEtry

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

/// The parts of a PoB build PoEtry uses. Only PoB's *inputs* are read (items, tree,
/// gems, level); none of PoB's computed numbers.
type PobBuild =
    { Level: int
      ClassName: string
      Ascendancy: string
      /// Slot name -> raw item text.
      Items: (string * string) list
      TreeNodes: int list
      /// Passives allocated with weapon set points: they only count while that set is in use.
      TreeNodesSet1: int list
      TreeNodesSet2: int list
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
                  // PoB marks a group an item grants with source="Item:..."
                  Slot = (match Xml.attr "source" g with | Some src when src.StartsWith "Item" -> "granted" | _ -> Xml.attr "slot" g |> Option.defaultValue "")
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
          TreeNodesSet1 = []
          TreeNodesSet2 = []
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

    /// Mage's Legacies (Mageblood): "Legacy of X" lines. Each kind applies once; every
    /// duplicate raises the effect of all of them by the belt's "increased effect per
    /// duplicate". Values: PoB CalcPerform `legacies` (Topaz taken like Ruby / Sapphire).
    let private legacyMods (lines: string list) : Mod list * string list =
        let prefix = "Legacy of "
        let names = lines |> List.map (fun l -> l.Trim()) |> List.filter (fun l -> l.StartsWith prefix) |> List.map (fun l -> l.Substring prefix.Length)
        if names.IsEmpty then [], lines
        else
            let perDuplicate =
                lines |> List.tryPick (fun l ->
                    if l.Contains "increased effect per duplicate Mage's Legacy" then
                        (match System.Double.TryParse(l.Trim().Split('%').[0].Replace("All Mage's Legacies have ", "")) with | true, v -> Some v | _ -> None)
                    else None)
                |> Option.defaultValue 0.0
            let duplicates = names |> List.countBy id |> List.sumBy (fun (_, n) -> n - 1)
            let effect = 1.0 + float duplicates * perDuplicate / 100.0
            let value (v: float) = floor (effect * v)
            let resist t = [ Mod.base' (Resist t) (value 60.0); Mod.base' (MaxResist t) (value 5.0) ]
            let mods =
                names |> List.distinct |> List.collect (fun n ->
                    match n with
                    | "Amethyst" -> [ Mod.base' (Resist Chaos) (value 45.0) ]
                    | "Basalt" -> [ Mod.inc Armour (value 150.0) ]
                    | "Bismuth" -> DamageType.elemental |> List.map (fun t -> Mod.base' (Resist t) (value 45.0))
                    | "Diamond" -> [ Mod.inc CritChance (value 75.0) ]
                    | "Granite" -> [ Mod.base' Armour (value 2000.0) ]
                    | "Jade" -> [ Mod.base' Evasion (value 2000.0) ]
                    | "Ruby" -> resist Fire
                    | "Sapphire" -> resist Cold
                    | "Topaz" -> resist Lightning
                    | "Silver" -> [ Mod.inc Speed (value 30.0) ]
                    | "Stibnite" -> [ Mod.inc Evasion (value 150.0) ]
                    | "Sulphur" -> [ Mod.inc (Damage AllDamage) (value 60.0) ]
                    // Gold (rarity), Quicksilver (movement speed): nothing in a fight
                    | _ -> [])
                |> List.map (Mod.from "Mage's Legacy")
            mods, lines |> List.filter (fun l -> not (l.Trim().StartsWith prefix) && not (l.Contains "increased effect per duplicate Mage's Legacy"))

    /// "Allocates <notable>" on an item (anoints, jewels) allocates that passive.
    let private allocatedByItems (tree: Map<int, TreeNode>) (texts: string list) =
        texts
        |> List.collect (fun t -> t.Split('\n') |> List.ofArray)
        |> List.choose (fun l ->
            let l = l.Trim()
            if l.StartsWith "Allocates " then
                let name = l.Substring 10
                tree |> Map.tryPick (fun id n -> if n.Name = name && (n.IsNotable || n.IsKeystone) then Some id else None)
            // a line that is just a keystone's name grants that keystone ("Eldritch Battery")
            else tree |> Map.tryPick (fun id n -> if n.IsKeystone && n.Name = l then Some id else None))

    /// Character mods from equipped items and allocated passives.
    let character (db: GameDb) (tree: Map<int, TreeNode>) (jewelSlots: int list) (build: PobBuild) : CharacterSheet =
        let items = build.Items |> List.filter (fst >> alwaysOn) |> List.map (fun (slot, text) -> slot, resolveBonded build slot text)
        // Kalandra's Touch: "Reflects opposite Ring" - the ring is a copy of the other one
        let items =
            let ring n = items |> List.tryFind (fun (slot, _) -> slot = n) |> Option.map snd
            let reflects (t: string) = t.Contains "Reflects opposite Ring" || t.Contains "Reflects your other Ring"
            items |> List.map (fun (slot, text) ->
                let other = if slot = "Ring 1" then ring "Ring 2" elif slot = "Ring 2" then ring "Ring 1" else None
                match other with
                | Some o when reflects text && not (reflects o) -> slot, o
                | _ -> slot, text)
        let extraNodes = allocatedByItems tree (List.map snd items)
        // those keystone lines are now passives, not item lines
        let items =
            let keystones = tree |> Map.toList |> List.choose (fun (_, n) -> if n.IsKeystone then Some n.Name else None) |> Set.ofList
            items |> List.map (fun (slot, text) -> slot, text.Split('\n') |> Array.filter (fun l -> not (keystones.Contains(l.Trim()))) |> String.concat "\n")
        // a passive allocated on the tree and by an item still counts once
        let build = { build with TreeNodes = List.distinct (build.TreeNodes @ extraNodes) }
        // Radius jewels: "Notable / Small Passive Skills in Radius also grant X" gives X once per
        // allocated passive of that kind within the jewel's radius of its socket.
        let radiusMods, items =
            let allocated = build.TreeNodes |> List.distinct |> List.choose (fun id -> Map.tryFind id tree)
            items
            |> List.map (fun (slot, text) ->
                let lines = text.Split('\n') |> List.ofArray
                let socket =
                    if slot.StartsWith "Jewel " then
                        match System.Int32.TryParse(slot.Substring 6) with
                        | true, n -> (if n < 1000 then List.tryItem n jewelSlots else Some n) |> Option.bind (fun id -> Map.tryFind id tree)
                        | _ -> None
                    else None
                let radius = lines |> List.tryPick (fun l -> if l.StartsWith "Radius:" then Tree.jewelRadius (l.Substring 7) else None)
                let grant (l: string) =
                    let l = l.Trim()
                    [ "Notable Passive Skills in Radius also grant ", true; "Small Passive Skills in Radius also grant ", false ]
                    |> List.tryPick (fun (prefix, notable) -> if l.StartsWith prefix then Some(notable, l.Substring prefix.Length) else None)
                // lines about the jewel itself, with no effect of their own
                let inert (l: string) =
                    l.Contains "Passives in Radius of" || l.Trim() = "without being connected to your tree"
                    // The Adorned: read where the jewels' modifiers are scaled
                    || l.Contains "increased Effect of Jewel Socket Passive Skills" || l.Trim() = "containing Corrupted Magic Jewels"
                let grants = lines |> List.choose grant
                let rest = lines |> List.filter (fun l -> (grant l).IsNone && not (inert l))
                let mods =
                    match socket, radius with
                    | Some s, Some r ->
                        grants
                        |> List.collect (fun (notable, line) ->
                            let count =
                                allocated
                                |> List.filter (fun n ->
                                    n.Id <> s.Id && not n.IsKeystone && not n.IsJewelSocket && n.IsNotable = notable
                                    && (n.X <> 0.0 || n.Y <> 0.0)
                                    && sqrt ((n.X - s.X) ** 2.0 + (n.Y - s.Y) ** 2.0) <= r)
                                |> List.length
                            if count = 0 then []
                            else
                                let stats, _ = StatText.parseLines db.Text [ line ]
                                let ms, _, _ = Items.interpret stats
                                // once per passive (so "more" mods multiply)
                                List.replicate count (ms |> List.map (Mod.from (slot + " radius"))) |> List.concat)
                    | _ -> []
                mods, (slot, String.concat "\n" rest))
            |> List.unzip
            |> fun (ms, its) -> List.concat ms, its
        // Rune / idol / soul core lines that depend on where they sit ("Boots: 5% increased
        // Movement Speed"): keep the ones for this item's kind, without the prefix.
        let items =
            items
            |> List.map (fun (slot, text) ->
                let lines = text.Split('\n') |> List.ofArray
                let isWeaponSlot = slot.StartsWith "Weapon 1"
                let martial = isWeaponSlot && lines |> List.exists (fun l -> Map.containsKey (l.Trim()) db.Weapons)
                let fits (kind: string) =
                    match kind with
                    | "All Equipment" -> Some true
                    | "Body Armours" -> Some(slot = "Body Armour")
                    | "Helmets" -> Some(slot = "Helmet")
                    | "Gloves" -> Some(slot = "Gloves")
                    | "Boots" -> Some(slot = "Boots")
                    | "Shields" | "Shield" -> Some(slot.StartsWith "Weapon 2" && not martial)
                    | "Martial Weapon" | "Martial Weapons" -> Some martial
                    | "Wand or Staff" | "Caster Weapon" | "Caster Weapons" -> Some(isWeaponSlot && not martial)
                    | "Crossbow, Bow or Spear" -> Some martial
                    | "Armour" -> Some(List.contains slot [ "Body Armour"; "Helmet"; "Gloves"; "Boots" ] || slot.StartsWith "Weapon 2")
                    | _ -> None
                let lines =
                    lines |> List.choose (fun l ->
                        let t = l.Trim()
                        match t.IndexOf ": " with
                        // "Armour: 1842" is the item's own armour, not a rune line for armour pieces
                        | k when k > 0 && System.Char.IsDigit t.[k + 2] && t.Substring(k + 2) |> Seq.forall (fun ch -> System.Char.IsDigit ch || ch = '.') -> Some l
                        | k when k > 0 ->
                            match fits (t.Substring(0, k)) with
                            | Some true -> Some(t.Substring(k + 2))
                            | Some false -> None
                            | None -> Some l
                        | _ ->
                            // a bare category header ("Body Armours:")
                            if t.EndsWith ":" && (fits (t.TrimEnd(':'))).IsSome then None else Some l)
                slot, String.concat "\n" lines)
        let legacies, items =
            items
            |> List.map (fun (slot, text) ->
                let mods, rest = legacyMods (text.Split('\n') |> List.ofArray)
                mods, (slot, String.concat "\n" rest))
            |> List.unzip
            |> fun (ms, its) -> List.concat ms, its
        let parsed = items |> List.map (fun (slot, text) -> slot, Items.parse db text)
        let mainHand = parsed |> List.tryFind (fun (slot, _) -> slot = "Weapon 1") |> Option.bind (fun (_, p) -> p.Weapon)
        let swapHand =
            parsed |> List.tryFind (fun (slot, _) -> slot = "Weapon 1 Swap") |> Option.bind (fun (_, p) -> p.Weapon)
            |> Option.map (fun w -> { w with Tags = Tag "WeaponSet2" :: w.Tags })
        // A weapon's local mods are already in its Weapon; only its global mods go to the sheet.
        let sourced (source: string) (m: Mod) = if m.Source = "" then Mod.from source m else m
        let itemMods = parsed |> List.map (fun (slot, p) -> slot, p.Mods |> List.map (slotCondition slot >> sourced (slot + ": " + p.Name)))
        let itemCov =
            parsed
            |> List.map (fun (slot, p) -> { p.Coverage with UnknownText = p.Coverage.UnknownText |> List.map (fun l -> slot + ": " + l) })
            |> List.fold Coverage.merge Coverage.empty
        let tree = build.TreeOverrides |> Map.fold (fun t id stats -> match Map.tryFind id t with Some n -> Map.add id { n with Stats = stats } t | None -> t) tree
        let extraStats, extraUnknown = StatText.parseLines db.Text build.ExtraLines
        let extraMods, _, extraCov = Items.interpret extraStats
        let extraRescued, extraUnknown = Items.rescue db extraUnknown
        let extraMods = extraMods @ extraRescued
        // small passives apart: "increased effect of Small Passive Skills" scales only them
        let isSmall id =
            match Map.tryFind id tree with
            // not the "+5 to any Attribute" nodes: they are not Small Passive Skills in this sense
            | Some n -> not n.IsNotable && not n.IsKeystone && not n.IsJewelSocket && n.Ascendancy = "" && n.Name <> "Attribute"
            | None -> false
        let smallMods, treeMods, treeCov =
            let split ids cond =
                let small, cs = Tree.mods db tree (ids |> List.filter isSmall)
                let rest, cr = Tree.mods db tree (ids |> List.filter (isSmall >> not))
                small |> List.map cond, rest |> List.map cond, Coverage.merge cs cr
            let s0, r0, c0 = split build.TreeNodes id
            let s1, r1, c1 = split build.TreeNodesSet1 (Mod.whenever (Not setTwo))
            let s2, r2, c2 = split build.TreeNodesSet2 (Mod.whenever setTwo)
            s0 @ s1 @ s2, r0 @ r1 @ r2, Coverage.merge c0 (Coverage.merge c1 c2)
        // "increased effect of Small Passive Skills", "increased bonuses gained from Equipped
        // Quiver / Rings and Amulets / left Ring", "increased Effect of bonuses gained from
        // Socketed Jewel" (an ascendancy's jewel socket): the mods of that source count for more.
        // The game's stats are whole numbers, so a scaled value is cut off (+5 at 50% is +7).
        let itemMods, smallMods =
            let everything = (itemMods |> List.collect snd) @ smallMods @ treeMods @ extraMods
            let effect (what: string) =
                everything |> List.sumBy (fun m -> if m.Stat = Named("EffectOf:" + what) && m.Kind = Inc && m.Conds.IsEmpty then m.Value else 0.0)
            let scale (pct: float) (m: Mod) =
                let isEffect = match m.Stat with Named n -> n.StartsWith "EffectOf:" | _ -> false
                if pct = 0.0 || isEffect || not (m.Kind = Base || m.Kind = Inc || m.Kind = More) then m
                else
                    let v = m.Value * (1.0 + pct / 100.0)
                    { m with Value = if m.Value = System.Math.Truncate m.Value then System.Math.Truncate v else v }
            // The Adorned: "N% increased Effect of Jewel Socket Passive Skills containing Corrupted Magic Jewels"
            let adorned =
                build.Items |> List.sumBy (fun (slot, text) ->
                    if not (slot.StartsWith "Jewel ") then 0.0
                    else
                        text.Split('\n') |> Array.sumBy (fun l ->
                            let l = l.Trim()
                            let k = l.IndexOf "% increased Effect of Jewel Socket Passive Skills"
                            if k > 0 then (match System.Double.TryParse(l.Substring(0, k)) with | true, v -> v | _ -> 0.0) else 0.0))
            let slotEffect (slot: string) (text: string) =
                if slot = "Ring 1" || slot = "Ring 2" || slot = "Amulet" then effect slot
                elif slot = "Ring3" || slot = "Ring 3" then effect "Ring 3"
                elif slot = "Weapon 2" && text.Contains "Quiver" then effect "Quiver"
                elif slot.StartsWith "Jewel " then
                    let socket =
                        match System.Int32.TryParse(slot.Substring 6) with
                        | true, n -> (if n < 1000 then List.tryItem n jewelSlots else Some n) |> Option.bind (fun id -> Map.tryFind id tree)
                        | _ -> None
                    let lines = text.Split('\n') |> Array.map (fun l -> l.Trim())
                    let corruptedMagic = adorned > 0.0 && Array.contains "Rarity: Magic" lines && Array.contains "Corrupted" lines
                    (match socket with
                     | Some n when n.Ascendancy <> "" -> effect "AscendancyJewel"
                     | _ -> 0.0)
                    + (if corruptedMagic then adorned else 0.0)
                else 0.0
            let texts = Map.ofList build.Items
            itemMods |> List.collect (fun (slot, ms) -> ms |> List.map (scale (slotEffect slot (Map.tryFind slot texts |> Option.defaultValue "")))),
            smallMods |> List.map (scale (effect "SmallPassives"))
        let treeMods = smallMods @ treeMods
        // "+1 to maximum Life per 8 Item Armour on Equipped Helmet": a flat amount from what
        // that item shows (StatMap names them PerItemDefence|target|slot|defence|per)
        let perItemDefence (ms: Mod list) =
            ms |> List.map (fun m ->
                match m.Stat with
                | Named n when n.StartsWith "PerItemDefence|" ->
                    let p = n.Split '|'
                    let stat (s: string) = match s with | "MaxLife" -> MaxLife | "MaxEnergyShield" -> MaxEnergyShield | "Evasion" -> Evasion | _ -> Armour
                    let shown = parsed |> List.sumBy (fun (slot, it) -> if slot = p.[2] then it.Defences |> List.sumBy (fun (st, v) -> if st = stat p.[3] then v else 0.0) else 0.0)
                    { m with Stat = stat p.[1]; Value = m.Value * floor (shown / float p.[4]) }
                | _ -> m)
        let itemMods, treeMods = perItemDefence itemMods, perItemDefence treeMods
        // "+200 to Armour for each Connected Notable Passive Skill Allocated" (Smith's Masterwork):
        // the allocated notables joined to that passive
        let treeMods =
            let allocated = build.TreeNodes |> List.distinct |> List.choose (fun id -> Map.tryFind id tree)
            let connected =
                allocated
                |> List.filter (fun n -> n.Stats |> List.exists (fun l -> l.Contains "for each Connected Notable Passive Skill Allocated"))
                |> List.sumBy (fun n ->
                    allocated |> List.filter (fun m -> m.IsNotable && m.Id <> n.Id && (List.contains m.Id n.Connections || List.contains n.Id m.Connections)) |> List.length)
            treeMods |> List.map (fun m -> if m.Stat = Named "ArmourPerConnectedNotable" then { m with Stat = Armour; Value = m.Value * float connected } else m)
        // "This Flask cannot be Used but applies its Effect constantly" (Lavianga's Spirits): not
        // a flask to drink but so much life or mana per second, and its effect is always on
        let constant (text: string) = text.Contains "cannot be Used but applies its Effect constantly"
        let constantFlaskMods =
            build.Items
            |> List.filter (fun (slot, text) -> slot.StartsWith "Flask" && constant text)
            |> List.collect (fun (slot, text) ->
                let p = Items.parse db text
                let per (f: (float * float * float * float) option) = match f with Some (amount, every, _, _) when every > 0.0 -> amount / every | _ -> 0.0
                [ if per p.ManaFlask > 0.0 then
                      yield Mod.base' (Named "ConstantManaRecovery") (per p.ManaFlask)
                      yield Mod.flag (Named "ConstantFlask:Mana")
                  if per p.LifeFlask > 0.0 then
                      yield Mod.base' (Named "ConstantLifeRecovery") (per p.LifeFlask)
                      yield Mod.flag (Named "ConstantFlask:Life") ]
                |> List.map (Mod.from (slot + ": " + p.Name)))
        let flaskOf (pick: ParsedItem -> (float * float * float * float) option) =
            build.Items
            |> List.filter (fun (slot, text) -> slot.StartsWith "Flask" && not (constant text))
            |> List.tryPick (fun (_, text) ->
                let p = Items.parse db text
                pick p |> Option.map (fun f ->
                    let inc = text.Split('\n') |> Array.tryPick (fun l -> if l.Contains "increased Charges gained" then (match System.Double.TryParse(l.Trim().Split('%').[0]) with | true, v -> Some v | _ -> None) else None)
                    f, inc |> Option.defaultValue 0.0))
        // the shield's own armour and evasion, for skills that hit with it
        let shieldMods =
            parsed |> List.collect (fun (slot, p) ->
                if slot = "Weapon 2" then
                    p.Defences |> List.choose (fun (st, v) ->
                        match st with
                        | Armour -> Some(Mod.base' (Named "ShieldArmour") v |> Mod.from slot)
                        | Evasion -> Some(Mod.base' (Named "ShieldEvasion") v |> Mod.from slot)
                        | _ -> None)
                else [])
        // Spirit on the second weapon set's items (a sceptre kept there)
        let swapSpirit =
            parsed |> List.sumBy (fun (slot, p) -> if slot.Contains "Swap" then p.Defences |> List.sumBy (fun (st, v) -> if st = Named "Spirit" then v else 0.0) else 0.0)
        let shieldMods = if swapSpirit > 0.0 then (Mod.base' (Named "SpiritOnSetTwo") swapSpirit |> Mod.from "weapon set two") :: shieldMods else shieldMods
        { Mods = itemMods @ constantFlaskMods @ shieldMods @ legacies @ radiusMods @ treeMods @ (extraMods |> List.map (sourced "quest"))
          MainHand = mainHand
          Weapons = Option.toList mainHand @ Option.toList swapHand
          // the second weapon set's items keep their "... Swap" slot: Defence picks the set
          ItemDefences = parsed |> List.collect (fun (slot, p) -> p.Defences |> List.map (fun (st, v) -> slot, st, v))
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
                    // auras (Purity of Fire, Discipline) scale with "increased effect of Auras"
                    let auraEffect =
                        if skill.Tags.Contains(Tag "Type:Aura") then Mods.inc Ctx.empty [ Named "AuraEffect" ] (ModDb.ofList sheet.Mods) else 1.0
                    { sheet with
                        Mods = sheet.Mods @ (skill.Mods |> List.filter characterWide |> List.map (Mod.scaled auraEffect >> Mod.from ("buff:" + skill.Name)))
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
