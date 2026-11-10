namespace PoEsie

/// What the game itself shows for a weapon (API "properties"), for checking our maths.
type DisplayedWeapon =
    { Damage: Map<DamageType, Range>
      CritChance: float option
      AttacksPerSecond: float option }

/// Import from the official character API (GET /character/poe2/<name>).
module Api =
    /// "[ElementalDamage|Elemental] Damage" -> "Elemental Damage"; "[Attack] Speed" -> "Attack Speed".
    let stripMarkup (s: string) =
        let sb = System.Text.StringBuilder()
        let mutable i = 0
        while i < s.Length do
            if s.[i] = '[' && s.IndexOf(']', i) > i then
                let close = s.IndexOf(']', i)
                let inner = s.Substring(i + 1, close - i - 1)
                let shown = match inner.IndexOf('|') with | -1 -> inner | k -> inner.Substring(k + 1)
                sb.Append(shown) |> ignore
                i <- close + 1
            else
                sb.Append(s.[i]) |> ignore
                i <- i + 1
        sb.ToString()

    /// Text with numbers removed, for matching a host's summed line to its source.
    let private shape (line: string) =
        (stripMarkup line).ToLowerInvariant()
        |> String.filter (fun c -> not (System.Char.IsDigit c || c = '.' || c = '+' || c = '%' || c = '-'))
        |> fun t -> t.Trim()

    /// A socketed item's bonded lines without the "Bonded:" header and host category.
    let private bondedShapes (socketed: Json) =
        Json.list (Json.field "bondedMods" socketed)
        |> List.choose Json.str
        |> List.map stripMarkup
        |> List.filter (fun l -> not (l.EndsWith "Bonded:"))
        |> List.map (fun l -> match l.IndexOf(": ") with | -1 -> l | k -> l.Substring(k + 2))
        |> List.map shape

    /// "Bonded:" lines on the host count only if they come from an idol and the item
    /// says "Idols socketed in this item gain the benefits of their Bonded modifiers".
    /// Those lines lose their prefix; all other Bonded lines keep it (Shaman only).
    let private resolveBonded (item: Json) (lines: string list) =
        let socketed = Json.list (Json.field "socketedItems" item)
        let isIdol (s: Json) = (Json.strField "typeLine" s |> Option.defaultValue "").Contains "Idol"
        // The enabling line is a "Body Armours:" idol line: it needs a body armour, or an
        // item that gains bonuses from socketed items as though it were one.
        let countsAsBodyArmour =
            Json.strField "inventoryId" item = Some "BodyArmour"
            || lines |> List.exists (fun l -> l.Contains "as though it was a Body Armour")
        let enabled =
            countsAsBodyArmour && lines |> List.exists (fun l -> l.Contains "gain the benefits of their Bonded modifiers")
        let idolBonded = socketed |> List.filter isIdol |> List.collect bondedShapes |> Set.ofList
        lines
        |> List.choose (fun l ->
            if l.Contains "gain the benefits of their Bonded modifiers" then None
            elif l.StartsWith "Bonded: " then
                let body = l.Substring 8
                if enabled && idolBonded.Contains(shape body) then Some body else Some l
            else Some l)

    let private modLines (item: Json) =
        [ "enchantMods"; "implicitMods"; "runeMods"; "explicitMods"; "craftedMods"; "fracturedMods"; "desecratedMods" ]
        |> List.collect (fun key ->
            Json.list (Json.field key item)
            |> List.choose (fun m ->
                match m with
                | JStr s -> Some s
                | JObj _ -> Json.strField "description" m
                | _ -> None))
        |> List.collect (fun l -> (stripMarkup l).Split('\n') |> List.ofArray)
        |> resolveBonded item

    let private property name (item: Json) =
        Json.list (Json.field "properties" item)
        |> List.tryFind (fun p -> Json.strField "name" p |> Option.map stripMarkup = Some name)
        |> Option.bind (fun p -> Json.list (Json.field "values" p) |> List.tryHead)
        |> Option.bind (fun v -> Json.list (Some v) |> List.tryHead |> Option.bind Json.str)

    let private parseRange (s: string) =
        match s.Split('-') with
        | [| lo; hi |] -> Some { Min = float lo; Max = float hi }
        | _ -> None

    /// The weapon numbers the game displays.
    let displayedWeapon (item: Json) : DisplayedWeapon =
        let damage =
            DamageType.all
            |> List.choose (fun t -> property (DamageType.name t + " Damage") item |> Option.bind parseRange |> Option.map (fun r -> t, r))
            |> Map.ofList
        { Damage = damage
          CritChance = property "Critical Hit Chance" item |> Option.map (fun s -> float (s.TrimEnd('%')))
          AttacksPerSecond = property "Attacks per Second" item |> Option.map float }

    let private slotName (inventoryId: string) =
        match inventoryId with
        | "Weapon" -> "Weapon 1"
        | "Offhand" -> "Weapon 2"
        | "Weapon2" -> "Weapon 1 Swap"
        | "Offhand2" -> "Weapon 2 Swap"
        | "Ring" -> "Ring 1"
        | "Ring2" -> "Ring 2"
        | "BodyArmour" -> "Body Armour"
        | "Helm" -> "Helmet"
        | other -> other

    /// The item in the game's text layout, so Items.parse handles it.
    let itemText (item: Json) =
        let get k = Json.strField k item |> Option.defaultValue ""
        let quality = property "Quality" item |> Option.map (fun q -> "Quality: " + q) |> Option.toList
        let defences =
            [ "Armour", "Armour"; "Evasion Rating", "Evasion Rating"; "Energy Shield", "Energy Shield"; "Block chance", "Block chance" ]
            |> List.choose (fun (prop, label) -> property prop item |> Option.map (fun v -> label + ": " + v))
        // flask properties with placeholders: "Recovers {0} Mana over {1} Seconds" [465; 3.50]
        let templated =
            Json.list (Json.field "properties" item)
            |> List.choose (fun p ->
                match Json.strField "name" p with
                | Some name when name.Contains "{0}" ->
                    let values = Json.list (Json.field "values" p) |> List.choose (fun v -> Json.list (Some v) |> List.tryHead |> Option.bind Json.str)
                    let filled = values |> List.indexed |> List.fold (fun (t: string) (i, v) -> t.Replace(sprintf "{%d}" i, v)) (stripMarkup name)
                    if filled.StartsWith "Recovers" || filled.StartsWith "Consumes" then Some("Flask: " + filled) else None
                | _ -> None)
        [ "Rarity: " + get "rarity"; get "name"; get "typeLine"; "--------" ] @ quality @ defences @ templated @ [ "--------" ] @ modLines item
        |> String.concat "\n"

    let private gemLevel (gem: Json) =
        property "Level" gem
        |> Option.map (fun s -> s.Split(' ').[0])
        |> Option.bind (fun s -> match System.Int32.TryParse s with | true, v -> Some v | _ -> None)
        |> Option.defaultValue 1

    /// Skill gems are taken to have 20% quality unless the API says otherwise.
    let private gemQuality (gem: Json) =
        property "Quality" gem
        |> Option.map (fun s -> s.TrimStart('+').TrimEnd('%'))
        |> Option.bind (fun s -> match System.Int32.TryParse s with | true, v -> Some v | _ -> None)
        |> Option.defaultValue 20

    /// Convert an API character into the same shape a PoB build is read into.
    let read (json: string) : PobBuild * (string * DisplayedWeapon) list =
        let root = Json.parse json
        let c = Json.field "character" root |> Option.defaultValue root
        let equipment = Json.list (Json.field "equipment" c)
        let items =
            equipment
            |> List.map (fun it -> slotName (Json.strField "inventoryId" it |> Option.defaultValue ""), itemText it)
        let jewels =
            Json.list (Json.field "jewels" c) |> List.mapi (fun i j -> sprintf "Jewel %d" (i + 1), itemText j)
        let displayed =
            equipment
            |> List.filter (fun it -> Json.field "properties" it |> Option.isSome && (property "Attacks per Second" it).IsSome)
            |> List.map (fun it -> slotName (Json.strField "inventoryId" it |> Option.defaultValue ""), displayedWeapon it)
        let groups =
            Json.list (Json.field "skills" c)
            |> List.map (fun g ->
                let active =
                    { Name = Json.strField "typeLine" g |> Option.defaultValue ""
                      Level = gemLevel g
                      Quality = gemQuality g
                      Enabled = true
                      IsSupport = false
                      StatSet = 1 }
                let supports =
                    // socketed gems: supports, and active spells socketed into meta gems
                    Json.list (Json.field "socketedItems" g)
                    |> List.map (fun s ->
                        let support = Json.field "support" s = Some(JBool true)
                        { Name = Json.strField "typeLine" s |> Option.defaultValue ""
                          Level = gemLevel s
                          Quality = if support then 0 else gemQuality s
                          Enabled = true
                          IsSupport = support
                          StatSet = 1 })
                { Label = active.Name
                  Slot = ""
                  Enabled = true
                  Gems = active :: supports })
        let hashes =
            Json.list (Json.field "passives" c |> Option.bind (Json.field "hashes"))
            |> List.choose Json.num
            |> List.map int
        let passives = Json.field "passives" c
        // chosen attributes etc.: node id -> the stats it really grants
        let overrides =
            match passives |> Option.bind (Json.field "skill_overrides") with
            | Some (JObj m) ->
                m |> Map.toList |> List.choose (fun (k, v) ->
                    match System.Int32.TryParse k with
                    | true, id -> Some(id, Json.list (Json.field "stats" v) |> List.choose Json.str |> List.map stripMarkup)
                    | _ -> None)
            | _ -> []
        let quests = Json.list (passives |> Option.bind (Json.field "quest_stats")) |> List.choose Json.str |> List.map stripMarkup
        { Level = Json.numField "level" c |> Option.map int |> Option.defaultValue 1
          ClassName = Json.strField "class" c |> Option.defaultValue ""
          Ascendancy = ""
          Items = items @ jewels
          TreeNodes = hashes
          Groups = groups
          MainGroup = 1
          Config = Map.empty
          TreeOverrides = Map.ofList overrides
          ExtraLines = quests },
        displayed
