namespace PoEsie

type ParsedItem =
    { Name: string
      BaseName: string
      /// Present if the item is a weapon.
      Weapon: Weapon option
      /// Global modifiers the item gives the character.
      Mods: Mod list
      /// Armour / evasion / energy shield as the item shows them (local mods and quality included).
      Defences: (Stat * float) list
      /// Mana flask: (amount, duration, charges per use, max charges).
      ManaFlask: (float * float * float * float) option
      LifeFlask: (float * float * float * float) option
      Coverage: Coverage }

type TreeNode =
    { Id: int
      Name: string
      Stats: string list
      IsKeystone: bool
      IsNotable: bool }

module Items =
    let private suffixes = [ " (implicit)"; " (rune)"; " (enchant)"; " (crafted)"; " (fractured)"; " (desecrated)"; " (augmented)" ]

    let private clean (line: string) =
        suffixes |> List.fold (fun (l: string) s -> if l.EndsWith s then l.Substring(0, l.Length - s.Length) else l) (line.Trim())

    /// Header lines the game prints that are not modifiers.
    let private isHeader (line: string) =
        let prefixes =
            [ "Item Class:"; "Rarity:"; "Quality:"; "Physical Damage:"; "Elemental Damage:"; "Fire Damage:"; "Cold Damage:"
              "Lightning Damage:"; "Chaos Damage:"; "Critical Hit Chance:"; "Attacks per Second:"; "Weapon Range:"
              "Reload Time:"; "Requirements:"; "Requires:"; "Level:"; "Str:"; "Dex:"; "Int:"; "Sockets:"; "Item Level:"
              "Armour:"; "Evasion Rating:"; "Energy Shield:"; "Block chance:"; "Spirit:"; "Charm Slots:"; "Corrupted"
              "Unidentified"; "Note:"; "Stack Size:"; "Limited to:"; "Radius:"; "--------"
              // PoB's raw item format
              "Unique ID:"; "League:"; "Rune:"; "LevelReq:"; "Implicits:"; "Prefix:"; "Suffix:"; "Variant:"
              "Selected Variant:"; "Has Alt Variant"; "Selected Alt Variant"; "Allow Duplicate Variants:"; "Crafted:"
              "Unreleased:"; "Catalyst:"; "CatalystQuality:"; "Talisman Tier:"; "Evasion:"; "Ward:"; "ArmourBasePercentile:"
              "EvasionBasePercentile:"; "EnergyShieldBasePercentile:"; "Cluster Jewel"; "Flask: "; "Runic Ward:" ]
        prefixes |> List.exists (fun p -> line.StartsWith p)

    /// Weapon kind from the base: its type, refined by base tags (quarterstaves are type
    /// "Staff" with the tag "warstaff").
    let private weaponTags (b: WeaponBase) =
        let t = b.Type.ToLowerInvariant()
        let kind =
            if b.BaseTags.Contains "warstaff" then [ Quarterstaff ]
            else
                [ "mace", Mace; "sword", Sword; "axe", Axe; "dagger", Dagger; "claw", Claw; "spear", Spear
                  "crossbow", Crossbow; "bow", Bow; "flail", Flail; "wand", Wand; "staff", Staff ]
                |> List.tryFind (fun (k, _) -> t.Contains k && not (k = "bow" && t.Contains "crossbow"))
                |> Option.map snd
                |> Option.toList
        let hands = if b.BaseTags.Contains "twohand" || t.StartsWith "two hand" || t = "bow" || t = "crossbow" then Tag "TwoHanded" else Tag "OneHanded"
        hands :: kind

    let private quality (lines: string list) =
        lines
        |> List.tryPick (fun l ->
            if l.StartsWith "Quality:" then
                let digits = l.Substring(8).Trim().TrimStart('+').Split('%').[0]
                match System.Double.TryParse digits with
                | true, v -> Some v
                | _ -> None
            else
                None)
        |> Option.defaultValue 0.0

    /// Apply local stats to a weapon base. Quality counts as increased physical damage.
    let weapon (b: WeaponBase) (qualityPct: float) (locals: (LocalStat * float) list) =
        let sum pred = locals |> List.sumBy (fun (s, v) -> if pred s then v else 0.0)
        let incPhys = sum ((=) LocalDamageInc) + qualityPct
        let added t =
            { Min = sum ((=) (LocalAddedMin t))
              Max = sum ((=) (LocalAddedMax t)) }
        let damage =
            DamageType.all
            |> List.choose (fun t ->
                let b = Map.tryFind t b.Damage |> Option.defaultValue Range.zero
                let r = Range.add b (added t)
                let r = if t = Physical then Range.scale (1.0 + incPhys / 100.0) r else r
                if r.Max > 0.0 then Some(t, r) else None)
            |> Map.ofList
        { Damage = damage
          // local crit is stored in hundredths of a percent
          CritChance = (b.CritChance + sum ((=) LocalCritChance) / 100.0) * (1.0 + sum ((=) LocalCritChanceInc) / 100.0)
          AttacksPerSecond = b.AttacksPerSecond * (1.0 + sum ((=) LocalAttackSpeedInc) / 100.0)
          Tags = weaponTags b }

    /// Turn parsed stats into mods + local stats + coverage.
    let interpret (stats: (string * float) list) =
        stats
        |> List.fold
            (fun (mods, locals, cov: Coverage) (id, v) ->
                match StatMap.meaning id v with
                | Mods ms -> mods @ ms, locals, cov
                | Local (l, v) -> mods, locals @ [ l, v ], cov
                | NotOffensive
                | Structural -> mods, locals, cov
                | Unsimulated m -> mods, locals, { cov with Unsimulated = cov.Unsimulated @ [ id, m ] }
                | Unmapped -> mods, locals, { cov with Unmapped = cov.Unmapped @ [ id, v ] })
            ([], [], Coverage.empty)

    /// Replace "(lo-hi)" ranges by the value at position r (0..1), as PoB's {range:r} means.
    let private applyRange (r: float) (line: string) =
        let sb = System.Text.StringBuilder()
        let mutable i = 0
        let isNum c = (c >= '0' && c <= '9') || c = '.'
        while i < line.Length do
            let c = line.[i]
            let close = if c = '(' then line.IndexOf(')', i) else -1
            let inner = if close > i then line.Substring(i + 1, close - i - 1) else ""
            let dash = if inner.Length > 1 then inner.IndexOf('-', 1) else -1
            if close > i && dash > 0 && inner |> Seq.forall (fun ch -> isNum ch || ch = '-') then
                let lo = float (inner.Substring(0, dash))
                let hi = float (inner.Substring(dash + 1))
                let v = lo + (hi - lo) * r
                let isInt (x: float) = x = System.Math.Round x
                let v = if isInt lo && isInt hi then System.Math.Round v else System.Math.Round(v, 2)
                sb.Append(string v) |> ignore
                i <- close + 1
            else
                sb.Append(c) |> ignore
                i <- i + 1
        sb.ToString()

    /// PoB prefixes mod lines with {range:x}, {variant:1,2}, {tags:...}, {crafted}...
    /// Resolve them; drop lines that belong to a variant that isn't selected.
    let private pobLine (selectedVariant: string option) (line: string) =
        let rec strip (line: string) range keep =
            if line.StartsWith "{" && line.IndexOf('}') > 0 then
                let close = line.IndexOf('}')
                let tag = line.Substring(1, close - 1)
                let rest = line.Substring(close + 1)
                let key, value =
                    match tag.IndexOf(':') with
                    | -1 -> tag, ""
                    | k -> tag.Substring(0, k), tag.Substring(k + 1)
                match key with
                | "range" -> strip rest (Some(float value)) keep
                | "variant" ->
                    let ok =
                        match selectedVariant with
                        | Some sel -> value.Split(',') |> Array.contains sel
                        | None -> true
                    strip rest range (keep && ok)
                | _ -> strip rest range keep
            else
                line, range, keep
        let line, range, keep = strip line None true
        if not keep then None
        else Some(match range with Some r -> applyRange r line | None -> line)

    /// Parse an item in the game's Ctrl+C text format, or PoB's raw item format.
    let parse (db: GameDb) (text: string) : ParsedItem =
        let raw = text.Replace("\r", "").Split('\n') |> List.ofArray |> List.map (fun l -> l.Trim())
        let selectedVariant =
            raw |> List.tryPick (fun l -> if l.StartsWith "Selected Variant:" then Some(l.Substring(17).Trim()) else None)
        let lines =
            raw
            |> List.choose (pobLine selectedVariant)
            |> List.map clean
            |> List.filter (fun l -> l <> "")
        let baseName = lines |> List.tryFind (fun l -> Map.containsKey l db.Weapons)
        let name =
            lines |> List.tryFind (fun l -> not (isHeader l)) |> Option.defaultValue ""
        // Mod lines: everything that isn't a header, the name or the base.
        let candidates =
            lines
            |> List.filter (fun l ->
                not (isHeader l) && Some l <> baseName && l <> name && not (db.BaseNames.Contains l)
                // handled by the build: allocated passives
                && not (l.StartsWith "Allocates ")
                // already reflected in the reported values of socketed runes / idols
                && not (l.Contains "effect of Socketed") && not (l.StartsWith "This item gains bonuses from Socketed Items"))
        // armour pieces and weapons carry local stats; jewellery, jewels and charms don't
        let hasLocalDefences =
            lines |> List.exists (fun l -> l.StartsWith "Armour:" || l.StartsWith "Evasion" || l.StartsWith "Energy Shield:")
        let context = if baseName.IsSome || hasLocalDefences then StatText.ItemLocal else StatText.Global
        let stats, unknown = StatText.parseLinesIn context db.Text candidates
        let mods, locals, cov = interpret stats
        // Lines before the first understood stat are usually the name block; don't report them.
        let unknown = unknown |> List.filter (fun l -> l.Contains " " && not (Some l = baseName))
        let defences =
            lines
            |> List.choose (fun l ->
                let value (prefix: string) = match System.Double.TryParse(l.Substring(prefix.Length).Trim().Split(' ').[0].TrimEnd('%')) with | true, v -> Some v | _ -> None
                if l.StartsWith "Armour:" then value "Armour:" |> Option.map (fun v -> Armour, v)
                elif l.StartsWith "Evasion Rating:" then value "Evasion Rating:" |> Option.map (fun v -> Evasion, v)
                elif l.StartsWith "Evasion:" then value "Evasion:" |> Option.map (fun v -> Evasion, v)
                elif l.StartsWith "Energy Shield:" then value "Energy Shield:" |> Option.map (fun v -> MaxEnergyShield, v)
                elif l.StartsWith "Block chance:" then value "Block chance:" |> Option.map (fun v -> Named "ShieldBlock", v)
                elif l.StartsWith "Runic Ward:" then value "Runic Ward:" |> Option.map (fun v -> Named "Ward", v)
                else None)
        let numbers (l: string) =
            l.Split(' ') |> Array.choose (fun w -> match System.Double.TryParse(w.Trim(',')) with | true, v -> Some v | _ -> None) |> List.ofArray
        let flaskOf (pool: string) =
            let recovers = lines |> List.tryFind (fun l -> l.StartsWith "Flask: Recovers" && l.Contains pool)
            let consumes = lines |> List.tryFind (fun l -> l.StartsWith "Flask: Consumes")
            match recovers |> Option.map numbers, consumes |> Option.map numbers with
            | Some [ amount; duration ], Some [ use'; maxC ] -> Some(amount, duration, use', maxC)
            | _ -> None
        let flask = flaskOf "Mana"
        { Name = name
          BaseName = baseName |> Option.defaultValue ""
          Weapon = baseName |> Option.map (fun b -> weapon db.Weapons.[b] (quality lines) locals)
          Mods = mods
          Defences = defences
          ManaFlask = flask
          LifeFlask = flaskOf "Life"
          Coverage = { cov with UnknownText = unknown } }

module Tree =
    let parseNodes (json: string) : Map<int, TreeNode> =
        match Json.field "nodes" (Json.parse json) with
        | Some (JObj m) ->
            m
            |> Map.toList
            |> List.choose (fun (k, j) ->
                match System.Int32.TryParse k with
                | true, id ->
                    Some(
                        id,
                        { Id = id
                          Name = Json.strField "name" j |> Option.defaultValue ""
                          Stats = Json.list (Json.field "stats" j) |> List.choose Json.str
                          IsKeystone = Json.field "isKeystone" j = Some(JBool true)
                          IsNotable = Json.field "isNotable" j = Some(JBool true) }
                    )
                | _ -> None)
            |> Map.ofList
        | _ -> Map.empty

    /// Keystones the engine handles by name (their text is a rule, not a stat).
    let private ruledKeystones = set [ "Chaos Inoculation"; "Pyromantic Pact" ]

    /// Mods from allocated passives. Handled keystones become a flag `Named "Keystone:<name>"`.
    let mods (db: GameDb) (nodes: Map<int, TreeNode>) (allocated: int list) =
        allocated
        |> List.choose (fun id -> Map.tryFind id nodes)
        |> List.fold
            (fun (mods, cov: Coverage) node ->
                let ruled = if ruledKeystones.Contains node.Name then [ Mod.flag (Named("Keystone:" + node.Name)) |> Mod.from node.Name ] else []
                let mods = mods @ ruled
                if node.IsKeystone && not ruled.IsEmpty then mods, cov
                else
                let relevant =
                    node.Stats
                    |> List.filter (fun l ->
                        not (l.StartsWith "+5 to any Attribute") && not (l.StartsWith "Grants Skill:")
                        && not (ruledKeystones.Contains node.Name && (l.Contains "Infernal Flame instead" || l.StartsWith "Maximum Mana is replaced")))
                let stats, unknown = StatText.parseLines db.Text relevant
                let ms, _, c = Items.interpret stats
                let unknown = unknown |> List.map (fun l -> sprintf "%s: %s" node.Name l)
                mods @ ms, Coverage.merge cov { c with UnknownText = c.UnknownText @ unknown })
            ([], Coverage.empty)
