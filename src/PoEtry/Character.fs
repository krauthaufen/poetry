namespace PoEtry

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
      IsNotable: bool
      IsJewelSocket: bool
      /// The ascendancy the node belongs to, "" on the main tree.
      Ascendancy: string
      /// Node ids this node's own connections lead to.
      Connections: int list
      /// Position on the tree (tools/treegeo.py); 0, 0 if unknown.
      X: float
      Y: float }

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
              "EvasionBasePercentile:"; "EnergyShieldBasePercentile:"; "Cluster Jewel"; "Flask: "; "Runic Ward:"; "Sockets filled:"; "Idols socketed:"; "Grants Skill:" ]
        prefixes |> List.exists (fun p -> line.StartsWith p)

    /// Lines that describe the item itself or character-building, with no effect in a fight.
    let private inertLine (l: string) =
        [ "Corrupting will always result in change"; "Grants "; "Forged by "; "Can tattoo Runes"; "additional Rune-only sockets"
          "Triple Attribute requirements"; "Has "; "Passive Skill Points become Weapon Set" ]
        |> List.exists (fun p ->
            if p = "Grants " then l.StartsWith p && l.Contains "Passive Skill Point"
            elif p = "Has " then l.StartsWith p && (l.EndsWith "Charm Slots" || l.EndsWith "Charm Slot" || l.Contains "Socket")
            else l.StartsWith p || l.EndsWith p)
        || (l.EndsWith " socket" || l.EndsWith " sockets") && l.Length < 30

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
        // the base type, as gems name it in their weapon requirements
        let baseType = Tag("Base:" + (if b.BaseTags.Contains "warstaff" then "Quarterstaff" else b.Type))
        hands :: baseType :: kind

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
        // quality multiplies: a bow with 255% increased Physical Damage and 30% quality shows
        // base x 3.55 x 1.30
        let incPhys = sum ((=) LocalDamageInc)
        let added t =
            { Min = sum ((=) (LocalAddedMin t))
              Max = sum ((=) (LocalAddedMax t)) }
        let damage =
            DamageType.all
            |> List.choose (fun t ->
                let b = Map.tryFind t b.Damage |> Option.defaultValue Range.zero
                let r = Range.add b (added t)
                let r = if t = Physical then Range.scale ((1.0 + incPhys / 100.0) * (1.0 + qualityPct / 100.0)) r else r
                if r.Max > 0.0 then Some(t, r) else None)
            |> Map.ofList
        { Damage = damage
          // local crit is stored in hundredths of a percent
          CritChance = (b.CritChance + sum ((=) LocalCritChance) / 100.0) * (1.0 + sum ((=) LocalCritChanceInc) / 100.0)
          AttacksPerSecond = b.AttacksPerSecond * (1.0 + sum ((=) LocalAttackSpeedInc) / 100.0)
          ReloadTime = b.ReloadTime / (1.0 + sum ((=) (LocalOther "local_reload_speed_+%")) / 100.0)
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

    /// Lines the stat descriptions don't cover on their own: a known line with a condition
    /// appended ("... for each Corrupted Item Equipped"), a few lines with rules of their own,
    /// and lines about building the character that do nothing in a fight.
    /// Returns the mods and the lines still not understood.
    let rescue (db: GameDb) (unknown: string list) : Mod list * string list =
        let number (l: string) = l.Split(' ') |> Array.tryPick (fun w -> match System.Double.TryParse(w.TrimStart('+').TrimEnd('%')) with | true, v -> Some v | _ -> None)
        let numbers (l: string) = l.Split(' ') |> Array.choose (fun w -> match System.Double.TryParse(w.TrimStart('+').TrimEnd('%')) with | true, v -> Some v | _ -> None) |> List.ofArray
        let inert (l: string) =
            [ "Passive Skill Point"; "Triple Attribute requirements"; "Can tattoo Runes"; "additional Rune-only sockets"
              "For each colour of Socketed Support Gem"; "Movement Speed Penalty from using Skills" ]
            |> List.exists l.Contains
            || ((l.EndsWith " socket" || l.EndsWith " sockets") && l.Length < 30)
        let conditional =
            [ " for each Corrupted Item Equipped", PerStack("CorruptedItems", 1.0) ]
        unknown
        |> List.fold
            (fun (mods, left) (line: string) ->
                let l = line.Trim()
                // "... per 2 Strength": the same line without it, once per so many of the attribute
                let perAttribute =
                    [ "Strength", "Str"; "Dexterity", "Dex"; "Intelligence", "Int" ]
                    |> List.tryPick (fun (long, short) ->
                        let i = l.LastIndexOf " per "
                        if i > 0 && l.EndsWith(" " + long) then
                            match System.Double.TryParse(l.Substring(i + 5, l.Length - i - 5 - long.Length - 1)) with
                            | true, n when n > 0.0 -> Some(" per " + l.Substring(i + 5), PerStack(short, n))
                            | _ -> None
                        else None)
                let conditional = conditional @ Option.toList perAttribute
                let suffixed =
                    conditional |> List.tryPick (fun (suffix, cond) ->
                        if l.EndsWith suffix then
                            StatText.parse db.Text (l.Substring(0, l.Length - suffix.Length))
                            |> Option.map (fun stats ->
                                let ms, locals, _ = interpret stats
                                // not on an item here: a line that reads as an item's own stat is the global one
                                let fromLocals =
                                    locals |> List.collect (fun l ->
                                        match l with
                                        | LocalOther id, v when id.StartsWith "local_" -> (match StatMap.meaning (id.Substring 6) v with | Mods m -> m | _ -> [])
                                        | _ -> [])
                                (if ms.IsEmpty then fromLocals else ms) |> List.map (Mod.whenever cond))
                        else None)
                match suffixed with
                | Some ms -> mods @ ms, left
                | None ->
                    if inert l then mods, left
                    elif l.EndsWith "to base Energy Shield Recharge delay" then
                        mods @ (number l |> Option.map (fun v -> Mod.base' (Named "ESRechargeDelayFlat") v) |> Option.toList), left
                    elif l = "Red: Hits against you have no Critical Damage Bonus" then
                        mods @ [ Mod.base' (Named "LessCritDamageTaken") 100.0 |> Mod.whenever (WhileBuff "SupportsMajority:red") ], left
                    elif l.StartsWith "Blue: Skills have " && l.EndsWith "% less cost" then
                        mods @ (number l |> Option.map (fun v -> Mod.more ManaCost -v |> Mod.whenever (WhileBuff "SupportsMajority:blue")) |> Option.toList), left
                    elif l.EndsWith "more Reservation Efficiency of Companion Skills" then
                        mods @ (number l |> Option.map (fun v -> Mod.more (Named "ReservationEfficiency") v |> Mod.whenever (SkillIs(Tag "Type:Companion"))) |> Option.toList), left
                    elif l.EndsWith "less Reservation Efficiency of non-Companion Skills" then
                        mods @ (number l |> Option.map (fun v -> Mod.more (Named "ReservationEfficiency") -v |> Mod.whenever (Not(SkillIs(Tag "Type:Companion")))) |> Option.toList), left
                    elif l.StartsWith "Non-Minion Skills have " && l.EndsWith "less Reservation Efficiency" then
                        mods @ (number l |> Option.map (fun v -> Mod.more (Named "ReservationEfficiency") -v |> Mod.whenever (Not(SkillIs(Tag "Type:Minion")))) |> Option.toList), left
                    // life flasks that are never drunk (see Pob.character). Their recovery is spread over
                    // time, so it is never instant; nothing but the player recovers from a flask here
                    elif l = "Non-Unique Life Flasks apply their Effects constantly" then mods @ [ Mod.flag (Named "ConstantLifeFlasks") ], left
                    elif l = "Recovery from Life Flasks cannot be Instant" || l = "Recovery from your Life Flasks cannot be applied to anything other than you" then mods, left
                    // Darkness: "You have no Spirit" is read by its own stat
                    elif l.StartsWith "Base Maximum Darkness is " then
                        mods @ (number l |> Option.map (fun v -> Mod.base' (Named "Darkness") v) |> Option.toList), left
                    elif l = "Damage taken is Reserved from Darkness before being taken from Life or Energy Shield" then mods, left
                    // Hollow Palm Technique (see Pob.character: the hands must be empty)
                    elif l = "Can Attack as though using a Quarterstaff while both of your hand slots are empty" then mods @ [ Mod.flag (Named "HollowPalm") ], left
                    elif l = "Unarmed Attacks that would use an Equipped Quarterstaff's damage have:" || l = "Base Unarmed Physical damage replaced with damage based on their Skill Level" then mods, left
                    elif l.EndsWith "more Attack Speed per 75 Item Evasion on Equipped Armour Items" || (l.Contains "more Attack Speed per " && l.EndsWith "Item Evasion on Equipped Armour Items") then
                        match numbers l with
                        | [ a; b ] when b > 0.0 -> mods @ [ Mod.base' (Named "HollowPalmSpeed") a; Mod.base' (Named "HollowPalmSpeedPerEvasion") b ], left
                        | _ -> mods, left @ [ line ]
                    elif l.Contains "to Critical Hit Chance per " && l.EndsWith "Item Energy Shield on Equipped Armour Items" then
                        match numbers l with
                        | [ a; b ] when b > 0.0 -> mods @ [ Mod.base' (Named "HollowPalmCrit") a; Mod.base' (Named "HollowPalmCritPerES") b ], left
                        | _ -> mods, left @ [ line ]
                    elif l.Contains "to maximum Mana per " && l.EndsWith "Item Armour on Equipped Helmet" then
                        match numbers l with
                        | [ a; b ] when b > 0.0 -> mods @ [ Mod.base' (Named "ManaPerHelmetArmour") (a / b) ], left
                        | _ -> mods, left @ [ line ]
                    else mods, left @ [ line ])
            ([], [])

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
    /// A line of an item's text that holds one of its mods as stat ids (the game's API sends
    /// them beside the text): "{stats} id=value;id=value".
    let statsLinePrefix = "{stats} "

    /// Lines the stat descriptions could not read, matched with the item's mods as the game
    /// sent them: a mod none of whose stat ids is already read from the item's text, whose
    /// values are all numbers of the line (or 1, a flag), explains the line. Each mod once, in
    /// order. Returns the stats found and the lines still not understood.
    let private fromStatLines (mods: (string * float) list list) (known: Set<string>) (unknown: string list) =
        let numbers (l: string) =
            System.Text.RegularExpressions.Regex.Matches(l, @"\d+(\.\d+)?") |> Seq.map (fun m -> float m.Value) |> List.ofSeq
        let fits (line: string) (m: (string * float) list) =
            let ns = numbers line
            not (m |> List.exists (fun (id, _) -> known.Contains id))
            && m |> List.forall (fun (_, v) -> abs v = 1.0 || abs v = 0.0 || ns |> List.exists (fun n -> abs (n - abs v) < 1e-6))
            && (ns.IsEmpty || m |> List.exists (fun (_, v) -> ns |> List.exists (fun n -> abs (n - abs v) < 1e-6)))
        unknown
        |> List.fold
            (fun (found, left, rest) line ->
                match left |> List.tryFindIndex (fits line) with
                | Some i -> found @ left.[i], List.removeAt i left, rest
                | None -> found, left, rest @ [ line ])
            ([], mods, [])
        |> fun (found, _, rest) -> found, rest

    let parse (db: GameDb) (text: string) : ParsedItem =
        let raw = text.Replace("\r", "").Split('\n') |> List.ofArray |> List.map (fun l -> l.Trim())
        // the mods as stat ids, apart from the text
        let statMods =
            raw |> List.filter (fun l -> l.StartsWith statsLinePrefix)
            |> List.map (fun l ->
                l.Substring(statsLinePrefix.Length).Split(';') |> List.ofArray
                |> List.choose (fun kv ->
                    match kv.Split('=') with
                    | [| k; v |] -> (match System.Double.TryParse(v, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with | true, x -> Some(k, x) | _ -> None)
                    | _ -> None))
        let raw = raw |> List.filter (fun l -> not (l.StartsWith statsLinePrefix))
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
                && not (l.Contains "effect of Socketed") && not (l.StartsWith "This item gains bonuses from Socketed Items")
                // about the item or the character sheet, not about a fight
                && not (inertLine l))
        // armour pieces and weapons carry local stats; jewellery, jewels and charms don't
        let hasLocalDefences =
            lines |> List.exists (fun l -> l.StartsWith "Armour:" || l.StartsWith "Evasion" || l.StartsWith "Energy Shield:" || l.StartsWith "Runic Ward:" || l.StartsWith "Block chance:" || l.StartsWith "Spirit:")
        let showsWard = lines |> List.exists (fun l -> l.StartsWith "Runic Ward:")
        let context = if baseName.IsSome || hasLocalDefences then StatText.ItemLocal else StatText.Global
        let stats, unknown = StatText.parseLinesIn context db.Text candidates
        // a line the descriptions here don't have (a new league's wording, a Foulborn unique's
        // line): the game's own stat ids for it, where it sent them
        let stats, unknown =
            if statMods.IsEmpty || unknown.IsEmpty then stats, unknown
            else
                // (not what is read where the item is used: a radius jewel's "Passive Skills in
                // Radius also grant ...", a "Bonded:" line)
                let later (l: string) = l.StartsWith "Bonded:" || l.Contains " in Radius" || l.Contains "Radius"
                let found, rest = fromStatLines statMods (stats |> List.map fst |> Set.ofList) (unknown |> List.filter (later >> not))
                stats @ found, (unknown |> List.filter later) @ rest
        // What only a weapon has of its own (attack speed, critical hits, accuracy) is the
        // character's on an armour piece: "16% increased Attack Speed" on gloves.
        let stats =
            if baseName.IsSome then stats
            else
                stats |> List.map (fun (id: string, v) ->
                    if id.StartsWith "local_" && (id.Contains "attack_speed" || id.Contains "critical_strike" || id.Contains "accuracy_rating") then id.Substring 6, v else id, v)
        let mods, locals, cov = interpret stats
        let rescued, unknown = rescue db unknown
        let mods = mods @ rescued
        // an armour base slows its wearer (the base's own, on no line of the item)
        let mods =
            match lines |> List.tryPick (fun l -> Map.tryFind l db.MovePenalties) with
            | Some p -> mods @ [ Mod.base' (Named "OwnMovePenalty") (p * 100.0) ]
            | None -> mods
        // Other item-local stats. Some only change what the item already displays (its
        // defences, requirements, sockets, quality) or how it can be crafted: nothing to add.
        // The rest act when the item is used: a weapon's ("Leeches 8% of Physical Damage as
        // Life", "+25% to Critical Damage Bonus") count for attacks with it, read like the
        // same stat without "local_". What that doesn't explain is reported.
        let socketsFilled = lines |> List.tryPick (fun l -> if l.StartsWith "Sockets filled:" then Some(float (l.Substring(15).Trim())) else None) |> Option.defaultValue 0.0
        let localMods, localCov =
            locals
            |> List.fold
                (fun (ms, c: Coverage) (l, v) ->
                    match l with
                    | LocalOther id ->
                        let has (w: string) = id.Contains w
                        let shown =
                            [ "armour"; "physical_damage_reduction_rating"; "evasion"; "energy_shield"; "ward"; "block_chance"; "requirement"; "prefixes"; "suffixes"; "quality"
                              "_display_"; "socket"; "local_is_"; "implicit_stat_magnitude"; "soul_core"; "weapon_range"; "reload_speed"
                              "corruption"; "charm"; "flask"; "local_jewel_"; "alternate_tree"; "cannot_be_thrown"; "mod_effect"
                              "hungry_loop"; "mages_legacy"; "synthesised"; "in_breach"; "base_crit_chance_permyriad_override"
                              "no_physical_damage"; "spirit"; "mod_magnitudes"; "can_be_instilled" ]
                            |> List.exists has
                        if id.EndsWith "_per_rune_or_soul_core" then
                            // "per Socket filled": once per rune, soul core or idol in the item
                            let n = v * socketsFilled
                            let attrs = [ Attribute "Str"; Attribute "Dex"; Attribute "Int" ]
                            let found =
                                match id.Substring(6, id.Length - 6 - "_per_rune_or_soul_core".Length) with
                                | "resist_all_elements_+%" -> Mod.resistAllElemental n
                                | "all_attributes_+" -> attrs |> List.map (fun a -> Mod.base' a n)
                                | "all_attributes_+%" -> attrs |> List.map (fun a -> Mod.inc a n)
                                | "base_chaos_damage_resistance_%" -> [ Mod.base' (Resist Chaos) n ]
                                | "base_life_regeneration_rate_per_minute_+" -> [ Mod.base' LifeRegen (n / 60.0) ]
                                | "base_maximum_life_+" -> [ Mod.base' MaxLife n ]
                                | "base_maximum_mana_+" -> [ Mod.base' MaxMana n ]
                                | "maximum_life_+%" -> [ Mod.inc MaxLife n ]
                                | "maximum_mana_+%" -> [ Mod.inc MaxMana n ]
                                | "global_armour_evasion_energy_shield_+%" -> [ Mod.inc Armour n; Mod.inc Evasion n; Mod.inc MaxEnergyShield n ]
                                | "stun_threshold_+" -> [ Mod.base' (Named "StunThreshold") n ]
                                | "spirit_+" -> [ Mod.base' (Named "Spirit") n ]
                                | _ -> []
                            ms @ found, c
                        elif id = "local_ring_duplicate_other_ring" then ms, c
                        // "+X to Spirit": part of a sceptre's own Spirit, a plain bonus on anything else
                        elif id = "local_spirit" && not (lines |> List.exists (fun l -> l.StartsWith "Spirit:")) then ms @ [ Mod.base' (Named "Spirit") v ], c
                        // "20% increased Spirit" likewise: local on a sceptre, global on anything else
                        elif id = "local_spirit_+%" && not (lines |> List.exists (fun l -> l.StartsWith "Spirit:")) then ms @ [ Mod.inc (Named "Spirit") v ], c
                        elif shown then ms, c
                        else
                            // local stats with a name of their own; the rest read like the same stat without "local_"
                            let known =
                                match id with
                                | "local_life_leech_from_physical_damage_permyriad" -> Mods [ Mod.base' (Named "LifeLeech") (v / 100.0) ]
                                | "local_mana_leech_from_physical_damage_permyriad" -> Mods [ Mod.base' ManaLeech (v / 100.0) ]
                                | "local_life_gain_per_target" -> Mods [ Mod.base' (Named "LifeOnHit") v ]
                                | "local_life_and_mana_gain_per_target" -> Mods [ Mod.base' (Named "LifeOnHit") v; Mod.base' (Named "ManaOnHit") v ]
                                | "local_chance_to_bleed_on_hit_%" -> Mods [ Mod.base' (AilmentChance Bleed) v ]
                                | "local_chance_to_bleed_on_hit_25%" -> Mods [ Mod.base' (AilmentChance Bleed) 25.0 ]
                                | "local_chance_to_bleed_on_hit_50%" -> Mods [ Mod.base' (AilmentChance Bleed) 50.0 ]
                                | "local_bleed_on_hit" -> Mods [ Mod.base' (AilmentChance Bleed) 100.0 ]
                                | "local_poison_on_hit_%" -> Mods [ Mod.base' (AilmentChance Poison) v ]
                                | "local_poison_on_hit" -> Mods [ Mod.base' (AilmentChance Poison) 100.0 ]
                                // "Bifurcates Critical Hits" on a weapon
                                | "local_weapon_roll_crits_twice" -> Mods [ Mod.flag (Named "BifurcateCrits") ]
                                | "local_chaos_penetration_%" -> Mods [ Mod.base' (Penetration Chaos) v ]
                                | "local_elemental_penetration_%" -> Mods (DamageType.elemental |> List.map (fun t -> Mod.base' (Penetration t) v))
                                | _ when id.StartsWith "local_%_chance_to_trigger_" && id.Contains "_on_hit_with_this_weapon_per_" ->
                                    // "Hits with this Weapon have 5% chance to Trigger Molten Shower per 25 Strength"
                                    let words (x: string) = x.Split('_') |> Array.filter (fun w -> w <> "")
                                    let skill = words (id.Substring(26, id.IndexOf "_on_hit_with_this_weapon_per_" - 26)) |> Array.map (fun w -> string (System.Char.ToUpperInvariant w.[0]) + w.Substring 1) |> String.concat " "
                                    match words (id.Substring(id.IndexOf "_per_" + 5)) with
                                    | [| n; attr |] ->
                                        let a = match attr with "strength" -> "Str" | "dexterity" -> "Dex" | _ -> "Int"
                                        Mods [ Mod.base' (Named("TriggerOnHit:" + skill)) v |> Mod.whenever (PerStack(a, float n)) ]
                                    | _ -> Unmapped
                                | _ -> StatMap.meaning (id.Substring 6) v
                            match known with
                            | Mods found ->
                                // a weapon's own stats are for attacks with it
                                ms @ (if baseName.IsSome then found |> List.map (Mod.whenever (SkillIs Attack)) else found), c
                            // (understood, and nothing in a fight: as the same stat without "local_")
                            | NotOffensive | Structural -> ms, c
                            | _ -> ms, { c with Unsimulated = c.Unsimulated @ [ id, "item-local" ] }
                    | _ -> ms, c)
                ([], Coverage.empty)
        // an item that shows Runic Ward has its own "+X to maximum Runic Ward" and
        // "X% increased Runic Ward" in that number already; an armour piece with no armour,
        // evasion or energy shield of its own gets nothing from its local increases
        let shownHere (st: Stat) =
            let has (prefix: string) = lines |> List.exists (fun l -> l.StartsWith prefix)
            match st with
            | Named "Ward" -> showsWard
            | Named "Spirit" -> has "Spirit:"
            | Armour -> hasLocalDefences && not (has "Armour:")
            | Evasion -> hasLocalDefences && not (has "Evasion")
            | MaxEnergyShield -> hasLocalDefences && not (has "Energy Shield:")
            | _ -> false
        let mods = if hasLocalDefences && baseName.IsNone then mods |> List.filter (fun m -> not (shownHere m.Stat && m.Conds.IsEmpty)) else mods
        let mods, cov = mods @ localMods, Coverage.merge cov localCov
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
                elif l.StartsWith "Ward:" then value "Ward:" |> Option.map (fun v -> Named "Ward", v)
                elif l.StartsWith "Spirit:" then value "Spirit:" |> Option.map (fun v -> Named "Spirit", v)
                // (the Strength the item asks for, where the character data says it)
                elif l.StartsWith "Str:" then value "Str:" |> Option.map (fun v -> Named "StrRequirement", v)
                else None)
        let numbers (l: string) =
            l.Split(' ') |> Array.choose (fun w -> match System.Double.TryParse(w.Trim(',')) with | true, v -> Some v | _ -> None) |> List.ofArray
        let flaskOf (pool: string) =
            let recovers = lines |> List.tryFind (fun l -> l.StartsWith "Flask: Recovers" && l.Contains pool)
            let consumes = lines |> List.tryFind (fun l -> l.StartsWith "Flask: Consumes")
            match recovers |> Option.map numbers, consumes |> Option.map numbers with
            | Some [ amount; duration ], Some [ use'; maxC ] -> Some(amount, duration, use', maxC)
            | _ ->
                // a build file names the flask and its modifiers, not what it then shows: from
                // the base (the longest base name within the item's), its quality and its lines
                let named = db.Flasks |> Map.toList |> List.filter (fun (b, f) -> f.Pool = pool && lines |> List.exists (fun l -> not (isHeader l) && l.Contains b))
                match recovers, (if named.IsEmpty then None else Some(named |> List.maxBy (fun (b, _) -> b.Length) |> snd)) with
                | None, Some f ->
                    // "50% reduced Amount Recovered": the line is exactly the number and these words
                    let changed (what: string) =
                        lines |> List.sumBy (fun l ->
                            if l.EndsWith("% increased " + what) then (match System.Double.TryParse(l.Substring(0, l.IndexOf '%')) with | true, v -> v | _ -> 0.0)
                            elif l.EndsWith("% reduced " + what) then (match System.Double.TryParse(l.Substring(0, l.IndexOf '%')) with | true, v -> -v | _ -> 0.0)
                            else 0.0)
                    let scaled v what = v * (1.0 + changed what / 100.0)
                    Some(floor (scaled (f.Amount * (1.0 + quality lines / 100.0)) "Amount Recovered"),
                         f.Duration / (1.0 + changed "Recovery rate" / 100.0),
                         floor (scaled f.ChargesUsed "Charges per use"),
                         floor (scaled f.ChargesMax "Charges"))
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
                          IsNotable = Json.field "isNotable" j = Some(JBool true)
                          IsJewelSocket = Json.field "isJewelSocket" j = Some(JBool true)
                          Ascendancy = Json.strField "ascendancyName" j |> Option.defaultValue ""
                          Connections = Json.list (Json.field "connections" j) |> List.choose (Json.numField "id") |> List.map int
                          X = Json.numField "x" j |> Option.defaultValue 0.0
                          Y = Json.numField "y" j |> Option.defaultValue 0.0 }
                    )
                | _ -> None)
            |> Map.ofList
        | _ -> Map.empty

    /// Jewel socket node ids in the order the character API numbers them (jewel "x").
    let jewelSlots (json: string) : int list =
        Json.list (Json.field "jewelSlots" (Json.parse json)) |> List.choose Json.num |> List.map int

    /// Radius of a jewel in tree units: its size x PassiveTreeJewelDistanceMultiplier
    /// (sizes: PoB data.jewelRadii).
    let jewelRadius (size: string) =
        let outer =
            match size.Trim() with
            | "Small" -> Some 1000.0
            | "Medium" -> Some 1150.0
            | "Large" -> Some 1300.0
            | "Very Large" -> Some 1500.0
            | _ -> None
        outer |> Option.map (fun o -> o * Map.find "PassiveTreeJewelDistanceMultiplier" Tables.gameConstants)

    /// Keystones the engine handles by name (their text is a rule, not a stat).
    let private ruledKeystones = set [ "Chaos Inoculation"; "Pyromantic Pact" ]

    /// Keystone lines the stat descriptions don't cover: (keystone, start of the line, mods).
    let private keystoneLines : (string * string * Mod list) list =
        [ "Crimson Assault", "Base Bleeding Duration is 1 second", [ Mod.override' (Named "BaseBleedDuration") 1.0 ]
          "Crimson Assault", "50% more Magnitude of Bleeding", [ Mod.more (AilmentMagnitude(Some Bleed)) 50.0 ]
          "Vaal Pact", "50% more amount of Life Leeched", [ Mod.more (Named "LifeLeechAmount") 50.0 ]
          "Vaal Pact", "Leech Life 67% less quickly", [ Mod.more (Named "LifeLeechRate") -67.0 ]
          "Vaal Pact", "Cannot Recover Life other than from Leech", [ Mod.flag (Named "OnlyLeechRecoversLife") ]
          "Giant's Blood", "Inherent Life granted by Strength is halved", [ Mod.flag (Named "HalfLifeFromStrength") ]
          "Wildsurge Incantation", "Storm and Plant Spells:", []
          "Wildsurge Incantation", "cost 50% less",
          [ for t in [ "Type:Storm"; "Type:Plant" ] -> Mod.more ManaCost -50.0 |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag t)) ]
          "Wildsurge Incantation", "have 75% less duration",
          [ for t in [ "Type:Storm"; "Type:Plant" ] -> Mod.more SkillDuration -75.0 |> Mod.whenever (SkillIs Spell) |> Mod.whenever (SkillIs(Tag t)) ]
          "Scarred Faith", "Energy Shield does not Recharge", [ Mod.flag (Named "NoESRecharge") ]
          "Scarred Faith", "5% of Physical Damage prevented Recouped as Energy Shield per enemy Power", [ Mod.base' (Named "RecoupPreventedPhysicalAsESPerPower") 5.0 ]
          "Scarred Faith", "You cannot Recover Energy Shield from Regeneration", [ Mod.flag (Named "NoESRegen") ]
          "Scarred Faith", "You cannot Recover Energy Shield to above Armour", [ Mod.flag (Named "ESRecoveryCappedAtArmour") ]
          "Whispers of Doom", "Double Activation Delay of Curses", []
          "Unwavering Stance", "Cannot Dodge Roll or Sprint", [] ]

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
                // "Body Armour grants 15% increased maximum Life" (Smith of Kitava): the rest of the
                // line is a modifier like any other, there while a body armour is worn
                let relevant =
                    relevant |> List.map (fun l ->
                        if l.StartsWith "Body Armour grants " && l.Length > 19 then string (System.Char.ToUpperInvariant l.[19]) + l.Substring 20 else l)
                let ruledLines, relevant =
                    relevant |> List.partition (fun l -> keystoneLines |> List.exists (fun (k, start, _) -> k = node.Name && l.StartsWith start))
                let lineMods =
                    ruledLines |> List.collect (fun l ->
                        keystoneLines |> List.collect (fun (k, start, ms) -> if k = node.Name && l.StartsWith start then ms |> List.map (Mod.from node.Name) else []))
                let stats, unknown = StatText.parseLines db.Text relevant
                let ms, _, c = Items.interpret stats
                let rescued, unknown = Items.rescue db unknown
                let ms = (ms @ rescued @ lineMods) |> List.map (fun m -> if m.Source = "" then Mod.from ("tree: " + node.Name) m else m)
                let unknown = unknown |> List.map (fun l -> sprintf "%s: %s" node.Name l)
                mods @ ms, Coverage.merge cov { c with UnknownText = c.UnknownText @ unknown })
            ([], Coverage.empty)
        // (a passive the tree data does not have: the tree is older than the character's game)
        |> fun (mods, cov) ->
            match allocated |> List.filter (fun id -> not (Map.containsKey id nodes)) with
            | [] -> mods, cov
            | missing ->
                let shown = missing |> List.truncate 5 |> List.map string |> String.concat ", "
                mods, { cov with UnknownText = cov.UnknownText @ [ sprintf "%d allocated passives are not in the tree data (%s%s): the passive tree data is out of date" missing.Length shown (if missing.Length > 5 then ", ..." else "") ] }
