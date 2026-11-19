// The character sheet of every corpus character against the one poe.ninja stores (PoB's).
//   dotnet fsi --optimize+ tools/corpus/sheet.fsx                 every difference, by stat
//   dotnet fsi --optimize+ tools/corpus/sheet.fsx 057 Armour ES   one character: the modifiers of the stats named
// Compared as run.fsx does it: with PoB's quest rewards, the nodes PoB ignores and the weapon set PoB had in hand.
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open System
open PoEtry

let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let db = GameDb.load read
let treeJson = read "tree.json"
let args = fsi.CommandLineArgs |> Array.skip 1

let load (path: string) =
    let text = System.IO.File.ReadAllText path
    let build, _ = Setup.parseBuild text
    let ninja = Json.field "ninja" (Json.parse text)
    let setTwo = ninja |> Option.bind (Json.field "pobSecondWeaponSet") = Some(JBool true)
    let o = Options.defaults
    let c =
        match ninja |> Option.bind (Json.field "pobQuestStats") with
        | Some (JArr lines) ->
            let ignored = Json.list (ninja |> Option.bind (Json.field "pobIgnoredNodes")) |> List.choose Json.num |> List.map int
            let overrides = ignored |> List.fold (fun m id -> Map.add id [] m) build.TreeOverrides
            Setup.loadWith db treeJson { build with ExtraLines = lines |> List.choose Json.str; TreeOverrides = overrides } { o with SheetSetTwo = Some setTwo }
        | _ -> Setup.loadWith db treeJson build o
    // PoB's configuration: the switches that are on
    let cfg =
        match ninja |> Option.bind (Json.strField "pathOfBuildingExport") with
        | Some code ->
            let bytes = Convert.FromBase64String(code.Replace('-', '+').Replace('_', '/'))
            use z = new IO.Compression.ZLibStream(new IO.MemoryStream(bytes), IO.Compression.CompressionMode.Decompress)
            use r = new IO.StreamReader(z)
            let xml = r.ReadToEnd()
            Text.RegularExpressions.Regex.Matches(xml, "<Input [^>]*>")
            |> Seq.choose (fun m ->
                let name = Text.RegularExpressions.Regex.Match(m.Value, "name=\"([^\"]*)\"").Groups.[1].Value
                if m.Value.Contains "boolean=\"true\"" then Some name else None)
            |> Set.ofSeq
        | None -> Set.empty
    c, ninja |> Option.bind (Json.field "defensiveStats"), (setTwo, cfg)

let compare (c: Loaded) (theirs: Json option) (setTwo, cfg: Set<string>) =
    let num key = theirs |> Option.bind (Json.field key) |> Option.bind Json.num
    // the conditions PoB was told hold (its configuration): moving, and which charges are up
    let v =
        if Environment.GetEnvironmentVariable "SHEET_OURS" = "1" then c.Vitals
        else
            let res =
                Map.ofList c.Resources
                |> Map.filter (fun k _ ->
                    match k with
                    | "PowerCharge" -> cfg.Contains "usePowerCharges"
                    | "FrenzyCharge" -> cfg.Contains "useFrenzyCharges"
                    | "EnduranceCharge" -> cfg.Contains "useEnduranceCharges"
                    | _ -> true)
                |> fun m -> if cfg.Contains "conditionMoving" then Map.add "Moving" 1 m else m
                |> fun m -> if setTwo then Map.add "WeaponSetTwo" 1 m else m
            Defence.computeWith res c.Bases c.Sheet
    let sheetDb = ModDb.ofList c.Sheet.Mods
    // PoB counts a charm's resistance as always on
    let charmsOn =
        { Ctx.empty with
            Buffs = [ 1 .. 3 ] |> List.fold (fun m i -> Map.add (sprintf "Charm%d" i) 1 m) c.MaxResources
            SkillTags = if setTwo then Set.singleton (Tag "WeaponSet2") else Set.empty }
    let res t = Defence.uncappedResist charmsOn sheetDb t
    let num key =
        match key with
        | "fireResistance" | "coldResistance" | "lightningResistance" | "chaosResistance" -> num key |> Option.map (fun r -> r + (num (key + "OverCap") |> Option.defaultValue 0.0))
        | _ -> num key
    [ "life", v.Life, "life"; "mana", v.Mana, "mana"; "energy shield", v.EnergyShield, "energyShield"
      "strength", v.Str, "strength"; "dexterity", v.Dex, "dexterity"; "intelligence", v.Int, "intelligence"
      "armour", v.Armour, "armour"; "evasion", v.Evasion, "evasionRating"; "deflection", v.Deflection, "deflectionRating"
      "block", v.Block, "blockChance"; "ward", v.Ward, "ward"; "spirit", c.Spirit, "spirit"
      "fire res", res Fire, "fireResistance"; "cold res", res Cold, "coldResistance"
      "lightning res", res Lightning, "lightningResistance"; "chaos res", res Chaos, "chaosResistance" ]
    |> List.choose (fun (name, ours, key) -> num key |> Option.map (fun t -> name, ours, t))
    |> List.filter (fun (name, _, _) -> name <> "ward" && not ((name = "chaos res" || name = "life") && v.Life <= 1.0) && not (name = "mana" && v.Mana <= 0.0))
    |> List.filter (fun (name, _, _) -> not (v.Ward > 0.0 && List.contains name [ "armour"; "evasion"; "energy shield"; "deflection"; "block" ]))
    |> List.map (fun (name, ours, theirs) -> if name = "block" && theirs > ours && ours >= 50.0 then name, ours, ours else name, ours, theirs)

let off (_, a: float, b: float) = abs (a - b) > max 2.0 (0.02 * abs b)
let files = System.IO.Directory.GetFiles(root + "corpus", "*.json") |> Array.filter (fun f -> System.Char.IsDigit (System.IO.Path.GetFileName f).[0]) |> Array.sort

let detail (path: string) (words: string[]) =
    let c, theirs, setTwo = load path
    let hit (s: string) = words |> Array.exists (fun w -> s.Contains w)
    printfn "#### %s (PoB: %s)" (System.IO.Path.GetFileName path) (String.concat ", " ((if fst setTwo then [ "weapon set two" ] else []) @ (snd setTwo |> Set.toList |> List.filter (fun n -> not (n.StartsWith "quest")))))
    for m in c.Sheet.Mods do
        let s = (sprintf "%A" m.Stat).Replace("\n", " ")
        if hit s then printfn "%-34s %-5A %8.1f  %-30s %s" s m.Kind m.Value (m.Conds |> List.map (sprintf "%A") |> String.concat ",") m.Source
    for slot, st, v in c.Sheet.ItemDefences do
        let s = sprintf "%A" st
        if hit s then printfn "%-34s item  %8.1f  %s" s v slot
    for (n, a, b) as r in compare c theirs setTwo do printfn "  %-14s ours %9.0f  theirs %9.0f%s" n a b (if off r then "   <--" else "")
    let cov = c.Sheet.Coverage
    printfn "unknown text:"; cov.UnknownText |> List.iter (printfn "  %s")
    printfn "unsimulated:"; cov.Unsimulated |> List.distinct |> List.iter (fun (s, m) -> printfn "  %s [%s]" s m)
    printfn "unmapped:"; cov.Unmapped |> List.iter (fun (s, v) -> printfn "  %s = %g" s v)

if args.Length > 0 then
    // several characters at once: 057,105,079
    for n in args.[0].Split ',' do
        detail (files |> Array.find (fun f -> (System.IO.Path.GetFileName f).StartsWith n)) (args |> Array.skip 1)
else
    let rows =
        files |> Array.toList |> List.collect (fun path ->
            let file = System.IO.Path.GetFileNameWithoutExtension path
            try let c, theirs, setTwo = load path in compare c theirs setTwo |> List.filter off |> List.map (fun (n, a, b) -> n, file, a, b)
            with e -> [ "failed", file, 0.0, 0.0 ])
    for n, xs in rows |> List.groupBy (fun (n, _, _, _) -> n) |> List.sortByDescending (fun (_, xs) -> xs.Length) do
        printfn "== %s (%d)" n xs.Length
        for _, file, a, b in xs |> List.sortBy (fun (_, _, a, b) -> if b <> 0.0 then a / b else 9.0) do
            printfn "   %-44s %9.0f %9.0f  %s" (if file.Length > 44 then file.Substring(0, 44) else file) a b (if b <> 0.0 then sprintf "%.3f" (a / b) else "-")
