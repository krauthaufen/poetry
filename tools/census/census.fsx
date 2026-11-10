// What the simulator understands of ALL game data, not of one build.
//   dotnet fsi tools/census/census.fsx      (after building src/PoEsie in Release)
// Every stat on affix mods, passive nodes and skill / support gems is classified:
//   simulated   maps to mods whose stat the engine reads
//   inert       maps to mods, but nothing outside the stat mapping reads that stat
//   local / not-offensive / structural / unsimulated / unmapped   as StatMap says
// and every skill type flag is checked against the engine source.
// Writes tools/census/census.txt; prints the summary.
#r "../../src/PoEsie/bin/Release/net8.0/PoEsie.dll"
open PoEsie

let root = __SOURCE_DIRECTORY__ + "/../../"
let dir = root + "data/game/"
let read f = System.IO.File.ReadAllText(dir + f)

// ---- engine source: which stats are actually read ----
let engineFiles =
    System.IO.Directory.GetFiles(root + "src/PoEsie", "*.fs")
    |> Array.filter (fun f ->
        let n = System.IO.Path.GetFileName f
        n <> "StatMap.fs" && n <> "StatText.fs" && n <> "Tables.fs" && n <> "RarityData.fs")
let engine = engineFiles |> Array.map System.IO.File.ReadAllText |> String.concat "\n"
let cliSource = System.IO.File.ReadAllText(root + "cli/Program.fs")
let allEngine = engine + "\n" + cliSource
let literals =
    System.Text.RegularExpressions.Regex.Matches(allEngine, "\"([A-Za-z:]+)\"")
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Set.ofSeq
/// A Named stat counts as read if its name, or a literal it is built from ("Max" + r), appears.
let namedRead (n: string) =
    literals.Contains n
    || literals |> Set.exists (fun l -> l.Length >= 4 && (n.StartsWith l || n.EndsWith l) && l <> n)
let caseName (s: Stat) =
    let t = sprintf "%A" s
    t.Split([| ' '; '(' |]).[0]
/// A non-Named stat counts as read if its case name appears outside the type definition.
let caseRead (c: string) =
    System.Text.RegularExpressions.Regex.Matches(engine.Replace("| " + c + "\n", "").Replace("| " + c + " of", ""), "\\b" + c + "\\b").Count > 0
let statRead (s: Stat) =
    match s with
    | Named n -> namedRead n
    | _ -> caseRead (caseName s)

/// Gem mechanics (base damage, radius, projectiles, energy, remnants) read stat ids directly.
let readById (id: string) = allEngine.Contains("\"" + id + "\"")
/// Conditions the engine never makes true ("while Shapeshifted"): the buff or enemy status
/// name must be set somewhere in the engine.
let rec condDead (c: Cond) =
    match c with
    | WhileBuff b | PerStack (b, _) -> not (namedRead b)
    | EnemyHas st | PerEnemyStack (st, _) -> not (namedRead st)
    | Not inner -> condDead inner
    | _ -> false
let classifyWith meaning id =
    if readById id then "simulated", "by id" else
    match meaning id 10.0 with
    | Mods ms when ms.IsEmpty -> "inert", ""
    | Mods ms ->
        match ms |> List.filter (fun m -> not (statRead m.Stat)) with
        | [] when ms |> List.forall (fun m -> m.Conds |> List.exists condDead) ->
            let dead = ms |> List.collect (fun m -> m.Conds |> List.filter condDead) |> List.distinct |> List.map (sprintf "%A") |> String.concat ","
            "inert", "condition never true: " + dead
        | [] -> "simulated", ""
        | dead -> "inert", (dead |> List.map (fun m -> match m.Stat with Named n -> n | s -> caseName s) |> List.distinct |> String.concat ",")
    | Local _ -> "local", ""
    // a word heuristic, not a mapping: defences ARE simulated now, so this is unreviewed
    | NotOffensive -> "not-offensive", ""
    | Structural -> "structural", ""
    | Unsimulated m -> "unsimulated", m
    | Unmapped -> "unmapped", ""

// ---- sources ----
let index = StatText.build [ read "statdesc_stat_descriptions.json"; read "statdesc_passive_skill_stat_descriptions.json" ]
let objs (j: Json) = match j with JObj m -> m |> Map.toList |> List.map snd | _ -> []
let modLines = objs (Json.parse (read "mods.json")) |> List.map (fun j -> Json.list (Json.field "lines" j) |> List.choose Json.str)
let treeLines =
    Json.field "nodes" (Json.parse (read "tree.json")) |> Option.map objs |> Option.defaultValue []
    |> List.map (fun j -> Json.list (Json.field "stats" j) |> List.choose Json.str)
let textIds (groups: string list list) =
    groups |> List.collect (StatText.parseLines index >> fst) |> List.map fst
let textUnknown (groups: string list list) =
    groups |> List.collect (StatText.parseLines index >> snd)

let skills = objs (Json.parse (read "skills.json"))
let playerGem (j: Json) = Json.strField "name" j |> Option.isSome
let gemStatIds (j: Json) =
    let pairs key (x: Json) = Json.list (Json.field key x) |> List.choose (fun p -> Json.list (Some p) |> List.tryHead |> Option.bind Json.str)
    let sets = Json.list (Json.field "statSets" j)
    (sets |> List.collect (fun s -> pairs "constantStats" s @ (Json.list (Json.field "stats" s) |> List.choose Json.str)))
    @ pairs "qualityStats" j @ pairs "altQualityStats" j
    |> List.distinct
let gems = skills |> List.filter playerGem
let gemIds = gems |> List.collect gemStatIds

// ---- report ----
let out = System.Text.StringBuilder()
let say (s: string) = printfn "%s" s; out.AppendLine s |> ignore
let table name meaning (ids: string list) =
    let counted = ids |> List.countBy id |> List.map (fun (k, n) -> k, n, classifyWith meaning k)
    say (sprintf "\n== %s: %d distinct stats, %d occurrences" name counted.Length ids.Length)
    for c in [ "simulated"; "inert"; "local"; "not-offensive"; "structural"; "unsimulated"; "unmapped" ] do
        let xs = counted |> List.filter (fun (_, _, (k, _)) -> k = c)
        let occ = xs |> List.sumBy (fun (_, n, _) -> n)
        say (sprintf "  %-14s %5d stats %6d occurrences (%4.1f%%)" c xs.Length occ (100.0 * float occ / float (max 1 ids.Length)))
    counted

let affix = table "affix mods" StatMap.meaning (textIds modLines)
let tree = table "passive tree" StatMap.meaning (textIds treeLines)
let gemT = table "skill and support gems" Gems.gemMeaning gemIds
let unknownText = textUnknown (modLines @ treeLines) |> List.distinct
say (sprintf "\ntext lines with no stat template: %d (affix + tree)" unknownText.Length)

// skill types the engine never mentions
let types =
    gems |> List.collect (fun j -> match Json.field "skillTypes" j with Some (JObj m) -> m |> Map.toList |> List.map fst | _ -> [])
    |> List.countBy id |> List.sortByDescending snd
let handled (t: string) = allEngine.Contains("\"" + t + "\"")
let unhandled = types |> List.filter (fst >> handled >> not)
say (sprintf "\nskill types: %d, engine reads %d; unhandled by gem count:" types.Length (types.Length - unhandled.Length))
say ("  " + (unhandled |> List.map (fun (t, n) -> sprintf "%s %d" t n) |> String.concat ", "))

// details file: ranked work lists
let details = System.Text.StringBuilder()
let section title (rows: (string * int * (string * string)) list) kinds =
    details.AppendLine(sprintf "\n## %s" title) |> ignore
    for k in kinds do
        details.AppendLine(sprintf "\n### %s" k) |> ignore
        rows |> List.filter (fun (_, _, (c, _)) -> c = k) |> List.sortByDescending (fun (_, n, _) -> n)
        |> List.iter (fun (id, n, (_, note)) -> details.AppendLine(sprintf "%5d  %s%s" n id (if note = "" then "" else "   [" + note + "]")) |> ignore)
for title, rows in [ "affix mods", affix; "passive tree", tree; "gems", gemT ] do
    section title rows [ "inert"; "unsimulated"; "unmapped"; "not-offensive" ]
details.AppendLine "\n## text lines with no stat template" |> ignore
unknownText |> List.iter (fun l -> details.AppendLine("  " + l) |> ignore)
System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/census.txt", out.ToString() + "\n" + details.ToString())
