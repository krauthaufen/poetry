/// poesie <build.xml | build code file> [options]
///   --group N        socket group to simulate (default: the build's main group)
///   --runs N         Monte Carlo runs (default 500)
///   --time S         fight length in seconds (default 60)
///   --scenario S     boss | pinnacle | pack | surrounded (default boss)
///   --pack N,M,R     pack of N normal, M magic, R rare monsters (default 8,3,1)
///   --distance D     how far in front the enemies stand, game units (default 30; 10 = 1 m)
///   --level L        monster level (default 83)
///   --life X         override life of every enemy (e.g. 1e12 to measure sustained DPS)
///   --res X          override elemental resistance of every enemy
///   --target T       nearest | strongest (default strongest)
///   --data DIR       exported game data (default: data/game next to the repo)
module PoEsie.Cli

open System
open System.IO
open PoEsie

/// PoB build codes: url-safe base64 of zlib-compressed XML.
let decodeBuildCode (code: string) =
    let b64 = code.Trim().Replace('-', '+').Replace('_', '/')
    let padded = b64 + String('=', (4 - b64.Length % 4) % 4)
    use input = new MemoryStream(Convert.FromBase64String padded)
    use z = new Compression.ZLibStream(input, Compression.CompressionMode.Decompress)
    use reader = new StreamReader(z)
    reader.ReadToEnd()

let rec private opts (args: string list) =
    match args with
    | k :: v :: rest when k.StartsWith "--" -> Map.add (k.Substring 2) v (opts rest)
    | _ :: rest -> opts rest
    | [] -> Map.empty

let private top n (xs: 'a list) = xs |> List.countBy id |> List.sortByDescending snd |> List.truncate n

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | [] ->
        printfn "usage: poesie <build.xml | build-code-file> [--group N] [--runs N] [--time S] [--life X] [--level L] [--res X]"
        1
    | file :: rest ->
        let o = opts rest
        let get k d = Map.tryFind k o |> Option.map float |> Option.defaultValue d
        let dataDir =
            Map.tryFind "data" o
            |> Option.defaultValue (Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "data", "game"))
        let text = File.ReadAllText file
        let xml = if text.TrimStart().StartsWith "<" then text else decodeBuildCode text
        let db = GameDb.load (fun f -> File.ReadAllText(Path.Combine(dataDir, f)))
        let tree = Tree.parseNodes (File.ReadAllText(Path.Combine(dataDir, "tree.json")))
        let build = Pob.read xml
        let sheet = Pob.character db tree build

        printfn "== %s %s, level %d: %d items, %d passives, %d socket groups" build.ClassName build.Ascendancy build.Level build.Items.Length build.TreeNodes.Length build.Groups.Length
        printfn "character: %d modifiers" sheet.Mods.Length
        match sheet.MainHand with
        | Some w -> printfn "main hand: %A, %.1f%% crit, %.2f APS" (w.Damage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max)) w.CritChance w.AttacksPerSecond
        | None -> printfn "main hand: none"
        let cov = sheet.Coverage
        printfn "\n-- not understood on the character (%d unmapped stats, %d unsimulated, %d unknown lines)" cov.Unmapped.Length cov.Unsimulated.Length cov.UnknownText.Length
        cov.Unmapped |> List.map fst |> top 15 |> List.iter (fun (s, n) -> printfn "  unmapped     %s%s" s (if n > 1 then sprintf " x%d" n else ""))
        cov.Unsimulated |> List.map snd |> top 10 |> List.iter (fun (s, n) -> printfn "  unsimulated  %s x%d" s n)
        cov.UnknownText |> List.truncate 15 |> List.iter (printfn "  unknown      %s")

        printfn "\n-- socket groups"
        build.Groups
        |> List.iteri (fun i g ->
            let names = g.Gems |> List.map (fun gem -> sprintf "%s%s %d" (if gem.IsSupport then "+" else "") gem.Name gem.Level) |> String.concat ", "
            printfn "  %2d%s %s" (i + 1) (if i + 1 = build.MainGroup then "*" else " ") names)

        let groupIdx = int (get "group" (float build.MainGroup))
        match List.tryItem (groupIdx - 1) build.Groups with
        | None -> printfn "no socket group %d" groupIdx; 1
        | Some group ->
            match Pob.groupSkill db sheet group |> List.tryHead with
            | None -> printfn "group %d has no active gem" groupIdx; 1
            | Some (Error e) -> printfn "group %d: %s" groupIdx e; 1
            | Some (Ok (skill, skillCov)) ->
                printfn "\n-- simulating %s (group %d)" skill.Name groupIdx
                printfn "  base %s, x%.2f, crit %.1f%%, %.3fs per use%s" (skill.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ") skill.BaseMultiplier skill.BaseCrit skill.BaseTime (match skill.Cooldown with Some c -> sprintf ", cooldown %.2fs" c | None -> "")
                skillCov.Unmapped |> List.iter (fun (s, v) -> printfn "  unmapped skill stat     %s = %g" s v)
                skillCov.Unsimulated |> List.distinctBy snd |> List.iter (fun (s, m) -> printfn "  unsimulated skill stat  %s (%s)" s m)
                let level = int (get "level" 83.0)
                let profile r =
                    let p = Rarity.defaults r
                    let p = match Map.tryFind "scenario" o, r with
                            | Some "pinnacle", Unique -> { p with ElementalResist = 50.0 } // PoB pinnacle default
                            | _ -> p
                    match Map.tryFind "res" o with
                    | Some v -> { p with ElementalResist = float v }
                    | None -> p
                let distance = get "distance" 30.0
                let enemies =
                    match Map.tryFind "scenario" o |> Option.defaultValue "boss" with
                    | "pack" | "surrounded" as sc ->
                        let counts =
                            (Map.tryFind "pack" o |> Option.defaultValue "8,3,1").Split(',') |> Array.map int |> List.ofArray
                        let n, m, r = match counts with [ a; b; c ] -> a, b, c | _ -> 8, 3, 1
                        Encounter.pack level profile [ Normal, n; Magic, m; Rare, r ] distance 25.0 (sc = "surrounded") 1u
                    | _ -> Encounter.boss level profile distance
                let enemies =
                    match Map.tryFind "life" o with
                    | Some l -> enemies |> List.map (fun e -> { e with Life = float l })
                    | None -> enemies
                printfn "  enemies: %s" (enemies |> List.countBy (fun e -> Rarity.name e.Rarity) |> List.map (fun (r, n) -> sprintf "%d %s" n r) |> String.concat ", ")
                printfn "  delivery: %A" skill.Delivery
                let fight =
                    { Enemies = enemies
                      Player = Actor.make "Player" sheet.Mods [ skill ]
                      Minions = []
                      Targeting = (if Map.tryFind "target" o = Some "nearest" then Nearest else Strongest)
                      MaxTime = get "time" 60.0 }
                printfn ""
                fight |> Summary.simulate (int (get "runs" 500.0)) |> Summary.format |> printfn "%s"
                0
