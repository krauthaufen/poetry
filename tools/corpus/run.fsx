// Run every character in corpus/ through import and a short boss fight; report what the
// simulator doesn't understand, ranked by how many builds it affects.
//   dotnet fsi --optimize+ tools/corpus/run.fsx [corpus dir]
// Writes tools/corpus/report.txt (no character data, only aggregate stat ids and counts).
#r "../../src/PoEsie/bin/Release/net8.0/PoEsie.dll"
open PoEsie

let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let dir = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs.[1] else root + "corpus"
let db = GameDb.load read
let treeJson = read "tree.json"
let slug (s: string) = s.ToLowerInvariant() |> String.map (fun c -> if System.Char.IsLetterOrDigit c then c else '-')

type Result =
    { File: string
      Error: string option
      Main: string
      Dps: float
      Unknown: string list
      Unmapped: string list
      Unsimulated: string list
      MissingGems: string list
      Notes: string list }

let runOne (path: string) : Result =
    let file = System.IO.Path.GetFileNameWithoutExtension path
    let blank = { File = file; Error = None; Main = ""; Dps = 0.0; Unknown = []; Unmapped = []; Unsimulated = []; MissingGems = []; Notes = [] }
    try
        let build, _ = Setup.parseBuild (System.IO.File.ReadAllText path)
        let o = { Options.defaults with MaxTime = 20.0 }
        let c = Setup.loadWith db treeJson build o
        let actives =
            c.Build.Groups |> List.collect (fun g ->
                g.Gems |> List.filter (fun x -> not x.IsSupport && x.Enabled)
                |> List.map (fun x -> x.Name, (g.Gems |> List.filter (fun y -> y.IsSupport)).Length, g))
        let missing =
            c.Build.Groups |> List.collect (fun g -> g.Gems) |> List.map (fun x -> x.Name)
            |> List.filter (fun n -> (Gems.data db n).IsNone) |> List.distinct
        // main skill: the one the file is named after, else the most supported damaging one
        let wanted = file.Substring(file.IndexOf('-') + 1)
        let usable (n: string) = not (Gems.isMeta db n) && not (Gems.isPersistentBuff db n)
        let main =
            actives |> List.filter (fun (n, _, _) -> wanted.EndsWith(slug n)) |> List.sortByDescending (fun (n, _, _) -> n.Length) |> List.tryHead
            |> Option.orElse (actives |> List.filter (fun (n, _, _) -> usable n) |> List.sortByDescending (fun (_, k, _) -> k) |> List.tryHead)
        match main with
        | None -> { blank with Error = Some "no active skill"; MissingGems = missing }
        | Some (mainGem, _, group) ->
            // a main skill socketed in a meta gem is triggered; press the most supported other skill
            let inMeta = group.Gems |> List.exists (fun x -> not x.IsSupport && Gems.isMeta db x.Name)
            let pressedGem =
                if inMeta || not (usable mainGem) then
                    actives |> List.filter (fun (n, _, g) -> usable n && not (g.Gems |> List.exists (fun x -> not x.IsSupport && Gems.isMeta db x.Name)))
                    |> List.sortByDescending (fun (_, k, _) -> k) |> List.tryHead |> Option.map (fun (n, _, _) -> n)
                else Some mainGem
            // nothing to press: the minions fight alone
            match pressedGem |> Option.orElse (Some "") with
            | None -> blank
            | Some pg ->
                let play = if pg = "" then [] else Setup.auto c o pg
                let failed = play |> List.collect (function Spam g | OnCooldown g | KeepBuff g -> [ g ] | _ -> []) |> List.filter (fun g -> (Setup.pressed c o g).IsNone)
                let skillCov =
                    match Setup.pressed c o pg with
                    | Some _ ->
                        c.Build.Groups |> List.tryFind (fun g -> g.Gems |> List.exists (fun x -> x.Name = pg && not x.IsSupport))
                        |> Option.bind (fun g -> Pob.groupSkill db c.Sheet g |> List.tryHead)
                        |> Option.bind (function Ok (_, cov) -> Some cov | Error _ -> None)
                    | None -> None
                let player = Setup.player c o play None
                let sum = Setup.fight c o player (Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })) |> Summary.simulate 3
                let cov = c.Sheet.Coverage
                let sk = skillCov |> Option.defaultValue Coverage.empty
                { blank with
                    Main = (if pg = mainGem then pg elif pg = "" then mainGem + " (minions only)" else sprintf "%s (pressed %s)" mainGem pg)
                    Dps = sum.Dps.Mean
                    Unknown = cov.UnknownText |> List.map (fun l -> let k = l.IndexOf(": ") in if k > 0 then l.Substring(k + 2) else l) |> List.distinct
                    Unmapped = (cov.Unmapped |> List.map fst) @ (sk.Unmapped |> List.map (fun (s, _) -> "skill: " + s)) |> List.distinct
                    Unsimulated = (cov.Unsimulated |> List.map (fun (s, m) -> sprintf "%s [%s]" s m)) @ (sk.Unsimulated |> List.map (fun (s, m) -> sprintf "skill: %s [%s]" s m)) |> List.distinct
                    MissingGems = missing
                    Notes = (failed |> List.map (fun g -> "cannot build skill: " + g)) @ (c.MinionNotes |> List.map (fun n -> "minion: " + (n.Substring(n.IndexOf(": ") + 2)))) }
    with e -> { blank with Error = Some(e.GetType().Name + ": " + e.Message.Split('\n').[0]) }

let files = System.IO.Directory.GetFiles(dir, "*.json") |> Array.filter (fun f -> System.Char.IsDigit (System.IO.Path.GetFileName f).[0]) |> Array.sort
let results =
    files
    |> Array.Parallel.map (fun f ->
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let r = runOne f
        eprintfn "%5.1fs %s" sw.Elapsed.TotalSeconds r.File
        r)
    |> List.ofArray
let out = System.Text.StringBuilder()
let say (s: string) = out.AppendLine s |> ignore
let n = results.Length
let errors = results |> List.filter (fun r -> r.Error.IsSome)
say (sprintf "%d builds: %d ran, %d failed; %d with zero damage" n (n - errors.Length) errors.Length (results |> List.filter (fun r -> r.Error.IsNone && r.Dps <= 0.0) |> List.length))
say "\n== failures"
errors |> List.countBy (fun r -> r.Error.Value) |> List.sortByDescending snd |> List.iter (fun (e, k) -> say (sprintf "%4d  %s" k e))
say "\n== zero damage (main skill)"
results |> List.filter (fun r -> r.Error.IsNone && r.Dps <= 0.0) |> List.countBy (fun r -> r.Main) |> List.sortByDescending snd |> List.iter (fun (m, k) -> say (sprintf "%4d  %s" k m))
let ranked title (pick: Result -> string list) top =
    say (sprintf "\n== %s (builds affected)" title)
    results |> List.collect (pick >> List.distinct) |> List.countBy id |> List.sortByDescending snd |> List.truncate top
    |> List.iter (fun (s, k) -> say (sprintf "%4d  %s" k s))
ranked "gems not in the data" (fun r -> r.MissingGems) 100
ranked "skills that could not be built" (fun r -> r.Notes) 100
ranked "text lines not understood" (fun r -> r.Unknown) 150
ranked "unmapped stats" (fun r -> r.Unmapped) 200
ranked "unsimulated stats" (fun r -> r.Unsimulated) 200
say "\n== per build"
results |> List.iter (fun r ->
    say (sprintf "%-52s %-40s %s" r.File r.Main (match r.Error with Some e -> "ERROR " + e | None -> sprintf "%12.0f dps  %d unknown, %d unmapped, %d unsimulated" r.Dps r.Unknown.Length r.Unmapped.Length r.Unsimulated.Length)))
System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/report.txt", out.ToString())
printfn "%s" (out.ToString().Split('\n') |> Array.truncate 60 |> String.concat "\n")
