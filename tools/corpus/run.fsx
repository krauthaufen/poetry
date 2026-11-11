// Run every character in corpus/ through import and a short boss fight; report what the
// simulator doesn't understand, ranked by how many builds it affects.
//   dotnet fsi --optimize+ tools/corpus/run.fsx [corpus dir]
// Writes tools/corpus/report.txt (no character data, only aggregate stat ids and counts).
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

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
      Notes: string list
      /// Character-sheet values against the ones poe.ninja stores: (stat, ours, theirs).
      Sheet: (string * float * float) list
      /// poe.ninja's (PoB's) DPS for the main skill: a lead, never the truth.
      TheirDps: float }

let runOne (path: string) : Result =
    let file = System.IO.Path.GetFileNameWithoutExtension path
    let blank = { File = file; Error = None; Main = ""; Dps = 0.0; Unknown = []; Unmapped = []; Unsimulated = []; MissingGems = []; Notes = []; Sheet = []; TheirDps = 0.0 }
    try
        let text = System.IO.File.ReadAllText path
        let build, _ = Setup.parseBuild text
        // experiment: POETRY_SWAP_SETS=1 swaps the weapon-set passives
        let build =
            match System.Environment.GetEnvironmentVariable "POETRY_SWAP_SETS" with
            | "1" -> { build with TreeNodesSet1 = build.TreeNodesSet2; TreeNodesSet2 = build.TreeNodesSet1 }
            | "none" -> { build with TreeNodesSet1 = []; TreeNodesSet2 = [] }
            | "both" -> { build with TreeNodes = build.TreeNodes @ build.TreeNodesSet1 @ build.TreeNodesSet2; TreeNodesSet1 = []; TreeNodesSet2 = [] }
            | _ -> build
        let ninja = Json.field "ninja" (Json.parse text)
        let theirs = ninja |> Option.bind (Json.field "defensiveStats")
        let num key = theirs |> Option.bind (Json.field key) |> Option.bind Json.num
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
        let sheet =
            let v = c.Vitals
            // uncapped, so a missing source shows as its exact value
            let sheetDb = ModDb.ofList c.Sheet.Mods
            let res t = Mods.sumBase Ctx.empty [ Resist t ] sheetDb + (if DamageType.isElemental t then Constants.endgameResistPenalty else 0.0)
            let num key =
                match key with
                | "fireResistance" | "coldResistance" | "lightningResistance" | "chaosResistance" -> num key |> Option.map (fun r -> r + (num (key + "OverCap") |> Option.defaultValue 0.0))
                | _ -> num key
            [ "life", v.Life, "life"; "mana", v.Mana, "mana"; "energy shield", v.EnergyShield, "energyShield"
              "strength", v.Str, "strength"; "dexterity", v.Dex, "dexterity"; "intelligence", v.Int, "intelligence"
              "armour", v.Armour, "armour"; "evasion", v.Evasion, "evasionRating"; "deflection", v.Deflection, "deflectionRating"
              "block", v.Block, "blockChance"; "ward", v.Ward, "ward"
              "fire res", res Fire, "fireResistance"; "cold res", res Cold, "coldResistance"
              "lightning res", res Lightning, "lightningResistance"; "chaos res", res Chaos, "chaosResistance" ]
            |> List.choose (fun (name, ours, key) -> num key |> Option.map (fun t -> name, ours, t))
            // Chaos Inoculation: immune, not a resistance; Runic Ward is newer than PoB's data
            |> List.filter (fun (name, _, _) -> name <> "ward" && not (name = "chaos res" && v.Life <= 1.0))
            // their block chance is not capped
            |> List.map (fun (name, ours, theirs) -> if name = "block" && theirs > ours && ours >= 50.0 then name, ours, ours else name, ours, theirs)
        let dpsOf (gem: string) =
            ninja |> Option.map (fun n -> Json.list (Json.field "skillDps" n)) |> Option.defaultValue []
            |> List.tryPick (fun j -> if Json.strField "name" j = Some gem then Json.field "dps" j |> Option.bind Json.num |> Option.map (fun d -> d + (Json.field "dotDps" j |> Option.bind Json.num |> Option.defaultValue 0.0)) else None)
            |> Option.defaultValue 0.0
        let blank = { blank with Sheet = sheet }
        match main with
        | None -> { blank with Error = Some "no active skill"; MissingGems = missing }
        | Some (mainGem, _, group) ->
            // a main skill socketed in a meta gem is triggered; press the most supported other skill
            let inMeta = group.Gems |> List.exists (fun x -> not x.IsSupport && Gems.isMeta db x.Name)
            let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
            let pressedGem =
                if inMeta || not (usable mainGem) || (Setup.pressed c o mainGem).IsNone then Setup.strongest c o boss
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
                let sum = Setup.fight c o player boss |> Summary.simulate 3
                let cov = c.Sheet.Coverage
                let sk = skillCov |> Option.defaultValue Coverage.empty
                { blank with
                    Main = (if pg = mainGem then pg elif pg = "" then mainGem + " (minions only)" else sprintf "%s (pressed %s)" mainGem pg)
                    Dps = sum.Dps.Mean
                    TheirDps = dpsOf mainGem
                    Unknown = cov.UnknownText |> List.map (fun l -> let k = l.IndexOf(": ") in if k > 0 then l.Substring(k + 2) else l) |> List.distinct
                    Unmapped = (cov.Unmapped |> List.map fst) @ (sk.Unmapped |> List.map (fun (s, _) -> "skill: " + s)) |> List.distinct
                    Unsimulated = (cov.Unsimulated |> List.map (fun (s, m) -> sprintf "%s [%s]" s m)) @ (sk.Unsimulated |> List.map (fun (s, m) -> sprintf "skill: %s [%s]" s m)) |> List.distinct
                    MissingGems = missing
                    Notes = (failed |> List.map (fun g -> "cannot build skill: " + g)) @ (Gems.unmodelledTypes db mainGem |> List.map (fun t -> "main skill type not modelled: " + t)) @ (c.MinionNotes |> List.map (fun n -> "minion: " + (n.Substring(n.IndexOf(": ") + 2)))) }
    with e ->
        // the innermost exception and where it was thrown
        let rec inner (x: exn) = if isNull x.InnerException then x else inner x.InnerException
        let x = inner e
        let at = (string x.StackTrace).Split('\n') |> Array.tryFind (fun l -> l.Contains "PoEtry.") |> Option.map (fun l -> l.Trim()) |> Option.defaultValue ""
        { blank with Error = Some(sprintf "%s: %s %s" (x.GetType().Name) (x.Message.Split('\n').[0]) at) }

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
// character sheet against poe.ninja's: a value is off when it differs by more than 2% and 2 points
let off (_, ours: float, theirs: float) = abs (ours - theirs) > 2.0 && abs (ours - theirs) > 0.02 * max (abs ours) (abs theirs)
say "\n== character sheet differs from poe.ninja's (builds affected, of those that ran)"
results |> List.collect (fun r -> r.Sheet |> List.filter off |> List.map (fun (n, _, _) -> n)) |> List.countBy id |> List.sortByDescending snd
|> List.iter (fun (s, k) -> say (sprintf "%4d  %s" k s))
say "\n== DPS against poe.ninja's (a lead, not the truth): ours / theirs"
let ratios = results |> List.filter (fun r -> r.Error.IsNone && r.TheirDps > 0.0) |> List.map (fun r -> r.Dps / r.TheirDps)
for lo, hi, label in [ 0.0, 0.01, "under 1%"; 0.01, 0.1, "1% to 10%"; 0.1, 0.5, "10% to 50%"; 0.5, 2.0, "50% to 200%"; 2.0, 10.0, "2x to 10x"; 10.0, infinity, "over 10x" ] do
    say (sprintf "%4d  %s" (ratios |> List.filter (fun x -> x >= lo && x < hi) |> List.length) label)
say "\n== per build"
results |> List.iter (fun r ->
    say (sprintf "%-52s %-40s %s" r.File r.Main (match r.Error with Some e -> "ERROR " + e | None -> sprintf "%12.0f dps (theirs %10.0f)  %d unknown, %d unmapped, %d unsimulated%s" r.Dps r.TheirDps r.Unknown.Length r.Unmapped.Length r.Unsimulated.Length
                                                                                                             (r.Sheet |> List.filter off |> List.map (fun (n, a, b) -> sprintf "  %s %.0f/%.0f" n a b) |> String.concat ""))))
System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/report.txt", out.ToString())
printfn "%s" (out.ToString().Split('\n') |> Array.truncate 60 |> String.concat "\n")
