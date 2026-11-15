// How long each corpus character's fight takes in both engines, as the corpus fights it
// (mortal) or with a player that cannot be hurt: the file's number, the reference's and the
// fast engine's milliseconds a fight (20 seeds after 4 to warm up), and the events of a fight.
//   dotnet fsi --optimize+ tools/flat/times.fsx [corpus dir] [immortal]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let dir = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs.[1] else root + "corpus"
let immortal = fsi.CommandLineArgs |> Array.contains "immortal"
let db = GameDb.load read
let treeJson = read "tree.json"
let slug (s: string) = s.ToLowerInvariant() |> String.map (fun c -> if System.Char.IsLetterOrDigit c then c else '-')

/// The fight run.fsx gives the character, with these options.
let fightOf (path: string) (o: Options) : Fight option =
    let file = System.IO.Path.GetFileNameWithoutExtension path
    let build, _ = Setup.parseBuild (System.IO.File.ReadAllText path)
    let c = Setup.loadWith db treeJson build o
    let actives =
        c.Build.Groups |> List.collect (fun g ->
            g.Gems |> List.filter (fun x -> not x.IsSupport && x.Enabled)
            |> List.map (fun x -> x.Name, (g.Gems |> List.filter (fun y -> y.IsSupport)).Length, g))
    let wanted = file.Substring(file.IndexOf('-') + 1)
    let usable (n: string) = not (Gems.isMeta db n) && not (Gems.isPersistentBuff db n)
    let main =
        actives |> List.filter (fun (n, _, _) -> wanted.EndsWith(slug n)) |> List.sortByDescending (fun (n, _, _) -> n.Length) |> List.tryHead
        |> Option.orElse (actives |> List.filter (fun (n, _, _) -> usable n) |> List.sortByDescending (fun (_, k, _) -> k) |> List.tryHead)
    match main with
    | None -> None
    | Some (mainGem, _, group) ->
        let inMeta = group.Gems |> List.exists (fun x -> not x.IsSupport && Gems.isMeta db x.Name)
        let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
        let pressedGem =
            if inMeta || not (usable mainGem) || (Setup.pressed c o mainGem).IsNone then Setup.strongest c o boss
            else Some mainGem
        let pg = pressedGem |> Option.defaultValue ""
        let play = if pg = "" then [] else Setup.auto c o pg
        Some(Setup.fight c o (Setup.player c o play None) boss)

let files = System.IO.Directory.GetFiles(dir, "*.json") |> Array.filter (fun f -> System.Char.IsDigit (System.IO.Path.GetFileName f).[0]) |> Array.sort
let o = { Options.defaults with MaxTime = 20.0; Immortal = immortal }
let mutable simAll, flatAll = 0.0, 0.0
for f in files do
    try
        match fightOf f o with
        | Some fight ->
            match Flat.compile fight with
            | Ok p ->
                for seed in 100u .. 103u do
                    Flat.run p seed |> ignore
                    Sim.run fight seed |> ignore
                let sw = System.Diagnostics.Stopwatch.StartNew()
                for seed in 1u .. 20u do Flat.run p seed |> ignore
                let flat = sw.Elapsed.TotalMilliseconds / 20.0
                sw.Restart()
                for seed in 1u .. 20u do Sim.run fight seed |> ignore
                let sim = sw.Elapsed.TotalMilliseconds / 20.0
                let events = ref 0
                Flat.traced (fun _ k _ _ _ -> if k <> "hit" && k <> "crit" then events.Value <- events.Value + 1) p 1u |> ignore
                simAll <- simAll + sim
                flatAll <- flatAll + flat
                printfn "%s %.2f %.3f %d" ((System.IO.Path.GetFileNameWithoutExtension f).Substring(0, 3)) sim flat events.Value
            | _ -> ()
        | None -> ()
    with _ -> ()
printfn "all %.1f %.1f" simAll flatAll
