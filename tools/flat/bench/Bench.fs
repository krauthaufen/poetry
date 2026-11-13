/// Both engines timed on corpus characters, on .NET and (compiled by Fable) in Node: the
/// characters whose fight the fast engine plays, as tools/corpus/run.fsx fights them and
/// with a player that cannot be hurt. Rough: a few fights each. Prints numbers only.
///   dotnet run -c Release --project tools/flat/bench -- corpus/*.json
///   dotnet fable tools/flat/bench -o out/flatbench && node out/flatbench/Bench.js corpus/*.json
module Bench

open PoEtry

let private read (f: string) = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/../../../data/game/" + f)
let private slug (s: string) = s.ToLowerInvariant() |> String.map (fun c -> if System.Char.IsLetterOrDigit c then c else '-')

/// The fight run.fsx gives the character, with these options.
let private fightOf (db: GameDb) (treeJson: string) (path: string) (o: Options) : Fight option =
    let file = (let name = path.Substring(path.LastIndexOf '/' + 1) in name.Substring(0, name.Length - 5))
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

let private ms (f: unit -> unit) =
    let t0 = System.DateTime.UtcNow
    f ()
    (System.DateTime.UtcNow - t0).TotalMilliseconds

[<EntryPoint>]
let main paths =
    let db = GameDb.load read
    let treeJson = read "tree.json"
    for (title, o) in [ "as the corpus fights them", { Options.defaults with MaxTime = 20.0 }; "immortal", { Options.defaults with MaxTime = 20.0; Immortal = true } ] do
        let fights =
            paths |> List.ofArray |> List.choose (fun path ->
                try
                    match fightOf db treeJson path o with
                    | Some fight -> (match Flat.compile fight with Ok _ -> Some fight | Error _ -> None)
                    | None -> None
                with _ -> None)
        let mutable sim = 0.0
        let mutable flat = 0.0
        let mutable compile = 0.0
        let mutable first = 0.0
        let mutable second = 0.0
        let mutable tenth = 0.0
        let mutable events = 0
        for fight in fights do
            // (the fast engine has run before, on another program of the fight, so the runtime has compiled it)
            (match Flat.compile fight with
             | Ok p -> (for seed in 1u .. 300u do Flat.run p seed |> ignore)
             | Error _ -> ())
            for seed in 1u .. 2u do
                Sim.run fight seed |> ignore
            let t0 = System.DateTime.UtcNow
            match Flat.compile fight with
            | Ok p ->
                compile <- compile + (System.DateTime.UtcNow - t0).TotalMilliseconds
                let play = Flat.runner p
                first <- first + ms (fun () -> play 101u |> ignore)
                second <- second + ms (fun () -> play 102u |> ignore)
                for seed in 103u .. 109u do
                    play seed |> ignore
                tenth <- tenth + ms (fun () -> play 110u |> ignore)
                sim <- sim + ms (fun () -> for seed in 1u .. 6u do Sim.run fight seed |> ignore) / 6.0
                flat <- flat + ms (fun () -> for seed in 1u .. 60u do play seed |> ignore) / 60.0
                let mutable n = 0
                Flat.traced (fun _ kind _ _ _ -> if kind <> "hit" && kind <> "crit" then n <- n + 1) p 1u |> ignore
                events <- events + n
            | Error _ -> ()
        let n = float (max 1 fights.Length)
        printfn "%s, %d fights: reference %.2f ms a fight (%.1f us an event), flat %.3f ms (%.2f us an event), %.1f times as fast"
            title fights.Length (sim / n) (sim * 1000.0 / float (max 1 events)) (flat / n) (flat * 1000.0 / float (max 1 events)) (sim / flat)
        printfn "   a compile %.2f ms; a runner's first run %.2f ms, its second %.2f ms, its tenth %.2f ms" (compile / n) (first / n) (second / n) (tenth / n)
    0
