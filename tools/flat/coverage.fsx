// How many of the corpus's fights the fast engine (Flat) plays, and what keeps it from the
// others, ranked by how many characters each thing stops. The characters are loaded and
// fought as tools/corpus/run.fsx does. Prints counts only, nothing of a character.
//   dotnet fsi --optimize+ tools/flat/coverage.fsx [corpus dir]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let dir = if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs.[1] else root + "corpus"
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

let report (title: string) (o: Options) =
    let results =
        files |> Array.Parallel.map (fun f ->
            try
                match fightOf f o with
                | Some fight ->
                    let reasons = Flat.unsupported fight
                    let same = match reasons, Flat.compile fight with [], Ok p -> Some(fight, p) | _ -> None
                    Some(reasons, same)
                | None -> None
            with _ -> None)
        |> Array.choose id
    let compiled = results |> Array.filter (fun (r, _) -> r.IsEmpty)
    // a fight that compiles is played by both engines with a few seeds, the outcomes compared,
    // and both timed, one after the other, after a few fights to warm up (rough all the same)
    let same (a: Outcome) (b: Outcome) =
        a.Duration = b.Duration && a.Ttk = b.Ttk && a.Kills = b.Kills && a.DamageBy = b.DamageBy && a.DamageByType = b.DamageByType && a.Counts = b.Counts
        && a.Uptime = b.Uptime && a.Died = b.Died && a.DamageTaken = b.DamageTaken && a.Evened = b.Evened && a.MinLife = b.MinLife
        && a.ManaGained = b.ManaGained && a.ManaStarved = b.ManaStarved
    let seeds = [ 1u .. 40u ]
    let timed =
        compiled |> Array.choose snd |> Array.map (fun (fight, p) ->
            for seed in 100u .. 103u do
                Sim.run fight seed |> ignore
                Flat.run p seed |> ignore
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let a = seeds |> List.map (Sim.run fight)
            let simMs = sw.Elapsed.TotalMilliseconds / float seeds.Length
            sw.Restart()
            let b = seeds |> List.map (Flat.run p)
            let flatMs = sw.Elapsed.TotalMilliseconds / float seeds.Length
            let events = ref 0
            Flat.traced (fun _ k _ _ _ -> if k <> "hit" && k <> "crit" then events.Value <- events.Value + 1) p 1u |> ignore
            List.forall2 same a b, simMs, flatMs, events.Value)
    printfn "\n== %s: %d characters with a fight, %d compile (%d of them the same as the reference with seeds 1 to 40)" title results.Length compiled.Length (timed |> Array.filter (fun (ok, _, _, _) -> ok) |> Array.length)
    for (_, simMs, flatMs, events) in timed do
        printfn "   a fight of %d events: reference %.2f ms, flat %.2f ms (%.1f times as fast)" events simMs flatMs (simMs / flatMs)
    if timed.Length > 0 then
        let total f = timed |> Array.sumBy f
        printfn "   all of them: reference %.2f ms a fight, flat %.2f ms a fight (%.1f times as fast; rough)" (total (fun (_, a, _, _) -> a) / float timed.Length) (total (fun (_, _, b, _) -> b) / float timed.Length) (total (fun (_, a, _, _) -> a) / total (fun (_, _, b, _) -> b))
    printfn "-- what stops the others (characters stopped by it; in brackets: by it alone)"
    results |> Array.collect (fun (r, _) -> Array.ofList r) |> Array.countBy id |> Array.sortByDescending snd
    |> Array.iter (fun (reason, n) ->
        let alone = results |> Array.filter (fun (r, _) -> r = [ reason ]) |> Array.length
        printfn "%4d [%3d]  %s" n alone reason)
    // which to build first: always the thing that, once built, lets the most characters through
    // (of several that let none through yet, the one that stops the most)
    printfn "-- built in this order, so many characters compile"
    let rec order (left: string list[]) (built: int) =
        let open' = left |> Array.filter (fun r -> not r.IsEmpty)
        if open'.Length > 0 then
            let reasons = open' |> Array.collect Array.ofList |> Array.distinct
            let best =
                reasons |> Array.maxBy (fun reason ->
                    open' |> Array.filter (fun r -> r = [ reason ]) |> Array.length, open' |> Array.filter (List.contains reason) |> Array.length)
            let left = left |> Array.map (List.filter (fun r -> r <> best))
            let through = left |> Array.filter (fun r -> r.IsEmpty) |> Array.length
            if built < 25 then printfn "%4d  + %s" through best
            order left (built + 1)
    order (results |> Array.map fst) 0
    printfn "-- how many things stop a character"
    results |> Array.countBy (fun (r, _) -> r.Length) |> Array.sortBy fst |> Array.iter (fun (k, n) -> printfn "%4d characters: %d" n k)

let o = { Options.defaults with MaxTime = 20.0 }
report "as the corpus fights them" o
report "with a player that cannot be hurt (Immortal)" { o with Immortal = true }
