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
                    // a fight that compiles is played by both engines once, and the two compared
                    let same =
                        match reasons, Flat.compile fight with
                        | [], Ok p ->
                            let a, b = Sim.run fight 1u, Flat.run p 1u
                            Some(a.Duration = b.Duration && a.DamageBy = b.DamageBy && a.Counts = b.Counts && a.Uptime = b.Uptime && a.Evened = b.Evened && a.ManaGained = b.ManaGained && a.ManaStarved = b.ManaStarved)
                        | _ -> None
                    Some(reasons, same)
                | None -> None
            with _ -> None)
        |> Array.choose id
    let compiled = results |> Array.filter (fun (r, _) -> r.IsEmpty)
    printfn "\n== %s: %d characters with a fight, %d compile (%d of them the same as the reference with seed 1)" title results.Length compiled.Length (compiled |> Array.filter (fun (_, s) -> s = Some true) |> Array.length)
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
