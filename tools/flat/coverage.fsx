// How many of the corpus's fights the fast engine (Flat) plays, and what keeps it from the
// others, ranked by how many characters each thing stops. The characters are loaded and
// fought as tools/corpus/run.fsx does. Prints counts, and of a character whose fight the two
// engines play differently its file's name, the first seed and what differs; nothing else.
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
                    let same = match reasons, Flat.compile fight with [], Ok p -> Some(System.IO.Path.GetFileNameWithoutExtension f, fight, p) | _ -> None
                    Some(reasons, same)
                | None -> None
            with _ -> None)
        |> Array.choose id
    let compiled = results |> Array.filter (fun (r, _) -> r.IsEmpty)
    // a fight that compiles is played by both engines with a few seeds, the outcomes compared,
    // and both timed, one after the other, after a few fights to warm up (rough all the same)
    // (numbers the same within 1e-9: the fast engine may add in another order; counts identical)
    let near (x: float) (y: float) = x = y || abs (x - y) <= 1e-9 || abs (x - y) <= 1e-9 * max (abs x) (abs y)
    let nearMap (a: Map<string, float>) (b: Map<string, float>) = a.Count = b.Count && a |> Map.forall (fun k v -> match Map.tryFind k b with Some w -> near v w | None -> false)
    let same (a: Outcome) (b: Outcome) =
        near a.Duration b.Duration && a.Ttk.IsSome = b.Ttk.IsSome && a.Kills.Length = b.Kills.Length && nearMap a.DamageBy b.DamageBy && nearMap a.DamageByType b.DamageByType && a.Counts = b.Counts
        && nearMap a.Uptime b.Uptime && a.Died.IsSome = b.Died.IsSome && near a.DamageTaken b.DamageTaken && near a.Evened b.Evened && near a.Overkill b.Overkill && near a.MinLife b.MinLife
        && nearMap a.ManaGained b.ManaGained && near a.ManaStarved b.ManaStarved
    /// What of two outcomes is not the same (the first thing found).
    let differing (a: Outcome) (b: Outcome) : string =
        let maps (what: string) (x: Map<string, float>) (y: Map<string, float>) =
            if nearMap x y then "" else
                let keys = (x |> Map.toList |> List.map fst) @ (y |> Map.toList |> List.map fst) |> List.distinct
                let key = keys |> List.find (fun k -> match Map.tryFind k x, Map.tryFind k y with Some v, Some w -> not (near v w) | _ -> true)
                sprintf "%s[%s]: reference %A, flat %A" what key (Map.tryFind key x) (Map.tryFind key y)
        [ (if near a.Duration b.Duration then "" else sprintf "Duration: reference %.17g, flat %.17g" a.Duration b.Duration)
          (if a.Ttk.IsSome = b.Ttk.IsSome then "" else "Ttk")
          (if a.Kills.Length = b.Kills.Length then "" else sprintf "Kills: reference %d, flat %d" a.Kills.Length b.Kills.Length)
          maps "DamageBy" a.DamageBy b.DamageBy
          maps "DamageByType" a.DamageByType b.DamageByType
          (if a.Counts = b.Counts then "" else
               let keys = (a.Counts |> Map.toList |> List.map fst) @ (b.Counts |> Map.toList |> List.map fst) |> List.distinct
               let key = keys |> List.find (fun k -> Map.tryFind k a.Counts <> Map.tryFind k b.Counts)
               sprintf "Counts[%s]: reference %A, flat %A" key (Map.tryFind key a.Counts) (Map.tryFind key b.Counts))
          maps "Uptime" a.Uptime b.Uptime
          (if a.Died.IsSome = b.Died.IsSome then "" else sprintf "Died: reference %A, flat %A" a.Died b.Died)
          (if near a.DamageTaken b.DamageTaken then "" else sprintf "DamageTaken: reference %.17g, flat %.17g" a.DamageTaken b.DamageTaken)
          (if near a.Evened b.Evened then "" else sprintf "Evened: reference %.17g, flat %.17g" a.Evened b.Evened)
          (if near a.Overkill b.Overkill then "" else sprintf "Overkill: reference %.17g, flat %.17g" a.Overkill b.Overkill)
          (if near a.MinLife b.MinLife then "" else sprintf "MinLife: reference %.17g, flat %.17g" a.MinLife b.MinLife)
          maps "ManaGained" a.ManaGained b.ManaGained
          (if near a.ManaStarved b.ManaStarved then "" else sprintf "ManaStarved: reference %.17g, flat %.17g" a.ManaStarved b.ManaStarved) ]
        |> List.tryFind (fun d -> d <> "") |> Option.defaultValue "?"
    let seeds = [ 1u .. 40u ]
    let timed =
        compiled |> Array.choose snd |> Array.map (fun (file, fight, p) ->
            for seed in 100u .. 103u do
                Sim.run fight seed |> ignore
                Flat.run p seed |> ignore
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let a = seeds |> List.map (Sim.run fight)
            let simMs = sw.Elapsed.TotalMilliseconds / float seeds.Length
            sw.Restart()
            let b = seeds |> List.map (Flat.run p)
            let flatMs = sw.Elapsed.TotalMilliseconds / float seeds.Length
            // and once more with both engines warm (the runtime compiles code better the more it has run)
            for seed in 200u .. 500u do
                Flat.run p seed |> ignore
            sw.Restart()
            for seed in 1u .. 100u do
                Flat.run p seed |> ignore
            let flatWarm = sw.Elapsed.TotalMilliseconds / 100.0
            sw.Restart()
            for seed in 1u .. 20u do
                Sim.run fight seed |> ignore
            let simWarm = sw.Elapsed.TotalMilliseconds / 20.0
            let events = ref 0
            Flat.traced (fun _ k _ _ _ -> if k <> "hit" && k <> "crit" then events.Value <- events.Value + 1) p 1u |> ignore
            (match List.zip3 seeds a b |> List.tryFind (fun (_, x, y) -> not (same x y)) with
             | Some (seed, x, y) -> printfn "   DIFFERS %s, seed %d: %s" file seed (differing x y)
             | None -> ())
            List.forall2 same a b, simMs, flatMs, events.Value, simWarm, flatWarm)
    printfn "\n== %s: %d characters with a fight, %d compile (%d of them the same as the reference within 1e-9 with seeds 1 to 40)" title results.Length compiled.Length (timed |> Array.filter (fun (ok, _, _, _, _, _) -> ok) |> Array.length)
    for (_, simMs, flatMs, events, simWarm, flatWarm) in timed do
        printfn "   a fight of %d events: reference %.2f ms, flat %.3f ms (%.1f times as fast); warm: %.2f ms, %.3f ms (%.1f times)" events simMs flatMs (simMs / flatMs) simWarm flatWarm (simWarm / flatWarm)
    if timed.Length > 0 then
        let total f = timed |> Array.sumBy f
        let events = total (fun (_, _, _, n, _, _) -> float n)
        let line (what: string) (a: float) (b: float) =
            printfn "   all of them, %s: reference %.2f ms a fight (%.1f us an event), flat %.3f ms a fight (%.2f us an event): %.1f times as fast (rough)" what (a / float timed.Length) (a * 1000.0 / events) (b / float timed.Length) (b * 1000.0 / events) (a / b)
        line "as before (40 fights after 4)" (total (fun (_, a, _, _, _, _) -> a)) (total (fun (_, _, b, _, _, _) -> b))
        line "warm" (total (fun (_, _, _, _, a, _) -> a)) (total (fun (_, _, _, _, _, b) -> b))
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
