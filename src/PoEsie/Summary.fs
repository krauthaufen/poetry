namespace PoEsie

type Stats =
    { Mean: float
      P10: float
      P50: float
      P90: float }

type Summary =
    { Runs: int
      /// Fraction of runs that killed the enemy within MaxTime.
      KillRate: float
      /// Time to kill over the runs that killed.
      Ttk: Stats option
      /// Effective DPS per run: total damage / fight duration.
      Dps: Stats
      /// Mean share of total damage per source.
      DamageShare: (string * float) list
      /// Mean uptime fraction per enemy status.
      Uptime: (string * float) list
      /// Mean count per run.
      Counts: (string * float) list
      /// Per rarity: time until the last enemy of that rarity died (runs where all did).
      KillTimes: (Rarity * Stats) list }

module Summary =
    let stats (xs: float list) =
        let a = xs |> List.sort |> Array.ofList
        let pick q = a.[min (a.Length - 1) (int (q * float a.Length))]
        { Mean = Array.average a
          P10 = pick 0.1
          P50 = pick 0.5
          P90 = pick 0.9 }

    let private meanOf (keys: (Outcome -> Map<string, float>)) (outcomes: Outcome list) =
        let n = float outcomes.Length
        outcomes
        |> List.collect (keys >> Map.toList)
        |> List.groupBy fst
        |> List.map (fun (k, vs) -> k, (vs |> List.sumBy snd) / n)
        |> List.sortByDescending snd

    let ofOutcomes (outcomes: Outcome list) =
        let kills = outcomes |> List.choose (fun o -> o.Ttk)
        let total (o: Outcome) = o.DamageBy |> Map.fold (fun a _ v -> a + v) 0.0
        { Runs = outcomes.Length
          KillRate = float kills.Length / float outcomes.Length
          Ttk = if kills.IsEmpty then None else Some(stats kills)
          Dps = outcomes |> List.map (fun o -> total o / max 1e-9 o.Duration) |> stats
          DamageShare =
            outcomes
            |> meanOf (fun o -> let t = max 1e-9 (total o) in o.DamageBy |> Map.map (fun _ v -> v / t))
          Uptime = outcomes |> meanOf (fun o -> o.Uptime |> Map.map (fun _ v -> v / max 1e-9 o.Duration))
          Counts = outcomes |> meanOf (fun o -> o.Counts |> Map.map (fun _ v -> float v))
          KillTimes =
            [ Normal; Magic; Rare; Unique ]
            |> List.choose (fun r ->
                let times =
                    outcomes
                    |> List.choose (fun o ->
                        let ks = o.Kills |> List.filter (fun (_, rr, _) -> rr = r)
                        if ks.IsEmpty then None else Some(ks |> List.map (fun (_, _, t) -> t) |> List.max))
                if times.IsEmpty then None else Some(r, stats times)) }

    /// Run the fight `runs` times with consecutive seeds.
    let simulate runs (fight: Fight) =
        Array.init runs (fun i -> uint32 (i + 1))
        |> Array.Parallel.map (Sim.run fight)
        |> List.ofArray
        |> ofOutcomes

    let format (s: Summary) =
        let st (x: Stats) = sprintf "mean %.2f  p10 %.2f  p50 %.2f  p90 %.2f" x.Mean x.P10 x.P50 x.P90
        let lines =
            [ sprintf "runs %d, cleared %.0f%%" s.Runs (s.KillRate * 100.0)
              (match s.Ttk with
               | Some t -> "clear (s) " + st t
               | None -> "clear     never") ]
            @ (s.KillTimes |> List.map (fun (r, t) -> sprintf "  last %-7s dead at %s" (Rarity.name r) (st t)))
            @ [ "DPS      " + st s.Dps; "damage share:" ]
            @ (s.DamageShare |> List.map (fun (k, v) -> sprintf "  %-28s %5.1f%%" k (v * 100.0)))
            @ [ "uptime:" ]
            @ (s.Uptime |> List.map (fun (k, v) -> sprintf "  %-28s %5.1f%%" k (v * 100.0)))
            @ [ "per fight:" ]
            @ (s.Counts |> List.sortBy fst |> List.map (fun (k, v) -> sprintf "  %-28s %8.1f" k v))
        String.concat "\n" lines
