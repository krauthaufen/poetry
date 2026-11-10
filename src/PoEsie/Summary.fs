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
      KillTimes: (Rarity * Stats) list
      /// Fraction of runs in which the player died.
      DeathRate: float
      /// Damage taken per second.
      DamageTakenPerSecond: float
      /// Lowest life fraction reached (mean, worst).
      MinLife: float * float
      /// Fraction of the fight spent waiting for mana.
      Starved: float
      /// Mean mana per second by source.
      ManaGained: (string * float) list }

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
          DeathRate = float (outcomes |> List.filter (fun o -> o.Died.IsSome) |> List.length) / float outcomes.Length
          DamageTakenPerSecond = outcomes |> List.averageBy (fun o -> o.DamageTaken / max 1e-9 o.Duration)
          MinLife = (outcomes |> List.averageBy (fun o -> o.MinLife)), (outcomes |> List.map (fun o -> o.MinLife) |> List.min)
          Starved = outcomes |> List.averageBy (fun o -> o.ManaStarved / max 1e-9 o.Duration)
          ManaGained = outcomes |> meanOf (fun o -> o.ManaGained |> Map.map (fun _ v -> v / max 1e-9 o.Duration))
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
            @ [ "DPS      " + st s.Dps
                sprintf "player   died in %.0f%% of runs, took %.0f damage/s, lowest life %.0f%% (worst %.0f%%)" (s.DeathRate * 100.0) s.DamageTakenPerSecond (fst s.MinLife * 100.0) (snd s.MinLife * 100.0)
                sprintf "mana     waited for mana %.1f%% of the time; gained per second: %s" (s.Starved * 100.0) (s.ManaGained |> List.map (fun (k, v) -> sprintf "%s %.0f" k v) |> String.concat ", ")
                "damage share:" ]
            @ (s.DamageShare |> List.map (fun (k, v) -> sprintf "  %-28s %5.1f%%" k (v * 100.0)))
            @ [ "uptime:" ]
            @ (s.Uptime |> List.map (fun (k, v) -> sprintf "  %-28s %5.1f%%" k (v * 100.0)))
            @ [ "per fight:" ]
            @ (s.Counts |> List.sortBy fst |> List.map (fun (k, v) -> sprintf "  %-28s %8.1f" k v))
        String.concat "\n" lines

/// Standard fights to compare builds on.
module Scenarios =
    /// (name, enemies). Packs are placed with a fixed seed so every build faces the same layout.
    let standard level (profile: Rarity -> RarityProfile) =
        [ "pack in front", Encounter.pack level profile [ Normal, 8; Magic, 3 ] 35.0 25.0 false 1u
          "pack in front + 2 rares", Encounter.pack level profile [ Normal, 8; Magic, 3; Rare, 2 ] 35.0 25.0 false 1u
          "surrounded", Encounter.pack level profile [ Normal, 8; Magic, 3 ] 25.0 10.0 true 1u
          "single boss", Encounter.boss level profile 30.0 ]

    /// Run each scenario `runs` times with the given player / minions.
    let run runs (maxTime: float) (player: Actor) (minions: Actor list) targeting scenarios =
        scenarios
        |> List.map (fun (name, enemies) ->
            name,
            Summary.simulate
                runs
                { Enemies = enemies
                  Player = player
                  Minions = minions
                  Targeting = targeting
                  Reach = 15.0
                  CascadeSpacing = 1.0
                  Walls = []
                  MaxTime = maxTime })

    let format (results: (string * Summary) list) =
        let header = sprintf "%-26s %8s %9s %17s %10s" "scenario" "cleared" "clear p50" "clear p10-p90" "DPS"
        let row (name, s: Summary) =
            match s.Ttk with
            | Some t -> sprintf "%-26s %7.0f%% %8.2fs %17s %10.0f" name (s.KillRate * 100.0) t.P50 (sprintf "%.1f-%.1fs" t.P10 t.P90) s.Dps.Mean
            | None -> sprintf "%-26s %7.0f%% %9s %17s %10.0f" name (s.KillRate * 100.0) "-" "-" s.Dps.Mean
        String.concat "\n" (header :: List.map row results)
