namespace PoEtry

/// What a front end asks the simulator and gets back, as JSON: a character's sheet, and
/// fights. Pure, so it is the same on .NET and in a browser worker; the game data and the
/// loaded character are kept by whoever calls (see `Session`).
module Report =
    let private esc (s: string) =
        let sb = System.Text.StringBuilder()
        for c in s do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | '\n' -> sb.Append "\\n" |> ignore
            | '\r' -> sb.Append "\\r" |> ignore
            | '\t' -> sb.Append "\\t" |> ignore
            | c when c < ' ' -> sb.Append ' ' |> ignore
            | c -> sb.Append c |> ignore
        sb.ToString()

    /// JSON text of a value. Numbers that are not finite are written as null.
    let rec write (j: Json) : string =
        match j with
        | JNull -> "null"
        | JBool b -> if b then "true" else "false"
        | JNum n when System.Double.IsNaN n || System.Double.IsInfinity n -> "null"
        | JNum n when n = floor n && abs n < 1e15 -> sprintf "%.0f" n
        | JNum n -> sprintf "%.6g" n
        | JStr s -> "\"" + esc s + "\""
        | JArr xs -> "[" + (xs |> List.map write |> String.concat ",") + "]"
        | JObj m -> "{" + (m |> Map.toList |> List.map (fun (k, v) -> "\"" + esc k + "\":" + write v) |> String.concat ",") + "}"

    let private obj (fields: (string * Json) list) = JObj(Map.ofList fields)
    let private num (v: float) = JNum v
    let private strs (xs: string list) = JArr(xs |> List.map JStr)
    let private stats (s: Stats) = obj [ "mean", num s.Mean; "p10", num s.P10; "p50", num s.P50; "p90", num s.P90 ]
    let private pairs (xs: (string * float) list) = JArr(xs |> List.map (fun (k, v) -> obj [ "name", JStr k; "value", num v ]))

    /// The character sheet: who it is, pools, defences, resistances, what can be pressed, the
    /// socket groups, the minions, and what was not understood.
    /// (`slots`: the tree's jewel sockets in the order a character's jewels are numbered.)
    let character (slots: int list) (c: Loaded) : Json =
        // a jewel in a socket of the tree: its item text is rarity, name, base, properties, mods
        let jewel (slot: string, text: string) =
            if not (slot.StartsWith "Jewel ") then None
            else
                match System.Int32.TryParse(slot.Substring 6) with
                | true, n ->
                    (if n < 1000 then List.tryItem n slots else Some n)
                    |> Option.map (fun node ->
                        let lines = text.Split('\n') |> List.ofArray
                        let parts = lines |> List.fold (fun acc l -> if l = "--------" then [] :: acc else (match acc with h :: r -> (h @ [ l ]) :: r | [] -> [ [ l ] ])) [ [] ] |> List.rev
                        let head = parts |> List.tryHead |> Option.defaultValue []
                        let rarity = head |> List.tryHead |> Option.map (fun (l: string) -> l.Replace("Rarity: ", "")) |> Option.defaultValue ""
                        let rest = parts |> List.skip (min 1 parts.Length) |> List.concat |> List.filter (fun l -> l <> "")
                        // its radius in tree units (PoB data.jewelRadii x PassiveTreeJewelDistanceMultiplier):
                        // a disc by its "Radius:" size, or the ring "Only affects Passives in ... Ring" names
                        let scale = Map.find "PassiveTreeJewelDistanceMultiplier" Tables.gameConstants
                        let ring =
                            rest |> List.tryPick (fun l ->
                                match l.Trim() with
                                | "Only affects Passives in Very Small Ring" -> Some(650.0, 950.0)
                                | "Only affects Passives in Small Ring" -> Some(800.0, 1100.0)
                                | "Only affects Passives in Medium-Small Ring" -> Some(950.0, 1250.0)
                                | "Only affects Passives in Medium Ring" -> Some(1100.0, 1400.0)
                                | "Only affects Passives in Medium-Large Ring" -> Some(1250.0, 1550.0)
                                | "Only affects Passives in Large Ring" -> Some(1400.0, 1700.0)
                                | "Only affects Passives in Very Large Ring" -> Some(1650.0, 1950.0)
                                | _ -> None)
                        let disc = rest |> List.tryPick (fun l -> if l.StartsWith "Radius:" then Tree.jewelRadius (l.Substring 7) else None)
                        let inner, outer =
                            match ring, disc with
                            | Some(i, o), _ -> i * scale, o * scale
                            | None, Some o -> 0.0, o
                            | None, None -> 0.0, 0.0
                        // "Passives in Radius [of <Keystone>] can be Allocated without being connected to your tree"
                        let leap = rest |> List.exists (fun l -> l.Contains "can be Allocated")
                        let centres =
                            rest |> List.choose (fun l ->
                                let a = l.IndexOf "Passives in Radius of "
                                let b = l.IndexOf " can be Allocated"
                                if a >= 0 && b > a then Some(JStr(l.Substring(a + 22, b - a - 22).Trim())) else None)
                        obj [ "node", num (float node); "rarity", JStr rarity
                              "inner", num inner; "outer", num outer; "leap", JBool leap; "centres", JArr centres
                              "name", JStr (head |> List.tryItem 1 |> Option.defaultValue "")
                              "base", JStr (head |> List.tryItem 2 |> Option.defaultValue "")
                              "mods", JArr(rest |> List.map JStr) ])
                | _ -> None
        let v = c.Vitals
        let cov = c.Sheet.Coverage
        obj
            [ "class", JStr c.Sheet.ClassName
              "ascendancy", JStr c.Build.Ascendancy
              "level", num (float c.Sheet.Level)
              // the passives allocated (those bound to a weapon set apart)
              "passives", JArr(c.Build.TreeNodes |> List.map (fun i -> num (float i)))
              // what an attribute passive was chosen to be
              "choices", JArr(c.Build.TreeOverrides |> Map.toList |> List.choose (fun (i, stats) ->
                  [ "Strength"; "Dexterity"; "Intelligence" ]
                  |> List.tryFind (fun a -> stats |> List.exists (fun (s: string) -> s.EndsWith("to " + a)))
                  |> Option.map (fun a -> obj [ "id", num (float i); "name", JStr a ])))
              "jewels", JArr(c.Build.Items |> List.choose jewel)
              "passivesSet1", JArr(c.Build.TreeNodesSet1 |> List.map (fun i -> num (float i)))
              "passivesSet2", JArr(c.Build.TreeNodesSet2 |> List.map (fun i -> num (float i)))
              "attributes", obj [ "str", num v.Str; "dex", num v.Dex; "int", num v.Int ]
              "pools", obj [ "life", num v.Life; "mana", num v.Mana; "energyShield", num v.EnergyShield; "ward", num v.Ward; "spirit", num c.Spirit ]
              "defences",
              obj [ "armour", num v.Armour; "evasion", num v.Evasion; "deflection", num v.Deflection; "block", num v.Block
                    "stunThreshold", num v.StunThreshold; "ailmentThreshold", num v.AilmentThreshold ]
              "resistances", JArr [ for t in DamageType.all do if t <> Physical then yield obj [ "name", JStr(DamageType.name t); "value", num (Map.tryFind t v.Resist |> Option.defaultValue 0.0) ] ]
              "recovery", obj [ "lifeRegen", num v.LifeRegen; "manaRegen", num v.ManaRegen; "energyShieldRecharge", num v.EnergyShieldRecharge ]
              "pressable", strs (Setup.pressable c)
              "groups",
              JArr
                  [ for g in c.Build.Groups do
                      if g.Enabled then
                          yield obj [ "slot", JStr g.Slot
                                      "gems", JArr [ for x in g.Gems do if x.Enabled then yield obj [ "name", JStr x.Name; "level", num (float x.Level); "quality", num (float x.Quality); "support", JBool x.IsSupport ] ] ] ]
              "minions",
              JArr
                  [ for (name, n) in c.Minions |> List.countBy (fun m -> m.Name.TrimEnd([| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]).TrimEnd()) do
                      yield obj [ "name", JStr name; "count", num (float n) ] ]
              "notUnderstood",
              obj [ "unmapped", strs (cov.Unmapped |> List.map fst |> List.distinct)
                    "unsimulated", strs (cov.Unsimulated |> List.map fst |> List.distinct)
                    "unknown", strs cov.UnknownText
                    "minions", strs c.MinionNotes ] ]

    /// A fight as asked for: { scenario: "boss" | "pinnacle" | "pack" | "surrounded", level,
    /// time, runs, life (0: the enemies' own), gem ("": the strongest to press) }.
    /// Missing fields take the defaults (a level 83 boss, 30 s, 20 runs).
    let fight (c: Loaded) (request: Json) : Json =
        let n key d = Json.numField key request |> Option.defaultValue d
        let s key d = Json.strField key request |> Option.defaultValue d
        let level, runs = int (n "level" 83.0), max 1 (int (n "runs" 20.0))
        let o = { Options.defaults with MaxTime = n "time" 30.0 }
        let scenario = s "scenario" "boss"
        let profile r =
            let p = Rarity.defaults r
            if scenario = "pinnacle" && r = Unique then { p with ElementalResist = 50.0 } else p
        let enemies =
            match scenario with
            | "pack" | "surrounded" -> Encounter.pack level profile [ Normal, 8; Magic, 3; Rare, 1 ] 30.0 25.0 (scenario = "surrounded") 1u
            | _ -> Encounter.boss level profile 30.0
        let enemies = match n "life" 0.0 with l when l > 0.0 -> enemies |> List.map (fun e -> { e with Life = l }) | _ -> enemies
        let gem =
            match s "gem" "" with
            | "" -> Setup.strongest c o (Encounter.boss level Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e12 })) |> Option.defaultValue ""
            | g -> g
        let play = Setup.auto c o gem
        let f = Setup.fight c o (Setup.player c o play None) enemies
        let summary = [ for seed in 1 .. runs -> Sim.run f (uint32 seed) ] |> Summary.ofOutcomes
        obj
            [ "gem", JStr gem
              "scenario", JStr scenario
              "runs", num (float summary.Runs)
              "killRate", num summary.KillRate
              "clear", (match summary.Ttk with Some t -> stats t | None -> JNull)
              "dps", stats summary.Dps
              "damageShare", pairs summary.DamageShare
              "uptime", pairs summary.Uptime
              "counts", pairs (summary.Counts |> List.sortBy fst)
              "deathRate", num summary.DeathRate
              "damageTakenPerSecond", num summary.DamageTakenPerSecond
              "lowestLife", obj [ "mean", num (fst summary.MinLife); "worst", num (snd summary.MinLife) ]
              "waitingForMana", num summary.Starved ]

/// The simulator for a front end: the game data read once, the last character kept.
/// Requests and answers are JSON text:
///   { "op": "character", "build": "<character JSON or PoB XML>", "passives": "id,id,..." }
///   { "op": "fight", "build": "...", "scenario": ..., "time": ..., "runs": ..., "gem": ... }
/// An answer is the report, or { "error": "..." }.
type Session(read: string -> string) =
    let db = lazy (GameDb.load read)
    let tree = lazy (read "tree.json")
    let mutable parsed: (string * PobBuild) option = None
    let mutable last: (string * Loaded) option = None

    /// The character of a build text; `passives` ("" or ids separated by commas) replaces
    /// the passives allocated in it.
    member _.Load(build: string, passives: string, choices: string) : Loaded =
        let key = passives + "|" + choices + "|" + build
        match last with
        | Some (k, c) when k = key -> c
        | _ ->
            let b =
                match parsed with
                | Some (text, b) when text = build -> b
                | _ ->
                    let b, _ = Setup.parseBuild build
                    parsed <- Some(build, b)
                    b
            let b =
                if passives = "" then b
                else
                    let ids = passives.Split(',') |> Array.choose (fun s -> match System.Int32.TryParse(s.Trim()) with | true, v -> Some v | _ -> None) |> List.ofArray
                    // "id:Strength,...": what the attribute passives are chosen to be
                    let chosen =
                        choices.Split(',') |> Array.choose (fun s ->
                            match s.Split(':') with
                            | [| i; a |] -> (match System.Int32.TryParse(i.Trim()) with | true, v -> Some(v, [ "+5 to " + a.Trim() ]) | _ -> None)
                            | _ -> None)
                    { b with TreeNodes = ids; TreeOverrides = chosen |> Array.fold (fun m (i, s) -> Map.add i s m) b.TreeOverrides }
            let c = Setup.loadWith db.Value tree.Value b Options.defaults
            last <- Some(key, c)
            c

    member this.Handle(request: string) : string =
        try
            let j = Json.parse request
            let build = Json.strField "build" j |> Option.defaultValue ""
            // (a single id arrives as a number)
            let passives =
                match Json.field "passives" j with
                | Some (JStr s) -> s
                | Some (JNum n) -> sprintf "%.0f" n
                | _ -> ""
            let choices = Json.strField "choices" j |> Option.defaultValue ""
            match Json.strField "op" j with
            | Some "character" -> Report.write (Report.character (Tree.jewelSlots tree.Value) (this.Load(build, passives, choices)))
            | Some "fight" -> Report.write (Report.fight (this.Load(build, passives, choices)) j)
            | other -> Report.write (JObj(Map.ofList [ "error", JStr(sprintf "unknown op %A" other) ]))
        with e ->
            Report.write (JObj(Map.ofList [ "error", JStr e.Message ]))

/// The session for JavaScript callers (a browser worker): plain functions, whose names stay
/// as written when compiled with Fable.
module Front =
    let create (read: string -> string) : Session = Session read
    let handle (session: Session) (request: string) : string = session.Handle request
