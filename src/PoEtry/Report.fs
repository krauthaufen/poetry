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
    let character (slots: int list) (anointed: int list) (c: Loaded) : Json =
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
              // the passives the items allocate (an anoint, a jewel's "Allocates"): no points, no way to them
              "anointed", JArr(anointed |> List.map (fun i -> num (float i)))
              // what an attribute passive was chosen to be
              "choices", JArr(c.Build.TreeOverrides |> Map.toList |> List.choose (fun (i, stats) ->
                  [ "Strength"; "Dexterity"; "Intelligence" ]
                  |> List.tryFind (fun a -> stats |> List.exists (fun (s: string) -> s.EndsWith("to " + a)))
                  |> Option.map (fun a -> obj [ "id", num (float i); "name", JStr a ])))
              "jewels", JArr(c.Build.Items |> List.choose jewel)
              "passivesSet1", JArr(c.Build.TreeNodesSet1 |> List.map (fun i -> num (float i)))
              "passivesSet2", JArr(c.Build.TreeNodesSet2 |> List.map (fun i -> num (float i)))
              "attributes", obj [ "str", num v.Str; "dex", num v.Dex; "int", num v.Int ]
              // what the character has a count of going into a fight (charges, rage, ...)
              "resources", JArr(c.Resources |> List.filter (fun (k, _) -> k.EndsWith "Charge" || k = "Rage" || k.StartsWith "Mote:") |> List.map (fun (k, n) -> obj [ "name", JStr k; "start", num (float n); "max", num (float (Map.tryFind k c.MaxResources |> Option.defaultValue n)) ]))
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
    /// time, runs, seed (the first run's, 1), life (0: the enemies' own), gem ("": the strongest to press) }.
    /// Missing fields take the defaults (a level 83 boss, 30 s, 20 runs).
    /// One run for showing: who is there at the start (the player in the middle), and what
    /// happened in order, as short arrays:
    ///   ["u", time, actor, skill, marks]   a use; a mark is ["r", x, y, radius, delay] (an area),
    ///                                      ["f", x1, y1, x2, y2, ...] (a projectile's way) or ["l", x1, y1, x2, y2]
    ///   ["s", time, enemy, damage, crit]   a hit on an enemy
    ///   ["m", time, enemy, x, y]           an enemy is here now
    ///   ["h", time, enemy, damage]         an enemy's hit on the player
    ///   ["k", time, enemy]                 an enemy dies
    ///   ["e", time, enemy, x, y, size, rarity]  a new enemy comes into the fight there
    let private replay (f: Fight) (o: Outcome) : Json =
        let r (v: float) = num (System.Math.Round(v, 1))
        let mark (m: Mark) =
            match m with
            | Ring (c, radius, delay) -> JArr [ JStr "r"; r c.X; r c.Y; r radius; num (System.Math.Round(delay, 3)) ]
            | Flight points -> JArr(JStr "f" :: (points |> List.collect (fun p -> [ r p.X; r p.Y ])))
            | Line (a, b) -> JArr [ JStr "l"; r a.X; r a.Y; r b.X; r b.Y ]
        let t (v: float) = num (System.Math.Round(v, 3))
        let happened =
            o.Shown |> List.map (fun s ->
                match s with
                | Used (time, actor, skill, marks) -> time, JArr [ JStr "u"; t time; JStr actor; JStr skill; JArr(marks |> List.map mark) ]
                | Struck (time, enemy, damage, crit) -> time, JArr [ JStr "s"; t time; JStr enemy; num (System.Math.Round damage); JBool crit ]
                | Moved (time, enemy, p) -> time, JArr [ JStr "m"; t time; JStr enemy; r p.X; r p.Y ]
                | Hurt (time, enemy, damage) -> time, JArr [ JStr "h"; t time; JStr enemy; num (System.Math.Round damage) ]
                | Entered (time, e) -> time, JArr [ JStr "e"; t time; JStr e.Name; r e.Position.X; r e.Position.Y; r e.Size; JStr(Rarity.name e.Rarity) ])
        let kills = o.Kills |> List.map (fun (enemy, _, time) -> time, JArr [ JStr "k"; t time; JStr enemy ])
        obj
            [ "duration", num o.Duration
              "enemies", JArr(f.Enemies |> List.map (fun e ->
                  obj [ "name", JStr e.Name; "x", r e.Position.X; "y", r e.Position.Y; "size", r e.Size; "rarity", JStr(Rarity.name e.Rarity); "life", num e.Life ]))
              "actors", JArr((f.Player :: f.Minions) |> List.map (fun a ->
                  obj [ "name", JStr a.Name; "minion", JBool(a.Name <> f.Player.Name)
                        "melee", JBool(a.Body |> Option.exists (fun b -> b.Melee)); "size", num (a.Body |> Option.map (fun b -> b.Size) |> Option.defaultValue 0.0)
                        // a companion (a tamed monster): shown apart from the summoned ones
                        "companion", JBool(a.Skills |> List.exists (fun sk -> sk.Tags.Contains(Tag "Companion"))) ]))
              // (a stable sort: what happened at the same time stays in its order)
              "events", JArr(happened @ kills |> List.sortBy fst |> List.map snd) ]

    /// "PowerCharge:3,Rage:0": the counts a character begins with.
    let starts (text: string) : (string * int) list =
        text.Split(',') |> Array.choose (fun x ->
            // (a mote's name has a colon of its own: the count is after the last)
            let cut = x.LastIndexOf ':'
            if cut <= 0 then None
            else match System.Int32.TryParse(x.Substring(cut + 1).Trim()) with | true, v -> Some(x.Substring(0, cut).Trim(), v) | _ -> None)
        |> List.ofArray

    let fight (c: Loaded) (request: Json) : Json =
        let n key d = Json.numField key request |> Option.defaultValue d
        let s key d = Json.strField key request |> Option.defaultValue d
        let level, runs = int (n "level" 83.0), max 1 (int (n "runs" 20.0))
        // how the character goes into the fight: "start" is "PowerCharge:3,Rage:0" (counts to
        // begin with), the others are switches (1: on)
        let on key = n key 0.0 > 0.0
        let start = starts (s "start" "")
        let o = { Options.defaults with MaxTime = n "time" 30.0; Start = start; Infused = on "infused"; NoFlask = on "noflask"; Immortal = on "immortal"; CastOnSelf = on "self"; LowLife = on "lowlife" }
        // a pack does not end: who dies is replaced (unless the enemies are made unkillable)
        let o = { o with Replace = (s "scenario" "boss" = "pack" || s "scenario" "boss" = "surrounded") && n "life" 0.0 <= 0.0 }
        let scenario = s "scenario" "boss"
        let profile r =
            let p = Rarity.defaults r
            if scenario = "pinnacle" && r = Unique then { p with ElementalResist = 50.0 } else p
        let gem =
            match s "gem" "" with
            | "" -> Setup.strongest c o (Encounter.boss level Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e12 })) |> Option.defaultValue ""
            | g -> g
        let play = Setup.auto c o gem
        let player = Setup.player c o play None
        // where the enemies begin: in a melee player's face; one who presses something else (or
        // lets minions fight) stands back twice as far (the distances are this page's choice)
        let melee = player.Skills |> List.exists (fun sk -> sk.Name = gem && sk.Tags.Contains Melee)
        let distance = if melee then 30.0 else 60.0
        let enemies =
            match scenario with
            | "pack" | "surrounded" -> Encounter.pack level profile [ Normal, 8; Magic, 3; Rare, 1 ] distance 25.0 (scenario = "surrounded") 1u
            | _ -> Encounter.boss level profile distance
        let enemies = match n "life" 0.0 with l when l > 0.0 -> enemies |> List.map (fun e -> { e with Life = l }) | _ -> enemies
        let f = Setup.fight c o player enemies
        // (`seed`: the first of the runs' consecutive seeds, so that several askers each run their own)
        let first = max 1 (int (n "seed" 1.0))
        // (`trace`: the first run is kept for showing, see `replay`)
        let tracing = n "trace" 0.0 > 0.0
        let outcomes = [ for seed in first .. first + runs - 1 -> (if tracing && seed = first then Sim.shown else Sim.run) f (uint32 seed) ]
        let summary = outcomes |> Summary.ofOutcomes
        obj
            [ "gem", JStr gem
              "scenario", JStr scenario
              "runs", num (float summary.Runs)
              "killRate", num summary.KillRate
              "clear", (match summary.Ttk with Some t -> stats t | None -> JNull)
              "dps", stats summary.Dps
              "damageShare", pairs summary.DamageShare
              "damageType", pairs summary.TypeShare
              "uptime", pairs summary.Uptime
              "counts", pairs (summary.Counts |> List.sortBy fst)
              "deathRate", num summary.DeathRate
              "damageTakenPerSecond", num summary.DamageTakenPerSecond
              "lowestLife", obj [ "mean", num (fst summary.MinLife); "worst", num (snd summary.MinLife) ]
              "waitingForMana", num summary.Starved
              "replay", (if tracing then replay f outcomes.Head else JNull) ]

/// The simulator for a front end: the game data read once, the last character kept.
/// Requests and answers are JSON text:
///   { "op": "character", "build": "<character JSON or PoB XML>", "passives": "id,id,..." }
///   { "op": "fight", "build": "...", "scenario": ..., "time": ..., "runs": ..., "gem": ... }
/// An answer is the report, or { "error": "..." }.
type Session(read: string -> string) =
    let db = lazy (GameDb.load read)
    let tree = lazy (read "tree.json")
    let nodes = lazy (Tree.parseNodes tree.Value)
    let mutable parsed: (string * PobBuild) option = None
    let mutable last: (string * Loaded) option = None

    /// The character of a build text; `passives` ("" or ids separated by commas) replaces
    /// the passives allocated in it.
    member _.Load(build: string, passives: string, choices: string, lowLife: bool, start: string) : Loaded =
        let key = (if lowLife then "low|" else "") + start + "|" + passives + "|" + choices + "|" + build
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
            let c = Setup.loadWith db.Value tree.Value b { Options.defaults with LowLife = lowLife; Start = Report.starts start }
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
            let low = Json.numField "lowlife" j |> Option.exists (fun v -> v > 0.0)
            let start = Json.strField "start" j |> Option.defaultValue ""
            match Json.strField "op" j with
            | Some "character" -> (let c = this.Load(build, passives, choices, low, start)
                                   Report.write (Report.character (Tree.jewelSlots tree.Value) (Pob.itemPassives nodes.Value c.Build) c))
            | Some "fight" -> Report.write (Report.fight (this.Load(build, passives, choices, low, start)) j)
            | other -> Report.write (JObj(Map.ofList [ "error", JStr(sprintf "unknown op %A" other) ]))
        with e ->
            Report.write (JObj(Map.ofList [ "error", JStr e.Message ]))

/// The session for JavaScript callers (a browser worker): plain functions, whose names stay
/// as written when compiled with Fable.
module Front =
    let create (read: string -> string) : Session = Session read
    let handle (session: Session) (request: string) : string = session.Handle request
