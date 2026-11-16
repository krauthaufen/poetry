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
    /// The pictures of a character's gems (their skill's where the gem shows one), by gem name:
    /// out of the game's own character JSON; a PoB build has none.
    let gemIcons (build: string) : Map<string, string> =
        if not (build.TrimStart().StartsWith "{") then Map.empty
        else
            let root = Json.parse build
            let c = Json.field "character" root |> Option.defaultValue root
            let rec gems (items: Json list) =
                items |> List.collect (fun g ->
                    let name = Json.strField "typeLine" g |> Option.defaultValue ""
                    // the skill's picture (a gem that grants a skill has one), and the gem's own after a "|"
                    let own = Json.strField "icon" g |> Option.defaultValue ""
                    let icon = (Json.strField "gemSkill" g |> Option.defaultValue own) + "|" + own
                    (if name <> "" && icon <> "|" then [ name, icon ] else []) @ gems (Json.list (Json.field "socketedItems" g)))
            gems (Json.list (Json.field "skills" c)) |> Map.ofList

    /// The build's items for an inventory, one for each of its Items (`index`: its place there,
    /// as "unequip" names it): what it is called, its lines, and from the game's own character
    /// JSON its picture, size and what is in its sockets (a PoB build has only the text).
    let items (build: string) (b: PobBuild) : Json =
        let raw = if build.TrimStart().StartsWith "{" then Api.rawItems build else []
        let raw = if raw.Length = b.Items.Length then List.map Some raw else b.Items |> List.map (fun _ -> None)
        let line (kind: string) (text: string) = obj [ "kind", JStr kind; "text", JStr text ]
        JArr
            [ for (i, ((slot, text), r)) in List.indexed (List.zip b.Items raw) do
                match r with
                | Some it ->
                    let str k = Json.strField k it |> Option.defaultValue ""
                    let flag k = Json.field k it = Some(JBool true)
                    let mods (key: string) (kind: string) =
                        Json.list (Json.field key it)
                        |> List.collect (fun m ->
                            let t, k =
                                match m with
                                | JStr s -> s, kind
                                | _ ->
                                    let f = Json.field "flags" m
                                    let has n = f |> Option.bind (Json.field n) = Some(JBool true)
                                    Json.strField "description" m |> Option.defaultValue "",
                                    (if has "fractured" then "fractured" elif has "desecrated" then "desecrated" elif has "crafted" then "crafted" else kind)
                            (Api.stripMarkup t).Split('\n') |> List.ofArray |> List.filter (fun l -> l <> "") |> List.map (line k))
                    let value (p: Json) = Json.list (Json.field "values" p) |> List.choose (fun v -> Json.list (Some v) |> List.tryHead |> Option.bind Json.str)
                    let props =
                        Json.list (Json.field "properties" it)
                        |> List.choose (fun p ->
                            let name = Json.strField "name" p |> Option.defaultValue "" |> Api.stripMarkup
                            match value p with
                            | [] -> if name = "" then None else Some name
                            | vs when name.Contains "{0}" -> Some(vs |> List.indexed |> List.fold (fun (t: string) (k, v) -> t.Replace(sprintf "{%d}" k, v)) name)
                            | vs -> Some(name + ": " + String.concat ", " vs))
                    let needs =
                        Json.list (Json.field "requirements" it)
                        |> List.choose (fun p ->
                            let name = Json.strField "name" p |> Option.defaultValue "" |> Api.stripMarkup
                            match value p with
                            | v :: _ -> Some(if name = "Level" then "Level " + v else v + " " + name)
                            | [] -> None)
                    yield obj [ "index", num (float i); "slot", JStr slot; "at", num (Json.numField "x" it |> Option.defaultValue 0.0)
                                "name", JStr(str "name"); "base", JStr(str "typeLine"); "rarity", JStr((str "rarity").ToLower())
                                "icon", JStr(str "icon"); "w", num (Json.numField "w" it |> Option.defaultValue 1.0); "h", num (Json.numField "h" it |> Option.defaultValue 1.0)
                                "props", strs props
                                "requires", JStr(String.concat ", " needs)
                                "mods", JArr(mods "enchantMods" "enchant" @ mods "implicitMods" "implicit" @ mods "runeMods" "rune" @ mods "utilityMods" "utility" @ mods "explicitMods" "explicit" @ mods "mutatedMods" "mutated")
                                "sockets", JArr(Json.list (Json.field "sockets" it) |> List.mapi (fun k _ ->
                                    match Json.list (Json.field "socketedItems" it) |> List.tryFind (fun s -> Json.numField "socket" s = Some(float k)) with
                                    | Some s ->
                                        // (an augment says what it does in each kind of item)
                                        let says key = Json.list (Json.field key s) |> List.choose (fun m -> match m with | JStr t -> Some t | _ -> Json.strField "description" m) |> List.collect (fun t -> (Api.stripMarkup t).Split('\n') |> List.ofArray) |> List.filter (fun l -> l <> "")
                                        obj [ "name", JStr(Json.strField "typeLine" s |> Option.defaultValue ""); "icon", JStr(Json.strField "icon" s |> Option.defaultValue ""); "lines", strs (says "explicitMods" @ says "bondedMods") ]
                                    | None -> obj [ "name", JStr ""; "icon", JStr ""; "lines", strs [] ]))
                                // "Grants Skill: Level 17 ..." with the skill's picture
                                "grants", JArr(Json.list (Json.field "grantedSkills" it) |> List.map (fun g ->
                                    obj [ "text", JStr((Json.strField "name" g |> Option.defaultValue "" |> Api.stripMarkup) + ": " + (value g |> String.concat ", ")); "icon", JStr(Json.strField "icon" g |> Option.defaultValue "") ]))
                                "corrupted", JBool(flag "corrupted") ]
                | None ->
                    // rarity, name, base, then whatever the text says
                    let lines = text.Split('\n') |> Array.map (fun l -> l.Trim()) |> Array.filter (fun l -> l <> "" && not (l.StartsWith "---")) |> List.ofArray
                    let rarity, rest = match lines with | l :: rest when l.StartsWith "Rarity:" -> l.Substring(7).Trim().ToLower(), rest | _ -> "normal", lines
                    let name, bas, rest = match rest with | a :: c :: rest when rarity = "rare" || rarity = "unique" -> a, c, rest | a :: rest -> "", a, rest | [] -> "", "", []
                    yield obj [ "index", num (float i); "slot", JStr slot; "at", num 0.0; "name", JStr name; "base", JStr bas; "rarity", JStr rarity
                                "icon", JStr ""; "w", num 1.0; "h", num 1.0; "props", strs []; "requires", JStr ""
                                "mods", JArr(rest |> List.map (line "explicit")); "sockets", JArr []; "grants", JArr []; "corrupted", JBool(rest |> List.contains "Corrupted") ] ]

    /// What a minion is, without the number that tells it from the others of its kind
    /// ("Wolf Companion 3 (2)", "Skeletal Warrior 4": the name before the numbers).
    let rec private kindOf (name: string) : string =
        let digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]
        let t = name.TrimEnd().TrimEnd(digits).TrimEnd()
        let cut = t.LastIndexOf " ("
        let t = if t.EndsWith ")" && cut > 0 && t.Substring(cut + 2, t.Length - cut - 3).Trim(digits) = "" then t.Substring(0, cut) else t
        if t = name then name else kindOf t

    /// (`groups`: the build's socket groups before any gem was turned off, `icons`: see `gemIcons`.)
    let character (slots: int list) (anointed: int list) (groups: PobGroup list) (levelsWithSupports: bool) (icons: Map<string, string>) (c: Loaded) : Json =
        let minionCounts = Behaviours.minionCounts c.Spirit c.SpiritUses
        let sheetMods = ModDb.ofList c.Sheet.Mods
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
              "movement", (let speed, more = Defence.movement c.Sheet in obj [ "speed", num speed; "percent", num more ])
              "resistances", JArr [ for t in DamageType.all do if t <> Physical then yield obj [ "name", JStr(DamageType.name t); "value", num (Map.tryFind t v.Resist |> Option.defaultValue 0.0) ] ]
              // what a hit must get through: the largest hit of each type survived from full
              // pools (-1: immune), how often a monster of level 83 connects, what recovers
              "defence",
              (let acc = Constants.monsterAccuracy 83
               obj [ "maxHit", JArr [ for t in DamageType.all do
                                        let m = Defence.maxHit v t
                                        yield obj [ "name", JStr(DamageType.name t); "value", num (if System.Double.IsInfinity m then -1.0 else m) ] ]
                     "level", num 83.0
                     "hitChance", num (Vitals.monsterHitChance v.Evasion acc)
                     "deflectChance", num (Vitals.deflectChance v.Deflection acc)
                     "deflectEffect", num v.DeflectEffect
                     "blockLucky", JBool v.LuckyBlock
                     "blockedTaken", num v.BlockedDamageTaken
                     "freezeThreshold", num v.FreezeThreshold
                     "wardRegen", num v.WardRegen
                     "esDelay", num v.ESRechargeDelay
                     "esRegen", num v.ESRegen ])
              "recovery", obj [ "lifeRegen", num v.LifeRegen; "manaRegen", num v.ManaRegen; "energyShieldRecharge", num v.EnergyShieldRecharge ]
              "pressable", strs (Setup.pressable c)
              // what a rotation's conditions can ask about: the states an enemy can have in this
              // character's fights ("has:", "lacks:"), its skills that leave something standing ("up:", "down:")
              "conditions", (let states, skills = Setup.conditions c Options.defaults in obj [ "states", strs states; "skills", strs skills; "counts", JArr(Setup.infusionsAsked c Options.defaults |> List.map (fun (k, n) -> obj [ "name", JStr k; "max", num (float n) ])) ])
              "groups",
              JArr
                  [ for (i, g) in List.indexed groups do
                      if g.Enabled then
                          yield obj [ "slot", JStr g.Slot
                                      // (the group's number, as "without" names it)
                                      "index", num (float (i + 1))
                                      "gems", JArr [ for (at, x) in List.indexed g.Gems do
                                                       if x.Enabled then
                                                           // (a skill's level with what gear, passives and its supports add)
                                                           let level =
                                                               if x.IsSupport then x.Level
                                                               else
                                                                   let setup supports = { Gem = x.Name; Level = x.Level; Quality = x.Quality; StatSet = 1; Supports = supports }
                                                                   let all = g.Gems |> List.filter (fun y -> y.Enabled && y.IsSupport) |> List.map (fun y -> y.Name, y.Level)
                                                                   // (the supports still on: those of the group as the character was built)
                                                                   let on = List.tryItem i c.Build.Groups |> Option.map (fun h -> h.Gems |> List.filter (fun y -> y.Enabled && y.IsSupport) |> List.map (fun y -> y.Name, y.Level)) |> Option.defaultValue all
                                                                   let own = if levelsWithSupports then max 1 (x.Level - Gems.supportLevels c.Db (setup all)) else x.Level
                                                                   Gems.level c.Db sheetMods c.Sheet.Weapons { setup on with Level = own }
                                                           yield obj [ "name", JStr x.Name; "at", num (float at); "level", num (float level); "socketed", num (float x.Level); "quality", num (float x.Quality); "support", JBool x.IsSupport
                                                                       "icon", JStr((Map.tryFind x.Name icons |> Option.defaultValue "|").Split('|').[0])
                                                                       "gemIcon", JStr((Map.tryFind x.Name icons |> Option.defaultValue "|").Split('|').[1])
                                                                       // what the gem reserves in all: a minion gem for every minion that is not free
                                                                       "reserved", num (c.SpiritUses |> List.tryFind (fun u -> u.Gem = x.Name) |> Option.map (fun u -> u.Flat + float (max 0 ((Map.tryFind x.Name minionCounts |> Option.defaultValue 0) - u.FreeMinions)) * u.PerMinion) |> Option.defaultValue 0.0)
                                                                       // how many minions a persistent minion gem has (0: it is none)
                                                                       "minions", num (float (Map.tryFind x.Name minionCounts |> Option.defaultValue 0))
                                                                       // kept up by Spirit: a persistent buff, a meta gem, a minion
                                                                       "spirit", num (c.SpiritUses |> List.tryFind (fun u -> u.Gem = x.Name) |> Option.map (fun u -> u.Flat + u.PerMinion) |> Option.defaultValue 0.0)
                                                                       "persistent", JBool(not x.IsSupport && (Gems.isPersistentBuff c.Db x.Name || Gems.isMeta c.Db x.Name)) ] ] ] ]
              "minions",
              JArr
                  // (several of a kind are one line: "Wolf Companion 3 (2)" is a Wolf Companion)
                  [ for (name, n) in c.Minions |> List.countBy (fun m -> kindOf m.Name) do
                      yield obj [ "name", JStr name; "count", num (float n) ] ]
              "notUnderstood",
              obj [ "unmapped", strs (cov.Unmapped |> List.map fst |> List.distinct)
                    "unsimulated", strs (cov.Unsimulated |> List.map fst |> List.distinct)
                    "unknown", strs cov.UnknownText
                    "minions", strs c.MinionNotes ] ]

    /// A fight as asked for: { scenario: "boss" | "pinnacle" | "endless" | "pack" | "surrounded", level,
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

    /// A fight made ready: who fights whom, what is pressed, and the way one run of it is
    /// fought (by the fast engine where it can play the fight, `Engine` says which and why).
    /// Making it ready costs far more than a run: whoever asks again and again keeps it.
    type Ready =
        { Gem: string
          Scenario: string
          Steps: (Play * string) list
          Fight: Fight
          Run: uint32 -> Outcome
          Engine: string
          /// How many times their own life the enemies have (see "least").
          Scale: float
          /// A map: how the character plays it (found by playing, or as asked).
          Way: string }

    let prepare (c: Loaded) (request: Json) : Ready =
        let n key d = Json.numField key request |> Option.defaultValue d
        let s key d = Json.strField key request |> Option.defaultValue d
        // (not said: an endgame map's, or the character's own level while that is lower: a
        // character still in the campaign fights what is there for it)
        let level = int (n "level" (float (min 83 (max 1 c.Sheet.Level))))
        // how the character goes into the fight: "start" is "PowerCharge:3,Rage:0" (counts to
        // begin with), the others are switches (1: on)
        let on key = n key 0.0 > 0.0
        let start = starts (s "start" "")
        let o = { Options.defaults with MaxTime = n "time" 30.0; Start = start; Infused = on "infused"; NoFlask = on "noflask"; Immortal = on "immortal"; CastOnSelf = on "self"; LowLife = on "lowlife" }
        // a pack does not end: who dies is replaced (unless the enemies are made unkillable)
        // who the player aims at ("target"): the strongest, or the nearest; in a pack that
        // never ends the nearest, unless said (a rare far out is not what one aims at while
        // others are upon one)
        let packs = s "scenario" "boss" = "pack" || s "scenario" "boss" = "surrounded"
        let o = { o with Targeting = (match s "target" (if packs then "nearest" else "strongest") with "nearest" -> Nearest | _ -> Strongest) }
        // ("least": the enemies die for real, with so much life that the fight lasts at least so
        // many seconds; then a pack ends too)
        let least = if n "life" 0.0 > 0.0 then 0.0 else n "least" 0.0
        let o = { o with Replace = (s "scenario" "boss" = "pack" || s "scenario" "boss" = "surrounded") && n "life" 0.0 <= 0.0 && least <= 0.0 }
        let scenario = s "scenario" "boss"
        // ("burst", "endless": the pinnacle boss, asked with "least" so that it lasts 5 and 30 seconds)
        let pinnacle = scenario = "pinnacle" || scenario = "endless" || scenario = "burst"
        let profile r =
            let p = Rarity.defaults r
            let p = if pinnacle && r = Unique then { p with ElementalResist = 50.0 } else p
            // (a boss has several times the life of its level and rarity: the game's own number
            // for each monster, MonsterVarieties.LifeMultiplier. A map boss: 250%, the middle of
            // them; the pinnacle: The Arbiter of Ash, 990%)
            let p = if r = Unique then { p with Life = p.Life * (if pinnacle then 9.9 else 2.5) } else p
            // (the enemies' resistances as asked for, whoever they are)
            let p = match Json.numField "eres" request with Some v -> { p with ElementalResist = v } | None -> p
            match Json.numField "cres" request with Some v -> { p with ChaosResist = v } | None -> p
        let gem =
            match s "gem" "" with
            | "" -> Setup.strongest c o (Encounter.boss level Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e12 })) |> Option.defaultValue ""
            | g -> g
        // what is pressed: by skill type, or ("rotation", names apart by "|") these in this order,
        // the first that is ready: each as its type says (a buff kept up, a curse kept on the
        // target), otherwise whenever its cooldown is over; those at the end that have no
        // cooldown are pressed in turn
        // (an entry may say when itself, after an "@": "ready", "buff" (kept up), "curse" (kept
        // on the enemy), "below:<resource>:<count>"; "has:<State>" / "lacks:<State>": the enemy
        // aimed at has the state, or has it not; "up:<Skill>" / "down:<Skill>": something the
        // skill left stands (ground, a storm, a totem), or nothing of it does. Several, joined
        // by "+", must all hold: "Flameblast@has:Oil+lacks:Ignite". A state or a skill that
        // nothing goes by is never had and never up)
        let chosen =
            (s "rotation" "").Split('|') |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x <> "") |> List.ofArray
            |> List.map (fun x -> match x.IndexOf '@' with | -1 -> x, "" | k -> x.Substring(0, k), x.Substring(k + 1))
        let gem = match List.tryLast chosen with Some (g, _) -> g | None -> gem
        let usual = Setup.auto c o gem
        let steps : (Play * string) list =
            match chosen with
            | [] -> usual |> List.map (fun p -> p, "")
            | _ ->
                let special g = usual |> List.tryFind (fun p -> match p with | Spam _ | Cycle _ -> false | _ -> Setup.gemOf p = [ g ])
                let alone (g: string) (code: string) =
                    match code.Split(':') with
                    | [| "ready" |] -> Some(OnCooldown g)
                    | [| "buff" |] -> Some(KeepBuff g)
                    | [| "curse" |] -> Some(KeepCurse g)
                    // (a resource's name may have colons of its own: "below:Mote:blue:1")
                    | parts when parts.Length >= 3 && parts.[0] = "below" ->
                        let r = System.String.Join(":", parts.[1 .. parts.Length - 2])
                        Some(WhileBelow(r, (match System.Int32.TryParse parts.[parts.Length - 1] with | true, v -> v | _ -> 1), g))
                    | _ -> None
                // (what a part asks of what the player sees; None: a word that says nothing)
                let asks (g: string) (part: string) : (View -> bool) option =
                    let rest (word: string) = part.Substring(word.Length + 1).Trim()
                    if part.StartsWith "has:" then (let keys = Setup.stateKeys (rest "has") in Some(fun (v: View) -> keys |> List.exists v.EnemyStatus.ContainsKey))
                    elif part.StartsWith "lacks:" then (let keys = Setup.stateKeys (rest "lacks") in Some(fun (v: View) -> not (keys |> List.exists v.EnemyStatus.ContainsKey)))
                    elif part.StartsWith "up:" then Some(Setup.standing c o (rest "up"))
                    elif part.StartsWith "down:" then (let up = Setup.standing c o (rest "down") in Some(fun (v: View) -> not (up v)))
                    else alone g part |> Option.map (Setup.condition c o)
                let said (g: string, code: string) =
                    match code.Split('+') |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x <> "") |> List.ofArray with
                    | [ one ] when (alone g one).IsSome -> alone g one
                    | parts ->
                        match parts |> List.choose (asks g) with
                        | [] -> None
                        | conds -> Some(When((fun (v: View) -> conds |> List.forall (fun cond -> cond v)), g))
                let waits (g, code) = code <> "" || special g |> Option.isSome || (Setup.pressed c o g |> Option.exists (fun p -> (p.Skill.Cooldown |> Option.defaultValue 0.0) > 0.0))
                let tail = chosen |> List.rev |> List.takeWhile (fun x -> not (waits x)) |> List.rev
                // (the last is pressed whenever nothing before it is, unless it says when itself)
                let tail = if tail.IsEmpty && (said (List.last chosen)).IsNone then [ List.last chosen ] else tail
                let before = chosen |> List.take (chosen.Length - tail.Length)
                (before |> List.map (fun (g, code) -> (said (g, code) |> Option.orElse (special g) |> Option.defaultValue (OnCooldown g)), (if (said (g, code)).IsSome then code else "")))
                @ (match tail with [] -> [] | [ (one, _) ] -> [ Spam one, "" ] | many -> [ Cycle(many |> List.map fst), "" ])
        let player = Setup.player c o (steps |> List.map fst) None
        // where the enemies begin: in a melee player's face; one who presses something else (or
        // lets minions fight) stands back twice as far (the distances are this page's choice)
        let melee = player.Skills |> List.exists (fun sk -> sk.Name = gem && sk.Tags.Contains Melee)
        let distance = if melee then 30.0 else 60.0
        let enemies =
            match scenario with
            | "pack" | "surrounded" -> Encounter.pack level profile [ Normal, 8; Magic, 3; Rare, 1 ] distance 25.0 (scenario = "surrounded") 1u
            | _ -> Encounter.boss level profile distance
        let enemies = match n "life" 0.0 with l when l > 0.0 -> enemies |> List.map (fun e -> { e with Life = l }) | _ -> enemies
        // What is pressed "when ready" need not open the fight: against enemies that are dead
        // in a hit or two, a bomb with a fuse or a cry only puts off the hit that kills. So,
        // where nobody chose the order, the fight is fought both ways (five fights each, with
        // the enemies as the game has them): as the skill types say, and with those skills
        // held back until something else has been used. The second stays if the enemies are
        // dead sooner with it (or, where they do not die, more is dealt).
        let steps, player =
            let held = steps |> List.map (fun (p, code) -> match p with | OnCooldown g -> When((fun (v: View) -> v.LastUsed <> ""), g), "held" | _ -> p, code)
            if not chosen.IsEmpty || not (steps |> List.exists (fun (p, _) -> match p with OnCooldown _ -> true | _ -> false)) then steps, player
            else
                let heldPlayer = Setup.player c o (held |> List.map fst) None
                let fights (pl: Actor) =
                    let f = Setup.fight c o pl enemies
                    let run = match Flat.compile f with | Ok program -> Flat.runner program | Error _ -> Sim.run f
                    [ 7001u .. 7005u ] |> List.map run
                let dealt (os: Outcome list) = os |> List.sumBy (fun o -> o.DamageBy |> Map.fold (fun a _ v -> a + v) 0.0)
                let time (os: Outcome list) = os |> List.sumBy (fun o -> o.Duration)
                let a, b = fights player, fights heldPlayer
                let dead (os: Outcome list) = os |> List.forall (fun o -> o.Ttk.IsSome)
                let better = if dead a && dead b then time b < 0.99 * time a elif dead a || dead b then dead b else dealt b > 1.01 * dealt a
                if better then held, heldPlayer else steps, player
        // "map": a march through a map (`Fight.March`): "packs" packs (10), one every "spacing" units,
        // of "pack" monsters each (a rare in every third, a quarter of the others magic; every
        // third normal one attacks from afar), and the map's boss after the last. It is cleared
        // when all of them are dead. The player uses its skills on what is within "engage"
        // units. (How near a monster wakes, 60 units, is a guess.)
        let size = max 1 (int (n "pack" 20.0))
        // (ten packs, 3.75 m from one to the next: a quarter of a map of 150 m and 40 packs, which
        // takes a quarter of the time to play and tells the same of a character)
        // ("rooms": each pack in a room of its own, 6 m long, and awake when the player comes in;
        // 0: packs 3.75 m apart that wake by how near the player is)
        let rooms = n "rooms" 1.0 > 0.0
        let packs = max 1 (int (n "packs" 10.0))
        let spacing = max 5.0 (n "spacing" (if rooms then 60.0 else 37.5))
        // (a map of so many packs, played so: `engage`, `hold` of `Sim.March`)
        let marchOf (count: int) (engage: float) (hold: bool) : March =
            { Speed = fst (Defence.movement c.Sheet)
              Spacing = spacing
              Packs = count
              Rooms = rooms
              Aggro = 60.0
              Engage = engage
              Hold = hold
              Pack = fun k ->
                  if k >= count then Encounter.boss level profile 0.0 |> List.map (fun e -> { e with Name = "Map boss" })
                  else
                      let rare = if k % 3 = 2 then 1 else 0
                      let magic = (size - rare) / 4
                      Encounter.pack level profile [ Normal, size - rare - magic; Magic, magic; Rare, rare ] 0.0 15.0 false (uint32 (1000 + k))
                      |> List.mapi (fun i e ->
                          match e.Attack with
                          | Some a when e.Rarity = Normal && i % 3 = 2 -> { e with Attack = Some { a with Range = 60.0 } }
                          | _ -> e) }
        let marching (m: March) (f: Fight) = { f with March = Some m; Enemies = []; Replace = false; Targeting = Nearest }
        // How the map is played is found by playing: a short map of the same packs is played
        // in six ways (walking on, using the skills from 1, 2.5, 4.5 or 7 m; standing until what
        // is near is dead, from 2.5 or 4.5 m), and the way that clears it soonest is the character's way.
        // (Not cleared, or dead: the more it killed the better.) "engage" and "hold" say the
        // way themselves.
        let march, way =
            if scenario <> "map" then None, ""
            else
                let said (engage: float) (hold: bool) =
                    sprintf "%s, using its skills on what is within %.1f m" (if hold then "stands until what is near is dead" else "walks on") (engage / 10.0)
                match Json.numField "engage" request, Json.numField "hold" request with
                | Some engage, hold -> (let hold = hold <> Some 0.0 in Some (marchOf packs (max 5.0 engage) hold), said (max 5.0 engage) hold)
                | None, Some hold -> Some (marchOf packs 45.0 (hold > 0.0)), said 45.0 (hold > 0.0)
                | None, None ->
                    let trial (engage: float, hold: bool) =
                        let f = marching (marchOf (min packs 3) engage hold) { Setup.fight c o player enemies with MaxTime = 60.0 }
                        let run = match Flat.compile f with | Ok program -> Flat.runner program | Error _ -> Sim.run f
                        let o = run 7001u
                        match o.Ttk with
                        | Some t when o.Died.IsNone -> t
                        | _ -> 1e6 - float o.Kills.Length
                    let engage, hold = [ 10.0, false; 25.0, false; 45.0, false; 70.0, false; 25.0, true; 45.0, true ] |> List.minBy trial
                    Some (marchOf packs engage hold), said engage hold
        // ("engine": "reference" asks for the reference engine whatever the fight)
        let made (k: float) =
            let f = Setup.fight c o player (if k = 1.0 then enemies else enemies |> List.map (fun e -> { e with Life = e.Life * k }))
            // (on a march the player aims at what is nearest: the strongest may be three packs on)
            let f = match march with Some m -> marching m f | None -> f
            if s "engine" "" = "reference" then f, Sim.run f, "reference (asked for)"
            else
                match Flat.compile f with
                | Ok program -> f, Flat.runner program, "flat"
                | Error why -> f, Sim.run f, "reference: " + why
        // How much life makes the fight last "least" seconds is found by fighting: nine fights
        // (seeds of their own, the same for whoever asks), and more life while the middle one
        // of them is over too soon. ("scale": the answer of an earlier question, e.g. of the fight this one is
        // compared with: the same enemies for both.)
        let scale =
            match Json.numField "scale" request with
            | Some k when k > 0.0 -> k
            | _ when least <= 0.0 -> 1.0
            | _ ->
                // (life and length do not go together evenly: there is the time before the first
                // hit, and what is dealt early is not what is dealt later. So: three times the
                // life until it lasts, then between the last two, three times halved)
                let lasts (k: float) =
                    let _, run, _ = made k
                    ([ 7001u .. 7009u ] |> List.map (fun seed -> (run seed).Duration) |> List.sort).[4] >= least
                let rec up (k: float) (left: int) = if left = 0 || lasts k then k else up (k * 3.0) (left - 1)
                let rec between (short: float) (long: float) (left: int) =
                    if left = 0 then long
                    else
                        let mid = sqrt (short * long)
                        if lasts mid then between short mid (left - 1) else between mid long (left - 1)
                match up 1.0 16 with
                | 1.0 -> 1.0
                | k -> between (k / 3.0) k 3
        let f, run, engine = made scale
        { Gem = gem; Scenario = scenario; Steps = steps; Fight = f; Run = run; Engine = engine; Scale = scale; Way = way }

    let fought (ready: Ready) (request: Json) : Json =
        let n key d = Json.numField key request |> Option.defaultValue d
        let gem, scenario, steps, f = ready.Gem, ready.Scenario, ready.Steps, ready.Fight
        let runs = max 1 (int (n "runs" 20.0))
        // (`seed`: the first of the runs' consecutive seeds, so that several askers each run their own)
        let first = max 1 (int (n "seed" 1.0))
        // (`trace`: the first run is kept for showing, see `replay`; that one is the reference's)
        let tracing = n "trace" 0.0 > 0.0
        let outcomes = [ for seed in first .. first + runs - 1 -> if tracing && seed = first then Sim.shown f (uint32 seed) else ready.Run (uint32 seed) ]
        let summary = outcomes |> Summary.ofOutcomes
        obj
            [ "gem", JStr gem
              "engine", JStr ready.Engine
              "way", JStr ready.Way
              "scale", num ready.Scale
              // what is pressed, first to last
              "play", JArr(steps |> List.collect (fun (p, code) ->
                  let how =
                      match p with
                      | Spam _ -> "always" | Cycle _ -> "in turn" | OnCooldown _ -> "when ready" | KeepBuff _ -> "kept up" | KeepCurse _ -> "kept on the enemy"
                      | WhileBelow (r, n, _) -> (if n <= 1 then "when out of " else sprintf "while below %d " n) + r
                      | When _ when code = "held" -> "when ready, not first"
                      | When _ -> "when it fits"
                  Setup.gemOf p |> List.map (fun g -> obj [ "gem", JStr g; "how", JStr how; "cond", JStr code ])))
              "scenario", JStr scenario
              "runs", num (float summary.Runs)
              "killRate", num summary.KillRate
              "clear", (match summary.Ttk with Some t -> stats t | None -> JNull)
              "dps", stats summary.Dps
              // a march: monsters killed a second, metres walked a second, how many were left behind
              "kills", num (outcomes |> List.averageBy (fun o -> float o.Kills.Length / max 1e-9 o.Duration))
              "walked", num (outcomes |> List.averageBy (fun o -> float (Map.tryFind "march:walked" o.Counts |> Option.defaultValue 0) / 10.0 / max 1e-9 o.Duration))
              "leftBehind", num (outcomes |> List.averageBy (fun o -> float (Map.tryFind "march:left behind" o.Counts |> Option.defaultValue 0)))
              "diedIn", num (outcomes |> List.averageBy (fun o -> if o.Died.IsSome then 1.0 else 0.0))
              "dpsEvened", num summary.DpsEvened
              "dpsOverkill", num summary.DpsOverkill
              "damageShare", pairs summary.DamageShare
              "damageType", pairs summary.TypeShare
              "uptime", pairs summary.Uptime
              "counts", pairs (summary.Counts |> List.sortBy fst)
              "deathRate", num summary.DeathRate
              "damageTakenPerSecond", num summary.DamageTakenPerSecond
              "lowestLife", obj [ "mean", num (fst summary.MinLife); "worst", num (snd summary.MinLife) ]
              "waitingForMana", num summary.Starved
              "replay", (if tracing then replay f outcomes.Head else JNull) ]

    let fight (c: Loaded) (request: Json) : Json = fought (prepare c request) request

    /// The build's gems changed, one change after the other (apart by ";", fields by "|";
    /// groups count from 1 as "without" names them, a group's gems from 0):
    ///   add|group|name|level|quality   del|group|gem   set|group|gem|level|quality
    ///   swap|group|gem|name            new|name|level|quality (a group of its own)
    ///   count|group|gem|minions (how many minions a persistent minion gem has)
    let editGems (db: GameDb) (text: string) (b: PobBuild) : PobBuild =
        let supports name = Map.tryFind name db.GemVariants |> Option.defaultValue [] |> List.exists (fun g -> g.Support)
        let skills name = Map.tryFind name db.GemVariants |> Option.defaultValue [] |> List.exists (fun g -> not g.Support)
        let n (x: string) = match System.Int32.TryParse(x.Trim()) with | true, v -> v | _ -> 0
        let gem name level quality support = { Name = name; Level = max 1 level; Quality = quality; Enabled = true; IsSupport = support; StatSet = 1; Minion = None; Count = None; Corrupted = false }
        let inGroup (b: PobBuild) at f = { b with Groups = b.Groups |> List.mapi (fun i g -> if i + 1 = at then { g with Gems = f g.Gems } else g) }
        text.Split(';') |> Array.fold (fun (b: PobBuild) (cmd: string) ->
            match cmd.Split('|') |> List.ofArray with
            | [ "add"; g; name; level; quality ] ->
                inGroup b (n g) (fun gems ->
                    if gems |> List.exists (fun x -> x.Name = name) then gems
                    else gems @ [ gem name (n level) (n quality) (supports name && (not (skills name) || gems |> List.exists (fun x -> not x.IsSupport))) ])
            | [ "del"; g; k ] -> inGroup b (n g) (fun gems -> gems |> List.indexed |> List.filter (fun (i, _) -> i <> n k) |> List.map snd)
            | [ "set"; g; k; level; quality ] -> inGroup b (n g) (List.mapi (fun i x -> if i = n k then { x with Level = max 1 (n level); Quality = n quality } else x))
            | [ "count"; g; k; count ] -> inGroup b (n g) (List.mapi (fun i x -> if i = n k then { x with Count = Some (max 1 (n count)) } else x))
            | [ "swap"; g; k; name ] -> inGroup b (n g) (List.mapi (fun i x -> if i = n k then { x with Name = name; Minion = None; Count = None; Corrupted = false } else x))
            | [ "new"; name; level; quality ] -> { b with Groups = b.Groups @ [ { Label = ""; Slot = ""; Enabled = true; Gems = [ gem name (n level) (n quality) false ] } ] }
            | _ -> b) b

    /// What the Spirit that is left can be spent on one by one: the persistent minion gems (and
    /// whatever else is paid for so), each with where it is ("group|gem", as "count|group|gem|n"
    /// names it), what one costs, how many are free and how many there are; and the Spirit
    /// there is for them. `groups`: where the gems are.
    let minionGems (groups: PobGroup list) (c: Loaded) : Json =
        let place name =
            groups |> List.indexed |> List.tryPick (fun (i, g) ->
                if not g.Enabled then None
                else g.Gems |> List.indexed |> List.tryPick (fun (k, x) -> if x.Enabled && not x.IsSupport && x.Name = name then Some (sprintf "%d|%d" (i + 1) k) else None))
        let counts = Behaviours.minionCounts c.Spirit c.SpiritUses
        obj [ "spirit", num (c.Spirit - (c.SpiritUses |> List.sumBy (fun u -> u.Flat)))
              "gems", JArr(c.SpiritUses |> List.filter (fun u -> u.PerMinion > 0.0) |> List.choose (fun u ->
                          place u.Gem |> Option.map (fun at ->
                              obj [ "name", JStr u.Gem; "at", JStr at; "each", num u.PerMinion; "free", num (float u.FreeMinions)
                                    "count", num (float (Map.tryFind u.Gem counts |> Option.defaultValue 1)) ]))) ]

    /// Lines taken out of items: "<the item's place>\n<the line>", several apart by a line "====".
    /// The first line of the item that reads so goes (a line of a rune with it: it is one line).
    let stripLines (text: string) (b: PobBuild) : PobBuild =
        text.Replace("\r", "").Split([| "\n====\n" |], System.StringSplitOptions.None)
        |> Array.fold (fun (b: PobBuild) (entry: string) ->
            match entry.IndexOf '\n' with
            | -1 -> b
            | k ->
                match System.Int32.TryParse(entry.Substring(0, k).Trim()) with
                | true, at ->
                    let line = entry.Substring(k + 1).Trim()
                    { b with
                        Items =
                            b.Items |> List.mapi (fun i (slot, t: string) ->
                                if i <> at then slot, t
                                else
                                    let lines = t.Split('\n') |> List.ofArray
                                    match lines |> List.tryFindIndex (fun l -> l.Trim() = line) with
                                    | Some j -> slot, (lines |> List.indexed |> List.filter (fun (x, _) -> x <> j) |> List.map snd |> String.concat "\n")
                                    | None -> slot, t) }
                | _ -> b) b

    /// Every gem there is to socket: its name, colour, tags, what it says; and (`group`: the
    /// gems it would go to) whether a support can support one of the group's skills.
    let gems (db: GameDb) (group: PobGem list) : Json =
        let types = group |> List.filter (fun x -> not x.IsSupport) |> List.choose (fun x -> Gems.data db x.Name) |> List.map (fun d -> d.Types)
        JArr
            [ for KeyValue (name, variants) in db.GemVariants do
                for support in [ false; true ] do
                    match variants |> List.tryFind (fun g -> g.Support = support) with
                    | Some g ->
                        match Map.tryFind g.GrantedEffectId db.Skills with
                        | Some d when not d.Hidden ->
                            yield obj [ "name", JStr name; "support", JBool support; "color", num (float d.Color)
                                        "tags", strs (g.Tags |> Set.toList |> List.filter (fun t -> t <> "support"))
                                        "says", JStr d.Description
                                        "fits", JBool(not support || types |> List.exists (Gems.canSupport d)) ]
                        | _ -> ()
                    | None -> () ]

    let private statName (s: Stat) : string =
        let scope d = match d with | AllDamage -> "" | OfType t -> DamageType.name t + " " | Elemental -> "Elemental "
        let ail a = sprintf "%A" a
        let rec name s =
            match s with
            | Damage d -> scope d + "Damage"
            | AddedMin t -> "Added minimum " + DamageType.name t + " Damage"
            | AddedMax t -> "Added maximum " + DamageType.name t + " Damage"
            | AddedDamageEffect -> "Effect of added Damage"
            | SkillConvert (a, b) -> DamageType.name a + " converted to " + DamageType.name b + " (skill)"
            | Convert (a, b) -> DamageType.name a + " converted to " + DamageType.name b
            | GainAs (a, b) -> DamageType.name a + " gained as extra " + DamageType.name b
            | LuckyDamage -> "Lucky Damage"
            | CritChance -> "Critical Hit Chance"
            | CritBonus -> "Critical Damage Bonus"
            | Penetration t -> DamageType.name t + " Penetration"
            | HitChance -> "Accuracy"
            | DoubleDamageChance -> "Chance to deal double Damage"
            | AreaOfEffect -> "Area of Effect"
            | AdditionalProjectiles -> "Additional Projectiles"
            | Pierce -> "Pierce"
            | Chains -> "Chains"
            | Speed -> "Skill Speed"
            | CooldownRecovery -> "Cooldown Recovery"
            | SkillDuration -> "Skill Duration"
            | AilmentChance a -> "Chance to inflict " + ail a
            | AilmentMagnitude a -> "Magnitude of " + (match a with Some a -> ail a | None -> "Ailments")
            | AilmentDuration a -> "Duration of " + (match a with Some a -> ail a | None -> "Ailments")
            | AilmentStacks a -> "Stacks of " + ail a
            | Buildup a -> "Buildup of " + ail a
            | Resist t -> DamageType.name t + " Resistance"
            | MaxResist t -> "Maximum " + DamageType.name t + " Resistance"
            | DamageTaken d -> scope d + "Damage taken"
            | DotTaken -> "Damage over Time taken"
            | Armour -> "Armour"
            | PhysicalReduction -> "Physical Damage Reduction"
            | AilmentThreshold -> "Ailment Threshold"
            | PoiseThreshold -> "Stun Threshold"
            | Attribute a -> (match a with "Str" -> "Strength" | "Dex" -> "Dexterity" | "Int" -> "Intelligence" | a -> a)
            | MaxLife -> "Maximum Life"
            | MaxMana -> "Maximum Mana"
            | MaxEnergyShield -> "Maximum Energy Shield"
            | Evasion -> "Evasion Rating"
            | Deflection -> "Deflection Rating"
            | LifeRegen -> "Life Regeneration"
            | LifeRegenPercent -> "Life Regeneration (% of Life)"
            | ManaRegen -> "Mana Regeneration Rate"
            | ManaCostEfficiency -> "Mana Cost Efficiency"
            | ManaCost -> "Mana Cost"
            | ManaOnKill -> "Mana on Kill"
            | ManaLeech -> "Mana Leech"
            | ForMinions s -> "Minions: " + name s
            | Named n -> n
        name s

    let rec private condName (c: Cond) : string =
        match c with
        | IsCrit -> "on a critical hit"
        | SkillIs t -> (match t with | Tag n -> n | t -> sprintf "%A" t) + " skills"
        | SkillNamed n -> n
        | WhileBuff b -> "while " + b
        | EnemyHas s -> "enemy has " + s
        | PerStack (b, every) -> (if every = 1.0 then "per " else sprintf "per %g " every) + b
        | PerEnemyStack (s, every) -> (if every = 1.0 then "per " else sprintf "per %g " every) + s + " on the enemy"
        | Not c -> "not " + condName c
        | OfOwner c -> "the owner " + condName c

    /// Where the character's numbers come from: every modifier of the character, grouped by
    /// what it changes, each with its kind, value, conditions and source; and what the worn
    /// items show of armour, evasion and energy shield.
    let calcs (c: Loaded) : Json =
        let kind k = match k with | Base -> "base" | Inc -> "increased" | More -> "more" | Flag -> "flag" | Override -> "is"
        let line (m: Mod) = obj [ "kind", JStr(kind m.Kind); "value", num m.Value; "when", JStr(m.Conds |> List.map condName |> String.concat ", "); "source", JStr m.Source ]
        let mods = c.Sheet.Mods |> List.groupBy (fun m -> statName m.Stat) |> List.map (fun (k, ms) -> k, ms |> List.map line)
        let worn = c.Sheet.ItemDefences |> List.groupBy (fun (_, st, _) -> statName st + " on items") |> List.map (fun (k, xs) -> k, xs |> List.map (fun (slot, _, v) -> obj [ "kind", JStr "base"; "value", num v; "when", JStr ""; "source", JStr slot ]))
        JArr((mods @ worn) |> List.sortBy fst |> List.map (fun (k, ls) -> obj [ "stat", JStr k; "mods", JArr ls ]))

/// The simulator for a front end: the game data read once, the last character kept.
/// Requests and answers are JSON text:
///   { "op": "character", "build": "<character JSON or PoB XML>", "passives": "id,id,..." }
///   { "op": "fight", "build": "...", "scenario": ..., "time": ..., "runs": ..., "gem": ... }
/// An answer is the report, or { "error": "..." }.
type Session(read: string -> string) =
    let db = lazy (GameDb.load read)
    let tree = lazy (read "tree.json")
    let nodes = lazy (Tree.parseNodes tree.Value)
    let slots = lazy (Tree.jewelSlots tree.Value)
    let bases = lazy (Defence.classBases tree.Value)
    let mutable parsed: (string * PobBuild) option = None
    let mutable last: (string * Loaded) option = None
    /// The fights made ready last, by what was asked (all of it but which runs).
    let mutable ready: (string * Report.Ready) list = []
    /// The build as its text says (gem levels without what supports add, see `Setup.ownLevels`).
    let origin (build: string) : PobBuild =
        match parsed with
        | Some (text, b) when text = build -> b
        | _ ->
            let b = Setup.ownLevels db.Value (fst (Setup.parseBuild build))
            parsed <- Some(build, b)
            b

    /// The character of a build text as the request changes it: "passives" ("" or ids apart
    /// by commas) replaces the passives allocated in it, "choices" says what attribute
    /// passives are, "without" turns gems off, "gems" changes them (see `Report.editGems`),
    /// "unequip", "equip" and "strip" change what is worn, "custom" adds lines of modifiers.
    member _.Load(build: string, j: Json) : Loaded =
        // (a single number arrives as a number)
        let s key =
            match Json.field key j with
            | Some (JStr s) -> s
            | Some (JNum n) -> sprintf "%.0f" n
            | _ -> ""
        let passives, choices, start, without, unequip, equip = s "passives", s "choices", s "start", s "without", s "unequip", s "equip"
        let gems, custom, strip = s "gems", s "custom", s "strip"
        let lowLife = Json.numField "lowlife" j |> Option.exists (fun v -> v > 0.0)
        let key = String.concat "|" [ (if lowLife then "low" else ""); start; without; unequip; equip; gems; custom; strip; passives; choices; build ]
        match last with
        | Some (k, c) when k = key -> c
        | _ ->
            let b = Report.editGems db.Value gems (origin build)
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
            // items taken off: their places among the build's items
            let off = unequip.Split(',') |> Array.choose (fun x -> match System.Int32.TryParse(x.Trim()) with | true, v -> Some v | _ -> None) |> Set.ofArray
            // items worn instead: "<where>\n<the item's text as the game copies it>", several apart
            // by a line "====". Where: the place among the build's items of the one it replaces,
            // or the name of a slot that is empty
            let b =
                equip.Replace("\r", "").Split([| "\n====\n" |], System.StringSplitOptions.None)
                |> Array.fold (fun (b: PobBuild) (entry: string) ->
                    match entry.IndexOf '\n' with
                    | -1 -> b
                    | k ->
                        let where, text = entry.Substring(0, k).Trim(), entry.Substring(k + 1)
                        match System.Int32.TryParse where with
                        | true, at -> { b with Items = b.Items |> List.mapi (fun i (slot, t) -> if i = at then slot, text else slot, t) }
                        | _ -> { b with Items = b.Items @ [ where, text ] }) b
            let b = Report.stripLines strip b
            let b = if off.IsEmpty then b else { b with Items = b.Items |> List.indexed |> List.filter (fun (i, _) -> not (off.Contains i)) |> List.map snd }
            let lines = custom.Replace("\r", "").Split('\n') |> Array.map (fun l -> l.Trim()) |> Array.filter (fun l -> l <> "") |> List.ofArray
            let b = if lines.IsEmpty then b else { b with ExtraLines = b.ExtraLines @ lines }
            let c = Setup.loadParsed db.Value nodes.Value slots.Value bases.Value b { Options.defaults with LowLife = lowLife; Start = Report.starts start; Without = without.Split(',') |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x <> "") |> List.ofArray }
            last <- Some(key, c)
            c

    member this.Handle(request: string) : string =
        this.HandleWith(request, "")

    /// The same with the build's text apart from the request (it is long, and the same again
    /// and again: no need to read it out of the request every time).
    member this.HandleWith(request: string, text: string) : string =
        try
            let j = Json.parse request
            let build = if text <> "" then text else Json.strField "build" j |> Option.defaultValue ""
            let edited () = Report.editGems db.Value (Json.strField "gems" j |> Option.defaultValue "") (origin build)
            match Json.strField "op" j with
            | Some "character" ->
                let c = this.Load(build, j)
                // (gems and items as the character was built: one turned or taken off here is still listed)
                let sheet = Report.character slots.Value (Pob.itemPassives nodes.Value c.Build) (edited ()).Groups false (Report.gemIcons build) c
                Report.write (match sheet with | JObj m -> JObj(Map.add "items" (Report.items build (origin build)) m) | _ -> sheet)
            | Some "fight" ->
                // (the same fight is asked for seed after seed: it is made ready once)
                let key =
                    match j with
                    | JObj m -> Report.write (JObj(m |> Map.remove "seed" |> Map.remove "runs" |> Map.remove "trace" |> Map.remove "build")) + "|" + string build.Length + "|" + string (hash build)
                    | _ -> ""
                let r =
                    match ready |> List.tryFind (fun (k, _) -> k = key) with
                    | Some (_, r) -> r
                    | None ->
                        let r = Report.prepare (this.Load(build, j)) j
                        ready <- (key, r) :: (if ready.Length >= 8 then List.truncate 7 ready else ready)
                        r
                Report.write (Report.fought r j)
            | Some "calcs" -> Report.write (Report.calcs (this.Load(build, j)))
            | Some "minions" ->
                let c = this.Load(build, j)
                Report.write (Report.minionGems c.Build.Groups c)
            | Some "gems" ->
                let at = Json.numField "group" j |> Option.map int |> Option.defaultValue 0
                Report.write (Report.gems db.Value ((edited ()).Groups |> List.tryItem (at - 1) |> Option.map (fun g -> g.Gems) |> Option.defaultValue []))
            | other -> Report.write (JObj(Map.ofList [ "error", JStr(sprintf "unknown op %A" other) ]))
        with e ->
            Report.write (JObj(Map.ofList [ "error", JStr e.Message ]))

/// The session for JavaScript callers (a browser worker): plain functions, whose names stay
/// as written when compiled with Fable.
module Front =
    let create (read: string -> string) : Session = Session read
    let handle (session: Session) (request: string) : string = session.Handle request
    let handleWith (session: Session) (request: string) (build: string) : string = session.HandleWith(request, build)
