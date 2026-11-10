/// poesie <build.xml | build code file> [options]
///   --group N        socket group to simulate (default: the build's main group)
///   --runs N         Monte Carlo runs (default 500)
///   --time S         fight length in seconds (default 60)
///   --scenario S     boss | pinnacle | pack | surrounded (default boss)
///   --pack N,M,R     pack of N normal, M magic, R rare monsters (default 8,3,1)
///   --distance D     how far in front the enemies stand, game units (default 30; 10 = 1 m)
///   --level L        monster level (default 83)
///   --life X         override life of every enemy (e.g. 1e12 to measure sustained DPS)
///   --res X          override elemental resistance of every enemy
///   --target T       nearest | strongest (default strongest)
///   --charges N      power charges at the start (default: your maximum)
///   --skip P         override the chance (%) to not remove charges when consuming
///   --bonded         apply "Bonded:" rune lines (normally Shaman only)
///   --shotgun        let several projectiles of one use hit the same enemy
///   --positions F    sweep gap to the boss / slam reach / boss radius; write an HTML page with
///                    the table and pictures of the best and worst layout to file F
///   --suite          run the standard scenarios (pack, pack + 2 rares, surrounded, boss) and print a table
///   --data DIR       exported game data (default: data/game next to the repo)
module PoEsie.Cli

open System
open System.IO
open PoEsie

/// PoB build codes: url-safe base64 of zlib-compressed XML.
let decodeBuildCode (code: string) =
    let b64 = code.Trim().Replace('-', '+').Replace('_', '/')
    let padded = b64 + String('=', (4 - b64.Length % 4) % 4)
    use input = new MemoryStream(Convert.FromBase64String padded)
    use z = new Compression.ZLibStream(input, Compression.CompressionMode.Decompress)
    use reader = new StreamReader(z)
    reader.ReadToEnd()

let rec private opts (args: string list) =
    match args with
    | "--suite" :: rest -> Map.add "suite" "1" (opts rest)
    | "--bonded" :: rest -> Map.add "bonded" "1" (opts rest)
    | "--all" :: rest -> Map.add "all" "1" (opts rest)
    | "--shotgun" :: rest -> Map.add "shotgun" "1" (opts rest)
    | k :: v :: rest when k.StartsWith "--" -> Map.add (k.Substring 2) v (opts rest)
    | _ :: rest -> opts rest
    | [] -> Map.empty

let private top n (xs: 'a list) = xs |> List.countBy id |> List.sortByDescending snd |> List.truncate n

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | [] ->
        printfn "usage: poesie <build.xml | build-code-file> [--group N] [--runs N] [--time S] [--life X] [--level L] [--res X]"
        1
    | file :: rest ->
        let o = opts rest
        let get k d = Map.tryFind k o |> Option.map float |> Option.defaultValue d
        let dataDir =
            Map.tryFind "data" o
            |> Option.defaultValue (Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "data", "game"))
        let text = File.ReadAllText file
        let db = GameDb.load (fun f -> File.ReadAllText(Path.Combine(dataDir, f)))
        let tree = Tree.parseNodes (File.ReadAllText(Path.Combine(dataDir, "tree.json")))
        // A PoB build (XML or code), or a character from the official API (JSON).
        let build, displayed =
            if text.TrimStart().StartsWith "{" then Api.read text
            elif text.TrimStart().StartsWith "<" then Pob.read text, []
            else Pob.read (decodeBuildCode text), []
        let build = if Map.containsKey "bonded" o then { build with Config = Map.add "bonded" "1" build.Config } else build
        let sheet = Pob.character db tree build |> fun s -> Pob.withPersistentBuffs db s build
        let sheet =
            match Map.tryFind "skip" o with
            | Some p ->
                { sheet with
                    Mods = (sheet.Mods |> List.filter (fun m -> m.Stat <> Named "ChargeSkipConsume")) @ [ Mod.base' (Named "ChargeSkipConsume") (float p) ] }
            | None -> sheet
        let cycles =
            build.Groups
            |> List.filter (fun g -> g.Enabled)
            |> List.choose (fun g ->
                match g.Gems |> List.tryFind (fun gem -> not gem.IsSupport && Gems.isPersistentBuff db gem.Name) with
                | Some a ->
                    Some
                        { Gem = a.Name; Level = a.Level; Quality = a.Quality; StatSet = a.StatSet
                          Supports = g.Gems |> List.filter (fun x -> x.IsSupport) |> List.map (fun x -> x.Name, x.Level) }
                | None -> None)
            |> Behaviours.cycles db (ModDb.ofList sheet.Mods)
        for c in cycles do printfn "cycle: %s, %.1fs per element, %A" c.Name c.Period (c.Variants |> List.map (fun v -> v.Head.Stat, v.Head.Value))
        let resources =
            Pob.maxResources sheet
            |> List.map (fun (k, n) ->
                match k, Map.tryFind "charges" o with
                | "PowerCharge", Some c -> k, int c
                | _ -> k, n)

        printfn "== %s %s, level %d: %d items, %d passives, %d socket groups" build.ClassName build.Ascendancy build.Level build.Items.Length build.TreeNodes.Length build.Groups.Length
        printfn "character: %d modifiers; starts with %s" sheet.Mods.Length (resources |> List.map (fun (k, n) -> sprintf "%d %s" n k) |> String.concat ", ")
        match sheet.MainHand with
        | Some w -> printfn "main hand: %A, %.1f%% crit, %.2f APS" (w.Damage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max)) w.CritChance w.AttacksPerSecond
        | None -> printfn "main hand: none"
        // Check our weapon maths against what the game displays.
        for slot, d in displayed do
            if slot = "Weapon 1" then
                let show (m: Map<DamageType, Range>) = m |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat ", "
                printfn "  game shows: %s, %s crit, %s APS" (show d.Damage) (d.CritChance |> Option.map (sprintf "%.2f%%") |> Option.defaultValue "?") (d.AttacksPerSecond |> Option.map (sprintf "%.2f") |> Option.defaultValue "?")
        let cov = sheet.Coverage
        printfn "\n-- not understood on the character (%d unmapped stats, %d unsimulated, %d unknown lines)" cov.Unmapped.Length cov.Unsimulated.Length cov.UnknownText.Length
        let topN = if Map.containsKey "all" o then 1000 else 15
        cov.Unmapped |> List.map fst |> top topN |> List.iter (fun (s, n) -> printfn "  unmapped     %s%s" s (if n > 1 then sprintf " x%d" n else ""))
        cov.Unsimulated |> List.countBy id |> List.sortBy (fun ((_, m), _) -> m) |> List.truncate topN |> List.iter (fun ((s, m), n) -> printfn "  unsimulated  %-14s %s%s" m s (if n > 1 then sprintf " x%d" n else ""))
        cov.UnknownText |> List.truncate 15 |> List.iter (printfn "  unknown      %s")

        printfn "\n-- socket groups"
        build.Groups
        |> List.iteri (fun i g ->
            let names = g.Gems |> List.map (fun gem -> sprintf "%s%s %d" (if gem.IsSupport then "+" else "") gem.Name gem.Level) |> String.concat ", "
            printfn "  %2d%s %s" (i + 1) (if i + 1 = build.MainGroup then "*" else " ") names)

        let groupIdx = int (get "group" (float build.MainGroup))
        match List.tryItem (groupIdx - 1) build.Groups with
        | None -> printfn "no socket group %d" groupIdx; 1
        | Some group ->
            match Pob.groupSkill db sheet group |> List.tryHead with
            | None -> printfn "group %d has no active gem" groupIdx; 1
            | Some (Error e) -> printfn "group %d: %s" groupIdx e; 1
            | Some (Ok (skill, skillCov)) ->
                printfn "\n-- simulating %s (group %d)" skill.Name groupIdx
                printfn "  base %s, x%.2f, crit %.1f%%, %.3fs per use%s" (skill.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ") skill.BaseMultiplier skill.BaseCrit skill.BaseTime (match skill.Cooldown with Some c -> sprintf ", cooldown %.2fs" c | None -> "")
                skillCov.Unmapped |> List.iter (fun (s, v) -> printfn "  unmapped skill stat     %s = %g" s v)
                skillCov.Unsimulated |> List.distinctBy snd |> List.iter (fun (s, m) -> printfn "  unsimulated skill stat  %s (%s)" s m)
                // --shotgun: several projectiles of one use may hit the same enemy
                let rec shotgun (s: Skill) =
                    { s with
                        Delivery = (match s.Delivery with Projectiles p -> Projectiles { p with CanShotgun = true } | d -> d)
                        Parts = s.Parts |> List.map shotgun }
                let skill = if Map.containsKey "shotgun" o then shotgun skill else skill
                let level = int (get "level" 83.0)
                let profile r =
                    let p = Rarity.defaults r
                    let p = match Map.tryFind "scenario" o, r with
                            | Some "pinnacle", Unique -> { p with ElementalResist = 50.0 } // PoB pinnacle default
                            | _ -> p
                    match Map.tryFind "res" o with
                    | Some v -> { p with ElementalResist = float v }
                    | None -> p
                let distance = get "distance" 30.0
                let enemies =
                    match Map.tryFind "scenario" o |> Option.defaultValue "boss" with
                    | "pack" | "surrounded" as sc ->
                        let counts =
                            (Map.tryFind "pack" o |> Option.defaultValue "8,3,1").Split(',') |> Array.map int |> List.ofArray
                        let n, m, r = match counts with [ a; b; c ] -> a, b, c | _ -> 8, 3, 1
                        Encounter.pack level profile [ Normal, n; Magic, m; Rare, r ] distance 25.0 (sc = "surrounded") 1u
                    | _ -> Encounter.boss level profile distance
                let enemies =
                    match Map.tryFind "life" o with
                    | Some l -> enemies |> List.map (fun e -> { e with Life = float l })
                    | None -> enemies
                printfn "  enemies: %s" (enemies |> List.countBy (fun e -> Rarity.name e.Rarity) |> List.map (fun (r, n) -> sprintf "%d %s" n r) |> String.concat ", ")
                printfn "  delivery: %A" skill.Delivery
                let fight =
                    { Enemies = enemies
                      Player = { Actor.make "Player" sheet.Mods [ skill ] with Resources = resources; Cycles = cycles }
                      Minions = []
                      Targeting = (if Map.tryFind "target" o = Some "nearest" then Nearest else Strongest)
                      Reach = get "reach" 15.0
                      MaxTime = get "time" 60.0 }
                printfn ""
                if Map.containsKey "positions" o then
                    let projectilePart =
                        (skill :: skill.Parts) |> List.tryPick (fun s -> match s.Delivery with Projectiles p -> Some p | _ -> None)
                    let charges = resources |> List.tryPick (fun (k, n) -> if k = "PowerCharge" then Some n else None) |> Option.defaultValue 0
                    let layouts =
                        [ for size in [ 8.0; 12.0; 20.0 ] do
                            for gap in [ 0.0; 5.0; 10.0; 20.0; 35.0 ] do
                                for reach in [ 10.0; 15.0; 25.0 ] -> size, gap, reach ]
                    let runs = int (get "runs" 30.0)
                    let results =
                        layouts
                        |> List.map (fun (size, gap, reach) ->
                            let boss =
                                { (List.head (Encounter.boss level profile (gap + size))) with
                                    Size = size
                                    Life = get "life" 1e12 }
                            let f = { fight with Enemies = [ boss ]; Reach = reach; MaxTime = get "time" 20.0 }
                            let sum = Summary.simulate runs f
                            let perSlam =
                                let c k = sum.Counts |> List.tryPick (fun (n, v) -> if n = k then Some v else None) |> Option.defaultValue 0.0
                                let parts = skill.Parts |> List.sumBy (fun p -> c ("hits:" + p.Name))
                                if c ("uses:" + skill.Name) > 0.0 then parts / c ("uses:" + skill.Name) else 0.0
                            (size, gap, reach, boss), sum.Dps.Mean, perSlam)
                        |> List.sortByDescending (fun (_, d, _) -> d)
                    printfn "%-7s %-6s %-6s %10s %14s" "radius" "gap" "reach" "proj/slam" "DPS"
                    for ((size, gap, reach, _), dps, per) in results do
                        printfn "%-7.0f %-6.0f %-6.0f %10.1f %14.0f" size gap reach per dps
                    match projectilePart with
                    | Some p ->
                        let p = { p with Count = p.Count + (match p.PerCharge with Some (_, per) -> per * charges | None -> 0) }
                        let pic ((size, gap, reach, boss: EnemyDef), dps: float, per: float) label =
                            Picture.svg (sprintf "%s: boss radius %.0f, gap %.0f, slam reach %.0f, %.1fM DPS" label size gap reach (dps / 1e6)) reach p boss [ boss ]
                        let rows =
                            results
                            |> List.map (fun ((size, gap, reach, _), dps, per) ->
                                sprintf "<tr><td>%.0f</td><td>%.0f</td><td>%.0f</td><td>%.1f</td><td>%.1fM</td></tr>" size gap reach per (dps / 1e6))
                            |> String.concat ""
                        let html =
                            sprintf """<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>%s positioning</title>
<style>
:root{--bg:#fbfaf7;--fg:#1d1d1b;--muted:#6b6a66;--line:#e2dfd8;--enemy:#c9c3b6;--hit:#c2410c;--miss:#b9b4aa;--player:#1d4ed8}
@media (prefers-color-scheme:dark){:root{--bg:#161614;--fg:#ecebe7;--muted:#a3a199;--line:#34332f;--enemy:#4a4740;--hit:#fb923c;--miss:#55524b;--player:#60a5fa}}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 system-ui,sans-serif}
main{max-width:980px;margin:0 auto;padding:24px 16px}
h1{font-size:22px;margin:0 0 4px}p{color:var(--muted);margin:0 0 20px}
.pics{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:16px;margin-bottom:28px}
figure{margin:0;border:1px solid var(--line);border-radius:10px;padding:12px}
svg{width:100%%;height:auto;max-height:420px}
figcaption{font-size:13px;color:var(--muted);margin-top:8px}
.enemy{fill:var(--enemy);stroke:none}.hit{stroke:var(--hit);stroke-width:.8}.miss{stroke:var(--miss);stroke-width:.4;stroke-dasharray:2 2}
.player{fill:var(--player)}.impact{stroke:var(--fg);stroke-width:.8;fill:none}
table{border-collapse:collapse;width:100%%;font-variant-numeric:tabular-nums;font-size:14px}
th,td{text-align:right;padding:4px 10px;border-bottom:1px solid var(--line)}th{color:var(--muted);font-weight:600}
</style></head><body><main>
<h1>%s: positioning against a single boss</h1>
<p>%d power charges, %d projectiles per slam in a nova from the impact point. Player (blue) at the left, boss in grey, × marks where the slam lands. Orange: projectiles that hit, dashed: misses. Boss radii are assumptions (game units, 10 = 1 m); the game stores only a size category.</p>
<div class="pics">%s%s</div>
<table><thead><tr><th>boss radius</th><th>gap</th><th>slam reach</th><th>projectile hits / slam</th><th>DPS</th></tr></thead><tbody>%s</tbody></table>
</main></body></html>"""
                                skill.Name skill.Name charges p.Count (pic (List.head results) "Best") (pic (List.last results) "Worst") rows
                        File.WriteAllText(o.["positions"], html)
                        printfn "wrote %s" o.["positions"]
                    | None -> printfn "skill has no projectiles to draw"
                elif Map.containsKey "suite" o then
                    Scenarios.standard level profile
                    |> List.map (fun (n, es) ->
                        n, (match Map.tryFind "life" o with Some l -> es |> List.map (fun e -> { e with Life = float l }) | None -> es))
                    |> Scenarios.run (int (get "runs" 200.0)) (get "time" 120.0) fight.Player [] fight.Targeting
                    |> Scenarios.format
                    |> printfn "%s"
                else
                    fight |> Summary.simulate (int (get "runs" 500.0)) |> Summary.format |> printfn "%s"
                0
