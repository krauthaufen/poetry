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
///   --without NAMES  drop these support gems (comma separated) from the simulated group
///   --spread DEG     override the projectile fan angle of the simulated skill
///   --shotgun        let several projectiles of one use hit the same enemy
///   --layouts F      random boss and pack layouts (sizes, distances, slam reach, arena walls);
///                    write an HTML page with best / median / worst pictures to file F
///   --count N        number of layouts per scenario (default 40)
///   --immortal       ignore incoming damage and mana costs
///   --start S        full (default: charges, rage, motes at maximum) | empty (only power charges and rage)
///   --chargegen S    seconds between Thaumaturgical Dynamism charges (default 5)
///   --max K=N,...    override resource maxima with observed ones, e.g. "Mote:blue=0"
///   --noflask        don't use the mana flask
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
    | "--immortal" :: rest -> Map.add "immortal" "1" (opts rest)
    | "--noflask" :: rest -> Map.add "noflask" "1" (opts rest)
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
        let bases = Defence.classBases (File.ReadAllText(Path.Combine(dataDir, "tree.json")))
        // motes: base maximum from the mote-granting gem's description stat
        let moteBase =
            build.Groups
            |> List.tryPick (fun g ->
                g.Gems
                |> List.tryPick (fun gem ->
                    if gem.IsSupport then None
                    else
                        let raw = Gems.rawStats db (ModDb.ofList sheet.Mods) { Gem = gem.Name; Level = gem.Level; Quality = gem.Quality; StatSet = 1; Supports = [] }
                        if raw |> List.exists (fun (k, _) -> k = "base_active_skill_buff_stack_gain_frequency_ms") then
                            raw |> List.tryPick (fun (k, v) -> if k = "skill_specific_stat_description_mode" then Some(int v) else None)
                        else None))
        let moteMax = match moteBase with Some b -> Behaviours.moteMaxima db build b | None -> []
        // observed maxima override the computed ones: --max "Mote:blue=0,Mote:red=9"
        let overrides =
            (Map.tryFind "max" o |> Option.defaultValue "").Split(',')
            |> Array.choose (fun kv -> match kv.Split('=') with | [| k; v |] -> Some(k.Trim(), int v) | _ -> None)
            |> List.ofArray
        let maxResources = (Pob.maxResources sheet @ moteMax @ overrides) |> Map.ofList
        let startResources =
            maxResources
            |> Map.map (fun k n ->
                match k, Map.tryFind "charges" o, Map.tryFind "start" o with
                | "PowerCharge", Some c, _ -> int c
                | _, _, Some "empty" when k <> "PowerCharge" && k <> "Rage" -> 0
                | _ -> n)
        let vitals = Defence.computeWith startResources bases sheet
        let periodics = Behaviours.periodics db sheet build (Map.tryFind "chargegen" o |> Option.map float |> Option.defaultValue 5.0)
        for p in periodics do printfn "periodic: %s every %.1fs" p.Name p.Every
        if not moteMax.IsEmpty then printfn "motes: %s" (moteMax |> List.map (fun (k, n) -> sprintf "%s max %d" k n) |> String.concat ", ")
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
        let resources = startResources |> Map.toList

        printfn "== %s %s, level %d: %d items, %d passives, %d socket groups" build.ClassName build.Ascendancy build.Level build.Items.Length build.TreeNodes.Length build.Groups.Length
        printfn "player: life %.0f, mana %.0f, ES %.0f | armour %.0f, evasion %.0f, deflection %.0f | res %s | regen life %.0f/s, mana %.0f/s"
            vitals.Life vitals.Mana vitals.EnergyShield vitals.Armour vitals.Evasion vitals.Deflection
            (vitals.Resist |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f" (DamageType.name t) r) |> String.concat " / ") vitals.LifeRegen vitals.ManaRegen
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
            let dropped = (Map.tryFind "without" o |> Option.defaultValue "").Split(',') |> Array.map (fun x -> x.Trim()) |> Array.filter ((<>) "") |> Set.ofArray
            let group = { group with Gems = group.Gems |> List.filter (fun g -> not (dropped.Contains g.Name)) }
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
                let rec withSpread deg (s: Skill) =
                    { s with
                        Delivery = (match s.Delivery with Projectiles p -> Projectiles { p with Spread = deg } | d -> d)
                        Parts = s.Parts |> List.map (withSpread deg) }
                let skill = match Map.tryFind "spread" o with Some d -> withSpread (float d) skill | None -> skill
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
                      Player =
                        { Actor.make "Player" sheet.Mods [ skill ] with
                            Resources = resources
                            Cycles = cycles
                            Vitals = (if Map.containsKey "immortal" o then None else Some vitals)
                            VitalsAt = Some(fun current -> Defence.computeWith current bases sheet)
                            Periodics = periodics
                            MaxResources = maxResources
                            ManaFlask =
                                if Map.containsKey "immortal" o || Map.containsKey "noflask" o then None
                                else
                                    sheet.ManaFlask
                                    |> Option.map (fun ((amount, duration, perUse, maxC), inc) ->
                                        { Amount = amount
                                          Duration = duration
                                          ChargesPerUse = perUse
                                          MaxCharges = maxC
                                          ChargesPerSecond = Mods.sumBase Ctx.empty [ Named "FlaskChargesPerSecond" ] (ModDb.ofList sheet.Mods)
                                          ChargeGainMult = 1.0 + inc / 100.0 }) }
                      Minions = []
                      Targeting = (if Map.tryFind "target" o = Some "nearest" then Nearest else Strongest)
                      Reach = get "reach" 15.0
                      Walls = []
                      MaxTime = get "time" 60.0 }
                printfn ""
                if Map.containsKey "layouts" o then
                    let projectilePart =
                        (skill :: skill.Parts) |> List.tryPick (fun s -> match s.Delivery with Projectiles p -> Some p | _ -> None)
                    let charges = resources |> List.tryPick (fun (k, n) -> if k = "PowerCharge" then Some n else None) |> Option.defaultValue 0
                    let terrain = Mods.sumBase Ctx.empty [ Named "TerrainChain" ] (ModDb.ofList (sheet.Mods @ (skill.Parts |> List.collect (fun p -> p.Mods))))
                    let count = int (get "count" 40.0)
                    let runs = int (get "runs" 20.0)
                    let rotate (a: float) (v: Vec) = Vec.make (v.X * cos a - v.Y * sin a) (v.X * sin a + v.Y * cos a)
                    /// A random arena around `center` that encloses the player and every monster.
                    let arena (rng: Rng) (center: Vec) (enemies: EnemyDef list) =
                        let reachOut = (Vec.length center :: (enemies |> List.map (fun e -> Vec.dist center e.Position + e.Size))) |> List.max
                        match int (rng.Float() * 3.0) with
                        | 0 -> "open", Arena.openField
                        | 1 ->
                            let r = reachOut + 10.0 + rng.Float() * 60.0
                            sprintf "round room r%.0f" r, Arena.round center r
                        | _ ->
                            // corridor along player -> center; wide enough for everyone
                            let d = Vec.scale (1.0 / max 1e-9 (Vec.length center)) center
                            let n = Vec.make -d.Y d.X
                            let lateral = enemies |> List.map (fun e -> abs (e.Position.X * n.X + e.Position.Y * n.Y) + e.Size) |> List.fold max 5.0
                            let w = 2.0 * lateral + 10.0 + rng.Float() * 50.0
                            sprintf "corridor w%.0f" w, Arena.corridor center w 400.0
                    let simulate (enemies: EnemyDef list) walls reach maxTime =
                        Summary.simulate runs { fight with Enemies = enemies; Walls = walls; Reach = reach; MaxTime = maxTime }
                    let bossLayouts =
                        [ for i in 1..count ->
                            let rng = Rng(uint32 (1000 + i))
                            let size = 6.0 + rng.Float() * 19.0
                            let gap = rng.Float() * 40.0
                            let reach = 8.0 + rng.Float() * 17.0
                            let angle = rng.Float() * 2.0 * System.Math.PI
                            let boss =
                                { (List.head (Encounter.boss level profile 0.0)) with
                                    Size = size
                                    Position = rotate angle (Vec.make (gap + size) 0.0)
                                    Life = get "life" 1e12 }
                            let arenaName, walls = arena rng boss.Position [ boss ]
                            let sum = simulate [ boss ] walls reach 20.0
                            let label = sprintf "boss r%.0f, gap %.0f, reach %.0f, %s" size gap reach arenaName
                            (label, [ boss ], walls, reach), sum.Dps.Mean, (sprintf "%.1fM DPS" (sum.Dps.Mean / 1e6)) ]
                        |> List.sortByDescending (fun (_, v, _) -> v)
                    let packLayouts =
                        [ for i in 1..count ->
                            let rng = Rng(uint32 (2000 + i))
                            let around = rng.Float() < 0.3
                            let distance = (if around then 15.0 else 25.0) + rng.Float() * 35.0
                            let rares = int (rng.Float() * 3.0)
                            let enemies =
                                Encounter.pack level profile [ Normal, 8; Magic, 3; Rare, rares ] distance (10.0 + rng.Float() * 20.0) around (uint32 i)
                            let center = Vec.make (enemies |> List.averageBy (fun e -> e.Position.X)) (enemies |> List.averageBy (fun e -> e.Position.Y))
                            let arenaName, walls = arena rng center enemies
                            let reach = 8.0 + rng.Float() * 17.0
                            let sum = simulate enemies walls reach 30.0
                            let clear = sum.Ttk |> Option.map (fun t -> t.Mean) |> Option.defaultValue infinity
                            let label = sprintf "%s pack, %d rares, at %.0f, reach %.0f, %s" (if around then "surrounding" else "frontal") rares distance reach arenaName
                            (label, enemies, walls, reach), -clear, (if clear < infinity then sprintf "cleared in %.2fs" clear else "not cleared in 30s") ]
                        |> List.sortByDescending (fun (_, v, _) -> v)
                    match projectilePart with
                    | None -> printfn "skill has no projectiles to draw"
                    | Some p ->
                        let p = { p with Count = p.Count + (match p.PerCharge with Some (_, per) -> per * charges | None -> 0); TerrainChain = p.TerrainChain + terrain }
                        let pick (xs: 'a list) = [ "Best", List.head xs; "Median", xs.[xs.Length / 2]; "Worst", List.last xs ]
                        let section title (xs: ((string * EnemyDef list * Geometry.Wall list * float) * float * string) list) =
                            let pics =
                                pick xs
                                |> List.mapi (fun k (tag, ((label, enemies, walls, reach), _, result)) ->
                                    let primary = Geometry.aim Strongest enemies |> Option.get
                                    Picture.svg (sprintf "<b>%s</b>: %s. %s" tag result label) (uint32 (k + 7)) walls reach p primary enemies)
                                |> String.concat ""
                            let rows =
                                xs |> List.map (fun ((label, _, _, _), _, result) -> sprintf "<tr><td>%s</td><td>%s</td></tr>" label result) |> String.concat ""
                            sprintf "<h2>%s</h2><div class='pics'>%s</div><details><summary>All %d layouts</summary><table><tbody>%s</tbody></table></details>" title pics xs.Length rows
                        printfn "boss layouts: best %s, median %s, worst %s"
                            (let (_, _, r) = List.head bossLayouts in r) (let (_, _, r) = bossLayouts.[bossLayouts.Length / 2] in r) (let (_, _, r) = List.last bossLayouts in r)
                        printfn "pack layouts: best %s, median %s, worst %s"
                            (let (_, _, r) = List.head packLayouts in r) (let (_, _, r) = packLayouts.[packLayouts.Length / 2] in r) (let (_, _, r) = List.last packLayouts in r)
                        let html =
                            sprintf """<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>%s layouts</title>
<style>
:root{--bg:#fbfaf7;--fg:#1d1d1b;--muted:#6b6a66;--line:#e2dfd8;--enemy:#c9c3b6;--hit:#c2410c;--miss:#b9b4aa;--player:#1d4ed8;--wall:#57534e}
@media (prefers-color-scheme:dark){:root{--bg:#161614;--fg:#ecebe7;--muted:#a3a199;--line:#34332f;--enemy:#4a4740;--hit:#fb923c;--miss:#55524b;--player:#60a5fa;--wall:#a8a29e}}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 system-ui,sans-serif}
main{max-width:1100px;margin:0 auto;padding:24px 16px}
h1{font-size:22px;margin:0 0 4px}h2{font-size:18px;margin:28px 0 10px}p{color:var(--muted);margin:0 0 16px}
.pics{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:14px}
figure{margin:0;border:1px solid var(--line);border-radius:10px;padding:10px}
svg{width:100%%;height:auto;aspect-ratio:1}
figcaption{font-size:13px;color:var(--muted);margin-top:6px}figcaption b{color:var(--fg)}
.enemy{fill:var(--enemy)}.hit{stroke:var(--hit);stroke-width:.8;fill:none}.miss{stroke:var(--miss);stroke-width:.4;stroke-dasharray:2 2;fill:none}
.wall{stroke:var(--wall);stroke-width:1.6}.player{fill:var(--player)}.impact{stroke:var(--fg);stroke-width:.8;fill:none}
details{margin-top:10px}summary{cursor:pointer;color:var(--muted)}
table{border-collapse:collapse;width:100%%;font-size:14px;font-variant-numeric:tabular-nums}td{padding:3px 8px;border-bottom:1px solid var(--line)}td:last-child{text-align:right}
</style></head><body><main>
<h1>%s: random layouts</h1>
<p>%d power charges: %d projectiles per slam, %s from the impact point (×), %.0f%% chance to chain once from a wall (towards the nearest enemy within chain range it has not hit; mirror bounce if none). Player in blue, monsters grey, walls dark. Orange flights hit something, dashed ones miss. Each layout is simulated %d times; pictures show one slam. Boss radii are assumptions (game units, 10 = 1 m).</p>
%s%s
</main></body></html>"""
                                skill.Name skill.Name charges p.Count (if p.Spread >= 360.0 then "nova" else sprintf "%.0f° fan forwards" p.Spread) p.TerrainChain runs
                                (section "Single boss (sustained DPS)" bossLayouts) (section "Packs (8 normal, 3 magic, 0–2 rares, real life)" packLayouts)
                        File.WriteAllText(o.["layouts"], html)
                        printfn "wrote %s" o.["layouts"]
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
