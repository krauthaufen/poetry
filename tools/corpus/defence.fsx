// What a corpus character can take while it fights: the largest hit of each type
// (`Sim.defences`), every half second of a 20 s boss fight, beside the same from the
// character sheet alone (`Defence.maxHit`); then what a boss has to throw to kill it in
// 10 s, found by fighting (`Report.killing`).
//   dotnet fsi --optimize+ tools/corpus/defence.fsx <build.json> [gem to press]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let o = { Options.defaults with MaxTime = 20.0 }
let c = Setup.load read build o
let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
let gem = if args.Length > 1 then args.[1] else (Setup.strongest c o boss |> Option.defaultValue "")
let player = Setup.player c o (Setup.auto c o gem) None
let out, samples = Sim.defences 0.5 (Setup.fight c o player boss) 1u
let show (x: float) = if System.Double.IsInfinity x then "    immune" else sprintf "%10.0f" x
printfn "pressed %s; the boss hits for %s" gem (boss |> List.tryPick (fun e -> e.Attack) |> Option.map (fun a -> sprintf "%.0f every %.2f s" (a.Damage |> Map.toList |> List.sumBy snd) a.Interval) |> Option.defaultValue "nothing")
printfn "%-6s %s" "" (DamageType.all |> List.map (fun t -> sprintf "%10s" (DamageType.name t)) |> String.concat "")
printfn "%-6s %s" "sheet" (DamageType.all |> List.map (fun t -> show (Defence.maxHit c.Vitals t)) |> String.concat "")
for s in samples do
    printfn "%6.1f %s" s.Time (DamageType.all |> List.map (fun t -> show s.MaxHit.[t]) |> String.concat "")
printfn "died %A, damage taken %.0f" out.Died out.DamageTaken
let sw = System.Diagnostics.Stopwatch.StartNew()
for t, r in Report.killing 10.0 (Setup.fight c o player (Encounter.boss 83 Rarity.defaults 30.0)) do
    match r with
    | Some (thrown, hit) -> printfn "killed in 10 s by %10.0f %s damage thrown, in hits of %.0f" thrown (DamageType.name t) hit
    | None -> printfn "not killed by %s damage" (DamageType.name t)
printfn "%.1f s" sw.Elapsed.TotalSeconds
