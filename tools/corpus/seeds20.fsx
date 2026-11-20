// Mean DPS of one build over 20 seeds, the way the corpus run plays it, optionally without
// the character modifiers of some names (to find what moved a build).
//   dotnet fsi --optimize+ tools/corpus/seeds20.fsx <build.json> [Named stat to drop,...]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let drop = if args.Length > 1 then args.[1].Split(',') |> Set.ofArray else Set.empty
let o = { Options.defaults with MaxTime = 20.0 }
let c0 = Setup.load read build o
let c = { c0 with Sheet = { c0.Sheet with Mods = c0.Sheet.Mods |> List.filter (fun m -> match m.Stat with Named n -> not (drop.Contains n) | _ -> true) } }
let boss = Encounter.boss 81 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
let gem = Setup.strongest c o boss |> Option.defaultValue ""
let player = Setup.player c o (Setup.auto c o gem) None
let fight = Setup.fight c o player boss
let dps = [ for s in 1u .. 20u -> let out = Sim.run fight s in (out.DamageBy |> Map.toList |> List.sumBy snd) / max 0.1 out.Duration ]
printfn "%s: mean %.0f over 20 seeds (min %.0f, max %.0f)" gem (List.average dps) (List.min dps) (List.max dps)
