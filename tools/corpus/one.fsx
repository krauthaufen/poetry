// One build the way the corpus run plays it: the play style `Setup.auto` derives, a 30 s
// boss fight, what dealt the damage and what happened.
//   dotnet fsi --optimize+ tools/corpus/one.fsx <build.json> [gem to press]
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
let play = Setup.auto c o gem
printfn "pressable: %s" (Setup.pressable c |> String.concat ", ")
printfn "play: %s" (play |> List.map (function Spam g -> "spam " + g | OnCooldown g -> "on cooldown " + g | KeepBuff g -> "keep buff " + g | KeepCurse g -> "keep curse " + g | _ -> "?") |> String.concat "; ")
let player = Setup.player c o play None
for s in player.Skills do
    printfn "  skill %-40s base %s x%.2f, %.2fs, cd %A, cost %.0f, %A%s" s.Name (s.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ") s.BaseMultiplier s.BaseTime s.Cooldown s.Cost (match s.Delivery with Projectiles p -> sprintf "%d projectiles" p.Count |> box | d -> box d) (if s.Hits.IsEmpty then " (no hits)" else "")
let out = Sim.run (Setup.fight c o player boss) 1u
printfn "dps %.0f over %.1fs" ((out.DamageBy |> Map.toList |> List.sumBy snd) / max 0.1 out.Duration) out.Duration
out.DamageBy |> Map.toList |> List.sortByDescending snd |> List.iter (fun (k, v) -> printfn "  %-50s %12.0f" k v)
out.Counts |> Map.toList |> List.iter (fun (k, v) -> printfn "  %-50s %6d" k v)
