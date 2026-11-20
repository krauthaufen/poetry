// Our minions against PoB's numbers for the same characters (corpus/pob-minions.jsonl, from
// `node tools/corpus/pob-minions.mjs`): level-dependent life, and damage per second of each
// minion skill for one minion. PoB's DPS is that skill used alone and all the time; ours is
// what one minion of the kind dealt with it in a 30 s boss fight, so a minion with several
// skills shows less on each. A table to find differences with, not a truth.
//   dotnet fsi --optimize+ tools/corpus/minion-parity.fsx [nnn]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let only = fsi.CommandLineArgs |> Array.skip 1 |> Array.tryHead
let rows =
    System.IO.File.ReadAllLines(root + "corpus/pob-minions.jsonl")
    |> Array.choose (fun l -> match Json.parse l with JObj m -> Some m | _ -> None)
let str k (m: Map<string, Json>) = match Map.tryFind k m with Some (JStr s) -> s | _ -> ""
let num k (m: Map<string, Json>) = match Map.tryFind k m with Some (JNum n) -> n | _ -> 0.0
printfn "%-4s %-26s %-26s %5s %5s %8s %8s %10s %10s %6s" "file" "minion" "skill" "lvl" "ours" "life" "ours" "pob dps" "ours" "ratio"
for file, rs in rows |> Array.groupBy (str "file") do
    if only |> Option.forall (fun o -> o = file) then
        let path = System.IO.Directory.GetFiles(root + "corpus", file + "-*.json").[0]
        let build, _ = Setup.parseBuild (System.IO.File.ReadAllText path)
        let o = { Options.defaults with MaxTime = 30.0 }
        let c = Setup.load read build o
        let boss = Encounter.boss 81 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
        let gem = Setup.strongest c o boss |> Option.defaultValue ""
        let out = Sim.run (Setup.fight c o (Setup.player c o (Setup.auto c o gem) None) boss) 1u
        for r in rs |> Array.distinctBy (fun r -> str "minion" r, str "skill" r) do
            let minion, skill = str "minion" r, str "skill" r
            let actors = c.Minions |> List.filter (fun m -> m.Name = minion || m.Name.StartsWith(minion + " "))
            let key = minion + ": " + skill
            let dealt = out.DamageBy |> Map.toList |> List.filter (fun (k, _) -> k.StartsWith key) |> List.sumBy snd
            let ours = if actors.IsEmpty then 0.0 else dealt / out.Duration / float actors.Length
            let life = actors |> List.tryHead |> Option.bind (fun a -> a.Body) |> Option.map (fun b -> b.Life) |> Option.defaultValue 0.0
            let pob = num "dps" r
            if pob > 0.0 || ours > 0.0 then
                printfn "%-4s %-26s %-26s %5.0f %5d %8.0f %8.0f %10.0f %10.0f %6s" file minion skill (num "level" r) actors.Length (num "life" r) life pob ours
                    (if pob > 0.0 && ours > 0.0 then sprintf "%.2f" (ours / pob) else "-")
