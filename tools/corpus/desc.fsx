// The game's description of stat ids (one per line on stdin, or as arguments).
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let db = GameDb.load (fun f -> System.IO.File.ReadAllText(root + "data/game/" + f))
let ids =
    if fsi.CommandLineArgs.Length > 1 then fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray
    else stdin.ReadToEnd().Split('\n') |> Array.map (fun l -> l.Trim()) |> Array.filter (fun l -> l <> "") |> List.ofArray
for id in ids do
    printfn "%-70s %s" id (Map.tryFind id db.Descriptions |> Option.defaultValue "?" |> fun s -> s.Replace("\n", " / "))
