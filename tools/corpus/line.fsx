// How a line of item / passive text is understood: stat ids and their meaning.
//   dotnet fsi tools/corpus/line.fsx "<line>" ["<line>"...]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let db = GameDb.load (fun f -> System.IO.File.ReadAllText(root + "data/game/" + f))
for line in fsi.CommandLineArgs |> Array.skip 1 do
    printfn "%s" line
    for ctx, name in [ StatText.Global, "global"; StatText.ItemLocal, "item" ] do
        let stats, unknown = StatText.parseLinesIn ctx db.Text [ line ]
        for id, v in stats do printfn "  [%s] %s = %g -> %s" name id v ((sprintf "%A" (StatMap.meaning id v)).Replace("\n", " ") |> fun s -> if s.Length > 300 then s.Substring(0, 300) else s)
        for u in unknown do printfn "  [%s] NOT UNDERSTOOD: %s" name u
