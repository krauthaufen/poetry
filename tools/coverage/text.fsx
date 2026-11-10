// How much item / passive text the reverse parser understands.
// dotnet fsi tools/coverage/text.fsx
#r "../../src/PoEsie/bin/Release/net8.0/PoEsie.dll"
open PoEsie
let dir = __SOURCE_DIRECTORY__ + "/../../data/game/"
let read f = System.IO.File.ReadAllText(dir + f)
let sw = System.Diagnostics.Stopwatch.StartNew()
let index = StatText.build [ read "statdesc_stat_descriptions.json"; read "statdesc_passive_skill_stat_descriptions.json" ]
printfn "index built in %dms" sw.ElapsedMilliseconds

let lines name (json: Json) pick =
    match json with
    | JObj m -> m |> Map.toList |> List.collect (snd >> pick) |> List.distinct
    | _ -> []
let modLines = lines "mods" (Json.parse (read "mods.json")) (fun j -> Json.list (Json.field "lines" j) |> List.choose Json.str)
let treeNodes =
    match Json.field "nodes" (Json.parse (read "tree.json")) with
    | Some (JObj n) -> n |> Map.toList |> List.map (fun (_, j) -> Json.list (Json.field "stats" j) |> List.choose Json.str)
    | _ -> []

let report name (ls: string list) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let failed = ls |> List.filter (fun l -> l.Split('\n') |> Array.exists (fun p -> (StatText.parse index p).IsNone))
    printfn "%s: %d lines, %d parsed (%.1f%%), %.1f us/line" name ls.Length (ls.Length - failed.Length)
        (100.0 * float (ls.Length - failed.Length) / float ls.Length) (float sw.ElapsedMilliseconds * 1000.0 / float ls.Length)
    failed |> List.truncate 25 |> List.iter (printfn "   x %s")

report "affix mods" modLines
let treeUnknown = treeNodes |> List.collect (StatText.parseLines index >> snd) |> List.distinct
let treeTotal = treeNodes |> List.concat |> List.distinct
printfn "passive tree: %d distinct lines, %d not understood (%.1f%%)" treeTotal.Length treeUnknown.Length (100.0 * float treeUnknown.Length / float treeTotal.Length)
treeUnknown |> List.truncate 40 |> List.iter (printfn "   x %s")
