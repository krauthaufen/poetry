#r "../../src/PoEsie/bin/Release/net8.0/PoEsie.dll"
open PoEsie
let dir = __SOURCE_DIRECTORY__ + "/../../data/game/"
let read f = System.IO.File.ReadAllText(dir + f)
let index = StatText.build [ read "statdesc_stat_descriptions.json"; read "statdesc_passive_skill_stat_descriptions.json" ]
let mods = match Json.parse (read "mods.json") with JObj m -> m |> Map.toList |> List.map (fun (_, j) -> Json.list (Json.field "lines" j) |> List.choose Json.str) | _ -> []
let tree = match Json.field "nodes" (Json.parse (read "tree.json")) with Some (JObj n) -> n |> Map.toList |> List.map (fun (_, j) -> Json.list (Json.field "stats" j) |> List.choose Json.str) | _ -> []
let ids = (mods @ tree) |> List.collect (StatText.parseLines index >> fst) |> List.map fst |> List.countBy id |> List.sortByDescending snd
System.IO.File.WriteAllLines(__SOURCE_DIRECTORY__ + "/statids.txt", ids |> List.map (fun (k, n) -> sprintf "%d %s" n k))
printfn "%d distinct stat ids" ids.Length
// meaning coverage, weighted by how often a stat occurs
let classify (id, n) =
    match StatMap.meaning id 10.0 with
    | Mods _ -> "mods", id, n
    | Local _ -> "local", id, n
    | NotOffensive -> "not-offensive", id, n
    | Unsimulated m -> "unsimulated", id, n
    | Unmapped -> "unmapped", id, n
let cls = ids |> List.map classify
cls |> List.groupBy (fun (c, _, _) -> c) |> List.iter (fun (c, xs) -> printfn "%-14s %5d ids, %5d occurrences" c xs.Length (xs |> List.sumBy (fun (_, _, n) -> n)))
printfn "top unmapped:"
cls |> List.filter (fun (c, _, _) -> c = "unmapped") |> List.truncate 60 |> List.iter (fun (_, id, n) -> printf "%s(%d) " id n)
printfn ""
