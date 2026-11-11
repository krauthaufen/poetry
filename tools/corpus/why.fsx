// Where a character-sheet value comes from: every mod of the named stats, with its source.
//   dotnet fsi tools/corpus/why.fsx <build.json> <word> [<word>...]
// A word matches the printed stat ("Str", "Armour", "Resist Cold", "MaxLife").
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let text = System.IO.File.ReadAllText args.[0]
let build, _ = Setup.parseBuild text
let c = Setup.load read build Options.defaults
let words = args |> Array.skip 1
for m in c.Sheet.Mods do
    let s = sprintf "%A" m.Stat
    if words |> Array.exists (fun w -> s.Contains w) then
        printfn "%-34s %-5A %8.1f  %-30s %s" (s.Replace("\n", " ")) m.Kind m.Value (m.Conds |> List.map (sprintf "%A") |> String.concat ",") m.Source
for slot, st, v in c.Sheet.ItemDefences do
    let s = sprintf "%A" st
    if words |> Array.exists (fun w -> s.Contains w) then printfn "%-34s item  %8.1f  %s" s v slot
let sdb = ModDb.ofList c.Sheet.Mods
for st in c.Sheet.Mods |> List.map (fun m -> m.Stat) |> List.distinct do
    let s = sprintf "%A" st
    if words |> Array.exists (fun w -> s.Contains w) then
        printfn "TOTAL %s: base %.1f, inc %.0f%%, more x%.3f (no conditions)" (s.Replace("\n", " ")) (Mods.sumBase Ctx.empty [ st ] sdb) (Mods.sumInc Ctx.empty [ st ] sdb) (Mods.more Ctx.empty [ st ] sdb)
let v = c.Vitals
printfn "ours: str %.0f dex %.0f int %.0f life %.0f mana %.0f es %.0f armour %.0f evasion %.0f deflection %.0f block %.0f ward %.0f res %A" v.Str v.Dex v.Int v.Life v.Mana v.EnergyShield v.Armour v.Evasion v.Deflection v.Block v.Ward (v.Resist |> Map.toList)
match Json.field "ninja" (Json.parse text) |> Option.bind (Json.field "defensiveStats") with
| Some (JObj m) -> printfn "theirs: %s" (m |> Map.toList |> List.choose (fun (k, j) -> Json.num j |> Option.map (fun x -> sprintf "%s %.0f" k x)) |> String.concat ", ")
| _ -> ()
let cov = c.Sheet.Coverage
printfn "unknown text:"; cov.UnknownText |> List.iter (printfn "  %s")
printfn "unsimulated:"; cov.Unsimulated |> List.iter (fun (s, m) -> printfn "  %s [%s]" s m)
printfn "unmapped:"; cov.Unmapped |> List.iter (fun (s, v) -> printfn "  %s = %g" s v)
