// The minions of a build as the simulator builds them: body (life, energy shield, armour,
// evasion, resistances, revive time) and skills. Compare with `node tools/corpus/pob-minions.mjs <nnn>`.
//   dotnet fsi --optimize+ tools/corpus/minions.fsx <build.json>
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let c = Setup.load read build Options.defaults
for m in c.Minions do
    match m.Body with
    | Some b ->
        printfn "%-28s life %6.0f es %5.0f armour %5.0f evasion %5.0f res %s %s revive %A power %.2f"
            m.Name b.Life b.EnergyShield b.Armour b.Evasion
            (b.Resist |> Map.toList |> List.map (fun (t, v) -> sprintf "%s %.0f" (DamageType.name t) v) |> String.concat " ")
            (if b.Melee then "melee" else "ranged") b.Revive b.Power
    | None -> printfn "%-28s (no body)" m.Name
    for s in m.Skills do
        printfn "    %-40s %s x%.2f, %.2fs, cd %A" s.Name (s.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ") s.BaseMultiplier s.BaseTime s.Cooldown
for n in c.MinionNotes do printfn "note: %s" n
