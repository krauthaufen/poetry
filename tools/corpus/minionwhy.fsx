// The modifiers a minion's skill is rolled with: the minion's (the owner's "Minions ..." mods)
// and the skill's own, by stat; and what one hit is before the enemy.
//   dotnet fsi --optimize+ tools/corpus/minionwhy.fsx <build.json> "<minion>" "<skill>" <word> [<word>...]
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let c = Setup.load read build Options.defaults
let words = args |> Array.skip 3
match c.Minions |> List.tryFind (fun m -> m.Name = args.[1] || m.Name = args.[1] + " 1") with
| None -> printfn "no minion %s among %A" args.[1] (c.Minions |> List.map (fun m -> m.Name))
| Some m ->
    match m.Skills |> List.tryFind (fun s -> s.Name.EndsWith args.[2]) with
    | None -> printfn "no skill %s among %A" args.[2] (m.Skills |> List.map (fun s -> s.Name))
    | Some s ->
        printfn "%s: tags %A" s.Name (s.Tags |> Set.toList)
        let ctx = { Ctx.empty with SkillTags = s.Tags; SkillName = s.Name }
        let expected (sk: Skill) =
            let db = ModDb.ofList (m.Mods @ sk.Mods)
            let rng = Rng 1u
            let n = 3000
            let sums = [ for _ in 1 .. n -> Hit.rollUnmitigated rng ctx db sk false ] |> List.collect Map.toList |> List.groupBy fst |> List.map (fun (t, l) -> t, (l |> List.sumBy snd) / float n)
            printfn "  %s: base %s; non-crit %s = %.0f; crit %.1f%% x%.2f; %.2fs cd %A" sk.Name
                (sk.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ")
                (sums |> List.map (fun (t, v) -> sprintf "%s %.0f" (DamageType.name t) v) |> String.concat " ") (sums |> List.sumBy snd)
                (Hit.critChance ctx db sk) (Hit.critMultiplier { ctx with Crit = true } db) sk.BaseTime sk.Cooldown
        expected s
        for p in s.Parts do expected p
        let show (source: string) (ms: Mod list) =
            for x in ms do
                let st = (sprintf "%A" x.Stat).Replace("\n", " ")
                if words |> Array.exists (fun w -> st.Contains w) then
                    printfn "  %-7s %-38s %-5A %8.1f  %-40s %s" source st x.Kind x.Value (x.Conds |> List.map (sprintf "%A") |> String.concat ",") x.Source
        show "minion" m.Mods
        show "skill" s.Mods
