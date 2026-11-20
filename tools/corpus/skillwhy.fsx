// The modifiers a skill is rolled with: the character's and the skill's own, by stat.
//   dotnet fsi --optimize+ tools/corpus/skillwhy.fsx <build.json> "<gem>" <word> [<word>...]
// A word matches the printed stat ("CritChance", "Damage", "Speed", "GainAs", "Penetration").
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let o = { Options.defaults with MaxTime = 20.0 }
let c = Setup.load read build o
let gem = args.[1]
let words = args |> Array.skip 2
let player = Setup.player c o (Setup.auto c o gem) None
match player.Skills |> List.tryFind (fun s -> s.Name = gem) with
| None -> printfn "no skill %s among %A" gem (player.Skills |> List.map (fun s -> s.Name))
| Some s ->
    printfn "%s: tags %A" s.Name (s.Tags |> Set.toList)
    printfn "  base %s x%.2f, crit %.2f%%, %.3fs, hits %A, %A" (s.BaseDamage |> Map.toList |> List.map (fun (t, r) -> sprintf "%s %.0f-%.0f" (DamageType.name t) r.Min r.Max) |> String.concat " ") s.BaseMultiplier s.BaseCrit s.BaseTime s.Hits s.Delivery
    // what one hit is before the enemy: non-crit average per type, crit chance and multiplier, with
    // what the fight starts with (charges, flags) and nothing on the enemy
    let fight = Setup.fight c o player (Encounter.boss 81 Rarity.defaults 30.0)
    let ctx = { Ctx.empty with SkillTags = s.Tags; SkillName = s.Name; Buffs = Map.ofList fight.Player.Resources }
    let expected (sk: Skill) =
        let db = ModDb.ofList (fight.Player.Mods @ sk.Mods)
        let rng = Rng 1u
        let n = 3000
        let sums = [ for _ in 1 .. n -> Hit.rollUnmitigated rng ctx db sk false ] |> List.collect Map.toList |> List.groupBy fst |> List.map (fun (t, l) -> t, (l |> List.sumBy snd) / float n)
        printfn "  %s: non-crit %s = %.0f; crit %.1f%% x%.2f" sk.Name (sums |> List.map (fun (t, v) -> sprintf "%s %.0f" (DamageType.name t) v) |> String.concat " ") (sums |> List.sumBy snd)
            (Hit.critChance ctx db sk) (Hit.critMultiplier { ctx with Crit = true } db)
    expected s
    for p in s.Parts do expected p
    let show (source: string) (ms: Mod list) =
        for m in ms do
            let st = (sprintf "%A" m.Stat).Replace("\n", " ")
            if words |> Array.exists (fun w -> st.Contains w) then
                printfn "  %-7s %-38s %-5A %8.1f  %-40s %s" source st m.Kind m.Value (m.Conds |> List.map (sprintf "%A") |> String.concat ",") m.Source
    show "char" player.Mods
    show "skill" s.Mods
    for p in s.Parts do
        printfn "part %s: base x%.2f, %.3fs, hits %A, %A" p.Name p.BaseMultiplier p.BaseTime p.Hits p.Delivery
        show "part" p.Mods
