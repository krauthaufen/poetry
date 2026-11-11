// Every skill gem a player can use, with what the simulator does not read of it.
//   dotnet fsi --optimize+ tools/skills/ledger.fsx      (after building src/PoEtry in Release)
// Per skill: its stat sets, the skill types the engine has no model for, and the stats of the
// skill that are unmapped, mapped as "not simulated", or not read anywhere. A skill with none of
// these is a plain one: it hits, and the engine has all it needs. The others are the work list.
// With a corpus (corpus/*.json) the list is ordered by how many characters use the skill.
// Writes tools/skills/skills.txt.
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let root = __SOURCE_DIRECTORY__ + "/../../"
let read f = System.IO.File.ReadAllText(root + "data/game/" + f)
let db = GameDb.load read

// string literals in the engine: a stat is read raw when its id, or a part the engine matches by, is there
let engine =
    System.IO.Directory.GetFiles(root + "src/PoEtry", "*.fs")
    |> Array.filter (fun f -> let n = System.IO.Path.GetFileName f in n <> "StatMap.fs" && n <> "StatText.fs" && n <> "Tables.fs" && n <> "RarityData.fs")
    |> Array.map System.IO.File.ReadAllText |> String.concat "\n"
let literals =
    System.Text.RegularExpressions.Regex.Matches(engine, "\"([a-z0-9_+%\\-]{6,})\"") |> Seq.map (fun m -> m.Groups.[1].Value) |> Set.ofSeq
let rawRead (id: string) =
    literals.Contains id
    || literals |> Set.exists (fun l -> (l.StartsWith "_" && id.EndsWith l) || (l.EndsWith "_" && id.StartsWith l))

// how many corpus characters have the gem
let usage =
    let dir = root + "corpus"
    if not (System.IO.Directory.Exists dir) then Map.empty
    else
        System.IO.Directory.GetFiles(dir, "*.json") |> Array.filter (fun f -> System.Char.IsDigit (System.IO.Path.GetFileName f).[0])
        |> Array.collect (fun f ->
            try
                let build, _ = Setup.parseBuild (System.IO.File.ReadAllText f)
                build.Groups |> List.collect (fun g -> g.Gems |> List.filter (fun x -> not x.IsSupport) |> List.map (fun x -> x.Name)) |> List.distinct |> Array.ofList
            with _ -> [||])
        |> Array.countBy id |> Map.ofArray

// stats that only describe how a skill looks or is aimed
let cosmetic (id: string) =
    [ "movement_speed_"; "visual_"; "display_"; "quality_display"; "should_use_"; "precise_cursor"; "projectile_uses_contact"; "check_for_targets"; "can_perform_skill_while_moving"
      "action_"; "maintain_projectile"; "has_modular_projectiles"; "base_is_projectile"; "is_area_damage"; "skill_can_fire_arrows"; "cannot_cancel"; "ground_"; "art_variation"
      "minimum_projectile_target"; "melee_conditional_step"; "active_skill_override_turn"; "projectile_maximum_downwards"; "base_skill_show_average" ]
    |> List.exists id.Contains

type Row = { Name: string; Used: int; Sets: string list; Types: string list; Unmapped: string list; Unsimulated: string list; Description: string }

let rows =
    db.Gems |> Map.toList
    |> List.choose (fun (name, g) ->
        match Gems.data db name with
        | Some d when not d.Support && not d.Hidden && not g.Support ->
            let ids = d.StatSets |> List.collect (fun s -> s.Stats @ (s.ConstantStats |> List.map fst)) |> List.distinct |> List.filter (cosmetic >> not)
            let classify (id: string) =
                if rawRead id then None
                else
                    match StatMap.meaning id 1.0 with
                    | Unmapped -> Some(true, id)
                    | Unsimulated _ -> Some(false, id)
                    | _ -> None
            let flagged = ids |> List.choose classify
            Some { Name = name; Used = Map.tryFind name usage |> Option.defaultValue 0
                   Sets = d.StatSets |> List.map (fun s -> s.Label)
                   Types = Gems.unmodelledTypes db name
                   Unmapped = flagged |> List.filter fst |> List.map snd
                   Unsimulated = flagged |> List.filter (fst >> not) |> List.map snd
                   Description = d.Description }
        | _ -> None)
    |> List.distinctBy (fun r -> r.Name)

let plain, intricate = rows |> List.partition (fun r -> r.Types.IsEmpty && r.Unmapped.IsEmpty && r.Unsimulated.IsEmpty)
let out = System.Text.StringBuilder()
let say (s: string) = out.AppendLine s |> ignore
say (sprintf "%d skills: %d with nothing unread, %d with something the engine does not read" rows.Length plain.Length intricate.Length)
say ""
say "== to do (most used in the corpus first)"
for r in intricate |> List.sortBy (fun r -> -r.Used, r.Name) do
    say (sprintf "%-28s used by %2d  sets: %s" r.Name r.Used (String.concat ", " r.Sets))
    say (sprintf "    %s" r.Description)
    if not r.Types.IsEmpty then say (sprintf "    types without a model: %s" (String.concat ", " r.Types))
    if not r.Unmapped.IsEmpty then say (sprintf "    unmapped: %s" (String.concat ", " r.Unmapped))
    if not r.Unsimulated.IsEmpty then say (sprintf "    not simulated: %s" (String.concat ", " r.Unsimulated))
say ""
say "== nothing unread"
say (plain |> List.sortBy (fun r -> r.Name) |> List.map (fun r -> r.Name) |> String.concat ", ")
System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/skills.txt", out.ToString())
printfn "%s" (out.ToString().Split('\n').[0])
