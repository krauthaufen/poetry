// One skill at a time on a bare level-90 character with a plain weapon of the kind it needs:
// what the gem says it does, what the simulator builds from it, and what a 20 s boss fight shows.
//   dotnet fsi --optimize+ tools/skills/dossier.fsx ["Skill name" ...]     (no names: every skill)
// Writes tools/skills/dossiers.txt when run for every skill; prints otherwise.
// The point is to read each skill's description against what happens: uses per second, hits
// per use, which parts hit, what is inflicted. Anything the description says and the fight
// does not show is a gap.
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let root = __SOURCE_DIRECTORY__ + "/../../"
let read f = System.IO.File.ReadAllText(root + "data/game/" + f)
let db = GameDb.load read
let treeJson = read "tree.json"
let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray

// a plain weapon for each requirement the gems name
let weaponFor (reqs: string list) : (string * string) list =
    let has (w: string) = reqs |> List.exists (fun r -> r.Contains w)
    let item slot name = slot, sprintf "Rarity: Normal\n%s\n--------\n" name
    if reqs.IsEmpty then []
    elif has "Crossbow" then [ item "Weapon 1" "Alloy Crossbow" ]
    elif has "Bow" then [ item "Weapon 1" "Adherent Bow" ]
    elif has "Spear" then [ item "Weapon 1" "Akoyan Spear" ]
    elif has "Quarterstaff" then [ item "Weapon 1" "Aegis Quarterstaff" ]
    elif has "Talisman" then [ item "Weapon 1" "Alpha Talisman" ]
    elif has "Two Hand Mace" then [ item "Weapon 1" "Anvil Maul" ]
    elif has "Shield" then [ item "Weapon 1" "Akoyan Club"; "Weapon 2", "Rarity: Normal\nTest Shield\n--------\nArmour: 300\nBlock chance: 25%\n" ]
    elif has "Unarmed" && not (has "Martial") then []
    else [ item "Weapon 1" "Akoyan Club" ]

let dossier (name: string) : string list =
    match Gems.data db name with
    | None -> [ sprintf "#### %s: not in the data" name ]
    | Some d ->
        let reqs = Map.tryFind name db.GemVariants |> Option.defaultValue [] |> List.collect (fun g -> g.WeaponRequirements) |> List.distinct
        let reqs =
            if not reqs.IsEmpty then reqs
            else [ "Crossbow"; "Bow"; "Spear"; "Quarterstaff" ] |> List.filter d.Types.Contains |> fun l -> if l.IsEmpty && d.Types.Contains "Attack" && not (d.Types.Contains "NonWeaponAttack") then [ "One Hand Mace" ] else l
        let gem = { Name = name; Level = 20; Quality = 0; Enabled = true; IsSupport = false; StatSet = 1; Minion = None; Count = None; Corrupted = false }
        let build : PobBuild =
            { Level = 90; ClassName = "Warrior"; Ascendancy = ""; Items = weaponFor reqs; TreeNodes = []; TreeNodesSet1 = []; TreeNodesSet2 = []
              Groups = [ { Label = ""; Slot = ""; Enabled = true; Gems = [ gem ] } ]; MainGroup = 1; Config = Map.empty; TreeOverrides = Map.empty
              // enough of everything that nothing but the skill decides
              ExtraLines = [ "+5000 to maximum Mana"; "+5000 to maximum Life"; "+300 to Spirit"; "+2000 to Accuracy Rating" ] }
        let head =
            [ sprintf "#### %s   [%s]%s" name (d.Types |> Set.toList |> List.filter (fun t -> not (t.StartsWith "Usable" || t.EndsWith "able")) |> String.concat " ") (if reqs.IsEmpty then "" else "   needs: " + String.concat " / " reqs)
              "  " + d.Description
              (match Map.tryFind 20 d.Levels with
               | Some l -> sprintf "  level 20: cost %A (per minute %A), cooldown %A x%A, multiplier %A, crit %A" l.ManaCost l.ManaPerMinute l.Cooldown l.StoredUses l.BaseMultiplier l.CritChance
               | None -> "") ]
            @ (d.StatSets |> List.map (fun s ->
                let consts = s.ConstantStats |> List.filter (fun (k, _) -> not (k.StartsWith "movement_speed")) |> List.map (fun (k, v) -> sprintf "%s=%g" k v)
                sprintf "  set %-16s %s%s" s.Label (String.concat " " consts) (if s.Stats.IsEmpty then "" else "  | " + String.concat " " (s.Stats |> List.filter (fun k -> not (k.Contains "display") && not (k.StartsWith "projectile_uses") && not (k.StartsWith "should_use") && not (k.StartsWith "check_for"))))))
        try
            let o = { Options.defaults with MaxTime = 20.0 }
            let c = Setup.loadWith db treeJson build o
            let play = if List.contains name (Setup.pressable c) && (Setup.pressed c o name).IsSome then Setup.auto c o name else []
            let player = Setup.player c o play None
            let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
            let out = Sim.run (Setup.fight c o player boss) 1u
            let built =
                player.Skills |> List.collect (fun s ->
                    (sprintf "  built %-34s x%.2f, %.2fs, cd %A x%d, cost %.0f +%.0f/s, hits %A, %s" s.Name s.BaseMultiplier s.BaseTime s.Cooldown s.StoredUses s.Cost s.CostPerSecond s.Hits
                        (match s.Delivery with Single -> "single" | AreaAtTarget r -> sprintf "area %.0f" r | Cone (r, a, _) -> sprintf "cone %.0f, %.0f degrees" r a | AreaAroundSelf r -> sprintf "around self %.0f" r | Projectiles p -> sprintf "%d projectiles, pierce %d, chain %d" p.Count p.Pierce p.Chains))
                    :: (s.Parts |> List.map (fun p -> sprintf "        part %-28s x%.2f, hits %A" p.Name p.BaseMultiplier p.Hits)))
            let total = out.DamageBy |> Map.toList |> List.sumBy snd
            let counts = out.Counts |> Map.toList |> List.filter (fun (k, _) -> not (k.StartsWith "enemy:" || k.StartsWith "player:" || k.StartsWith "expired" || k.StartsWith "flask")) |> List.map (fun (k, v) -> sprintf "%s %d" k v)
            head
            @ [ sprintf "  play: %s%s" (play |> List.map (function Spam g -> "spam " + g | OnCooldown g -> "cd " + g | KeepBuff g -> "buff " + g | KeepCurse g -> "curse " + g | _ -> "?") |> String.concat "; ") (if play.IsEmpty then "(cannot be pressed)" else "") ]
            @ built
            @ (c.Minions |> List.map (fun m -> sprintf "  minion %s: %s" m.Name (m.Skills |> List.map (fun s -> s.Name) |> String.concat ", ")))
            @ (c.MinionNotes |> List.map (fun n -> "  note: " + n))
            @ [ sprintf "  fight: %.0f dps; %s" (total / max 0.1 out.Duration) (out.DamageBy |> Map.toList |> List.sortByDescending snd |> List.map (fun (k, v) -> sprintf "%s %.0f%%" k (100.0 * v / max 1.0 total)) |> String.concat ", ")
                "         " + String.concat ", " counts ]
            @ (match Gems.unmodelledTypes db name with [] -> [] | ts -> [ "  types without a model: " + String.concat ", " ts ])
        with e -> head @ [ "  FAILED: " + e.Message.Split('\n').[0] ]

let names =
    if not args.IsEmpty then args
    else db.Gems |> Map.toList |> List.choose (fun (n, g) -> match Gems.data db n with Some d when not d.Support && not d.Hidden && not g.Support -> Some n | _ -> None) |> List.distinct |> List.sort
let text = names |> List.collect (fun n -> dossier n @ [ "" ])
if args.IsEmpty then
    System.IO.File.WriteAllLines(__SOURCE_DIRECTORY__ + "/dossiers.txt", text)
    printfn "%d skills written" names.Length
else text |> List.iter (printfn "%s")
