// One build fought with a rotation the way the page asks for it (`Report.prepare`): a
// priority list, entries apart by "|", each "Gem" or "Gem@conditions" (see RULES.md, "Rotation").
// Prints what is pressed, the DPS over some seeds, what dealt the damage, and of the first
// fight what happened on the ground and which skills were used when.
//   dotnet fsi --optimize+ tools/corpus/rotation.fsx <build.json> "<rotation>" [scenario] [seconds] [life]
//   ("": the play style `Setup.auto` derives; scenario: boss (default), pinnacle, pack, surrounded;
//    life: of every enemy, 0 for their own)
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry
let root = __SOURCE_DIRECTORY__ + "/../../"
let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
let args = fsi.CommandLineArgs |> Array.skip 1
let arg i d = if args.Length > i then args.[i] else d
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText args.[0])
let seconds = float (arg 3 "30")
let c = Setup.load read build { Options.defaults with MaxTime = seconds }
let quote (s: string) = "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
let request =
    Json.parse (sprintf "{\"rotation\":%s,\"scenario\":%s,\"time\":%s,\"life\":%s,\"runs\":20}" (quote (arg 1 "")) (quote (arg 2 "boss")) (arg 3 "30") (arg 4 "0"))
let states, skills = Setup.conditions c Options.defaults
printfn "pressable: %s" (Setup.pressable c |> String.concat ", ")
printfn "states: %s" (String.concat ", " states)
printfn "standing: %s" (String.concat ", " skills)
let ready = Report.prepare c request
let result = Report.fought ready request
for step in Json.field "play" result |> Json.list do
    printfn "  %-28s %-22s %s" (Json.strField "gem" step |> Option.defaultValue "") (Json.strField "how" step |> Option.defaultValue "") (Json.strField "cond" step |> Option.defaultValue "")
let stat key = Json.field key result |> Option.bind (Json.numField "mean") |> Option.defaultValue 0.0
printfn "engine %s; dps %.0f over 20 fights (evened %.0f); kill rate %.0f%%"
    (Json.strField "engine" result |> Option.defaultValue "") (stat "dps") (Json.numField "dpsEvened" result |> Option.defaultValue 0.0) (100.0 * (Json.numField "killRate" result |> Option.defaultValue 0.0))
printfn "waiting for mana %.0f%% of the time" (100.0 * (Json.numField "waitingForMana" result |> Option.defaultValue 0.0))
for share in Json.field "damageShare" result |> Json.list do
    printfn "  %-44s %5.1f%%" (Json.strField "name" share |> Option.defaultValue "") (100.0 * (Json.numField "value" share |> Option.defaultValue 0.0))
// the first fight, as the reference tells it: the ground, and the first strike of every use
printfn "-- the first fight"
let player = ready.Fight.Player.Name
let mutable last = ""
let mutable shown = 0
let out =
    Sim.traced (fun t kind who what x ->
        if shown < 80 then
            if kind = "Ground" then
                shown <- shown + 1
                printfn "  %7.3f  %-34s %-9s %.0f" t who what x
            elif kind = "Strike" && who = player && what <> last then
                last <- what
                shown <- shown + 1
                printfn "  %7.3f  %s" t what) ready.Fight 1u
printfn "  ... dps %.0f over %.1f s" ((out.DamageBy |> Map.toList |> List.sumBy snd) / max 0.1 out.Duration) out.Duration
for (k, v) in out.Counts |> Map.toList do
    if k.StartsWith "ground" || k.StartsWith "lit:" || k.StartsWith "absorbed:" || k.StartsWith "uses:" then printfn "  %-44s %6d" k v
for (k, v) in out.Uptime |> Map.toList |> List.sortByDescending snd do
    printfn "  %-44s %5.1f s" k v
