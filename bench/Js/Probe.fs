/// How long the whole path takes, on .NET and (compiled with Fable) in JavaScript: reading the
/// game data, building a character, finding what to press, and fights.
///   dotnet run -c Release --project bench/Js -- <build.json>
///   dotnet fable bench/Js -o out/probe && node out/probe/Probe.js <build.json>
module Probe

open PoEtry

[<EntryPoint>]
let main args =
    let root = __SOURCE_DIRECTORY__ + "/../../"
    let clock () = float System.DateTime.Now.Ticks / 1e7
    let timed name f =
        let t = clock ()
        let r = f ()
        printfn "%-28s %6.2f s" name (clock () - t)
        r
    let read (f: string) = System.IO.File.ReadAllText(root + "data/game/" + f)
    let text = System.IO.File.ReadAllText args.[0]
    let build, _ = Setup.parseBuild text
    let o = { Options.defaults with MaxTime = 30.0 }
    let c = timed "game data + character" (fun () -> Setup.load read build o)
    let boss = Encounter.boss 83 Rarity.defaults 30.0 |> List.map (fun e -> { e with Life = 1e13 })
    let gem = timed "what to press" (fun () -> Setup.strongest c o boss |> Option.defaultValue "")
    let fight = Setup.fight c o (Setup.player c o (Setup.auto c o gem) None) boss
    let outs = timed "20 fights of 30 s" (fun () -> [ for seed in 1u .. 20u -> Sim.run fight seed ])
    // the same through the front end's JSON interface
    let session = Session read
    let ask (fields: (string * Json) list) = Report.write (JObj(Map.ofList (("build", JStr text) :: fields)))
    let sheet = timed "session: character" (fun () -> session.Handle(ask [ "op", JStr "character" ]))
    let result = timed "session: 20 fights" (fun () -> session.Handle(ask [ "op", JStr "fight"; "runs", JNum 20.0 ]))
    printfn "sheet %d characters, fight: %s" sheet.Length (result.Substring(0, min 160 result.Length))
    printfn "pressing %s: %.0f dps" gem (outs |> List.averageBy (fun out -> (out.DamageBy |> Map.toList |> List.sumBy snd) / max 0.1 out.Duration))
    0
