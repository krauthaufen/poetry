// Many more random fights than the test suite plays: each one that the fast engine (Flat)
// compiles is played by both engines with one seed and the outcomes compared; what differs
// is printed with the fight's case number (FlatTests.randomFight makes the fight again from it).
//   dotnet build -c Release tests/PoEtry.Tests
//   dotnet fsi --optimize+ tools/flat/stress.fsx <first case> <last case>
#r "../../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
#r "../../tests/PoEtry.Tests/bin/Release/net8.0/PoEtry.Tests.dll"
open PoEtry

let lo, hi = uint32 fsi.CommandLineArgs.[1], uint32 fsi.CommandLineArgs.[2]
exception TooLong
let results =
    [| lo .. hi |] |> Array.Parallel.map (fun case ->
        try
            let fight, described = FlatTests.randomFight case
            match Flat.compile fight with
            | Error _ -> 0, [], 0
            | Ok p ->
                // a fight of very many events is left out (the reference takes minutes for it)
                let n = ref 0
                let long' =
                    try
                        Flat.traced (fun _ _ _ _ _ -> n.Value <- n.Value + 1; if n.Value > 60000 then raise TooLong) p (case + 17u) |> ignore
                        false
                    with TooLong -> true
                if long' then 0, [], 1
                else
                    match FlatTests.check fight (case + 17u) with
                    | Some (Error d) -> 1, [ described + "\n  " + d ], 0
                    | _ -> 1, [], 0
        with e -> 0, [ sprintf "case %d threw %s" case e.Message ], 0)
let bad = results |> Array.collect (fun (_, b, _) -> Array.ofList b)
printfn "%d cases, %d compiled and compared, %d left out as too long, %d differ" results.Length (results |> Array.sumBy (fun (c, _, _) -> c)) (results |> Array.sumBy (fun (_, _, l) -> l)) bad.Length
for b in bad |> Array.truncate 5 do
    printfn "%s" b
