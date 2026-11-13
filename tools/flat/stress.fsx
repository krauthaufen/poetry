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
            | Error _ -> None
            | Ok p ->
                // a fight of very many events is left out (the reference takes minutes for it)
                let n = ref 0
                let long' =
                    try
                        Flat.traced (fun _ _ _ _ _ -> n.Value <- n.Value + 1; if n.Value > 60000 then raise TooLong) p (case + 17u) |> ignore
                        false
                    with TooLong -> true
                if long' then None
                else
                    match FlatTests.verdict fight (case + 17u) with
                    | FlatTests.AtThreshold d -> Some(FlatTests.AtThreshold(described + "\n  " + d))
                    | FlatTests.Wrong d -> Some(FlatTests.Wrong(described + "\n  " + d))
                    | v -> Some v
        with e -> Some(FlatTests.Wrong(sprintf "case %d threw %s" case e.Message)))
    |> Array.choose id
let count f = results |> Array.filter f |> Array.length
let threshold = results |> Array.choose (function FlatTests.AtThreshold d -> Some d | _ -> None)
let wrong = results |> Array.choose (function FlatTests.Wrong d -> Some d | _ -> None)
printfn "%d fights compared: %d the same in every bit, %d the same within %g, %d parted at a threshold after a rounding difference, %d wrong"
    results.Length (count (fun v -> v = FlatTests.Exact)) (count (fun v -> v = FlatTests.Within)) FlatTests.tolerance threshold.Length wrong.Length
if threshold.Length > 0 then printfn "parted at a threshold, for example:\n%s" threshold.[0]
for b in wrong |> Array.truncate 5 do
    printfn "WRONG %s" b
