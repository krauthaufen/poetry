// A play style is an F# program: load a character, say how it's played, pick fights.
//   dotnet fsi examples/play.fsx <character.json>
#r "../src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let data = __SOURCE_DIRECTORY__ + "/../data/game/"
let read (f: string) = System.IO.File.ReadAllText(data + f)
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText fsi.CommandLineArgs.[1])

let options = { Options.defaults with CastOnSelf = true }
let character = Setup.load read build options

// first step that is ready and whose condition holds
let play =
    [ KeepBuff "Charged Staff"
      WhileBelow("PowerCharge", 5, "Frost Bomb")
      When((fun v -> v.EnemiesAlive > 3 && v.EnemyLifeFraction > 0.5), "Spark")
      Spam "Falling Thunder" ]

let player = Setup.player character options play None
let level = 83
let fights =
    [ "boss", Encounter.boss level Rarity.defaults 30.0
      "pack", Encounter.pack level Rarity.defaults [ Normal, 8; Magic, 3; Rare, 1 ] 30.0 25.0 false 1u ]
for name, enemies in fights do
    printfn "== %s" name
    Setup.fight character options player enemies |> Summary.simulate 50 |> Summary.format |> printfn "%s"
