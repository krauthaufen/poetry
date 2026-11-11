# PoEtry

An event-driven Path of Exile 2 combat simulator. It plays a fight out hit by hit: skill timing, triggers and meta-gem energy, charges and buffs, ailment stacks, minions and totems, monsters hitting back. Many fights with different seeds give a distribution, not one averaged DPS number.

Nothing about a build is hard-coded. A character is read from the game's own API output (or a PoB build), its items, passives and gems are turned into modifiers from the game's data files, and every mechanic the engine applies is listed in [RULES.md](RULES.md) with its source and how far it is trusted. What the simulator does not understand about a character is printed, never dropped silently.

Written in F#, with no dependencies and no reflection, and its own deterministic RNG: the same seed gives the same fight on .NET and, compiled with Fable, in JavaScript.

## Quick start

```bash
dotnet build -c Release cli
# a character as GET /character/poe2/<name> returns it, a PoB build XML, or a PoB build code
dotnet cli/bin/Release/net8.0/poetry-sim.dll character.json
dotnet cli/bin/Release/net8.0/poetry-sim.dll --help
```

With only a file it picks the skill that deals the most when pressed, plays everything else by its skill type (buffs kept up, cooldown skills on cooldown, curses kept on the target) and fights a level 83 boss. It prints the character sheet (pools, defences, maximum hit per damage type), what it did not understand, and then clear time and DPS percentiles, damage share, uptimes and event counts.

Useful options: `--scenario boss|pinnacle|pack|surrounded`, `--life 1e12 --time 30` for sustained DPS, `--group N` or `--rotation "A,B"` to choose what is pressed, `--runs N`.

## A play style is an F# value

How a character is played matters as much as its gear. The priority list is plain F#, with any condition on what the player can see:

```fsharp
#r "src/PoEtry/bin/Release/net8.0/PoEtry.dll"
open PoEtry

let read (f: string) = System.IO.File.ReadAllText("data/game/" + f)
let build, _ = Setup.parseBuild (System.IO.File.ReadAllText "character.json")
let options = Options.defaults
let character = Setup.load read build options

let play =
    [ KeepBuff "Charged Staff"
      OnCooldown "Frost Bomb"
      When((fun v -> v.EnemiesAlive > 3), "Spark")
      Spam "Falling Thunder" ]

let player = Setup.player character options play None
Setup.fight character options player (Encounter.boss 83 Rarity.defaults 30.0)
|> Summary.simulate 200 |> Summary.format |> printfn "%s"
```

`Setup.auto character options "Falling Thunder"` derives such a list from skill types, and `Setup.strongest` finds the gem worth pressing. See `examples/play.fsx`. Actors can also be built by hand from modifiers and skills, without any game data: `examples/Demo`.

## How it knows things

| Step | Where | What |
|---|---|---|
| Game data | `tools/export/export.lua` → `data/game/*.json` | skills and gems per level, item bases, affixes, the passive tree, monsters, stat description templates: exported from the data files PoB-PoE2 extracts from the game. PoB's own parser and calculations are not used |
| Text → stat ids | `StatText.fs` | item and passive text is matched against the game's own description templates, reversed |
| Stat ids → modifiers | `StatMap.fs` | read word by word; an unknown word makes the stat *unmapped*, never guessed |
| Gems → skills | `Gems.fs`, `Behaviours.fs` | gem level data, supports, weapon; what a skill does besides hitting (energy, remnants, buffs, curses, totems, ammunition, stages) from its stats |
| Character | `Api.fs`, `Pob.fs`, `Character.fs`, `Defence.fs` | items, passives, weapon sets, jewels → modifiers; pools and defences |
| The fight | `Sim.fs`, `Hit.fs`, `Ailments.fs`, `Encounter.fs` | the discrete-event engine, one hit, what a hit inflicts, monsters and geometry |
| Setup | `Setup.fs` | options, play styles, building the player and the fight |
| Results | `Summary.fs` | Monte Carlo percentiles, damage share, uptimes, counts |

## How it is checked

```bash
dotnet run -c Release --project tests/PoEtry.Tests                       # engine and data tests
dotnet fable tests/PoEtry.Tests -o out/js && node out/js/Tests.js        # the engine tests in JavaScript
dotnet fsi tools/census/census.fsx                                       # what of ALL game data is simulated
python3 tools/corpus/fetch.py && dotnet fsi --optimize+ tools/corpus/run.fsx   # 131 public characters
```

- **Tests**: every rule that can be stated as a number has one, worked out by hand or read off the raw data.
- **Census** (`tools/census/census.txt`): every stat on every affix, passive and gem in the game data, classified by whether the engine actually reads what it maps to.
- **Corpus** (`tools/corpus/report.txt`): public ladder characters of every ascendancy are imported and fought. The report ranks what was not understood by how many builds it affects, and compares our character sheet (life, mana, energy shield, attributes, armour, evasion, resistances) with PoB's for the same character. PoB's defensive numbers are a good reference; its DPS is only used as a hint where to look. The sheet is compared on PoB's own inputs (its quest rewards, the weapon set it had in hand, no alternative attribute nodes), so a difference is about arithmetic, not about PoB lagging behind the game.
- `tools/corpus/pob.mjs <nnn> <ModName>` lists PoB's own modifiers for a stat on a corpus character (needs a local pob-web checkout), to find the source a value differs by. `tools/corpus/why.fsx <build> <stat>` shows where a sheet value comes from, `line.fsx "<text>"` how a line of text is read, `one.fsx <build>` one build as the corpus plays it.

- **Skills** (`tools/skills/`): `ledger.fsx` lists every player skill with the stats and skill types the engine does not read; `dossier.fsx` puts each skill on a bare character and shows its description beside what the simulator builds and what a boss fight does (`dossiers.txt`). Reading the two against each other is how conditional parts, detonation and corpse skills were found.
- **Parity** (`tools/corpus/parity.py`): our fight against PoB's own breakdown of the same skill, character by character: hits per second, crit chance, average hit, and how long the character waits for mana. `pob-all.mjs` dumps PoB's side, `skillwhy.fsx <build> "<gem>" <stat>` shows the modifiers and the expected hit on ours. PoB's damage is no truth; the table is there so every difference gets an explanation (a gap of ours, or something PoB assumes: full stacks, no mana, no reload, one hit per use).

Corpus characters and your own builds stay out of the repository (`corpus/`, `builds-private/`).

## Status

Version 1.0 release candidate. Modelled: hit damage, conversion, crits, accuracy, penetration and mitigation; all ailments and buildup; cooldowns with stored uses, charges, rage, infusions and remnants; meta gems and invocations; curses, marks and curse auras; skill damage over time, lingering and staged skills; crossbow ammunition; shield attacks; persistent minions and totems; the player's pools, defences, recovery, flasks, ailments and stuns; keystones; weapon sets; radius jewels.

Not modelled yet, and reported when a build uses it: charms, warcries, temporary minions, companions and spectres missing from the data, heralds' own effects, heavy stun on the player, combo and parry mechanics, and a long tail of unique-specific stats. RULES.md marks every rule that is a guess.
