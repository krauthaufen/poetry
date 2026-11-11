# PoEtry

An event-driven Path of Exile 2 combat simulator. It plays out a fight hit by hit, with timing, triggers, meta-gem energy, ailment stacks, minions and Monte Carlo variance, instead of averaging everything into one DPS number.

Written in F#: functional, readable, and portable. It runs on .NET and is kept compatible with Fable, so it can compile to JavaScript for the browser. No reflection and no dependencies, and it uses its own deterministic RNG, so the same seed gives the same fight everywhere.

## Layout

| File | What |
|---|---|
| `src/PoEtry/Types.fs` | damage types, ranges, skill tags, ailments |
| `src/PoEtry/Mods.fs` | modifiers (base / increased / more / flag / override), conditions, the stat-indexed `ModDb` |
| `src/PoEtry/Tables.fs` | GENERATED game tables (monster life/armour/thresholds, GameConstants) |
| `src/PoEtry/Constants.fs` | named constants with their sources |
| `src/PoEtry/Hit.fs` | one hit: base, conversion, gain, roll, crit, inc/more, armour, resist, penetration |
| `src/PoEtry/Ailments.fs` | what a hit inflicts: ignite/bleed/poison, shock/chill, buildup ailments, flat-chance ailments |
| `src/PoEtry/Sim.fs` | the discrete-event engine: actors, rotations, rules (on use/hit/crit/inflict), buffs, debuffs, meta gems, DoT integration |
| `src/PoEtry/Summary.fs` | Monte Carlo: TTK / DPS percentiles, damage share, uptimes, event counts |
| `src/PoEtry/Json.fs`, `Xml.fs` | dependency-free readers (same behaviour on .NET and Fable) |
| `src/PoEtry/GameData.fs` | typed skills, gems, weapon bases from `data/game` |
| `src/PoEtry/StatText.fs` | item / passive text → game stat ids, by reversing the game's own stat description templates |
| `src/PoEtry/StatMap.fs` | game stat id → modifiers. **The simulator's knowledge of what stats mean.** Reads ids word by word; unknown words → Unmapped, never guessed |
| `src/PoEtry/Gems.fs` | gem + level + quality + supports (+ weapon) → `Skill` |
| `src/PoEtry/Character.fs` | items (game Ctrl+C or PoB format, local weapon mods) and passive tree → modifiers |
| `src/PoEtry/Pob.fs` | read a PoB build's inputs (items, tree, gems). Never its computed numbers |
| `cli/` | `poetry-sim build.xml`: coverage report + simulation of a socket group |
| `RULES.md` | **every mechanic, its source and how far we trust it** |

## Run

```bash
dotnet run --project tests/PoEtry.Tests          # test suite (.NET)
dotnet fable tests/PoEtry.Tests -o out/js && node out/js/Tests.js   # same suite in JS (add {"type":"module"} package.json in out/js)
dotnet run -c Release --project examples/Demo    # illustrative build, 2000 fights
dotnet run -c Release --project cli -- examples/builds/spark-test.xml --data data/game --hits 3
```

## Game data

`data/game/*.json` is exported from PoB-PoE2's *generated* Lua files, which PoB extracts from the game's .dat files: skills and gems per level, item bases, affixes, the passive tree, and the stat description templates. PoB's hand-written parts (its mod parser, stat maps, calc code) are not used.

```bash
# needs a Lua 5.2 interpreter (lua.org source, `make posix`)
lua tools/export/export.lua <PoB>/src data/game [treeVersion]
python3 tools/gen_tables.py <PoB>/src/Data/Misc.lua > src/PoEtry/Tables.fs
dotnet fsi tools/coverage/text.fsx      # how much item / tree text parses
dotnet fsi tools/coverage/statids.fsx   # how many stat ids have a meaning
```

Current coverage: 96.8% of affix lines and ~96% of passive tree lines parse to stat ids. Of the stat ids they produce, ~3,500 occurrences map to modifiers, ~5,100 are understood as non-offensive, ~1,600 are understood but not simulated yet (area, projectiles, curses, charges...), ~1,350 (long tail) are unmapped.

## Describing a build

A fight is an enemy, a player `Actor`, and optional minion actors. An actor has:

- `Mods`: the character sheet as modifier lines, e.g. `Mod.inc (Damage(OfType Fire)) 90.0`. Conditions read like the item text: `|> Mod.whenever (EnemyHas "Shocked")`, `Mod.whenever (PerStack("Frenzy", 1.0))`.
- `Skills`: base damage, crit, use time, cooldown, hit pattern, and the skill's own mods (gem and support stats).
- `Rotation`: a priority list. The first step whose condition holds and whose skill is off cooldown is used.
- `Rules`: on use / hit / crit / inflict → grant a buff or charge, apply a debuff, trigger a skill, or feed meta-gem energy.
- `Metas`: meta gems that trigger a skill when their energy is full.

Minions get the owner's `ForMinions` mods via `Mods.forMinions`.

See `examples/Demo/Program.fs`.

## Status

- Done: hit pipeline, mitigation, ailments, event engine, triggers, metas, minions, Monte Carlo summary, 24 tests.
- Done: game data import, item / tree text parsing, stat meanings, gem assembly, PoB build import, CLI.
- Next: per-skill behaviours (how many projectiles / hits land on one target, combos, stages); keystones and "Grants Skill"; curse / aura / exposure effect; charges; accuracy; buildup decay; resource costs; in-game verification of the "guess" rows in RULES.md.
