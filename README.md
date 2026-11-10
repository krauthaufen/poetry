# PoEsie

An event-driven Path of Exile 2 combat simulator. It plays out a fight hit by hit, with timing, triggers, meta-gem energy, ailment stacks, minions and Monte Carlo variance, instead of averaging everything into one DPS number.

Written in F#: functional, readable, and portable. It runs on .NET and is kept compatible with Fable, so it can compile to JavaScript for the browser. No reflection and no dependencies, and it uses its own deterministic RNG, so the same seed gives the same fight everywhere.

## Layout

| File | What |
|---|---|
| `src/PoEsie/Types.fs` | damage types, ranges, skill tags, ailments |
| `src/PoEsie/Mods.fs` | modifiers (base / increased / more / flag / override), conditions, the stat-indexed `ModDb` |
| `src/PoEsie/Tables.fs` | GENERATED game tables (monster life/armour/thresholds, GameConstants) |
| `src/PoEsie/Constants.fs` | named constants with their sources |
| `src/PoEsie/Hit.fs` | one hit: base, conversion, gain, roll, crit, inc/more, armour, resist, penetration |
| `src/PoEsie/Ailments.fs` | what a hit inflicts: ignite/bleed/poison, shock/chill, buildup ailments, flat-chance ailments |
| `src/PoEsie/Sim.fs` | the discrete-event engine: actors, rotations, rules (on use/hit/crit/inflict), buffs, debuffs, meta gems, DoT integration |
| `src/PoEsie/Summary.fs` | Monte Carlo: TTK / DPS percentiles, damage share, uptimes, event counts |
| `RULES.md` | **every mechanic, its source and how far we trust it** |

## Run

```bash
dotnet run --project tests/PoEsie.Tests          # test suite
dotnet run -c Release --project examples/Demo    # illustrative build, 2000 fights
python3 tools/gen_tables.py <PoB>/src/Data/Misc.lua > src/PoEsie/Tables.fs
```

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
- Next: import skill and gem data from the game files (base damage per level, stat sets, supports); parse item and tree mod text into `Mod`s; accuracy and evasion; buildup decay; resource costs; in-game verification of the "guess" rows in RULES.md.
