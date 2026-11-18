# The reference engine

`src/PoEtry/Sim/` is the simulator as a person is meant to read and change it. Whatever it does is, by definition, what PoEtry does. The fast engine (`src/PoEtry/Flat.fs`) is machine-written, nobody is expected to read it, and tests hold it to this one bit for bit.

## The idea in one paragraph

A fight is a `World` (an immutable record: the player, every actor and enemy, what lies on the ground, the queue of events) and a function that takes the next event off the queue and returns the next world. Nothing is mutated. Every function you will touch has the shape `... -> World -> World`, so mechanics are chained with `|>`, and a mechanic that does not apply returns the world it was given.

## Which file holds what

Files are compiled in this order, and a file only uses the ones above it.

| File | What is in it |
|---|---|
| `Model.fs` | The public description of a fight: `Actor`, `Skill` rules (`Hook`, `Reaction`, `Rule`), meta gems, `Ground`, `Fight`, `Outcome`. This is what `Behaviours.fs` and `Setup.fs` build. |
| `State.fs` | What exists while a fight runs: `Event`, `ActorState`, `EnemyState`, `PlayerState`, `World`, and `Env` (what is fixed for the whole fight). Small helpers: `schedule`, `bump`, `alive`. |
| `Lookup.fs` | Reading the world: an enemy's statuses, an actor's modifiers under its current buffs (`skillDb`), the player's defences now (`vitalsNow`), what the player can see (`view`). |
| `Clock.fs` | Time passing between two events (`advance`): damage over time, recovery, flasks, expiry. Also `takeDamage` and `recover` for the player. |
| `Effects.fs` | Putting things on enemies and the ground: debuffs, ailments (`inflict`), exposure, ground surfaces. |
| `Skills.fs` | Questions about a skill before it is used (ready? requirements met?) and summoning. |
| `Reactions.fs` | Rules: what an actor does besides hitting when something happens. One function per `Reaction`, and `perform`, which sends each reaction to its function. Also `startUse`: what a use of a skill sets going. |
| `Using.fs` | An actor is ready: which skill it uses next, what that costs, flasks. |
| `Hitting.fs` | One hit on one enemy (`hitEnemy`): the roll and the damage, then everything a landed hit leaves behind, one function each, in a fixed order. |
| `Striking.fs` | A use lands (`onStrike`): which enemies it reaches, then `hitEnemy` for each. |
| `Monsters.fs` | Monsters hitting the player or minions; minions dying and coming back. |
| `Periodic.fs` | What happens on a timer; kills by damage over time. |
| `Events.fs` | One step of the loop (`stepEvent`): take the next event, let time pass, call the function for that kind of event (`happen`), run the standing checks. |
| `Run.fs` | A whole fight (`runWith`): the world at the start, the loop, the outcome. |
| `Sim.fs` | The only thing the rest of the program sees: `Sim.run`, `Sim.shown`, `Sim.traced`. |

## How a use of a skill travels through it

1. `Ready` event → `Using.onReady` picks a skill by the actor's priority list and pays for it.
2. `Reactions.startUse` works out when its hits are due and schedules a `Strike` event for each. `OnUse` rules fire.
3. `Strike` event → `Striking.onStrike` finds the enemies in the skill's shape, and calls `Hitting.hitEnemy` for each.
4. `hitEnemy` rolls accuracy and crit, works out the damage (`Hit.fs`), takes it off the enemy, then runs the list of consequences (ailments, leech, energy for meta gems, remnants, rage, knockback) and the `OnHit`, `OnCrit` and `OnKill` rules.
5. The actor's next `Ready` event is already in the queue for when the use ends.

## Adding a mechanic

Most mechanics are one of three kinds. Find the kind first.

**A modifier** ("20% more damage against Frozen enemies"). Nothing in the engine changes. The stat is given its meaning in `StatMap.fs`; the engine already asks the modifier database with the right context.

**Something that happens when something else happens** ("gain a Power Charge on critical hit", "kills explode"). That is a `Rule`: a `Hook` (when), a `Reaction` (what), a filter (for which skills) and a chance.
1. If an existing `Reaction` says it, only `Behaviours.fs` changes: it reads the stat and emits the rule.
2. Otherwise add a case to `Reaction` in `Model.fs`, write its function in `Reactions.fs` beside the others, and add one line to `perform`. A reaction's function takes the `Reacting` record (who, with what skill, concerning which enemy) and the world:

```fsharp
/// `RemoveBuff`: the actor loses the buff.
let removeBuff (c: Reacting) name (w: World) =
    updateActor c.Actor.Name (fun st -> { st with Buffs = Map.remove name st.Buffs }) w
```

**A consequence of every hit** (a new kind of buildup, a new resource gained from hits). Write a function `LandedHit -> World -> World` in `Hitting.fs` and add it to the chain at the end of `hitEnemy`, where the order is spelled out. Its place in the chain matters: later ones see what earlier ones did.

Something delayed is an `Event`: add a case in `State.fs`, `schedule` it, and give it a function in `Events.fs` with a line in `happen`.

## Rules that keep the two engines and all results stable

- **Randomness.** Only use the `rng` you are handed, and draw from it only when the draw is needed. A hit's rolls come from a stream of that skill alone, so that two fights with the same seed that differ by one passive differ by what the passive does and not by luck. An extra draw in one place shifts every later roll of that stream.
- **Order.** Events at the same time happen in the order they were scheduled. If you schedule two things at once, the order you do it in is part of the behaviour.
- **Counts.** `bump "name" w.Counts` records that something happened; the page and the tests read these. Give a new mechanic a count.
- **Nothing silently.** If the engine cannot do what a stat asks, the stat is reported as not understood (in `StatMap.fs`), never approximated without a row in `RULES.md`.

## After a change

```bash
dotnet build -c Release cli
dotnet run --project tests/PoEtry.Tests -c Release
```

1. Add a test in `tests/PoEtry.Tests/Tests.fs`: build two actors by hand, run the fight, and assert on the outcome or its counts. `examples/Demo` shows how to build a fight without any game data.
2. Add or change the row in `RULES.md` with the source and the confidence. A guess also gets a line in `tools/rules/guesses-plain.txt`; `python3 tools/rules/guesses.py` rebuilds the site's pages.
3. The tests in `FlatTests.fs` compare the fast engine with this one and will now fail wherever your mechanic shows. That is expected. Leave `Flat.fs` alone and say so in the pull request; it is brought into line before the change goes live.

## What is still hard to read

Honest list, largest first. These are long chains in which each step rebinds the world and a handful of locals, and cutting them would need functions with a dozen parameters. They have section comments instead.

| Function | Lines | Why it is long |
|---|---|---|
| `Run.runWith` | 250 | builds the starting world (charges, buffs, timers, the first events), then reads the outcome off the last one |
| `Monsters.attackPlayer` | 250 | one monster hit on the player: block, evasion, mitigation per type, ailments, what the player gains from being hit |
| `Reactions.startUse` (first half) | 240 | builds a use's hit pattern: seals, Barrage, Flicker, stages |
| `Hitting.hitEnemy` (head) | 215 | the roll and the damage, a chain of early exits |
| `Striking.onStrike` | 150 | three ways a strike reaches enemies (area, projectiles, direct) |
| `Using.onReadyUnlocked` | 140 | choosing, reloading, paying |

Many things are still keyed by string (buff names, count names, statuses). A misspelt name does not fail to compile; it silently never matches. Search for an existing name before inventing one.
