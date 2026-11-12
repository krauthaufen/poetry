# Derived stats as an adaptive table, compiled by FableCube

The simulator asks the same questions at every step: what is this skill's damage range per
type, its crit chance, its speed, its cost, this actor's armour and resistances. Each answer
is a fold over the actor's mods under the conditions that hold right now. Most answers change
rarely (when a buff's stacks change, when the build is edited), but they are computed again
at every hit.

The idea: write the answers once as a **pure function of an immutable model** and let
FableCube compile that function into an adaptive program that keeps a table of them up to
date. The simulator pushes the model when it changes and reads cells. This note says how
that could look, what FableCube already does for it, and what would have to change, in
FableCube and in PoEtry.

Nothing here is built. Where a statement about FableCube comes from reading its design or
source and not from running it on PoEtry's code, it says so.

## Where the time goes today

- `Sim.fs` and `Hit.fs` together make about 200 `Mods.*` queries (`total`, `mult`, `sumBase`,
  `flag`, `override'`), most of them inside `hitEnemy`, `rollWith` and `attackPlayer`. Each
  query walks the mods of some stats and evaluates every mod's conditions against a `Ctx`
  (crit, skill name and tags, the actor's buff stacks, the enemy's statuses).
- The simulator already caches by hand, because this was measured to be expensive:
  - `env.SkillDbs`: the sheet's and the skill's mods as one `ModDb` per (actor, skill),
    built once per fight;
  - `skillDb`: that db plus the buff mods, kept per (actor, skill) while `st.Buffs` is the
    same object (`ReferenceEquals`);
  - `status`: a ring of the last four enemies' status maps;
  - `ModDb` itself: an index by stat and kind, in layers, so a query sees only mods that
    can matter (the comment there: comparing stats in an ordered map "was a quarter of a
    fight's time").
- These caches hold *inputs* of the queries (the mods to look at). The queries themselves,
  and everything computed from them (conversion, ranges, hit chance), run again every time.

So the table would replace the hand-made caches and go one level further: it would hold the
*results*.

## The shape

One pure function, in a file of its own, with no knowledge of adaptivity:

```fsharp
/// What the answers depend on: a projection of Fight and World that leaves out
/// everything that changes at every step (life, time, positions, the RNG).
type Inputs =
    { Actors : ActorIn list }      // the player and the minions
and ActorIn =
    { Name : string
      Sheet : Mod list             // gear, tree, permanent buffs: changes when the build is edited
      Skills : Skill list
      Stacks : Map<string, int> }  // buffs, charges, rage: changes during a fight

/// One row per actor, skill and crit: what a use of that skill needs.
type SkillRow =
    { Actor : string; Skill : string; Crit : bool
      Ranges : Map<DamageType, Range>   // after added damage, conversion and gain
      DamageMult : Map<DamageType, float>
      CritChance : float; CritBonus : float
      Speed : float; Cost : float
      AilmentChance : Map<Ailment, float>
      Penetration : Map<DamageType, float> }

let table (i : Inputs) : SkillRow list =
    [ for a in i.Actors do
        for s in allSkills a do
            for crit in [ false; true ] do
                let ctx = Skill.ctx s crit a.Stacks Map.empty
                let mods = a.Sheet @ s.Mods @ buffMods a
                yield { Actor = a.Name; Skill = s.Name; Crit = crit
                        Ranges = Hit.ranges ctx mods s
                        CritChance = Hit.critChance ctx mods s
                        ... } ]
```

FableCube turns `table` into a generated module with two entry points, as it does for a
view (`examples/Bars` in the FableCube repo is the closest existing case: a pure `frame`
whose output is a set of records with a cell per field):

```fsharp
let cells, push = Generated.Stats.createInput inputs   // once per fight (or per build)
let rows = Generated.Stats.residual cells              // the table, kept up to date

// in the simulator, when an actor's stacks changed:
transact (fun () -> push (inputsOf fight world))
// at a hit:
let row = lookup actor.Name skill.Name crit            // a row object
let ranges = AVal.force row.Ranges                     // recomputed only if something it read changed
```

The pure `table` stays the specification. It can be called directly, which is what the
tests and the first version of the simulator would do; the generated module is an
optimisation that must give the same values.

### What belongs in the table and what does not

The table pays off for values that are read often and change rarely. That splits the mods'
conditions into three classes:

| condition | changes | where |
|---|---|---|
| none, `SkillIs`, `SkillNamed` | never during a fight | in the table; fixed per row |
| `WhileBuff`, `PerStack` (the actor's buffs and charges) | at some events | in the table; a cell per buff a row reads |
| `IsCrit` | at every hit | two rows per skill, as above |
| `EnemyHas`, `PerEnemyStack`, the per-hit `Isolated` / `Close` / `OpenWeakness` | per target, per hit | not in the table: applied at the hit, as today |

For the last class the row would hold the part that does not depend on the enemy, and the
enemy-dependent mods would be evaluated at the hit on top of it. That needs the formulas to
be separable: sums of `Inc` and products of `More` are (the row holds the sum and the
product without the enemy-dependent mods, the hit adds theirs); conversion and `Override`
are not, so a row whose conversion mods depend on the enemy would have to be computed at
the hit as today. How many such mods there are in the corpus is worth counting before
deciding (`tools/census`).

A second, smaller table of the same kind covers the defensive side (`VitalsAt` is already
"mitigation from the current buffs and charges"), and a third the monsters' side per enemy
(`defender`: the enemy's db plus its debuffs), where the changing input is the debuff map.

## What FableCube does for this as it is

From its design document, its tests and the generated code of its examples:

- **Record outputs with a cell per field.** A view may return records; the generated
  module has a record type whose fields are `aval`s, one per field, except those named
  `--plain` (here `Actor`, `Skill`, `Crit`). A change that affects one field's value steps
  that cell only. This is `examples/Bars`: `Scene_Draw_adaptive2 = { Offset : aval<V3>;
  Size : aval<V3>; Color : aval<Rgb>; Tag : string }`.
- **F# `Map` and `Set` inputs.** "A map in the model has a cell per entry read": a row that
  reads `a.Stacks.["Power Charge"]` depends on that entry, not on the map. That is the
  dependency the hand-made `ReferenceEquals` cache cannot express (it drops everything
  when any buff changes).
- **Ordinary F#.** Local mutation (`let mutable`, loops), ref cells, classes with mutable
  fields, `dict`, exceptions and `sprintf` are lowered to pure code first, so `Mods.fold`'s
  accumulator loop is not an obstacle by itself. What the compiler cannot see through "runs
  as compiled": correct, but one cell that recomputes whenever any of its arguments changes.
- **Folds over lists.** Sums and counts of a list that changes element by element cost the
  change, not the list. As far as I read the source, only integer sums are treated as a
  group (add the new, subtract the old); float sums keep their order, so the results should
  be the same bits as the pure function's. That matters here (the regression numbers are
  compared to the last digit) and should be the first thing a spike checks.
- **Equality cuts.** A cell that is recomputed to the same value does not disturb its
  readers. `floor (stacks / every)` changing from 2.9 to 2.95 stops there.
- **Both targets.** The generated module is plain F# over FSharp.Data.Adaptive; it compiles
  on .NET (the CLI, the corpus tools) and with Fable (the worker), where FSharp.Data.Adaptive
  is a shim over wombat.adaptive.
- **An oracle.** FableCube's own tests run random models and edits and compare the residual
  with the pure function. The same harness shape fits PoEtry: random stacks and builds, the
  table against `table inputs`.
- **Hand-written incremental code where needed.** `Foreign.incremental f fa` declares a
  function with an adaptive counterpart that takes cells, lists, sets or maps. `ModDb`'s
  index could stay what it is behind such a declaration.

## What would have to change in PoEtry

1. **A pure entry function and its input type**, as sketched. Most of the code it calls
   exists (`Hit.ranges`, `Hit.critChance`, `Hit.convert`, `Mods.*`); what is new is the
   projection `inputsOf fight world` and the row types.
2. **Splitting each formula at the enemy-dependent mods** (see the table above), so a row
   holds what does not depend on the target. This is the real modelling work, and it is
   useful without FableCube: it says exactly what a hit still has to compute.
3. **`ModDb` for the compiler.** Two ways:
   - leave it opaque: `Mods.total ctx stats db` becomes one cell per call that recomputes
     when `db` or anything in `ctx` changes. Simple, and already better than today (results
     are kept), but a row then depends on *all* stacks, because `ctx.Buffs` is passed whole;
   - give the compiler the mods as a list filtered by stat, so it sees `Mod.eval` reading
     `ctx.Buffs.[name]` and makes each row depend only on the buffs its mods name. This is
     the point of the exercise; the index then only has to exist for the static part (the
     sheet), which never changes during a fight and is folded once.
4. **Reading rows in the simulator.** `hitEnemy` and friends take values from a row and no
   longer build a `Ctx` and query. The simulator itself stays a pure fold over `World`; the
   table is a cache of a pure function of it, like the caches it replaces.
5. **Dependencies.** The engine has none today ("no deps, no reflection"). This adds
   FSharp.Data.Adaptive on .NET and wombat.adaptive (through Aardvark.Portable's shim) in
   the worker, plus `FableCube.Runtime` (one small F# project). The web front end has all of
   them already; the CLI does not. The table could be an optional layer: the pure `table`
   is always there, and a build without the generated module calls it directly.
6. **A generation step in both builds**, and the decision whether the generated file is
   checked in.

## What would have to change in FableCube

1. **A keyed output.** The residual's result would be a set or list of rows; the simulator
   wants `row (actor, skill, crit)` in constant time. FableCube deliberately has no map
   output ("a set or map result would be one value again, rebuilt on every change"). What
   fits is a small runtime piece: a reader over the output set that maintains a dictionary
   by a key function (rows come and go only when skills or actors do). It is general (any
   non-UI consumer wants it) and belongs in `FableCube.Runtime`.
2. **Use outside a UI.** Everything about hosting so far lives in the toy's framework
   (`Host.fs`, `Entry.fs`, the DOM and scene constructors). A headless consumer needs only
   `createInput`, `residual` and reads, which exist, but nobody has used them that way
   outside FableCube's tests and `examples/Bars/desktop`. Expect small things: an input
   update that is cheap when called thousands of times a second, reads between
   transactions, disposal of a table.
3. **Cheap pushes.** `push model` writes every input cell and compares maps by value to
   find the changed entries ("a set or map is one value in the view, so its changes are
   found by comparing"). At one push per simulator event that comparison could cost more
   than it saves. Needed: reference equality as the first test everywhere (an unchanged
   `Stacks` map is the same object), and probably a way to push one actor's stacks without
   presenting the whole model. FableCube already skips a list that is the very list it
   holds; maps should get the same.
4. **Per-cell cost.** FableCube's work so far aimed at scenes of thousands of objects where
   a cell costs memory; here a cell is read in a hot loop. A forced, up-to-date `aval` is a
   field read and a version check on .NET, but the residual's cells are FSharp.Data.Adaptive
   objects, allocated per row and field. For a build with 30 skills and parts that is a few
   hundred rows of ten fields: small. For the corpus (131 builds, many seeds) the table
   should be built once per build and reused across seeds, which is where it wins most:
   sheet-only values are computed once per build edit, not once per fight.
5. **Several tables at once.** Fights of different builds, or parallel seeds on .NET
   threads, each need their own instance. `createInput` gives independent inputs; that the
   generated module has no shared mutable state besides constants should be verified, and
   FSharp.Data.Adaptive's transactions under threads considered (one table per thread is
   the simple rule).
6. **Packaging.** The compiler is run as `FableCube.Tests.dll emit <Module> <entry> <out.fs>
   [--plain F,G] [--ref a.dll] <files...>` out of a checkout next to the application. For a
   second repository that is workable (the toy does it) but it ties PoEtry's build to a
   FableCube checkout; a dotnet tool would be the clean form.
7. **What the compiler has not met.** PoEtry's engine is a different kind of code from
   views: a 60-case union as a key (`Stat`), `Map<DamageType, Range>` values built by
   folds, records holding functions (`Step.When`), string keys everywhere. FableCube's
   design says all of F# is in scope and what is left "runs as compiled", and the web front
   end already compiles the simulator's types as data, but the engine's own code has never
   been through the specializer. The first run of `emit` over `Mods.fs` and `Hit.fs` will
   say what is missing; I would expect a handful of front-end gaps, as PRo3D found.

## What stays outside either way

- **Randomness.** Rolls happen at the hit. A row gives the range and the chance; the roll
  uses them.
- **Per-hit situation** (`Isolated`, `Close`, a weakness roll, distance): at the hit.
- **Time.** Buff expiry is an event in the simulator; the table sees the stacks after it.
- **The enemy's side per target**, unless the third table is built.

## A first step that decides it

A spike small enough to throw away, measured with `bench/`:

1. `table` for one actor with three outputs that are certainly hot: `Hit.ranges`,
   `Hit.critChance` and the damage multiplier per type, with `ModDb` left opaque.
2. `emit` it. Note every "left out" and "runs as compiled" line: that is the list of
   front-end work.
3. Compare the generated table with the pure function on random stacks (same bits).
4. In the simulator, read those three from the table for the player only, keep everything
   else. Run the two private builds and compare with the regression numbers.
5. Measure a fight and a corpus run before and after, on .NET and under Fable.

If step 5 shows a clear gain with the opaque `ModDb`, the per-buff version (point 3 of the
PoEtry list) is the follow-up. If the push or the cells cost more than the queries they
save, the split of the formulas (point 2) is still worth keeping, with a hand-written cache
per row keyed by the stacks it reads.

## Open questions

- How often do an actor's stacks change in a typical fight compared with the number of
  hits? The table only wins if the second is much larger. A counter in `Sim.run` answers it.
  **Measured** (2026-11-12, one 20 s boss fight each, JavaScript, counters in `skillDb` and
  `Hit.rollWith`; corpus builds 121, 024, 011, 047):

  | build | actors | events | hits rolled | asks for a skill's mods | buff map is another object | stacks really differ (per actor and skill asked) | per actor |
  |---|---|---|---|---|---|---|---|
  | Oracle, no minions | 1 | 1,033 | 190 | 864 | 337 | 100 | 37 |
  | Spirit Walker, 35 minions | 36 | 7,107 | 1,841 | 11,188 | 2,102 | 414 | 323 |
  | Infernalist | 2 | 797 | 157 | 709 | 225 | 99 | 43 |
  | Monk, 16 minions | 17 | not counted | 642 | 3,365 | 719 | 128 | 90 |

  So a skill's mods are asked for 7 to 27 times per real change of the stacks they could
  depend on, and hits outnumber an actor's stack changes about 5 to 1. The hand-made
  `ReferenceEquals` cache misses three times more often than it must (a buff refreshed with
  the same stacks is another map). A fight is 150 to 400 µs per event today; each hit makes
  roughly 140 to 230 `Mods.*` queries.
- Is the no-dependency rule for the engine firm, or is an optional layer acceptable?
- Should the table also serve the front end (the sheet's numbers shown live while the tree
  is edited)? Then the same generated module runs in the page with the build as its model,
  and "allocate one passive, see what changes" is the table's delta. That use needs no
  simulator change at all and may be the better first customer.
