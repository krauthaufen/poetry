# Contributing

The short version is on the site: <https://poetry.awx.at/about.html>. Everything goes through GitHub issues, and most of it needs no code.

## What helps most

1. **Check a skill against the game.** Record one boss kill and open a *skill check* issue. Only Falling Thunder has been checked so far; for every other skill the number of hits that land per use is untested.
2. **Report a wrong number**, with the game's side as a screenshot and the character.
3. **Report a line that is not understood** and matters for a build.
4. **Correct a rule**, with a source.

Never post login tokens. A character is best given as a Path of Building code.

## What counts as evidence

Strongest first: the game's data files, something seen in the game (recording or screenshot, with the character), a statement by the developers, the wiki, Path of Building, a guess. `RULES.md` records which of these each rule rests on.

## Changing the code

```bash
dotnet build -c Release cli
dotnet run --project tests/PoEtry.Tests -c Release
```

- Mechanics live in the reference engine, `src/PoEtry/Sim/`. [ENGINE.md](ENGINE.md) says which file holds what and walks through adding one.
- The fast engine, `src/PoEtry/Flat.fs`, is machine-written and held to the reference by tests (`tests/PoEtry.Tests/FlatTests.fs`). Do not edit it by hand. A change to the reference makes those tests fail until the fast engine has been brought into line; say so in the pull request and leave it at that.
- A new or changed rule gets its row in `RULES.md` (source and confidence) and a test in `tests/PoEtry.Tests/Tests.fs`.
- Nothing about a particular build or skill is hard-coded. A skill's behaviour is read from its stats (`Gems.fs`, `Behaviours.fs`); if the data does not say something, it goes into `tools/gamefiles/export/hand/` with a comment on where it comes from.
- What is not understood is reported, never guessed and never dropped.
- Open an issue before anything larger than a fix.
