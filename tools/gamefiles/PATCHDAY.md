# Patch day: regenerating the game data without PoB

Everything in `data/game/*.json` and the generated tables can be made straight from the installed
game. Nothing here needs the network except step 2.

## Steps

```bash
cd tools/gamefiles

# 1. update the game in Steam (config.json "steam" points at the install; POE2_DIR overrides it)

# 2. optional, when a table no longer fits: get the newest community schema
curl -L -o schema.min.json https://github.com/poe-tool-dev/dat-schema/releases/download/latest/schema.min.json

# 3. does the schema fit the tables we read?   exit code 1 = something to fix (next section)
nice -n 19 node export/check-schema.mjs

# 4. everything at once: check, export, generated F#, comparison with data/game, what is new
export/regen.sh
```

`regen.sh` writes, and touches nothing outside `out/`:

| What | Where |
|---|---|
| `skills.json gems.json minions.json bases.json mods.json tree.json statdesc_*.json` | `out/game-direct/` |
| `tree-full.json` (input of `tools/export/tree_ui.py`), `stats-used.json` | `out/game-direct/` |
| `Tables.fs`, `RarityData.fs`, `RareModsData.fs` | `out/generated/` |
| `compare.txt` (against `data/game`), `whatsnew.txt` | `out/game-direct/` |

Then, by hand, once the comparison looks right:

```bash
cp out/game-direct/{skills,gems,minions,bases,mods,tree}.json out/game-direct/statdesc_*.json data/game/
cp out/generated/*.fs src/PoEtry/
python3 tools/export/tree_ui.py out/game-direct/tree-full.json <pob-web>/packages/web/public/data/sprites/_sprites.json
```

The single steps:

```bash
node export/export.mjs [out dir] [--only statdesc,skills,minions,bases,mods,tree] [--all-effects] [--tree-version 0_5]
node export/tables.mjs [out file]                 # Tables.fs (replaces tools/gen_tables.py)
node export/dump-tables.mjs [out dir]             # tables/English/*.json per config.json, without the network
node export/compare.mjs [ours] [theirs] [--file skills.json] [--field 'statSets[].label'] [--examples 10] [--keys]
node export/whatsnew.mjs <old dir> <new dir> [--all] [--changed]
node export/check-schema.mjs [--verbose] [--all] [Table ...]
```

Decoded tables are cached in `~/.cache/poetry-export/tables/<game index + schema>/`; a new patch or a
changed schema makes a new cache and the export removes the old one. `rm -rf ~/.cache/poetry-export`
is always safe.

## When a table no longer fits

`check-schema.mjs` says per table:

- **TOO WIDE**: the schema describes more bytes than a row has. Columns were removed or the schema is of
  another version. The export refuses to read such a table.
- **SHIFTED?**: the widths may agree, but a column holds values that cannot be right (a key past the end of
  the table it points to, text outside the file, a flag that is not 0 or 1). The layout is wrong from the
  first column named, or shortly before it.
- **new columns**: a row is longer than the schema says. If the new columns were added at the end, everything
  still reads correctly; if they were inserted, later columns show up as SHIFTED.
- **NO SUCH FILE**: the table was renamed or removed; find its successor with
  `node export/dev/raw.mjs ls data/balance`.

Where to fix it, in this order:

1. A newer `schema.min.json` (step 2). The community usually has the big tables within days.
2. `export/schema-overrides.json`, which is applied on top of the schema. Per table one of
   - `"patch": { "<column number>": "Name" }` or `{ "<n>": { "name": .., "type": .., "array": true, "references": {"table": ..} } }`
     to name or retype a column (this is how `Stats.CannotGrantToMinion` and friends are named today),
   - `"append": [ { "name": .., "type": .. } ]` for columns added at the end,
   - `"columns": [ ... ]` to give the whole row by hand.
   Types: `bool string i16 u16 i32 u32 f32 enumrow row foreignrow array`; `"array": true` for lists,
   `"interval": true` for a pair. Widths: bool 1, i32/u32/f32/enumrow 4, string 8, row 8, foreignrow 16, any list 16.
3. To work out a layout: `node export/dev/peek.mjs <Table> [row]` shows a row's bytes column by column,
   `node export/dev/cols.mjs <Table> [<PoB>/src/Export/spec.lua]` shows the schema beside PoB's layout by byte
   offset (PoB's `spec.lua` is a second opinion once PoB has caught up; it is not needed).
4. The column names the exporter uses are the schema's. If the community renames a column, the export
   fails with `undefined` values in that file; the name is used in `export/<file>.mjs`, each with the PoB
   name in a comment where the two differ.

Not tables, and checked by `check-schema.mjs` as "OTHER FILE":

- the passive graph `metadata/*.psg` (named by `PassiveSkillTrees`): `export/tree.mjs` `parsePsg` reads
  version 3 and stops with a message on another version;
- `data/statdescriptions/*.csd` (text); `metadata/**/*.it`, `*.ot` (text).

After the export, read its console lines: skills naming a description file the game does not have,
minion skills without a monster, description handlers `describe.mjs` does not know.

## What is kept by hand

- `export/hand/minion-lists.json`: which monster a minion skill summons. The client files have no table
  for it. The export links new minion skills by name where it can and prints the rest.
- `export/hand/minions.json`: the monsters whose skills were picked by hand, with the simulator's key for
  them where it is not the monster id ("RaisedZombie") and the name of their count limit.
- `export/hand/monster-skills.json`: which granted effects of those monsters are skills (a monster also
  has animations, triggers and audio as granted effects; PoB picks by hand, 310 of about 1600).
- `export/hand/skill-names.json`: display names of skills the game leaves unnamed or all calls "Basic Attack".
- A monster that is not in `minions.json` gets its skills by rule (`minions.mjs`: no helper effects, nothing a
  reviewed monster has) and the game's names. The export writes them to `out/game-direct/derived-minions.txt`
  and `derived-skills.txt`: review these on patch day and move what matters into the hand files.
- Rules, not lists, written in the code: item class to type name and rune socket limit (`bases.mjs`),
  which monsters can be spectres (`minions.mjs`, PoB's rule), orbit radii of the tree (`tree.mjs`,
  constants of the game's tree drawing), `baseFlags` from skill types (`baseflags.mjs`).

## Still depending on something else

- The community dat schema (`schema.min.json`), for the 55 tables in `export/tables-used.json`.
- `tools/export/tree_ui.py` needs the tree art as PNG sheets, which pob-web makes from PoB's tree assets;
  the tree data it needs comes from `tree-full.json`.
- `tools/export/export.lua`, `tools/gen_tables.py`, `tools/export/minion_sizes.py` are the PoB route and are
  not needed for any of the above.

## After the data is in: what the simulator does not know yet

```bash
python3 tools/gamefiles/gen_web_classes.py                         # new classes and ascendancies for "new build" on the page
nice -n 19 dotnet build -c Release cli
nice -n 19 dotnet run --project tests/PoEtry.Tests -c Release      # tests that fail name what the game changed
nice -n 19 dotnet fsi tools/census/census.fsx                      # every stat of every affix, passive and gem: read, unmapped, unsimulated
nice -n 19 dotnet fsi tools/skills/ledger.fsx                      # every player skill with the stats and skill types nothing reads
```

Work the lists from the top (most used first):

- **`out/game-direct/whatsnew.txt`**: new skills, supports, bases, mods, passives and the stat ids seen for the first time.
- **Unmapped stats** go into `src/PoEtry/StatMap.fs`; many new ones are a known stat with a new prefix or suffix and need one line.
- **New skill types** show as "NOT MODELLED for this skill" until they are in `modelledTypes` or `descriptiveTypes` in `Gems.fs`.
- **A character from the new patch** says under "not understood" what the data lacks: gems and classes that are not in the game data, passives that are not in the tree data. If those appear the export is older than the character.
- **New mechanics** get a row in `RULES.md` with the keyword text as source (`reference/poe2db-keywords.txt`, or the game's own keyword table) before they are built.
- **`out/game-direct/derived-minions.txt`, `derived-skills.txt`**: monsters and monster skills chosen by rule, to be reviewed into `export/hand/`.
