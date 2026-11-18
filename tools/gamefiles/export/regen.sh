#!/bin/sh
# Regenerates everything from the installed game into <repo>/out. See ../PATCHDAY.md.
# Usage: export/regen.sh            (from tools/gamefiles)
set -e
cd "$(dirname "$0")/.."
OUT=../../out
N="nice -n 19"
$N node export/check-schema.mjs                      # stops here when a table layout no longer fits
$N node export/export.mjs "$OUT/game-direct"         # data/game/*.json, tree-full.json, stats-used.json
$N node export/tables.mjs "$OUT/generated/Tables.fs"
$N node export/dump-tables.mjs                       # tables/English/*.json for the two generators below
$N python3 gen_rarity.py tables/English > "$OUT/generated/RarityData.fs"
$N python3 gen_rare_mods.py tables/English > "$OUT/generated/RareModsData.fs"
$N node export/compare.mjs "$OUT/game-direct" ../../data/game > "$OUT/game-direct/compare.txt"
$N node export/whatsnew.mjs ../../data/game "$OUT/game-direct" > "$OUT/game-direct/whatsnew.txt"
grep '^== ' "$OUT/game-direct/compare.txt"
echo "details: $OUT/game-direct/compare.txt, $OUT/game-direct/whatsnew.txt"
