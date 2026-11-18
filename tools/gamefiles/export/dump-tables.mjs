#!/usr/bin/env node
// Writes tables/English/<Table>.json for the tables and columns listed in ../config.json, as
// `npx pathofexile-dat` does, but with the local schema (schema.min.json + schema-overrides.json)
// and without the network. gen_rarity.py and gen_rare_mods.py read these files.
// Usage: node export/dump-tables.mjs [out dir = tables/English]
import * as fs from 'node:fs';
import * as path from 'node:path';
import { openGame, loadConfig, GAMEFILES_DIR } from './lib/game.mjs';

const outDir = path.resolve(process.argv[2] || path.join(GAMEFILES_DIR, 'tables', 'English'));
const game = await openGame();
fs.mkdirSync(outDir, { recursive: true });
for (const t of loadConfig().tables) {
  const rows = game.table(t.name);
  const missing = t.columns.filter(c => rows.length && !(c in rows[0]));
  if (missing.length) { console.error(`table ${t.name} has no column ${missing.join(', ')} in the schema`); process.exitCode = 1; continue; }
  const want = new Set(t.columns);
  // columns in the schema's order, as the pathofexile-dat tool writes them
  const out = rows.map(r => { const o = { _index: r._index }; for (const c of Object.keys(r)) if (want.has(c)) o[c] = r[c]; return o; });
  fs.writeFileSync(path.join(outDir, t.name + '.json'), JSON.stringify(out, null, 2));
  console.log(`wrote ${t.name}.json (${rows.length} rows)`);
}
