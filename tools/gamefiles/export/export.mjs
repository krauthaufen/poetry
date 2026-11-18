#!/usr/bin/env node
// Exports the simulator's game data (data/game/*.json) straight from the installed game.
// Usage: node export/export.mjs [out dir] [--only statdesc,skills,...] [--all-effects] [--tree-version 0_5]
//   --all-effects: every granted effect of the game (8000+, mostly monster skills), not only gems', players' and minions'
//   out dir defaults to <repo>/out/game-direct
import * as path from 'node:path';
import * as fs from 'node:fs';
import { openGame, loadConfig, GAMEFILES_DIR } from './lib/game.mjs';
import { writeJson } from './lib/lua.mjs';

const args = process.argv.slice(2);
let only = null;
const ctx = {};
const pos = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === '--only') only = args[++i].split(',');
  else if (args[i] === '--all-effects') ctx.allEffects = true;
  else if (args[i] === '--tree-version') ctx.treeVersion = args[++i];
  else pos.push(args[i]);
}
const outDir = path.resolve(pos[0] || path.join(GAMEFILES_DIR, '..', '..', 'out', 'game-direct'));
const steps = ['statdesc', 'minions', 'skills', 'bases', 'mods', 'tree'];
const game = await openGame();
console.log(`game: ${game.gameDir}\nout:  ${outDir}`);
ctx.outDir = outDir;
// which stat ids are used by what (skills, mods, passives): written as stats-used.json for whatsnew.mjs
const statUse = {};
ctx.use = (kind, stat, example) => {
  const u = statUse[stat] || (statUse[stat] = { skills: 0, mods: 0, passives: 0, examples: [] });
  u[kind]++;
  if (u.examples.length < 3 && !u.examples.includes(example)) u.examples.push(example);
};
for (const step of steps) {
  if (only && !only.includes(step)) continue;
  const t0 = Date.now();
  const mod = await import(`./${step}.mjs`);
  const files = await mod.run(game, ctx);
  for (const [name, value] of Object.entries(files)) writeJson(outDir, name, value);
  console.log(`  ${step}: ${((Date.now() - t0) / 1000).toFixed(1)} s`);
}
// which tables the export read: check-schema.mjs checks exactly these (kept in the repository, see PATCHDAY.md)
if (!only) {
  writeJson(outDir, 'stats-used.json', { plainJson: statUse });
  const { generate } = await import('./tables.mjs');
  generate(game);
  for (const t of loadConfig().tables) game.table(t.name);
  const listFile = path.join(GAMEFILES_DIR, 'export', 'tables-used.json');
  const names = game.usedTables().map(k => game.schema.tables.get(k).name);
  const text = JSON.stringify(names, null, 1) + '\n';
  if (!fs.existsSync(listFile) || fs.readFileSync(listFile, 'utf8') !== text) { fs.writeFileSync(listFile, text); console.log(`updated export/tables-used.json (${names.length} tables)`); }
}
game.pruneCache();
