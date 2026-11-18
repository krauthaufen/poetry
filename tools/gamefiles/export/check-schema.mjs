#!/usr/bin/env node
// Patch-day check: does the dat schema still fit the game's tables?
// For every table the exporter reads (export/tables-used.json) it compares the row width the schema
// describes with the row width of the real .datc64 file, and looks at every column for values that
// cannot be right (a key pointing past the end of its table, a string outside the data, a flag that
// is not 0 or 1), which is what a shifted layout looks like. Also checks the other files read.
// Usage: node export/check-schema.mjs [--all] [Table ...]      exit code 1 when something does not fit
import * as fs from 'node:fs';
import * as path from 'node:path';
import { openGame, columnWidth, columnNames, GAMEFILES_DIR } from './lib/game.mjs';

const args = process.argv.slice(2);
const game = await openGame();
let names = args.filter(a => !a.startsWith('--'));
if (!names.length) names = JSON.parse(fs.readFileSync(path.join(GAMEFILES_DIR, 'export', 'tables-used.json'), 'utf8'));
if (args.includes('--all')) names = [...game.schema.tables.values()].filter(t => t.validFor & 2).map(t => t.name).sort();

const rowCounts = new Map();
const rowCount = name => {
  const k = name.toLowerCase();
  if (!rowCounts.has(k)) { const l = game.tableLayout(name); rowCounts.set(k, l.missing ? null : l.rowCount); }
  return rowCounts.get(k);
};
const NULL32 = 0xfefefefe;

/** Per column: how many rows hold a value that cannot be right. */
function implausible(lay) {
  const { dat, schema } = lay;
  const fixed = dat.readerFixed, varLen = dat.dataVariable.byteLength;
  const names_ = columnNames(schema);
  const out = [];
  let off = 0;
  schema.columns.forEach((c, ci) => {
    const w = columnWidth(c);
    if (off + w > lay.rowLength) { out.push({ name: names_[ci], offset: off, bad: lay.rowCount, why: 'beyond the row' }); off += w; return; }
    const target = c.references && c.references.table && (c.type === 'foreignrow' || c.type === 'row') ? (c.type === 'row' ? lay.rowCount : rowCount(c.references.table)) : null;
    const elem = { bool: 1, string: 8, row: 8, foreignrow: 16, enumrow: 4, i16: 2, u16: 2, i32: 4, u32: 4, f32: 4 }[c.type] || 1;
    let bad = 0, why = '';
    for (let r = 0; r < lay.rowCount; r++) {
      const p = r * lay.rowLength + off;
      if (c.array) {
        const n = Number(fixed.getBigUint64(p, true)), o = Number(fixed.getBigUint64(p + 8, true));
        if (n > 1e6 || o > varLen || (n > 0 && c.type !== 'array' && o + n * elem > varLen)) { bad++; why = 'list outside the data'; }
        else if (target !== null && n > 0) {
          const k = dat.readerVariable.getUint32(o, true);
          if (k !== NULL32 && k >= target) { bad++; why = `key past the end of ${c.references.table}`; }
        }
      } else if (c.type === 'bool') { if (fixed.getUint8(p) > 1) { bad++; why = 'flag not 0 or 1'; } }
      else if (c.type === 'string') { const o = Number(fixed.getBigUint64(p, true)); if (o < 8 || o >= varLen) { bad++; why = 'text outside the data'; } }
      else if (c.type === 'foreignrow' || c.type === 'row') {
        const k = fixed.getUint32(p, true), hi = fixed.getUint32(p + 4, true);
        if (k === NULL32) { if (hi !== NULL32) { bad++; why = 'half a null key'; } }
        else if (hi !== 0 || (target !== null && k >= target)) { bad++; why = target !== null ? `key past the end of ${c.references.table}` : 'not a key'; }
      } else if (c.type === 'f32') { const v = fixed.getFloat32(p, true); if (!Number.isFinite(v) || (v !== 0 && (Math.abs(v) < 1e-12 || Math.abs(v) > 1e12))) { bad++; why = 'not a sensible number'; } }
    }
    if (bad) out.push({ name: names_[ci], offset: off, bad, why });
    off += w;
  });
  return out;
}

let problems = 0, warnings = 0;
const lines = [];
for (const name of names) {
  const lay = game.tableLayout(name);
  if (!lay.schema) { lines.push(`MISSING IN SCHEMA  ${name}`); problems++; continue; }
  if (lay.missing) { lines.push(`NO SUCH FILE       ${name}   (${game.datPath(name)}: renamed or removed?)`); problems++; continue; }
  const cols = lay.rowCount ? implausible(lay) : [];
  const width = `schema ${lay.schemaWidth} bytes, file ${lay.rowLength} bytes, ${lay.rowCount} rows`;
  let status = 'ok';
  if (lay.rowCount === 0) status = 'ok (empty)';
  else if (lay.schemaWidth > lay.rowLength) status = 'TOO WIDE';
  else if (cols.length) status = 'SHIFTED?';
  else if (lay.schemaWidth < lay.rowLength) status = 'new columns';
  if (status === 'TOO WIDE' || status === 'SHIFTED?') problems++;
  if (status === 'new columns') warnings++;
  if (status === 'ok' || status === 'ok (empty)') { if (args.includes('--verbose')) lines.push(`ok                 ${name}   ${width}`); continue; }
  lines.push(`${status.padEnd(18)} ${name}   ${width}${status === 'new columns' ? `: ${lay.rowLength - lay.schemaWidth} bytes at the end of a row are not described (readable as it is, if they were added at the end)` : ''}`);
  for (const c of cols.slice(0, 4)) lines.push(`${' '.repeat(21)}column ${c.name} at byte ${c.offset}: ${c.bad} of ${lay.rowCount} rows, ${c.why}`);
  if (cols.length) lines.push(`${' '.repeat(21)}the layout is wrong from the first of these columns, or shortly before it`);
}

// ---- the other files the exporter reads ----
const other = [];
const tree = (() => { try { return game.rowBy('PassiveSkillTrees', 'Id', 'Default'); } catch { return null; } })();
if (!tree) other.push('passive tree: no row "Default" in PassiveSkillTrees');
else {
  const psg = game.readFile(tree.PassiveSkillGraph + '.psg');
  if (!psg) other.push(`passive tree: graph file ${tree.PassiveSkillGraph}.psg is missing`);
  else { const v = new DataView(psg.buffer, psg.byteOffset).getUint16(0, true); if (v !== 3) other.push(`passive tree: graph file has version ${v}, export/tree.mjs reads version 3`); }
}
for (const f of ['stat_descriptions', 'passive_skill_stat_descriptions', 'gem_stat_descriptions', 'skill_stat_descriptions', 'active_skill_gem_stat_descriptions', 'meta_gem_stat_descriptions']) {
  if (!game.readFile(`data/statdescriptions/${f}.csd`)) other.push(`stat descriptions: data/statdescriptions/${f}.csd is missing`);
}
for (const f of ['metadata/monsters/monster.ot', 'metadata/items/item.it']) if (!game.readFile(f)) other.push(`missing file ${f}`);
problems += other.length;

console.log(`game:   ${game.gameDir}`);
console.log(`schema: ${new Date(game.schema.createdAt * 1000).toISOString().slice(0, 10)} (schema.min.json) + export/schema-overrides.json`);
console.log(`${names.length} tables checked: ${names.length - problems + other.length - warnings} fit, ${warnings} with new columns at the end, ${problems - other.length} do not fit`);
for (const l of lines) console.log(l);
for (const l of other) console.log('OTHER FILE         ' + l);
if (problems) { console.log('\nFix the layouts in export/schema-overrides.json (see PATCHDAY.md), then run this again.'); process.exitCode = 1; }
