// Development aid: a table's columns in the community schema beside PoB's spec.lua, by byte offset.
// Usage: node export/dev/cols.mjs <Table> [path/to/PoB/src/Export/spec.lua]
import * as fs from 'node:fs';
import { openGame, columnWidth, columnNames } from '../lib/game.mjs';
const [name, specPath] = process.argv.slice(2);
const game = await openGame();
const lay = game.tableLayout(name);
console.log(`${name}: rows ${lay.rowCount}, row bytes ${lay.rowLength}, schema bytes ${lay.schemaWidth}`);
const ours = [];
if (lay.schema) { let off = 0; const names = columnNames(lay.schema); lay.schema.columns.forEach((c, i) => { ours.push([off, `${names[i]} ${c.type}${c.array ? '[]' : ''}${c.references ? '->' + c.references.table : ''}`]); off += columnWidth(c); }); }
const theirs = [];
if (specPath) {
  const src = fs.readFileSync(specPath, 'utf8');
  const m = src.match(new RegExp(`\\n\\t${name.toLowerCase()}=\\{\\n([\\s\\S]*?)\\n\\t\\}`));
  if (m) {
    const W = { Bool: 1, Int: 4, UInt: 4, Float: 4, String: 8, Enum: 4, ShortKey: 8, Key: 16, Interval: 8, UInt16: 2 };
    let off = 0;
    for (const b of m[1].matchAll(/\[\d+\]=\{([\s\S]*?)\n\t\t\}/g)) {
      const f = k => (b[1].match(new RegExp(k + '="?([^",\\n]*)"?')) || [])[1] || '';
      theirs.push([off, `${f('name') || '?'} ${f('type')}${f('list') === 'true' ? '[]' : ''}${f('refTo') ? '->' + f('refTo') : ''}`]);
      off += f('list') === 'true' ? 16 : (W[f('type')] ?? NaN);
    }
    console.log('spec.lua bytes', off);
  }
}
const offs = [...new Set([...ours.map(x => x[0]), ...theirs.map(x => x[0])])].sort((a, b) => a - b);
for (const o of offs) console.log(String(o).padStart(4), (ours.find(x => x[0] === o)?.[1] ?? '').padEnd(60), theirs.find(x => x[0] === o)?.[1] ?? '');
