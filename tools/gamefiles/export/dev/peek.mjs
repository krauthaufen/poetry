// Development aid: one row of a table as the schema reads it, and the row's raw bytes, to work out a changed layout.
// Usage: node export/dev/peek.mjs <Table> [row = 0]
import { openGame, columnWidth, columnNames } from '../lib/game.mjs';
const [name, rowArg] = process.argv.slice(2);
const game = await openGame();
const lay = game.tableLayout(name);
const r = Number(rowArg || 0);
console.log(`${name}: ${lay.rowCount} rows of ${lay.rowLength} bytes; the schema describes ${lay.schemaWidth}`);
const row = lay.dat.dataFixed.subarray(r * lay.rowLength, (r + 1) * lay.rowLength);
const hex = (a, b) => [...row.subarray(a, Math.min(b, row.length))].map(x => x.toString(16).padStart(2, '0')).join(' ');
let off = 0;
const names = columnNames(lay.schema);
lay.schema.columns.forEach((c, i) => { const w = columnWidth(c); console.log(String(off).padStart(4), (names[i] + ' ' + c.type + (c.array ? '[]' : '')).padEnd(46), hex(off, off + w)); off += w; });
if (off < lay.rowLength) console.log(String(off).padStart(4), '(not described)'.padEnd(46), hex(off, lay.rowLength));
