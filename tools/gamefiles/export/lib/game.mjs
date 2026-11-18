// Access to the installed game: raw files out of the bundles and .datc64 tables.
//
// Tables are decoded with the community dat schema (../../schema.min.json), patched by
// ../schema-overrides.json. Every decoded table is cached as JSON under
// ~/.cache/poetry-export/tables/<stamp>/, where the stamp changes with the game's bundle
// index, the schema and the overrides, so nothing is decoded twice.
import * as fs from 'node:fs';
import * as path from 'node:path';
import * as os from 'node:os';
import * as crypto from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { decompressSliceInBundle, decompressedBundleSize, readIndexBundle, getDirContent, getRootDirs } from 'pathofexile-dat/bundles.js';
import { readDatFile, readColumn } from 'pathofexile-dat/dat.js';

const here = path.dirname(fileURLToPath(import.meta.url));
export const GAMEFILES_DIR = path.resolve(here, '..', '..');
export const CACHE_ROOT = process.env.POETRY_EXPORT_CACHE || path.join(os.homedir(), '.cache', 'poetry-export');
const VALID_POE2 = 2;
const utf16 = new TextDecoder('utf-16le');
const utf8 = new TextDecoder('utf-8');
const utf8enc = new TextEncoder();

/** MurmurHash64A with the seed the bundle index uses for file paths. */
function murmur64a(text) {
  const data = utf8enc.encode(text);
  const M = 0xc6a4a7935bd1e995n, R = 47n, U = 64;
  let h = 0x1337b33fn ^ BigInt.asUintN(U, BigInt(data.length) * M);
  const rem = data.length & 7, aligned = data.length - rem;
  const dv = new DataView(data.buffer, data.byteOffset, data.byteLength);
  for (let i = 0; i < aligned; i += 8) {
    let k = BigInt.asUintN(U, dv.getBigUint64(i, true) * M);
    k ^= k >> R;
    k = BigInt.asUintN(U, k * M);
    h ^= k;
    h = BigInt.asUintN(U, h * M);
  }
  if (rem) {
    for (let i = rem - 1; i >= 0; i--) h ^= BigInt(data[aligned + i]) << BigInt(8 * i);
    h = BigInt.asUintN(U, h * M);
  }
  h ^= h >> R;
  h = BigInt.asUintN(U, h * M);
  h ^= h >> R;
  return h;
}

const FIELD = { bool: 1, string: 8, row: 8, foreignrow: 16, array: 16, enumrow: 4, i16: 2, u16: 2, i32: 4, u32: 4, f32: 4, i8: 1, u8: 1, i64: 8, u64: 8, f64: 8 };

/** Bytes a schema column takes in a row. */
export function columnWidth(col) {
  if (col.array) return FIELD.array;
  const w = FIELD[col.type];
  if (w === undefined) throw new Error('unknown column type ' + col.type);
  return col.interval ? w * 2 : w;
}

function toHeader(col, offset) {
  const t = col.type;
  const int = { u8: [true, 1], u16: [true, 2], u32: [true, 4], u64: [true, 8], i8: [false, 1], i16: [false, 2], i32: [false, 4], i64: [false, 8], enumrow: [false, 4] }[t];
  return {
    name: col.name || '',
    offset,
    type: {
      // a column of type "array" is an array of unknown element type: read as empty
      array: col.array, interval: col.interval,
      integer: int ? { unsigned: int[0], size: int[1] } : undefined,
      decimal: t === 'f32' ? { size: 4 } : t === 'f64' ? { size: 8 } : undefined,
      string: t === 'string' ? {} : undefined,
      boolean: t === 'bool' ? {} : undefined,
      key: t === 'row' || t === 'foreignrow' ? { foreign: t === 'foreignrow' } : undefined,
    },
  };
}

export function loadConfig() {
  return JSON.parse(fs.readFileSync(path.join(GAMEFILES_DIR, 'config.json'), 'utf8'));
}

/** The schema (community + overrides): table name (lower case) -> { name, columns }. */
export function loadSchema() {
  const schemaPath = path.join(GAMEFILES_DIR, 'schema.min.json');
  const overridesPath = path.join(GAMEFILES_DIR, 'export', 'schema-overrides.json');
  const schemaText = fs.readFileSync(schemaPath, 'utf8');
  const overridesText = fs.existsSync(overridesPath) ? fs.readFileSync(overridesPath, 'utf8') : '{}';
  const schema = JSON.parse(schemaText);
  const overrides = JSON.parse(overridesText);
  const tables = new Map();
  for (const t of schema.tables) {
    const key = t.name.toLowerCase();
    // a table can be listed once for each game: prefer the PoE2 one
    if (!tables.has(key) || (t.validFor & VALID_POE2)) {
      if (tables.has(key) && (tables.get(key).validFor & VALID_POE2) && !(t.validFor & VALID_POE2)) continue;
      tables.set(key, { name: t.name, validFor: t.validFor, columns: t.columns.map(c => ({ ...c })) });
    }
  }
  const enums = new Map();
  for (const e of schema.enumerations) enums.set(e.name.toLowerCase(), e);
  for (const [name, o] of Object.entries(overrides.tables || {})) {
    const key = name.toLowerCase();
    if (o.columns) {
      // whole layout given by hand
      tables.set(key, { name, validFor: VALID_POE2, byHand: true, columns: o.columns.map(c => ({ array: false, interval: false, references: null, ...c })) });
      continue;
    }
    const t = tables.get(key);
    if (!t) throw new Error('schema-overrides.json: no table ' + name + ' in the schema; give "columns"');
    // name columns the schema leaves unnamed, by column number (0-based)
    for (const [idx, patch] of Object.entries(o.patch || {})) {
      const col = t.columns[Number(idx)];
      if (!col) throw new Error(`schema-overrides.json: ${name} has no column ${idx}`);
      Object.assign(col, typeof patch === 'string' ? { name: patch } : patch);
    }
    // columns added at the end of the row
    for (const c of o.append || []) t.columns.push({ array: false, interval: false, references: null, ...c });
  }
  const stamp = crypto.createHash('sha1').update(schemaText).update(overridesText).digest('hex').slice(0, 10);
  return { tables, enums, stamp, createdAt: schema.createdAt };
}

/** Column names as used in the decoded rows: unnamed columns are "_<number>". */
export function columnNames(tableSchema) {
  const seen = new Set();
  return tableSchema.columns.map((c, i) => {
    let n = c.name || '_' + i;
    if (seen.has(n)) n = n + '_' + i;
    seen.add(n);
    return n;
  });
}

export async function openGame(opts = {}) {
  const config = loadConfig();
  const gameDir = opts.gameDir || process.env.POE2_DIR || config.steam;
  const bundlesDir = path.join(gameDir, 'Bundles2');
  const indexPath = path.join(bundlesDir, '_.index.bin');
  const indexStat = fs.statSync(indexPath);
  const schema = loadSchema();
  const stamp = `${indexStat.size}-${Math.floor(indexStat.mtimeMs / 1000)}-${schema.stamp}`;
  const tableCache = path.join(CACHE_ROOT, 'tables', stamp);

  let index = null;      // loaded on first use: with a warm cache the bundles are never opened
  const bundles = new Map();
  function loadIndex() {
    if (index) return index;
    const raw = fs.readFileSync(indexPath);
    const ib = new Uint8Array(decompressedBundleSize(raw));
    decompressSliceInBundle(raw, 0, ib);
    const i = readIndexBundle(ib);
    const pr = new Uint8Array(decompressedBundleSize(i.pathRepsBundle));
    decompressSliceInBundle(i.pathRepsBundle, 0, pr);
    // bundle names, and file hash -> record: the library's own lookup scans the whole index for every file
    const bundleNames = [];
    const bv = new DataView(i.bundlesInfo.buffer, i.bundlesInfo.byteOffset, i.bundlesInfo.byteLength);
    for (let o = 0; o < i.bundlesInfo.byteLength;) {
      const len = bv.getInt32(o, true);
      bundleNames.push(utf8.decode(i.bundlesInfo.subarray(o + 4, o + 4 + len)) + '.bundle.bin');
      o += 4 + len + 4;
    }
    const fv = new DataView(i.filesInfo.buffer, i.filesInfo.byteOffset, i.filesInfo.byteLength);
    // open-addressing table over the low half of the 64 bit path hash -> record offset
    const count = i.filesInfo.byteLength / 20;
    let size = 1; while (size < count * 2) size *= 2;
    const slots = new Int32Array(size).fill(-1);
    for (let o = 0; o < i.filesInfo.byteLength; o += 20) {
      let h = fv.getUint32(o, true) & (size - 1);
      while (slots[h] !== -1) h = (h + 1) & (size - 1);
      slots[h] = o;
    }
    const files = {
      get(hash) {
        const lo = Number(hash & 0xffffffffn), hi = Number(hash >> 32n);
        for (let h = lo & (size - 1); slots[h] !== -1; h = (h + 1) & (size - 1)) {
          const o = slots[h];
          if (fv.getUint32(o, true) === lo && fv.getUint32(o + 4, true) === hi) return o;
        }
        return undefined;
      },
    };
    index = { bundleNames, files, filesView: fv, dirsInfo: i.dirsInfo, pathReps: pr };
    return index;
  }
  function bundle(name) {
    let b = bundles.get(name);
    if (!b) {
      b = fs.readFileSync(path.join(bundlesDir, name));
      // keep a few bundles only: they are large
      if (bundles.size >= 64) bundles.delete(bundles.keys().next().value);
      bundles.set(name, b);
    }
    return b;
  }

  /** Raw bytes of a game file (path as in the game, any case), or null. */
  function readFile(file) {
    const ix = loadIndex();
    const o = ix.files.get(murmur64a(file.toLowerCase()));
    if (o === undefined) return null;
    const out = new Uint8Array(ix.filesView.getInt32(o + 16, true));
    decompressSliceInBundle(bundle(ix.bundleNames[ix.filesView.getInt32(o + 8, true)]), ix.filesView.getInt32(o + 12, true), out);
    return out;
  }
  /** A text file (UTF-16LE, as the game stores them), or null. */
  function readText(file) {
    const b = readFile(file);
    if (!b) return null;
    let t = utf16.decode(b);
    if (t.charCodeAt(0) === 0xfeff) t = t.slice(1);
    return t;
  }
  function listDir(dir) {
    const ix = loadIndex();
    return getDirContent(dir.toLowerCase().replace(/\/$/, ''), ix.pathReps, ix.dirsInfo);
  }
  function roots() {
    const ix = loadIndex();
    return getRootDirs(ix.pathReps, ix.dirsInfo);
  }

  function datPath(name) { return `data/balance/${name.toLowerCase()}.datc64`; }

  /** The real dat file: { rowCount, rowLength } and the schema's idea of the row. */
  function tableLayout(name) {
    const sch = schema.tables.get(name.toLowerCase());
    const bytes = readFile(datPath(name));
    if (!bytes) return { name, missing: true, schema: sch };
    const dat = readDatFile('.datc64', bytes);
    const schemaWidth = sch ? sch.columns.reduce((a, c) => a + columnWidth(c), 0) : null;
    return { name, rowCount: dat.rowCount, rowLength: dat.rowLength, schemaWidth, schema: sch, dat };
  }

  const tables = new Map();
  const used = new Set();
  /** All rows of a table as objects (all columns; keys to other tables are row numbers). */
  function table(name) {
    const key = name.toLowerCase();
    used.add(key);
    if (tables.has(key)) return tables.get(key);
    const cacheFile = path.join(tableCache, key + '.json');
    let rows;
    if (fs.existsSync(cacheFile)) {
      rows = JSON.parse(fs.readFileSync(cacheFile, 'utf8'));
    } else {
      const lay = tableLayout(name);
      if (!lay.schema) throw new Error(`no schema for table ${name}`);
      if (lay.missing) throw new Error(`the game has no table ${name} (${datPath(name)})`);
      if (lay.rowCount > 0 && lay.schemaWidth > lay.rowLength) {
        throw new Error(`table ${name}: the schema describes ${lay.schemaWidth} bytes a row, the file has ${lay.rowLength}. Fix the layout (see PATCHDAY.md).`);
      }
      const names = columnNames(lay.schema);
      let offset = 0;
      const cols = [];
      lay.schema.columns.forEach((c, i) => {
        const h = toHeader(c, offset);
        offset += columnWidth(c);
        if (c.type === 'array') return;   // unknown element type
        cols.push([names[i], readColumn(h, lay.dat)]);
      });
      rows = [];
      for (let r = 0; r < lay.rowCount; r++) {
        const row = { _index: r };
        for (const [n, data] of cols) row[n] = data[r];
        rows.push(row);
      }
      fs.mkdirSync(tableCache, { recursive: true });
      fs.writeFileSync(cacheFile, JSON.stringify(rows));
    }
    tables.set(key, rows);
    return rows;
  }

  const indexes = new Map();
  /** Rows of a table grouped by a column (array columns: by each element). */
  function index_(name, column) {
    const k = name.toLowerCase() + '\0' + column;
    let m = indexes.get(k);
    if (!m) {
      m = new Map();
      for (const row of table(name)) {
        const v = row[column];
        for (const x of Array.isArray(v) ? v : [v]) {
          if (x === null || x === undefined) continue;
          if (!m.has(x)) m.set(x, []);
          m.get(x).push(row);
        }
      }
      indexes.set(k, m);
    }
    return m;
  }
  const rowsBy = (name, column, value) => index_(name, column).get(value) || [];
  const rowBy = (name, column, value) => rowsBy(name, column, value)[0] || null;
  /** The row a key column points at (null for a null key). */
  const ref = (name, idx) => (idx === null || idx === undefined) ? null : (table(name)[idx] || null);
  const refs = (name, list) => (list || []).map(i => ref(name, i)).filter(r => r);

  /** Remove caches of older game versions or schemas. */
  function pruneCache() {
    const root = path.join(CACHE_ROOT, 'tables');
    if (!fs.existsSync(root)) return;
    for (const d of fs.readdirSync(root)) if (d !== stamp) fs.rmSync(path.join(root, d), { recursive: true, force: true });
  }

  return { gameDir, schema, stamp, readFile, readText, listDir, roots, table, tableLayout, rowBy, rowsBy, ref, refs, pruneCache, usedTables: () => [...used].sort(), datPath };
}
