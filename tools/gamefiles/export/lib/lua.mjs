// JSON written the way tools/export/export.lua writes Lua tables, so the files have the same shape:
// an empty table is {}, a table with keys 1..n is an array, anything else an object with sorted keys.
import * as fs from 'node:fs';
import * as path from 'node:path';

function num(v) {
  if (!Number.isFinite(v)) return 'null';
  if (Number.isInteger(v) && Math.abs(v) < 2 ** 53) return String(v);
  return String(Number(v.toPrecision(10)));
}

export function luaJson(v) {
  if (v === null || v === undefined) return 'null';
  const t = typeof v;
  if (t === 'string') return JSON.stringify(v);
  if (t === 'number') return num(v);
  if (t === 'boolean') return v ? 'true' : 'false';
  if (Array.isArray(v)) {
    if (v.length === 0) return '{}';
    return '[' + v.map(luaJson).join(',') + ']';
  }
  if (v instanceof LuaTable) v = v.toValue();
  if (Array.isArray(v)) return luaJson(v);
  const keys = Object.keys(v).filter(k => v[k] !== undefined && v[k] !== null).sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
  if (keys.length === 0) return '{}';
  return '{' + keys.map(k => JSON.stringify(k) + ':' + luaJson(v[k])).join(',') + '}';
}

/** A Lua table with an array part and named keys (a level row: values 1..n plus named fields). */
export class LuaTable {
  constructor(list = [], named = {}) { this.list = list; this.named = named; }
  toValue() {
    const namedKeys = Object.keys(this.named).filter(k => this.named[k] !== undefined && this.named[k] !== null);
    if (namedKeys.length === 0) return this.list;
    const o = {};
    this.list.forEach((x, i) => { o[String(i + 1)] = x; });
    for (const k of namedKeys) o[k] = this.named[k];
    return o;
  }
}

/** A Lua table keyed by integers (gem levels): an array when the keys are exactly 1..n, else an object. */
export function byInt(pairs) {
  const sorted = [...pairs].sort((a, b) => a[0] - b[0]);
  if (sorted.every(([k], i) => k === i + 1)) return sorted.map(p => p[1]);
  const o = {};
  for (const [k, v] of sorted) o[String(k)] = v;
  return o;
}

/** { a: true, b: true } from a list of names. */
export function flagSet(names) {
  const o = {};
  for (const n of names) o[n] = true;
  return o;
}

export function writeJson(outDir, name, value) {
  fs.mkdirSync(outDir, { recursive: true });
  if (value && typeof value.text === 'string' && Object.keys(value).length === 1) { fs.writeFileSync(path.join(outDir, name), value.text); console.log(`wrote ${name}`); return; }
  // { plainJson: value } is written as ordinary JSON (empty lists stay lists): files that are not read like data/game
  const plain = value && value.plainJson !== undefined && Object.keys(value).length === 1;
  if (plain) value = value.plainJson;
  const text = plain ? JSON.stringify(value) : luaJson(value);
  fs.writeFileSync(path.join(outDir, name), text);
  const n = Array.isArray(value) ? value.length : Object.keys(value).length;
  console.log(`wrote ${name} (${n} entries, ${(text.length / 1024).toFixed(0)} kB)`);
}
