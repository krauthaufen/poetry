#!/usr/bin/env node
// Compares a direct export with data/game (or any two export directories), file by file:
// keys on one side only, and for common keys which fields differ, with counts and examples.
// Usage: node export/compare.mjs [ours = out/game-direct] [theirs = data/game] [--file skills.json] [--field <path>] [--examples N] [--keys]
import * as fs from 'node:fs';
import * as path from 'node:path';
import { GAMEFILES_DIR } from './lib/game.mjs';

const args = process.argv.slice(2);
const opt = { examples: 3 };
const pos = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === '--file') opt.file = args[++i];
  else if (args[i] === '--field') opt.field = args[++i];
  else if (args[i] === '--examples') opt.examples = Number(args[++i]);
  else if (args[i] === '--keys') opt.keys = true;
  else pos.push(args[i]);
}
const repo = path.resolve(GAMEFILES_DIR, '..', '..');
const oursDir = path.resolve(pos[0] || path.join(repo, 'out', 'game-direct'));
const theirsDir = path.resolve(pos[1] || path.join(repo, 'data', 'game'));

const isObj = x => x !== null && typeof x === 'object';
const isEmpty = x => isObj(x) && (Array.isArray(x) ? x.length === 0 : Object.keys(x).length === 0);
function numEq(a, b) { return a === b || Math.abs(a - b) <= 1e-6 * Math.max(Math.abs(a), Math.abs(b)); }

/** Calls report(path, a, b) for every leaf that differs. Numeric path parts are written as []. */
function diff(a, b, p, report) {
  if (a === b) return;
  if (typeof a === 'number' && typeof b === 'number') { if (!numEq(a, b)) report(p, a, b); return; }
  if (isEmpty(a) && isEmpty(b)) return;
  if (isObj(a) && isObj(b) && Array.isArray(a) === Array.isArray(b)) {
    if (Array.isArray(a)) {
      if (a.length !== b.length) { report(p + '.#', a.length, b.length); }
      // lists of plain values that only differ in order or content: report once, as a set difference
      if (a.every(x => !isObj(x)) && b.every(x => !isObj(x))) {
        if (a.length === b.length && a.every((x, i) => x === b[i] || (typeof x === 'number' && typeof b[i] === 'number' && numEq(x, b[i])))) return;
        const sa = new Set(a), sb = new Set(b);
        const onlyA = a.filter(x => !sb.has(x)), onlyB = b.filter(x => !sa.has(x));
        if (onlyA.length === 0 && onlyB.length === 0) { if (a.length === b.length) report(p + '.(order)', a.join(','), b.join(',')); }
        else report(p + '[]', onlyA.length ? '+' + JSON.stringify(onlyA) : '', onlyB.length ? '-' + JSON.stringify(onlyB) : '');
        return;
      }
      for (let i = 0; i < Math.min(a.length, b.length); i++) diff(a[i], b[i], p + '[]', report);
      return;
    }
    for (const k of new Set([...Object.keys(a), ...Object.keys(b)])) {
      const kp = /^\d+$/.test(k) ? p + '[]' : (p ? p + '.' + k : k);
      if (!(k in a)) report(kp + ' (missing in ours)', undefined, b[k]);
      else if (!(k in b)) report(kp + ' (only in ours)', a[k], undefined);
      else diff(a[k], b[k], kp, report);
    }
    return;
  }
  report(p + (isObj(a) !== isObj(b) || Array.isArray(a) !== Array.isArray(b) ? ' (shape)' : ''), a, b);
}

// Stat description files number their entries; compare them by the stats an entry describes.
function statdescByStats(d) {
  const o = {};
  for (const [k, v] of Object.entries(d)) {
    if (/^\d+$/.test(k)) o[(v.stats || []).join(' ')] = v;
    else if (k === 'parent') o['(parent)'] = v;
  }
  return o;
}
function normalise(file, d) {
  if (file === 'statdesc_specific.json') {
    const o = {};
    for (const [f, v] of Object.entries(d)) {
      o[f] = { '(file)': true };
      for (const [k, x] of Object.entries(statdescByStats(v))) o[f + ' :: ' + k] = x;
    }
    return o;
  }
  if (file.startsWith('statdesc_')) return statdescByStats(d);
  if (file === 'tree.json') {
    const o = {};
    for (const [k, v] of Object.entries(d.nodes || {})) o['node ' + k] = v;
    (d.classes || []).forEach(c => { o['class ' + c.name] = c; });
    o.jewelSlots = { list: d.jewelSlots };
    o.version = { version: d.version };
    return o;
  }
  return d;
}
// PoB gives 32 minions short names by hand; ours are keyed by the monster id. Match them by name and skills.
function aliasMinions(a, b) {
  const by = new Map();
  for (const [k, v] of Object.entries(a)) {
    const sig = v.name + '|' + (v.skillList || []).join(',');
    if (!by.has(sig)) by.set(sig, []);
    by.get(sig).push(k);
  }
  const alias = {};
  for (const [k, v] of Object.entries(b)) {
    if (k.startsWith('Metadata/') || k in a) continue;
    const cands = by.get(v.name + '|' + (v.skillList || []).join(',')) || [];
    // several monsters can share name and skills: take the one that agrees in most fields
    const score = c => Object.keys(v).filter(f => JSON.stringify(v[f]) === JSON.stringify(a[c][f])).length;
    if (cands.length) alias[k] = cands.reduce((x, y) => (score(y) > score(x) ? y : x));
  }
  return alias;
}
function renameKeys(d, alias) {
  const o = {};
  for (const [k, v] of Object.entries(d)) o[alias[k] || k] = v;
  return o;
}

const short = v => { const s = v === undefined ? '-' : JSON.stringify(v); return s.length > 110 ? s.slice(0, 107) + '...' : s; };

function compareFile(file) {
  const pa = path.join(oursDir, file), pb = path.join(theirsDir, file);
  if (!fs.existsSync(pa) || !fs.existsSync(pb)) { console.log(`\n== ${file}: ${fs.existsSync(pa) ? 'missing in ' + theirsDir : 'missing in ' + oursDir}`); return; }
  const a = normalise(file, JSON.parse(fs.readFileSync(pa, 'utf8')));
  let b = normalise(file, JSON.parse(fs.readFileSync(pb, 'utf8')));
  const minionFiles = ['minions.json'].map(f => [path.join(oursDir, f), path.join(theirsDir, f)]).filter(([x, y]) => fs.existsSync(x) && fs.existsSync(y));
  if (minionFiles.length && (file === 'minions.json' || file === 'skills.json')) {
    const alias = aliasMinions(JSON.parse(fs.readFileSync(minionFiles[0][0], 'utf8')), JSON.parse(fs.readFileSync(minionFiles[0][1], 'utf8')));
    if (file === 'minions.json') b = renameKeys(b, alias);
    else {
      // monsters with the same name and skills count as the same monster
      const ours = JSON.parse(fs.readFileSync(minionFiles[0][0], 'utf8'));
      const first = new Map(), canon = {};
      for (const [k, v] of Object.entries(ours)) { const sig = v.name + '|' + (v.skillList || []).join(','); if (!first.has(sig)) first.set(sig, k); canon[k] = first.get(sig); }
      for (const v of Object.values(b)) if (Array.isArray(v.minionList)) v.minionList = v.minionList.map(m => canon[alias[m] || m] || m);
      for (const v of Object.values(a)) if (Array.isArray(v.minionList)) v.minionList = v.minionList.map(m => canon[m] || m);
    }
  }
  const onlyA = Object.keys(a).filter(k => !(k in b)), onlyB = Object.keys(b).filter(k => !(k in a));
  const common = Object.keys(a).filter(k => k in b);
  const fields = new Map();   // field -> { keys: Set, examples: [] }
  const differing = new Set();
  for (const k of common) {
    diff(a[k], b[k], '', (p, x, y) => {
      differing.add(k);
      let f = fields.get(p);
      if (!f) fields.set(p, f = { keys: new Set(), examples: [] });
      if (!f.keys.has(k) && f.examples.length < (opt.field === p ? 1e9 : opt.examples)) f.examples.push([k, x, y]);
      f.keys.add(k);
    });
  }
  console.log(`\n== ${file}: same ${common.length - differing.size}, differing ${differing.size}, only ours ${onlyA.length}, only theirs ${onlyB.length}`);
  if (opt.field) {
    const f = fields.get(opt.field);
    if (f) for (const [k, x, y] of f.examples) console.log(`   ${k}: ours ${short(x)} | theirs ${short(y)}`);
    return;
  }
  const show = (label, keys) => { if (keys.length) console.log(`  ${label} (${keys.length}): ${opt.keys ? keys.join(', ') : keys.slice(0, 12).join(', ') + (keys.length > 12 ? ', ...' : '')}`); };
  show('only ours', onlyA);
  show('only theirs', onlyB);
  for (const [p, f] of [...fields].sort((x, y) => y[1].keys.size - x[1].keys.size)) {
    console.log(`  ${String(f.keys.size).padStart(5)}  ${p || '(value)'}`);
    for (const [k, x, y] of f.examples) console.log(`           ${k}: ours ${short(x)} | theirs ${short(y)}`);
  }
}

const files = opt.file ? [opt.file] : fs.readdirSync(theirsDir).filter(f => f.endsWith('.json')).sort();
console.log(`ours:   ${oursDir}\ntheirs: ${theirsDir}`);
for (const f of files) compareFile(f);
