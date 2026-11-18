#!/usr/bin/env node
// What is new between two exports: added and removed skills, supports, gems, bases, mods and passive nodes,
// those that changed, and the stat ids that appear for the first time, most used first.
// Usage: node export/whatsnew.mjs <old dir> <new dir> [--all] [--changed]
//   --all      list everything (default: at most 40 names per list)
//   --changed  also list what changed, not only the count
import * as fs from 'node:fs';
import * as path from 'node:path';

const args = process.argv.slice(2);
const dirs = args.filter(a => !a.startsWith('--'));
if (dirs.length !== 2) { console.error('usage: node export/whatsnew.mjs <old dir> <new dir> [--all] [--changed]'); process.exit(2); }
const all = args.includes('--all'), showChanged = args.includes('--changed');
const load = (dir, f) => { const p = path.join(dir, f); return fs.existsSync(p) ? JSON.parse(fs.readFileSync(p, 'utf8')) : null; };
const [oldDir, newDir] = dirs.map(d => path.resolve(d));
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

function section(title, oldMap, newMap, label, ignore = []) {
  if (!oldMap || !newMap) { console.log(`\n## ${title}: not in both exports`); return; }
  const strip = v => { if (!ignore.length || typeof v !== 'object') return v; const o = { ...v }; for (const k of ignore) delete o[k]; return o; };
  const added = Object.keys(newMap).filter(k => !(k in oldMap));
  const removed = Object.keys(oldMap).filter(k => !(k in newMap));
  const changed = Object.keys(newMap).filter(k => k in oldMap && !same(strip(oldMap[k]), strip(newMap[k])));
  console.log(`\n## ${title}: ${added.length} added, ${removed.length} removed, ${changed.length} changed (of ${Object.keys(newMap).length})`);
  const show = (word, keys, m) => {
    if (!keys.length) return;
    const names = keys.map(k => label(k, m[k])).sort();
    const cut = all ? names : names.slice(0, 40);
    console.log(`  ${word}: ${cut.join(', ')}${names.length > cut.length ? `, ... (${names.length - cut.length} more, --all)` : ''}`);
  };
  show('added', added, newMap);
  show('removed', removed, oldMap);
  if (showChanged) show('changed', changed, newMap);
}

console.log(`old: ${oldDir}\nnew: ${newDir}`);
const oS = load(oldDir, 'skills.json'), nS = load(newDir, 'skills.json');
const split = (s, f) => s && Object.fromEntries(Object.entries(s).filter(([k, v]) => f(k, v)));
const named = (k, v) => (v && v.name && v.name !== k ? `${v.name} [${k}]` : k);
const isSupport = (k, v) => v.support === true;
const isGemSkill = (k, v) => !v.support && !v.hidden;
section('Skills (gems)', split(oS, isGemSkill), split(nS, isGemSkill), named, ['baseFlags']);
section('Supports', split(oS, isSupport), split(nS, isSupport), named, ['baseFlags']);
section('Other skills (triggered, from items, of minions and monsters)', split(oS, (k, v) => !v.support && v.hidden), split(nS, (k, v) => !v.support && v.hidden), named, ['baseFlags']);
section('Gems', load(oldDir, 'gems.json'), load(newDir, 'gems.json'), (k, v) => `${v.name}${v.tags && v.tags.support ? ' (support)' : ''}`);
section('Item bases', load(oldDir, 'bases.json'), load(newDir, 'bases.json'), (k, v) => `${k} (${v.type})`);
section('Mods', load(oldDir, 'mods.json'), load(newDir, 'mods.json'), (k, v) => `${k}${v.lines && v.lines.length ? ' "' + v.lines.join(' / ') + '"' : ''}`);
section('Minions and spectres', load(oldDir, 'minions.json'), load(newDir, 'minions.json'), (k, v) => `${v.name} [${k.split('/').pop()}]`);
const oT = load(oldDir, 'tree.json'), nT = load(newDir, 'tree.json');
const nodeLabel = (k, v) => `${v.name || '(unnamed)'} #${k}${v.isKeystone ? ' keystone' : v.isNotable ? ' notable' : ''}${v.ascendancyName ? ' (' + v.ascendancyName + ')' : ''}${v.stats && v.stats.length ? ': ' + v.stats.join(' / ') : ''}`;
// a moved node is not a changed node: compare what it is, not where it is
section('Passive nodes', oT && oT.nodes, nT && nT.nodes, nodeLabel, ['x', 'y', 'group', 'orbit', 'orbitIndex', 'connections']);
if (oT && nT) {
  const asc = t => new Set(t.classes.flatMap(c => [c.name, ...c.ascendancies.map(a => `${c.name}: ${a.name}`)]));
  const a = asc(oT), b = asc(nT);
  const add = [...b].filter(x => !a.has(x)), rem = [...a].filter(x => !b.has(x));
  console.log(`\n## Classes and ascendancies: ${add.length} added, ${rem.length} removed`);
  if (add.length) console.log('  added: ' + add.join(', '));
  if (rem.length) console.log('  removed: ' + rem.join(', '));
}

// ---- stat ids ----
// every stat id an export knows: used by a skill, a mod or a passive (stats-used.json, written by the direct
// export), or at least described in a stat description file
function statUse(dir, skills) {
  const use = load(dir, 'stats-used.json') || {};
  if (!Object.keys(use).length && skills) {
    for (const [k, s] of Object.entries(skills)) {
      const ids = new Set();
      for (const set of Array.isArray(s.statSets) ? s.statSets : []) {
        for (const id of Array.isArray(set.stats) ? set.stats : []) ids.add(id);
        for (const c of Array.isArray(set.constantStats) ? set.constantStats : []) ids.add(c[0]);
      }
      for (const q of [...(Array.isArray(s.qualityStats) ? s.qualityStats : []), ...(Array.isArray(s.altQualityStats) ? s.altQualityStats : [])]) ids.add(q[0]);
      for (const id of ids) { const u = use[id] || (use[id] = { skills: 0, mods: 0, passives: 0, examples: [] }); u.skills++; if (u.examples.length < 3) u.examples.push(k); }
    }
  }
  const known = new Set(Object.keys(use));
  for (const f of fs.existsSync(dir) ? fs.readdirSync(dir) : []) {
    if (!f.startsWith('statdesc_')) continue;
    const d = load(dir, f);
    for (const file of f === 'statdesc_specific.json' ? Object.values(d) : [d]) for (const k of Object.keys(file)) if (!/^\d+$/.test(k) && k !== 'parent') known.add(k);
  }
  return { use, known };
}
const o = statUse(oldDir, oS), n = statUse(newDir, nS);
const fresh = [...n.known].filter(id => !o.known.has(id));
const total = id => { const u = n.use[id]; return u ? u.skills + u.mods + u.passives : 0; };
fresh.sort((a, b) => total(b) - total(a) || (a < b ? -1 : 1));
const usedFresh = fresh.filter(id => total(id) > 0);
console.log(`\n## Stat ids: ${fresh.length} appear for the first time, ${usedFresh.length} of them used by a skill, mod or passive`);
if (!load(oldDir, 'stats-used.json')) console.log('  (the old export has no stats-used.json: "known before" = used by one of its skills or described in one of its stat description files)');
for (const id of all ? fresh : usedFresh.slice(0, 60)) {
  const u = n.use[id];
  if (!u) { console.log(`  ${String(0).padStart(4)}  ${id}   (described only)`); continue; }
  console.log(`  ${String(total(id)).padStart(4)}  ${id}   skills ${u.skills}, mods ${u.mods}, passives ${u.passives}   e.g. ${u.examples.join(', ')}`);
}
if (!all && usedFresh.length > 60) console.log(`  ... (${usedFresh.length - 60} more used ones and ${fresh.length - usedFresh.length} that are only described, --all)`);
const gone = [...o.known].filter(id => !n.known.has(id));
console.log(`\n## Stat ids no longer known: ${gone.length}${gone.length ? ': ' + (all ? gone : gone.slice(0, 30)).join(', ') + (!all && gone.length > 30 ? ', ...' : '') : ''}`);
