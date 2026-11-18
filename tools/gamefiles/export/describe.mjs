// Turns stat values into the text lines the game shows, with the templates of stat_descriptions.csd.
// A port of PoB's Export/statdesc.lua (describeStats, describeMod), including its quirks, so that
// mod and passive lines are the same strings the simulator's text parser was built on.
import { loadCsd } from './statdesc.mjs';
import { sanitise, fmtG, round } from './lib/text.mjs';

function matchLimit(lang, val) {
  for (const desc of lang) {
    let ok = true;
    for (let i = 0; i < desc.limit.length; i++) {
      const [lo, hi] = desc.limit[i];
      const v = val[i] ? val[i].min : 0;
      if (lo === '!') { if (v === hi) { ok = false; break; } }
      else if ((hi !== '#' && v > hi) || (lo !== '#' && v < lo)) { ok = false; break; }
    }
    if (ok) return desc;
  }
  return null;
}

const each = f => v => { v.min = f(v.min); v.max = f(v.max); };
const eachG = f => v => { v.min = f(v.min); v.max = f(v.max); v.fmt = 'g'; };
const swapped = f => v => { const mn = v.min, mx = v.max; v.max = f(mn); v.min = f(mx); };
// value handlers by name (the words after a template in the .csd)
const HANDLERS = {
  negate: swapped(x => -x),
  invert_chance: swapped(x => 100 - x),
  negate_and_double: swapped(x => -2 * x),
  passive_hash: v => { if (v.min < 0) { v.min += 65536; v.max += 65536; } },
  divide_by_two_0dp: each(x => round(x / 2)),
  // as in statdesc.lua: the maximum is derived from the already divided minimum
  divide_by_three: v => { v.min = v.min / 3; v.max = v.min / 3; v.fmt = 'g'; },
  divide_by_four: v => { v.min = v.min / 4; v.max = v.min / 4; v.fmt = 'g'; },
  divide_by_five: eachG(x => x / 5),
  divide_by_six: eachG(x => x / 6),
  divide_by_ten_0dp: each(x => round(x / 10)),
  divide_by_ten_1dp: eachG(x => round(x / 10, 1)),
  divide_by_ten_1dp_if_required: eachG(x => round(x / 10, 1)),
  divide_by_twelve: eachG(x => x / 12),
  divide_by_fifteen_0dp: each(x => round(x / 15)),
  divide_by_twenty: eachG(x => x / 20),
  divide_by_twenty_then_double_0dp: each(x => round(x / 20) * 2),
  divide_by_fifty: eachG(x => x / 50),
  divide_by_one_hundred: eachG(x => x / 100),
  divide_by_one_hundred_0dp: eachG(x => round(x / 100)),
  divide_by_one_hundred_1dp: eachG(x => round(x / 100, 1)),
  divide_by_one_hundred_2dp_if_required: eachG(x => round(x / 100, 2)),
  divide_by_one_hundred_2dp: eachG(x => round(x / 100, 2)),
  divide_by_one_hundred_and_negate: eachG(x => -x / 100),
  divide_by_one_thousand: eachG(x => x / 1000),
  divide_by_ten_thousand_1dp: eachG(x => round(x / 10000, 1)),
  per_minute_to_per_second: eachG(x => x / 60),
  per_minute_to_per_second_0dp: each(x => round(x / 60)),
  per_minute_to_per_second_1dp: eachG(x => round(x / 60, 1)),
  per_minute_to_per_second_2dp_if_required: eachG(x => round(x / 60, 2)),
  per_minute_to_per_second_2dp: eachG(x => round(x / 60, 2)),
  milliseconds_to_seconds: eachG(x => x / 1000),
  milliseconds_to_seconds_halved: eachG(x => x / 1000 / 2),
  milliseconds_to_seconds_0dp: each(x => round(x / 1000)),
  milliseconds_to_seconds_1dp: eachG(x => round(x / 1000, 1)),
  milliseconds_to_seconds_2dp: eachG(x => round(x / 1000, 2)),
  milliseconds_to_seconds_2dp_if_required: eachG(x => round(x / 1000, 2)),
  deciseconds_to_seconds: v => { v.min /= 10; v.max /= 10; v.fmt = '.2f'; },
  '30%_of_value': each(x => x * 0.3),
  '60%_of_value': each(x => x * 0.6),
  one_hundred_divide_by_value: eachG(x => round(100 / x, 2)),
  multiplicative_damage_modifier: each(x => 100 + x),
  multiplicative_permyriad_damage_modifier: eachG(x => 100 + x / 100),
  times_one_point_five: each(x => x * 1.5),
  double: each(x => x * 2),
  multiply_by_four: each(x => x * 4),
  multiply_by_four_and_negate: each(x => -x * 4),
  multiply_by_ten: each(x => x * 10),
  times_twenty: each(x => x * 20),
  multiply_by_one_hundred: each(x => x * 100),
  plus_two_hundred: each(x => x + 200),
  reminderstring: () => {}, canonical_line: () => {}, canonical_stat: () => {},
};
export const unknownHandlers = new Set();

function fmt(v, x, plus) {
  let s;
  if (v.fmt === 'd') s = String(Math.trunc(x));
  else if (v.fmt === '.2f') s = x.toFixed(2);
  else s = fmtG(x);
  if (s === '-0') s = '0';
  return plus && x >= 0 ? '+' + s : s;
}

/**
 * stats: Map stat id -> { min, max }. Returns { lines, orders, missing } like describeStats.
 * file: the description file, by default stat_descriptions.
 */
const merged = new Map();
/** A description file together with the files it includes: the including file's entries win. */
function mergedCsd(game, file) {
  if (merged.has(file)) return merged.get(file);
  const own = loadCsd(game, file);
  if (!own) throw new Error('missing stat description file ' + file);
  let out = own;
  if (own.parent) {
    const base = mergedCsd(game, own.parent);
    const list = [...base.list, ...own.list];
    const byStat = new Map(base.byStat);
    for (const [s, i] of own.byStat) byStat.set(s, i + base.list.length);
    out = { list, byStat };
  }
  merged.set(file, out);
  return out;
}

export function describeStats(game, stats, file = 'stat_descriptions') {
  const csd = mergedCsd(game, file);
  const picked = new Set();
  const missing = [];
  for (const [s, v] of stats) {
    const idx = csd.byStat.get(s);
    const d = idx ? csd.list[idx - 1] : null;
    if (d && d.stats && d.lang) { if (v.min !== 0 || v.max !== 0) picked.add(idx); }
    else if (!d || !d.stats) missing.push(s);
  }
  const lines = [], orders = [];
  for (const idx of [...picked].sort((a, b) => a - b)) {
    const d = csd.list[idx - 1];
    const val = d.stats.map(s => { const v = stats.get(s); return v ? { min: v.min, max: v.max, fmt: 'd' } : { min: 0, max: 0, fmt: 'd' }; });
    let desc = matchLimit(d.lang, val);
    if (!desc) {
      // a range starting or ending at 0, with no template for 0
      for (const s of val) {
        if (s.min === 0 && s.max > 0) { s.min = 1; s.minZ = true; }
        else if (s.min < 0 && s.max === 0) { s.max = -1; s.maxZ = true; }
      }
      desc = matchLimit(d.lang, val);
      for (const s of val) { if (s.minZ) s.min = 0; if (s.maxZ) s.max = 0; }
    }
    if (!desc) continue;
    for (const spec of desc.specs) {
      const h = HANDLERS[spec.k];
      if (!h) { unknownHandlers.add(spec.k); continue; }
      const v = val[spec.v - 1];
      if (typeof spec.v === 'number' && v) h(v);
    }
    const one = (v, plus) => (v.min === v.max ? fmt(v, v.min, plus) : `(${fmt(v, v.min, plus)}-${fmt(v, v.max, plus)})`);
    const text = desc.text
      .replace(/\{(\d)\}/g, (m, n) => (val[Number(n)] ? one(val[Number(n)], false) : m))
      .replace(/\{\}/g, () => one(val[0], false))
      .replace(/\{(\d?):(\+?)d?\}/g, (m, n, plus) => {
        const v = val[n === '' ? 0 : Number(n)];
        if (!v) return m;
        if (v.min === v.max) return fmt(v, v.min, plus === '+');
        if (plus === '+') return v.max < 0 ? `-(${fmt(v, -v.min)}-${fmt(v, -v.max)})` : `+(${fmt(v, v.min)}-${fmt(v, v.max)})`;
        return `(${fmt(v, v.min)}-${fmt(v, v.max)})`;
      })
      .replace(/%%/g, '%');
    let order = idx;
    for (const line of text.split('\n')) {
      if (line === '') continue;
      lines.push(sanitise(line));
      orders.push(order);
      order += 0.1;
    }
  }
  return { lines, orders, missing };
}

/** The stats of a Mods row: Map stat id -> { min, max }. */
export function modStats(game, mod) {
  const STATS = game.table('Stats');
  const stats = new Map();
  for (let i = 1; i <= 6; i++) {
    const s = mod['Stat' + i];
    if (s === null || s === undefined) continue;
    const v = mod[`Stat${i}Value`];
    stats.set(STATS[s].Id, { min: v[0], max: v[1] });
  }
  return stats;
}

/** Text lines of a mod, and its tag ids (PoB: modTags). */
export function describeMod(game, mod) {
  const r = describeStats(game, modStats(game, mod));
  r.modTags = game.refs('Tags', mod.ImplicitTags).map(t => t.Id);
  return r;
}
