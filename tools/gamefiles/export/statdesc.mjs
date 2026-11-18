// Stat description files (Data/StatDescriptions/*.csd): text templates for stat ids.
// Same structure as PoB's Data/StatDescriptions/*.lua (Export/Scripts/statdesc.lua).
import { escapeGGG, sanitise } from './lib/text.mjs';

const DIR = 'data/statdescriptions/';
export const MAIN_FILES = ['stat_descriptions', 'passive_skill_stat_descriptions', 'gem_stat_descriptions', 'skill_stat_descriptions', 'active_skill_gem_stat_descriptions', 'meta_gem_stat_descriptions'];

function parseLimit(s) {
  if (s === '#') return ['#', '#'];
  if (/^-?\d+$/.test(s)) return [Number(s), Number(s)];
  const neg = s.match(/^!(-?\d+)$/);
  if (neg) return ['!', Number(neg[1])];
  const m = s.match(/([\d\-#]+)\|([\d\-#]+)/);
  if (!m) return [];
  const n = x => (/^-?\d+$/.test(x) ? Number(x) : x);
  return [n(m[1]), n(m[2])];
}

function luaNumber(s) {
  return /^-?(\d+\.?\d*|\.\d+)(e[+-]?\d+)?$/i.test(s) || /^0x[0-9a-f]+$/i.test(s) ? Number(s) : s;
}

/** Parses one .csd text. Returns { list: [descriptor], byStat: Map stat -> 1-based index, parent }. */
export function parseCsd(text) {
  const list = [];
  const byStat = new Map();
  let parent;
  let curLang = null;
  let cur = {};
  let prepend = '';
  for (let line of text.split(/[\r\n]+/)) {
    if (line === '') continue;
    line = prepend + line;
    prepend = '';
    const inc = line.match(/include "Data\/StatDescriptions\/(.+)\.csd"$/i);
    if (inc) { parent = inc[1].replace(/\\/g, '/').replace(/\/statset/g, '_statset'); continue; }
    const noDesc = line.match(/no_description ([\w+\-%]+)/);
    if (noDesc) {
      list.push({ stats: [noDesc[1]] });
      byStat.set(noDesc[1], list.length);
    } else if (/handed_description/.test(line) || (/description/.test(line) && !/_description/.test(line))) {
      const name = line.match(/description ([\w]+)/);
      curLang = [];
      cur = { lang: curLang, name: name ? name[1] : undefined };
      list.push(cur);
    } else if (!cur.stats) {
      const stats = line.match(/\d+\s+([\w+\-% ]+)/);
      if (stats) {
        cur.stats = stats[1].match(/[\w+\-%]+/g) || [];
        for (const s of cur.stats) byStat.set(s, list.length);
      } else {
        prepend = line;
      }
    } else if (/lang "(.+)"/.test(line)) {
      curLang = null;
    } else if (curLang && !/table_only/.test(line)) {
      const m = line.match(/([\d\-#| !]+)\s*(\w*)\s*"([^]*?)"\s*([^]*)/);
      if (!m) continue;
      const [, limits, quality, rawText, special] = m;
      let t = sanitise(escapeGGG(rawText)).replace(/\\([^nb])/g, '\\n$1');
      t = t.replace(/\\n/g, '\n').replace(/\\b/g, '\b');
      const desc = { text: t, limit: (limits.match(/[!\d\-#|]+/g) || []).map(parseLimit), specs: [] };
      const tokens = special.match(/[\w%]+/g) || [];
      for (let i = 0; i < tokens.length;) {
        if (tokens[i] === 'canonical_line') { desc.specs.push({ k: 'canonical_line', v: true }); i++; }
        else if (i + 1 < tokens.length) { desc.specs.push({ k: tokens[i], v: luaNumber(tokens[i + 1]) }); i += 2; }
        else i++;
      }
      if (/gem_quality/.test(quality)) desc.quality = quality;
      curLang.push(desc);
    }
  }
  return { list, byStat, parent };
}

/** The JSON form: descriptors under "1".."n", stat id -> index, "parent". */
export function toJson(parsed) {
  const o = {};
  parsed.list.forEach((d, i) => {
    const e = {};
    if (d.lang) {
      e['1'] = d.lang.map(desc => {
        const x = { text: desc.text, limit: desc.limit };
        desc.specs.forEach((s, k) => { x[String(k + 1)] = s; });
        if (desc.quality) x[desc.quality] = true;
        return x;
      });
    }
    if (d.name !== undefined) e.name = d.name;
    if (d.stats) e.stats = d.stats;
    o[String(i + 1)] = e;
  });
  for (const [s, i] of parsed.byStat) o[s] = i;
  if (parsed.parent !== undefined) o.parent = parsed.parent;
  return o;
}

const cache = new Map();
/** A parsed description file by its path below Data/StatDescriptions, without ".csd". */
export function loadCsd(game, name) {
  const key = name.toLowerCase();
  if (!cache.has(key)) {
    const text = game.readText(DIR + key + '.csd');
    cache.set(key, text === null ? null : parseCsd(text));
  }
  return cache.get(key);
}

/** The per-skill files: Specific_Skill_Stat_Descriptions/*.csd and one folder level below. */
export function specificFiles(game) {
  const root = DIR + 'specific_skill_stat_descriptions';
  const out = [];   // [key, path below StatDescriptions]
  const top = game.listDir(root);
  const base = f => f.split('/').pop().replace(/\.csd$/, '');
  for (const f of top.files) if (f.endsWith('.csd')) out.push([base(f), 'specific_skill_stat_descriptions/' + base(f)]);
  for (const d of top.dirs) {
    const dir = d.split('/').pop();
    for (const f of game.listDir(root + '/' + dir).files) {
      if (f.endsWith('.csd')) out.push([dir + '_' + base(f), `specific_skill_stat_descriptions/${dir}/${base(f)}`]);
    }
  }
  return out;
}

export function run(game) {
  const files = {};
  for (const f of MAIN_FILES) {
    const p = loadCsd(game, f);
    if (!p) throw new Error('missing stat description file ' + f);
    files[`statdesc_${f}.json`] = toJson(p);
  }
  const specific = {};
  for (const [key, file] of specificFiles(game)) specific[key] = toJson(loadCsd(game, file));
  files['statdesc_specific.json'] = specific;
  return files;
}
