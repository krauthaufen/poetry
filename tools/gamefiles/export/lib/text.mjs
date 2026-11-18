// Text clean-up, as PoB's exporter does it (Common.lua escapeGGGString and sanitiseText), so that
// names and description lines are the same strings as in the data the simulator was built on.

/** "[Tag|Shown text]" -> "Shown text", "[Tag]" -> "Tag", "<x>{text}" -> "text". */
export function escapeGGG(text) {
  return text
    .replace(/<[^>]+>\{([^}]+)\}/g, '$1')
    .replace(/\[([^|\]]+)\]/g, '$1')
    .replace(/\[[^|]+\|([^|]+)\]/g, '$1');
}

const utf8 = new TextEncoder();
const DASH = new Set([0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2015, 0x2212]);
const PLAIN = new Map([[0xe4, 'a'], [0xf6, 'o'], [0xed, 'i'], [0xf3, 'o']]);

/** Removes <...> markup and replaces what is not ASCII: dashes by "-", a few letters, the rest by "?" per UTF-8 byte. */
export function sanitise(text) {
  if (text === null || text === undefined) return text;
  if (!/[<\u0080-￿]/.test(text)) return text;
  text = removeBalanced(text);
  let out = '';
  for (let i = 0; i < text.length; i++) {
    const cp = text.codePointAt(i);
    if (cp > 0xffff) i++;
    if (cp < 0x80) { out += text[i]; continue; }
    if (DASH.has(cp)) { out += '-'; continue; }
    if (cp === 0x2022) { if (text[i + 1] === ' ') i++; continue; }
    if (PLAIN.has(cp)) { out += PLAIN.get(cp); continue; }
    out += '?'.repeat(utf8.encode(String.fromCodePoint(cp)).length);
  }
  return out;
}

// Lua's %b<>: from a "<" to its matching ">"
function removeBalanced(text) {
  let out = '';
  let i = 0;
  while (i < text.length) {
    if (text[i] === '<') {
      let depth = 0, j = i;
      for (; j < text.length; j++) {
        if (text[j] === '<') depth++;
        else if (text[j] === '>') { depth--; if (depth === 0) break; }
      }
      if (j < text.length) { i = j + 1; continue; }
    }
    out += text[i++];
  }
  return out;
}

/** Lua's string.format("%g") for the numbers that occur in stat lines. */
export function fmtG(v) {
  if (Number.isInteger(v)) return Math.abs(v) < 1e6 ? String(v) : Number(v.toPrecision(6)).toExponential().replace(/e([+-])(\d)$/, 'e$10$2');
  const s = Number(v.toPrecision(6));
  const a = Math.abs(s);
  if (a !== 0 && (a < 1e-4 || a >= 1e6)) return s.toExponential().replace(/e([+-])(\d)$/, 'e$10$2');
  return String(s);
}

/** Lua's round(x, dp): half away from zero is not what it does; it is floor(x * 10^dp + 0.5) / 10^dp. */
export function round(v, dp = 0) {
  const m = 10 ** dp;
  return Math.floor(v * m + 0.5) / m;
}
