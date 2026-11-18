// Development aid: look at raw game files.  node export/dev/raw.mjs roots | ls <dir>... | cat <file>... | grep <regex> <dir>... (DEPTH=n)
import { openGame } from '../lib/game.mjs';
const game = await openGame();
const [mode, ...args] = process.argv.slice(2);
if (mode === 'roots') console.log(game.roots().join('\n'));
if (mode === 'ls') for (const d of args) { const c = game.listDir(d); console.log(`${d}\n dirs (${c.dirs.length}): ${c.dirs.join(' | ')}\n files (${c.files.length}): ${c.files.join(' | ')}`); }
if (mode === 'cat') for (const f of args) { console.log('=====', f); console.log(game.readText(f) ?? 'MISSING'); }
if (mode === 'grep') {
  const [re, ...dirs] = args; const rx = new RegExp(re, 'i');
  const walk = (d, depth) => { const c = game.listDir(d); for (const f of c.files) { const t = game.readText(f); if (t && rx.test(t)) console.log('HIT', f); } if (depth > 0) for (const s of c.dirs) walk(s, depth - 1); };
  for (const d of dirs) walk(d, Number(process.env.DEPTH || 0));
}
