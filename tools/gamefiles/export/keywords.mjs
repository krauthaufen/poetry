// keywords.txt: the game's own keyword definitions (the popups behind highlighted words such as Rage,
// Armour Break, Recoup), from the table KeywordPopups. Plain text: the term on a line, then its text.
// keywords.json has the same by popup id, for whatsnew.mjs.
import { escapeGGG } from './lib/text.mjs';

// "<<icon>>", "<italic>{text}", "<rgb(1,2,3)>{text}" (nested) -> text
function plain(t) {
  t = t.replace(/<<[^<>]*>>/g, '');
  for (let prev = null; prev !== t;) { prev = t; t = t.replace(/<[^<>{}]+>\{([^{}]*)\}/g, '$1'); }
  return escapeGGG(t);
}

export function run(game) {
  const byId = {};
  const text = [];
  let skipped = 0, dnt = 0;
  for (const r of game.table('KeywordPopups')) {
    const term = plain(r.Term).trim();
    const def = plain(r.Definition).replace(/\r/g, '').replace(/[ \t]+\n/g, '\n').trim();
    // rows without a text, and the developers' own test rows
    if (term !== '' && def !== '' && /DNT/.test(term)) dnt++;
    if (term === '' || def === '' || /DNT/.test(term) || r.Id === 'Test') { skipped++; continue; }
    byId[r.Id] = { term, text: def };
    text.push(term + '\n' + def + '\n');
  }
  if (skipped) console.log(`  ${skipped} keyword popups were left out: no term or no text, or (${dnt}) marked DNT by the developers`);
  return { 'keywords.txt': { text: text.join('') }, 'keywords.json': { plainJson: byId } };
}
