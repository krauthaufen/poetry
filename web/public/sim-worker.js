// The simulator in a worker: reads the game data once, then answers requests
// ({ id, request: <JSON text>, build: <the character's text> } in, { id, answer: <JSON text> } out) with PoEtry's Session.
import { Front_create, Front_handleWith } from "./sim/Report.js";

const files = ["bases", "gems", "minions", "mods", "skills", "tree", "statdesc_active_skill_gem_stat_descriptions",
  "statdesc_gem_stat_descriptions", "statdesc_meta_gem_stat_descriptions", "statdesc_passive_skill_stat_descriptions",
  "statdesc_skill_stat_descriptions", "statdesc_specific", "statdesc_stat_descriptions"];
const data = {};
const ready = Promise.all(files.map(async (f) => {
  const r = await fetch(new URL(`./data/${f}.json`, import.meta.url));
  if (!r.ok) throw new Error(`game data ${f}.json: status ${r.status}`);
  data[`${f}.json`] = await r.text();
}));
let session;
let freed = false;

self.onmessage = async (e) => {
  const { id, request, build } = e.data;
  try {
    await ready;
    session ??= Front_create((name) => {
      if (data[name] === undefined) throw new Error(`no game data file ${name}`);
      return data[name];
    });
    const answer = Front_handleWith(session, request, build || "");
    // (once a character was read, the game data is all parsed: its text, some 25 MB in
    // memory, is let go; a phone closes a page that holds too much)
    if (!freed && !answer.startsWith('{"error"')) { freed = true; for (const k of Object.keys(data)) data[k] = undefined; }
    self.postMessage({ id, answer });
  } catch (err) {
    self.postMessage({ id, answer: JSON.stringify({ error: String(err && err.message || err) }) });
  }
};
