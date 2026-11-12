// The simulator in a worker: reads the game data once, then answers requests
// ({ id, request: <JSON text> } in, { id, answer: <JSON text> } out) with PoEtry's Session.
import { Front_create, Front_handle } from "./sim/Report.js";

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

self.onmessage = async (e) => {
  const { id, request } = e.data;
  try {
    await ready;
    session ??= Front_create((name) => {
      if (!(name in data)) throw new Error(`no game data file ${name}`);
      return data[name];
    });
    self.postMessage({ id, answer: Front_handle(session, request) });
  } catch (err) {
    self.postMessage({ id, answer: JSON.stringify({ error: String(err && err.message || err) }) });
  }
};
