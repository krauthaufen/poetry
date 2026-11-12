// PoB's numbers for every minion skill of a corpus character, one JSON line each:
//   node tools/corpus/pob-minions.mjs [nnn] > corpus/pob-minions.jsonl
// Level, base weapon damage, life and defences, average hit and speed per minion skill. Needs a local pob-web checkout (see pob.mjs).
import { readFileSync, readdirSync } from "fs";
import zlib from "zlib";
import { ALL_SHIMS } from "/home/schorsch/projects/poe2-craft/tools/pobcalc/lua-shims.mjs";
const W = "/home/schorsch/projects/pob-web/packages/web";
const dir = new URL("../../corpus/", import.meta.url).pathname;
const createModule = (await import(W + "/wasm/lua.mjs")).default;
const Module = await createModule({ wasmBinary: readFileSync(W + "/wasm/lua.wasm") });
const FS = Module.FS;
const ex = Module.cwrap("bridge_exec", "string", ["string"]);
const call = Module.cwrap("bridge_call_json", "string", ["string", "string"]);
Module.cwrap("bridge_init", "number", [])();
let e = ex(ALL_SHIMS); if (e) throw e;
const bundle = JSON.parse(readFileSync(W + "/public/data/pob-lua.json", "utf-8"));
const mk = p => { let c = ""; for (const s of p.split("/").filter(Boolean)) { c += "/" + s; try { FS.mkdir(c); } catch {} } };
for (let [path, text] of Object.entries(bundle.files)) {
  if (path === "runtime/lua/dkjson.lua") text = text.replace("local concat = table.concat", "local concat = table.concat\nlocal sort = table.sort").replace(/table\.sort\(/g, "sort(");
  if (path === "src/Modules/CalcDefence.lua") text = text.replace('local lifeProtected = output["FrostShieldLife"] / (output["FrostShieldDamageMitigation"] / 100) * (1 - output["FrostShieldDamageMitigation"] / 100)', 'local lifeProtected = (output["FrostShieldDamageMitigation"] or 0) > 0 and (output["FrostShieldLife"] / (output["FrostShieldDamageMitigation"] / 100) * (1 - output["FrostShieldDamageMitigation"] / 100)) or 0');
  const fp = "/pob/" + path; mk(fp.slice(0, fp.lastIndexOf("/"))); FS.writeFile(fp, text);
}
mk("/pob/user");
ex('package.path = "/pob/runtime/lua/?.lua;/pob/runtime/lua/?/init.lua;" .. package.path');
FS.chdir("/pob/src");
e = ex(readFileSync("/home/schorsch/projects/poe2-craft/tools/pobcalc/hw.lua", "utf-8")); if (e) throw e;

ex(`local dkjson = require("dkjson")
function minions(args)
  local res = {}
  for i, g in ipairs(build.skillsTab.socketGroupList) do
    if g.enabled ~= false then
      local idx = 0
      for _, gi in ipairs(g.gemList or {}) do
        local ge = gi.gemData and gi.gemData.grantedEffect
        if ge and not ge.support and gi.enabled ~= false then
          idx = idx + 1
          if ge.minionList or (gi.nameSpec or ""):match("^Spectre") or (gi.nameSpec or ""):match("^Companion") then
            build.mainSocketGroup = i; g.mainActiveSkill = idx
            for skillIdx = 1, 6 do
              gi.skillMinionSkill = skillIdx
              build.modFlag = true; build.buildFlag = true
              pcall(runCallback, "OnFrame")
              local env = build.calcsTab.mainEnv
              local sk = env and env.player.mainSkill
              local m = sk and sk.minion
              if not m or not m.mainSkill then break end
              local name = m.mainSkill.activeEffect.grantedEffect.name
              local o = build.calcsTab.mainOutput.Minion or {}
              local row = { gem = gi.nameSpec, gemLevel = gi.level, minion = m.minionData and m.minionData.name, level = m.level, skill = name,
                wmin = m.weaponData1 and m.weaponData1.PhysicalMin, wmax = m.weaponData1 and m.weaponData1.PhysicalMax,
                life = o.Life, es = o.EnergyShield, armour = o.Armour, evasion = o.Evasion, fireRes = o.FireResist, chaosRes = o.ChaosResist,
                avgHit = o.AverageHit, avgDamage = o.AverageDamage, speed = o.Speed, crit = o.CritChance, dps = o.TotalDPS, count = sk.activeEffect and sk.activeEffect.srcInstance and sk.activeEffect.srcInstance.count }
              if #res > 0 and res[#res].gem == row.gem and res[#res].skill == row.skill then break end
              res[#res+1] = row
            end
          end
        end
      end
    end
  end
  return dkjson.encode(res)
end`);
const files = readdirSync(dir).filter(n => /^[0-9]{3}-.*\.json$/.test(n)).sort();
const only = process.argv[2];
for (const f of files) {
  if (only && !f.startsWith(only)) continue;
  try {
    const code = JSON.parse(readFileSync(dir + f, "utf-8")).ninja.pathOfBuildingExport.replace(/-/g, "+").replace(/_/g, "/");
    const xml = zlib.inflateSync(Buffer.from(code, "base64")).toString("utf8");
    const r = call("pobWebLoadBuild", JSON.stringify({ xml, name: f }));
    const out = JSON.parse(call("minions", "{}"));
    for (const row of out) console.log(JSON.stringify({ file: f.slice(0, 3), ...row }));
  } catch (e) { console.log(JSON.stringify({ file: f.slice(0, -5), error: String(e).slice(0, 200) })); }
}
