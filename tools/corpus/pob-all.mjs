// PoB's own breakdown of the main skill of every corpus character, one JSON line each:
//   node tools/corpus/pob-all.mjs > corpus/pob-skills.jsonl
// The main skill is the gem the file is named after. Needs a local pob-web checkout (see pob.mjs).
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
local function slug(s) return (s:lower():gsub("[^a-z0-9]+", "-"):gsub("^-", ""):gsub("-$", "")) end
function mainSkill(args)
  local want = WANT
  local best, bestGem
  for i, g in ipairs(build.skillsTab.socketGroupList) do
    for _, gi in ipairs(g.gemList or {}) do
      local sl = slug(gi.nameSpec or "")
      if sl ~= "" and want:sub(-#sl) == sl and (not bestGem or #sl > #slug(bestGem)) and not (gi.gemData and gi.gemData.grantedEffect and gi.gemData.grantedEffect.support) then best = i; bestGem = gi.nameSpec end
    end
  end
  if not best then return dkjson.encode({ error = "no gem" }) end
  build.mainSocketGroup = best; build.modFlag = true; build.buildFlag = true
  pcall(runCallback, "OnFrame")
  local g = build.skillsTab.socketGroupList[best]
  -- make the named gem the group's main active skill
  local idx = 0
  for _, gi in ipairs(g.gemList or {}) do
    if not (gi.gemData and gi.gemData.grantedEffect and gi.gemData.grantedEffect.support) and gi.enabled ~= false then
      idx = idx + 1
      if gi.nameSpec == bestGem then g.mainActiveSkill = idx end
    end
  end
  build.modFlag = true; build.buildFlag = true
  pcall(runCallback, "OnFrame")
  local o = build.calcsTab.mainOutput
  local res = { gem = bestGem, group = best, out = {}, hand = {}, gems = {}, config = {} }
  for _, gi in ipairs(g.gemList or {}) do res.gems[#res.gems+1] = gi.nameSpec end
  local sk = build.calcsTab.mainEnv and build.calcsTab.mainEnv.player.mainSkill
  if sk and sk.activeEffect then res.active = sk.activeEffect.grantedEffect.name; res.part = sk.skillPartName end
  local function keep(k) return k:match("DPS") or k:match("^Average") or k == "Speed" or k:match("^Crit") or k:match("HitAverage$") or k:match("StoredHitAvg$") or k == "HitChance" or k:match("MinBase$") or k:match("MaxBase$") or k:match("Stacks") or k:match("Multiplier") or k == "Repeats" or k:match("^Pre") or k == "Time" or k:match("Duration$") or k:match("EffMult$") or k == "Cooldown" or k == "ManaCost" or k:match("^Total") end
  for k, v in pairs(o) do if type(v) == "number" and v ~= 0 and keep(k) then res.out[k] = v end end
  if type(o.MainHand) == "table" then for k, v in pairs(o.MainHand) do if type(v) == "number" and v ~= 0 and keep(k) then res.hand[k] = v end end end
  if type(o.Minion) == "table" then res.minion = {}; for k, v in pairs(o.Minion) do if type(v) == "number" and v ~= 0 and keep(k) then res.minion[k] = v end end end
  -- what PoB counts only under a condition or per some multiplier: the assumptions behind its number
  res.assumes = {}
  if sk and sk.skillModList then
    for _, name in ipairs({"Damage","PhysicalDamage","ElementalDamage","FireDamage","ColdDamage","LightningDamage","ChaosDamage","CritChance","CritMultiplier","Speed","DamageGainAsChaos","DamageGainAsFire","DamageGainAsCold","DamageGainAsLightning"}) do
      for _, t in ipairs({"BASE","INC","MORE"}) do
        for _, row in ipairs(sk.skillModList:Tabulate(t, sk.skillCfg, name)) do
          for _, tag in ipairs(row.mod) do
            if tag.type == "Condition" or tag.type == "Multiplier" or tag.type == "ActorCondition" or tag.type == "MultiplierThreshold" or tag.type == "StatThreshold" or tag.type == "PerStat" or tag.type == "DistanceRamp" then
              local var = tag.var or tag.stat or tag.type
              if type(var) == "table" then var = table.concat(var, "/") end
              local key = tostring(tag.type == "ActorCondition" and "Enemy" or "") .. tostring(var) .. (tag.neg and "(not)" or "")
              if not tostring(var):match("^WeaponSet") and tostring(row.value) ~= "0" then
                res.assumes[key] = res.assumes[key] or {}
                local slot = name .. " " .. t
                res.assumes[key][slot] = (res.assumes[key][slot] or 0) + (tonumber(row.value) or 0)
              end
            end
          end
        end
      end
    end
  end
  for k, v in pairs(build.configTab.input or {}) do if type(v) ~= "table" and not tostring(k):match("^quest") then res.config[tostring(k)] = v end end
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
    ex("WANT = " + JSON.stringify(f.replace(/\.json$/, "").slice(4)));
    const out = call("mainSkill", "{}");
    console.log(JSON.stringify({ file: f.slice(0, -5), load: /success/.test(r), pob: JSON.parse(out) }));
  } catch (e) { console.log(JSON.stringify({ file: f.slice(0, -5), error: String(e).slice(0, 200) })); }
}
