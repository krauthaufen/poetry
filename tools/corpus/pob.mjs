// PoB's own numbers for a corpus character, to see which source a sheet or damage value differs by.
// Runs the PoB-PoE2 engine of a local pob-web checkout in Node (paths below); a development
// aid, not part of the simulator, and PoB stays a reference for defences only.
//   node tools/corpus/pob.mjs <nnn> <ModName>...              modifiers of the player (Life, Armour, FireResist, Spirit, ...)
//   node tools/corpus/pob.mjs <nnn> skill <Gem name>          the output PoB computes for that gem's group
//   node tools/corpus/pob.mjs <nnn> skillmods <Gem> <ModName>...   modifiers in that skill's context
// Lines starting with [Lua] are PoB's log: pipe through  grep -v "^\\[Lua\\]"
import { readFileSync, readdirSync } from "fs";
import zlib from "zlib";
import { ALL_SHIMS } from "/home/schorsch/projects/poe2-craft/tools/pobcalc/lua-shims.mjs";
const W = "/home/schorsch/projects/pob-web/packages/web";
const dir = new URL("../../corpus/", import.meta.url).pathname;
const f = readdirSync(dir).find(n => n.startsWith(process.argv[2] + "-"));
const code = JSON.parse(readFileSync(dir + f, "utf-8")).ninja.pathOfBuildingExport.replace(/-/g, "+").replace(/_/g, "/");
const xml = zlib.inflateSync(Buffer.from(code, "base64")).toString("utf8");
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
function dumpMods()
  local args = { names = DUMP_NAMES }
  local res = {}
  local env = build.calcsTab.mainEnv
  local o = build.calcsTab.mainOutput
  for _, name in ipairs(args.names) do
    res[#res+1] = "== " .. name .. " = " .. tostring(o[name])
    for _, t in ipairs({"BASE","INC","MORE","OVERRIDE","FLAG"}) do
      for _, row in ipairs(env.player.modDB:Tabulate(t, nil, name)) do
        local tags = {}
        for _, tag in ipairs(row.mod) do tags[#tags+1] = tostring(tag.type) .. ":" .. tostring(tag.var or tag.stat or tag.skillType or "") end
        res[#res+1] = string.format("  %-5s %10s  %-40s %s", t, tostring(row.value), tostring(row.mod.source), table.concat(tags, ","))
      end
    end
  end
  return dkjson.encode(res)
end`);
ex(`function dumpSkill()
  local res = {}
  local want = DUMP_SKILL
  for i, g in ipairs(build.skillsTab.socketGroupList) do
    local nm = {}
    local hit = false
    for _, gi in ipairs(g.gemList or {}) do nm[#nm+1] = gi.nameSpec; if gi.nameSpec == want then hit = true end end
    if hit then
      build.mainSocketGroup = i; build.modFlag = true; build.buildFlag = true
      pcall(runCallback, "OnFrame")
      local o = build.calcsTab.mainOutput
      res[#res+1] = "group " .. i .. ": " .. table.concat(nm, " + ")
      local keys = {}
      for k, v in pairs(o) do if type(v) == "number" and v ~= 0 then keys[#keys+1] = k end end
      table.sort(keys)
      for _, k in ipairs(keys) do
        if k:match("DPS") or k:match("^Average") or k:match("Speed$") or k:match("^Crit") or k:match("HitAverage$") or k:match("^Hit") or k:match("Poison") or k:match("Bleed") or k:match("Ignite") or k:match("Min$") or k:match("Max$") or k:match("Multi") or k:match("Pen") or k:match("Stack") or k:match("Dot") then
          res[#res+1] = string.format("  %-34s %s", k, tostring(o[k]))
        end
      end
    end
  end
  return require("dkjson").encode(res)
end`);
ex(`function dumpSkillMods()
  local res = {}
  for i, g in ipairs(build.skillsTab.socketGroupList) do
    local hit = false
    for _, gi in ipairs(g.gemList or {}) do if gi.nameSpec == DUMP_SKILL then hit = true end end
    if hit then
      build.mainSocketGroup = i; build.modFlag = true; build.buildFlag = true
      pcall(runCallback, "OnFrame")
      local sk = build.calcsTab.mainEnv.player.mainSkill
      for _, name in ipairs(DUMP_NAMES) do
        res[#res+1] = "== " .. name
        for _, t in ipairs({"BASE","INC","MORE","OVERRIDE","FLAG"}) do
          for _, row in ipairs(sk.skillModList:Tabulate(t, sk.skillCfg, name)) do
            local tags = {}
            for _, tag in ipairs(row.mod) do tags[#tags+1] = tostring(tag.type) .. ":" .. tostring(tag.var or tag.stat or tag.skillType or "") end
            res[#res+1] = string.format("  %-5s %10s  %-44s %s", t, tostring(row.value), tostring(row.mod.source), table.concat(tags, ","))
          end
        end
      end
    end
  end
  return require("dkjson").encode(res)
end`);
const r = call("pobWebLoadBuild", JSON.stringify({ xml, name: "x" }));
if (!/success/.test(r)) console.log("load:", r.slice(0, 300));
if (process.argv[3] === "skillmods") { ex("DUMP_SKILL = " + JSON.stringify(process.argv[4])); ex("DUMP_NAMES = {" + process.argv.slice(5).map(n => JSON.stringify(n)).join(",") + "}"); for (const l of JSON.parse(call("dumpSkillMods", "{}"))) console.log(l); process.exit(0); }
if (process.argv[3] === "skill") { ex("DUMP_SKILL = " + JSON.stringify(process.argv.slice(4).join(" "))); for (const l of JSON.parse(call("dumpSkill", "{}"))) console.log(l); process.exit(0); }
ex("DUMP_NAMES = {" + process.argv.slice(3).map(n => JSON.stringify(n)).join(",") + "}");
for (const l of JSON.parse(call("dumpMods", "{}"))) console.log(l);
