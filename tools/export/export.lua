-- Export the game-data parts of PoB-PoE2's generated Lua files to JSON.
--
-- PoB generates Data/Skills, Data/Gems, Data/Bases, Data/StatDescriptions from the
-- game's .dat files. We take only that data: PoB's hand-written semantics
-- (statMap, skill functions) are dropped, PoEsie has its own.
--
-- Usage: lua export.lua <PoB src dir> <out dir>

local src, out = arg[1], arg[2]

-- ---- minimal JSON encoder ----
local function isArray(t)
  local n = 0
  for k in pairs(t) do
    if type(k) ~= "number" or k < 1 or k % 1 ~= 0 then return false end
    n = n + 1
  end
  for i = 1, n do if t[i] == nil then return false end end
  return true
end

local function esc(s)
  return '"' .. s:gsub('[%c"\\]', function(c)
    if c == '"' then return '\\"' elseif c == "\\" then return "\\\\"
    elseif c == "\n" then return "\\n" elseif c == "\t" then return "\\t"
    else return string.format("\\u%04x", c:byte()) end
  end) .. '"'
end

local encode
function encode(v, seen)
  local t = type(v)
  if t == "string" then return esc(v)
  elseif t == "number" then
    if v ~= v or v == math.huge or v == -math.huge then return "null" end
    if v % 1 == 0 and math.abs(v) < 2^53 then return string.format("%d", v) end
    return string.format("%.10g", v)
  elseif t == "boolean" then return tostring(v)
  elseif t == "table" then
    seen = seen or {}
    if seen[v] then return "null" end
    seen[v] = true
    local parts = {}
    if next(v) == nil then seen[v] = nil return "{}" end
    if isArray(v) then
      for _, x in ipairs(v) do parts[#parts + 1] = encode(x, seen) end
      seen[v] = nil
      return "[" .. table.concat(parts, ",") .. "]"
    end
    local keys = {}
    for k, x in pairs(v) do
      if type(x) ~= "function" and type(x) ~= "userdata" then keys[#keys + 1] = k end
    end
    table.sort(keys, function(a, b) return tostring(a) < tostring(b) end)
    for _, k in ipairs(keys) do
      parts[#parts + 1] = esc(tostring(k)) .. ":" .. encode(v[k], seen)
    end
    seen[v] = nil
    return "{" .. table.concat(parts, ",") .. "}"
  end
  return "null"
end

local function write(name, value)
  local f = assert(io.open(out .. "/" .. name, "w"))
  f:write(encode(value))
  f:close()
  print("wrote " .. name)
end

-- ---- stubs for the globals the generated files reference ----
SkillType = setmetatable({}, { __index = function(_, k) return k end })
local namedConst = setmetatable({}, { __index = function(_, k) return 0 end })
ModFlag, KeywordFlag = namedConst, namedConst
bit = { band = function() return 0 end, bor = function() return 0 end, bnot = function() return 0 end, lshift = function() return 0 end, rshift = function() return 0 end }
band, bor, bnot = bit.band, bit.bor, bit.bnot
local stub = function() return {} end

local function load(path, ...)
  local chunk = assert(loadfile(src .. "/" .. path))
  return chunk(...)
end

-- ---- skills (active, support, minion) ----
local skills = {}
for _, f in ipairs({ "act_str", "act_dex", "act_int", "sup_str", "sup_dex", "sup_int", "other", "minion" }) do
  load("Data/Skills/" .. f .. ".lua", skills, stub, stub, stub)
end
-- keep only data fields
local keep = {
  name = true, baseTypeName = true, description = true, skillTypes = true, minionSkillTypes = true,
  castTime = true, support = true, hidden = true, levels = true, statSets = true, qualityStats = true, altQualityStats = true,
  requireSkillTypes = true, addSkillTypes = true, excludeSkillTypes = true, weaponTypes = true,
  isTrigger = true, color = true, minionList = true, gemFamily = true,
}
local statSetKeep = {
  label = true, baseEffectiveness = true, incrementalEffectiveness = true,
  damageIncrementalEffectiveness = true, baseFlags = true, constantStats = true, stats = true,
  levels = true, notMinionStat = true, statDescriptionScope = true,
}
local clean = {}
for id, s in pairs(skills) do
  local c = {}
  for k, v in pairs(s) do if keep[k] then c[k] = v end end
  if c.statSets then
    for i, set in pairs(c.statSets) do
      local cs = {}
      for k, v in pairs(set) do if statSetKeep[k] then cs[k] = v end end
      c.statSets[i] = cs
    end
  end
  clean[id] = c
end
write("skills.json", clean)

-- ---- gems ----
write("gems.json", load("Data/Gems.lua"))

-- ---- item bases ----
local bases = {}
for _, f in ipairs({ "amulet", "axe", "belt", "body", "boots", "bow", "claw", "crossbow", "dagger",
  "flail", "focus", "gloves", "helmet", "jewel", "mace", "quiver", "ring", "sceptre", "shield",
  "spear", "staff", "sword", "talisman", "traptool", "wand", "flask", "charm" }) do
  local ok = pcall(load, "Data/Bases/" .. f .. ".lua", bases)
  if not ok then print("skipped bases " .. f) end
end
write("bases.json", bases)

-- ---- stat descriptions (text <-> stat id) ----
for _, f in ipairs({ "stat_descriptions", "passive_skill_stat_descriptions" }) do
  write("statdesc_" .. f .. ".json", load("Data/StatDescriptions/" .. f .. ".lua"))
end

-- ---- affix mods: id -> { type, group, level, text lines } ----
local mods = {}
for _, f in ipairs({ "ModItem", "ModJewel", "ModRunes", "ModCorrupted", "ModItemExclusive" }) do
  local ok, t = pcall(load, "Data/" .. f .. ".lua")
  if ok and type(t) == "table" then
    for id, m in pairs(t) do
      local lines = {}
      for _, l in ipairs(m) do if type(l) == "string" then lines[#lines + 1] = l end end
      mods[f .. ":" .. id] = { type = m.type, group = m.group, level = m.level, affix = m.affix, lines = lines }
    end
  else
    print("skipped " .. f)
  end
end
write("mods.json", mods)

-- ---- passive tree (newest version) ----
local treeVersion = arg[3] or "0_5"
local tree = load("TreeData/" .. treeVersion .. "/tree.lua")
local nodes = {}
for id, n in pairs(tree.nodes or {}) do
  nodes[tostring(id)] = {
    name = n.name, stats = n.stats, isNotable = n.isNotable, isKeystone = n.isKeystone,
    ascendancyName = n.ascendancyName, isAscendancyStart = n.isAscendancyStart,
    isJewelSocket = n.isJewelSocket, connections = n.connections, group = n.group, orbit = n.orbit,
    orbitIndex = n.orbitIndex, classStartIndex = n.classStartIndex, skill = n.skill,
  }
end
write("tree.json", { version = treeVersion, nodes = nodes, classes = tree.classes })
