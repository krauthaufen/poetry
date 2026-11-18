// tree.json: the passive tree, from the game's graph file (the .psg named by PassiveSkillTrees) and
// PassiveSkills. Follows PoB's Export/Scripts/passivetree.lua, without the art.
// Also writes tree-full.json, in the shape of PoB's TreeData/<version>/tree.json (groups, constants,
// every node field), which tools/export/tree_ui.py takes as its input.
import { describeStats } from './describe.mjs';
import { escapeGGG } from './lib/text.mjs';

const IGNORE = /^\[DNT/;
const KEEP_DNT = new Set(['[DNT] Kite Fisher', '[DNT] Troller', '[DNT] Spearfisher', '[DNT] Angler', '[DNT] Whaler']);
const ORBIT_RADII = [0, 82, 162, 335, 493, 662, 846, 251, 1080, 1322];
const ARC_ANGLE = [0, 0, 12, 24, 36, 48, 60, 72, 84, 96];
const round2 = v => Math.floor(v * 100 + 0.5) / 100;
const rad = d => d * (Math.PI / 180);   // as Lua's math.rad, to the last bit

function orbitAngles(n) {
  let deg;
  if (n === 16) deg = [0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330];
  else if (n === 40) deg = [0, 10, 20, 30, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120, 130, 135, 140, 150, 160, 170, 180, 190, 200, 210, 220, 225, 230, 240, 250, 260, 270, 280, 290, 300, 310, 315, 320, 330, 340, 350];
  else { deg = []; for (let i = 0; i <= n; i++) deg.push(360 * i / n); }
  return deg.map(rad);
}

/** The graph file: root passives (class starts) and groups of passives with their connections. */
export function parsePsg(bytes) {
  const dv = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let o = 0;
  const u16 = () => { const v = dv.getUint16(o, true); o += 2; return v; };
  const i32 = () => { const v = dv.getInt32(o, true); o += 4; return v; };
  const f32 = () => { const v = dv.getFloat32(o, true); o += 4; return v; };
  const version = u16();
  if (version !== 3) throw new Error(`passive graph file has version ${version}; this reader knows version 3 (see PATCHDAY.md)`);
  o += 11;
  const roots = [];
  const rootCount = i32();
  for (let i = 0; i < rootCount; i++) { roots.push(Number(dv.getBigUint64(o, true))); o += 8; }
  const groups = [];
  const groupCount = i32();
  for (let i = 0; i < groupCount; i++) {
    const g = { x: f32(), y: f32(), flags: i32(), unk1: i32(), unk2: dv.getUint8(o++), passives: [] };
    const n = i32();
    for (let j = 0; j < n; j++) {
      const p = { id: i32(), radius: i32(), position: i32(), connections: [] };
      const c = i32();
      for (let k = 0; k < c; k++) p.connections.push({ id: i32(), radius: i32() });
      g.passives.push(p);
    }
    groups.push(g);
  }
  if (o !== bytes.byteLength) console.log(`  passive graph: ${bytes.byteLength - o} bytes after the last group were not read`);
  return { roots, groups };
}

export function run(game, ctx) {
  const STATS = game.table('Stats'), CHARS = game.table('Characters');
  const treeRow = game.rowBy('PassiveSkillTrees', 'Id', ctx.treeId || 'Default');
  if (!treeRow) throw new Error('no passive tree "Default" in PassiveSkillTrees');
  const psgBytes = game.readFile(treeRow.PassiveSkillGraph + '.psg');
  if (!psgBytes) throw new Error('missing graph file ' + treeRow.PassiveSkillGraph + '.psg');
  const psg = parsePsg(psgBytes);
  const passiveById = id => game.rowBy('PassiveSkills', 'PassiveSkillGraphId', id);
  const statValue = (row, k) => row[`Stat${k}Value`];
  const statLines = row => {
    const m = new Map();
    row.Stats.forEach((s, k) => { const v = statValue(row, k + 1) ?? 0; m.set(STATS[s].Id, { min: v, max: v }); });
    return describeStats(game, m, 'passive_skill_stat_descriptions').lines;
  };

  // ---- classes ----
  const classes = [];
  const replacements = new Map();   // ascendancy name -> the one that replaces it
  for (const rootId of psg.roots) {
    const row = passiveById(rootId);
    if (!row || IGNORE.test(row.Name)) continue;
    for (const ci of row.Characters) {
      const ch = CHARS[ci];
      if (IGNORE.test(ch.Name)) continue;
      const cls = {
        name: ch.Name, integerId: ch.IntegerId, base_str: ch.BaseStrength, base_dex: ch.BaseDexterity, base_int: ch.BaseIntelligence,
        ascendancies: [],
        background: { active: { width: 2000, height: 2000 }, bg: { width: 2000, height: 2000 }, image: 'Classes' + ch.Name, section: 'AscendancyBackground', x: 0, y: 0, width: 1500, height: 1500 },
      };
      for (const a of game.rowsBy('Ascendancy', 'Character', ci)) {
        if (IGNORE.test(a.Name) || a.Disabled) continue;
        const rep = game.ref('Ascendancy', a.BaseAscendancy);
        if (rep) replacements.set(rep.Name, a.Name);
        cls.ascendancies.push({ id: a.Name, name: a.Name, internalId: a.Id, replace: rep ? rep.Name : undefined,
          background: { image: 'Classes' + a.Name, section: 'AscendancyBackground', x: 0, y: 0, width: 1500, height: 1500 } });
      }
      if (cls.ascendancies.length) classes.push(cls);
    }
  }

  // the three attribute passives an attribute node can be
  const attributes = [26297, 14927, 57022].map(id => {
    const b = passiveById(id);
    return b && !IGNORE.test(b.Name) ? { id, name: b.Name, icon: b.Icon_DDSFile, stats: statLines(b) } : { id };
  });

  // ---- groups and nodes ----
  const nodes = {}, groups = {};
  const orbitMax = [];
  const ascGroups = new Map();   // ascendancy name -> { startId, groups: Set }
  const box = { min_x: 0, min_y: 0, max_x: 0, max_y: 0 };
  psg.groups.forEach((group, gi) => {
    const i = gi + 1;
    let isAsc = false;
    const tg = { x: round2(group.x), y: round2(group.y), orbits: [], nodes: [] };
    const orbits = new Set();
    for (const passive of group.passives) {
      const node = { skill: passive.id, group: i, orbit: passive.radius, orbitIndex: passive.position, connections: [] };
      const row = passiveById(passive.id);
      if (row) {
        if (row.Name === '') continue;
        if (IGNORE.test(row.Name) && !KEEP_DNT.has(row.Name)) continue;
        node.name = escapeGGG(row.Name);
        node.icon = row.Icon_DDSFile;
        if (row.FlavourText !== '') node.flavourText = row.FlavourText.replace(/\r/g, '');
        if (row.IsKeystone) node.isKeystone = true;
        else if (row.IsNotable) node.isNotable = true;
        else if (row.IsJustIcon) node.isOnlyImage = true;
        else if (row.IsJewelSocket) node.isJewelSocket = true;
        if (row.IsJewelSocket && row.IsAnointmentOnly) { node.aliasPassiveSocket = row.Id; node.sinister = true; node.noRadius = true; }
        const asc = game.ref('Ascendancy', row.Ascendancy);
        if (asc) {
          isAsc = true;
          if (IGNORE.test(asc.Name) || asc.Disabled) continue;
          node.ascendancyName = asc.Name;
          if (row.IsAscendancyStartingNode) { node.isAscendancyStart = true; node.name = asc.Name; }
          if (row.IsJewelSocket) node.containJewelSocket = true;
          if (!ascGroups.has(asc.Name)) ascGroups.set(asc.Name, { startId: undefined, groups: new Set() });
          const info = ascGroups.get(asc.Name);
          if (row.IsAscendancyStartingNode) info.startId = passive.id;
          info.groups.add(i);
        }
        if (row.UnlockedBy.length > 0) {
          const unlockAsc = game.ref('Ascendancy', row.VisibleForAscendancy);
          node.unlockConstraint = { ascendancy: unlockAsc ? unlockAsc.Name : undefined, nodes: row.UnlockedBy.map(r => game.table('PassiveSkills')[r].PassiveSkillGraphId) };
          node.connectionArt = 'CharacterPlanned';
        }
        node.stats = statLines(row);
        if (ctx.use) for (const si of row.Stats) ctx.use('passives', STATS[si].Id, node.name);
        if (row.IsAttribute) {
          node.isAttribute = true;
          node.options = attributes.map(a => ({ id: a.id, name: a.name, icon: a.icon, stats: a.stats }));
        }
        const grantedGem = game.ref('SkillGems', row.GrantedSkill);
        if (grantedGem) for (const _ of grantedGem.GemEffects) node.stats.push('Grants Skill: ' + game.table('BaseItemTypes')[grantedGem.BaseItemType].Name);
        if (row.SkillPointsGranted > 0) node.stats.push(`Grants ${row.SkillPointsGranted} Passive Skill Point`);
        if (row.WeaponPointsGranted > 0) node.stats.push(`${row.WeaponPointsGranted} Passive Skill Points become Weapon Set Skill Points`);
        const sw = game.rowBy('ClassPassiveSkillOverrides', 'SkillToOverride', row._index);
        if (sw) {
          const other = game.ref('PassiveSkills', sw.Override), ch = game.ref('Characters', sw.CharacterToOverrideFor);
          if (other && ch) {
            node.isSwitchable = true;
            node.options = { [ch.Name]: { id: other.PassiveSkillGraphId, name: other.Name, icon: other.Icon_DDSFile, stats: statLines(other) } };
          }
        }
        if (asc && replacements.has(asc.Name)) {
          node.isSwitchable = true;
          const info = { ascendancyName: replacements.get(asc.Name) };
          const so = game.rowBy('AscendancyPassiveSkillOverrides', 'SkillToOverride', row._index);
          const other = so && game.ref('PassiveSkills', so.Override);
          if (other) Object.assign(info, { id: other.PassiveSkillGraphId, name: other.Name, icon: other.Icon_DDSFile, stats: statLines(other) });
          node.options = { [replacements.get(asc.Name)]: info };
        }
        if (row.Characters.length > 0) node.classesStart = row.Characters.map(c => CHARS[c].Name).filter(n => !IGNORE.test(n));
        if (row.IsMultipleChoice) node.isMultipleChoice = true;
        if (row.IsMultipleChoiceOption) node.isMultipleChoiceOption = true;
        if (row.IsFree) node.isFreeAllocate = true;
        if (row._51 === true) node.applyToArmour = true;
      }
      for (const c of passive.connections) node.connections.push({ id: c.id, orbit: c.radius });
      orbits.add(passive.radius);
      orbitMax[passive.radius] = Math.max(orbitMax[passive.radius] || 1, passive.position);
      nodes[passive.id] = node;
      tg.nodes.push(passive.id);
    }
    tg.orbits = [...orbits].sort((a, b) => a - b);
    if (tg.nodes.length > 0) {
      groups[i] = tg;
      if (!isAsc) {
        box.min_x = Math.min(box.min_x, group.x); box.min_y = Math.min(box.min_y, group.y);
        box.max_x = Math.max(box.max_x, group.x); box.max_y = Math.max(box.max_y, group.y);
      }
    }
  });

  // ---- constants ----
  const skillsPerOrbit = [];
  for (let k = 0; k < orbitMax.length && orbitMax[k] !== undefined; k++) skillsPerOrbit.push(k === 0 ? orbitMax[k] : Math.ceil(orbitMax[k] / 12) * 12);
  const angles = skillsPerOrbit.map(orbitAngles);

  // ---- ascendancies: placed on a circle around the tree, near their class ----
  const radiusTree = (Math.max(box.max_x - box.min_x, box.max_y - box.min_y) + 700) / 2;
  const far = radiusTree + 2800;
  for (const rootId of psg.roots) {
    const start = nodes[rootId];
    if (!start || !groups[start.group]) continue;
    const g = groups[start.group];
    const toCentre = Math.atan2(g.y, g.x);
    const mine = classes.filter(c => (start.classesStart || []).includes(c.name));
    const total = mine.reduce((a, c) => a + c.ascendancies.length, 0);
    const startAngle = toCentre - rad(ARC_ANGLE[total] / 2);
    const step = rad(ARC_ANGLE[total] / (total - 1));
    let j = 1;
    for (const cls of mine) for (const asc of cls.ascendancies) {
      const angle = startAngle + (j - 1) * step;
      const cX = far * Math.cos(angle), cY = far * Math.sin(angle);
      const info = ascGroups.get(asc.id);
      const ascNode = info && nodes[info.startId];
      if (!ascNode) continue;
      asc.background.x = cX; asc.background.y = cY;
      const ag = groups[ascNode.group];
      const inner = game.rowBy('Ascendancy', 'Id', asc.internalId).TreeRegionVector;   // PoB: distanceTree
      const a = angles[ascNode.orbit][ascNode.orbitIndex], r = ORBIT_RADII[ascNode.orbit];
      const offX = cX + Math.cos(toCentre) * inner - Math.sin(a) * r - ag.x;
      const offY = cY + Math.sin(toCentre) * inner + Math.cos(a) * r - ag.y;
      for (const gid of info.groups) {
        const gg = groups[gid];
        if (!gg) continue;
        gg.x += offX; gg.y += offY;
        box.min_x = Math.min(box.min_x, gg.x - far / 2); box.min_y = Math.min(box.min_y, gg.y - far / 2);
        box.max_x = Math.max(box.max_x, gg.x + far / 2); box.max_y = Math.max(box.max_y, gg.y + far / 2);
      }
      j++;
    }
  }
  for (const [from, to] of replacements) {
    let f, t;
    for (const c of classes) for (const a of c.ascendancies) { if (a.name === from) f = a; if (a.name === to) t = a; }
    if (!f || !t) continue;
    f.replaceBy = to;
    t.background.x = f.background.x; t.background.y = f.background.y;
  }

  const jewelSlots = game.table('PassiveJewelSlots').map(s => game.ref('PassiveSkills', s.Slot)).filter(p => p).map(p => p.PassiveSkillGraphId);

  // ---- the simulator's tree.json: the node fields tools/export/export.lua keeps, with positions ----
  const KEEP = ['name', 'stats', 'isNotable', 'isKeystone', 'ascendancyName', 'isAscendancyStart', 'isJewelSocket', 'connections', 'group', 'orbit', 'orbitIndex', 'skill'];
  const simNodes = {};
  for (const [id, n] of Object.entries(nodes)) {
    const o = {};
    for (const k of KEEP) if (n[k] !== undefined) o[k] = n[k];
    const g = groups[n.group], a = angles[n.orbit] && angles[n.orbit][n.orbitIndex];
    if (g && a !== undefined) { o.x = g.x + Math.sin(a) * ORBIT_RADII[n.orbit]; o.y = g.y - Math.cos(a) * ORBIT_RADII[n.orbit]; }
    simNodes[id] = o;
  }
  const full = {
    tree: treeRow.Id, ...box, classes, groups, nodes, jewelSlots,
    constants: {
      classes: { StrDexIntClass: 0, StrClass: 1, DexClass: 2, IntClass: 3, StrDexClass: 4, StrIntClass: 5, DexIntClass: 6 },
      characterAttributes: { Strength: 0, Dexterity: 1, Intelligence: 2 },
      // angles with 14 digits, as PoB's tree.json has them: tree_ui.py then rounds positions the same way
      PSSCentreInnerRadius: 130, skillsPerOrbit, orbitAnglesByOrbit: angles.map(o => o.map(a => Number(a.toPrecision(14)))), orbitRadii: ORBIT_RADII,
    },
  };
  return {
    'tree.json': { version: ctx.treeVersion || '0_5', nodes: simNodes, classes, jewelSlots },
    'tree-full.json': { plainJson: full },
  };
}
