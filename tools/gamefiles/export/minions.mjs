// minions.json: the monsters a player can have as minions: everything that can be raised as a spectre
// or tamed (the rule of PoB's Export/Scripts/spectreList.lua) and the monsters of minion skills
// (monster types flagged as a player's minion). Fields as PoB's Export/Scripts/minions.lua writes them.
// The key is the monster's id in MonsterVarieties (PoB uses hand-given short names for 32 of them).
import * as fs from 'node:fs';
import * as path from 'node:path';
import { GAMEFILES_DIR } from './lib/game.mjs';
import { round } from './lib/text.mjs';

const hand = name => JSON.parse(fs.readFileSync(path.join(GAMEFILES_DIR, 'export', 'hand', name), 'utf8'));
let handMinions = null;
/** hand/minions.json: the monsters reviewed by hand, monster id -> { key?, limit? }. */
export function reviewedMinions() {
  if (!handMinions) handMinions = hand('minions.json').monsters;
  return handMinions;
}
/** The id the simulator knows a minion by: the hand-given key where there is one, else the monster id. */
export function minionKey(id) {
  const h = reviewedMinions()[id];
  return (h && h.key) || id;
}

// Granted effects of a monster that are no skill of its own: by the prefix of the id (EA, EAA, EAS: empty
// action; EG: execute; GT: geometry trigger; SO: spawn object; MD: monster dodge) or a word in it. None of
// PoB's 310 hand-picked monster skills is one of these, of 1600 granted effects on the monsters it has.
const HELPER = /^(EA|EAA|EAS|EG|GT|SO|MD|EDA|CTS|GCE)(?=[A-Z][a-z])|EmptyAction|Audio|DoLiterallyNothing|WalkEmerge|Dodge(Left|Right|Back)/;

const itemClassMap = {
  'Claw': 'Claw', 'Dagger': 'Dagger', 'Wand': 'Wand', 'One Hand Sword': 'One Hand Sword', 'Thrusting One Hand Sword': 'One Hand Sword',
  'One Hand Axe': 'One Hand Axe', 'One Hand Mace': 'One Hand Mace', 'Crossbow': 'Crossbow', 'Bow': 'Bow', 'Fishing Rod': 'Fishing Rod',
  'Staff': 'Staff', 'Warstaff': 'Warstaff', 'Two Hand Sword': 'Two Hand Sword', 'Two Hand Axe': 'Two Hand Axe', 'Two Hand Mace': 'Two Hand Mace',
  'Shield': 'Shield', 'Sceptre': 'One Hand Mace', 'Flail': 'Flail', 'Spear': 'Spear', 'Talisman': 'Talisman', 'Unarmed': 'None',
};

let cached = null;
/** The MonsterVarieties rows that are exported: [{ row, why }] */
export function minionVarieties(game) {
  if (cached) return cached;
  const MV = game.table('MonsterVarieties'), MT = game.table('MonsterTypes'), MODS = game.table('Mods'), TAGS = game.table('Tags');
  const picked = new Map();   // row index -> why
  // ---- spectres and beasts ----
  const overrideOf = new Map();
  for (const o of game.table('SpectreOverrides')) if (o.Monster !== null && o.Spectre !== null && !overrideOf.has(o.Monster)) overrideOf.set(o.Monster, o.Spectre);
  const seen = new Set();
  for (const m of MV) {
    const type = MT[m.MonsterType];
    const cat = game.ref('MonsterCategories', m.MonsterCategory);
    const isBeast = cat && cat.Name === 'Beast';
    if (m.NotSpectre !== false || !(m.BossHealthBar === false || isBeast) || !type || type.IsSummoned) continue;
    if (/NPC/.test(m.Id) || /DNT|Daemon|Invisible/.test(m.Name) || /TestSpawn|Mtx|InsectTest|TEMP|GenericTest|PhysicsTest|Royale/.test(m.Id)) continue;
    if (/NoAI/.test(m.AISFile) || m.GrantedEffects.length === 0) continue;
    if ([...m.Mods, ...m.Mods2].some(i => MODS[i].Id === 'CannotBeUsedAsMinion')) continue;
    if (m.Tags.some(i => TAGS[i].Id === 'unusable_corpse')) continue;
    const source = overrideOf.has(m._index) ? MV[overrideOf.get(m._index)] : m;
    const skillIds = source.GrantedEffects.map(i => game.table('GrantedEffects')[i].Id).sort();
    const signature = `${m.Name}|${source.ExperienceMultiplier}|${skillIds.join(',')}`;
    if (seen.has(signature)) continue;
    seen.add(signature);
    if (!picked.has(source._index)) picked.set(source._index, 'spectre');
  }
  // ---- monsters of minion skills ----
  for (const m of MV) {
    const type = MT[m.MonsterType];
    if (!type || !type.IsSummoned || m.GrantedEffects.length === 0) continue;
    if (/^Daemon$|DNT|^\[/.test(m.Name) && !/PlayerSummoned|Minion/.test(m.Id)) continue;
    if (/Mtx|Test|Royale/.test(m.Id)) continue;
    if (!picked.has(m._index)) picked.set(m._index, 'minion');
  }
  // ---- every monster reviewed by hand, whatever the rules above say ----
  const missing = [];
  for (const id of Object.keys(reviewedMinions())) {
    const m = game.rowBy('MonsterVarieties', 'Id', id);
    if (!m) missing.push(id);
    else picked.set(m._index, 'hand');
  }
  if (missing.length) console.log(`  monsters of hand/minions.json the game no longer has: ${missing.join(', ')}`);
  cached = [...picked].sort((a, b) => a[0] - b[0]).map(([i, why]) => ({ row: MV[i], why }));
  return cached;
}

let skillChoice = null;
/**
 * Which granted effects of the exported monsters go to skills.json.
 * A monster in hand/minions.json: those of hand/monster-skills.json (PoB's hand-picked ones).
 * Any other monster: its granted effects that are not helpers (HELPER) and that no reviewed monster has
 * (for a reviewed monster, a skill left out was left out on purpose).
 * Returns { include: Set, derived: Map monster id -> [skill id] }.
 */
export function monsterSkills(game) {
  if (skillChoice) return skillChoice;
  const GE = game.table('GrantedEffects');
  const reviewed = reviewedMinions();
  const handSkills = new Set(hand('monster-skills.json').skills);
  const include = new Set(), ofReviewed = new Set();
  const all = minionVarieties(game);
  for (const { row } of all) if (reviewed[row.Id]) for (const i of row.GrantedEffects) ofReviewed.add(GE[i].Id);
  for (const id of handSkills) if (game.rowBy('GrantedEffects', 'Id', id)) include.add(id);
  const derived = new Map();
  for (const { row } of all) {
    if (reviewed[row.Id]) continue;
    const own = [];
    for (const i of row.GrantedEffects) {
      const id = GE[i].Id;
      if (handSkills.has(id)) continue;
      if (HELPER.test(id) || ofReviewed.has(id)) continue;
      include.add(id);
      own.push(id);
    }
    derived.set(row.Id, own);
  }
  skillChoice = { include, derived, handSkills, ofReviewed };
  return skillChoice;
}

export function run(game, ctx = {}) {
  const MT = game.table('MonsterTypes'), TAGS = game.table('Tags'), GE = game.table('GrantedEffects');
  const minions = {};
  for (const { row: m } of minionVarieties(game)) {
    const type = MT[m.MonsterType];
    const res = game.ref('MonsterResistances', type.MonsterResistances[0]);
    const o = {};
    o.name = m.Name;
    o.monsterTags = m.Tags.map(i => TAGS[i].Id);
    o.life = m.LifeMultiplier / 100;
    if (type.BaseDamageIgnoresAttackSpeed) o.baseDamageIgnoresAttackSpeed = true;
    if (type.EnergyShieldFromLife !== 0) o.energyShield = type.EnergyShieldFromLife / 100;
    if (type.Armour !== 0) o.armour = type.Armour / 100;
    if (type.Evasion !== 0) o.evasion = type.Evasion / 100;
    if (res) {
      o.fireResist = res.Fire1[0];
      o.coldResist = res.Cold1[0];
      o.lightningResist = res.Lightning1[0];
      o.chaosResist = res.Chaos1[0];
    }
    o.damage = m.DamageMultiplier / 100;
    o.damageSpread = type.DamageSpread / 100;
    o.attackTime = m.AttackSpeed / 1000;             // PoB: AttackDuration
    o.attackRange = m.MaximumAttackDistance;
    o.accuracy = 1;
    o.critChance = round(m.AttackCrit * 100, 2);
    const main = game.ref('ItemClasses', m.MainHand_ItemClass), off = game.ref('ItemClasses', m.OffHand_ItemClass);
    if (main && itemClassMap[main.Id]) o.weaponType1 = itemClassMap[main.Id];
    if (off && itemClassMap[off.Id]) o.weaponType2 = itemClassMap[off.Id];
    o.spectreReservation = Math.floor(((m.ExperienceMultiplier / 100) ** 0.75) * 50);
    o.companionReservation = Math.floor(Math.sqrt(m.ExperienceMultiplier / 100) * 100) / 100 * 30;
    const cat = game.ref('MonsterCategories', m.MonsterCategory);
    if (cat) o.monsterCategory = cat.Name;
    o.skillList = m.GrantedEffects.map(i => GE[i].Id);
    o.objectSize = m.ObjectSize;                      // tools/export/minion_sizes.py
    const h = reviewedMinions()[m.Id];
    if (h && h.limit) o.limit = h.limit;
    minions[minionKey(m.Id)] = o;
  }
  // what was decided by rule, not by hand: to be looked at on patch day
  const { derived } = monsterSkills(game);
  if (derived.size) {
    const lines = [...derived].map(([id, skills]) => `${id}\t${game.rowBy('MonsterVarieties', 'Id', id).Name}\t${skills.join(' ')}`);
    console.log(`  ${derived.size} monsters are not in hand/minions.json; their skills were chosen by rule (list: derived-minions.txt), e.g. ${[...derived.keys()].slice(0, 4).map(k => k.split('/').pop()).join(', ')}`);
    return { 'minions.json': minions, 'derived-minions.txt': { text: '# monster id, name, skills exported by rule (no hand entry in export/hand/minions.json)\n' + lines.join('\n') + '\n' } };
  }
  return { 'minions.json': minions };
}
