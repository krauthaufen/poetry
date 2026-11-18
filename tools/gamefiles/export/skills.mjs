// skills.json and gems.json: every skill gem, every granted effect a gem or a player can have, and the
// skills of the monsters in minions.json. Follows PoB's Export/Scripts/skills.lua, without its hand-kept
// skill lists (Export/Skills/*.txt): which skills exist comes from the tables.
//
// Column names are the community schema's; where PoB's spec.lua calls a column differently the PoB name
// is given in a comment, because skills.lua is the specification of what each value means.
import { escapeGGG, sanitise } from './lib/text.mjs';
import { LuaTable, byInt, flagSet } from './lib/lua.mjs';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { GAMEFILES_DIR } from './lib/game.mjs';
import { monsterSkillIds, minionVarieties } from './minions.mjs';
import { deriveBaseFlags } from './baseflags.mjs';
import { MAIN_FILES, specificFiles } from './statdesc.mjs';

// ItemClasses.Id -> the weapon type names the simulator's item bases use
const weaponClassMap = {
  'Claw': 'Claw', 'Dagger': 'Dagger', 'One Hand Sword': 'One Hand Sword', 'Thrusting One Hand Sword': 'Thrusting One Hand Sword',
  'One Hand Axe': 'One Hand Axe', 'One Hand Mace': 'One Hand Mace', 'Bow': 'Bow', 'Crossbow': 'Crossbow', 'Fishing Rod': 'Fishing Rod',
  'Warstaff': 'Staff', 'Two Hand Sword': 'Two Hand Sword', 'Two Hand Axe': 'Two Hand Axe', 'Two Hand Mace': 'Two Hand Mace',
  'Unarmed': 'None', 'Flail': 'Flail', 'Spear': 'Spear', 'Talisman': 'Talisman',
};
// gems whose item name keeps its " Support" (name collision with an active skill)
const fullNameGems = new Set(['Metadata/Items/Gems/SupportGemBarrage']);

const concat = (a, b) => [...(a || []), ...(b || [])];

export function run(game, ctx) {
  const T = n => game.table(n);
  const GE = T('GrantedEffects'), AS = T('ActiveSkills'), AST = T('ActiveSkillType'), STATS = T('Stats');
  const GEMFX = T('GemEffects'), SKILLGEMS = T('SkillGems'), BASEITEMS = T('BaseItemTypes'), GESS = T('GrantedEffectStatSets');
  const statId = i => STATS[i].Id;
  const typeName = i => AST[i] ? AST[i].Id : 'Unknown' + (i + 1);

  // gem effect -> the first gem that has it
  const gemOfEffect = new Map();
  for (const gem of SKILLGEMS) for (const e of gem.GemEffects) if (!gemOfEffect.has(e)) gemOfEffect.set(e, gem);
  const gemColor = gem => gem.StrengthRequirementPercent >= 50 ? 1 : gem.IntelligenceRequirementPercent >= 50 ? 3 : gem.DexterityRequirementPercent >= 50 ? 2 : 4;
  // GemType: 0 active skill, 1 support, 2 meta skill (PoB reads its first byte as the flag "IsSupport")
  const gemIsSupport = gem => gem.GemType === 1;
  const supportGemOf = gem => game.rowBy('SupportGems', 'SkillGem', gem._index);
  const gemLevelsOf = gem => game.rowsBy('ItemExperiencePerLevel', 'ItemExperienceType', gem.ItemExperienceType);   // PoB: GemLevelProgression
  const statSetById = id => game.rowBy('GrantedEffectStatSets', 'Id', id);

  // ---- which granted effects are skills of ours ----
  const wanted = new Map();   // GrantedEffects row index -> why
  const want = (i, why) => { if (i !== null && i !== undefined && !wanted.has(i)) wanted.set(i, why); };
  for (const e of GEMFX) { want(e.GrantedEffect, 'gem'); for (const a of e.AdditionalGrantedEffects) want(a, 'gem'); }
  for (const r of GE) if (/Player/.test(r.Id)) want(r._index, 'player');
  for (const id of monsterSkillIds(game)) { const r = game.rowBy('GrantedEffects', 'Id', id); if (r) want(r._index, 'monster'); }
  if (ctx.allEffects) for (const r of GE) want(r._index, 'all');

  const trueGemNames = new Map();
  const gemsUsed = new Set();
  const skills = {};
  const skipped = [];

  for (const gi of [...wanted.keys()].sort((a, b) => a - b)) {
    const granted = GE[gi];
    const active = game.ref('ActiveSkills', granted.ActiveSkill);
    if (!granted.IsSupport && !active) { skipped.push(granted.Id); continue; }
    // Only a gem's first effect counts as "the gem's skill". A further effect of a gem (the support part of a
    // meta gem, the explosion of a skill) is exported as a hidden skill with all its levels, as PoB does:
    // its lookup of the gem by an additional effect never matches.
    const gemEffect = game.rowBy('GemEffects', 'GrantedEffect', gi);
    const secondary = false;
    const skillGem = gemEffect ? gemOfEffect.get(gemEffect._index) : null;
    if (skillGem && gemEffect.Name !== '') trueGemNames.set(gemEffect.Id, gemEffect.Name);

    const out = {};
    let displayName;
    const perLevel = game.rowsBy('GrantedEffectsPerLevel', 'GrantedEffect', gi);
    let gemLevels = perLevel.length;
    if (skillGem) {
      gemsUsed.add(gemEffect.Id);
      const baseItem = BASEITEMS[skillGem.BaseItemType];
      if (granted.IsSupport) {
        displayName = sanitise(fullNameGems.has(baseItem.Id) ? baseItem.Name : baseItem.Name.replace(/ Support/g, ''));
        out.name = displayName;
        if (gemEffect.SupportText.length > 0) out.description = escapeGGG(gemEffect.SupportText.replace(/\r/g, ''));
        gemLevels = Math.min(1, perLevel.length);
      } else {
        displayName = secondary ? active.DisplayedName : (trueGemNames.get(gemEffect.Id) || active.DisplayedName);
        out.name = sanitise(displayName);
        out.baseTypeName = active.DisplayedName;
      }
      out.color = gemColor(skillGem);
    } else {
      // many monster skills have no display name (PoB names them by hand in its lists): the id then
      displayName = granted.IsSupport ? granted.Id : ((gemEffect && trueGemNames.get(gemEffect.Id)) || active.DisplayedName || granted.Id);
      out.name = displayName;
      displayName = sanitise(displayName);
      out.hidden = true;
    }

    // ---- levels ----
    // the per-level rows of the stat set that carries the skill's own crit chance and attack multiplier
    let statsPerLevel = game.rowsBy('GrantedEffectStatSetsPerLevel', 'GrantedEffects', gi);
    const ownSet = statSetById(granted.Id);
    const primaryPerLevel = ownSet ? game.rowsBy('GrantedEffectStatSetsPerLevel', 'StatSet', ownSet._index) : [];
    if (primaryPerLevel.length >= gemLevels && primaryPerLevel.length > 0) statsPerLevel = primaryPerLevel;
    const progression = skillGem ? gemLevelsOf(skillGem) : null;
    const costTypes = game.refs('CostTypes', granted.CostTypes);
    const baseStatRow = [];
    const skillLevels = [];   // level tables in order, for the stat sets
    let nextReq = 0;
    const levels = [];
    for (let i = 0; i < gemLevels; i++) {
      const lr = perLevel[i], sr = statsPerLevel[i];
      baseStatRow[i] = sr;
      const lv = {}, cost = {};
      lv.levelRequirement = Math.max((progression && progression[i] && progression[i].Level) || 0, nextReq);   // PoB: PlayerLevel
      nextReq = lv.levelRequirement;
      costTypes.forEach((c, k) => { cost[c.Id] = lr.CostAmounts[k]; });
      if (lr.Reservation !== 0) lv.spiritReservationFlat = lr.Reservation;                         // PoB: SpiritReservation
      if (lr.EffectOnPlayer !== 100) lv.reservationMultiplier = lr.EffectOnPlayer - 100;           // PoB: ReservationMultiplier
      if (lr.CostMultiplier !== 100) lv.manaMultiplier = lr.CostMultiplier - 100;
      if (lr.AttackSpeedMultiplier) lv.attackSpeedMultiplier = lr.AttackSpeedMultiplier;
      if (lr.AttackTime !== 0) lv.attackTime = lr.AttackTime;
      if (lr.Cooldown) lv.cooldown = lr.Cooldown / 1000;
      if (lr.PvPDamageMultiplier !== 0) lv.PvPDamageMultiplier = lr.PvPDamageMultiplier;
      if (lr.StoredUses !== 0) lv.storedUses = lr.StoredUses;
      if (sr && sr.SpellCritChance !== 0) lv.critChance = sr.SpellCritChance / 100;                // PoB: AttackCritChance
      if (sr && sr.AttackCritChance !== 0) lv.critChance = sr.AttackCritChance / 100;              // PoB: OffhandCritChance
      if (sr && sr.BaseMultiplier) lv.baseMultiplier = sr.BaseMultiplier / 10000 + 1;
      if (lr.VaalSouls !== 0) cost.Soul = lr.VaalSouls;
      if (lr.VaalStoredUses !== 0) lv.vaalStoredUses = lr.VaalStoredUses;
      if (lr.SoulGainPreventionDuration !== 0) lv.soulPreventionDuration = lr.SoulGainPreventionDuration / 1000;
      if (Object.keys(cost).length) lv.cost = cost;
      skillLevels.push(lv);
      levels.push([lr.Level, lv]);
    }

    if (!(skillGem && granted.IsSupport)) {
      out.qualityStats = [];
      out.altQualityStats = [];
      const q = game.rowBy('GrantedEffectQualityStats', 'GrantedEffect', gi);
      if (q) {
        q.Stats.forEach((s, k) => out.qualityStats.push([statId(s), q.StatsValuesPermille[k] / 1000, q.ApplyToStatSets]));
        q.AltStats.forEach((s, k) => out.altQualityStats.push([statId(s), q.AltStatValuesPermille[k] / 1000, q.AltApplyToStatSets]));
      }
    }

    let isMeta = false;
    if (granted.IsSupport) {
      out.support = true;
      out.requireSkillTypes = granted.AllowedActiveSkillTypes.map(typeName);
      out.addSkillTypes = granted.AddedActiveSkillTypes.map(typeName);
      out.excludeSkillTypes = granted.ExcludedActiveSkillTypes.map(typeName);
      if (out.addSkillTypes.includes('Triggered')) out.isTrigger = true;
      if (skillGem) {
        const sg = supportGemOf(skillGem);
        const family = sg ? game.refs('SupportGemFamily', sg.Family).map(f => f.Id) : [];
        if (family.length) out.gemFamily = family;
      }
      const viaMeta = game.rowBy('GemEffects', 'AdditionalGrantedEffects', gi);
      if (viaMeta && game.refs('GemTags', viaMeta.GemTags).some(t => t.Id === 'meta')) isMeta = true;
    } else {
      if (active.Description.length > 0) out.description = escapeGGG(active.Description.replace(/\r/g, ''));
      out.skillTypes = flagSet(active.ActiveSkillTypes.map(typeName));
      if (active.MinionActiveSkillTypes.length) out.minionSkillTypes = flagSet(active.MinionActiveSkillTypes.map(typeName));
      const req = game.ref('ActiveSkillWeaponRequirement', active.WeaponRequirements);
      const weaponTypes = [];
      if (req) for (const w of game.refs('WieldableClasses', req.WieldableClasses)) {
        const cls = game.ref('ItemClasses', w.ItemClass);
        if (cls && weaponClassMap[cls.Id]) weaponTypes.push(weaponClassMap[cls.Id]);
      }
      if (weaponTypes.length) out.weaponTypes = flagSet(weaponTypes);
      out.castTime = granted.CastTime / 1000;
    }
    out.levels = byInt(levels);

    // ---- stat sets: the skill's own, then the additional ones ----
    const setRows = [game.ref('GrantedEffectStatSets', granted.StatSet), ...game.refs('GrantedEffectStatSets', granted.AdditionalStatSets)].filter(r => r);
    const state = { game, statId, granted, active, displayName, baseStatRow, skillLevels, baseSet: null, isMeta };
    out.statSets = setRows.map((row, k) => statSet(state, row, k + 1));
    if (!out.statSets.length) delete out.statSets;

    if (!granted.IsSupport) {
      for (const set of out.statSets || []) set.baseFlags = deriveBaseFlags(game, granted, active, set, out);
      const minionList = minionListOf(game, granted, active, out);
      if (minionList.length) out.minionList = minionList;
    }
    if (ctx.use) {
      const used = new Set();
      for (const set of out.statSets || []) { for (const id of set.stats) used.add(id); for (const c of set.constantStats || []) used.add(c[0]); }
      for (const q of [...(out.qualityStats || []), ...(out.altQualityStats || [])]) used.add(q[0]);
      for (const id of used) ctx.use('skills', id, granted.Id);
    }
    skills[granted.Id] = out;
  }
  if (skipped.length) console.log(`  ${skipped.length} granted effects have no active skill and are no support: skipped (${skipped.slice(0, 5).join(', ')}...)`);

  // every description file a skill names should exist
  const scopes = new Set([...MAIN_FILES, ...specificFiles(game).map(f => f[0])]);
  const noScope = new Map();
  for (const [id, s] of Object.entries(skills)) for (const set of s.statSets || []) if (!scopes.has(set.statDescriptionScope)) noScope.set(set.statDescriptionScope, id);
  if (noScope.size) console.log(`  stat description files named by a skill but not in the game: ${[...noScope].map(([f, id]) => `${f} (${id})`).join(', ')}`);
  if (minionLinks) {
    if (minionLinks.guessed.length) console.log(`  minion skills linked to a monster by name: ${minionLinks.guessed.join('; ')}`);
    if (minionLinks.unlinked.length) console.log(`  minion skills without a monster (add them to export/hand/minion-lists.json): ${minionLinks.unlinked.join(', ')}`);
  }

  // ---- gems.json ----
  const gems = {};
  for (const gem of SKILLGEMS) {
    const baseItem = BASEITEMS[gem.BaseItemType];
    for (const ei of gem.GemEffects) {
      const e = GEMFX[ei];
      if (!gemsUsed.has(e.Id) || e.GrantedEffect === null) continue;
      const granted = GE[e.GrantedEffect];
      const active = game.ref('ActiveSkills', granted.ActiveSkill);
      const support = gemIsSupport(gem);
      const g = {};
      g.name = sanitise(fullNameGems.has(baseItem.Id) ? baseItem.Name : (trueGemNames.get(e.Id) || baseItem.Name.replace(/ Support/g, '')));
      if (!support && active) g.baseTypeName = active.DisplayedName;
      g.gameId = baseItem.Id;
      g.variantId = e.Id;
      g.grantedEffectId = granted.Id;
      granted.AdditionalStatSets.forEach((s, k) => { g['additionalStatSet' + (k + 1)] = GESS[s].Id; });
      e.AdditionalGrantedEffects.forEach((a, k) => { g['additionalGrantedEffectId' + (k + 1)] = GE[a].Id; });
      if (e.GrantedEffectDisplayOrder.length) g.grantedEffectDisplayOrder = e.GrantedEffectDisplayOrder;
      if (e.SupportName.length > 0) g.secondaryEffectName = e.SupportName;
      if (gem.IsVaalVariant) g.vaalGem = true;
      const tags = {};
      let gemType;
      const tagNames = [];
      for (const tag of game.refs('GemTags', e.GemTags)) {
        tags[tag.Id] = true;
        if (tag.Name.length > 0) {
          const n = escapeGGG(tag.Name);
          if (gemType === undefined) gemType = n; else tagNames.push(n);
        }
      }
      g.tags = tags;
      const weaponReq = [];
      const req = active ? game.ref('ActiveSkillWeaponRequirement', active.WeaponRequirements) : null;
      if (req) {
        const str = game.ref('ClientStrings', req.String);
        if (str) weaponReq.push(escapeGGG(str.Text));
        else for (const w of game.refs('WieldableClasses', req.WieldableClasses)) {
          const cls = game.ref('ItemClasses', w.ItemClass);
          if (cls && weaponClassMap[cls.Id]) { const cat = game.ref('ItemClassCategories', cls.ItemClassCategory); if (cat) weaponReq.push(escapeGGG(cat.Text)); }
        }
      }
      if (gemType !== undefined) g.gemType = gemType;
      if (support) {
        const sg = supportGemOf(gem);
        const family = sg ? game.refs('SupportGemFamily', sg.Family).map(f => f.Text) : [];
        if (family.length) g.gemFamily = family.join(', ');
      }
      g.tagString = tagNames.join(', ');
      if (weaponReq.length) g.weaponRequirements = weaponReq.join(', ');
      g.reqStr = gem.StrengthRequirementPercent;
      g.reqDex = gem.DexterityRequirementPercent;
      g.reqInt = gem.IntelligenceRequirementPercent;
      g.Tier = gem.CraftingLevel;   // PoB: Tier
      const natural = support ? 1 : gemLevelsOf(gem).length;
      g.naturalMaxLevel = natural > 0 ? natural : 1;
      gems['Metadata/Items/Gems/SkillGem' + e.Id] = g;
    }
  }
  return { 'skills.json': skills, 'gems.json': gems };
}

// One stat set of a skill (skills.lua, the "#set" and "#mods" directives).
function statSet(st, original, setIndex) {
  const { game, statId } = st;
  const STATS = game.table('Stats');
  const ss = { ...original };
  const perLevel = game.rowsBy('GrantedEffectStatSetsPerLevel', 'StatSet', original._index).map(r => ({
    ...r, BaseResolvedValues: [...r.BaseResolvedValues], AdditionalStatsValues: [...r.AdditionalStatsValues],
    StatInterpolations: [...r.StatInterpolations], FloatStats: [...r.FloatStats], FloatStatsValues: [...r.FloatStatsValues],
    InterpolationBases: [...r.InterpolationBases], AdditionalStats: [...r.AdditionalStats],
  }));
  const labelRow = game.ref('GrantedEffectLabels', ss.Label);
  const label = sanitise(labelRow ? labelRow.Text : st.displayName);
  const set = { stats: [], notMinion: [], constantStats: [], levels: [] };
  let removeStats = ss.IgnoredStats.map(statId);   // PoB: RemoveStats

  if (setIndex === 1) {
    st.baseSet = ss;
  } else {
    // a further set is the first set plus its own
    const b = st.baseSet;
    ss.ImplicitStats = concat(b.ImplicitStats, ss.ImplicitStats);
    ss.ConstantStats = concat(b.ConstantStats, ss.ConstantStats);
    ss.ConstantStatsValues = concat(b.ConstantStatsValues, ss.ConstantStatsValues);
    if (ss.BaseEffectiveness === 1) ss.BaseEffectiveness = b.BaseEffectiveness;
    if (ss.IncrementalEffectiveness === 0) ss.IncrementalEffectiveness = b.IncrementalEffectiveness;
    if (ss.DamageIncrementalEffectiveness === 0) ss.DamageIncrementalEffectiveness = b.DamageIncrementalEffectiveness;
  }

  const statMap = new Set();
  const order = [];
  const addStat = (id, first) => {
    statMap.add(id);
    set.stats.push(id);
    if (first) order.push(id);
  };
  const noteMinion = s => { if (STATS[s].CannotGrantToMinion && !set.notMinion.includes(STATS[s].Id)) set.notMinion.push(STATS[s].Id); };
  const dense = a => { let n = 0; while (a[n] !== undefined) n++; return n; };
  const EMPTY = { SpellCritChance: 0, AttackCritChance: 0, BaseResolvedValues: [], FloatStats: [], FloatStatsValues: [], StatInterpolations: [], InterpolationBases: [], AdditionalStats: [], AdditionalStatsValues: [] };

  perLevel.forEach((row, n) => {
    const first = n === 0;
    const base = st.baseStatRow[n] || EMPTY;
    const extra = {};
    if (setIndex !== 1 && row.SpellCritChance !== 0) extra.critChance = (base.SpellCritChance + row.SpellCritChance) / 100;
    if (setIndex !== 1 && row.AttackCritChance !== 0) extra.critChance = (base.AttackCritChance + row.AttackCritChance) / 100;
    if (setIndex !== 1 && row.BaseMultiplier) extra.baseMultiplier = row.BaseMultiplier / 10000 + 1;
    let baseStats = [];
    if (setIndex !== 1) {
      row.BaseResolvedValues = concat(base.BaseResolvedValues, row.BaseResolvedValues);
      row.FloatStats = concat(base.FloatStats, row.FloatStats);
      row.FloatStatsValues = concat(base.FloatStatsValues, row.FloatStatsValues);
      row.StatInterpolations = concat(base.StatInterpolations, row.StatInterpolations);
      row.InterpolationBases = concat(base.InterpolationBases, row.InterpolationBases);
      row.AdditionalStats = concat(base.AdditionalStats, row.AdditionalStats);
      row.AdditionalStatsValues = concat(base.AdditionalStatsValues, row.AdditionalStatsValues);
      baseStats = [...st.baseSet.ImplicitStats, ...st.baseSet.ConstantStats, ...base.FloatStats, ...base.AdditionalStats].map(statId);
    }
    const interp = row.StatInterpolations;
    const values = [];
    // an ignored stat the first set does not have stays
    const temp = [...removeStats];
    for (let i = 0; i < removeStats.length; i++) {
      if (!baseStats.includes(removeStats[i])) { temp.splice(i, 1); removeStats.splice(i, 1); }
    }
    const takeRemoved = id => { const k = temp.indexOf(id); if (k < 0) return false; temp.splice(k, 1); return true; };
    let oi = 1;   // position in the order of the first level's stats (1-based, as in skills.lua)
    const place = (s) => {
      const id = statId(s);
      if (!statMap.has(id) || first) {
        addStat(id, first);
        if (first) noteMinion(s);
      } else if (order[oi - 1] !== id) {
        // a stat of the first level is missing here: fill with 0
        while (oi < order.length && order[oi - 1] !== id) {
          values.push(0);
          if (dense(interp) < order.length) interp.splice(Math.min(oi - 1, interp.length), 0, 0);
          oi++;
        }
      }
    };
    row.FloatStats.forEach((s, i) => {
      if (takeRemoved(statId(s))) row.BaseResolvedValues[i] = 0;
      place(s);
      if (perLevel.length > 5) {
        values.push(row.BaseResolvedValues[i]);
        if (setIndex !== 1) interp[values.length - 1] = 1; else interp[oi - 1] = 1;
      } else {
        const ib = game.ref('EffectivenessCostConstants', row.InterpolationBases[i]);
        values.push(row.FloatStatsValues[i] / Math.max(ib ? ib.Multiplier : 0, 0.00001));   // PoB: Value
      }
      oi++;
    });
    row.AdditionalStats.forEach((s, i) => {
      if (takeRemoved(statId(s))) row.AdditionalStatsValues[i] = 0;
      place(s);
      values.push(row.AdditionalStatsValues[i]);
      interp[oi - 1] = 1;
      oi++;
    });
    for (const s of row.AdditionalFlags) {   // PoB: AdditionalBooleanStats
      const id = statId(s);
      if (takeRemoved(id)) continue;
      if (!statMap.has(id)) { addStat(id, false); noteMinion(s); }
    }
    const named = { ...extra, actorLevel: row.ActorLevel };
    if (interp.some(x => x !== undefined)) named.statInterpolation = interp.slice(0, dense(interp));
    set.levels.push([row.GemLevel, new LuaTable(values.map(v => (v === undefined ? 0 : v)), named)]);
  });

  const dropRemoved = id => { const k = removeStats.indexOf(id); if (k < 0) return false; removeStats.splice(k, 1); return true; };
  for (const s of ss.ImplicitStats) {
    const id = statId(s);
    if (dropRemoved(id)) continue;
    if (!statMap.has(id)) addStat(id, false);
  }
  ss.ConstantStats.forEach((s, i) => {
    const id = statId(s);
    if (dropRemoved(id)) return;
    set.constantStats.push([id, ss.ConstantStatsValues[i]]);
  });

  const out = { label };
  if (ss.BaseEffectiveness !== 1) out.baseEffectiveness = ss.BaseEffectiveness;
  if (ss.IncrementalEffectiveness !== 0) out.incrementalEffectiveness = ss.IncrementalEffectiveness;
  if (ss.DamageIncrementalEffectiveness !== 0) out.damageIncrementalEffectiveness = ss.DamageIncrementalEffectiveness;
  if (st.granted.IsSupport) {
    out.statDescriptionScope = st.isMeta ? 'meta_gem_stat_descriptions' : 'gem_stat_descriptions';
  } else {
    out.statDescriptionScope = st.active.StatDescription
      .replace(/^Data\/StatDescriptions\//i, '').replace(/specific_skill_stat_descriptions\//gi, '')
      .replace(/statset_0/g, 'statset_' + (setIndex - 1)).replace(/\/$/, '').replace(/\//g, '_').replace(/.csd/g, '');
  }
  out.baseFlags = {};
  if (set.constantStats.length) out.constantStats = set.constantStats;
  out.stats = set.stats;
  if (set.notMinion.length) out.notMinionStat = set.notMinion;
  out.levels = byInt(set.levels);
  return out;
}

// The monsters a skill summons. The client data has no table for this, so: hand/minion-lists.json first,
// else the player-minion monster whose name is the skill's name ("Skeletal Sniper", "Raging Spirits").
let minionLinks = null;
function minionListOf(game, granted, active, out) {
  if (!minionLinks) {
    const hand = JSON.parse(fs.readFileSync(path.join(GAMEFILES_DIR, 'export', 'hand', 'minion-lists.json'), 'utf8')).skills;
    const byName = new Map();
    for (const { row, why } of minionVarieties(game)) {
      if (why !== 'minion') continue;
      const n = normName(row.Name);
      if (!byName.has(n)) byName.set(n, []);
      byName.get(n).push(row.Id);
    }
    minionLinks = { hand, byName, unlinked: [], guessed: [] };
  }
  const known = new Set(minionVarieties(game).map(m => m.row.Id));
  if (minionLinks.hand[granted.Id]) return minionLinks.hand[granted.Id].filter(id => known.has(id));
  if (!out.skillTypes || !out.skillTypes.CreatesMinion) return [];
  const found = minionLinks.byName.get(normName(out.name)) || [];
  if (found.length === 1) { minionLinks.guessed.push(`${granted.Id} -> ${found[0]}`); return found; }
  minionLinks.unlinked.push(granted.Id);
  return [];
}
function normName(n) {
  return n.toLowerCase().replace(/[^a-z ]/g, '').split(/\s+/).filter(w => w && !['summon', 'summoned', 'raise', 'raised', 'companion'].includes(w)).map(w => w.replace(/s$/, '')).join(' ');
}
export function minionLinkReport() { return minionLinks; }
