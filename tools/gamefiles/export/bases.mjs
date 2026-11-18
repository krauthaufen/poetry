// bases.json: every base item that can be worn (weapons, armour, jewellery, flasks, charms, jewels), keyed by
// name. Fields as PoB's Export/Scripts/bases.lua writes them. Which bases exist comes from the tables: every
// BaseItemTypes row whose item class is equipment; PoB's hand-kept lists (Export/Bases/*.txt) are not used.
import { describeMod, describeStats, modStats } from './describe.mjs';
import { round } from './lib/text.mjs';
import { flagSet } from './lib/lua.mjs';

// ItemClasses.Id -> the simulator's type name, where they differ, and a sub type the class itself implies
const CLASS = {
  Warstaff: { type: 'Staff', subType: 'Warstaff' }, Buckler: { type: 'Shield' }, LifeFlask: { type: 'Flask', subType: 'Life' },
  ManaFlask: { type: 'Flask', subType: 'Mana' }, UtilityFlask: { type: 'Charm' }, FishingRod: { type: 'Fishing Rod' },
};
// classes that are equipment without a row in WeaponTypes or ArmourTypes
const ARMOUR_CLASSES = new Set(['Body Armour', 'Boots', 'Gloves', 'Helmet', 'Shield', 'Buckler']);
const OTHER_EQUIPMENT = new Set(['Amulet', 'Ring', 'Belt', 'Quiver', 'Jewel', 'LifeFlask', 'ManaFlask', 'UtilityFlask', 'TrapTool', 'Sceptre', 'Wand', 'Staff', 'Focus', 'Talisman']);

export function run(game) {
  const BASE = game.table('BaseItemTypes'), IC = game.table('ItemClasses'), TAGS = game.table('Tags'), MODS = game.table('Mods'), STATS = game.table('Stats');
  const MODTYPE = game.table('ModType');
  const itCache = new Map();
  const itLines = path => {
    if (!itCache.has(path)) { const t = game.readText(path + '.it'); itCache.set(path, t === null ? null : t.split(/[\r\n]+/)); }
    return itCache.get(path);
  };
  // tags and maximum quality come from the item's .it file and the files it extends
  const itTags = path => {
    if (path === 'nothing') return [];
    const lines = itLines(path);
    if (!lines) return null;
    const tags = [];
    for (const line of lines) {
      const sup = line.match(/extends "(.+)"/);
      if (sup) { const t = itTags(sup[1]); if (t) tags.push(...t); }
      else if (/remove_tag/.test(line)) {
        const m = line.match(/remove_tag = "(.+)"/);
        const i = m ? tags.indexOf(m[1]) : -1;
        if (i >= 0) tags.splice(i, 1); else tags.pop();   // as bases.lua: an unknown tag removes the last one
      } else if (/tag/.test(line)) { const m = line.match(/tag = "(.+)"/); if (m) tags.push(m[1]); }
    }
    return tags;
  };
  const itQuality = path => {
    if (path === 'nothing') return 0;
    const lines = itLines(path);
    if (!lines) return null;
    let inherited;
    for (const line of lines) {
      const sup = line.match(/extends "(.+)"/);
      if (sup) inherited = itQuality(sup[1]);
      else if (/max_quality/.test(line)) { const m = line.match(/max_quality = (.+)/); if (m) return Number(m[1]); }
    }
    return inherited;
  };

  const beltCharmSlots = game.rowBy('Mods', 'Id', 'BeltImplicitCharmSlots3');
  const bases = {};
  let skippedNoFile = 0;
  for (const b of BASE) {
    const cls = IC[b.ItemClass];
    if (!cls) continue;
    const weaponType = game.rowBy('WeaponTypes', 'BaseItemType', b._index);
    const armourType = game.rowBy('ArmourTypes', 'BaseItemType', b._index);
    if (!weaponType && !armourType && !OTHER_EQUIPMENT.has(cls.Id)) continue;
    if (/^Heist/.test(cls.Id)) continue;
    const type = (CLASS[cls.Id] && CLASS[cls.Id].type) || cls.Id;
    let subType = CLASS[cls.Id] && CLASS[cls.Id].subType;
    if (armourType && ARMOUR_CLASSES.has(cls.Id)) {
      // PoB names the sub type by hand from the item id: Str = Armour, Dex = Evasion, Int = Energy Shield.
      // Same rule here; a base whose id says nothing gets it from the defences it has.
      const m = b.Id.split('/').pop().match(/(?:Body|Boots|Gloves|Helmet|Shield|Buckler)((?:Str|Dex|Int)+)/);
      const parts = m
        ? [m[1].includes('Str') && 'Armour', m[1].includes('Dex') && 'Evasion', m[1].includes('Int') && 'Energy Shield']
        : [armourType.Armour > 0 && 'Armour', armourType.Evasion > 0 && 'Evasion', armourType.EnergyShield > 0 && 'Energy Shield'];
      subType = parts.filter(x => x).join('/') || undefined;
    }
    if (cls.Id === 'Jewel') subType = /Radius/.test(b.Id) ? 'Radius' : /Timeless/.test(b.Id) ? 'Timeless' : undefined;
    let name = b.Name.replace(/ö/g, 'o').trim();
    if (name === '') continue;
    if (name === 'Energy Blade') name = type === 'One Hand Sword' ? 'Energy Blade One Handed' : 'Energy Blade Two Handed';
    if (name.includes('DNT')) continue;
    const baseTags = itTags(b.InheritsFrom);
    const quality = itQuality(b.InheritsFrom);
    if (baseTags === null) skippedNoFile++;
    const o = { type };
    if (subType) o.subType = subType;
    if (quality !== 0 && quality !== null && quality !== undefined) o.quality = quality;
    if (type === 'Belt') o.charmLimit = 0;
    const spirit = game.rowBy('ItemSpirit', 'BaseItemType', b._index);
    if (spirit) o.spirit = spirit.SpiritGranted;
    if (b.Id.includes('Unique') || name.includes('Runemastered')) o.hidden = true;
    const tags = new Set([...(baseTags || []), ...b.Tags.map(i => TAGS[i].Id)]);
    if (tags.has('belt') || tags.has('ring')) { tags.add('genesis_tree_minion'); tags.add('genesis_tree_caster'); }
    // rune sockets: PoB sets the limit by hand per type; it is 4 for body armour and two-handed weapons, else 3
    if (weaponType || armourType || ['Sceptre', 'Wand', 'Staff', 'Focus', 'Talisman'].includes(cls.Id)) {
      o.socketLimit = (cls.Id === 'Body Armour' || tags.has('two_hand_weapon') || tags.has('twohand')) ? 4 : 3;
    }
    o.tags = flagSet([...tags].sort());

    // ---- implicits ----
    const implicitMods = b.Implicit_Mods.map(i => MODS[i]);
    const shown = [...implicitMods];
    if (type === 'Belt' && !implicitMods.some(m => MODTYPE[m.ModType] && MODTYPE[m.ModType].Name === 'CharmSlots') && beltCharmSlots) shown.push(beltCharmSlots);
    const described = shown.map(m => ({ m, d: describeMod(game, m) }));
    described.sort((x, y) => (x.d.orders[0] || 0) - (y.d.orders[0] || 0));
    const implicitLines = [], implicitModTypes = [];
    for (const { m, d } of described) {
      for (const line of d.lines) { implicitLines.push(line); implicitModTypes.push(d.modTags); }
      if (m.Id === 'SpearImplicitDisplaySpearThrow1') implicitLines.push('Grants Skill: Spear Throw');
    }
    const inherent = game.rowBy('ItemInherentSkills', 'BaseItemType', b._index);
    const variantList = [];
    if (inherent) {
      if (inherent.IsWeapon) o.grantedSkillsHaveNoReservation = true;   // PoB: NoReservation
      const many = inherent.SkillsGranted.length > 1;
      inherent.SkillsGranted.forEach((gi, k) => {
        const gem = game.table('SkillGems')[gi];
        const effect = gem && game.ref('GemEffects', gem.GemEffects[0]);
        const granted = effect && game.ref('GrantedEffects', effect.GrantedEffect);
        const active = granted && game.ref('ActiveSkills', granted.ActiveSkill);
        if (!active) return;
        let levels = gem.GemType === 1 ? 1 : game.rowsBy('ItemExperiencePerLevel', 'ItemExperienceType', gem.ItemExperienceType).length;
        if (levels <= 0) levels = 1;
        let line = 'Grants Skill: ' + (levels === 1 ? '' : `Level (1-${levels}) `) + active.DisplayedName;
        if (many) { variantList.push(active.DisplayedName); line = `{variant:${k + 1}}` + line; }
        implicitLines.push(line);
      });
    }
    if (variantList.length) o.variantList = variantList;
    if (implicitLines.length) o.implicit = implicitLines.join('\n');
    o.implicitModTypes = implicitModTypes;

    // ---- weapon ----
    if (weaponType) {
      const w = {};
      const TYPES = ['Physical', 'Fire', 'Cold', 'Lightning', 'Chaos'];
      const conv = { Physical: 100, Fire: 0, Cold: 0, Lightning: 0, Chaos: 0 }, addMin = {}, addMax = {};
      let total = 0;
      for (const m of implicitMods) for (const [id, v] of modStats(game, m)) {
        let x = id.match(/^local_weapon_implicit_hidden_%_base_damage_is_(fire|cold|lightning|chaos)$/);
        if (x) { const t = x[1][0].toUpperCase() + x[1].slice(1); conv[t] += v.min; total += v.min; }
        x = id.match(/^local_weapon_implicit_hidden_added_(minimum|maximum)_(fire|cold|lightning|chaos)_damage$/);
        if (x) { const t = x[2][0].toUpperCase() + x[2].slice(1); const tgt = x[1] === 'minimum' ? addMin : addMax; tgt[t] = (tgt[t] || 0) + v.min; }
      }
      const factor = total > 100 ? 100 / total : 1;
      for (const t of TYPES) {
        const c = t === 'Physical' ? 1 - Math.min(total / 100, 1) : conv[t] * factor / 100;
        if (c !== 0) { w[t + 'Min'] = Math.floor(weaponType.DamageMin * c); w[t + 'Max'] = Math.floor(weaponType.DamageMax * c); }
        if (addMin[t]) w[t + 'Min'] = addMin[t];
        if (addMax[t]) w[t + 'Max'] = addMax[t];
      }
      w.CritChanceBase = weaponType.CritChance / 100;
      w.AttackRateBase = round(1000 / weaponType.Speed, 2);
      w.Range = weaponType.RangeMax;
      if (weaponType.ReloadTime > 0) w.ReloadTimeBase = round(weaponType.ReloadTime / 1000, 2);
      o.weapon = w;
    }
    // ---- armour ----
    if (armourType) {
      const a = {};
      const shield = game.rowBy('ShieldTypes', 'BaseItemType', b._index);
      if (shield) a.BlockChance = shield.Block;
      if (armourType.Armour > 0) a.Armour = armourType.Armour;
      if (armourType.Evasion > 0) a.Evasion = armourType.Evasion;
      if (armourType.EnergyShield > 0) a.EnergyShield = armourType.EnergyShield;
      if (armourType.Ward > 0) a.Ward = armourType.Ward;
      if (armourType.IncreasedMovementSpeed !== 0) a.MovementPenalty = -armourType.IncreasedMovementSpeed / 10000;
      o.armour = a;
    }
    // ---- flask, charm ----
    if (type === 'Flask' || type === 'Charm') {
      const flask = game.rowBy('Flasks', 'BaseItemType', b._index);
      const charges = game.rowBy('ComponentCharges', 'BaseItemTypesKey', b.Id);
      if (flask && charges) {
        const f = {};
        if (flask.LifePerUse > 0) f.life = flask.LifePerUse;
        if (flask.ManaPerUse > 0) f.mana = flask.ManaPerUse;
        f.duration = flask.RecoveryTime / 10;
        f.chargesUsed = charges.PerCharge;
        f.chargesMax = charges.MaxCharges;
        if (flask.UtilityBuff.length) {
          const stats = new Map();
          for (const u of game.refs('UtilityFlaskBuffs', flask.UtilityBuff)) {
            const def = game.ref('BuffDefinitions', u.BuffDefinition);
            if (!def) continue;
            def.GrantedStats.forEach((s, k) => stats.set(STATS[s].Id, { min: u.StatValues[k], max: u.StatValues[k] }));
            def.GrantedFlags.forEach(s => stats.set(STATS[s].Id, { min: 1, max: 1 }));
          }
          f.buff = describeStats(game, stats).lines;
          if (f.buff.length === 0) f.buff = [''];   // as bases.lua writes it
        }
        o[type === 'Charm' ? 'charm' : 'flask'] = f;
      }
    }
    // ---- requirements ----
    const req = {};
    let level = 1;
    const att = game.rowBy('AttributeRequirements', 'BaseItemType', b._index);
    if ((weaponType || armourType || inherent || att) && b.DropLevel > 4) level = b.DropLevel;
    if ((type === 'Flask' || type === 'Charm') && b.DropLevel > 2) level = b.DropLevel;
    for (const m of implicitMods) level = Math.max(level, Math.floor(m.Level * 0.8));
    if (level > 1) req.level = level;
    if (att) {
      if (att.ReqStr > 0) req.str = att.ReqStr;
      if (att.ReqDex > 0) req.dex = att.ReqDex;
      if (att.ReqInt > 0) req.int = att.ReqInt;
    }
    o.req = req;
    bases[name] = o;
  }
  if (skippedNoFile) console.log(`  ${skippedNoFile} bases without an .it file: no inherited tags or quality`);
  return { 'bases.json': bases };
}
