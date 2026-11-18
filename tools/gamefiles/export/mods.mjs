// mods.json: every modifier an item can have, with its text lines: "<file>:<mod id>" -> { type, group, level,
// affix, lines }. The files are PoB's (Export/Scripts/mods.lua): ModItem (prefixes and suffixes),
// ModJewel, ModCorrupted, ModItemExclusive (implicits and unique modifiers), ModRunes (names only).
import { describeMod, modStats, unknownHandlers } from './describe.mjs';

const Domain = { GenericMod: 1, FlaskCharm: 2, Jewel: 11, UniqueJewel: 22 };
const Gen = { Prefix: 1, Suffix: 2, Intrinsic: 3, Corruption: 5 };

export function run(game, ctx = {}) {
  const MODS = game.table('Mods'), FAMILY = game.table('ModFamily'), MODTYPE = game.table('ModType');
  const firstFamily = m => (m.Families.length ? FAMILY[m.Families[0]].Id : null);
  const files = {
    ModItem: m => m.Domain === Domain.GenericMod && (m.GenerationType === Gen.Prefix || m.GenerationType === Gen.Suffix)
      && firstFamily(m) !== 'AuraBonus' && !/Cowards/.test(m.Id) && !/Master/.test(m.Id),
    ModJewel: m => m.Domain === Domain.Jewel && (m.GenerationType === Gen.Prefix || m.GenerationType === Gen.Suffix),
    ModCorrupted: m => (m.Domain === Domain.Jewel || m.Domain === Domain.GenericMod)
      && ((m.GenerationType === Gen.Intrinsic && /SpecialCorruption/.test(m.Id)) || m.GenerationType === Gen.Corruption),
    ModItemExclusive: m => (m.Domain === Domain.GenericMod || m.Domain === Domain.FlaskCharm || m.Domain === Domain.Jewel || m.Domain === Domain.UniqueJewel)
      && m.GenerationType === Gen.Intrinsic && firstFamily(m) !== 'AuraBonus'
      && !/^Synthesis/.test(m.Id) && !/Royale|Cowards|Map|Ultimatum|SpecialCorruption/.test(m.Id),
  };
  const out = {};
  const described = new Map();
  for (const [file, cond] of Object.entries(files)) {
    for (const mod of MODS) {
      if (!cond(mod)) continue;
      if (!described.has(mod._index)) described.set(mod._index, describeMod(game, mod));
      const d = described.get(mod._index);
      if (d.orders.length === 0) continue;
      const lines = [...d.lines];
      // a "nearby enemies" display line is dropped (mods.lua)
      for (let k = 0; k < mod.Families.length; k++) {
        if (FAMILY[mod.Families[k]].Id.includes('LocalDisplayNearbyEnemy') && lines.length > k + 1) { lines.splice(k, 1); break; }
      }
      const e = { affix: mod.Name, group: MODTYPE[mod.ModType].Name, level: mod.Level, lines };
      if (mod.GenerationType === Gen.Prefix) e.type = 'Prefix';
      else if (mod.GenerationType === Gen.Suffix) e.type = 'Suffix';
      else if (mod.GenerationType === Gen.Intrinsic) { if (/SpecialCorruption/.test(mod.Id)) e.type = 'SpecialCorrupted'; }
      else if (mod.GenerationType === Gen.Corruption) e.type = 'Corrupted';
      out[`${file}:${mod.Id}`] = e;
      if (ctx.use) for (const id of modStats(game, mod).keys()) ctx.use('mods', id, mod.Id);
    }
  }
  // runes and soul cores: PoB's file has their lines per item slot, which the simulator's export drops; the names remain
  const BASE = game.table('BaseItemTypes');
  for (const sc of game.table('SoulCores')) {
    const b = BASE[sc.BaseItemType];
    if (!b || /DNT/.test(b.Name)) continue;
    out['ModRunes:' + b.Name.replace(/ö/g, 'o').trim()] = { lines: [] };
  }
  if (unknownHandlers.size) console.log('  stat description handlers not known to describe.mjs: ' + [...unknownHandlers].join(', '));
  return { 'mods.json': out };
}
