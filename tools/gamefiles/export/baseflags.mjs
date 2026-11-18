// A stat set's baseFlags. PoB writes them by hand ("#flags spell area projectile" in Export/Skills/*.txt);
// here they come from the skill's types and the set's stats. They agree with PoB's for the common flags
// (see the compare tool); PoB's "hit", "triggerable", "buff", "thorns" and a few one-off flags have no
// counterpart in the data and are not written. The rules are the ones that agree best with PoB's flags on
// the player skills both have (area and projectile by the set's stats, the others by skill type).
const BY_TYPE = {
  attack: ['Attack'], spell: ['Spell'], melee: ['Melee'], duration: ['Duration'],
  minion: ['Minion', 'CreatesMinion'], totem: ['SummonsTotem', 'UsedByTotem'], chaining: ['Chains'], warcry: ['Warcry'],
  curse: ['AppliesCurse'], aura: ['Aura'], nonWeaponAttack: ['UseGlobalStats'],
};
const BY_STAT = {
  area: ['is_area_damage'], projectile: ['base_is_projectile'], shieldAttack: ['replace_off_hand_unarmed_attack_stats_with_shield_type'],
};
export function deriveBaseFlags(game, granted, active, set, skill) {
  const types = skill.skillTypes || {};
  const stats = new Set([...(set.stats || []), ...(set.constantStats || []).map(c => c[0])]);
  const flags = {};
  for (const [flag, list] of Object.entries(BY_TYPE)) if (list.some(t => types[t])) flags[flag] = true;
  for (const [flag, list] of Object.entries(BY_STAT)) if (list.some(s => stats.has(s))) flags[flag] = true;
  return flags;
}
