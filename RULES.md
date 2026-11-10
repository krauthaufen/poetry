# PoEsie rules ledger

Every mechanic the engine implements, where the formula came from, and how much we trust it.
Mechanics are only as good as this file. When an in-game test confirms or refutes one, update it here and add a test.

Confidence levels:
- **data**: a number read from the game's own data files (via PoB's generated `Data/Misc.lua`, see `tools/gen_tables.py`).
- **pob**: formula taken from PoB-PoE2 (`vendor/PathOfBuilding-PoE2`, release 0.23.1), not independently checked.
- **guess**: our assumption, no good source yet. Needs an in-game test.
- **pob-wrong**: PoB does something we believe is wrong for PoE2. We do it differently, and say why.

## Hit damage

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Base damage | (skill/weapon base + added × added-damage-effectiveness × `AddedDamageEffect` mods) × base multiplier | CalcOffence.lua ~3940 | pob |
| Conversion order | Physical → Lightning → Cold → Fire → Chaos, never backwards | CalcOffence.lua `dmgTypeList` | pob |
| Conversion | per source type: skill conversion first (scaled to ≤100%), then global conversion on the unconverted rest (scaled to ≤100%) | CalcOffence.lua ~2256 | pob |
| Conversion chains | damage that arrives at a type can convert again further down the order | PoB handles one extra step; we chain fully | guess |
| Converted damage scaling | uses only the modifiers of its final type, none from the source type | CalcOffence.lua `calcDamage` sums converted base into the destination | pob |
| Gain as extra | taken from post-conversion damage of the source type; gained damage doesn't convert | CalcOffence.lua `calcGainedDamage` | pob |
| Damage roll | each type rolled independently and uniformly in [min, max] | — | guess |
| Lucky damage | roll twice, take the higher (per type) | PoB uses an expected value of (min + 2·max)/3, the same distribution | pob |
| Increased / more | (1 + Σinc/100) × Π(1 + more/100), scopes: all, elemental, the type itself | CalcOffence.lua `calcDamage` | pob |
| Crit chance | (base + flat) × inc × more, capped 0–100 | CalcOffence.lua ~3713 | pob |
| Crit damage | × (1 + (100 + flat bonus) × inc × more / 100); player base bonus 100% | Character.ot `base_critical_hit_damage_bonus` | data |
| Double damage | chance per hit to deal ×2 | — | guess |

## Mitigation

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Resist layers | Σ resist mods (base, exposure, curses all as flat resist on the enemy), capped at max resist (75 + mods, ≤90), floor −200 | CalcOffence.lua `calcResistForType` | pob |
| **Order of exposure / curses / penetration** | exposure and curses lower resist first, then penetration applies | PoB does the same | guess, **needs an in-game test** |
| Penetration | lowers resist, but never below 0; does nothing to negative resist | CalcOffence.lua `calcPenResist` (minPen = 0) | pob |
| Armour | reduction = A / (A + 10 × damage), using that hit's physical damage, plus flat physical reduction, capped at 75% for monsters | CalcDefence.lua `armourReductionF`; Monster.ot cap | pob + data |
| Damage taken | (1 + Σinc taken) × Π more taken; scopes as for damage | CalcOffence.lua ~4060 | pob |
| Hit chance | 100% unless a `HitChance` override is given. Accuracy vs evasion is not modelled yet. | — | guess |

## Ailments

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Ailment base damage | from the hit's **unmitigated** damage (after crit and attacker mods, before enemy mitigation) | CalcOffence.lua "Store pre-resist/armour/penetration hit damage for ailment calculations" | pob |
| Ignite | 20% of the fire part of the hit per second, 4 s | GameConstants `IgniteHitDamagePercentPerMinute`=1200, `BaseIgniteDuration`=4 | data |
| Bleed | 15% of the physical part per second, 5 s, ×2 while the target moves | GameConstants 900/min, 5 s, `BloodstainedMultiplier...`=2 | data |
| Poison | 20% of the physical + chaos part per second, 2 s | GameConstants 1200/min, 2 s | data |
| Stacking | default 1 instance of each damaging ailment counts (the strongest); `AilmentStacks` raises that | PoB `maxStacks = 1` unless `CanStack` | pob |
| Ailment magnitude | × inc × more of `AilmentMagnitude` (this ailment or all) | PoB `AilmentMagnitude` | pob |
| DoT mitigation | enemy resist of the dealt type (no penetration), damage taken mods, `DotTaken`. Bleeding ignores armour. Recomputed continuously, so a curse applied later still counts. | CalcOffence.lua ~5180 | pob |
| Ignite / shock chance | (dmg / ailment threshold × 20 (ignite) or 25 (shock) + flat chance) × inc × more, capped 100 | CalcOffence.lua ~5457; GameConstants | pob + data |
| Ailment threshold | monster table by level × `AilmentThreshold` mods | Misc.lua `monsterAilmentThresholdTable` | data |
| Shock | 20% increased damage taken, strongest applies, doesn't stack | GameConstants `BaseShockMagnitude`=20 | data |
| Shock duration | `BaseShockDuration` = 8 | GameConstants | data, **but the commonly quoted value is 4 s — check** |
| Chill | magnitude = 100 × cold / threshold × mods, capped 50%; applied when ≥1% | CalcOffence.lua ~5386 | pob |
| PoB's shock formula | PoB still has the PoE1 `50 × (dmg/threshold)^0.4` shock effect | CalcOffence.lua ~5515 | pob-wrong, we use the flat 20% |
| Freeze / electrocute / heavy stun / pin buildup | per hit: dmg × scale / poise threshold × `Buildup` mods × 100% of the bar; at 100% the status applies and the bar resets | CalcOffence.lua ~5405; GameConstants `*DamageScale` | pob + data |
| Buildup sources | freeze: cold; electrocute: lightning, only with `Named "CanElectrocute"`; heavy stun: all damage; pin: physical, only with `Named "CanPin"` | PoB `buildupTypes` | guess for heavy stun (no melee/physical bonus yet) |
| Buildup decay | none: the bar never drains | — | guess, **known to be wrong**, needs data |
| Repeat-freeze threshold growth (`FreezeModifierDuration` etc.) | not modelled | — | missing |
| Statuses after buildup | freeze 4 s, electrocute 5 s, pin 3 s, heavy stun 3 s | GameConstants; heavy stun is a placeholder | data / guess |
| Daze, maim, blind, hinder | flat chance × inc × more, 4 s | — | guess (duration) |

## Timing and the event engine

| Mechanic | Rule | Confidence |
|---|---|---|
| Use time | base time / (inc × more `Speed`) | pob |
| Cooldown | cooldown / (inc × more `CooldownRecovery`), starts when the use starts | guess |
| Hits | one hit at the end of the use, unless the skill or actor gives a hit pattern | modelling choice |
| Triggered skills | use immediately, cost no time, respect their cooldown | guess per trigger |
| Meta gems | energy from rules; at max energy, trigger and subtract max | modelling choice; energy amounts per hook must come from gem data |
| Server ticks | not quantised: actions happen at exact times | guess, PoE uses 33 ms ticks |
| DoT damage | integrated exactly between events (DoT dps is constant between events) | exact |
| Resources (mana, life costs) | not modelled | missing |

## Gems and items

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Gem stat values | interpolation 1: value as listed per level; 3: round(effectiveness × value), effectiveness = 3.885209 × baseEffectiveness × (1 + incrementalEffectiveness × (actorLevel − 1)) × (1 + damageIncrementalEffectiveness)^(actorLevel − 1) | PoB CalcTools.buildSkillInstanceStats; GameConstants `SkillDamageBaseEffectiveness` | pob + data |
| Interpolation 2 | not implemented (PoB itself says it is guessing) | — | missing |
| Gem quality | stat += truncate(per-quality value × quality) | PoB CalcTools | pob |
| +N gem levels | `GemLevel` mods matching the skill raise the level, capped at the gem's max | — | guess (cap) |
| Attack time | 1 / (weapon APS × (1 + attackSpeedMultiplier / 100)), or the gem's fixed attackTime | skill level data | data |
| Spell added damage | full effectiveness (1.0) | PoB `AddedDamage` multiplier only | pob |
| Weapon local damage | (base + local added) × (1 + (local increased physical + quality) / 100) for physical; other types just added | — | guess (rounding not applied) |
| Weapon local crit | base + local flat (stored in hundredths), × local increased | — | guess |
| Hits per use | 1 unless configured (`--hits`); how many projectiles hit one target is per-skill behaviour, not modelled yet | — | missing |

## Encounters and geometry

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Positions | 2D, game units (10 = 1 m), player at origin, enemies static | modelling choice | — |
| Rarity profiles | life ×2.48 / ×8 / ×29 (magic / rare / unique), poise +26% / +125% / +500%, curse and exposure effect −15% / −30% / −50%, rares 10% larger | game Mods.dat `MonsterMagicN` / `MonsterRareN` / `MonsterUniqueN`, generated into RarityData.fs | data |
| Monster resistances | rarity gives none; 0 for packs, 30 for a boss, 50 pinnacle, all overridable | boss values: PoB's boss config | pob |
| Ailment threshold by rarity | unchanged by rarity | no rarity mod touches it | data (absence) |
| Monster energy shield | ignored (rarity mods scale ES, but base monster ES is not modelled) | — | missing |
| Area radius | floor(base × floor(100 × sqrt(area multiplier)) / 100); hits enemies whose body overlaps | PoB calcRadius | pob |
| Projectiles | fanned evenly over the spread angle towards the aimed enemy; first enemy along the path is hit; pierce continues, chains jump to the nearest unhit enemy in chain range | — | guess (geometry), data (counts) |
| Default spread | 30° unless the skill sets `total_projectile_spread_angle_override` | — | guess |
| Shotgun | one use's projectiles hit each enemy once unless the skill has `projectiles_can_shotgun` | inferred from the flag existing on only a few skills | guess |
| Damage per target | rolled independently for every enemy hit | — | guess |
| Per-skill motion (Spark wandering, Ice Shot shards from the impact point, Frost Bomb pulses) | not modelled yet: needs per-skill behaviours | — | missing |
| Resistances "treated as negated" | positive elemental resist counts as its negative, rolled per hit for a chance | game text | guess (interaction with penetration/exposure order) |
| Weapon set | set one is assumed active ("in weapon set two" mods are off) | modelling choice | — |
| "Recently" | hit / crit / kill recently = within 4 s, plus "crit in the past 8 s"; tracked by the engine from actual events | PoE convention (4 s) | pob |
