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
| Gain as extra | taken from the **base** damage of the source type; conversion then applies to base and gained damage alike (so a fully converted skill stays fully converted) | user's in-game damage split (Falling Thunder is "almost only elemental" with 28% gain as extra physical); PoB does the opposite (gain after conversion) | user observation |
| Damage roll | each type rolled independently and uniformly in [min, max] | — | guess |
| Lucky damage | roll twice, take the higher (per type) | PoB uses an expected value of (min + 2·max)/3, the same distribution | pob |
| Increased / more | (1 + Σinc/100) × Π(1 + more/100), scopes: all, elemental, the type itself | CalcOffence.lua `calcDamage` | pob |
| Crit chance | (base + flat) × inc × more, capped 0–100 | CalcOffence.lua ~3713 | pob |
| Crit damage | × (1 + (100 + flat bonus) × inc × more / 100); player base bonus 100% | Character.ot `base_critical_hit_damage_bonus` | data |
| Double damage | chance per hit to deal ×2 | — | guess |

### Accuracy

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Who rolls | attacks roll accuracy against the monster's evasion; spells always hit; "always hit" overrides | game rule | data |
| Player accuracy | 6 per level above 1 + 6 per Dexterity + flat, x increased / more | Character.ot, PoB AccuracyPerDexBase | pob |
| Hit chance | round(1.25 A / (A + 0.3 E)), between 5% and 100% | PoB calcs.hitChance | pob |
| Distance | accuracy is full up to 2 m and falls linearly to 90% less at 9 m | Character.ot accuracy_rating_+%_final_at_max_distance_scaled, PoB falloff | pob |
| Monster evasion | DefaultMonsterStats evasion for its level | game data | data |

## Mitigation

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Resist layers | Σ resist mods (base, exposure, curses all as flat resist on the enemy), capped at max resist (75 + mods, ≤90), floor −200 | CalcOffence.lua `calcResistForType` | pob |
| **Order of exposure / curses / penetration** | exposure and curses lower resist first, then penetration applies | PoB does the same | guess, **needs an in-game test** |
| Penetration | lowers resist, but never below 0 (or below the "can penetrate down to a minimum of X" floor); does nothing to resist already at or below the floor | CalcOffence.lua `calcPenResist` (minPen = 0); floor: game text | pob + data |
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
| Shock | 20% increased damage taken, strongest applies, doesn't stack; a shock a hit applies already counts for that hit | GameConstants `BaseShockMagnitude`=20; same-hit: user | data + user |
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
| Several projectiles, one enemy | every projectile of a use may hit the same enemy; each projectile hits an enemy at most once (pierce continues, it never re-hits). How many reach the enemy is pure geometry | user: "some of them may hit the boss, it's geometric" | guess (geometry) |
| Damage per target | rolled independently for every enemy hit | — | guess |
| Per-skill motion (Spark wandering, Ice Shot shards from the impact point, Frost Bomb pulses) | not modelled yet: needs per-skill behaviours | — | missing |
| Resistances "treated as negated" | positive elemental resist counts as its negative, rolled per hit for a chance | game text | guess (interaction with penetration/exposure order) |
| Weapon set | set one is assumed active ("in weapon set two" mods are off) | modelling choice | — |
| "Recently" | hit / crit / kill recently = within 4 s, plus "crit in the past 8 s"; tracked by the engine from actual events | PoE convention (4 s) | pob |
| Alternate quality | with `ascendancy_gemling_enable_thaumaturgy_quality_stats` (Gemling Advanced Thaumaturgy) a gem's `altQualityStats` apply too, per quality like normal quality; supports get no quality | PoB GemTooltip `GemlingQuality` flag; skill data | pob + data |
| Chance to not remove charges | summed over character, skill and alternate quality; at ≥100% charges are never removed; "still count as consuming" starts the consumed-recently status either way | game text | guess (cap: Flicker Strike's equivalent is hard-capped at 75%) |
| "Bonded:" lines | apply for Shamans. Otherwise only an **idol's** bonded lines, and only in an item that has "Idols socketed in this item gain the benefits of their Bonded modifiers" (itself an idol's "Body Armours:" line, so it only exists on a body armour or an item that gains bonuses from socketed items as though it were one, e.g. Fox Idol in Darkness Enthroned). Bonded lines from runes never apply. API import attributes each host line to its socketed source via `bondedMods`; PoB text has no socket info and uses the item-level rule | game text; confirmed in game (46% gem quality) | data |
| Projectiles of `ProjectilesNotFromUser` skills (Falling Thunder) | start at the impact point on the aimed enemy; a projectile starting inside an enemy hits it at once; "forwards" = away from the player; Nova = full circle | skill text, skill type | guess (geometry) |
| Increased area of effect | `<scope> area of effect +% [final]` scales radii by the square root, PoB's calcRadius | — | pob |
| Default gem quality | 20% unless the import states one; supports have none | user convention | — |
| Persistent buffs | their gem stats apply to the character. Stats named after the skill apply only if the game's description presents them as granted ("Buff grants 46% increased Fire damage", "Grants ...", "... you ...", from the stat description files); otherwise they belong to the skill itself | stat descriptions | data |
| Slam impact point | `Reach` units in front of the player towards the aimed enemy, at most its centre | setting (default 15) | guess |
| Walls and terrain chain | projectiles stop at walls; with "chance to chain 1 extra time from terrain" (Ricochet) they chain once from the wall: towards the nearest enemy within chain range that this projectile hasn't hit, with the remaining range. No target in range: mirror bounce | game text ("chain" = targeted) | guess (no-target fallback, chain range 3 m) |
| Arenas | straight walls only: none, one wall, or two parallel walls (a bridge), at a random angle, with player and monsters on the walkable side | user ("never that many walls around") | user |

## The player: pools, costs, incoming damage

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Attributes | class base (tree data) + attribute mods; the API's `skill_overrides` give the chosen "+5 to any Attribute" nodes; quest rewards from `quest_stats` | data | data |
| Life | (16 + 12 × level + 2 × Str + flat) × inc × more | PoB CalcSetup / CalcPerform; Character.ot life_per_level | pob + data |
| Mana | (30 + 4 × level + 2 × Int + flat) × inc × more | same | pob + data |
| Armour, evasion, energy shield | items' displayed values (local mods and quality included) + flat, × global inc × more | API / PoB item text | data |
| Deflection | % of evasion (`base_deflection_rating_%_of_evasion_rating`) + flat | game text | data |
| Resistances | sum, capped at 75 + max resist mods (≤ 90); no penalty | — | guess (no campaign penalty) |
| Mana regeneration | 4% of max mana per second × inc | Character.ot 240 per minute | data |
| Energy shield recharge | 12.5% per second after 4 s without being hit | Character.ot 750 per minute; PoB delay 4 s | data + pob |
| Skill cost | gem level cost × support cost multipliers (`manaMultiplier`, percent more) × increased cost / (1 + cost efficiency) | skill data; PoB SupportManaMultiplier | data (efficiency formula: guess) |
| Not enough mana | the use waits until regeneration and leech cover it | — | modelling choice |
| Mana leech | % of physical attack damage dealt, each hit counted as at most 40,000 damage, from **one enemy per hit event** (the largest instance), recovered at most 20% of max mana per second | GameConstants EffectiveMaxDamageForLeech; one enemy and rate cap: user | data + user |
| Mana on kill | flat and % of max mana per kill, also for damage-over-time kills? no: hit kills only | game text | guess (DoT kills) |
| Monster hits | 1.5 × level damage × (1/4.4 normal/magic/rare, 4/4.4 unique) × rarity damage modifier, every 1.5 s, 40% physical / 20% each element | PoB enemy presets; rarity mod from Mods.dat | pob heuristic + guess |
| Monster movement | walk towards the player at 3.5 m/s until within 0.8 m; frozen / stunned / electrocuted / pinned don't move; maimed and hindered 30% slower; chilled slower by its magnitude | maim −30% from BuffTemplates; hinder, speed, range: guesses | guess |
| Monster hit chance | 1 − 0.95 E / (E + 4 Acc); blinded monsters have 20% less accuracy | PoB monsterHitChance; blind value a guess | pob + guess |
| Deflection | P(not deflected) = Acc / (Acc + 0.12 D) × 1.5 − 0.5, capped 95%; deflected hits deal 40% less | PoB deflectChance; GameConstants | pob + data |
| Mitigation | armour A / (A + 10 D) capped 90% for physical, resistances for the rest; "damage removed from mana first", then energy shield, then life | PoB; game text | pob |
| Frozen / stunned / electrocuted monsters | don't attack | buff data `stopped_by_*` | data |
| Chill | monsters attack slower by the chill magnitude | buff data action_speed_+%_final_from_chill | data |
| Mana flask | values as the item shows them (amount over duration, charges per use / max); starts full; gains charges per second (`generate_x_charges_for_any_flask_per_minute`) and per kill (1 per normal monster × rarity `monster_base_flask_charges_percent_of_monster_power`), both × "increased charges gained"; drunk at no time cost when mana < 2 × the next cost and it isn't running | item / data; base charges per kill and drink policy: guesses | data + guess |
| Item defences | armour pieces and weapons are read in local context (their "increased" lines are local and already in the shown values, runes included); jewellery, jewels, tree and quest rewards in global context | game text is identical for local and global variants | data |
| Body armour scoped | "increased Evasion / Energy Shield from Equipped Body Armour" scale that item's values; "+1 ES per X Item Evasion on Equipped Body Armour" reads the scaled body evasion | game text | guess (order) |
| Keystones | handled by name: Chaos Inoculation = maximum life 1, chaos immunity, no life regeneration | game text | data |
| Not modelled | life flasks and charms, life leech, block, evade-based crit, monster skills and phases, ranged monsters, other keystones | — | missing |

### Incoming damage in detail

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Order | energy shield, then the "taken from mana before life" share of the rest, life down to 1, Runic Ward, death | game text, user (ward) | data + user |
| Chaos vs energy shield | chaos damage removes twice as much energy shield | PoE2 rule (recollection) | guess, **check** |
| Bypass | "X% of damage bypasses energy shield" skips it | stat text | data |
| Armour vs elemental | "X% of armour applies to <type> damage": armour formula with that share, after resistance | stat text; order a guess | guess |
| Damage taken | increased / reduced damage taken by type multiplies after mitigation; flat physical reduction applies to melee hits before armour | stat text | guess (order) |
| Monster crits | 5% chance, +30% damage (monster base_critical_hit_damage_bonus), less with "enemies deal less extra crit damage" | game data; 5% PoB default | data |
| Recoup | share of damage taken (by type) returns to the pool evenly over 4 s; "recoup speed" shortens it | stat text | data |
| ES recharge | starts 4 s after the last damage taken (hits and damage over time), / (1 + faster start); 12.5% per second x recharge rate | PoB, Character.ot | pob |
| Life flask | drunk when life falls below half, if charges allow; amount x life recovery rate over its duration | player behaviour | guess |
| Leech (life) | % of physical attack damage, from one enemy per hit event, recovered at most 20% of maximum per second | as mana leech | guess |
| Ailments on the player | threshold = half of maximum life (PlayerAilmentThresholdLifeFactor) + mods. Ignite: chance = fire / threshold x IgniteChanceMultiplier, burns 4 s, strongest only, lowered by fire resistance. Shock: chance likewise, +20% damage taken for 4 s. Chill: slow = ChillEffectMultiplier x cold / threshold for 2 s. Freeze: builds cold x FreezeDamageScalePlayer / freeze threshold, frozen 2 s at 100%. "Avoid", "effect on you" and duration stats apply. Unmitigated hit damage is used. Bleed / poison from monsters: not modelled | GameConstants "...Player" | data; using unmitigated damage is a guess |
| Light stun on the player | chance = hit / stun threshold (maximum life before CI + mods); physical +100%, melee +33% (monster object stats); nothing below 15% (LightStunMinimumChancePlayer); interrupts for 0.35 s (GUESS); a stun only delays the next action, it does not cancel one under way. Heavy stun on the player: not modelled | game constants; formula a guess | guess |
| Evasion | every monster hit can be evaded | PoE2 0.3 (recollection) | guess |

### Keystones

| Keystone | Rule | Confidence |
|---|---|---|
| Chaos Inoculation, Pyromantic Pact | by name (see above) | data |
| Pain Attunement | 30% less crit bonus on full life, 30% more on low life (35% or less). With Chaos Inoculation you are always on full life | data; CI = full life is a PoE convention |
| Zealot's Oath | life regeneration goes to energy shield; no ES recharge | data |
| Iron Reflexes | all evasion becomes armour | data |
| Avatar of Fire | 75% of every non-fire type converted to fire; the rest deals nothing | data |
| Glancing Blows | evade unlucky (two rolls must both evade), deflect lucky | data |
| Oasis | 30% more flask recovery (charms are not simulated anyway) | data |
| Eternal Youth | life recharges like energy shield would; 50% less life flask recovery | data |
| Mind Over Matter | all damage from mana before life; 50% less mana recovery | data |
| Blood Magic | no mana; skills cost life (a use that would kill waits) | data |
| Eldritch Battery | energy shield becomes mana (after its own increases); costs doubled | data; which increases apply is a guess |
| Unwavering Stance | cannot be light stunned | data |
| Resolute Technique | never crit; accuracy doubled (accuracy not simulated yet) | data |
| Dance with Death | 25% more skill speed with a one-handed weapon and empty off hand | data |
| Blackflame Covenant | fire spells convert fire to chaos; ignite stays fire (NOT yet chaos) | partial |
| Crimson Assault | bleeding is aggravated (as if the enemy moved), base duration 1 s, 50% more magnitude | data |
| Vaal Pact | 50% more life leeched, 67% slower, no other life recovery | data |
| Giant's Blood | life from Strength halved (the weapons are already equipped) | data |
| Elemental Equilibrium | infusion remnants rotate: fire -> lightning -> cold -> fire | data |
| Wildsurge Incantation | Storm and Plant spells: 50% more damage, 50% less cost, 75% less duration | data |
| Whispers of Doom | +1 curse (curses themselves not simulated yet) | partial |
| Ritual Cadence, Resonance, Heartstopper, Primal Hunger, Scarred Faith, Hollow Palm Technique, Conduit, Necromantic Talisman, Trusted Kinship, Ancestral Bond, Lord of the Wilds, Bulwark | NOT modelled | todo |

## Resources over time

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Charges, rage, motes | stacks on the player with a maximum each (charges: 3 + max charge stats; rage 30 + max rage; motes below) | data | data |
| Charge duration | 15 s × (1 + increased charge duration); all charges of a kind share one timer, gaining one (also at maximum) refreshes it, and they expire together. Removing a charge doesn't refresh. The user's power charges: 15 × 2.2 = 33 s, matching the game | PoB CalcSetup base 15 s; user (33 s, expiry behaviour) | pob + user |
| Start of a fight | everything at its maximum (steady state mid-map); `--start empty` starts frenzy / endurance / motes at 0. Changing areas resets charges to 0 in game, so a map starts empty and the charge-building phase is part of every map | user | — |
| Kept charges and their timer | a consume that keeps the charges ("chance to not remove") freezes their timer at its current value (it does not restart it); the user sees the timer stand still while slamming continuously, so the pause is at least the time between slams; default 1 s (any value above ~0.6 s gives the same results for this build), `--kept-pause S` | user observation | user |
| Power charge expiry rate | the model gives ~8.5 per hour of continuous Falling Thunder (weighted generator, 33 s duration); the user can't measure it (area changes reset charges) but rarely notices it | model; user | unverified |
| "Consumes one of each Charge every X s" | `consume_frenzy_power_and_endurance_charge_every_x_ms` (with quality): removes one of each charge the player has; each removal rolls the character's "chance to not remove charges" and counts as consuming either way | skill text | data + guess (skip chance applies) |
| Thaumaturgical Dynamism | a charge every 5 s, its type weighted by the total attribute requirements of socketed skill gems (Str → endurance, Dex → frenzy, Int → power); at maximum it only refreshes the duration. Gem requirement at its level: round((5 + (level − 3) × 1.7) × (share/100)^0.9) + 4, 0 below 8 and for supports | vaal.tools, poe2vault (weighting, 5 s); PoB getGemStatRequirement (formula) | node text; the game derives the interval from socketed gem attribute requirements (formula unknown), 5 s from the user | user |
| "Gained / Consumed a <charge> Recently" | 4 s after each gain or consume | PoE convention | pob |
| Count as consuming N additional | a skill that consumes charges sees N extra stacks for "per charge consumed" effects (damage, projectile count) | game text | data |
| Charge-conditional defences | "more Armour, Evasion and Energy Shield while you have an Endurance Charge" etc. are evaluated from the current charges at every monster hit (pools: at the start) | game text | data |
| Virtuous Barrier motes (observed) | user's character: Strength 9, Dexterity 8, Intelligence 12, as the formula gives (in-game advanced info) | user | confirmed |
| Virtuous Barrier motes | a random colour not at maximum every `base_active_skill_buff_stack_gain_frequency_ms`; maximum per colour = the description's base (`skill_specific_stat_description_mode`, 3) + active skills requiring that attribute, single-attribute skills counting twice; `gem_barrier_<colour>_grants_<stat>` is per mote | skill text and data; supports not counted as skills | data + guess |
| Gem levels by attribute | `<attribute>_skill_gem_level_+` applies to gems with that attribute requirement | gem data | data |
| Observed per-skill behaviour | `Behaviours.observed`: facts seen in game that the data lacks. Falling Thunder: projectiles appear ~0.5 m (5 units) out from the impact, radially | user | user |

## Meta gems and triggers

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Meta gems | every enabled group whose active gem is typed Meta; the group's other skill gems are triggered (copies with that group's supports, tagged Triggered); the group's supports never reach the character | data | data |
| Energy sources | from the gem's stats: `*_centienergy_per_monster_power_on_crit / freeze / shock / ignite`, `*_centienergy_on_block`, `*_centienergy_per_10ms_base_cast_time` (spells the player casts) | data | data |
| No energy from triggered skills | hits and ailments of triggered skills (and spells socketed in meta gems) give no energy, so triggers can't feed themselves | poe2wiki / poe2db: "Energy cannot be gained from direct effects of Triggered Skills" | community |
| Energy per event | per-power value × monster power × (damage of the relevant types / enemy ailment threshold) × (1 + energy generated: gem + group supports) | PoB developer draft (GitHub issue #181); not confirmed game logic | community |
| Monster power | normal 1, magic 2, rare 5, unique / boss 20 | Mobalytics; boss 20 agreed, magic / rare disputed | community |
| Energy per trigger | 100 per second of base cast time of the socketed spells (cast time + `total_cast_time_+_ms`); Comet: 1 + 1 s = 200 | "10 per 0.1 s" (community); a guide quotes 300 for Comet (older version?) | community |
| Release | "Cast on ...": trigger all socketed spells on reaching maximum (= the cost), energy spent. Invocations (Elemental Invocation, Spellslinger): bank up to `generic_ongoing_trigger_maximum_energy`; used as soon as a trigger is covered and its cooldown (0.2 s) is ready, triggering as many times as the energy covers | gem descriptions | data |
| Trigger cost of fixed added cast time | `total_cast_time_+_ms` counts double: Comet (1 s base, +1 s fixed) costs 300 energy, not 200 | forum bug reports 3623092, 3860952; user | observed, maybe a bug GGG fixes |
| Infusions | fire / cold / lightning / verisium stacks, max 3 each (BaseMaximumInfusionCount + "maximum X infusion stacks"), 20 s (BaseInfusionDurationMs); gaining refreshes the duration | game constants | data |
| Infusion use | a skill with `active_skill_consumes_a_X_infusion` uses its "-Infused" stat set when it consumes one; every impact consumes one (Spell Cascade: each impact, centre first; the centre's radius sets the spacing); missing element -> a Verisium one; none -> plain stat set; `chance_to_not_consume_infusion_%` keeps it | user (cascades eat separately, Verisium as any element) | user |
| Infusion remnants | Siphon Elements: each ignite / shock inflicted, and each new freeze, has the gem's chance (5% + 0.1%/quality) to drop a remnant; Harmonic Remnants: chance for two; picked up after `--pickup` seconds | gem data; pickup delay a guess | data + guess |
| Powered by Verisium | grants 4 Verisium infusions per cast, capped at 3 (one wasted); Ward cost not deducted (TODO) | gem data, user | data |
| Fights start with infusions | every consumed element and Verisium start at maximum x `--banked` (picked up while clearing) | user | user |
| Infusion-generating skills | a non-persistent skill with types GeneratesInfusion + GeneratesRemnants and one element (Frost Bomb) drops a remnant of that element per use, at detonation (`base_skill_detonation_time` x Short Fuse) + pickup; Harmonic Remnants: chance for two | gem data + description, user | data |
| Frost Bomb exposure | `active_skill_all_elemental_exposure_magnitude` to fire, cold and lightning on use, x (1 + exposure effect), for `base_secondary_skill_effect_duration`; compounding per pulse ignored (Short Fuse orb lives 0.8 s) | gem data | data |
| Atziri's Impatience | "Lose 25% of maximum Life, Mana and Energy Shield when you use Supported Skills": applied on use, life floors at 1 | stat description | data |
| Stored cooldown uses | gem level `storedUses`, doubled by Second Wind; each use recharges independently ("just two cooldowns"); `--sequential-uses` for one after another | user (poedb describes sequential) | user |
| Verisium in the rotation | cast when ready, but only while Verisium infusions are below maximum | player behaviour | guess |
| Charge buffs (Charged Staff) | on use: one stack per charge held (Heightened Charges: chance for twice as many), each adding the gem's per-stack added damage to quarterstaff attacks and per-stack duration x skill duration; a new cast replaces the buff; charges consumed as usual (skip chance applies). `--buff` casts it when the buff is down and charges are full; fights that start prepared start with it up | gem data, user (replaces, duration per charge, 100% not consumed) | data + user |
| Second Wind | `base_cooldown_speed_+%_final` = less cooldown recovery; Verisium 4.9 s -> 5.4 s, as the game shows | gem data, in-game value | data |
| Banked invocations | fights start with invocations at maximum energy (banked while clearing); `--banked 0` starts empty | user (presses both invocations when ready) | user |
| Stat text with only a literal | a template with no placeholder but one number ("down to a minimum of -50%") gives that number as the stat value | game text | data |
| Energy Retention | chance to get half the spent energy back | gem text | data |
| Triggered spells | no mana cost, no time spent; hits after `*_fall_time_ms` (Comet 0.55 s) | data | data |
| Spell Cascade | `number_of_cascades_per_side` extra impacts on either side of the target along a randomly chosen axis; side impacts have their own less area (`support_spell_cascade_area_of_effect_+%_for_cascade`); spacing between impacts = 1.5 × the centre radius (`--cascade-spacing`) | data; random axis: gem descriptions (outof.games, Game8); spacing: user observation | data + user |
| Repeated stats | a stat listed more than once in a stat set adds up (infused sets add a second radius and damage entry: Cold-Infused Ice Nova radius 32 + 16) | data | data |
| Elemental Archon | BuffTemplates ArchonElemental: 25% more elemental damage with spells, 100% more ignite chance / freeze buildup / shock chance with spells; "Archon Buffs also grant ..." mods; all × increased Archon effect. Gained on an ignite by a skill with `gain_archon_elemental_when_you_ignite_enemy_chance_%` (every ignite counts); 10 s × increased duration; 20 s recovery ÷ "expires faster", none with `archon_no_recovery_period` (Dominion anoint: no recovery, −50% effect) | data; durations: poe2db | data + community |
| Infused stat sets | "X-Infused" sets are alternatives used when an infusion is consumed, never parts of a normal use (infusions not modelled yet) | data | data |
| Block | shield block (shown on the item) + flat, × increased block, capped at 50 + max block; lucky block rolls twice; blocked hits deal `base_block_%_damage_taken` | data / game text | data + guess (cap) |

## Runic Ward, remnants, Infernal Flame

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Runic Ward | a pool after life: damage goes to mana-first share, energy shield, life down to 1, then ward; death when ward can't absorb the rest. Maximum = item values (shield) + flat, × increased; regenerates 5% of maximum per second always, × increased ward regeneration (runes) | guides (Mobalytics, Fextralife); user | community + user |
| Ward on block | `recover_X_ward_on_block` | game text | data |
| Ward remnants | Remnants of Kalguur: chance per kill and per new stun (stun cooldown) to create a remnant granting `ward_remnants_gain_per_globe` × (1 + remnant effect); Harmonic Remnants: chance for a second; collected after 1 s (guess) + `remnant_delay_X_ms`; ward overflows to 1.5 × maximum | data; overflow: user | data + user + guess |
| Pyromantic Pact | skill costs build Infernal Flame (maximum: 2 × max mana) instead of spending mana; reaching the maximum empties it and deals `hit_for_%_max_life_es_on_max_infernal_flame` % of max life + ES as a fire hit, reduced by fire resistance; the 25%/s decay when no flame was gained for 2 s is not modelled | node text | data |
| Damage taken as | `<type>_hit_and_dot_damage_%_taken_as_<type>` moves part of each incoming type before mitigation | game text | data |
| All damage ignites / chills / shocks | ignite (chill, shock) scales from all damage types, also for chance and meta energy | game text | data |
| Infusions | not simulated; `--infused` assumes they are always available (skills use their "-Infused" stat sets) | user (always keeps them up) | user |
| Killing hits and ailments | a hit that kills still inflicts its ailments for "on freeze / shock / ignite" effects (meta energy, Archon); their damage over time on the dead monster is dropped | user (Cast on Elemental Ailment triggers much more in packs) | user |
| Named-skill lines | "+N to Level of all <Skill> Skills" (the description has a skill-name placeholder): gem level for that gem | game text | data |
| Triggered a skill recently | 4 s after any trigger | PoE convention | pob |
