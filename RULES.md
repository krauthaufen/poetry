# PoEtry rules ledger

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
| Conversion direction | any type to any other type (Blueflame Bracers: fire to cold); never to itself | wiki Damage conversion | data |
| Conversion | two steps: the skill's own conversion first, then conversion from everything else (items, supports, passives, buffs) on the result. Within a step all types convert at once; more than 100% from one type in a step is scaled down to 100% (100% cold and 50% fire become 67 / 33), so conversion never adds damage | wiki Damage conversion (two step process, overconversion); its examples are tests | data |
| Conversion chains | not recursive within a step: what arrives at a type is not converted again by the same step. What the skill converted can be converted again by the second step (60% physical to lightning by the skill, lightning to cold by an item: 60 cold) | wiki Damage conversion | data |
| Converted damage scaling | uses only the modifiers of its final type, none from the source type | CalcOffence.lua `calcDamage` sums converted base into the destination | pob |
| Gain as extra | taken from the **base** damage of the source type; "of Damage" counts every type, the gained one too (20% as extra fire on 50 physical and 50 fire gives 50 and 70). Conversion then applies to base and gained damage alike (so a fully converted skill stays fully converted). The wiki has it the other way round for gains that are not the skill's own (after the skill's conversion, before everything else's) | wiki Damage conversion (same-type gain); order: user's in-game damage split (Falling Thunder is "almost only elemental" with 28% gain as extra physical; the wiki's order would make a quarter of it physical); PoB gains after all conversion | user observation |
| Damage roll | each type rolled independently and uniformly in [min, max] | wiki: "randomly chooses a number within this range", mean (min+max)/2 | data |
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
| Hit chance | round(1.25 A / (A + 0.3 E)), between 5% and 100% | PoB calcs.hitChance; wiki Accuracy: chance = Acc x 1.25 x 100 / (Acc + Eva x 0.3), 5% to 100%; no penalty within 2 m, up to 90% less accuracy beyond 9 m | data |
| Distance | accuracy is full up to 2 m and falls linearly to 90% less at 9 m | Character.ot accuracy_rating_+%_final_at_max_distance_scaled, PoB falloff | pob |
| Monster evasion | DefaultMonsterStats evasion for its level | game data | data |

## Mitigation

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Resist layers | Σ resist mods (base, exposure, curses all as flat resist on the enemy), capped at max resist (75 + mods, ≤90), floor −200 | CalcOffence.lua `calcResistForType` | pob |
| **Order of exposure / curses / penetration** | exposure and curses lower resist first, then penetration applies | PoB does the same; wiki Penetration: "applied last and is only effective against positive resistances" | data |
| Penetration | lowers resist, but never below 0 (or below the "can penetrate down to a minimum of X" floor); does nothing to resist already at or below the floor | CalcOffence.lua `calcPenResist` (minPen = 0); floor: game text | pob + data |
| Armour | reduction = A / (A + 10 × damage), using that hit's physical damage, plus flat physical reduction, capped at 75% for monsters | CalcDefence.lua `armourReductionF`; Monster.ot cap | pob + data |
| Damage taken | (1 + Σinc taken) × Π more taken; scopes as for damage | CalcOffence.lua ~4060 | pob |
| Hit chance | 100% unless a `HitChance` override is given. Accuracy vs evasion is not modelled yet. | — | guess |

### Armour Break

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Breaking | a hit breaks armour by its share of the damage it dealt ("Break Armour equal to X% of Physical / Fire / Hit Damage dealt", flat amounts), x increased armour break; the monster's armour is lower by the total, up to all of it | stat text | data |
| Duration | fully broken armour lasts 12 s (x duration modifiers); we let partly broken armour come back after the same time without a new break | game keyword text ("Fully Broken for 12 seconds"); the partial case is our reading | data / guess |
| Fully Broken Armour | at zero armour the enemy takes 20% increased physical damage from hits (x effect), or more types with "also applies to ..." | game keyword text | data |
| Not modelled | armour break taken modifiers, breaking below zero, consuming broken armour, sundering | - | missing |

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
| Statuses after buildup | freeze 4 s, electrocute 5 s, pin 3 s, heavy stun 2 s (Monster.ot `base_heavy_stun_duration_ms`) | GameConstants, Monster.ot | data |
| Heavy stun buildup | physical damage and melee damage each build 50% more (multiplicative); a Dazed enemy takes 50% more; Daze lasts 8 s | game keyword text | data |
| Chill | magnitude from cold damage against the ailment threshold, at least 30%, at most 50% | game keyword text, GameConstants | data |
| Isolated | an enemy with no ally within 6 m | game keyword text | data |
| After a buildup ailment | once frozen, electrocuted, heavily stunned or pinned, the monster's threshold for that is 500% more while it lasts and for 14 / 15 / 16.5 / 13 s after (`<X>ThresholdModifier`, `<X>ModifierDuration`) | GameConstants; that the time starts when the status ends is our reading | data |
| Daze, maim, blind, hinder | flat chance × inc × more, 4 s | — | guess (duration) |

## Timing and the event engine

| Mechanic | Rule | Confidence |
|---|---|---|
| Use time | base time / (inc × more `Speed`) | pob |
| Cooldown | cooldown / (inc × more `CooldownRecovery`), starts when the use starts; wiki Cooldown: "By default, cooldowns begin from when you use a skill" | data |
| Hits | one hit at the end of the use, unless the skill or actor gives a hit pattern | modelling choice |
| Triggered skills | use immediately, cost no time, respect their cooldown | guess per trigger |
| Meta gems | energy from rules; at max energy, trigger and subtract max | modelling choice; energy amounts per hook must come from gem data |
| Server ticks | not quantised: actions happen at exact times; wiki Skill speed: one non-channelling action per server frame of 33 ms (the minimum use time we apply) | data |
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
| Weapon local damage | (base + local added) × (1 + local increased physical / 100) × (1 + quality / 100) for physical; other types just added | reproduces the damage 103 corpus weapons show in game (94 exactly, 9 within rounding of attacks per second) | data |
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
| Critical hit roll | rolled once when a skill is used or triggered: every hit of that use is a critical hit or none is. Skills that keep hitting on their own (Sustained) roll for every hit | wiki Critical hit: "When a skill is used or triggered, it performs a critical hit check by rolling a number ... all hits from that skill will be a critical hit"; "Sustained skill will roll critical hit chance for each hit individually" | data |
| Evasion entropy | an attack is not rolled against evasion each time: the defender has an entropy value, random at the first attack and again after 3.33 s without one; each attempt adds the chance to hit, at 100 it hits and 100 is taken off. All attackers share it. The second check that can downgrade a critical hit is not modelled | wiki Evasion | data |
| Flammability | a fire hit leaves Flammability on the enemy: 1% per 5% of the ailment threshold its fire damage is, for 8 s, each on its own. The chance to Ignite is all the Flammability on the enemy including the hit's own. Non-hit igniters (needing 50%) are not modelled | keyword text, wiki Ignite, `BaseFlammabilityDuration` | data |
| Chain range | 6 m for projectiles | wiki Chain: "base chaining distance of 6 metres" | data |
| Glory and Combo limits | a skill gains Glory at most once every 0.5 s per monster, and loses 2 per second once 15 s have passed without any; Combo counts once per strike and is lost after 8 s without one | wiki Glory, Combo | data |
| Armour Break by rarity | a player breaks three times the armour on normal and twice on magic monsters | wiki Armour Break | data |
| Blind and Maim on monsters | Blind: 20% less accuracy and evasion; Maim: 30% slower, 15% reduced evasion | wiki Blind, Maim | data |
| Rain of Arrows hit limit | an arrow lands every 0.08 s and an enemy is hit by the rain at most once every 0.12 s | `rain_of_arrows_delay_per_arrow`, `rain_hit_delay_ms` | data |
| Several projectiles, one enemy | projectiles that leave the player together (`ProjectilesFromUser`): only one of a volley can collide with the same enemy, the others fly on. Not so where the skill says otherwise ("These fragments can Merge" and the `projectiles_can_shotgun` flag, "Multiple Projectiles can hit the same enemy", "fired in sequence"), for shards that spring from an impact (Falling Thunder, Ice Shot: user), and for what hovers, is lobbed or rains down (no `ProjectilesFromUser`): there every projectile is flown and hits what it meets. Each projectile hits an enemy at most once | wiki Projectile ("If multiple projectiles are fired simultaneously, only one of them can collide with the same target, unless otherwise stated"); the skill descriptions that state the exception; user for impact shards | data |
| Spark | every spark is flown: a fan from the player that widens by 15 degrees per spark (three: a 30 degree cone; 25 or more: a circle), the cold-infused one a full circle at a random angle to the enemy. A spark keeps its heading but zigzags inside a band 20 units wide (`spark_pathing_width_bounds`), a new sideways offset every 20 units, for 2 s at 100 units per second (200 units). A wall ends it (the player's Spark lacks `spark_projectile_bounces_off_walls`). Sparks of one cast hit the same enemy at most once every 0.66 s (`spark_min_time_between_target_clearing_ms`): one that may not hit passes through, one that hits is gone | game tables (GrantedEffectStatSets, Projectiles), gem text; user: cone widening with the number, gone on hit. The 15 degrees and the zigzag step are guesses | data / guess |
| Full circle from the player | a nova of projectiles is not aimed: its first projectile lies at a random angle to the enemy | geometry | guess |
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

### The character sheet

Checked against PoB's defensive numbers for 131 public characters (`tools/corpus`, see the report's "character sheet differs" section). Where PoB and the game's text disagree, the text wins and the difference is listed below.

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Class base attributes | from the tree data, by class or ascendancy name (the API names an ascended character by its ascendancy) | tree.json | data |
| Elemental resistance penalty | -60% in the endgame (10% per act) | PoB CalcSetup; shown on the game's character sheet | pob |
| Base evasion | 7 | Character.ot base_evasion_rating | data |
| Item defences | armour, evasion, energy shield, block and Runic Ward as the item shows them: its own flat and increased lines are already in that number and are not applied again | game item text | data |
| Slot increases | "increased Evasion Rating from Equipped Body Armour", "increased Energy Shield from Equipped Focus": additive with the global increases, for that item's share | PoB CalcDefence slotCfg | pob |
| Item-local stats | a weapon's own leech, crit damage bonus, bleed / poison chance, penetration, "always hits": count for attacks with it. Local stats that only change what the item displays are skipped; any other is reported as not simulated | game item text | data |
| Per socket filled | "+X per Socket filled" counts the runes, soul cores and idols in the item | game item text | data |
| Reflected ring | "Reflects opposite Ring": the ring is a copy of the other one | Kalandra's Touch | data |
| Keystone by name | an item line that is just a keystone's name grants that keystone | unique jewels | data |
| Weapon set passives | passives allocated with weapon set points count only while that set is in use; the sheet is shown for set one | API `specialisations`; PoB counts them differently in some builds | data |
| Gem quality | only what the API shows; a gem without the property has none | API | data |
| Doubled chaos resistance, maximum resistance override, life from body armour energy shield, armour from mana / boots energy shield, evasion from helmet energy shield, deflection from armour, "per 10 Intelligence", "per Corrupted Item", Grand Spectrum | as their text says | game text | data |
| Charms | used by themselves when their condition occurs (frozen, stunned, ignited, shocked, slowed, a hit of their damage type, killing a rare or unique), if they have the charges; the effect ("Utility" lines: resistances...) lasts for the charm's duration; immunity charms stop the occurrence that used them and any other while they last. Charges: half the killed monster's power, per second, and by chance on kill. Life / mana on use and "Energy Shield Recharge starts on use" apply. Not modelled: Guard, "Possessed by Spirit", consecrated ground, armour during effect. PoB counts a charm's resistance as always on | game item text | data; charges per kill: half the monster's power (keyword text) |
| Block | capped at 50% + maximum block (poe.ninja shows it uncapped) | Character.ot | data |
| Runic Ward | item values x increases; PoB 0.23 does not know this league's ward items, so it is unchecked | game item text | guess |

### Spirit

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Total | flat Spirit (quests, mods, "per Socket filled", "per X on body armour") plus sceptres' own Spirit (their local lines are in the value they show), x increased Spirit. A sceptre counts for its weapon set: the set that gives more is taken. "You have no Spirit". "50% less Spirit" (Lord of the Wilds) multiplies. "+40 to Spirit for each of your empty Charm slots": slots from the belt and "+1 Charm Slot" lines, at most three, less the charms worn. "+25 to Spirit while you have at least 200 Dexterity" and "+1 to Maximum Spirit per 25 Maximum Life" read the sheet's own numbers. "increased Spirit" on an item that is not a sceptre is global | game item text; matches PoB on 113 of 131 corpus characters | data |
| Reserved | each persistent gem's `spiritReservationFlat` at its level plus its supports', Blasphemy's per socketed curse, x each support's `reservationMultiplier`, x "less Reservation", / reservation efficiency (increased x more). Efficiency: the character's (all skills; meta, herald, minion, companion skills; non-companion, non-minion) plus the gem's own from its level and quality, which is for that gem only | gem data (same values in the live game files) | data |
| Supports in a meta gem | a support that fits a skill socketed in the meta gem and not the meta gem itself (by the support's required / excluded skill types) adds nothing to the meta gem's reservation: Heightened Curse with Cast on Critical + Enfeeble | support type requirements in the gem data; the private build fits its Spirit only this way | data |
| Granted skills | a skill granted by an item reserves no Spirit (API: a skill without an inventory id; PoB: group with source "Item") | user observation | user observation |
| Spirit per corrupted item | "increased Spirit for each Corrupted Item Equipped" counts the equipped corrupted items | game item text | data |
| Not modelled | "Reservation Efficiency of Skills per Idol in your Equipment" | | todo |
| Over budget | only reported: the API doesn't say which persistent gems are switched on or bound to one weapon set, so every socketed one is simulated as active | - | guess |
| Minion counts | each persistent minion gem has its free minions plus what the Spirit left by everything else pays for (shared evenly between several minion gems) | the player chooses this in game; the API doesn't say | guess |

### Incoming damage in detail

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Order | energy shield, then the "taken from mana before life" share of the rest, life down to 1, Runic Ward, death | game text, user (ward) | data + user |
| Chaos vs energy shield | chaos damage removes twice as much energy shield | PoE2 rule (recollection); wiki Energy shield: "Chaos damage removes twice as much Energy Shield" | data |
| Bypass | "X% of damage bypasses energy shield" skips it | stat text | data |
| Armour vs elemental | "X% of armour applies to <type> damage": armour formula with that share, after resistance | stat text; order a guess | guess |
| Damage taken | increased / reduced damage taken by type multiplies after mitigation; flat physical reduction applies to melee hits before armour | stat text | guess (order) |
| Monster crits | 5% chance, +30% damage (monster base_critical_hit_damage_bonus), less with "enemies deal less extra crit damage" | game data; 5% PoB default | data |
| Recoup | share of damage taken from hits (by type) returns to the pool evenly over 8 s; "recoup speed" shortens it | game keyword text | data |
| ES recharge | starts 4 s after the last damage taken (hits and damage over time), / (1 + faster start); 12.5% per second x recharge rate | PoB, Character.ot | pob |
| Life flask | drunk when life falls below half, if charges allow; amount x life recovery rate over its duration | player behaviour | guess |
| Leech (life, mana) | % of physical attack damage dealt, from one enemy per hit event; each instance is recovered over one second ("Leech Life X% less quickly" stretches it) | game keyword text; the one-enemy rule is a guess; wiki Leech: over one second, hit damage capped at 40,000, "only the instance with the highest recovery rate will be active" | data |
| Ailments on the player | threshold = half of maximum life (PlayerAilmentThresholdLifeFactor) + mods. Ignite: chance = fire / threshold x IgniteChanceMultiplier, burns 4 s, strongest only, lowered by fire resistance. Shock: chance likewise, +20% damage taken for 4 s. Chill: slow = ChillEffectMultiplier x cold / threshold for 2 s. Freeze: builds cold x FreezeDamageScalePlayer / freeze threshold, frozen 2 s at 100%. "Avoid", "effect on you" and duration stats apply. Unmitigated hit damage is used. Bleed / poison from monsters: not modelled | GameConstants "...Player" | data; using unmitigated damage is a guess |
| Light stun on the player | chance = hit / stun threshold (maximum life before CI + mods); physical +100%, melee +33% (monster object stats); nothing below 15% (LightStunMinimumChancePlayer); interrupts for 0.35 s (GUESS); a stun only delays the next action, it does not cancel one under way. | game constants; formula a guess | guess |
| Heavy stun on the player | not applied: "Players and their Minions usually cannot be Heavily Stunned" (only while raising a shield, parrying or mounted, which are not modelled) | game keyword text | data |
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
| Eldritch Battery | the energy shield on items (and flat energy shield) becomes base mana; increases to energy shield don't apply to it, increases to mana do; costs doubled | user; game keyword "Stat Conversion" (the base value of A goes to B and scales with B's modifiers, not A's) | data |
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

### Minions

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Which monster | the gem's first minion (skills' minion list); "Spectre: X" / "Companion: X" by monster name | game data (PoB Minions / Spectres export) | data |
| Level | the gem level's level requirement (spectre / companion gems: a minion gem's curve) | PoB | pob |
| Attack damage | monster damage of that level x the monster's damage multiplier, +- its spread, as physical "weapon" damage at its attack time, 5% crit | PoB minion weaponData | pob |
| Skills | the monster's skills that deal damage, scaled to its level; cooldown skills when ready, else the strongest; the gem's supports apply | game data | data; choice of skill is a guess |
| Owner's mods | "Minions deal / have ..." mods apply; minion gem levels from "+ to level of minion skills" are NOT applied | | partial |
| Count | 1 per gem, or the gem's pack size; how many a build reserves spirit for is unknown from the API | | guess, usually too low |
| Not modelled | temporary minions (zombies, raging spirits), minion life and death, command skills, minion accuracy (always hit), auras from minions | | todo |

### Totems, curses, buffs, damage over time

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Totems | the totem is an actor using the gem's second skill (or, for meta totems, the socketed skill) with the owner's weapon, mods and the gem's supports; it acts while the totem stands: the gem's use places it for `base_totem_duration`. All totems up to the limit stand after one placement (placing each is not simulated); totem life is ignored | gem data | data; the simplifications are guesses |
| Curses and marks | applied to what the (damage-less) skill hits, for `base_skill_effect_duration` x duration mods, scaled by curse / mark effect and the monster's curse effect; one curse (+ "additional curses"), one mark; the oldest gives way. Effects from "base_skill_buff_..._to_apply": resistances, armour, damage dealt, action speed; marks: crit bonus, freeze / electrocute buildup against the marked enemy. Curse activation delay is NOT simulated | gem data | data |
| Curse activation delay | a cast curse lands `curse_delay_duration_ms` (1.5 s) after the cast, x "slower / faster Curse Activation"; none with "Curses have no Activation Delay". It is not cast again while on its way. Marks and Blasphemy auras have no delay | gem stat, stat text | data |
| Buff skills | a non-persistent Buff skill with a duration grants its own stats for that time; a skill that consumes charges grants one stack per charge with the gem's per-stack stats and duration | gem data | data |
| Skill damage over time | "base_<type>_damage_to_deal_per_minute": per skill and type one instance on each enemy hit, for the skill's duration; scaled by damage of that type and damage over time; spell damage only if the gem says so; the strongest instance stays | gem data | data |
| Archmage | non-channelling spells gain X% of damage as lightning per 100 maximum mana and cost an extra share of maximum mana | gem data | data |
| Trinity | each hit adds resonance to its strongest element (up to 100) and takes some from the others; X% more elemental damage per 50 total resonance; decay over time NOT simulated; the cap of 100 is a GUESS | gem data | guess |
| Mage's Legacies (Mageblood) | each kind applies once; all gain the belt's "increased effect per duplicate" per duplicate | PoB CalcPerform | pob |
| Auras | Purity, Discipline...: the gem's "..._to_apply" stat on you, x increased aura effect; presence range ignored | gem data | data |
| Aftershock | a slam with aftershock chance hits its area once more | stat text | guess (same damage and area) |

| Skills that keep hitting ("Sustained": Orb of Storms, Thunderstorm, Solar Orb...) | one use hits repeatedly: every "..._frequency_ms" / "..._pulse_delay_ms" / "..._every_x_ms", for "..._maximum_number_of_hits" / "..._number_of_pulses" hits or the skill's duration; recast when the effect ends. Extra bolts from other casts (Orb of Storms) and stages are NOT modelled | gem data | data; partial |
| Shapeshift skills | using one means "while Shapeshifted" conditions hold for the whole fight | | guess |

### Crossbows

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Ammunition gems | the gem grants "Load X" and the shot; the shot is the skill, the load its ammunition: bolts per load from `base_number_of_crossbow_bolts` (gem and supports) | gems.json additionalGrantedEffectId1 | data |
| Reload | after the last bolt: weapon reload time / ((1 + increased attack speed + increased reload speed) x more), or instant by chance | PoB CalcOffence calcCrossbowReloadTime | pob |
| Cost | paid per load, with the first shot of a new load; a fight starts loaded | game: the Load skill carries the cost | data |
| Not consuming ammunition | chance while "reloaded recently" (4 s) | stat text | data |
| Heat, barrage timing, grenades' own rules | not modelled | - | missing |

### Channelling and stages

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Staged channels | Flameblast, Supercharged Slam, Detonating Arrow, Volcano: one use is the channel to the maximum number of stages (each takes the skill's own time) plus the release animation; the hit carries the per-stage more damage, gain as fire and radius | gem stats `*_maximum_stages`, `*_damage_+%_final_per_stage`, `*_radius_+_per_stage` | data; always channelling to full is a play-style assumption |
| Held channels | a channelled skill with a cooldown (Flame Breath) is held for `base_minimum_channel_time_ms`: one use lasts that long and hits once per its own attack / cast time | gem stats; holding exactly the minimum is a play-style assumption | data |
| Other channels | used repeatedly, one hit per use time (Incinerate's fuel, Snipe's perfect timing, Rampage, shield skills: not modelled) | - | partial |

### Shield attacks

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Base damage | skills that hit with the shield (Resonating Shield, Shield Charge, Raise Shield, Parry) use `off_hand_weapon_minimum/maximum_physical_damage` plus `off_hand_..._added_physical_damage_per_N_shield_armour` (or evasion) x the shield's own armour / N | gem stats | data |
| Active block, retaliation, parry | not modelled | - | missing |

### Named effects

The game's standard buffs and debuffs, with the numbers of its keyword text. Stats grant them on hit, crit, kill or use ("X% chance to gain Arcane Surge when you deal a Critical Hit"), on the character for any skill and on a gem for its skill.

| Effect | Rule | Source | Confidence |
|---|---|---|---|
| Onslaught | 20% increased skill speed (x Onslaught effect) | keyword text | data |
| Arcane Surge | 15% increased cast speed (x effect); its 20% more mana regeneration is not applied; 4 s | keyword text; duration is a guess; wiki: 15% increased cast speed, 20% more mana regeneration, 10 s | data |
| Unholy Might | 30% of damage gained as extra chaos (x magnitude); "You have Unholy Might", also "while not on Low Mana" and "while you have no Energy Shield"; minions likewise | keyword text | data |
| Tailwind | per stack 2% increased skill speed and 10% increased evasion, up to 10; a stack per skill use, all lost when hit; it does not expire by itself (guess) | keyword text; buff table limit 10; wiki: 2% skill speed, 10% evasion, 1% movement speed per stack | data |
| Withered | 5% increased chaos damage taken per stack, up to 10 (x magnitude), for the stated seconds (4 if none) | keyword text | data |
| Critical Weakness | +0.5% crit chance against the enemy per stack, up to 20; "every second in your Presence for N seconds" is a steady N stacks on every enemy | keyword text; presence area ignored | data |
| Culling Strike | an enemy at or below 35% / 20% / 10% / 5% life (normal / magic / rare / unique; x increased threshold) is killed, checked before the hit's damage | GameConstants, keyword text | data |
| Not modelled | Impale, Incision, Sundered Armour, Volatility, Consecrated / Jagged Ground, Guard, Ancestral Boost, Decimating Strike | - | missing |

### What the attacker does to the enemy

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Resistance lowered by a skill | "Enemies Frozen by this Skill have -X% to Cold Resistance": counted for the attacker's own hits against frozen enemies (it would also help allies; not modelled) | stat text | data |
| Increased damage taken | "Enemies you Curse / Mark take X% increased Damage": additive with the enemy's other increased damage taken (shock...) | stat text | data |
| Faster damaging ailments | "deal damage X% faster": that much more per second for that much less time | stat text | data |
| Exposure effect | "Exposure applied has X% increased effect" scales the exposure the character applies | stat text | data |
| Withering Presence | Withered on every enemy: duration / interval stacks at any time, at most 10 (presence area ignored) | gem stats | data |
| Close Range | within 2 m of the player; Isolated: no ally within 6 m | keyword text | data |

### Warcries

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Empowering | a warcry empowers one attack per `warcry_empowers_per_X_monster_power` of enemy power (normal 1, magic 2, rare 5, unique 20), counting up to its cap, at least one; all living enemies count (the cry's area is not checked) | gem stats | data; area ignored |
| Using them up | each attack the player makes while empowered uses one, after its hits; triggered attacks and the cry itself don't | game rule | guess |
| What empowered attacks get | "Empowered Attacks deal X% increased Damage"; Infernal Cry: gain X% of damage as extra fire; Seismic Cry: slams cause an additional aftershock | stat text, gem stats | data |
| Not modelled | the cries' own explosions and knockback, Guard (Fortifying Cry), rage per power (Ferocious Roar), Ancestral Cry, Arctic Howl's added cold damage | - | missing |

### Heralds

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Herald buffs | the gem's granted stats are on the character while reserved | gem data | data |
| Herald of Ice | an attack that kills a frozen enemy triggers the gem's "Explosion" stat set (weapon damage as cold, area), centred on the enemy aimed at rather than the corpse | gem description + data | data; centre is a simplification |
| Herald of Thunder | after an attack kills a shocked enemy, the next N attack hits (`herald_of_thunder_storm_max_hits`) each trigger the "Bolt" stat set | gem description + data | data |
| Chains | a herald's own hits don't set heralds off | game rule (recollection) | guess |
| Herald of Ash | a non-herald attack that kills with overkill of at least `herald_of_ash_overkill_threshold_%` (20%) of the enemy's maximum life ignites the living enemies in the "Explosion" stat set's area (around the killed enemy) for `herald_of_ash_burning_%_overkill_damage_per_minute` / 60 of the overkill per second, for the explosion's duration. Overkill is the hit's damage beyond the enemy's remaining life | gem description + data | data; the ignite takes no further modifiers (guess) |
| Per 100 maximum mana | "increased Spell Damage / Spell Critical Hit Chance per 100 maximum Mana" count the full hundreds of the character's maximum mana | stat descriptions | data |
| Item modifier magnitudes | "increased Explicit Prefix / Suffix Modifier magnitudes" and "can be Instilled" add nothing: the item's lines already show the changed values | item text as the API gives it | guess |
| Per attribute | any stat "... per N Strength / Dexterity / Intelligence" is the stat without that tail, once per full N of the attribute (the attributes are stacks "Str", "Dex", "Int" on the sheet and in the fight). "Gain no inherent bonus from Strength": no life from Strength | stat ids and descriptions | data |
| Fire resistance mirrored | "Modifiers to Fire Resistance also grant Cold and Lightning Resistance at N% of their value": N% of the fire resistance modifiers' sum (not the penalty) is added to both; "Modifiers to Maximum Fire Resistance also grant Maximum Cold and Lightning Resistance" likewise for the caps | stat descriptions | data |
| Effect of small passives | "increased effect of Small Passive Skills": the modifiers of allocated passives that are not notables, keystones, jewel sockets, ascendancy passives or "+5 to any Attribute" nodes are multiplied, and cut off to whole numbers (+8 at 50% is +12, 3% is 4%) | stat description; PoB CalcSetup excludes attribute and ascendancy nodes and cuts off the same way | pob |
| Bonuses gained from an item | "increased bonuses gained from Equipped Quiver", "from Equipped Rings and Amulets", "from left / right Equipped Ring", "increased Effect of bonuses gained from Socketed Jewel" (the jewel in an ascendancy's socket): that item's modifiers are multiplied and cut off to whole numbers. Lines of runes and soul cores already show their "increased effect of Socketed Items" and are not scaled again | stat descriptions; rune values in the API compared with the socketed item's own text | data |
| Per defence on an item | "+X to A per N Item Armour / Evasion / Energy Shield on Equipped Helmet / Gloves / Boots / Body Armour / Shield": X for every full N of the value that item shows | stat descriptions | data |
| More maximum Life, Mana, Spirit | "N% more maximum Life" on a character is a multiplier of its own (Titan, Oracle, uniques); the same words on a monster describe its rarity, and a character's line is never read that way | stat descriptions: one text, several stat ids | data |
| Sheet of the second weapon set | the character sheet is the one of weapon set one; `SheetSetTwo` shows it with set two in hand: that set's weapons, shield and passives count, the first set's do not | game: only the weapons in hand apply | data |
| Alternative attribute nodes | an ascendancy's "Attribute Passive Skills can instead grant ..." (Pathfinder): the API gives the chosen line per node, the corpus reads it from the notable's text | stat text; user observation | data |
| Parts that only poison | a stat set marked "no hit damage" and "poisons without hit" (Poisonburst Arrow's burst) deals no hit damage; every enemy in its area is poisoned for what the hit would have dealt, always, and for the skill's duration where "poison duration is skill duration" | skill data flags and the skill's description | data |
| Channel cost | a channelled skill costs its "mana per minute" for as long as each use takes; what it costs to start is paid when the channel begins, not with every use that continues it | gem level data (`cost.ManaPerMinute`); before, such skills were free or paid the start cost every use | data |
| Barrage | "projectiles_barrage": the projectiles are fired one after the other at the target, so each can hit it (unless a support fires them in a nova) | skill description: "fired in sequence, allowing multiple Projectiles to Hit the same target" | data |
| Ice-Tipped Arrows | using it empowers the next 4 Repeatable bow or spear attacks: their physical damage is converted to cold, and each of their hits triggers Ice Fragments (the gem's second effect, an attack with its own multiplier and a 0.15 s cooldown). Each fragment is thrown on from the enemy hit to a random distance of 0.1 to 4 m within a 100 degree cone away from the player and explodes there 2.5 s later (radius 1.8 m): it hits what is in reach. An empowerment is used up when the attack's last hit has landed. The 20 s duration and bypassing the cooldown with a Frenzy Charge are not modelled. Used whenever ready if the main skill can repeat | `shearing_bolts_*` gem stats, `IceTippedArrowsIceFragmentPlayer` (`ice_fragment_min/max_chunk_distance`, `ballistic_ice_chunk_cone_angle`); a uniform distance is a guess | data |
| Ice Shot's shards | the cone of shards starts at the enemy the arrow hit and flies on: it never hits that enemy, only what stands behind it. The cone is 135 degrees (`total_projectile_spread_angle_override` 300, 0.45 degrees a unit); a shard lives 0.2 s at 210 units a second, its speed varying by half either way, so it reaches 2.1 to 6.3 m. Against a lone boss Ice Shot is the arrow alone. PoB adds the shards to the arrow | description ("sprays a cone of ice shards when it hits a target"); wiki Ice Shot (135 degree cone behind the primary target, shards last 200 ms; patch notes 90 degrees then +50%); game table Projectiles (IceShotShard 2100); user | data |
| Tempest Bell | once its Combo is built the bell is dropped: the Impact hits, and the bell then stands for 10 hits. Every attack hit with another skill sets off its Shockwave (at most one per 0.3 s) and uses one of the 10; a new bell replaces the old. The bell is taken to stand where the attacks land. Its 6 s duration, ailments on the bell adding elemental damage and knockback widening the shockwave are not modelled. PoB shows one shockwave | `bell_hit_limit`, `bell_shockwave_cooldown_ms`; description | data / guess |
| Lightning Warp | it only does anything to an enemy that can be culled, or to one's own Ball Lightning: a warp into a ball still on its way ends the ball (the bolts it had left are not fired) and explodes there, taken to be in reach of the target. The Lightning Infusion comes with the explosion, not with every cast. It is only used when there is something to warp into; with Ball Lightning as the main skill it is used into every ball. PoB adds a warp explosion per cast | description ("Can also be used on Ball Lightning Projectiles", "Creates a Lightning Infusion on successful use") | guess |
| Demon Form | in the form: 25% increased cast speed, one Demonflame per second (at most 10), 3% more spell damage per Demonflame, and life lost every second for each Demonflame (the gem's amount per minute). At 1 life the form ends, the Demonflame is gone and the 15 s cooldown starts; then it is entered again. It is not pressed while in the form. Using a non-spell skill ending the form is not modelled. PoB assumes the form with a chosen number of stacks, for ever | `demon_form_*` gem stats, description | data |
| Rain of Arrows | each arrow lands at a random point of the skill's area (radius 36) and hits what is within its own impact (radius 7): a boss in the middle is hit by about 22% of the arrows, a normal monster by 9%. All arrows are counted at the end of the use (the rain really takes 80 ms per arrow). PoB counts one arrow | `active_skill_base_area_of_effect_radius`, `active_skill_base_secondary_area_of_effect_radius`; uniform landing is a guess | data / guess |
| Volcano | the eruption (with its stages) hits once when the volcano rises; after that only the spray of projectiles comes every 0.5 s for the duration, each projectile landing at random within 30 units with an impact radius of 6 (a boss is hit by about 28% of them). The eruption's own spray has 4 more projectiles for each stage after the first (12 at four stages), landing the same way. Sprays from slamming the volcano are not modelled | `volcano_fires_projectiles_every_x_ms`, `volcano_initial_explosion_number_of_additional_projectiles_per_additional_stage`, the stat sets' radii; uniform landing is a guess | data / guess |
| Charge Regulation's buffs | 25% more Skill Speed while a Frenzy Charge is held, 26% more Critical Hit Chance while a Power Charge is held (level 20); the charges themselves are consumed every 10 s and must come from somewhere. More Defences with an Endurance Charge is mapped but the sheet does not look at charges | `charge_mastery_*` gem stats and their description | data |
| Sigil of Power | the sigil stands for its duration and the character is taken to stay in it: 14% more spell damage per Stage, one Stage at the start (guess), another each time 50% of maximum mana has been spent on pressed skills while it stands, at most one a second, up to 4. A new sigil starts over. Mana spent by triggered skills is not counted. PoB assumes a chosen number of stages for ever | `circle_of_power_*` gem stats and descriptions ("Gains a Stage when you Spend a total of {0}% of your Maximum Mana while in Area", "Minimum 1 second between gaining Stages") | data / guess |
| Mantra of Destruction | once its Combo is built it empowers the next attack: 69% of all damage gained as extra chaos damage (level 20). The Purple Flames from kills with that attack are not modelled | `mantra_of_destruction_grant_all_damage_%_to_gain_as_chaos_with_attacks`, `skill_empowers_next_x_melee_attacks` | data |
| Temper Weapon | each use is one strike of the hammer and empowers 3 attacks, 12 at most. Every hit of an empowered attack triggers Combust (the gem's second effect, a fire area attack); an empowerment is used up when the attack's last hit has landed | `temper_weapon_empowers_per_strike`, `imbue_weapon_max_exerts`, `TemperWeaponCombustionPlayer` | data / guess |
| Freezing Mark, Voltaic Mark | a hit on the marked enemy while it is Frozen (Electrocuted) gives the buff: 30% of damage gained as extra cold (lightning) damage for 10 s. The mark is not taken off, so the buff is refreshed for as long as the enemy stays frozen. Against a boss that is hard to freeze the buff is rare; PoB has it always | `*_damage_buff_damage_%_to_gain_as_cold`, `base_secondary_skill_effect_duration`, description | data / guess |
| Inevitable Agony | the debuff records 25% of the life the enemy loses while it lasts (12 s) and takes that much life again when it ends, or at once when that would kill. It is not cast again while it runs. Damage over time is recorded too (the game records hits only) | `curse_pain_synchronisation_base_recorded_damage_%_to_apply`, description; wiki: "25% of Hit Damage dealt to them while Debuffed": hits only | data |
| Berserk | besides stronger Rage: 6% of maximum life is lost per minute for every Rage held, while Rage is not draining (life stays at 1 or above) | `skill_base_life_loss_%_per_minute_per_rage_while_not_losing_rage_to_apply` | data |
| Time of Need | heals its amount of life every 8.1 s (faster with cooldown recovery). Removing curses and ailments is not modelled | `skill_time_of_need_gain_x_life`, `base_cooldown_modifiable_repeat_interval_ms` | data |
| Ghost Dance | up to `ghost_dance_max_stacks` Ghost Shrouds, all there at the start, one more every repeat interval (faster with cooldown recovery). A hit taken (not an evaded one) spends one, and for 4 s ("Recently") energy shield regenerates at 2% of evasion per second, x energy shield recovery rate; losing another only restarts the 4 s | gem stats, stat text | data |
| Spearfield | the stab itself deals nothing; the spears do when an enemy touches them. "Creates 1 Spear per 0.2 metres of Radius": 34 at level 20, each at a random spot of a cone in front of the player, 6.9 m long and 90 degrees wide. A spear an enemy stands on goes off (0.6 m) at once, and within 0.5 s of appearing it deals 80% less; the others wait and are not set off by a standing enemy. Enemies walking into waiting spears (full damage) and the limit of 70 are not modelled | `spear_wall_cm_per_spear`, `hazard_damage_+%_final_for_first_500_ms_when_not_killed_by_self`; wiki Spearfield ("Spears emerge within a (4-6.9) metre length cone"); the cone's angle and spears spread evenly at random are guesses | data / guess |
| Parts that go for other enemies | Lightning Arrow's beams, Lightning Spear's bolts and Molten Blast's shrapnel start at the enemy that was hit and cannot hit it; against a lone boss they do nothing | descriptions ("at nearby enemies", "at multiple other enemies", "in a cone behind it") | guess |
| More parts of one use | Detonating Arrow's explosion, Shield Charge's collision (on top of its area), Molten Crash's second slam and its fissures, Starborn Onslaught's star fall, one Wind Serpent when a Frenzy Charge is consumed. Earthshatter's spike explodes when a warcry is used. Detonate Living's explosion needs a cullable enemy like the skill itself. The numbers of fissures, stars and serpents beyond one are not modelled | stat sets of the gems; descriptions | data / guess |
| An explosion per ailment consumed | Elemental Sundering deals nothing by itself: a cold, fire or lightning explosion for a Frozen, Ignited or Shocked enemy, taking the ailment. Snap likewise needs one of the three and takes it. Acidic Concoction's burst needs Poison and takes it. Frost Darts' ice chunks need a Chilled or Frozen target, their lodged explosion a Cold Infusion; Fireball's firebolts a Fire Infusion; Ember Fusillade's beams a Lightning Infusion | descriptions | data / guess |
| Forms that spend something | Furious Slam spends 10 Rage when it has them and is then the Enraged slam instead of the plain one. Rend with a Power Charge to consume is the Lightning-Charged slash instead of the plain one (its buff lasting beyond the use is not modelled) | `bear_slam_spend_X_rage_for_stronger_slam`, stat sets, descriptions | data / guess |
| Crushing Fear | its shockwave happens when the skill kills an enemy, where that enemy stood; never against a lone boss | description | guess |
| Frost Bomb's exposure | 20% Elemental Exposure at once and 2% more with every pulse of the orb, up to 50%; each lasts the secondary duration, the strongest applies. A pulse every 0.25 s is assumed (not in the data), so one bomb reaches its cap as it detonates | "Initially applies {0}% Elemental Exposure and {1}% Exposure per pulse, up to a maximum of {2}%" (`active_skill_all_elemental_exposure_compounding_magnitude`, `_cap`) | data / guess |
| Sniper's Mark | the mark's extra critical damage bonus is for the next critical hit on the marked enemy only: that hit takes the mark off and grants a Frenzy Charge. The mark is put on again when its cooldown allows. PoB keeps the bonus on every crit | description ("The next Critical Hit the target receives Activates the Mark, Consuming it") | data |
| Volleys | a skill that comes down in a number of volleys hits once with each (Supporting Fire's Death From Above: six, over its whole area). The first comes down as the use ends, the others 0.75 s apart (4.75 s for all six; the delay shrinks with the minion's attack speed, which it should not) | `*_number_of_volleys`, `*_volley_delay_ms` | data |
| Offerings | Pain Offering: for its 6 s every minion deals 58% increased damage and attacks and casts 29% faster (level 20). Soul Offering: 77% increased spell damage for the player while it stands. Both are kept up; the skeleton they take is not removed. Bone Offering is not modelled | `pain_offering_*`, `power_offering_buff_spell_damage_+%` | data |
| Hexblast | it only does anything to an enemy with a curse of which at least half the duration has passed: that curse is detonated (one explosion) and gone. It is not cast otherwise; the curse is then put on again by the play style. PoB counts an explosion per cast | description | data |
| Righteous Descent, Fragments of Divinity | "Create a Fragment of Divinity in your Presence every 4 seconds": one lies ready (how many can lie about is not in the data). With one, the leap takes it at the start, is the Empowered set (x14.49 at level 20, larger area) and does not start its cooldown; a character that makes fragments waits for one. Without a source it is the plain leap on its 10 s cooldown. The 10% life and mana recovered is not modelled | stat text, gem stats; one fragment at a time is a guess | data |
| Bloodstone Lances, Shattering Spite | "Spear Skills inflict a Bloodstone Lance on Hit, up to a maximum of 30": a stack on the enemy per spear hit, lasting the fight. Shattering Spite (which cannot inflict them) does nothing without lances, takes them all and deals 20% more per lance. It is pressed only once 30 are there. The larger area from 5 lances and the leech from 10 are not modelled | stat text, gem stats; pressing at 30 is ours | data |
| Consumed states | what a hit consumes (a lance pile, Freeze for "Vs Frozen", a detonated curse) is gone when the hit has landed. Before, the enemy was written back as it was before the hit, so the state returned | engine | data |
| Ice Crystals (Grim Pillars) | the 8 pillars stay as Ice Crystals for the skill duration, each withstanding `frost_wall_maximum_life`. Each pillar stands at a random spot of the skill area (4 m around the caster); those whose explosion (2 m) reaches the enemy count, and the same share of the limit of 20. They are destroyed once the enemy beside them has lost one crystal's life since they appeared (the use's own eruption does not count), and each then explodes at full damage. Explosions break later crystals in turn. Frozen Locus and Glacial Bolt's wall count as one crystal each. Frost Wall is cast onto the target: a row of crystals through it, one per 0.8 m of the wall's length (seven at 5.6 m). Those that come up inside the monster's body are broken by it at once and deal 80% less; the others whose explosion (2 m) reaches it stand until broken like the pillars. Enemies breaking crystals (they take 832% more from enemies) is not modelled. Crystals that last out their duration vanish without exploding | gem stats (`wall_maximum_length`, `frost_wall_maximum_life`); wiki Ice Crystal ("larger enemies can walk through them and destroy them", 80% less within 0.5 s); user (no explosion on expiry); placement, one crystal per 0.8 m, and enemy life lost standing for damage to the crystal, are guesses | guess |
| Infused without a stat set of its own | a skill that consumes an infusion but has no "-Infused" stat set takes what its stats say consuming does: Arc deals 200% more damage and chains 6 more times when it has a Lightning Infusion to consume | `*_from_infusion_consumption` gem stats | data |
| What a fight starts with | Charges: a type is at its maximum at the start only if the build can generate it (Thaumaturgical Dynamism style generation, a charge when an infusion is consumed, a gem of the type "generates charges" whose description names the charge). Against a lone enemy, a source that needs kills, culls or corpses does not count, and rage starts at zero unless it regenerates by itself: a boss's introduction outlasts both. "Recently" conditions are never on at the start; they come from what happens in the fight | user: buffs that last may be up when the fight starts, what comes from the last kill is not, bosses have introductions far longer than 4 s | guess |
| Constant flasks | "This Flask cannot be Used but applies its Effect constantly" (Lavianga's Spirits): its "Recovers N every M seconds" is N / M per second of recovery, it is never drunk, and "during any Flask effect" / "during Mana Flask effect" conditions always hold. With a flask that is drunk they hold while it lasts | item text | data |
| Mana Remnants | the persistent gem: a remnant on a critical hit against an enemy with an elemental ailment, at most one per 2 s, and with 25% chance on killing such an enemy; collected (after the pick-up time), each gives the gem's mana x remnant effect, plus "N% of Maximum Mana when you collect a Remnant"; mana overflows up to twice the maximum (how far is a guess) | gem data and description | data |
| Weapon for a skill | when weapons in both sets fit a skill, the stronger one (damage x attacks per second) is used: the API does not say which set a gem is bound to | - | guess |
| Default attacks | a weapon's default attack (Bow Shot, Grenade, Mace Strike) is not kept on cooldown beside the main skill | play-style assumption | guess |
| Inevitable Critical Hits | a hit that could crit and does not rolls again until it does; 30% less Critical Damage Bonus for each re-roll (nothing left after four), and it counts as a critical hit | poe2db keyword text; PoB averages the same series | data |
| Reserved life | "Reserve Life instead of Spirit" (Atziri's Communion): the skill and its supports reserve 0.66% of maximum life per point of Spirit they would cost, and no Spirit. "Reserves N% of Life" adds. Life never rises above what is left, so with 65% or more reserved the character is on low life for the whole fight (26 corpus characters are built that way) | support gem data; PoB shows the same reserved share | data |
| Parts for one state of the enemy | a stat set labelled "Vs Frozen", "Vs Armour Broken", "Vs Dazed" only happens against an enemy in that state; skills that consume the state (Fragmentation Rounds, High Velocity Rounds) take it off | stat set labels and skill types | data |
| Charges consumed | "Consumes a Frenzy Charge": one per use; "Consume 3 Endurance Charges": three; "Consumes your Power Charges", "Consume all": every one held | skill descriptions | data |
| Charged variants | a stat set labelled "Frenzy-Charged" happens only when the use has a charge to consume, and then in place of the skill's plain "Explosion" (Explosive Spear) | skill description; which part it replaces is read off Explosive Spear only | guess |
| Radius jewels and weapon-set passives | "Passive Skills in Radius also grant ..." counts passives allocated with weapon-set points too, each while its set is in hand | PoB counts them (11 notables around one corpus jewel, all set-one passives) | pob |
| Bifurcating critical hits | "Bifurcates Critical Hits" (a weapon), Garukhan's Resolve: the critical hit chance is rolled twice; one success is a critical hit, two apply the Critical Damage Bonus twice | PoB CalcOffence (BifurcateCrit); no keyword text found | pob |
| Sealed skills | a skill gains a seal every so often (a fixed time for Salvo and Freezing Salvo, a share of its use time for Unleash and Zarokh's Revolt), up to a maximum; using it breaks all of them: one more projectile each (Salvo) or one repeat of the hits each at the support's repeat damage (Unleash: 50% less). A fight starts with the seals full. Repeats follow 0.1 s apart (guess) | support gem data and descriptions | data |
| Glory | a skill that requires Glory (Hammer of the Gods, Volcanic Fissure's finishers and the like) cannot be used until its own counter is full, and using it spends the counter. The counter grows by the monster's power (normal 1, magic 2, rare 5, boss 20) times the skill's percentage for each event the skill names: an attack hit, an elemental or chaos hit, a Heavy Stun, Freeze or Ignite inflicted; a fixed amount for the flat ones. Hits of the skill itself build nothing. A fight starts at zero. PoB assumes the skill is always usable | `active_skill_requires_X_glory`, `active_skill_generates_mp_%_glory_per_*`; monster power values are community numbers | data / guess |
| Less Glory required | Daresso's Passion and supports scale the amount needed | `support_glory_required_+%_final` | data |
| Combo | a skill that requires Combo (Tempest Bell, Flicker Strike without charges and the like) needs that many Combo, gained one per melee attack hit with any other skill, with a chance for one more where a stat gives it; using the skill spends it. A fight starts at zero. Combo that expires between hits is not modelled (the rotation never pauses long enough) | `active_skill_requires_X_combo`-style gem stats | data / guess |
| Skills that must be built up | in the automatic play style a skill needing Glory or Combo is used whenever it is ready; if it is the main skill, the weapon's default attack fills the time between | | guess |
| Banners | a banner gem reserves Spirit and grants the banner skill: 200 Glory at 100% of the monster's power per attack hit. At full Glory it is placed at once and its aura (more attack damage, attack speed, accuracy; scaled by banner aura effect) lasts for its duration, lengthened by skill duration. Glory is held at the full amount while the banner stands, so it is placed again with the first hit after it ends. The character is assumed to stay in the aura. Valour stacks and the defensive banner auras are not modelled. PoB assumes the banner always stands | the banner's additional effect in the gem data (`*BannerPlayer`) | data / guess |
| Perfect Timing | Perfect Strike, Snipe and Gathering Storm are always released with Perfect Timing: the perfect strike or dash takes the place of the normal one, Snipe's arrow explodes, and against a frozen enemy the Icy Blast replaces the explosion and takes the Freeze. One use lasts the channel up to the perfect moment (the skill's attack time, e.g. 30% of base attack speed) plus the release, a share of that time. Gathering Storm's Tempest Shockwave needs a Tempest Bell, which is not tracked | `perfectly_timed` stat sets, `channel_end_duration_as_%_of_attack_time`; that the release comes on top of the attack time is a reading of the stat name | data / guess |
| Chained attacks | Ice Strike, Tempest Flurry, Primal Strikes and Shred are a chain: each use is the next strike, with that strike's stat set (damage, conversion, shock) and its own attack speed. Ice Strike: two fast strikes, then the slow Third Strike. Tempest Flurry: two, a third that strikes three times, the Fourth Strike. Primal Strikes: two stabs, the Final Strike (which takes Shock off the enemy). Shred: two, three Fast Strikes, a Final Strike that hits twice. Using another skill in between starts the chain again. Primal Strikes' spirits come once per use against a shocked enemy; the stampede of six is not modelled. PoB shows each stat set as if it were used alone | `statset_index_for_<nth>_animation_combo_index`, `supplementary_stat_container_attack_speed_+%_final`; chain lengths and strike counts from the descriptions | data / guess |
| Several strikes in one use | Rapid Assault: five stabs spread over the use, then the Final Strike. Cross Slash: both gouges hit the target (a boss is wide enough; against small monsters this is generous) | descriptions | guess |
| Requiem (The Last Lament) | "Does not use Ammunition": composing it (the mana cost, once) opens 12 s in which the shot is fired at its own speed (400% of base) without loading; then it needs 100 anguish again, at 200 a minute: 30 s. A fight starts with it charged, and anguish is taken to build up only once the 12 s are over. Each shot comes down at a random spot within 2.4 m of the target and hits what is within 1.6 m of it | `rapidshot_requiem_active_duration_ms`, `_base_buff_count_requirement`, `_number_of_stacks_per_minute`, `crossbow_ammo_has_no_ammo`, the shot's two radii; wiki Requiem ("fixed duration of 12 seconds that cannot be scaled"); charged at the start and no anguish while it runs are guesses | data / guess |
| Hailstorm Rounds | no clip and no reloading: a bolt builds up every reload time (the weapon's, over the skill's 150% more reload speed and the character's reload and attack speed, as at the start of the fight), up to 50, and the fight starts with all of them. A use costs its mana, fires every bolt that has built up and needs at least one; each bolt lands at a random spot within 4 m of the target and hits what is within 1 m of it. It is used whenever there is a bolt (waiting for more saves mana, not time). Bolts added by supports are not counted | `crossbow_ammo_skill_has_passive_reloading`, `base_number_of_crossbow_bolts`, the shot's two radii, description; full at the start and uniform landing are guesses | data / guess |
| Stat sets that deal nothing | a spell's extra stat set whose own damage is 0 at the gem's level and that has no damage over time is not a hit (Exsanguinate's "Damage over Time" set is 0 to 0 for the player: its damage over time is the Bleeding its hit inflicts; Flame Wall's "Projectile Damage" set is what it adds to projectiles) | skills.json: `spell_minimum/maximum_base_*_damage` 0 at level 20; the monster version of Exsanguinate has `base_physical_damage_to_deal_per_minute`, the player's does not | data |
| Twister | the twister is flown: it leaves the player at 3 m a second, strays up to 1 m to either side of its line and lasts 3 s (9 m). An enemy inside it (radius 0.5 m plus the enemy's own size) is hit on contact and again every 0.66 s while it stays inside; twisters of one use share that interval, and fan out 15 degrees apart. A small boss gets one or two hits per twister, a large one three. Whirlwinds consumed for more twisters and elemental ground are not modelled. PoB counts one hit | `twister_base_movement_speed`, `twister_hit_interval_ms`, `minimum_per_projectile_spread_angle_degrees`, description ("Twisters fired at the same time can Hit the same target no more than once every 0.66 seconds"); how far it strays is a guess | data / guess |
| Ball Lightning | the ball itself never hits; it sends a bolt every 0.2 s at what is within its radius (18). The ball is flown: it travels 10 m at 4 m a second towards where it was aimed, and each discharge hits every enemy within the radius of where the ball is at that moment (a boss 3 m away is in reach for 1.4 s, seven bolts; one standing further off or beside the path gets fewer). The 10 m of flight is a guess. A fire-infused cast is counted as its one fire explosion (the Fire-Infused stat set); the bolts of an infused ball, projectile speed modifiers and the infused ball slowing down are not modelled. PoB counts one hit per cast | `ball_lightning_base_hit_frequency_ms`; game table Projectiles (BallLightningPlayer 400), read as 40 units per second | data / guess |
| Living Bomb | a cast plants a seed in the target and deals nothing. The seed explodes (the skill's hit, and its Fire Infusion remnant) once that enemy has taken 200% of its ailment threshold in damage from any source since the planting, or dies (the blast then strikes the enemy aimed at); after its duration it is gone without exploding. One seed at a time: it is not cast again while one is planted. PoB counts an explosion per cast | `living_bomb_damage_threshold_%_of_ailment_threshold`, `living_bomb_base_number_of_bombs_allowed`; that it fizzles when its time is up is a reading of the description | data / guess |
| Barrage | using it consumes all Frenzy Charges and readies 2 repeats plus one per charge for 5 s. The next Repeatable bow or spear attack (skill type Barrageable) fires once more per repeat within the same use, the repeats dealing 50% less; the use takes 5% longer per repeat. Barrage's cooldown (2 s) grows by 410% of that attack's time, to at most 30 s. In the automatic play style it is used whenever ready if the main skill can repeat. PoB multiplies every attack by the repeats, with no time and no cooldown | `empower_barrage_*` gem stats; keyword text "Repeating Skills" ("in quick succession from a single use", "5% longer to perform for each time they Repeat") | data |
| Support or skill by name | a gem name shared by a support and a skill (Unleash) is the support when it sits in a support socket | gems data | data |
| Ignored resistances | "Hits ignore non-negative Elemental Resistances of Frozen Enemies", "Critical Hits ignore non-negative Enemy Monster Elemental Resistances", "Critical Hits Ignore Enemy Monster Lightning Resistance": a positive resistance counts as none for such a hit; a negative one stays | stat descriptions | data |
| Open Weakness | "Reveal Weaknesses against Rare and Unique enemies": revealed weaknesses cover 45% of the enemy's life in random segments, and it has an Open Weakness while its life is inside one. A hit finds the weakness open with the share of the fight spent there, (c / m) / (1 - c + c / m) for coverage c and damage multiplier m against an open weakness (35% at 50% more). "Skill Speed while an enemy with an Open Weakness is in your Presence" is not applied | poe2db keyword text; the per-hit share is our way to put it into a fight against an enemy whose life does not run out | data |
| Per block chance, per lowest attribute | "N% increased Damage per 1% Chance to Block", "per 5 of your lowest Attribute": the sheet's own numbers as stack counts. The block chance counted is the one before "Chance to Block is Lucky" (54%, where the game's sheet shows the lucky 79%): PoB counts it that way; whether the game counts the displayed value is not known | stat descriptions; PoB for the lucky question | guess |
| While moving | "while moving" bonuses hold when the build has any and every skill it keeps using can be used while moving: a character that gains from moving moves | user | guess |
| Which weapon set is in hand | the character sheet (pools, defences, set passives, the shield) is that of the set whose weapons are stronger (damage x attacks per second); set one when neither deals damage. The API does not say which set is in hand | - | guess |
| Parts that need something | A part of a skill only happens when what its description asks for is there (table `partConditions` in Gems.fs): an enemy state, consumed where the skill consumes it (Siphoning Strike's shockwave: Shocked; Shockburst Rounds' pulse and Shockchain Arrow's shockwave: a lightning ailment; Boneshatter's and Wing Blast's shockwave: the heavy stun; Blood Hunt's explosion: Bleeding; Snipe's icy blast: Frozen; Icestorm's improved bolts: Chilled or Frozen; Disengage, Fangs of Frost: Parried; Gas Arrow / Gas Grenade explosion: an Ignited enemy); a charge to consume (Glacial Lance, Spear Throw explosions); an infusion, consumed (Firestorm's ice and lightning bolts, Incinerate's ignited ground); culling range (Lightning Warp, His Vile Intrusion, Detonate Living, Power Siphon do nothing above it) | skill descriptions: the data has no flag for these | data |
| Detonation | parts that wait for a detonator (Stormblast Bolts, Rapid Assault and Storm Lance explosions) or a warcry (Forge Hammer's fissures): each use puts one down, up to 20; the next use of a skill of that kind sets all of them off | skill descriptions | data |
| Corpses | a skill that works on a corpse (Detonate Dead, Profane Ritual, Volatile Dead, Devour ...) needs one: every kill leaves a corpse and each use takes one. Against a lone boss there are none | skill type `TargetsDestructibleCorpses`; PoB assumes a corpse | data |
| Attacks with their own damage | thrown flasks, runes, a herald's explosion ("main_hand_weapon_minimum_fire_damage"): those numbers stand in for the weapon's, with the skill's own attack time and critical hit chance | gem data | data |
| Sets that deal no damage | a stat set marked "deal no damage" beside others (Rolling Slam's and Conductive Runes' first set) does not hit; the set the gem shows its damage for does | gem data (`base_deal_no_damage`, `dps_display_statset_index`) | data |
| Totems | a totem uses its skill with the character's modifiers and the character's own numbers ("per 100 maximum Mana", "per 15 Strength", socketed support colours) | - | guess |
| Flames of Chayula | Into the Breach: a flame appears every `breach_flame_spawn_rate_ms`; one in three is purple (all with "can only spawn purple"), and a purple one gives `breach_flame_chaos_addition_%` x remnant effect of damage as extra Chaos for 8 s, each flame running out on its own, ten at most; "Effect and Duration of Flames of Chayula on You is Doubled" doubles both. Every flame is taken to be picked up at once | skill data; 8 s and the colours from poe2db patch notes; cap of ten from PoB; pick-up is a guess | guess |
| Low life | the fight starts at 35% life when the build has damage that needs low life (a modifier or a support gem) and something keeps it there: "Your Life cannot change while you have Energy Shield" (life then never rises above where it started), or nothing recovers life (no regeneration, flask, leech, on hit or kill). `LowLife` / `--low-life` forces it | user: low life must be justified by the build | guess |
| Surrounded | 5 enemies within 3 m, fewer with "Require N fewer enemies to be Surrounded", at least one | stat description; user observation | data |
| Defences from one item | "increased Armour, Evasion and Energy Shield from Equipped Shield", "increased Armour / Evasion / Energy Shield from Equipped Body Armour / Focus / Helmet", "50% more Armour from Equipped Body Armour": only for what that item shows, additive with the global increases there | stat descriptions; PoB CalcDefence slot configuration | data |
| Evasion gained as Armour | "Gain N% of Evasion Rating as extra Armour" and Iron Reflexes (all of it): N% of the evasion the items show plus flat evasion is added to armour before increases, so armour's increases and multipliers apply to it, evasion's do not | PoB CalcDefence (resource conversion on base values); matches its number on a corpus character to the point | pob |
| Body Armour grants | Smith of Kitava's "Body Armour grants X": X as a modifier of the character. "+200 to Armour for each Connected Notable Passive Skill Allocated": the allocated notables joined to that passive on the tree | passive text and tree connections | data |
| The Adorned | "N% increased Effect of Jewel Socket Passive Skills containing Corrupted Magic Jewels": the modifiers of every corrupted magic jewel are multiplied, cut off to whole numbers (24% at 146% is 59%) | item text; same numbers as PoB | data |
| Grand Spectrum | "... per socketed Grand Spectrum" (life, elemental resistances, Spirit, any other stat with that tail): once per Grand Spectrum socketed | stat ids `*_per_stackable_unique_jewel` | data |
| Evasion by slot | "Evasion Rating from Equipped Helmet, Gloves and Boots is doubled", "from Equipped Body Armour is halved": applied to what those items show, before increases | stat descriptions | data |
| Per Spirit | "+2 to Armour per 1 Spirit", "increased Attack Speed per 20 Spirit": the character's total Spirit is a stack count "Spirit" | stat ids | data |
| Resistances per Power Charge | "-N% to all Elemental Resistances per Power Charge": per charge held; the sheet is shown with maximum charges | stat description | data |
| Skills an item triggers | "Hits with this Weapon have N% chance to Trigger X per M Strength": every hit of an attack the player uses (not a triggered one) rolls N% x floor(Str / M); X is built from the item's granted gem and can't be pressed. Skills of type Triggered are never pressed | item text, skill types | data; "with this weapon" is taken as any attack (guess) |
| Extra charges | "N% chance to gain an additional Power / Frenzy / Endurance Charge": each gain rolls once for one more. "Gain a Power Charge when you consume an Elemental Infusion". "if you've gained a Power Charge Recently": 4 s after a gain | stat descriptions | data |
| Herald of Blood, Plague, the Royal Queen | not modelled | - | missing |

### Skills granted by supports

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Impending Doom | "trigger an explosion when the curse ends": casting the curse on an enemy that already has it ends the old one and triggers Doom Blast (the support's granted skill at the curse gem's level, with the group's supports). A curse running out by itself is not modelled | gem data `trigger_vicious_hex_explosion_when_curse_ends`, additionalGrantedEffectId1 | data; "recast ends the curse" is game knowledge |
| Other support-granted skills (Doedre's Undoing, ...) | not modelled | - | missing |

### Curse auras

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Blasphemy | the curses socketed with it are on every enemy for the whole fight, at the gem's less curse effect and the enemy's own curse effect; the aura's area and the curse limit are not checked | gem data `curse_apply_as_aura`, `support_blasphemy_curse_effect_+%_final` | data; area ignored |

## Resources over time

| Mechanic | Rule | Source | Confidence |
|---|---|---|---|
| Charges, rage, motes | stacks on the player with a maximum each (charges: 3 + max charge stats; rage 30 + max rage; motes below) | data | data |
| Rage | 1% more attack damage per rage (x rage effect). Gained on attack / melee hits (once per use, however many enemies it hits), on kills and when hit, and regenerated (Eternal Rage). Lost at 1 per 0.2 s; the loss pauses for 4 s on gaining rage or taking damage (decay and delay modifiers apply). A fight starts at maximum rage if anything on the character or its skills generates rage, else at none | game keyword text, GameConstants; "once per use" is a guess; wiki Rage: "Only one Hit every 0.5 seconds can cause you to gain Rage"; game constants for maximum, loss and delay | data |
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
| Monster power | normal 1, magic 2, rare 5 (x the monster's own base power, about 1); unique always 20 | game keyword text | data |
| Energy per trigger | 100 per second of base cast time of the socketed spells (cast time + `total_cast_time_+_ms`); Comet: 1 + 1 s = 200 | "10 per 0.1 s" (community); a guide quotes 300 for Comet (older version?) | community |
| Release | "Cast on ...": trigger all socketed spells on reaching maximum (= the cost), energy spent. Invocations (Elemental Invocation, Spellslinger): bank up to `generic_ongoing_trigger_maximum_energy`; used as soon as a trigger is covered and its cooldown (0.2 s) is ready, triggering as many times as the energy covers | gem descriptions | data |
| Trigger cost of fixed added cast time | `total_cast_time_+_ms` counts double: Comet (1 s base, +1 s fixed) costs 300 energy, not 200 | game keyword text ("modifiers to Total use time are treated as though they were double the value"); user | data |
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
| Energy from melee hits / kills | Fire Spell on Hit: per monster power for each melee hit, scaled like crits; Reaper's Invocation: per monster power for each melee kill | gem stats; the scaling is taken to be the same as for crits | guess |
| Energy from dodge rolls | Cast on Dodge: energy per unit travelled. The dodge roll is something the player presses (`Setup.dodgeRoll`): 4.6 m in 0.75 s (not shortened by speed) plus "Dodge Roll distance" | gem stat; game files (skill DodgeRollPlayer: `dodge_roll_base_travel_distance` 46, cast time 750 ms) | data |
| Energy from minion deaths | not modelled (Cast on Minion Death never triggers) | - | missing |
| Unaffordable skills | a skill that costs more than the whole mana pool is never used | - | data |

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
