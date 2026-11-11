#!/usr/bin/env python3
"""Our fight against PoB's breakdown, skill by skill.

  node tools/corpus/pob-all.mjs > corpus/pob-skills.jsonl      (PoB's side, slow)
  dotnet fsi --optimize+ tools/corpus/run.fsx                   (ours: corpus/ours-skills.jsonl)
  python3 tools/corpus/parity.py [build number]

One line per character whose main skill is the one pressed: uses per second, hits per use,
crit chance and the average hit, ours / PoB's, and the assumptions PoB's number rests on.
With a build number: everything both sides know about that character. PoB is no truth for
damage; the table shows where the two differ so each difference can be explained."""
import json, os, sys

root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "corpus")
load = lambda n: {j["file"]: j for j in (json.loads(l) for l in open(os.path.join(root, n)) if l.strip())}
ours, pob = load("ours-skills.jsonl"), load("pob-skills.jsonl")

# config switches PoB has on that make a number bigger than a fight would
ASSUME = ["conditionLowLife", "multiplierRage", "multiplierWitheredStackCount", "conditionEnemyShocked",
          "conditionEnemyFrozen", "conditionEnemyIgnited", "conditionEnemyChilled", "conditionEnemyBleeding", "conditionEnemyPoisoned",
          "conditionEnemyBlinded", "conditionEnemyDazed", "conditionKilledRecently", "conditionCritRecently", "conditionBeenHitRecently",
          "usePowerCharges", "useFrenzyCharges", "useEnduranceCharges", "conditionOnFullLife", "conditionEnemyIntimidated",
          "multiplierPoisonOnEnemy", "conditionFullEnergyShield", "conditionUsingFlask", "buffOnslaught", "conditionEnemyArmourBroken",
          "enemyIsBoss", "conditionEnemyHeavyStunned", "conditionEnemyOnLowLife", "conditionSurrounded", "multiplierNearbyEnemies"]

def side(f):
    o, p = ours.get(f), (pob.get(f) or {}).get("pob") or {}
    if not o: return None
    c, d, pr = o["counts"], o["damage"], o["pressed"]
    # the skill itself, its infused form and its parts ("X infused", "X: Explosion")
    own = lambda k: k == pr or k.startswith(pr + " infused") or k.startswith(pr + ": ")
    tot = lambda prefix: sum(v for k, v in c.items() if k.startswith(prefix) and own(k[len(prefix):]))
    uses, hits, crits = c.get("uses:" + pr, 0), tot("hits:"), tot("crits:")
    hit_dmg = sum(v for k, v in d.items() if k.endswith(" (hit)") and own(k[:-6]))
    total = sum(d.values())
    dot = sum(v for k, v in d.items() if "(hit)" not in k)
    h = dict(p.get("out") or {}); h.update(p.get("hand") or {})
    return dict(o=o, p=p, h=h, uses=uses / o["duration"], hpu=hits / uses if uses else 0, crit=100.0 * crits / hits if hits else 0,
                avg=hit_dmg / hits if hits else 0, dps=total / o["duration"], hitdps=hit_dmg / o["duration"], dot=dot / o["duration"])

def ratio(a, b): return "%6.2f" % (a / b) if b else "     -"

if len(sys.argv) > 1:
    f = next(k for k in ours if k.startswith(sys.argv[1]))
    s = side(f)
    print(f, "| pressed", s["o"]["pressed"], "| play:", s["o"]["play"], "| starts with", s["o"]["startsWith"])
    print("ours: %.2f uses/s, %.2f hits/use, crit %.1f%%, average hit %.0f, hit dps %.0f, other dps %.0f, total %.0f" % (s["uses"], s["hpu"], s["crit"], s["avg"], s["hitdps"], s["dot"], s["dps"]))
    for k, v in sorted(s["o"]["damage"].items(), key=lambda kv: -kv[1]): print("   %-52s %14.0f" % (k, v / s["o"]["duration"]))
    for k, v in sorted(s["o"]["counts"].items()): print("   %-52s %8d" % (k, v))
    print("pob:", s["p"].get("active"), s["p"].get("part"), "| gems:", " + ".join(s["p"].get("gems") or []))
    for k, v in sorted(s["h"].items()): print("   %-36s %s" % (k, round(v, 3)))
    if s["p"].get("minion"):
        for k, v in sorted(s["p"]["minion"].items()): print("   minion %-29s %s" % (k, round(v, 3)))
    print("pob config:", {k: v for k, v in (s["p"].get("config") or {}).items() if k in ASSUME or k.startswith("condition") or k.startswith("multiplier") or k.startswith("use")})
    sys.exit(0)

rows = []
for f in sorted(ours):
    s = side(f)
    if not s or not s["h"]: continue
    if s["o"]["pressed"] != s["o"]["main"]: continue
    h, cfg = s["h"], s["p"].get("config") or {}
    their_hit = h.get("AverageHit") or h.get("AverageDamage") or 0
    their_dps = h.get("TotalDPS", 0)
    rows.append((s["hitdps"] / their_dps if their_dps else 9e9, f[:34], ratio(s["uses"] * s["hpu"], h.get("Speed", 0)), "%5.1f/%5.1f" % (s["crit"], h.get("CritChance", 0)),
                 ratio(s["avg"], their_hit), ratio(s["hitdps"], their_dps), "%9.0f/%9.0f" % (s["dot"], h.get("TotalDotDPS", 0)),
                 " ".join(k.replace("condition", "").replace("multiplier", "") + ("=" + str(cfg[k]) if not isinstance(cfg[k], bool) else "") for k in ASSUME if cfg.get(k) and k not in ("enemyIsBoss", "multiplierNearbyEnemies"))))
rows.sort()
print("%-34s %6s %11s %6s %6s %19s  %s" % ("character (main skill pressed)", "hits/s", "crit o/p", "avghit", "hitdps", "other dps o/p", "PoB assumes"))
for r in rows: print("%-34s %6s %11s %6s %6s %19s  %s" % r[1:])
