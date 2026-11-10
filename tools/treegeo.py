#!/usr/bin/env python3
"""Add passive tree geometry to data/game/tree.json: node positions ("x", "y") and the
order of jewel sockets ("jewelSlots": the character API's jewel index -> node id).
Source: the same PoB-PoE2 tree export the nodes come from.
    python3 tools/treegeo.py <PoB src>/TreeData/<version>/tree.json data/game/tree.json
Positions follow the game's layout: group centre + orbit radius at the orbit angle."""
import json, math, sys

src, dst = sys.argv[1], sys.argv[2]
pob = json.load(open(src))
ours = json.load(open(dst))
const = pob["constants"]
radii, angles = const["orbitRadii"], const["orbitAnglesByOrbit"]
groups = pob["groups"]
group = (lambda g: groups[str(g)] if isinstance(groups, dict) else groups[g])
placed = 0
for nid, node in ours["nodes"].items():
    g, orbit, idx = node.get("group"), node.get("orbit"), node.get("orbitIndex")
    if g is None or orbit is None or idx is None:
        continue
    try:
        centre = group(g)
        a = angles[orbit][idx]
    except (KeyError, IndexError, TypeError):
        continue
    if centre is None:
        continue
    node["x"] = round(centre["x"] + math.sin(a) * radii[orbit], 1)
    node["y"] = round(centre["y"] - math.cos(a) * radii[orbit], 1)
    placed += 1
ours["jewelSlots"] = pob["jewelSlots"]
json.dump(ours, open(dst, "w"), separators=(",", ":"))
print(f"placed {placed} of {len(ours['nodes'])} nodes, {len(ours['jewelSlots'])} jewel sockets")
