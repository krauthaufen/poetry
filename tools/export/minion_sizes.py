#!/usr/bin/env python3
"""Add each minion's body size to data/game/minions.json: MonsterVarieties.dat ObjectSize
(tools/gamefiles/tables/English/MonsterVarieties.json), by monster id, else by name.
Run after export.lua:  python3 tools/export/minion_sizes.py
"""
import json, os
root = os.path.join(os.path.dirname(__file__), "..", "..")
varieties = json.load(open(os.path.join(root, "tools/gamefiles/tables/English/MonsterVarieties.json")))
by_id = {r["Id"]: r["ObjectSize"] for r in varieties}
by_tail = {r["Id"].split("/")[-1].rstrip("_"): r["ObjectSize"] for r in varieties}
by_name = {}
for r in varieties:
    by_name.setdefault(r["Name"], r["ObjectSize"])
path = os.path.join(root, "data/game/minions.json")
minions = json.load(open(path))
found = 0
for key, m in minions.items():
    size = by_id.get(key)
    if size is None:
        size = by_name.get(m.get("name"))
    if size is None:
        size = by_tail.get(key.split("/")[-1].rstrip("_"))
    if size is not None:
        m["objectSize"] = size
        found += 1
    else:
        m.pop("objectSize", None)
json.dump(minions, open(path, "w"), separators=(",", ":"))
print("%d of %d minions have a size" % (found, len(minions)))
