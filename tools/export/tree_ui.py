#!/usr/bin/env python3
"""The passive tree for the front end, from PoB-PoE2's TreeData/<version>/tree.json (the game's
tree export): web/public/tree.json (nodes with position, kind, name, stats, neighbours, the
classes that start there) and web/public/tree.svg (every connection drawn once, as the game
curves it).

  python3 tools/export/tree_ui.py <PoB src>/TreeData/0_5/tree.json [<pob-web>/packages/web/public/data/sprites/_sprites.json]
"""
import json, math, os, sys

src = json.load(open(sys.argv[1]))
root = os.path.join(os.path.dirname(__file__), "..", "..")
const = src["constants"]
radii, angles = const["orbitRadii"], const["orbitAnglesByOrbit"]
groups = src["groups"]

def group(n):
    g = groups[n["group"] - 1] if isinstance(groups, list) else groups[str(n["group"])]
    return g

# (ornaments are nodes too, "isOnlyImage", and so are nameless ones that hold art together:
# neither they nor their connections are part of the tree, as in PoB and pob-web)
def real(n):
    if n.get("isOnlyImage"): return False
    return bool(n.get("name")) or n.get("isAscendancyStart") or n.get("isJewelSocket") or bool(n.get("classesStart"))

pos = {}
for key, n in src["nodes"].items():
    if "group" not in n or "orbit" not in n or not real(n):
        continue
    g = group(n)
    a = angles[n["orbit"]][n["orbitIndex"]]
    pos[int(key)] = (g["x"] + math.sin(a) * radii[n["orbit"]], g["y"] - math.cos(a) * radii[n["orbit"]])

def kind(n):
    if n.get("classesStart"): return "start"
    if n.get("isAscendancyStart"): return "ascstart"
    if n.get("isJewelSocket"): return "jewel"
    if n.get("isKeystone"): return "keystone"
    if n.get("isNotable"): return "notable"
    if n.get("isAttribute"): return "attribute"
    return "small"

edges = {}
for key, n in src["nodes"].items():
    a = int(key)
    if a not in pos: continue
    for c in n.get("connections", []):
        b = int(c["id"])
        if b in pos and a != b:
            edges[(min(a, b), max(a, b))] = (a, b, c.get("orbit", 0))

neighbours = {}
for (a, b) in edges:
    neighbours.setdefault(a, []).append(b)
    neighbours.setdefault(b, []).append(a)

nodes = []
for key, n in src["nodes"].items():
    i = int(key)
    if i not in pos: continue
    x, y = pos[i]
    node = {"id": i, "x": round(x, 1), "y": round(y, 1), "kind": kind(n), "name": n.get("name", ""), "stats": n.get("stats", []),
            "to": sorted(neighbours.get(i, []))}
    if n.get("ascendancyName"): node["ascendancy"] = n["ascendancyName"]
    if n.get("classesStart"): node["classes"] = n["classesStart"]
    if n.get("options"):
        # an attribute node: what it can be made
        node["options"] = [{"name": o.get("name", ""), "stats": o.get("stats", [])} for o in (n["options"].values() if isinstance(n["options"], dict) else n["options"])]
    nodes.append(node)
nodes.sort(key=lambda n: n["id"])

# art: where each node's icon is in the sprite sheets (pob-web's PNG atlases of the game's
# tree art, with their index _sprites.json), and the frames per kind and state
sprites = json.load(open(sys.argv[2])) if len(sys.argv) > 2 else {}
sheets, sheetIndex = [], {}
def sheet(name):
    if name not in sheetIndex:
        s = next(v for v in sprites.values() if v["sheet"] == name)
        sheetIndex[name] = len(sheets)
        sheets.append({"name": name, "w": s["width"], "h": s["height"]})
    return sheetIndex[name]
def find(sprite, prefer):
    for want in prefer:
        for key, v in sprites.items():
            if key.startswith(want) and sprite in v["sprites"]:
                r = v["sprites"][sprite]
                return [sheet(v["sheet"]), r["x"], r["y"], r["w"], r["h"]]
    return None
if sprites:
    for node in nodes:
        icon = src["nodes"][str(node["id"])].get("icon")
        big = node["kind"] in ("notable", "keystone")
        at = icon and find(icon, ["skills_128", "skills_64", "skills_17"] if big else ["skills_64", "skills_128", "skills_17"])
        if at: node["icon"] = at
    def frame(normal, alloc):
        return {"normal": find(normal, ["group-background"]), "alloc": find(alloc, ["group-background"])}
    frames = {"small": frame("PSSkillFrame", "PSSkillFrameActive"), "attribute": frame("PSSkillFrame", "PSSkillFrameActive"),
              "notable": frame("NotableFrameUnallocated", "NotableFrameAllocated"), "keystone": frame("KeystoneFrameUnallocated", "KeystoneFrameAllocated"),
              "jewel": frame("JewelFrameUnallocated", "JewelFrameAllocated"), "ascstart": frame("AscendancyMiddle", "AscendancyMiddle")}
else:
    frames = {}

xs, ys = [p[0] for p in pos.values()], [p[1] for p in pos.values()]
pad = 600
box = [math.floor(min(xs) - pad), math.floor(min(ys) - pad), math.ceil(max(xs) - min(xs) + 2 * pad), math.ceil(max(ys) - min(ys) + 2 * pad)]

classes = [{"name": c["name"], "ascendancies": [a["name"] for a in c.get("ascendancies", [])]} for c in src["classes"]]
out = os.path.join(root, "web", "public")
os.makedirs(out, exist_ok=True)

# the connections, as the game lays them out (PoB PassiveTree.lua): between two nodes on the
# same orbit of one group an arc around the group's centre; with an orbit of its own an arc of
# that orbit's radius, its centre to one side by the sign; else a straight line. Always the
# shorter way round.
def arc(p1, p2, c, r):
    cross = (p1[0] - c[0]) * (p2[1] - c[1]) - (p1[1] - c[1]) * (p2[0] - c[0])
    return "M%.0f %.0fA%.0f %.0f 0 0 %d %.0f %.0f" % (p1[0], p1[1], r, r, 1 if cross > 0 else 0, p2[0], p2[1])

# the same for the page, which draws the allocated ones itself: [a, b] for a line,
# [a, b, cx, cy, r, from, sweep] for an arc (degrees clockwise from the top, as CSS counts)
def clock(p, c):
    return math.degrees(math.atan2(p[0] - c[0], -(p[1] - c[1]))) % 360

def arcData(a, b, p1, p2, c, r):
    a1, a2 = clock(p1, c), clock(p2, c)
    sweep = (a2 - a1) % 360
    start = a1
    if sweep > 180:
        start, sweep = a2, 360 - sweep
    return [a, b, round(c[0], 1), round(c[1], 1), round(r, 1), round(start, 2), round(sweep, 2)]

drawn = []
paths = []
starts = {int(k) for k, n in src["nodes"].items() if n.get("classesStart")}
for (a, b, orbit) in edges.values():
    # a class' start is the emblem in the middle: its connections are not drawn as lines
    if a in starts or b in starts:
        continue
    # nor is a way from an ascendancy into the tree (the Pathfinder's "Path of the ..."):
    # the ascendancy is drawn far from where it leads
    if src["nodes"][str(a)].get("ascendancyName") != src["nodes"][str(b)].get("ascendancyName"):
        continue
    (x1, y1), (x2, y2) = pos[a], pos[b]
    na, nb = src["nodes"][str(a)], src["nodes"][str(b)]
    dx, dy = x2 - x1, y2 - y1
    d = math.hypot(dx, dy)
    r = radii[abs(orbit)] if orbit != 0 and abs(orbit) < len(radii) else 0
    if r > 0 and 0 < d < 2 * r:
        perp = math.sqrt(r * r - d * d / 4) * (1 if orbit > 0 else -1)
        c = (x1 + dx / 2 + perp * (dy / d), y1 + dy / 2 - perp * (dx / d))
        paths.append(arc((x1, y1), (x2, y2), c, r))
        drawn.append(arcData(a, b, (x1, y1), (x2, y2), c, r))
    elif orbit == 0 and na["group"] == nb["group"] and na["orbit"] == nb["orbit"] and na["orbit"] > 0:
        g = group(na)
        paths.append(arc((x1, y1), (x2, y2), (g["x"], g["y"]), radii[na["orbit"]]))
        drawn.append(arcData(a, b, (x1, y1), (x2, y2), (g["x"], g["y"]), radii[na["orbit"]]))
    else:
        paths.append("M%.0f %.0fL%.0f %.0f" % (x1, y1, x2, y2))
        drawn.append([a, b])
json.dump({"box": box, "classes": classes, "nodes": nodes, "edges": drawn, "sheets": sheets, "frames": frames}, open(os.path.join(out, "tree.json"), "w"), separators=(",", ":"))
svg = ('<svg xmlns="http://www.w3.org/2000/svg" viewBox="%d %d %d %d">'
       '<path d="%s" fill="none" stroke="#4a4136" stroke-width="14" stroke-linecap="round"/></svg>') % (box[0], box[1], box[2], box[3], "".join(paths))
open(os.path.join(out, "tree.svg"), "w").write(svg)
print("%d nodes, %d connections, box %s" % (len(nodes), len(edges), box))
