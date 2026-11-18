#!/usr/bin/env python3
"""Rewrite the class list of the web front end (web/App.fs, between the markers
`// <classes>` and `// </classes>`) from the tree data, so a new class or ascendancy
appears in "new build" without anyone typing it.
Usage: gen_web_classes.py [data dir, default data/game] [--check]
"""
import json, os, sys

root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
args = [a for a in sys.argv[1:] if not a.startswith("--")]
data = args[0] if args else os.path.join(root, "data", "game")
tree = json.load(open(os.path.join(data, "tree.json")))
classes = sorted(tree["classes"], key=lambda c: c.get("integerId", 0))
rows = []
for c in classes:
    names = [a["name"] for a in c.get("ascendancies", []) if a.get("name")]
    if c.get("name") and names:
        rows.append('"%s", [ %s ]' % (c["name"], "; ".join('"%s"' % n for n in names)))
block = "let private classes =\n    [ " + "\n      ".join(rows) + " ]\n"

path = os.path.join(root, "web", "App.fs")
src = open(path).read()
a, b = "// <classes>\n", "// </classes>\n"
i, j = src.index(a) + len(a), src.index(b)
if src[i:j] == block:
    print("web classes: up to date (%d classes)" % len(rows))
elif "--check" in sys.argv:
    print("web classes: OUT OF DATE, run tools/gamefiles/gen_web_classes.py")
    sys.exit(1)
else:
    open(path, "w").write(src[:i] + block + src[j:])
    print("web classes: rewritten (%d classes)" % len(rows))
