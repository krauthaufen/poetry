#!/usr/bin/env python3
"""Build a corpus of public Path of Exile 2 characters for importer testing.

Source: poe.ninja PoE2 builds (undocumented API behind https://poe.ninja/poe2/builds).
Public endpoints only, no credentials, python3 stdlib only, <= 1 request/second.

Endpoints used
--------------
  GET https://poe.ninja/poe2/api/data/index-state
      JSON. `snapshotVersions[]` gives, per league: `url` (e.g. "forbiddenrites"),
      `version` (changes with every snapshot) and `snapshotName` (e.g. "forbidden-rites").
  GET https://poe.ninja/poe2/api/builds/<version>/search?overview=<snapshotName>&type=exp[&class=<Class>][&skills=<Gem>]
      protobuf (decoded by hand below). Top 100 ladder rows as columns (name, account,
      class, level, dps.skill = main skill, ...). Enum columns hold indexes into dictionaries.
  GET https://poe.ninja/poe2/api/builds/dictionary/<hash>
      "NDIC" binary string table (class names, gem names), hash comes from the search reply.
  GET https://poe.ninja/poe2/api/builds/<version>/character?account=<a>&name=<n>&overview=<snapshotName>&type=exp
      JSON, one character. Items/gems are the official API item objects wrapped in
      {"itemData": ..., "itemSlot": n}.

Output
------
  corpus/<n>-<class>-<mainskill>.json   converted to the official `{"character": {...}}` shape
  corpus/raw/<same name>.json           untouched poe.ninja reply (use --reconvert to redo the
                                        conversion offline after changing this script)
  corpus/index.json                     manifest (also makes reruns resume instead of refetch)

Conversion poe.ninja -> official character API  (everything renamed / synthesized / missing)
-------------------------------------------------------------------------------------------
  character.name/class/league/level   copied. `class` is the ascendancy name when ascended,
                                      like the official API.
  character.realm                     constant "poe2" (not in the source).
  character.id, character.experience  NOT AVAILABLE, omitted.
  character.equipment                 items[].itemData + flasks[].itemData (flasks and charms
                                      are a separate list at poe.ninja; both carry
                                      inventoryId "Flask"). `itemSlot` wrapper dropped.
  character.jewels                    jewels[].itemData.
  character.skills                    skills[].allGems[0].itemData, i.e. the skill gem of each
                                      group; its supports are already nested in `socketedItems`
                                      exactly like the official API. allGems[1..] repeat those
                                      supports and are dropped. poe.ninja also lists
                                      item-granted skills (no inventoryId) as groups; kept.
  item mods                           poe.ninja uses the old flat layout: string lists
                                      explicitMods / implicitMods plus separate fracturedMods,
                                      craftedMods, desecratedMods. Converted to the current
                                      official layout: implicitMods and explicitMods become
                                      [{"description": s}], and fractured/crafted/desecrated
                                      lines are appended to explicitMods with
                                      {"flags": {"fractured"|"crafted"|"desecrated": true}};
                                      the three separate lists are removed. The ORDER of
                                      flagged lines relative to plain explicit lines is not
                                      the official one (official interleaves, here they come
                                      last). enchantMods, runeMods, utilityMods, bondedMods,
                                      mutatedMods stay string lists (as in the official API).
                                      Applied recursively to socketedItems.
  item extras                         poe.ninja-only fields are left in place: `mods`
                                      (structured stat ids), `sanctified`, `synthesised`,
                                      `duplicated`, `replica`, `qualityProperty`,
                                      `additionalProperties`, `scourgeMods`, `crucibleMods`,
                                      and empty lists/false flags where the official API
                                      omits the key entirely.
  passives.hashes                     passiveSelection.
  passives.specialisations            {"set1": passiveSelectionSet1, "set2": passiveSelectionSet2}
                                      ({} when both are empty). Key names follow Path of
                                      Building's importer; not verified against a live
                                      official reply with weapon-set passives.
  passives.skill_overrides            poe.ninja only gives {hash: "<override id>"}. For the
                                      attribute nodes (generic_attribute_strength|dexterity|
                                      intelligence_) the official object is synthesized:
                                      {"id", "name", "grantedStrength": 5, "stats": ["+5 to [Strength]"]}
                                      (no "icon"). Any other id becomes {"id": ..., "stats": []}
                                      and is counted in index.json under "unknown_overrides".
  passives.jewel_data                 NOT AVAILABLE, {} (official: socket -> {type, radius}).
  passives.quest_stats                questStats.
  metadata                            {"version": passiveTreeName, "source": "poe.ninja"};
                                      the official version string has a different format.
  top-level "ninja"                   sibling of "character", poe.ninja-only data that is handy
                                      as a reference: account, defensiveStats, keystones,
                                      per-skill dps, passiveCounts, baseClass,
                                      useSecondWeaponSet, enableBondedMods, timestamps,
                                      pathOfBuildingExport. `breakdowns` is dropped (raw only).
"""
import argparse
import json
import math
import os
import re
import struct
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

BASE = "https://poe.ninja/poe2/api"
UA = "build-corpus-fetch/0.1"
MIN_INTERVAL = 1.1  # seconds between requests
BASE_CLASSES = {"Warrior", "Witch", "Ranger", "Sorceress", "Mercenary", "Monk", "Huntress", "Druid"}

_last = [0.0]


class Blocked(Exception):
    pass


def get(path, params=None):
    url = BASE + path
    if params:
        url += "?" + urllib.parse.urlencode(params)
    delay = 5.0
    for attempt in range(6):
        wait = _last[0] + MIN_INTERVAL - time.time()
        if wait > 0:
            time.sleep(wait)
        _last[0] = time.time()
        req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "*/*"})
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                return r.read()
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None
            if e.code in (401, 403):
                raise Blocked("HTTP %d for %s" % (e.code, path))
            if e.code == 429 or e.code >= 500:
                ra = e.headers.get("Retry-After")
                pause = float(ra) if ra and ra.isdigit() else delay
                print("  HTTP %d, backing off %.0fs" % (e.code, pause), file=sys.stderr)
                time.sleep(pause)
                delay *= 2
                continue
            raise
        except (urllib.error.URLError, TimeoutError) as e:
            print("  network error %s, retry in %.0fs" % (e, delay), file=sys.stderr)
            time.sleep(delay)
            delay *= 2
    raise Blocked("giving up on %s after repeated 429/5xx" % path)


# ---- minimal protobuf reader -------------------------------------------------

def _varint(b, i):
    r = s = 0
    while True:
        c = b[i]
        i += 1
        r |= (c & 0x7F) << s
        s += 7
        if c < 0x80:
            return r, i


def _fields(b):
    i = 0
    out = []
    while i < len(b):
        k, i = _varint(b, i)
        f, w = k >> 3, k & 7
        if w == 0:
            v, i = _varint(b, i)
        elif w == 2:
            n, i = _varint(b, i)
            v = b[i:i + n]
            i += n
        elif w == 1:
            v = b[i:i + 8]
            i += 8
        elif w == 5:
            v = b[i:i + 4]
            i += 4
        else:
            raise ValueError("wire type %d" % w)
        out.append((f, v))
    return out


def _packed(b):
    i = 0
    out = []
    while i < len(b):
        v, i = _varint(b, i)
        out.append(v)
    return out


def parse_search(b):
    """-> (total, {column: [values]}, {dictionary id: [hashes]}).
    reply.1 = result; result.1 = total, result.6 = dictionary refs (1=id, 2..=hashes),
    result.12 = column (1=name, 6=packed ints, 7=repeated strings, 9=repeated int lists,
    11=dictionary id)."""
    top = _fields(_fields(b)[0][1])
    total = next((v for f, v in top if f == 1), 0)
    cols, coldict, dicts = {}, {}, {}
    for f, v in top:
        if f == 6:
            parts = [x.decode() for _, x in _fields(v)]
            dicts[parts[0]] = parts[1:]
        elif f == 12:
            d = {}
            for ff, vv in _fields(v):
                d.setdefault(ff, []).append(vv)
            name = d[1][0].decode()
            if 7 in d:
                cols[name] = [x.decode("utf-8", "replace") for x in d[7]]
            elif 6 in d:
                cols[name] = _packed(d[6][0])
            elif 9 in d:
                cols[name] = [[x for _, pk in _fields(row) for x in _packed(pk)] for row in d[9]]
            if 11 in d:
                coldict[name] = d[11][0].decode()
    return total, cols, dicts


def parse_ndic(b):
    """'NDIC' string table: magic, u32 x8 (ver, 0, count, hash lo/hi, block size, blocks, count),
    blocks x (u32 string offset, u32 length offset), count x u8 lengths, strings."""
    if b[:4] != b"NDIC":
        raise ValueError("not an NDIC dictionary")
    hdr = struct.unpack("<8I", b[4:36])
    count, blocks = hdr[2], hdr[6]
    off = 36 + blocks * 8
    lens = b[off:off + count]
    off += count
    out = []
    for n in lens:
        out.append(b[off:off + n].decode("utf-8", "replace"))
        off += n
    return out


# ---- conversion --------------------------------------------------------------

ATTR = {"strength": "Strength", "dexterity": "Dexterity", "intelligence": "Intelligence"}


def conv_item(it):
    it = dict(it)
    expl = [m if isinstance(m, dict) else {"description": m} for m in it.get("explicitMods") or []]
    seen = {m.get("description"): m for m in expl}
    for key, flag in (("fracturedMods", "fractured"), ("craftedMods", "crafted"), ("desecratedMods", "desecrated")):
        for m in it.pop(key, None) or []:
            if isinstance(m, dict):
                expl.append(m)
            elif m in seen:
                seen[m].setdefault("flags", {})[flag] = True
            else:
                expl.append({"description": m, "flags": {flag: True}})
    it["explicitMods"] = expl
    it["implicitMods"] = [m if isinstance(m, dict) else {"description": m} for m in it.get("implicitMods") or []]
    if it.get("socketedItems"):
        it["socketedItems"] = [conv_item(s) for s in it["socketedItems"]]
    return it


def conv_override(oid):
    m = re.match(r"generic_attribute_(strength|dexterity|intelligence)_?$", oid or "")
    if m:
        name = ATTR[m.group(1)]
        return {"id": oid, "name": name, "granted" + name: 5, "stats": ["+5 to [%s]" % name]}, True
    return {"id": oid, "stats": []}, False


def convert(raw):
    """poe.ninja character reply -> ({"character": ..., "ninja": ...}, unknown override ids)."""
    unknown = []
    overrides = {}
    for h, oid in (raw.get("passiveSkillOverrides") or {}).items():
        overrides[str(h)], ok = conv_override(oid)
        if not ok:
            unknown.append(oid)
    s1, s2 = raw.get("passiveSelectionSet1") or [], raw.get("passiveSelectionSet2") or []
    skills = []
    for grp in raw.get("skills") or []:
        gems = grp.get("allGems") or []
        if gems and gems[0].get("itemData"):
            skills.append(conv_item(gems[0]["itemData"]))
    wrap = lambda key: [conv_item(x["itemData"]) for x in raw.get(key) or [] if x.get("itemData")]
    character = {
        "name": raw.get("name"),
        "realm": "poe2",
        "class": raw.get("class"),
        "league": raw.get("league"),
        "level": raw.get("level"),
        "equipment": wrap("items") + wrap("flasks"),
        "skills": skills,
        "jewels": wrap("jewels"),
        "passives": {
            "hashes": raw.get("passiveSelection") or [],
            "specialisations": {"set1": s1, "set2": s2} if (s1 or s2) else {},
            "skill_overrides": overrides,
            "jewel_data": {},
            "quest_stats": raw.get("questStats") or [],
        },
        "metadata": {"version": raw.get("passiveTreeName"), "source": "poe.ninja"},
    }
    ninja = {k: raw.get(k) for k in (
        "account", "defensiveStats", "keystones", "passiveCounts", "baseClass", "useSecondWeaponSet",
        "enableBondedMods", "lastSeenUtc", "updatedUtc", "lastCheckedUtc", "pathOfBuildingExport")}
    ninja["skillDps"] = [d for grp in raw.get("skills") or [] for d in grp.get("dps") or []]
    return {"character": character, "ninja": ninja}, unknown


def main_skill(raw, fallback):
    best = None
    for grp in raw.get("skills") or []:
        for d in grp.get("dps") or []:
            v = max(d.get("dps") or 0, d.get("dotDps") or 0)
            if d.get("name") and (best is None or v > best[0]):
                best = (v, d["name"])
    return best[1] if best else (fallback or "unknown")


def slug(s):
    return re.sub(r"[^a-z0-9]+", "-", (s or "unknown").lower()).strip("-") or "unknown"


# ---- sampling ----------------------------------------------------------------

def plan(rows_by_class, target, min_level):
    """Pick per class a quota of characters, each with a different main skill where possible,
    preferring skills that are still rare in the selection as a whole."""
    classes = sorted(rows_by_class)
    nbase = sum(1 for c in classes if c in BASE_CLASSES)
    nasc = max(1, len(classes) - nbase)
    base_quota = 2
    asc_quota = max(1, math.ceil((target - base_quota * nbase) / nasc))
    used = {}
    picks = []
    for rnd in range(asc_quota):
        for c in classes:
            if rnd >= (base_quota if c in BASE_CLASSES else asc_quota):
                continue
            rows = rows_by_class[c]
            taken = {(p["account"], p["name"]) for p in picks}
            mine = {p["skill"] for p in picks if p["class"] == c}
            cand = [r for r in rows if (r["account"], r["name"]) not in taken]
            if not cand:
                continue
            # new skill for this class first, then globally rare skill, then level, then ladder rank
            cand.sort(key=lambda r: (r["skill"] in mine, not r["skill"], r["level"] < min_level,
                                     used.get(r["skill"], 0), -r["level"], r["rank"]))
            r = cand[0]
            used[r["skill"]] = used.get(r["skill"], 0) + 1
            picks.append(r)
    return picks


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--out", default=os.path.normpath(os.path.join(here, "..", "..", "corpus")))
    ap.add_argument("--league", default="forbiddenrites", help="league url from index-state (default: %(default)s)")
    ap.add_argument("--target", type=int, default=150, help="approximate number of characters")
    ap.add_argument("--min-level", type=int, default=90)
    ap.add_argument("--plan-only", action="store_true", help="print the selection, download nothing")
    ap.add_argument("--reconvert", action="store_true", help="redo the conversion from corpus/raw, no network")
    a = ap.parse_args()

    rawdir = os.path.join(a.out, "raw")
    os.makedirs(rawdir, exist_ok=True)
    index_path = os.path.join(a.out, "index.json")
    index = json.load(open(index_path)) if os.path.exists(index_path) else {"characters": []}

    def save_index():
        with open(index_path, "w") as f:
            json.dump(index, f, indent=1)

    def write(fname, raw):
        doc, unknown = convert(raw)
        with open(os.path.join(a.out, fname), "w") as f:
            json.dump(doc, f, indent=1)
        return unknown

    if a.reconvert:
        for e in index["characters"]:
            e["unknown_overrides"] = sorted(set(write(e["file"], json.load(open(os.path.join(rawdir, e["file"]))))))
        save_index()
        print("reconverted %d characters" % len(index["characters"]))
        return 0

    try:
        state = json.loads(get("/data/index-state"))
        snap = next((s for s in state["snapshotVersions"] if s["url"] == a.league), None)
        if not snap:
            print("league %r not found; available: %s" % (a.league, ", ".join(s["url"] for s in state["snapshotVersions"][:12])))
            return 2
        ver, overview = snap["version"], snap["snapshotName"]
        base = {"overview": overview, "type": "exp"}
        total, cols, dicts = parse_search(get("/builds/%s/search" % ver, base))
        classes = parse_ndic(get("/builds/dictionary/" + dicts["class"][0]))
        gems = parse_ndic(get("/builds/dictionary/" + dicts["gem"][0]))
        print("league %s, snapshot %s, %d characters indexed, %d classes" % (snap["name"], ver, total, len(classes)))

        rows_by_class = {}
        for c in classes:
            t, cols, _ = parse_search(get("/builds/%s/search" % ver, dict(base, **{"class": c})))
            n = len(cols.get("name", []))
            sk = cols.get("dps.skill") or [None] * n
            lv = cols.get("level") or [0] * n
            rows = []
            for i in range(n):
                g = sk[i]
                rows.append({"account": cols["account"][i], "name": cols["name"][i], "class": c, "level": lv[i],
                             "skill": gems[g] if isinstance(g, int) and 0 <= g < len(gems) and cols.get("dps.total", [""] * n)[i] else "",
                             "rank": i})
            rows_by_class[c] = rows
            print("  %-22s %6d on ladder, %3d rows, %2d distinct main skills" % (c, t, n, len({r["skill"] for r in rows})))

        picks = plan(rows_by_class, a.target, a.min_level)
        print("selected %d characters" % len(picks))
        if a.plan_only:
            for p in picks:
                print("  %-22s L%-3d %s" % (p["class"], p["level"], p["skill"] or "?"))
            return 0

        done = {(e["account"], e["name"]) for e in index["characters"]}
        n = max([int(e["file"].split("-")[0]) for e in index["characters"]] + [0])
        for p in picks:
            if (p["account"], p["name"]) in done:
                continue
            body = get("/builds/%s/character" % ver, dict(base, account=p["account"], name=p["name"]))
            if body is None:
                print("  404 %s (private or gone), skipped" % p["class"])
                continue
            raw = json.loads(body)
            if not raw.get("items") and not raw.get("skills"):
                print("  empty character, skipped")
                continue
            n += 1
            skill = main_skill(raw, p["skill"])
            fname = "%03d-%s-%s.json" % (n, slug(raw.get("class") or p["class"]), slug(skill))
            with open(os.path.join(rawdir, fname), "wb") as f:
                f.write(body)
            unknown = write(fname, raw)
            index["characters"].append({"file": fname, "account": p["account"], "name": p["name"],
                                        "class": raw.get("class"), "level": raw.get("level"), "skill": skill,
                                        "league": raw.get("league"), "snapshot": ver,
                                        "unknown_overrides": sorted(set(unknown))})
            save_index()
            print("  %s" % fname)
    except Blocked as e:
        print("STOPPED: %s" % e, file=sys.stderr)
        save_index()
        return 1
    print("corpus now holds %d characters in %s" % (len(index["characters"]), a.out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
