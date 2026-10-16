#!/usr/bin/env python3
"""occurrence_square.py — a pattern citation as a blend square, from the 象 side.

CASCADE-SPEC-v3 §2 (futon2, e5c3a4604): a pattern's square is its four written
parts under the assignment IF = I1, HOWEVER = I2, BECAUSE = G, THEN = B.  An
occurrence adds only the four maps: for each part, the situation element it
names, with a source.  A part with no match voids the citation, and the void
names the failed part.  Occurrences that share a situation element are glued
there; a set with no shared element is a bag, not a cascade (§3.1).

  occurrence_square.py parts  <family/name> ...        print the four parts
  occurrence_square.py square <occurrences.json>       emit :wm/three-halves-square-v1 EDN
                              [--out FILE]              (one record per occurrence, plus the cascade)

occurrences.json: [{"turn": "...", "pattern": "family/name", "cited-by": "...",
                    "maps": {"I1": {"element": "...", "source": "..."}, "I2": ..,
                             "G": .., "B": ..}}]
A map whose "element" is null or missing is a void for that part.

The emitted squares use the same schema the War Machine validates with
futon2.aif.three-halves-square/validate: object texts are the pattern's parts
pinned to the pattern file's sha256; the theory of each object is the single
situation element the map names.  Written 2026-10-16 (claude-16).
"""
import hashlib, json, re, sys
from pathlib import Path

LIB = Path("/home/joe/code/futon3/library")
PARTS = ("IF", "HOWEVER", "THEN", "BECAUSE")
ROLE_OF = {"IF": "I1", "HOWEVER": "I2", "BECAUSE": "G", "THEN": "B"}
PART_OF = {v: k for k, v in ROLE_OF.items()}
TOP = re.compile(r"^  \+ ([A-Za-z-]+):(.*)$")  # two-space indent = a top-level part


def read_pattern(pid):
    path = LIB / (pid + ".flexiarg")
    data = path.read_bytes()
    text = data.decode("utf-8")
    parts, cur, buf = {}, None, []
    conclusion = None
    for line in text.split("\n"):
        if line.startswith("! conclusion:"):
            conclusion = line[len("! conclusion:"):].strip()
        m = TOP.match(line)
        if m:
            if cur:
                parts[cur] = "\n".join(buf).strip("\n")
            cur, buf = m.group(1), ([m.group(2).strip()] if m.group(2).strip() else [])
            continue
        if cur is not None:
            if line and not line.startswith("    ") and not line.startswith("  +"):
                # left the indented body (e.g. a blank-separated top-level tag)
                if line.strip():
                    parts[cur] = "\n".join(buf).strip("\n")
                    cur, buf = None, []
                    continue
            buf.append(line)
    if cur:
        parts[cur] = "\n".join(buf).strip("\n")
    check = None
    if "THEN" in parts:
        m = re.search(r"\+ CHECK:\n(.*?)(?=\n\s*\+ [A-Z]|\Z)", parts["THEN"], re.S)
        if m:
            check = m.group(1).strip()
    return {"id": pid, "path": "futon3/library/%s.flexiarg" % pid,
            "sha256": hashlib.sha256(data).hexdigest(),
            "parts": {p: parts.get(p) for p in PARTS},
            "conclusion": conclusion, "check": check}


def norm(s):
    return re.sub(r"\s+", " ", s or "").strip()


def elem_code(name):
    code = re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-")
    return ":" + code[:48]


def edn_str(s):
    return json.dumps(s, ensure_ascii=False)


def square(pat, occ):
    """One occurrence -> (record dict, voids list)."""
    voids = []
    objects, codes = {}, {}
    for part in PARTS:
        role = ROLE_OF[part]
        text = pat["parts"].get(part)
        if not text:
            voids.append({"part": part, "reason": "pattern-part-missing"})
            continue
        m = (occ.get("maps") or {}).get(role) or {}
        if not m.get("element"):
            voids.append({"part": part, "reason": "no-situation-element"})
            continue
        codes[role] = elem_code(m["element"])
        objects[role] = {"text": norm(text), "source": {"path": pat["path"], "sha256": pat["sha256"]},
                         "theory": {"elems": [codes[role]], "names": {codes[role]: m["element"]}},
                         "situation-source": m.get("source")}
    rec = {"pattern": pat["id"], "turn": occ.get("turn"), "cited-by": occ.get("cited-by"),
           "assignment": ROLE_OF}
    if voids:
        rec["status"] = "void"
        rec["void"] = voids
        return rec, voids
    rec["status"] = "occurrence"
    rec["schema"] = ":wm/three-halves-square-v1"
    rec["objects"] = objects
    rec["maps"] = {"a1": [[codes["G"], codes["I1"]]], "a2": [[codes["G"], codes["I2"]]],
                   "b1": [[codes["I1"], codes["B"]]], "b2": [[codes["I2"], codes["B"]]]}
    rec["auxiliary"] = {"b1": False, "b2": False}
    rec["check"] = pat["check"] or pat["conclusion"]
    return rec, voids


def glue(records):
    """Occurrences sharing a situation element (same name, any role) are glued there."""
    occs = [r for r in records if r["status"] == "occurrence"]
    where, shown = {}, {}
    for i, r in enumerate(occs):
        for role, o in r["objects"].items():
            label = norm(list(o["theory"]["names"].values())[0])
            key = label.lower()
            shown.setdefault(key, label)
            where.setdefault(key, []).append((i, role))
    glued, parent = [], list(range(len(occs)))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x
    for name, sites in where.items():
        for (i, ri), (j, rj) in zip(sites, sites[1:]):
            if i != j:
                glued.append({"shared": shown[name], "left": [occs[i]["pattern"], ri], "right": [occs[j]["pattern"], rj]})
                parent[find(i)] = find(j)
    comps = len({find(i) for i in range(len(occs))})
    return {"occurrences": [r["pattern"] for r in occs],
            "void": [r["pattern"] for r in records if r["status"] == "void"],
            "glued": glued, "components": comps,
            "verdict": "cascade" if comps == 1 and occs else ("bag" if comps > 1 else "empty")}


def to_edn(x):
    if isinstance(x, dict):
        return "{" + " ".join("%s %s" % (k if str(k).startswith(":") else ":" + str(k), to_edn(v)) for k, v in x.items()) + "}"
    if isinstance(x, list):
        # sets for theory elems / map pairs are what the validator wants
        return "[" + " ".join(to_edn(v) for v in x) + "]"
    if isinstance(x, bool):
        return "true" if x else "false"
    if x is None:
        return "nil"
    if isinstance(x, str):
        if x.startswith(":") and re.fullmatch(r":[a-z0-9/._-]+", x):
            return x
        return edn_str(x)
    return str(x)


def to_edn_square(rec):
    """Validator-shaped: elems and map relations as sets."""
    r = json.loads(json.dumps(rec))
    for o in r["objects"].values():
        o["theory"]["elems"] = "#{" + " ".join(o["theory"]["elems"]) + "}"
        o["theory"]["axioms"] = "#{}"
    for k, v in r["maps"].items():
        r["maps"][k] = "#{" + " ".join("[%s %s]" % (a, b) for a, b in v) + "}"
    s = to_edn(r)
    return s.replace('"#{', "#{").replace('}"', "}")


def main(argv):
    if len(argv) >= 2 and argv[0] == "parts":
        for pid in argv[1:]:
            p = read_pattern(pid)
            print("=== %s  sha256 %s" % (pid, p["sha256"][:12]))
            for part in PARTS:
                print("  [%s -> %s] %s" % (part, ROLE_OF[part], norm(p["parts"].get(part))[:300] or "MISSING"))
            print("  [CHECK] %s" % norm(p["check"] or p["conclusion"])[:300])
        return 0
    if len(argv) >= 2 and argv[0] == "square":
        occs = json.load(open(argv[1]))
        out = argv[argv.index("--out") + 1] if "--out" in argv else None
        records = []
        for occ in occs:
            pat = read_pattern(occ["pattern"])
            rec, voids = square(pat, occ)
            records.append(rec)
            print("%-45s %s%s" % (occ["pattern"], rec["status"],
                                   "  " + ", ".join("%s:%s" % (v["part"], v["reason"]) for v in voids) if voids else ""))
        cas = glue(records)
        print("cascade: %d occurrences, %d void, %d glued pairs, %d component(s) -> %s"
              % (len(cas["occurrences"]), len(cas["void"]), len(cas["glued"]), cas["components"], cas["verdict"]))
        for g in cas["glued"]:
            print("  glued at %r: %s/%s ~ %s/%s" % (g["shared"], g["left"][0], g["left"][1], g["right"][0], g["right"][1]))
        if out:
            squares = [to_edn_square(r) for r in records if r["status"] == "occurrence"]
            voids = [to_edn(r) for r in records if r["status"] == "void"]
            Path(out).write_text("{:squares [%s]\n :void [%s]\n :cascade %s}\n"
                                 % ("\n ".join(squares), "\n ".join(voids), to_edn(cas)))
            print("wrote", out)
        return 0
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
