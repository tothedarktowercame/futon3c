#!/usr/bin/env python3
"""cascade_check.py — validate a proto-cascade-v1 file against the library.

  cascade_check.py holes/labs/M-futon-seams/proto/instance-*.edn

Checks, per file:
  - every pattern id resolves to a file under futon3/library;
  - its id line matches its path, accepting @arg, @flexiarg and @multiarg as
    the loader does (futon3a projection.clj) and as session_turn_analysis.py
    now does since 3a486cdc;
  - the sha256 in each receipt still matches the file's bytes;
  - every token named in a guard, a :produces, :initial or :want is declared
    in :tokens, and every declared token is used;
  - every :above endpoint is a declared pattern;
  - which wants no pattern produces (a finding, not an error: the cascade may
    say so deliberately in :holes).

Exit 1 if any check fails. Unproduced wants alone do not fail the file.
"""
import hashlib, json, os, re, subprocess, sys

LIB = "/home/joe/code/futon3/library"
IDLINE = re.compile(r"^@(?:arg|flexiarg|multiarg)\s+(.+?)\s*$", re.M)


def edn(path):
    r = subprocess.run(
        ["bb", "-e",
         '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
         f'(print (j/generate-string (edn/read-string (slurp "{path}"))))'],
        capture_output=True, text=True)
    if r.returncode:
        sys.exit(f"cascade_check: cannot read {path}: {r.stderr.strip()}")
    return json.loads(r.stdout)


MISSION = "/home/joe/code/futon3c/holes/missions/M-futon-seams.md"


def check(path):
    c = edn(path)
    bad, notes = [], []

    # A :cue is an anchor into the mission exactly as an annotation's is, but
    # it carries no quote to re-check, so the most that can be verified is the
    # sha it was taken against and that the spans are inside the file.
    if os.path.isfile(MISSION):
        text = open(MISSION, encoding="utf-8").read()
        now = hashlib.sha256(text.encode()).hexdigest()
        if c.get("mission-sha") and c["mission-sha"] != now:
            bad.append(f"mission-sha {c['mission-sha'][:12]}… but the mission is now "
                       f"{now[:12]}… — every :cue may have moved")
        for tok, meta in (c.get("tokens") or {}).items():
            cue = meta.get("cue")
            if cue and (cue[0] < 0 or cue[1] > len(text) or cue[0] >= cue[1]):
                bad.append(f"token {tok}: cue {cue} is outside the mission")
    pats = c.get("patterns", {})
    declared = set(c.get("tokens", {}))
    used = set(c.get("initial", [])) | set(c.get("want", []))

    for pid, p in sorted(pats.items()):
        f = os.path.join(LIB, pid + ".flexiarg")
        if not os.path.isfile(f):
            bad.append(f"{pid}: no file at {f}")
            continue
        content = open(f, encoding="utf-8", errors="replace").read()
        ids = IDLINE.findall(content)
        if pid not in ids:
            bad.append(f"{pid}: id line says {ids[0] if ids else '(none)'}")
        recorded = p.get("receipt", {}).get("source", {}).get("sha256")
        now = hashlib.sha256(open(f, "rb").read()).hexdigest()
        if recorded and recorded != now:
            bad.append(f"{pid}: receipt sha256 {recorded[:12]}… but file is {now[:12]}…")
        used |= set(p.get("produces", [])) | set(p["guard"].get("needs", [])) \
            | set(p["guard"].get("forbids", []))

    for e in c.get("above", []):
        for side in ("context", "pattern"):
            if e[side] not in pats:
                bad.append(f"above edge names unknown pattern: {e[side]}")

    for t in sorted(used - declared):
        bad.append(f"token {t} is used but not declared in :tokens")
    for t in sorted(declared - used):
        notes.append(f"token {t} is declared but never used")

    produced = {t for p in pats.values() for t in p.get("produces", [])}
    for w in sorted(set(c.get("want", [])) - produced):
        holes = {h.get("token") for h in (c.get("holes") or [])}
        notes.append(f"want {w} has no producer"
                     + (" (declared in :holes)" if w in holes else " — AND NOT IN :holes"))

    print(f"{path}: {len(pats)} patterns, {len(c.get('above', []))} edges — "
          + ("OK" if not bad else f"{len(bad)} PROBLEM(S)"))
    for b in bad:
        print("  FAIL " + b)
    for n in notes:
        print("  note " + n)
    return not bad


if __name__ == "__main__":
    sys.exit(0 if all([check(p) for p in sys.argv[1:]]) else 1)
