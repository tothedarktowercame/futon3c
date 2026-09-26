#!/usr/bin/env python3
"""mission_c_check.py — does the mission-C reference still hold?

  mission_c_check.py holes/labs/M-futon-seams/item6/mission-C.edn

The file is the reference an extractor will be tested against, so it has to
be checkable itself: every :cue-quote must be the text at its :cue in the
mission at the recorded sha, every span must be a single occurrence (a
span-by-span comparison against an ambiguous quote proves nothing), and every
:served-by want must exist in its instance's cascade.

Exit 1 on any failure.
"""
import offset_unit
import hashlib, json, os, subprocess, sys

MISSION = "holes/missions/M-futon-seams.md"
PROTO = "holes/labs/M-futon-seams/proto/instance-%s.edn"


def edn(path):
    r = subprocess.run(["bb", "-e",
        '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
        f'(print (j/generate-string (edn/read-string (slurp "{path}"))))'],
        capture_output=True, text=True)
    if r.returncode:
        sys.exit(f"mission_c_check: {r.stderr.strip()}")
    return json.loads(r.stdout)


def main():
    c = edn(sys.argv[1])
    text = open(MISSION, encoding="utf-8").read()
    bad = []

    u = offset_unit.complaint(c.get("offset-unit"), os.path.basename(sys.argv[1]))
    if u:
        bad.append(u)

    now = hashlib.sha256(text.encode()).hexdigest()
    if c.get("mission-sha") != now:
        bad.append(f"mission-sha {c.get('mission-sha','')[:12]}… but the mission is "
                   f"{now[:12]}… — every cue may have moved")

    spans = list(c["outcomes"].items()) + [("(preference)", c["preference"])]
    for name, v in spans:
        cue, quote = v.get("cue"), v.get("cue-quote")
        if not cue or quote is None:
            bad.append(f"{name}: no cue or no cue-quote")
            continue
        a, b = cue
        if text[a:b] != quote:
            bad.append(f"{name}: cue [{a} {b}] is {text[a:b][:40]!r}, "
                       f"recorded {quote[:40]!r}")
        elif text.count(quote) != 1:
            bad.append(f"{name}: the quote occurs {text.count(quote)} times, "
                       f"so a span-by-span match against it proves nothing")

    wants = {}
    for s in c["served-by"]:
        if s.get("status") == "prospective":
            if s.get("want") is not None:
                bad.append(f"prospective entry for instance {s['instance']} names a want")
            continue
        i = s["instance"]
        if i not in wants:
            path = PROTO % i
            wants[i] = set(edn(path)["want"]) if os.path.exists(path) else None
        if wants[i] is None:
            bad.append(f"instance {i}: no cascade, but the mapping is not :prospective")
        elif s["want"] not in wants[i]:
            bad.append(f"instance {i}: {s['want']} is not one of its wants")
        if s["outcome"] not in c["outcomes"]:
            bad.append(f"instance {i}: outcome {s['outcome']} is not declared")

    served = {s["outcome"] for s in c["served-by"]}
    for o in sorted(set(c["outcomes"]) - served):
        bad.append(f"outcome {o} is declared and served by nothing")

    if c["preference"]["status"] == "unstated" and c["preference"].get("weights"):
        bad.append("preference is :unstated but carries weights")

    print(f"{sys.argv[1]}: {len(c['outcomes'])} outcomes, {len(c['served-by'])} mappings — "
          + ("OK" if not bad else f"{len(bad)} PROBLEM(S)"))
    for x in bad:
        print("  FAIL " + x)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
