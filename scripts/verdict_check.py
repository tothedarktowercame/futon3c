#!/usr/bin/env python3
"""verdict_check.py — does the mission's prose agree with its lifecycle data?

  verdict_check.py

lifecycle.edn carries a :status per phase and the mission text carries a
verdict line per section. Nothing kept them in step, and they drifted: DERIVE
was marked :exit-met in the data on 2026-09-24 while its section still read
"Met for the tooled steps; not met for four steps done by hand", because a
string replacement matched nothing and said nothing. The page rendered the
contradiction for hours, and an outside reader found it rather than any check
here.

A verdict line is the first **Met.** / **Not met.** / **Not started.** inside
a phase's section. Exactly those three, exactly so: a partial verdict like
"Met for instance 4" is not a verdict a reader -- human or machine -- can act
on, and the qualification belongs in the paragraph beneath.

Exit 1 on any disagreement.
"""
import json, os, re, subprocess, sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MISSION = os.path.join(REPO, "holes/missions/M-futon-seams.md")
LIFECYCLE = os.path.join(REPO, "holes/labs/M-futon-seams/lifecycle.edn")

VERDICT = {"Met.": {"exit-met"},
           "Not met.": {"in-progress", "blocked"},
           "Not started.": {"not-started"}}
# "Not met." is the verdict; whether the phase is in progress or blocked on
# another is WHY it is not met, and lives in :status rather than in the line.

# Which phases carry a verdict line is declared per phase in lifecycle.edn as
# :verdict-source, not listed here. It was a hardcoded set until 2026-09-24 --
# a property of a phase living in a script, which is the defect this mission
# is about, in the tooling that checks this mission.


def main():
    life = json.loads(subprocess.run(["bb", "-e",
        '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
        f'(print (j/generate-string (edn/read-string (slurp "{LIFECYCLE}"))))'],
        capture_output=True, text=True).stdout)
    text = open(MISSION, encoding="utf-8").read()
    bad, checked = [], 0

    for ph in life["phases"]:
        anc = ph.get("mission-anchor")
        if not anc:
            continue
        if not ph.get("verdict-source"):
            bad.append(f"{ph['id']}: no :verdict-source — say whether its "
                       f"verdict is :prose or :data-only")
            continue
        # the section runs from its heading to the next one
        start = anc["start"]
        nxt = re.search(r"\n## ", text[anc["end"]:])
        section = text[start:anc["end"] + (nxt.start() if nxt else len(text))]
        found = re.search(r"\*\*(Met\.|Not met\.|Not started\.)\*\*", section)
        if not found:
            loose = re.search(r"\*\*(Met[^*]{1,60}|Not met[^*]{1,60})\*\*", section)
            if loose:
                bad.append(f"{ph['id']}: partial verdict {loose.group(0)!r} — "
                           f"a reader cannot act on it; put the qualification "
                           f"in the paragraph and make the verdict exact")
            elif ph.get("verdict-source") == "data-only":
                pass     # closure recorded in lifecycle.edn and nowhere else
            else:
                bad.append(f"{ph['id']}: no verdict line at all")
            continue
        checked += 1
        allowed = VERDICT[found.group(1)]
        if ph["status"] not in allowed:
            bad.append(f"{ph['id']}: the section says {found.group(1)!r} "
                       f"({'/'.join(sorted(allowed))}), lifecycle.edn says "
                       f":{ph['status']}")
        if ph["status"] == "blocked" and not ph.get("blocked-on"):
            bad.append(f"{ph['id']}: :blocked without :blocked-on — say which "
                       f"phase it waits for")

    print(f"{checked} verdict lines checked against lifecycle.edn — "
          + ("OK" if not bad else f"{len(bad)} DISAGREEMENT(S)"))
    for b in bad:
        print("  FAIL " + b)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
