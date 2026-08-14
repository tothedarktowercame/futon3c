#!/usr/bin/env python3
"""Re-harvest: why 133 open problems never entered the hop-4 worklist.

Two causes, and only the first is fixable by widening a pattern:

  1. PHRASING. The first harvest matched "no packaged", "Mathlib has no",
     "remains open", "still requires". Real reports also say "The remaining
     bridge is ...", "no theorem proving that ...", "needs X bridged through
     Y", "a gap of ~15-20 tactic steps". Same content, different words.

  2. THE EXCERPT IS CODE. For some problems `boundary_excerpt` returns raw
     theorem text or tactic script rather than the closer's prose, so there is
     nothing to harvest at any regex width. That is a pipeline defect, not a
     vocabulary one, and widening cannot reach it.

Measure both before changing the worklist.
"""
import json
import pathlib
import re
import sys

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import gates
import statement_campaign as sc

DRIVER = pathlib.Path("/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
REPO = pathlib.Path("/home/joe/code/apm-lean")
OUT = DRIVER / "mathlib-holes-2.jsonl"

OLD = re.compile(r"(no packaged|not found|does not exist|Mathlib (?:has no|lacks|installs no)|"
                 r"no general|no lemma|no such|remains? open|still (?:requires|needs))", re.I)
WIDE = re.compile(
    r"(no packaged|not found|does not exist|Mathlib (?:has no|lacks|installs no)|no general|"
    r"no lemma|no such|remains? open|still (?:requires|needs)|remaining bridge|"
    r"no theorem|the gap|a gap of|bridged through|is still the|what remains|"
    r"still (?:missing|absent|open)|not available|would need|the missing)", re.I)
# A prose report, not a spill of source: tactic script and declaration headers
# both start with Lean keywords.
CODE_START = ("theorem", "lemma", "intro", "refine", "apply", "cases", "exact",
              "have", "constructor", "rw ", "simp", "unfold", "·", "|", "(")
IDENT = re.compile(r"`([A-Za-z_][A-Za-z0-9_.']*)`")


def main() -> int:
    dispatched = {json.loads(l)["problem-id"]
                  for l in (DRIVER / "bridge-pilot-jobs.jsonl").read_text().splitlines() if l.strip()}
    never = [f.parent.parent.name for f in sorted(REPO.glob("problems/*/lean/Main.lean"))
             if gates.count_sorries(f.read_text(encoding="utf-8")) > 0
             and f.parent.parent.name not in dispatched]

    rows, code_excerpt, no_match = [], [], []
    for pid in never:
        text = " ".join((sc.boundary_excerpt(pid) or "").split())
        if not text or text.lstrip().startswith(CODE_START):
            code_excerpt.append(pid)
            continue
        m = WIDE.search(text)
        if not m:
            no_match.append(pid)
            continue
        start = text.rfind(".", 0, m.start()) + 1
        end = text.find(".", m.end())
        rows.append({"problem-id": pid, "hops-spent": 3,
                     "hole": text[start: end if end > 0 else len(text)].strip()[:400],
                     "identifiers": sorted(set(IDENT.findall(text)))[:8],
                     "stage": "reharvested"})

    print(f"never dispatched            : {len(never)}")
    print(f"  excerpt is CODE not prose : {len(code_excerpt)}  (pipeline defect; widening cannot help)")
    print(f"  prose but still no match  : {len(no_match)}")
    print(f"  NEWLY HARVESTABLE         : {len(rows)}")
    print(f"     with a named identifier: {sum(1 for r in rows if r['identifiers'])}")
    OUT.write_text("\n".join(json.dumps(r) for r in rows) + "\n", encoding="utf-8")
    print(f"\nwritten to {OUT}")
    if code_excerpt:
        print("code-excerpt sample:", " ".join(code_excerpt[:10]))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
