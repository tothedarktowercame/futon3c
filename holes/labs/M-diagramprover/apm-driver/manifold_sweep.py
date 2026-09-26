#!/usr/bin/env python3
"""Manifold-convention sweep: 9 statements, one question each.

Why this is delegable where most of tonight's work was not: the question is
closed-form, the deliverable is checkable, and a YES must carry a counterexample
that I can verify independently. A bluffed YES fails on the counterexample; a
bluffed NO is caught by the same reasoning I would have done myself.

Three defects of exactly this shape were already found by accident (t01J04,
t94A06, t03J04), each false as frozen rather than merely hard. These 9 are the
open manifold problems carrying NO separation or countability hypothesis.
"""
import json
import pathlib
import sys

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import agency
import gates
import statement_campaign as sc

DRIVER = pathlib.Path("/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
REPO = pathlib.Path("/home/joe/code/apm-lean")
OUT = DRIVER / "manifold-sweep-jobs.jsonl"
SEATS = ["zai-1", "ams-zai-1", "ams-air-1"]  # zai-2/3/4 are on the roster but NOT dispatchable

TARGETS = ["t01A08", "t01J05", "t03J05", "t95J06", "t96A08",
           "t96J05", "t96J06", "t97J05", "t98A04"]

PACKET = """MANIFOLD-CONVENTION CHECK - apm-{pid}. ONE QUESTION ONLY.

Mathlib's manifold typeclasses are WEAKER than the informal notion textbooks
use. `ChartedSpace` and `IsManifold` do NOT imply Hausdorff, regularity, or
second countability. Sources say "manifold" and silently mean all three, so a
formalisation that copies the source can be FALSE AS FROZEN rather than merely
hard to prove.

Three have already been found this way:
  - t01J04 needed T2Space and RegularSpace. Counterexample: the line with a
    doubled origin, where a neighbourhood cannot be shrunk with closure inside.
  - t03J04 needed T2Space. Counterexample: the circle with a doubled point,
    which has H0 = Q and H1 = Q^2 by Mayer-Vietoris, so its Euler
    characteristic is -1, not the asserted 0.
  - t94A06 needed second countability. Counterexample: an uncountable DISCRETE
    space is a genuine zero-dimensional manifold via
    `IsManifold.of_discreteTopology`, and every map out of it is smooth, so a
    parametric-transversality claim fails for EVERY parameter.

THE STATEMENT UNDER TEST is `{main}` in {path}. It carries NO T2Space,
RegularSpace or SecondCountableTopology hypothesis.

ANSWER EXACTLY ONE OF:

  NEEDS <axiom> - the frozen statement is FALSE without it. You must give a
    CONCRETE COUNTEREXAMPLE: a specific space satisfying every stated
    hypothesis and violating the conclusion. Name the space, say why it really
    is a manifold under Mathlib's definition, and say which part of the
    conclusion fails and why. A general remark that pathologies exist is NOT a
    counterexample and will be sent back.

  SOUND AS FROZEN - the statement is true without any extra axiom. Say in one
    or two sentences WHY the pathologies cannot bite here: typically because
    the space is a concrete construction (a sphere, a torus, a quotient of one)
    rather than an arbitrary manifold, or because the conclusion does not
    depend on the separation properties.

DO NOT EDIT ANY FILE. Do not repair the statement, do not add hypotheses, do
not attempt a proof. This is a review question and the deliverable is your
written answer alone. Read the file first:
  cd /home/joe/code/apm-lean && sed -n '1,80p' {path}

REPORT: the verdict word (NEEDS <axiom> / SOUND AS FROZEN), then the
counterexample or the reason, in at most eight lines.
"""


def main() -> int:
    sent = []
    for i, pid in enumerate(TARGETS):
        path = REPO / "problems" / pid / "lean" / "Main.lean"
        src = path.read_text(encoding="utf-8")
        try:
            main_name = gates.statement_hash(src, pid)[0]
        except Exception:
            main_name = f"apm_{pid.lower()}"
        packet = PACKET.format(pid=pid, main=main_name,
                               path=f"problems/{pid}/lean/Main.lean")
        seat = SEATS[i % len(SEATS)]
        try:
            job = agency.dispatch_fn(seat, packet)["job-id"]
        except Exception as exc:
            print(f"  {pid}: DISPATCH FAILED ({exc})")
            continue
        sent.append({"at": sc.now_iso(), "problem-id": pid, "seat": seat, "job-id": job})
        print(f"  {pid:8s} -> {seat}  {job}")
    with OUT.open("a", encoding="utf-8") as fh:
        for r in sent:
            fh.write(json.dumps(r) + "\n")
    print(f"\ndispatched {len(sent)} of {len(TARGETS)}")
    print("JOBS:", " ".join(r["job-id"] for r in sent))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
