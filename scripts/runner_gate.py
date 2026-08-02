#!/usr/bin/env python3
"""Learning-loop gates for the runner contract (Joe's design, 2026-08-02).

A gate is not a filter, it is a TEACHER. On a violation it does four things:
  1. REJECT the run (it does not count toward the cohort's endpoints);
  2. PUSH BACK structured feedback so the agent learns its behaviour was wrong;
  3. RECORD the violation against that agent in an append-only ledger;
  4. on REPEAT violations of the same norm, STOP THE LINE for that agent and
     raise a meta-learning event -- the memory is not landing, which is a
     higher-order problem than any single run.

The feedback body IS a candidate correction-memory: a gate violation is the
dual of the fixes we store as memories (error->fix). Type A gates (re-verify,
don't trust the paste) teach VERIFICATION-discipline memories; Type B gates
(gate the coverage) teach REPORTING-discipline memories. Once the memory lands
and the agent stops violating, re-verification can be SAMPLED rather than run
every time -- the violation rate is the control signal, which is how the loop
avoids becoming makework.

Run the demonstration:  python3 scripts/runner_gate.py --demo
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

STATE_DIR = Path(os.environ.get("RUNNER_GATE_STATE_DIR",
                                Path(__file__).resolve().parent.parent
                                / ".state/runner-gate"))
LEDGER = STATE_DIR / "violations.jsonl"
STOP_THE_LINE_THRESHOLD = int(os.environ.get("RUNNER_GATE_STOP_THRESHOLD", "3"))


@dataclass
class Violation:
    agent: str
    norm: str
    run_id: str
    detail: str


@dataclass
class Run:
    """The minimal facts a gate needs about a completed run."""
    agent: str
    run_id: str
    report: str
    surfaced_ids: list = field(default_factory=list)   # from the offered receipt


# --------------------------------------------------------------------------
# Gates. Each declares the fix-memory it teaches, so the feedback pushed back
# to the agent is exactly the correction that would be stored as a memory.
# --------------------------------------------------------------------------

class UseAttributionGate:
    """TYPE B -- gate the coverage. Every surfaced id must carry exactly one
    USED/IGNORED verdict, checked against the offered receipt's surfaced set."""
    norm = "use-attribution"
    teaches = ("Emit `USED <id>: <mechanism>` or `IGNORED <id>: <reason>` for "
               "every surfaced memory id. A surfaced id with no verdict starves "
               "the use/ignore witness the whole retrieval result depends on.")

    _verdict = re.compile(r"^\s*(USED|IGNORED)\s+(\S+?):", re.MULTILINE)

    def check(self, run: Run) -> list:
        attributed = {m.group(2) for m in self._verdict.finditer(run.report)}
        missing = [i for i in run.surfaced_ids if i not in attributed]
        if not missing:
            return []
        return [Violation(run.agent, self.norm, run.run_id,
                          f"{len(missing)} surfaced id(s) with no verdict: "
                          f"{', '.join(missing)}")]


class AxiomReverifyGate:
    """TYPE A -- re-verify, don't trust the paste. Placeholder for the wired
    version that RE-RUNS `#print axioms` on each claimed-complete decl rather
    than trusting the runner's pasted output. Shown here so both enforcement
    types visibly share the one learning loop; the demo exercises Type B."""
    norm = "axiom-cleanliness"
    teaches = ("Re-run `#print axioms` before claiming complete; a stale olean "
               "reports a false-clean, and a pasted axiom block is not evidence "
               "the file is actually sorryAx-free.")

    def check(self, run: Run) -> list:   # pragma: no cover - placeholder
        raise NotImplementedError("wire to: lake env lean + #print axioms re-run")


# --------------------------------------------------------------------------
# The learning loop.
# --------------------------------------------------------------------------

def _prior_count(agent: str, norm: str) -> int:
    if not LEDGER.exists():
        return 0
    n = 0
    for line in LEDGER.read_text().splitlines():
        try:
            rec = json.loads(line)
        except ValueError:
            continue
        if rec.get("agent") == agent and rec.get("norm") == norm:
            n += 1
    return n


def _record(v: Violation) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    with LEDGER.open("a") as fh:
        fh.write(json.dumps({"agent": v.agent, "norm": v.norm,
                             "run_id": v.run_id, "detail": v.detail}) + "\n")


def feedback(v: Violation, gate) -> str:
    """The push-back message -- and the body of the candidate correction-memory."""
    return (f"RUN REJECTED ({v.norm}). {v.detail}\n"
            f"WHY IT MATTERS / WHAT TO LEARN: {gate.teaches}\n"
            f"Resubmit with the correction; this feedback is stored as a memory "
            f"and will surface on your next dispatch.")


def adjudicate(run: Run, gates, *, persist: bool = True) -> dict:
    """Run the gates over one run and return a verdict with feedback."""
    violations, messages, stop = [], [], False
    meta_learning = None
    for gate in gates:
        vs = gate.check(run)
        for v in vs:
            violations.append(v)
            if persist:
                _record(v)
            repeats = _prior_count(v.agent, v.norm) if persist else 0
            messages.append(feedback(v, gate))
            if repeats >= STOP_THE_LINE_THRESHOLD:
                stop = True
                meta_learning = (
                    f"STOP THE LINE: {v.agent} has {repeats} prior "
                    f"{v.norm} violations. The correction-memory is not landing "
                    f"-- escalate to meta-learning (is it surfacing? is the "
                    f"contract wrong? is this agent the problem?). Halt dispatch "
                    f"to {v.agent} until resolved.")
    if not violations:
        return {"verdict": "accept", "counts_toward_endpoints": True}
    return {"verdict": "stop-the-line" if stop else "reject-push-back",
            "counts_toward_endpoints": False,
            "feedback": messages,
            "meta_learning": meta_learning}


def _demo() -> int:
    surfaced = ["e-alpha", "e-beta", "e-gamma"]
    gates = [UseAttributionGate()]
    print(f"[demo uses an isolated ledger; threshold={STOP_THE_LINE_THRESHOLD}]\n")

    print("=== 1. Compliant run: every surfaced id has a verdict ===")
    ok = Run("codex-9", "run-1", surfaced_ids=surfaced, report=(
        "Memory usage:\n"
        "USED e-alpha: carried the Holder split.\n"
        "IGNORED e-beta: wrong route (Jensen).\n"
        "USED e-gamma: named the covering lemma.\n"))
    print(json.dumps(adjudicate(ok, gates), indent=2), "\n")

    print("=== 2. Violating run: two surfaced ids unaccounted ===")
    bad = Run("codex-7", "run-2", surfaced_ids=surfaced, report=(
        "Memory usage: used the first one, the others weren't relevant.\n"))
    r = adjudicate(bad, gates)
    print(json.dumps({k: v for k, v in r.items() if k != "feedback"}, indent=2))
    print("--- feedback pushed back to codex-7 (and stored as a memory) ---")
    print(r["feedback"][0], "\n")

    print("=== 3. Repeat violations by the same agent -> stop-the-line ===")
    for k in range(3, 3 + STOP_THE_LINE_THRESHOLD):
        r = adjudicate(Run("codex-7", f"run-{k}", surfaced_ids=surfaced,
                           report="Memory usage: n/a\n"), gates)
    print(f"after {STOP_THE_LINE_THRESHOLD} more codex-7 violations: "
          f"verdict={r['verdict']}")
    print(r["meta_learning"])
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--demo", action="store_true", help="run the worked demonstration")
    args = ap.parse_args()
    if args.demo:
        # isolate the demo ledger so it never pollutes real state
        global STATE_DIR, LEDGER
        import tempfile
        STATE_DIR = Path(tempfile.mkdtemp(prefix="runner-gate-demo-"))
        LEDGER = STATE_DIR / "violations.jsonl"
        rc = _demo()
        for p in (LEDGER,):
            p.unlink(missing_ok=True)
        STATE_DIR.rmdir()
        return rc
    ap.print_help()
    return 0


if __name__ == "__main__":
    sys.exit(main())
