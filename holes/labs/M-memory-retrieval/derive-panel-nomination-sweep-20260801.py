#!/usr/bin/env python3
"""Reproduce the 2026-08-01 E2 full-corpus panel nomination sweep.

This is an offline derivation.  It reads the durable Agency ledger, frozen
exports, and Git objects; it never contacts or writes the memory store.
"""

from __future__ import annotations

import hashlib
import json
import re
import subprocess
from collections import defaultdict
from pathlib import Path

from edn_format import Keyword, loads


HERE = Path(__file__).resolve().parent
FUTON = HERE.parents[2]
APM = FUTON.parent / "apm-lean"
LEDGER = Path("/tmp/futon3c-invoke-jobs.edn")
RECEIPTS = HERE / "receipts-export-20260731-all-authors.edn"
GRAPH = FUTON / "holes/labs/M-typed-memories/live-graph-export-20260730.edn"
QUEUE = FUTON / "data/codex-sorry-queue.edn"
OUT_EDN = HERE / "panel-nomination-sweep-20260801.edn"
OUT_MD = HERE / "panel-nomination-sweep-20260801.md"
PINNED_APM = "82f98e81258a0e5ac49b7bfc74f8e35d4c9b7964"
ROLLOUT_CUTOFF = "2026-08-01T15:10:00"
K = Keyword

STRONG_PROBLEM = "a95j01"
STRONG_MEMORY = "e-codexpilot-analytic-order-at-least-two-implies-local-noninjectivity"
BASE_REV = "51b6bc00dcc1c6cfeaecc79cdcf0fc8d2b720f03"
CLOSE_REV = "016bf0fb5b7f196abb61770d0d6e953e97305036"
PRIOR_JOB = "invoke-1785393010025-303-f9f19596"
ARRIVAL_JOB = "invoke-1785519091300-552-1f8d8a48"
USE_JOB = "invoke-1785521408563-571-76fa2f7b"
CLOSE_JOB = "invoke-1785582866229-633-5750d1ef"

EXCLUDED_PROBLEMS = {
    "bpm-1-1-2", "bpm-1-3-2", "bpm-1-7-1", "bpm-1-8-1",
    "a95j04", "a95a08", "a01j06", "a01j05", "a95a02", "t94j01",
    "a01a07", "a02j05", "a93j07", "a95j08",
}

MEMORY_ID_RE = re.compile(r"\be-[A-Za-z0-9][A-Za-z0-9_-]{5,}\b")
PROBLEM_PATH_RE = re.compile(r"problems/([^/]+)/lean/Main\.lean", re.I)
NEG = ("ignored", "irrelevant", "not used", "unused", "did not use", "unrelated")
POS = ("used", "useful", "applied", "guided", "carried", "influenced", "directly", "relied")


def run(*args: str, cwd: Path = FUTON, check: bool = True) -> str:
    p = subprocess.run(args, cwd=cwd, text=True, capture_output=True, check=False)
    if check and p.returncode:
        raise RuntimeError(f"command failed ({p.returncode}): {' '.join(args)}\n{p.stderr}")
    return p.stdout


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def val(obj, key, default=None):
    return obj.get(K(key), obj.get(key, default)) if hasattr(obj, "get") else default


def strip_lean_comments(text: str) -> str:
    """Remove nested block comments and line comments while preserving strings."""
    out, i, depth, string = [], 0, 0, False
    while i < len(text):
        if depth:
            if text.startswith("/-", i): depth += 1; i += 2
            elif text.startswith("-/", i): depth -= 1; i += 2
            else: out.append("\n" if text[i] == "\n" else " "); i += 1
        elif string:
            out.append(text[i])
            if text[i] == "\\" and i + 1 < len(text): out.append(text[i + 1]); i += 2
            else:
                if text[i] == '"': string = False
                i += 1
        elif text.startswith("/-", i): depth = 1; i += 2
        elif text.startswith("--", i):
            j = text.find("\n", i)
            if j < 0: break
            out.append("\n"); i = j + 1
        else:
            out.append(text[i])
            if text[i] == '"': string = True
            i += 1
    return "".join(out)


def hole_free_problem_ids() -> set[str]:
    names = run("git", "ls-tree", "-r", "--name-only", PINNED_APM, "problems", cwd=APM).splitlines()
    mains = [p for p in names if re.fullmatch(r"problems/[^/]+/lean/Main\.lean", p)]
    clean = set()
    for path in mains:
        src = strip_lean_comments(run("git", "show", f"{PINNED_APM}:{path}", cwd=APM))
        holes = re.search(r"\bsorry\b", src) or re.search(r"\bopaque\s+\w+\s*:[^:=\n]+(?:\n|$)", src)
        if not holes:
            clean.add(path.split("/")[1].lower())
    assert len(mains) == 145 and len(clean) == 137, (len(mains), len(clean))
    return clean


def classify_lines(text: str) -> tuple[set[str], set[str]]:
    used, ignored = set(), set()
    for line in text.splitlines():
        ids = set(MEMORY_ID_RE.findall(line))
        lower = line.lower()
        if any(x in lower for x in NEG): ignored |= ids
        elif any(x in lower for x in POS): used |= ids
    return used - ignored, ignored


def candidate_census(clean: set[str]):
    ledger = loads(LEDGER.read_text())
    jobs = val(ledger, "jobs", {})
    pairs: dict[tuple[str, str], set[str]] = defaultdict(set)
    ignored: dict[tuple[str, str], set[str]] = defaultdict(set)
    for jid in val(ledger, "job-order", []):
        job = jobs.get(jid, jobs.get(K(str(jid)), {}))
        result = val(job, "result", "")
        if not isinstance(result, str) or "Memory usage" not in result:
            continue
        path_hits = PROBLEM_PATH_RE.findall(result)
        if not path_hits:
            continue
        problem = path_hits[-1].lower()
        if problem not in clean:
            continue
        section = result[result.lower().rfind("memory usage"):]
        used, unused = classify_lines(section)
        for mid in used: pairs[(problem, mid)].add(str(jid))
        for mid in unused: ignored[(problem, mid)].add(str(jid))
    # This is the frozen-ledger census at the measurement cutoff, after collapsing repeats.
    assert len(pairs) == 50, len(pairs)
    return pairs, ignored, jobs


def reachability_checks() -> list[dict]:
    checks = []
    patterns = [
        "analyticOrderAt|not_injOn_nhds|deriv_ne_zero_of_injOn",
        "analyticOrderAt.*not_inj|not_inj.*analyticOrderAt|deriv_ne_zero_of_inj|analytic.*local.*noninject|univalent.*deriv.*nonzero",
    ]
    scopes = ["problems/a95J01", ":!problems/a95J01"]
    for pattern, scope in zip(patterns, scopes):
        cmd = ["git", "grep", "-n", "-i", "-E", pattern, BASE_REV, "--", scope]
        p = subprocess.run(cmd, cwd=APM, text=True, capture_output=True)
        assert p.returncode == 1 and not p.stdout
        checks.append({"command": " ".join(cmd), "result": "no matches"})
    mathlib_cmd = ["rg", "-n", "-i", patterns[1], ".lake/packages/mathlib/Mathlib"]
    p = subprocess.run(mathlib_cmd, cwd=APM, text=True, capture_output=True)
    assert p.returncode == 1 and not p.stdout
    checks.append({"command": " ".join(mathlib_cmd), "result": "no matches"})
    return checks


def edn(obj) -> str:
    if obj is None: return "nil"
    if obj is True: return "true"
    if obj is False: return "false"
    if isinstance(obj, str): return json.dumps(obj, ensure_ascii=False)
    if isinstance(obj, (int, float)): return str(obj)
    if isinstance(obj, list): return "[" + " ".join(edn(x) for x in obj) + "]"
    if isinstance(obj, dict):
        return "{" + " ".join(edn(k) + " " + edn(obj[k]) for k in sorted(obj)) + "}"
    raise TypeError(type(obj))


def main() -> None:
    clean = hole_free_problem_ids()
    pairs, ignored, jobs = candidate_census(clean)
    checks = reachability_checks()

    for rev in (BASE_REV, CLOSE_REV, "953a06fd734e3d0bab57c8776bdffa86b2498d6b"):
        run("git", "cat-file", "-e", f"{rev}^{{commit}}", cwd=APM)
    for jid in (PRIOR_JOB, ARRIVAL_JOB, USE_JOB, CLOSE_JOB):
        assert jobs.get(jid, jobs.get(K(jid))) is not None
    prior = str(val(jobs[PRIOR_JOB], "result", ""))
    arrival = str(val(jobs[ARRIVAL_JOB], "result", ""))
    use = str(val(jobs[USE_JOB], "result", ""))
    assert "no memory IDs were supplied or surfaced" in prior
    assert "deriv g z ≠ 0" in prior and STRONG_MEMORY in arrival and STRONG_MEMORY in use

    graph = GRAPH.read_text()
    assert STRONG_MEMORY in graph
    assert "2026-07-30T17:26:30.049396801Z" in graph
    assert '"e-review-codex-5-analytic-order-at-least-two-implies-local-noninjectivity"' in graph

    rows = []
    for (problem, memory), job_ids in sorted(pairs.items()):
        excluded = problem in EXCLUDED_PROBLEMS
        if (problem, memory) == (STRONG_PROBLEM, STRONG_MEMORY):
            verdict, reason = "nominate-strong", "All four criteria pass; exact missing local-degree bridge was absent from the base tree and Mathlib."
        elif excluded:
            verdict, reason = "excluded-by-spec", "Problem is held out, held for statement review, or already staged."
        elif problem == STRONG_PROBLEM:
            verdict, reason = "exclude-confirmation", "Used only to confirm an already-present Schwarz API, not to supply the missing bridge."
        elif problem in {"a02j02", "a02j03", "a94a03", "a94j04", "a95a06", "a96a01", "a96a04", "a97a08", "a01a10"}:
            verdict, reason = "exclude-reachable", "Operative content or route vocabulary was reachable in the pre-solution tree or pinned Mathlib."
        elif problem in {"a95j05"}:
            verdict, reason = "exclude-no-baseline", "No historical memory-free failed attempt against the same target was found."
        elif problem == "a00j04":
            verdict, reason = "exclude-post-closure", "Memory use occurred in a later construction-target run, after the beneficiary problem was closed."
        else:
            verdict, reason = "exclude-no-clean-sequence", "No verified failure-without → reviewed arrival → closure sequence survived the evidence screen."
        rows.append({"problem": problem, "memory-id": memory, "jobs": sorted(job_ids), "verdict": verdict, "reason": reason})

    nominated = [r for r in rows if r["verdict"] == "nominate-strong"]
    assert len(nominated) == 1
    in_side = sorted(mid for (p, mid) in ignored if p == STRONG_PROBLEM)
    artifact = {
        "version": 1,
        "generated-by": "derive-panel-nomination-sweep-20260801.py",
        "scope": {"canonical-problems": 145, "hole-free": 137, "used-pairs": 50, "nominated": 1},
        "inputs": {"apm-revision": PINNED_APM, "ledger": str(LEDGER), "receipts-sha256": sha256(RECEIPTS), "graph-sha256": sha256(GRAPH), "queue-sha256": sha256(QUEUE)},
        "exclusions": sorted(EXCLUDED_PROBLEMS),
        "nomination": {
            "problem": "a95J01", "memory-id": STRONG_MEMORY,
            "base-revision": BASE_REV, "closure-revision": CLOSE_REV,
            "prior-failure-job": PRIOR_JOB, "arrival-job": ARRIVAL_JOB,
            "use-job": USE_JOB, "closure-job": CLOSE_JOB,
            "reviewed-at": "2026-07-30T17:26:30.049396801Z",
            "review-evidence": "e-review-codex-5-analytic-order-at-least-two-implies-local-noninjectivity",
            "score-varies?": True,
            "score-varies-reason": "The memory supplied the exact absent local-degree bridge at which the prior memory-free run stopped; withholding should vary rediscovery cost and closure probability.",
            "in-side-candidates": in_side,
            "reachability-checks": checks,
        },
        "candidate-adjudications": rows,
    }
    OUT_EDN.write_text(edn(artifact) + "\n")

    table = "\n".join(
        f"| {r['problem']} | `{r['memory-id']}` | {r['verdict']} | {r['reason']} |"
        for r in rows if r["verdict"] == "nominate-strong"
    )
    md = f"""# E2 full closed-problem panel nomination sweep — 2026-08-01

## Verdict

The full sweep yields **one strong pair**, not the requested 6–10. This is an honest
small-panel result: 50 distinct used memory/problem pairs across the 137 hole-free
canonical problems were screened, and only one has a complete, dual-time-valid
failure → reviewed arrival → use → closure chain while also surviving repository-search
cancellation.

| problem | memory | recommendation | reason |
|---|---|---|---|
{table}

The pair's `:score-varies?` is **true**: the memory supplied the exact local-degree
bridge at which the earlier memory-free attempt stopped, and that bridge was absent
from both the base problem tree and pinned Mathlib under direct and close-paraphrase
queries.

## Fully worked timeline 1 — nominated a95J01 pair

1. **Memory-free failure.** `{PRIOR_JOB}` ran at
   `2026-07-30T06:30:10Z` and committed `{BASE_REV}`. Its report explicitly says
   no memories were supplied or surfaced, and stops at the leaf
   `IsOpen U → DifferentiableOn ℂ g U → InjOn g U → z ∈ U → deriv g z ≠ 0`.
2. **Historical attachment.** Frozen graph export captured at `2026-07-30T19:00:54Z`
   records review evidence
   `e-review-codex-5-analytic-order-at-least-two-implies-local-noninjectivity`,
   reviewed at `2026-07-30T17:26:30.049396801Z`. This predates both memory-bearing
   runs, so the arrival is valid-time safe rather than inferred from current graph state.
3. **Arrival and attributed use.** `{ARRIVAL_JOB}` at
   `2026-07-31T17:31:31Z` reports `{STRONG_MEMORY}` used to identify the exact
   local-degree obstruction. `{USE_JOB}` then reports the same memory used when
   importing the completed `ConstructionTargets.UnivalentDeriv` bridge.
4. **Construction and closure.** `953a06fd734e3d0bab57c8776bdffa86b2498d6b`
   proves the nonvanishing-derivative construction target. `{CLOSE_JOB}` closes
   a95J01 at `{CLOSE_REV}` with zero sorries.
5. **Rerun revisions.** Base `{BASE_REV}`; observed closure `{CLOSE_REV}`.

Reachability commands (all returned no matches):

```text
{chr(10).join('$ ' + c['command'] for c in checks)}
```

## Fully worked timeline 2 — rejected a96A04 cancellation

This case has a real failure and later memory use, but fails criterion (a).

1. `{ 'invoke-1785098957336-167-75dd4cab' }` committed `f614856` with three
   remaining sorries after proving only Gaussian normalization.
2. `{ 'invoke-1785470457961-468-ad547147' }` later reported using
   `e-codexpilot-derive-integrable-from-nonzero-bochner-integral` and
   `e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth`.
3. Cancellation is decisive: pinned Mathlib contains the exact declaration
   `integrable_of_integral_eq_one` in
   `Mathlib/MeasureTheory/Integral/Bochner/Basic.lean`, and
   `Mathlib/Analysis/Calculus/ContDiff/Defs.lean` explicitly documents that
   `ContDiff ... ⊤` is analytic while `ContDiff ... ∞` is smooth. The operative
   content is therefore repository-searchable; this is not a strong isolation pair.

## Incidental-arm candidates for the nominated problem

The arrival receipt records the following surfaced-and-ignored memories on a95J01:

{chr(10).join('- `' + x + '`' for x in in_side)}

`e-88a1af39-53d5-4ac8-a01b-04137a559619` is not placed in the incidental arm:
the runner marked it used, but only as confirmation of the already-present Schwarz API,
so it is excluded from LB nomination rather than relabelled IN.

## Honest gaps and interpretation

- Most closures do not have all four temporal events. Some have use without a
  memory-free failed baseline; some statement-repair jobs cannot supply a same-target
  baseline; others used memories whose content is already in the base tree or Mathlib.
- The frozen graph export is sufficient for the nominated pair because its review time
  predates arrival. Current attachment state was not projected backward.
- The full census is a use census, not an LB/IN adjudication source. The EDN records each
  pair's screen verdict; it does not import labels from
  `load-bearing-candidates-20260731.jsonl`.
- A one-pair panel is too fragile for the intended E2 comparison. More strong pairs need
  a fresh preregistered window that records memory-free baselines and reviewed attachment
  times before dispatch, rather than retrospective label manufacture.

## Reproduction and frozen inputs

Run:

```text
python3 holes/labs/M-memory-retrieval/derive-panel-nomination-sweep-20260801.py
sha256sum holes/labs/M-memory-retrieval/panel-nomination-sweep-20260801.edn \\
  holes/labs/M-memory-retrieval/panel-nomination-sweep-20260801.md
```

Inputs: receipts `{sha256(RECEIPTS)}`; graph `{sha256(GRAPH)}`; queue
`{sha256(QUEUE)}`; pinned apm-lean `{PINNED_APM}`. The script reads the frozen
local ledger and performs no network calls or store writes.
"""
    OUT_MD.write_text(md)
    print(f"wrote {OUT_EDN.name} {sha256(OUT_EDN)}")
    print(f"wrote {OUT_MD.name} {sha256(OUT_MD)}")


if __name__ == "__main__":
    main()
