#!/usr/bin/env python3
"""Derive the E2 panel cancellation pre-check from frozen repository revisions.

The evidence store is read only.  Repository snapshots are made with git archive
in a temporary directory and removed before exit; the live apm-lean worktree is
never checked out or modified.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tarfile
import tempfile
import urllib.request


HERE = Path(__file__).resolve().parent
FUTON3C = HERE.parents[2]
APM = Path("/home/joe/code/apm-lean")
MATHLIB = APM / ".lake/packages/mathlib/Mathlib"
STORE = "http://127.0.0.1:7073/api/alpha/evidence"
EDN_OUT = HERE / "panel-cancellation-check-20260801.edn"
MD_OUT = HERE / "panel-cancellation-check-20260801.md"
HASH_OUT = HERE / "panel-cancellation-check-20260801.sha256"


PAIRS = [
    {
        "problem": "a01A07",
        "revision": "81dccb3",
        "memory": "e-codexpilot-lift-the-circle-submean-bound-to-a-disk-area-bound",
        "summary": "Use the proved circle sub-mean inequality and polar coordinates/Fubini to bridge circle averages to the translated disk-area sub-mean bound.",
        "query": r"norm_le_circleAverage_norm|integral_closedBall_zero_eq_polar|integral_comp_polarCoord_symm|circleAverage",
        "mathlib_query": r"integral_comp_polarCoord_symm|circleAverage",
        "reachability": "reachable-cheaply",
        "prior_failure": "yes",
        "prior_evidence": "Attempts before the memory left the disk-area bridge open; queue receipt 57ca09c6-cfd2-441e-a99b-c96f6c2fffaa records a blocked partial.",
        "recommendation": "exclude",
        "score_varies": "No: the base file already contains the proved circle and polar bridge declarations and names the same API, so withholding the memory does not withhold its operative content.",
    },
    {
        "problem": "a01A07",
        "revision": "81dccb3",
        "memory": "e-codexpilot-upgrade-diskwise-L1-convergence-to-local-uniform-convergence",
        "summary": "Work on a local half-radius disk, prove uniform Cauchy convergence from two diskwise L1 errors, then package it with the local-uniform/compact-uniform API.",
        "query": r"smaller concentric disk|uniform Cauchy|tendstoLocallyUniformlyOn_iff_forall_isCompact|R / 2",
        "mathlib_query": r"tendstoLocallyUniformlyOn_iff_forall_isCompact|UniformCauchySeqOn",
        "reachability": "reachable-cheaply",
        "prior_failure": "yes",
        "prior_evidence": "The queue records attempt 1 as blocked-with-partial (receipt 57ca09c6-cfd2-441e-a99b-c96f6c2fffaa) before the later memory-carrying close.",
        "recommendation": "exclude",
        "score_varies": "No: the base theorem docstring spells out the local smaller-disk route, the uniform-Cauchy step, and the exact packaging declaration.",
    },
    {
        "problem": "a02J05",
        "revision": "fddc86c",
        "memory": "e-codexpilot-derive-a-sinc-tail-identity-by-differentiating-cosine-over-x",
        "summary": "Differentiate cos(x)/x to derive the finite-interval sinc identity and its 2/a Dirichlet tail bound.",
        "query": r"integral_sinc_eq_cos_div_sub|abs_integral_sinc_le_two_div|cos x / x",
        "mathlib_query": r"sinc|integral_eq_sub_of_hasDerivAt",
        "reachability": "reachable-cheaply",
        "prior_failure": "yes",
        "prior_evidence": "The first construction run proved the identity but remained blocked (receipt f8586558-7882-4ce9-b2cf-29402de797b1); later candidate rows 24 and 26 report the memory used.",
        "recommendation": "exclude",
        "score_varies": "No: both the named identity and the derived uniform tail theorem are already proved in the base file.",
    },
    {
        "problem": "a02J05",
        "revision": "fddc86c",
        "memory": "e-codexpilot-remove-Abel-regularization-from-the-Dirichlet-sinc-integral",
        "summary": "Evaluate the damped sinc integral by Fubini, send the damping to zero, and remove regularization with a uniform Dirichlet tail estimate.",
        "query": r"Abel regularization|arctan \(1/a\)|damped Fubini|uniform tail estimate",
        "mathlib_query": r"Abel|sinc.*integral|integral.*sinc",
        "reachability": "reachable-cheaply",
        "prior_failure": "yes",
        "prior_evidence": "Receipt f8586558-7882-4ce9-b2cf-29402de797b1 is the blocked run from which the frontier was drafted; a later attempt remained blocked before the third-attempt close.",
        "recommendation": "exclude",
        "score_varies": "No: the base file's remaining-obstruction comment gives the same Abel evaluation and uniform-removal plan.",
    },
    {
        "problem": "a95J08",
        "revision": "61ddc05",
        "memory": "e-codexpilot-bound-automatic-frontier-descent-when-a-leaf-recurses",
        "summary": "Stop automatic construction-target descent after one newly exposed frontier level and return the dependency chain for operator prioritisation.",
        "query": r"automatic.*frontier|frontier.*descent|leaf.*recurs|one newly exposed frontier",
        "mathlib_query": r"automatic.*frontier|frontier.*descent|leaf.*recurs|one newly exposed frontier",
        "reachability": "unreachable",
        "prior_failure": "no",
        "prior_evidence": "All three recorded a95J08 candidate rows (jobs 445, 460, 463) report this memory used; the frozen receipts contain no a95J08 attempt known to lack it.",
        "recommendation": "weak",
        "score_varies": "Possibly: the scope guard is absent from the base tree and Mathlib, but historical data provide no memory-free failure and it governed stopping rather than the successful proof route.",
    },
    {
        "problem": "a95J08",
        "revision": "61ddc05",
        "memory": "e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases",
        "summary": "Require an end-to-end library theorem before calling a task adapter work; constituent lemmas without that theorem indicate genuine construction.",
        "query": r"interface adapter|adapter work|end-to-end library theorem|constituent lemmas",
        "mathlib_query": r"interface adapter|adapter work|end-to-end library theorem|constituent lemmas",
        "reachability": "unreachable",
        "prior_failure": "no",
        "prior_evidence": "Jobs 445, 460, and 463 all report this memory used as a scope guard; no frozen a95J08 failure without it was identified.",
        "recommendation": "weak",
        "score_varies": "Possibly: its decision rule is not repository-searchable, but it only prevented a category error and did not supply the Hölder construction that eventually closed the theorem.",
    },
    {
        "problem": "a95J08",
        "revision": "61ddc05",
        "memory": "e-codexpilot-prove-general-probability-kernel-Lp-contraction-by-integral-Young",
        "summary": "Reduce probability-kernel Lp contraction to a general integral Young/Jensen-Tonelli bridge after kernel normalization and translation continuity.",
        "query": r"weighted Hölder|Jensen|Tonelli|Young.s convolution|lintegral_mul_norm_pow_le",
        "reachability": "reachable-with-route-knowledge",
        "prior_failure": "yes",
        "prior_evidence": "The first a95J08 candidate row (job 445) failed without this memory; later jobs 460 and 463 used it and still failed at the Jensen/ENNReal frontier.",
        "recommendation": "weak",
        "score_varies": "Yes, but direction is not preregistered safely: the base informal solution already exposes Hölder, while the memory steers toward Jensen; the eventual first-try close used Hölder and Mathlib's route-specific lintegral_mul_norm_pow_le.",
        "mathlib_query": r"lintegral_mul_norm_pow_le",
    },
]


def run(args: list[str], cwd: Path | None = None) -> str:
    proc = subprocess.run(args, cwd=cwd, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, check=False)
    if proc.returncode not in (0, 1):
        raise RuntimeError(f"command failed ({proc.returncode}): {' '.join(args)}\n{proc.stdout}")
    return proc.stdout.rstrip()


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify_store(memory_id: str) -> None:
    with urllib.request.urlopen(f"{STORE}/{memory_id}", timeout=10) as response:
        body = response.read().decode("utf-8")
    if response.status != 200 or memory_id not in body or ":evidence/type :memory" not in body:
        raise RuntimeError(f"store verification failed for {memory_id}")


def archive_revision(revision: str, target: Path) -> None:
    archive = target / "snapshot.tar"
    with archive.open("wb") as stream:
        proc = subprocess.run(["git", "archive", revision], cwd=APM, stdout=stream,
                              stderr=subprocess.PIPE, check=False)
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.decode("utf-8"))
    snapshot = target / "snapshot"
    snapshot.mkdir()
    with tarfile.open(archive) as tar:
        tar.extractall(snapshot, filter="data")
    archive.unlink()


def search(pattern: str, root: Path) -> str:
    output = run(["rg", "--no-heading", "-n", "-m", "12", "-i", pattern, str(root)])
    if not output:
        return "(no hits)"
    return output.replace(str(root) + os.sep, "")


def q(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def render_edn() -> str:
    lines = [
        "{:schema-version 1",
        ' :generated-at "2026-08-01"',
        ' :candidate-source "holes/labs/M-memory-retrieval/load-bearing-candidates-20260731.jsonl"',
        ' :candidate-provenance :used-memory-candidates-not-adjudicated',
        ' :provenance-warning "The named source report explicitly says no adjudication was performed; LB/IN labels are unavailable."',
        " :pairs [",
    ]
    for p in PAIRS:
        lines.extend([
            "  {:problem " + q(p["problem"]),
            "   :base-revision " + q(p["revision"]),
            "   :memory-id " + q(p["memory"]),
            "   :source-label :unadjudicated-used-candidate",
            "   :memory-summary " + q(p["summary"]),
            "   :reachability :" + p["reachability"],
            "   :prior-failure-without-memory? " + ({"yes": "true", "no": "false"}[p["prior_failure"]]),
            "   :prior-failure-evidence " + q(p["prior_evidence"]),
            "   :recommendation :" + p["recommendation"],
            "   :score-varies? " + q(p["score_varies"]) + "}",
        ])
    lines.extend([
        " ]",
        ' :panel-gaps [{:problem "a93J07"',
        '               :reason "No used-memory candidate exists for this problem in the named frozen candidate artifact; its five surfaced memories were recorded as ignored. No pair was invented."}]',
        ' :verdict {:strong 0 :weak 3 :exclude 4 :unassessable-problems 1}}',
    ])
    return "\n".join(lines) + "\n"


def render_markdown(searches: dict[tuple[str, str], dict[str, str]]) -> str:
    rows = []
    for p in PAIRS:
        short = p["memory"].removeprefix("e-codexpilot-")
        rows.append(f"| {p['problem']} | `{short}` | `{p['reachability']}` | {p['prior_failure']} | **{p['recommendation']}** | {p['score_varies']} |")
    sections = []
    for index, p in enumerate(PAIRS, 1):
        evidence = searches[(p["problem"], p["memory"])]
        mathlib_label = "Mathlib search"
        if p["reachability"] == "reachable-with-route-knowledge":
            mathlib_label += " (the G6 route-relative hit)"
        mathlib = ""
        if "mathlib" in evidence:
            mathlib = (f"\n{mathlib_label}:\n\n"
                       f"```text\n$ rg -n -i '{p['mathlib_query']}' .lake/packages/mathlib/Mathlib\n{evidence['mathlib']}\n```\n")
        sections.append(f"""### {index}. {p['problem']} × `{p['memory']}`

Memory content (read-only `GET {STORE}/{p['memory']}`): {p['summary']}

Base-tree search:

```text
$ git archive {p['revision']} | tar -x -C "$SNAPSHOT"
$ rg -n -i '{p['query']}' "$SNAPSHOT/problems/{p['problem']}"
{evidence['tree']}
```
{mathlib}
Verdict: **`:{p['reachability']}`**. {p['score_varies']}

Prior-failure evidence: {p['prior_evidence']}

Recommendation: **{p['recommendation']}**.
""")
    return f"""# E2 panel cancellation pre-check — 2026-08-01

## Result

The named source does **not** contain LB/IN adjudications. Its own report says:
“No adjudication was performed.” I therefore audited the seven concrete
used-memory pairs supported by the frozen JSONL and labelled them
`unadjudicated-used-candidate`; I did not manufacture LB/IN labels.

No pair clears the bar for **strong** inclusion. Four are excluded because the
operative content is already in the pre-solution tree. Three a95J08 pairs remain
weak: two non-searchable scope guards lack a historical memory-free failure,
while the mathematical memory is route-relative and points toward the Jensen
route that failed three times rather than the Hölder route that closed.

| problem | memory | reachability | prior failure without? | recommendation | registration `:score-varies?` text |
|---|---|---|---:|---|---|
{chr(10).join(rows)}

`a93J07` is unassessable from the named artifacts: it has no used-memory row in
the candidate JSONL. Its five surfaced memories were recorded as ignored. This
is a panel-construction gap, not evidence that any invented pair is incidental.

## Method and boundary

- Each revision was exported with `git archive` into a temporary directory;
  the live worktree was not changed and the directory was deleted on exit.
- The script searched only the target problem tree plus the installed Mathlib
  source for imported API content. It did not search future git objects or
  futon3c prose for reachability.
- Memory bodies were verified by read-only GETs on port 7073. No dispatch or
  store-write endpoint was called.
- `:reachable-cheaply` means the base problem names the operative theorem or
  route under an obvious query. `:reachable-with-route-knowledge` follows G6:
  the hit exists, but only vocabulary from the successful route exposes it.
  `:unreachable` means the operative content was absent under direct and close
  paraphrase queries in both allowed surfaces.
- V15 `repo_search` is the cancellation channel: a reachable pair is excluded
  when withholding the memory does not withhold the information.

## Evidence chains

{chr(10).join(sections)}

## Panel decision

Do not spend E2's full budget on this panel as currently sourced. The frozen
candidate artifact is a use census, not the preregistered LB/IN panel, and four
of seven supported pairs have structural cancellation. A run-ready registration
needs an actual adjudication artifact plus replacement pairs for the four
exclusions and for a93J07. The three a95J08 pairs can remain pilot candidates,
explicitly marked weak and without a predicted beneficial direction.
"""


def derive() -> None:
    candidate_report = HERE / "load-bearing-candidates-20260731-report.md"
    if "No adjudication was performed" not in candidate_report.read_text():
        raise RuntimeError("candidate provenance warning changed")
    searches: dict[tuple[str, str], dict[str, str]] = {}
    for p in PAIRS:
        verify_store(p["memory"])
    with tempfile.TemporaryDirectory(prefix="e2-panel-check-") as tmp_name:
        tmp = Path(tmp_name)
        snapshots: dict[str, Path] = {}
        for revision in sorted({p["revision"] for p in PAIRS}):
            target = tmp / revision
            target.mkdir()
            archive_revision(revision, target)
            snapshots[revision] = target / "snapshot"
        for p in PAIRS:
            root = snapshots[p["revision"]] / "problems" / p["problem"]
            evidence = {"tree": search(p["query"], root)}
            if p.get("mathlib_query"):
                evidence["mathlib"] = search(p["mathlib_query"], MATHLIB)
            searches[(p["problem"], p["memory"])] = evidence
    EDN_OUT.write_text(render_edn())
    MD_OUT.write_text(render_markdown(searches))
    manifest = "".join(
        f"{sha256(path)}  {path.name}\n"
        for path in (EDN_OUT, MD_OUT, Path(__file__))
    )
    HASH_OUT.write_text(manifest)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true",
                        help="derive twice and assert byte-identical artifacts")
    args = parser.parse_args()
    derive()
    if args.check:
        before = {p: p.read_bytes() for p in (EDN_OUT, MD_OUT, HASH_OUT)}
        derive()
        after = {p: p.read_bytes() for p in (EDN_OUT, MD_OUT, HASH_OUT)}
        if before != after:
            raise SystemExit("non-deterministic output")
        print("byte-identical rerun: PASS")
    else:
        print(EDN_OUT)
        print(MD_OUT)
        print(HASH_OUT)


if __name__ == "__main__":
    main()
