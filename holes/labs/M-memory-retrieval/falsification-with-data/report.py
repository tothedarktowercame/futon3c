#!/usr/bin/env python3
import hashlib
import json
from pathlib import Path

HERE = Path("holes/labs/M-memory-retrieval/falsification-with-data")
ROOT = Path(".")

ARTIFACTS = [
    ("receipts-export-20260731-all-authors.edn", ROOT / "holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn", "0cc527e2"),
    ("e8-query-binding-20260803.json", ROOT / "holes/labs/M-memory-retrieval/e8-query-binding-20260803.json", None),
    ("retrieval-stage-causal-spec.json", ROOT / "holes/labs/M-memory-retrieval/retrieval-stage-causal-spec.json", None),
]

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

engine = json.loads((HERE / "engine.json").read_text())
dagitty = json.loads((HERE / "dagitty-results.json").read_text())
dowhy = json.loads((HERE / "dowhy-results.json").read_text())
hashes = [(name, sha(path), prefix) for name, path, prefix in ARTIFACTS]
for name, digest, prefix in hashes:
    if prefix and not digest.startswith(prefix):
        raise RuntimeError(f"hash mismatch for {name}: {digest}")

mapping = [
 ("problem-difficulty", "UNMEASURED", "Latent by specification; no per-dispatch difficulty instrument."),
 ("query-cardinality", "MEASURED", "Receipt offered record: count(evidence/body.recall-query.terms). E8 independently supplies count(arms[*].terms), but its 30 arms are not pooled with the 129 dispatches."),
 ("query-vocabulary", "MEASURED, CONSTANT", "1 = shipped-builder vocabulary for every offered receipt. E8 interventions are supplementary, not observational rows."),
 ("corpus-coverage", "UNMEASURED", "Latent by specification; no frozen per-dispatch coverage counter."),
 ("pollution", "MEASURED PROXY", "Fraction of recall-query.terms containing at least one literal TeX marker matched by [\\\\{}_^]; 0 for an empty term vector."),
 ("text-match-set", "UNMEASURED", "Receipts freeze final surfacing, not the normalized text-match candidate set."),
 ("pattern-endpoints", "UNMEASURED", "surfacing-via=:pattern is a route label, not the endpoint set/count named by this node."),
 ("attachment-density", "UNMEASURED", "No frozen attachment-store export is joinable per dispatch; E8 store metadata is aggregate."),
 ("reachability", "UNMEASURED", "E8 manipulates/query-checks five cases, but has no join key to the 129 dispatch panel."),
 ("surfaced-set", "MEASURED", "Count distinct evidence/body.memory-use.memory-use/surfaced-ids in each offered receipt."),
 ("offered-set", "MEASURED", "Count distinct memory-id values in evidence/body.memory-use.memory-use/inclusion-reasons."),
 ("used-set", "MEASURED, INCOMPLETE", "Join offered to the outcome carrying memory-use on job-id; count distinct outcome.memory-use.memory-use/used-ids. Present for 106/129 (one additional instrumented outcome has no matching offered row); no imputation for 23."),
 ("use-mode", "UNMEASURED", "Only 6/129 outcomes carry memory-use/use-mode; sparse free labels do not define a panel column."),
 ("grep-channel", "UNMEASURED", "Latent by specification."),
 ("runner-outcome", "UNMEASURED", "Outcome receipts mix cumulative solved counts, heterogeneous free-form result classes, and 10 absent outcomes; no preregistered common endpoint permits a per-dispatch derivation."),
]

tests = dagitty["tests"]
counts = {s: sum(t["status"] == s for t in tests)
          for s in ["survived", "survived-vacuous", "violated", "untestable"]}
violations = [t for t in tests if t["status"] == "violated"]

lines = ["# Retrieval-stage graph falsification with frozen V2 data", "",
"The applied `retrieval-stage-causal-spec.json` graph was projected with the engine's `admg/latent-project`; v3/v4 candidates were not used. The observational panel is exactly the 129 offered-phase dispatch receipts. E8 is a schema/operationalization supplement only and is never pooled into that panel.", "",
"## Frozen inputs consumed", "", "| artifact | verified sha256 | Appendix A check |", "|---|---|---|"]
for name, digest, prefix in hashes:
    check = f"matches `{prefix}…`" if prefix else "full hash frozen here (not listed in Appendix A)"
    lines.append(f"| `{name}` | `{digest}` | {check} |")
lines += ["", "## Variable mapping", "", "| spec node | status | exact derivation / reason |", "|---|---|---|"]
for node, status, reason in mapping:
    lines.append(f"| `{node}` | {status} | {reason} |")
lines += ["", "## Measured latent projection", "",
f"Nodes ({len(engine['projection']['nodes'])}): " + ", ".join(f"`{x}`" for x in engine["projection"]["nodes"]) + ".",
"", "Directed edges:"]
for a,b in engine["projection"]["directed"]:
    lines.append(f"- `{a} -> {b}`")
lines += ["", f"Bidirected edges: {len(engine['projection']['bidirected'])}.", "",
"Projection onto a superset of the measured query variables preserves the observed-margin CI implications; here all non-measured nodes were marked latent before projection.", "",
"## dagitty localTests", "",
f"Counts: survived={counts['survived']}; survived-vacuous={counts['survived-vacuous']}; violated={counts['violated']}; untestable={counts['untestable']}.", "",
"A CI is explicitly `survived-vacuous` if any participating column is constant or fewer than five complete rows exist. These are thin-data survivals, not corroboration.", "",
"| CI (verbatim) | n complete | p-value | verdict / reason |", "|---|---:|---:|---|"]
for t in tests:
    p = "—" if t.get("p_value") is None else f"{t['p_value']:.8g}"
    reason = t["status"] + ((": " + t["reason"]) if t.get("reason") else "")
    table_test = t["test"].replace("|", "\\|")
    lines.append(f"| `{table_test}` | {t['n']} | {p} | {reason} |")
lines += ["", "### Violated CIs (verbatim)", ""]
if violations:
    for t in violations:
        lines.append(f"- `{t['test']}` — p={t['p_value']:.17g}, n={t['n']}")
else:
    lines.append("None.")
lines += ["", "## Per-edge status", "",
f"Edge counts: survived=0; violated=0; untestable-as-an-edge={len(engine['projection']['directed'])}.", "",
"Edges are causal commitments, not conditional-independence nulls, so observational data cannot mark an individual edge ‘survived’. Each projected edge is `untestable-as-an-edge`; falsification attaches to the graph's implied CIs.", "",
"| projected edge | status |", "|---|---|"]
for a,b in engine["projection"]["directed"]:
    lines.append(f"| `{a} -> {b}` | untestable-as-an-edge |")
lines += ["", "## DoWhy GCM permutation falsification", "",
f"Status: **{dowhy['status']}**." + ((" " + dowhy["reason"]) if dowhy.get("reason") else ""), "",
f"The full projected graph was tested on {dowhy.get('complete_case_n')} complete cases with {dowhy['permutations_requested']} seeded permutations; no graph node was removed and no value was imputed. `query-vocabulary` remains constant, so every GCM local Markov claim involving it is data-thin. DoWhy summary:", "", "```text", dowhy.get("summary", ""), "```", "",
"DoWhy's kernel-based local Markov checks are not the same finite regression tests as the exact dagitty CI list above; its non-rejection therefore does not cancel either named dagitty violation.", "",
"The frozen panel has n=129 (and only 106 complete cases across the measured projection), so power is limited. Dispatches span heterogeneous problems, attempts, runners, and time; treating them as i.i.d. is an additional unverified assumption.", "",
"## Reproduction", "", "```sh", "holes/labs/M-memory-retrieval/falsification-with-data/run.sh", "```", ""]

(HERE / "REPORT.md").write_text("\n".join(lines))
outputs = ["engine.json", "data.csv", "dagitty-results.json", "dowhy-results.json", "REPORT.md"]
(HERE / "SHA256SUMS").write_text("".join(f"{sha(HERE / f)}  {f}\n" for f in outputs))
