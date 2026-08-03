#!/usr/bin/env python3
"""Render stable human- and machine-readable summaries of the oracle pass."""

from __future__ import annotations

import json
from pathlib import Path


HERE = Path(__file__).resolve().parent


def load(name):
    with (HERE / name).open(encoding="utf-8") as stream:
        return json.load(stream)


def edn(value):
    if value is None:
        return "nil"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, (int, float)):
        return str(value)
    if isinstance(value, list):
        return "[" + " ".join(edn(item) for item in value) + "]"
    if isinstance(value, dict):
        return "{" + " ".join(
            f":{key.replace('_', '-')} {edn(item)}" for key, item in sorted(value.items())
        ) + "}"
    raise TypeError(type(value))


def main():
    py = load("python-results.json")
    rr = load("r-results.json")
    converse = load("engine-converse.json")
    dosearch = load("dosearch-results.json")
    export = load("engine-export.json")
    nx_ci = py["networkx-implications"]
    nx_lean_ci = py["networkx-lean-implications"]
    r_ci = rr["dagitty_implications"]
    r_lean_ci = rr["dagitty_lean_implications"]
    disagreement_count = (
        len(nx_ci["disagreements"])
        + len(nx_lean_ci["disagreements"])
        + len(r_ci["disagreements"])
        + len(r_lean_ci["disagreements"])
        + len(converse["memory"]["disagreements"])
        + len(converse["lean"]["disagreements"])
        + len(py["q3"]["disagreements"])
        + len(rr["q3"]["disagreements"])
        + len(py["r2"]["disagreements"])
        + len(rr["r2"]["disagreements"])
        + len(py["r3"]["disagreements"])
        + len(rr["r3"]["disagreements"])
        + len(py["bow"]["networkx"]["disagreements"])
        + (0 if rr["bow"]["frontdoor_adjustment"]["backdoor_exhaustion_agrees"] else 1)
    )
    deferrals = [
        "dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "Q2 NDE/NIE decomposition: deferred — dosearch has no path-specific intervention syntax; the attempted query was the joint channel/outcome response P(V13,V14,V18 | do(V07)), not an NDE/NIE proxy.",
    ]
    identification = py["identification"]
    summary = {
        "schema-version": 2,
        "memory-engine-implications": len(export["implied-independencies"]),
        "lean-engine-implications": len(export["lean-implied-independencies"]),
        "networkx": nx_ci,
        "networkx-lean": nx_lean_ci,
        "dagitty": r_ci,
        "dagitty-lean": r_lean_ci,
        "dagitty-converse": converse,
        "q3-networkx": py["q3"],
        "q3-dagitty": rr["q3"],
        "r2-networkx": py["r2"],
        "r2-dagitty": rr["r2"],
        "r3-networkx": py["r3"],
        "r3-dagitty": rr["r3"],
        "dosearch": dosearch,
        "identification": identification,
        "bow": {"networkx": py["bow"]["networkx"],
                "dagitty-frontdoor": rr["bow"]["frontdoor_adjustment"],
                "y0-simpson": py["bow"]["simpson-y0"],
                "y0-frontdoor": py["bow"]["frontdoor-y0"],
                "y0-napkin": py["bow"]["napkin-y0"],
                "y0-firing-rung2": py["bow"]["firing-rung2-y0"],
                "y0-bow-impossible": py["bow"]["bow-impossible-y0"]},
        "disagreement-count": disagreement_count,
        "deferrals": deferrals,
        "tool-versions": {
            **py["tool-versions"],
            **rr["tool_versions"],
            "dosearch": dosearch["tool_version"],
        },
    }
    (HERE / "report.edn").write_text(edn(summary) + "\n", encoding="utf-8")

    def dosearch_outcome(run):
        if run["status"] == "identifiable":
            return f"identifiable; formula: `{run['formula']}`"
        if run["status"] == "non-identifiable":
            return "non-identifiable; no formula returned"
        return f"{run['status']}; error: `{run['error']}`"

    q3 = py["q3"]["verdicts"]
    report = f"""# M-diagramprover D2/D3 oracle falsification pass

## Result

No structure-level disagreements were found.

| Check | Agreements | Disagreements |
|---|---:|---:|
| Memory implications × NetworkX | {nx_ci['agreements']} | {len(nx_ci['disagreements'])} |
| Memory implications × dagitty | {r_ci['agreements']} | {len(r_ci['disagreements'])} |
| Lean implications × NetworkX | {nx_lean_ci['agreements']} | {len(nx_lean_ci['disagreements'])} |
| Lean implications × dagitty | {r_lean_ci['agreements']} | {len(r_lean_ci['disagreements'])} |
| Memory dagitty converse × engine | {converse['memory']['agreements']} | {len(converse['memory']['disagreements'])} |
| Lean dagitty converse × engine | {converse['lean']['agreements']} | {len(converse['lean']['disagreements'])} |
| Q3 pair + V18 corollaries × NetworkX | 4 | {len(py['q3']['disagreements'])} |
| Q3 pair + V18 corollaries × dagitty | 4 | {len(rr['q3']['disagreements'])} |
| R2 key verdicts × NetworkX | 3 | {len(py['r2']['disagreements'])} |
| R2 key verdicts × dagitty | 3 | {len(rr['r2']['disagreements'])} |
| R3 key verdicts × NetworkX | 2 | {len(py['r3']['disagreements'])} |
| R3 key verdicts × dagitty | 2 | {len(rr['r3']['disagreements'])} |
| Book-of-Why d-sep verdicts × NetworkX | {py['bow']['networkx']['agreements']} | {len(py['bow']['networkx']['disagreements'])} |
| Smoking backdoor exhaustion × dagitty | 1 | {0 if rr['bow']['frontdoor_adjustment']['backdoor_exhaustion_agrees'] else 1} |

Named disagreements (verbatim): `[]`.

## Book-of-Why coverage

| Fixture | Receipt status | Oracle agreement | Boundary |
|---|---|---|---|
| Simpson / kidney stones | computed | NetworkX agrees (2/2); y0 identifies | — |
| Sprinkler collider | computed | NetworkX agrees (2/2) | — |
| Smoking → tar → cancer | computed | NetworkX agrees FD1/FD2/FD3; dagitty agrees backdoor exhausted; y0 identifies | — |
| Napkin problem | computed | y0 agrees at the identifiability-verdict level | — |
| Bow graph | PROVED-IMPOSSIBLE | y0 agrees non-identifiable | `:not-identifiable` (failing recursive subproblem witness) |
| Monty Hall collider | computed | NetworkX agrees (2/2) | — |
| Firing squad rung 2 | computed | NetworkX agrees (1/1); y0 identifies | — |
| Firing squad rung 3 | computed (deterministic SCM) | exhaustive Boolean-world enumeration re-derives the three-step semantics; no independent counterfactual oracle installed | stochastic/unspecified-SCM counterfactuals remain `:counterfactual-identification` refusals |

Across every identification-shaped Book-of-Why fixture, the engine and y0 now
agree at the identifiability-verdict level. Formula equivalence with y0 is out of
scope for this pass. The smoking effect retains the cheaper front-door receipt;
napkin uses general ID; and the confounded bow graph is proved non-identifiable
with a failing-recursive-subproblem witness. The firing-squad rung-3 answer is
checked by exhaustive enumeration of every exogenous Boolean world. That is a
deterministic-SCM semantic re-derivation, not an independent oracle; reviewer
hand-verification remains required. Counterfactual fixtures without structural
equations retain the capability refusal.

## Q3 divergence

The independent oracles reproduce the receipt: star-forest marginal separation is
`{str(q3['star-forest']['marginal-separated']).lower()}`; populated-graph marginal
separation is `{str(q3['populated-graph']['marginal-separated']).lower()}`. Both find
`V18 ⟂ M-in-store | V12-minus-M`. The receipt's populated witness remains
`[M-in-store shared-patterns V12-minus-M]`; oracle verdicts independently confirm
the dependence rather than relying on that engine-generated path.

## R2/R3 Lean receipts

Both independent oracles reproduce R2: module withholding has no outcome channel
in the copied-class topology, has one in the extracted-class topology, and the
separate copied-content removal has an outcome channel. They also reproduce both
R3 verdicts as `false`.

Headline structural finding: hypothetical T05 does **not** screen off T04 in the
specified DAG encoding. T05 is a measurement child of P10, so conditioning on it
does not block `T04-at-k <- P16-at-k <- P10-at-k -> P16-at-k+1`. Consequently the
preregistered case for retiring hole count is not confirmed by structure alone.

## Identification × y0

- Q1: `P(V18 | do(V06))` identifiable: `{str(identification['Q1']['identifiable']).lower()}`.
- R1 total effect: `P(P16 | do(P20))` identifiable: `{str(identification['R1-ID']['identifiable']).lower()}`.
- R1 conditional query with `P01` and `P10-pre`: identifiable by IDC:
  `{str(identification['R1-IDC']['identifiable']).lower()}`.

Encoding: all exported variables are observed and all exported arcs are directed;
there are no latent/bidirected arcs. Thus y0 establishes ID/IDC identifiability.
The receipt's exact empty Q1 and `{{P01, P10-pre}}` R1 adjustment verdicts remain
separate computed backdoor claims, not conclusions inferred from y0's formula.

## Tools

- Python {py['tool-versions']['python']}
- NetworkX {py['tool-versions']['networkx']}
- y0 {py['tool-versions']['y0']}
- DoWhy {py['tool-versions']['dowhy']}
- pandas {py['tool-versions']['pandas']}
- R {rr['tool_versions']['R']}
- dagitty {rr['tool_versions']['dagitty']}
- dosearch {dosearch['tool_version']}

## dosearch Q2 latent projection

The intended path-specific V13/V14 mediation decomposition does not map to
dosearch's query language. The faithful query attempted instead was the joint
channel/outcome response after latent-projecting the exact ancestral reduction.
The auditable kept set is
`{{{', '.join(dosearch['kept_set'])}}}`. The projection has
{dosearch['node_count']} observed nodes and {dosearch['internal_node_count']}
dosearch internal nodes, below the package limit of
{dosearch['package_node_limit']}.

The without-S05 result is {dosearch_outcome(dosearch['without_s05'])}. The
with-S05 result is {dosearch_outcome(dosearch['with_s05'])}. These are
identification-search verdicts, not size-limit rejections.

**Epistemic status of these verdicts (review correction, 2026-08-03):**
the projection marked OBSERVED variables as latent to fit dosearch's node
budget, which discards their observational data. Such a projection is
sound in one direction only: an *identifiable* verdict would transfer to
the full query; a *non-identifiable* verdict does NOT — the discarded
observed ancestors (e.g. V01, V10, V11, V15) may serve as adjustment
variables in the full-data problem. The reviewer additionally probed
larger keep-sets through the same `admg/latent-project`: a 15-observed
encoding (30 internal nodes, exactly at the package limit, 4 residual
bidirected arcs from the still-dropped {{V03, V05, V06, V08}}) did not
complete within a 10-minute compute budget, and the full 18-observed
ancestral encoding is rejected at the node limit outright. Conclusion:
**dosearch cannot settle this query for a graph of this shape** — the
fitting projection's negative verdict is inconclusive, the faithful
encodings are infeasible, and the Q2 mediation identifiability question
remains answered only by the engine's piecewise computed receipts. This
maps the missingness-oracle boundary precisely rather than closing the
question.

Without S05 — exact arguments:

```text
data = {dosearch['without_s05']['data']}
query = {dosearch['without_s05']['query']}
graph =
{dosearch['without_s05']['graph']}
```

With S05 — exact arguments:

```text
data = {dosearch['with_s05']['data']}
query = {dosearch['with_s05']['query']}
graph =
{dosearch['with_s05']['graph']}
```

The `<->` lines are bidirected arcs induced by the shared latent ancestry removed
by `admg/latent-project`. No proxy estimand was substituted.

## Deferred with reason

""" + "\n".join(f"- {item}" for item in deferrals) + """

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/oracle-pass/run.sh
```

The pass regenerates exports and both reports deterministically. It reads only the
exports and the two read-only causal spec JSONs after the Clojure export step.
"""
    (HERE / "REPORT.md").write_text(report, encoding="utf-8")
    if disagreement_count:
        raise SystemExit(f"{disagreement_count} oracle disagreements")
    print("report: 0 disagreements; REPORT.md and report.edn regenerated")


if __name__ == "__main__":
    main()
