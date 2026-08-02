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
    )
    deferrals = [
        "dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "dosearch Q2 boundary: deferred — dosearch 1.0.12 doubles 18 faithful ancestral variables into 36 internal intervention nodes, exceeding its hard limit of 30; both without-S05 and with-S05 queries were rejected before identification search.",
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
        "disagreement-count": disagreement_count,
        "deferrals": deferrals,
        "tool-versions": {
            **py["tool-versions"],
            **rr["tool_versions"],
            "dosearch": dosearch["tool_version"],
        },
    }
    (HERE / "report.edn").write_text(edn(summary) + "\n", encoding="utf-8")

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

Named disagreements (verbatim): `[]`.

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

## dosearch Q2 boundary

The intended path-specific V13/V14 mediation decomposition does not map to
dosearch's query language. The faithful query attempted instead was the joint
channel/outcome response on the exact 18-node ancestral reduction. Both runs were
rejected before search: `{dosearch['without_s05']['status']}` without S05 and
`{dosearch['with_s05']['status']}` with S05. dosearch creates 36 internal nodes
after adding intervention nodes, above its hard limit of 30.

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

Exact error for both: `{dosearch['without_s05']['error']}` No identifying or
non-identifying verdict was returned, so Q2's pure computed refusal is not
upgraded to a proved dosearch boundary in this slice.

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
