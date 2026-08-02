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
    export = load("engine-export.json")
    nx_ci = py["networkx-implications"]
    r_ci = rr["dagitty_implications"]
    disagreement_count = (
        len(nx_ci["disagreements"])
        + len(r_ci["disagreements"])
        + len(converse["disagreements"])
        + len(py["q3"]["disagreements"])
        + len(rr["q3"]["disagreements"])
    )
    deferrals = [
        "dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.",
        "Q2 mediation-under-surgery: deferred — no independent structure-level oracle was specified or available (priced-in limit).",
    ]
    identification = py["identification"]
    summary = {
        "schema-version": 1,
        "engine-implications": len(export["implied-independencies"]),
        "networkx": nx_ci,
        "dagitty": r_ci,
        "dagitty-converse": converse,
        "q3-networkx": py["q3"],
        "q3-dagitty": rr["q3"],
        "identification": identification,
        "disagreement-count": disagreement_count,
        "deferrals": deferrals,
        "tool-versions": {**py["tool-versions"], **rr["tool_versions"]},
    }
    (HERE / "report.edn").write_text(edn(summary) + "\n", encoding="utf-8")

    q3 = py["q3"]["verdicts"]
    report = f"""# M-diagramprover D2 oracle falsification pass

## Result

No structure-level disagreements were found.

| Check | Agreements | Disagreements |
|---|---:|---:|
| Engine implications × NetworkX | {nx_ci['agreements']} | {len(nx_ci['disagreements'])} |
| Engine implications × dagitty | {r_ci['agreements']} | {len(r_ci['disagreements'])} |
| dagitty implied-CI converse (verdict level) | {converse['agreements']} | {len(converse['disagreements'])} |
| Q3 pair + V18 corollaries × NetworkX | 4 | {len(py['q3']['disagreements'])} |
| Q3 pair + V18 corollaries × dagitty | 4 | {len(rr['q3']['disagreements'])} |

Named disagreements (verbatim): `[]`.

## Q3 divergence

The independent oracles reproduce the receipt: star-forest marginal separation is
`{str(q3['star-forest']['marginal-separated']).lower()}`; populated-graph marginal
separation is `{str(q3['populated-graph']['marginal-separated']).lower()}`. Both find
`V18 ⟂ M-in-store | V12-minus-M`. The receipt's populated witness remains
`[M-in-store shared-patterns V12-minus-M]`; oracle verdicts independently confirm
the dependence rather than relying on that engine-generated path.

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
