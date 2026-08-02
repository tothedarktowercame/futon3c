# M-diagramprover D2 oracle falsification pass

## Result

No structure-level disagreements were found.

| Check | Agreements | Disagreements |
|---|---:|---:|
| Engine implications × NetworkX | 209 | 0 |
| Engine implications × dagitty | 209 | 0 |
| dagitty implied-CI converse (verdict level) | 1382 | 0 |
| Q3 pair + V18 corollaries × NetworkX | 4 | 0 |
| Q3 pair + V18 corollaries × dagitty | 4 | 0 |

Named disagreements (verbatim): `[]`.

## Q3 divergence

The independent oracles reproduce the receipt: star-forest marginal separation is
`true`; populated-graph marginal
separation is `false`. Both find
`V18 ⟂ M-in-store | V12-minus-M`. The receipt's populated witness remains
`[M-in-store shared-patterns V12-minus-M]`; oracle verdicts independently confirm
the dependence rather than relying on that engine-generated path.

## Identification × y0

- Q1: `P(V18 | do(V06))` identifiable: `true`.
- R1 total effect: `P(P16 | do(P20))` identifiable: `true`.
- R1 conditional query with `P01` and `P10-pre`: identifiable by IDC:
  `true`.

Encoding: all exported variables are observed and all exported arcs are directed;
there are no latent/bidirected arcs. Thus y0 establishes ID/IDC identifiability.
The receipt's exact empty Q1 and `{P01, P10-pre}` R1 adjustment verdicts remain
separate computed backdoor claims, not conclusions inferred from y0's formula.

## Tools

- Python 3.12.3
- NetworkX 3.6.1
- y0 0.2.11
- DoWhy 0.14
- pandas 3.0.5
- R 4.3.3
- dagitty 0.3.4

## Deferred with reason

- dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.
- DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.
- Q2 mediation-under-surgery: deferred — no independent structure-level oracle was specified or available (priced-in limit).

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/oracle-pass/run.sh
```

The pass regenerates exports and both reports deterministically. It reads only the
exports and the two read-only causal spec JSONs after the Clojure export step.
