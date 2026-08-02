# M-diagramprover D2/D3 oracle falsification pass

## Result

No structure-level disagreements were found.

| Check | Agreements | Disagreements |
|---|---:|---:|
| Memory implications × NetworkX | 209 | 0 |
| Memory implications × dagitty | 209 | 0 |
| Lean implications × NetworkX | 202 | 0 |
| Lean implications × dagitty | 202 | 0 |
| Memory dagitty converse × engine | 1382 | 0 |
| Lean dagitty converse × engine | 426 | 0 |
| Q3 pair + V18 corollaries × NetworkX | 4 | 0 |
| Q3 pair + V18 corollaries × dagitty | 4 | 0 |
| R2 key verdicts × NetworkX | 3 | 0 |
| R2 key verdicts × dagitty | 3 | 0 |
| R3 key verdicts × NetworkX | 2 | 0 |
| R3 key verdicts × dagitty | 2 | 0 |

Named disagreements (verbatim): `[]`.

## Q3 divergence

The independent oracles reproduce the receipt: star-forest marginal separation is
`true`; populated-graph marginal
separation is `false`. Both find
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
- dosearch 1.0.12

## dosearch Q2 boundary

The intended path-specific V13/V14 mediation decomposition does not map to
dosearch's query language. The faithful query attempted instead was the joint
channel/outcome response on the exact 18-node ancestral reduction. Both runs were
rejected before search: `rejected-size-limit` without S05 and
`rejected-size-limit` with S05. dosearch creates 36 internal nodes
after adding intervention nodes, above its hard limit of 30.

Without S05 — exact arguments:

```text
data = p(V01,V02,V03,V04,V05,V06,V07,V08,V09,V10,V11,V12,V15,V16,V17,V18)
query = p(V13,V14,V18|do(V07))
graph =
V01 -> V08
V01 -> V09
V01 -> V16
V02 -> V10
V02 -> V11
V03 -> V15
V03 -> V17
V04 -> V08
V04 -> V16
V05 -> V02
V05 -> V03
V06 -> V09
V07 -> V02
V07 -> V12
V08 -> V09
V08 -> V15
V09 -> V10
V10 -> V11
V10 -> V12
V11 -> V12
V11 -> V12
V12 -> V13
V12 -> V14
V13 -> V16
V14 -> V16
V15 -> V16
V16 -> V17
V16 -> V18
V17 -> V18
```

With S05 — exact arguments:

```text
data = p(V01,V02,V03,V04,V05,V06,V07,V08,V09,V10,V11,V12,V13,V14,V15,V16,V17,V18)
query = p(V13,V14,V18|do(V07))
graph =
V01 -> V08
V01 -> V09
V01 -> V16
V02 -> V10
V02 -> V11
V03 -> V15
V03 -> V17
V04 -> V08
V04 -> V16
V05 -> V02
V05 -> V03
V06 -> V09
V07 -> V02
V07 -> V12
V08 -> V09
V08 -> V15
V09 -> V10
V10 -> V11
V10 -> V12
V11 -> V12
V11 -> V12
V12 -> V13
V12 -> V14
V13 -> V16
V14 -> V16
V15 -> V16
V16 -> V17
V16 -> V18
V17 -> V18
```

Exact error for both: `The inputs imply a graph with more than 30 nodes.` No identifying or
non-identifying verdict was returned, so Q2's pure computed refusal is not
upgraded to a proved dosearch boundary in this slice.

## Deferred with reason

- dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.
- DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.
- dosearch Q2 boundary: deferred — dosearch 1.0.12 doubles 18 faithful ancestral variables into 36 internal intervention nodes, exceeding its hard limit of 30; both without-S05 and with-S05 queries were rejected before identification search.
- Q2 NDE/NIE decomposition: deferred — dosearch has no path-specific intervention syntax; the attempted query was the joint channel/outcome response P(V13,V14,V18 | do(V07)), not an NDE/NIE proxy.

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/oracle-pass/run.sh
```

The pass regenerates exports and both reports deterministically. It reads only the
exports and the two read-only causal spec JSONs after the Clojure export step.
