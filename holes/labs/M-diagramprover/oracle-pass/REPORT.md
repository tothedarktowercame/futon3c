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
| Book-of-Why d-sep verdicts × NetworkX | 10 | 0 |
| Smoking backdoor exhaustion × dagitty | 1 | 0 |

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

## dosearch Q2 latent projection

The intended path-specific V13/V14 mediation decomposition does not map to
dosearch's query language. The faithful query attempted instead was the joint
channel/outcome response after latent-projecting the exact ancestral reduction.
The auditable kept set is
`{V07, V12, V13, V14, V16, V17, V18}`. The projection has
7 observed nodes and 14
dosearch internal nodes, below the package limit of
30.

The without-S05 result is non-identifiable; no formula returned. The
with-S05 result is non-identifiable; no formula returned. These are
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
bidirected arcs from the still-dropped {V03, V05, V06, V08}) did not
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
data = p(V07,V12,V16,V17,V18)
query = p(V13,V14,V18|do(V07))
graph =
V07 -> V12
V12 -> V13
V12 -> V14
V13 -> V16
V14 -> V16
V16 -> V17
V16 -> V18
V17 -> V18
V12 <-> V16
V12 <-> V17
V16 <-> V17
```

With S05 — exact arguments:

```text
data = p(V07,V12,V13,V14,V16,V17,V18)
query = p(V13,V14,V18|do(V07))
graph =
V07 -> V12
V12 -> V13
V12 -> V14
V13 -> V16
V14 -> V16
V16 -> V17
V16 -> V18
V17 -> V18
V12 <-> V16
V12 <-> V17
V16 <-> V17
```

The `<->` lines are bidirected arcs induced by the shared latent ancestry removed
by `admg/latent-project`. No proxy estimand was substituted.

## Deferred with reason

- dagitty localTests: deferred — data-dependent; requires M-memory-retrieval cohort data.
- DoWhy gcm.falsify_graph: deferred — data-dependent; requires M-memory-retrieval cohort data.
- Q2 NDE/NIE decomposition: deferred — dosearch has no path-specific intervention syntax; the attempted query was the joint channel/outcome response P(V13,V14,V18 | do(V07)), not an NDE/NIE proxy.

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/oracle-pass/run.sh
```

The pass regenerates exports and both reports deterministically. It reads only the
exports and the two read-only causal spec JSONs after the Clojure export step.
