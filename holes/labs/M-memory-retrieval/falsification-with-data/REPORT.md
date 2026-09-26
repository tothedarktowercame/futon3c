# Retrieval-stage graph falsification with frozen V2 data

The applied `retrieval-stage-causal-spec.json` graph was projected with the engine's `admg/latent-project`; v3/v4 candidates were not used. The observational panel is exactly the 129 offered-phase dispatch receipts. E8 is a schema/operationalization supplement only and is never pooled into that panel.

## Frozen inputs consumed

| artifact | verified sha256 | Appendix A check |
|---|---|---|
| `receipts-export-20260731-all-authors.edn` | `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3` | matches `0cc527e2…` |
| `e8-query-binding-20260803.json` | `07be2f39ee48aa38100aaf5ace7b70bcf2660de4681b0d565daedf510ca7b3a2` | full hash frozen here (not listed in Appendix A) |
| `retrieval-stage-causal-spec.json` | `f258c0abf80f294a21c9b062d391bff8f3dfc91468380e5e3d9080ff88150afe` | full hash frozen here (not listed in Appendix A) |

## Variable mapping

| spec node | status | exact derivation / reason |
|---|---|---|
| `problem-difficulty` | UNMEASURED | Latent by specification; no per-dispatch difficulty instrument. |
| `query-cardinality` | MEASURED | Receipt offered record: count(evidence/body.recall-query.terms). E8 independently supplies count(arms[*].terms), but its 30 arms are not pooled with the 129 dispatches. |
| `query-vocabulary` | MEASURED, CONSTANT | 1 = shipped-builder vocabulary for every offered receipt. E8 interventions are supplementary, not observational rows. |
| `corpus-coverage` | UNMEASURED | Latent by specification; no frozen per-dispatch coverage counter. |
| `pollution` | MEASURED PROXY | Fraction of recall-query.terms containing at least one literal TeX marker matched by [\\{}_^]; 0 for an empty term vector. |
| `text-match-set` | UNMEASURED | Receipts freeze final surfacing, not the normalized text-match candidate set. |
| `pattern-endpoints` | UNMEASURED | surfacing-via=:pattern is a route label, not the endpoint set/count named by this node. |
| `attachment-density` | UNMEASURED | No frozen attachment-store export is joinable per dispatch; E8 store metadata is aggregate. |
| `reachability` | UNMEASURED | E8 manipulates/query-checks five cases, but has no join key to the 129 dispatch panel. |
| `surfaced-set` | MEASURED | Count distinct evidence/body.memory-use.memory-use/surfaced-ids in each offered receipt. |
| `offered-set` | MEASURED | Count distinct memory-id values in evidence/body.memory-use.memory-use/inclusion-reasons. |
| `used-set` | MEASURED, INCOMPLETE | Join offered to the outcome carrying memory-use on job-id; count distinct outcome.memory-use.memory-use/used-ids. Present for 106/129 (one additional instrumented outcome has no matching offered row); no imputation for 23. |
| `use-mode` | UNMEASURED | Only 6/129 outcomes carry memory-use/use-mode; sparse free labels do not define a panel column. |
| `grep-channel` | UNMEASURED | Latent by specification. |
| `runner-outcome` | UNMEASURED | Outcome receipts mix cumulative solved counts, heterogeneous free-form result classes, and 10 absent outcomes; no preregistered common endpoint permits a per-dispatch derivation. |

## Measured latent projection

Nodes (6): `offered-set`, `pollution`, `query-cardinality`, `query-vocabulary`, `surfaced-set`, `used-set`.

Directed edges:
- `offered-set -> used-set`
- `pollution -> offered-set`
- `pollution -> surfaced-set`
- `query-cardinality -> surfaced-set`
- `query-vocabulary -> surfaced-set`
- `surfaced-set -> offered-set`

Bidirected edges: 0.

Projection onto a superset of the measured query variables preserves the observed-margin CI implications; here all non-measured nodes were marked latent before projection.

## dagitty localTests

Counts: survived=4; survived-vacuous=5; violated=2; untestable=0.

A CI is explicitly `survived-vacuous` if any participating column is constant or fewer than five complete rows exist. These are thin-data survivals, not corroboration.

| CI (verbatim) | n complete | p-value | verdict / reason |
|---|---:|---:|---|
| `"offered-set" _\|\|_ "query-cardinality" \| "pollution","surfaced-set"` | 129 | 0.012018719 | violated |
| `"offered-set" _\|\|_ "query-vocabulary" \| "pollution","surfaced-set"` | 129 | — | survived-vacuous: constant/thin column support: offered-set=6, query-vocabulary=1, pollution=6, surfaced-set=6 |
| `"pollution" _\|\|_ "query-cardinality"` | 129 | 0.90514074 | survived |
| `"pollution" _\|\|_ "query-vocabulary"` | 129 | — | survived-vacuous: constant/thin column support: pollution=6, query-vocabulary=1 |
| `"pollution" _\|\|_ "used-set" \| "offered-set"` | 106 | 0.19349335 | survived |
| `"query-cardinality" _\|\|_ "query-vocabulary"` | 129 | — | survived-vacuous: constant/thin column support: query-cardinality=18, query-vocabulary=1 |
| `"query-cardinality" _\|\|_ "used-set" \| "offered-set"` | 106 | 0.88053843 | survived |
| `"query-cardinality" _\|\|_ "used-set" \| "pollution","surfaced-set"` | 106 | 0.86954923 | survived |
| `"query-vocabulary" _\|\|_ "used-set" \| "offered-set"` | 106 | — | survived-vacuous: constant/thin column support: query-vocabulary=1, used-set=5, offered-set=6 |
| `"query-vocabulary" _\|\|_ "used-set" \| "pollution","surfaced-set"` | 106 | — | survived-vacuous: constant/thin column support: query-vocabulary=1, used-set=5, pollution=6, surfaced-set=6 |
| `"surfaced-set" _\|\|_ "used-set" \| "offered-set"` | 106 | 2.3799403e-09 | violated |

### Violated CIs (verbatim)

- `"offered-set" _||_ "query-cardinality" | "pollution","surfaced-set"` — p=0.012018719362762139, n=129
- `"surfaced-set" _||_ "used-set" | "offered-set"` — p=2.3799403207934806e-09, n=106

## Per-edge status

Edge counts: survived=0; violated=0; untestable-as-an-edge=6.

Edges are causal commitments, not conditional-independence nulls, so observational data cannot mark an individual edge ‘survived’. Each projected edge is `untestable-as-an-edge`; falsification attaches to the graph's implied CIs.

| projected edge | status |
|---|---|
| `offered-set -> used-set` | untestable-as-an-edge |
| `pollution -> offered-set` | untestable-as-an-edge |
| `pollution -> surfaced-set` | untestable-as-an-edge |
| `query-cardinality -> surfaced-set` | untestable-as-an-edge |
| `query-vocabulary -> surfaced-set` | untestable-as-an-edge |
| `surfaced-set -> offered-set` | untestable-as-an-edge |

## DoWhy GCM permutation falsification

Status: **tested-complete-case**.

The full projected graph was tested on 106 complete cases with 20 seeded permutations; no graph node was removed and no value was imputed. `query-vocabulary` remains constant, so every GCM local Markov claim involving it is data-thin. DoWhy summary:

```text
+-------------------------------------------------------------------------------------------------------+
|                                         Falsification Summary                                         |
+-------------------------------------------------------------------------------------------------------+
| The given DAG is informative because 0 / 20 of the permutations lie in the Markov                     |
| equivalence class of the given DAG (p-value: 0.00).                                                   |
| The given DAG violates 0/12 LMCs and is better than 95.0% of the permuted DAGs (p-value: 0.05).       |
| Based on the provided significance level (0.05) and because the DAG is informative,                   |
| we do not reject the DAG.                                                                             |
+-------------------------------------------------------------------------------------------------------+
```

DoWhy's kernel-based local Markov checks are not the same finite regression tests as the exact dagitty CI list above; its non-rejection therefore does not cancel either named dagitty violation.

The frozen panel has n=129 (and only 106 complete cases across the measured projection), so power is limited. Dispatches span heterogeneous problems, attempts, runners, and time; treating them as i.i.d. is an additional unverified assumption.

## Reproduction

```sh
holes/labs/M-memory-retrieval/falsification-with-data/run.sh
```
