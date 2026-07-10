# M-substrate-metric OR Sample - bounded E1 curvature

Status: complete

**Date:** 2026-06-01  
**Scope:** sampled/capped hop-distance Ollivier-Ricci calculation over the live E1 `feeds-mu?` graph.  
**Guardrail:** read-only E1 relation fetches only; no corpus entity load, embeddings, BGE, JAX, or Fisher-Rao run.

## Command

```bash
python3 scripts/substrate_metric_e1_or_sample.py --limit 2000 --sample 8
```

## Run Summary

| Metric | Value |
|---|---:|
| Per-type fetch cap | 2000 |
| Alpha | 0.5 |
| Nodes | 946 |
| Edges | 1715 |
| Connected components | 5 |
| Structural bridge candidates | 600 |
| Sampled bridge candidates | 8 |
| LP solve time, total | 31.31 ms |
| Total wall time, including HTTP fetch | 742.26 ms |

Fetched hyperedges:

| Relation query | Count |
|---|---:|
| `code/v05/related-mission` | 23 |
| `code/v05/mission-cross-ref` | 655 |
| `code/v05/file->mission` | 0 |
| `code/v05/file→mission` | 1037 |
| `code/v05/sorry->related-missions` | 0 |

Note: counts differ slightly from the R1 report because the live substrate changed after the R1 artifact was written; the top bridge now includes `M-substrate-metric.R1-report`.

## Sampled Curvature

| Kappa | W1 | Solve ms | Relation | Split | Edge |
|---:|---:|---:|---|---|---|
| -0.8583 | 1.8583 | 15.68 | `mission-cross-ref` | `[908 24]` | `futon3c-d/mission/substrate-metric` -- `futon3c-d/mission/substrate-metric.R1-report` |
| -0.5556 | 1.5556 | 3.79 | `mission-cross-ref` | `[3 929]` | `futon4-d/mission/essays-diachronic-model` -- `futon4-elisp-d/mission/essays-edit-cycle` |
| -0.3571 | 1.3571 | 3.36 | `mission-cross-ref` | `[930 2]` | `futon4-d/mission/or-training-as-learning-system.v1` -- `futon4-elisp-d/mission/or-training-as-learning-system` |
| -0.1667 | 1.1667 | 1.85 | `mission-cross-ref` | `[2 930]` | `futon6-d/mission/canon-fingerprint-store` -- `futon6-py-d/mission/bayesian-structure-learning` |
| -0.1667 | 1.1667 | 2.05 | `file->mission` | `[930 2]` | `futon3-d/file/musn/service.clj` -- `futon3-d/mission/native-plan-coherence` |
| 0.3333 | 0.6667 | 1.34 | `related-mission` | `[1 931]` | `futon2-d/sorry/handler-closure-route-rebinding` -- `futon3-d/mission/drawbridge-multi-agent` |
| 0.0345 | 0.9655 | 1.36 | `related-mission` | `[1 931]` | `futon2-d/sorry/r3a-likelihood-attack-coverage` -- `futon7-d/mission/war-machine-aif-completion` |
| 0.0345 | 0.9655 | 1.38 | `related-mission` | `[1 931]` | `futon2-d/sorry/r3a-likelihood-consulting-pct` -- `futon7-d/mission/war-machine-aif-completion` |

## Verdict

Bounded exact OR curvature over hop-distance is feasible on the E1 graph sample. The sample contains both negative bridge-like curvature and non-negative leaf-style sorry edges, which is the expected qualitative shape.

Next scaling step: run a larger capped sample after deciding whether to exclude the current metric/campaign self-artifacts from E1 consumer ranking, since newly written mission reports immediately appear in substrate-2 and can dominate the top structural bridge list.
