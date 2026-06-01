# M-substrate-metric R2 Curvature Full Pass - E1 Propose Surface

**Date:** 2026-06-01  
**Scope:** full current structural-bridge pass over the live E1 `feeds-mu?` graph, with mission-adjacent report/sample artifacts excluded from O1 nodes.

## Command

```bash
python3 scripts/substrate_metric_e1_curvature.py --edge-cap 2000 --top 25 > holes/missions/M-substrate-metric.R2-curvature-full.json
```

## Graph Summary

| Metric | Value |
|---|---:|
| Nodes | 923 |
| Structural edges | 1692 |
| Connected components | 5 |
| Largest component | 909 |
| Structural bridge candidates | 577 |
| OR curvature edges computed | 577 |

The graph is one node / one edge larger than the R1 snapshot because substrate-2 is live and mission-adjacent work continued after the first report. The artifact filter excludes `M-substrate-metric.R*` and `.OR-sample` endpoints from O1 nodes.

## Timing

| Metric | Value |
|---|---:|
| Total runtime | 3.316 seconds |
| Mean per edge | 2.497 ms |
| Max per edge | 22.055 ms |

This confirms that full E1 structural-bridge OR is laptop-scale under the bounded E1 read path.

## Curvature Distribution

| Quantile | Kappa |
|---|---:|
| min | -0.5556 |
| p10 | 0.0167 |
| median | 0.0400 |
| p90 | 0.0909 |
| max | 1.0000 |

The distribution confirms the sampled result: negative curvature is a thin tail, not a flood.

## Top Propose Candidates After R2 Filter

| Rank | Node | Phase | Resolvedness | Min kappa | Action intensity | Strain edge |
|---:|---|---|---:|---:|---:|---|
| 1 | `futon4-d/mission/essays-diachronic-model` | `identify` | 0.10 | -0.5556 | 0.5000 | `futon4-d/mission/essays-diachronic-model` -- `futon4-elisp-d/mission/essays-edit-cycle` |
| 2 | `futon4-d/mission/or-training-as-learning-system.v1` | `identify` | 0.10 | -0.3571 | 0.3214 | `futon4-d/mission/or-training-as-learning-system.v1` -- `futon4-elisp-d/mission/or-training-as-learning-system` |
| 3 | `futon6-d/mission/canon-fingerprint-store` | `derive` | 0.10 | -0.1667 | 0.1500 | `futon6-d/mission/canon-fingerprint-store` -- `futon6-py-d/mission/bayesian-structure-learning` |
| 4 | `futon6-d/mission/bayesian-structure-learning` | `derive` | 0.10 | -0.1667 | 0.1500 | `futon6-d/mission/bayesian-structure-learning` -- `futon6-py-d/mission/symbol-grounding` |

## Verdict

The full current E1 pass confirms the sampled finding. OR-curvature produces a selective negative tail; applying metric-owned resolvedness/actionability yields a small actionable mission set. No hidden large negative set appeared outside the top-200 sampled bridge candidates.
