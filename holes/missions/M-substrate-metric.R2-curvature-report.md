# M-substrate-metric R2 Curvature Sample - E1 Propose Surface

**Date:** 2026-06-01  
**Scope:** bounded on-box Ollivier-Ricci sample over the E1 `feeds-mu?` graph.  
**Input graph:** canonical-filtered R1 graph; mission-adjacent report/sample artifacts excluded from O1 nodes.

## Command

```bash
python3 scripts/substrate_metric_e1_curvature.py --edge-cap 200 --top 25 > holes/missions/M-substrate-metric.R2-curvature-sample.json
```

## Runtime Posture

- Reads only E1 `feeds-mu?` hyperedge families plus bounded mission/sorry docs for `:resolution-state`.
- No full entity corpus.
- No BGE / JAX / Fisher-Rao.
- Curvature sample uses exact sparse transport via `scipy.optimize.linprog`.
- Edge sample: top `200` structural bridge candidates from the R1 graph.

## Graph Summary

| Metric | Value |
|---|---:|
| Nodes | 922 |
| Structural edges | 1691 |
| Connected components | 5 |
| Largest component | 908 |
| Structural bridge candidates | 576 |
| OR curvature edges sampled | 200 |

## Timing

| Metric | Value |
|---|---:|
| Total runtime | 2.079 seconds |
| Mean per edge | 1.279 ms |
| Max per edge | 9.377 ms |

This timing supports scaling beyond the 200-edge sample on the laptop, still under the E1-only memory guardrail.

## Curvature Distribution

For sampled edges:

| Quantile | Kappa |
|---|---:|
| min | -0.5556 |
| p10 | 0.0164 |
| median | 0.0417 |
| p90 | 0.0909 |
| max | 0.5000 |

Interpretation: only a small head of the bridge-ranked sample is actually negative under lazy-walk OR curvature. This confirms E1's warning: bridge-candidate status alone is too permissive; `min-incident-kappa` magnitude is the selector.

## Top Propose Candidates After R2 Filter

Filter applied:

```text
propose-here? =
  curvature-strain?
  AND numeric(resolvedness)
  AND resolvedness < 1.0
  AND actionable?
```

Files are non-actionable directly and route through `file->mission`; closed/complete nodes do not fire.

| Rank | Node | Phase | Resolvedness | Min kappa | Action intensity | Strain edge |
|---:|---|---|---:|---:|---:|---|
| 1 | `futon4-d/mission/essays-diachronic-model` | `identify` | 0.10 | -0.5556 | 0.5000 | `futon4-d/mission/essays-diachronic-model` -- `futon4-elisp-d/mission/essays-edit-cycle` |
| 2 | `futon4-d/mission/or-training-as-learning-system.v1` | `identify` | 0.10 | -0.3571 | 0.3214 | `futon4-d/mission/or-training-as-learning-system.v1` -- `futon4-elisp-d/mission/or-training-as-learning-system` |
| 3 | `futon6-d/mission/canon-fingerprint-store` | `derive` | 0.10 | -0.1667 | 0.1500 | `futon6-d/mission/canon-fingerprint-store` -- `futon6-py-d/mission/bayesian-structure-learning` |

No sorry or pattern node fired in this sample. Pattern coverage is absent from current E1 `feeds-mu?` relation families; sorry nodes exist in the graph but did not appear in the sampled negative/actionable head.

## Top Negative Edges

| Kappa | W1 | Relation | Edge |
|---:|---:|---|---|
| -0.5556 | 1.5556 | `mission-cross-ref` | `futon4-d/mission/essays-diachronic-model` -- `futon4-elisp-d/mission/essays-edit-cycle` |
| -0.3571 | 1.3571 | `mission-cross-ref` | `futon4-d/mission/or-training-as-learning-system.v1` -- `futon4-elisp-d/mission/or-training-as-learning-system` |
| -0.1667 | 1.1667 | `mission-cross-ref` | `futon6-d/mission/canon-fingerprint-store` -- `futon6-py-d/mission/bayesian-structure-learning` |
| -0.1667 | 1.1667 | `file->mission` | `futon3-d/file/musn/service.clj` -- `futon3-d/mission/native-plan-coherence` |

The fourth negative edge is file-incident; it contributes strain geometry but does not directly fire because `:file` has `:resolution-state :unknown` and `actionable? false`.

## R2 Verdict

R2 is live on real nodes for the sampled E1 curvature surface. The first bounded OR run returns a small, magnitude-ranked actionable propose set after applying metric-owned resolvedness/actionability. This is the first concrete "where would the E1 tension-proposer fire" answer under the verified metric contract.
