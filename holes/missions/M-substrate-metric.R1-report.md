# M-substrate-metric R1 Report - E1 feeds-mu graph

Status: complete

**Date:** 2026-06-01  
**Rung:** Campaign `C-substrate-completion` RUN/DELIVER R1  
**Scope:** bounded, read-only E1 substrate graph extraction. No full entity corpus, code embedding, JAX, BGE, or Fisher-Rao run.

## Command

```bash
clojure -M -e "(require '[futon3c.metric.e1-report :as r]) (prn (r/report {:limit 2000 :top-bridges 20}))"
```

## Fetch Posture

Per-type cap: `2000`.

Fetched hyperedges by type:

| Relation query | Count |
|---|---:|
| `code/v05/related-mission` | 23 |
| `code/v05/mission-cross-ref` | 654 |
| `code/v05/file->mission` | 0 |
| `code/v05/file→mission` | 1014 |
| `code/v05/sorry->related-missions` | 0 |

After E1 `feeds-mu?` filtering:

| Metric | Value |
|---|---:|
| Nodes | 922 |
| Structural edges | 1691 |
| Connected components | 5 |
| Candidate bridge edges | 576 |

Node counts by grain:

| Grain | Count |
|---|---:|
| `:file` | 719 |
| `:mission` | 187 |
| `:sorry` | 16 |

No `:pattern` nodes appeared in this R1 graph because no pattern-bearing `feeds-mu?` relation was present in the queried E1 relation families.

## Connected Components

Top components:

| Size | Node grain mix | Sample |
|---:|---|---|
| 908 | `{:file 714, :mission 178, :sorry 16}` | `.claude-d/file/projects/-home-joe-code/memory/project_aliveness_synthesis.md`, `algorithms-d/file/apm-daily-batch.md`, `algorithms-d/file/arxana-essays-strikethrough-handoff.md`, `algorithms-d/file/author-flexiarg-core-and-wire-discoverability.md`, `algorithms-d/file/de-ai-ify.md`, `algorithms-d/file/eoi-engine-verification-handoff.md`, `algorithms-d/file/eoi-engine.md`, `algorithms-d/file/eoi-finalisation.md` |
| 5 | `{:file 1, :mission 4}` | `futon6-d/file/bayesian_grounding.py`, `futon6-d/mission/bayesian-structure-learning`, `futon6-d/mission/symbol-grounding-scaling-plan`, `futon6-py-d/mission/symbol-grounding`, `futon6-py-d/mission/symbol-grounding-scaling-plan` |
| 4 | `{:mission 2, :file 2}` | `futon6-d/file/math_ast.py`, `futon6-d/file/symbol_grounding.py`, `futon6-d/mission/symbol-grounding`, `futon6-py-d/mission/structure-seed-promotion` |
| 3 | `{:file 2, :mission 1}` | `futon3-d/file/notebook/core.cljs`, `futon3-d/file/src/f2/transport.clj`, `futon3-d/mission/ws-emacs-log-stream` |
| 2 | `{:mission 2}` | `futon3c-d/mission/operational-readiness`, `futon3c-d/mission/operational-readiness-traceability` |

## Candidate Bridges

The extractor reports simple structural bridge candidates: unique endpoint pairs whose removal disconnects their connected component. Scores are the product of split sizes.

Top 20 bridge candidates:

| Score | Split | Relation | Edge |
|---:|---|---|---|
| 2715 | `[3 905]` | `:mission-cross-ref` | `futon4-d/mission/essays-diachronic-model` -- `futon4-elisp-d/mission/essays-edit-cycle` |
| 1812 | `[906 2]` | `:file->mission` | `futon3-d/file/musn/service.clj` -- `futon3-d/mission/native-plan-coherence` |
| 1812 | `[2 906]` | `:mission-cross-ref` | `futon6-d/mission/canon-fingerprint-store` -- `futon6-py-d/mission/bayesian-structure-learning` |
| 1812 | `[906 2]` | `:mission-cross-ref` | `futon4-d/mission/or-training-as-learning-system.v1` -- `futon4-elisp-d/mission/or-training-as-learning-system` |
| 907 | `[1 907]` | `:file->mission` | `futon3c-d/file/logic/probe_taps.clj` -- `futon3c-d/mission/invariant-queue-unstuck` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/library/structure/mana-allostasis.flexiarg` -- `futon3c-d/mission/bounded-in-flight-state` |
| 907 | `[1 907]` | `:file->mission` | `futon4-d/file/dev/web/webarxana/src/webarxana/client/mission_search.cljs` -- `futon4-elisp-d/mission/web-arxana-ui-improvements` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/holes/excursions/E-substrate-metrics.md` -- `futon6-d/mission/superpod-mark3` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/library/math-strategy/hypothesis-category-check.flexiarg` -- `futon6-d/mission/P8-rational-reconstruction` |
| 907 | `[1 907]` | `:file->mission` | `futon-theory-d/file/futonic-logic.flexiarg` -- `futon3-d/mission/weird-modernism` |
| 907 | `[1 907]` | `:file->mission` | `futon3c-d/file/src/futon3c/logic/probe.clj` -- `futon3c-d/mission/archaeology-control` |
| 907 | `[907 1]` | `:file->mission` | `futon3-d/mission/emacs-cursor-peripheral` -- `futon3c-d/file/src/futon3c/transport/protocol.clj` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/gate/observe.clj` -- `futon3b-d/mission/coordination-rewrite` |
| 907 | `[1 907]` | `:file->mission` | `futon7-d/file/analysis-demo/invoices.edn` -- `futon7-d/mission/daily-scan` |
| 907 | `[907 1]` | `:file->mission` | `futon0-d/mission/the-futon-stack-Q6-r12-design-choices` -- `futon2-d/file/src/futon2/aif/intrinsic_values.clj` |
| 907 | `[907 1]` | `:file->mission` | `futon4-d/mission/three-column-stack` -- `futon6-d/file/data/showcases/hypergraph-showcase.json` |
| 907 | `[907 1]` | `:file->mission` | `futon3c-d/mission/war-machine-tuning` -- `futon5-d/file/docs/core-terminal-vocabulary.md` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/drawbridge/codex.clj` -- `futon3-d/mission/agency-unified-routing` |
| 907 | `[1 907]` | `:file->mission` | `futon3-d/file/library/math-strategy/preemptive-objection-clearance.flexiarg` -- `futon6-d/mission/P7-rational-reconstruction` |
| 907 | `[1 907]` | `:file->mission` | `futon3c-d/file/src/futon3c/portfolio/observe.clj` -- `futon4-d/mission/self-representing-stack` |

## R1 Verdict

R1 extraction is live and bounded. The E1 `feeds-mu?` substrate graph is nontrivial on real substrate-2 data, dominated by one large component with several small residual components. The bridge-candidate surface is populated and ready for bounded hop-distance Ollivier-Ricci calculation.

This report does not claim OR curvature values yet; it supplies the graph/count/component/bridge surface requested for R1.
