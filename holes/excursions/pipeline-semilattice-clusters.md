# Pipeline Semilattice Clusters

Derived for `E-pipeline-pipecleaner`: mission clusters, pattern clusters, and explicit mission->pattern warrant links.

## Sources

- Missions: `/home/joe/code/futon3a/resources/notions/bge_mission_embeddings.json` (229 embedded missions, BGE).
- Patterns: `/home/joe/code/futon3/library` (1010 flexiarg files, TF-IDF/SVD fallback embeddings).
- Warrant links: `/home/joe/code/futon6/data/mission-pattern-scopes.edn` (550 resolved applied links).

Temporal anchor sources: `{'date-field': 105, 'early-text-date': 53, 'git-first-commit': 29, 'missing': 3, 'status-stamp': 39}`.

## Mission Clusters

- M0 size=161 terms=stack, pattern, evidence, m-substrate-metric, clj, patterns; sample=M-capability-star-map, M-futonzero-capability, M-futonzero-generative, M-futonzero-mvp, M-futonzero-prelim-practice
  - M0.2 size=56; sample=M-futonzero-capability, M-futonzero-generative, M-futonzero-mvp, M-futonzero-prelim-practice
  - M0.1 size=41; sample=M-capability-star-map, M-agency-forum, M-mission-coherence-patterns, M-pattern-application-diagnostic
  - M0.3 size=40; sample=M-reflective-discipline, M-f6-arxiv, M-f6-eval, M-graph-unification
  - M0.0 size=24; sample=M-bounded-in-flight-state, M-war-machine, M-war-machine-first-outing, M-war-machine-first-outing-expectations
- M1 size=27 terms=peripheral, essays, parity, emacs, plan, m-editorial-assistant; sample=M-arxana-graph-persistence, M-codex-parity, M-coordination-rewrite, M-emacs-cursor-peripheral, M-f6-agents
  - M1.1 size=13; sample=M-codex-parity, M-coordination-rewrite, M-f6-agents, M-f6-ingest
  - M1.2 size=9; sample=M-arxana-graph-persistence, M-editorial-assistant, M-essays-diachronic-model, M-essays-edit-cycle
  - M1.0 size=4; sample=M-peripheral-phenomenology, M-social-exotype, M-peeragogy-rewrite, M-peeragogy-rewrite
  - M1.3 size=1; sample=M-emacs-cursor-peripheral
- M2 size=4 terms=date, m-operational-readiness, stability, conversation, moved, head; sample=M-IRC-stability, M-agency-hardening, M-operational-readiness, M-pilot-appearance
  - M2.0 size=4; sample=M-IRC-stability, M-agency-hardening, M-operational-readiness, M-pilot-appearance
- M3 size=3 terms=maintenance, probe, section, concepts, identity, keeps; sample=M-d11-maintenance-test, M-d12-probe2, M-walkie-talkie
  - M3.0 size=3; sample=M-d11-maintenance-test, M-d12-probe2, M-walkie-talkie
- M4 size=3 terms=forum, claude, assign, tbd, owner, open; sample=M-forum-organization, M-labs-integration, M-par-session-punctuation
  - M4.0 size=3; sample=M-forum-organization, M-labs-integration, M-par-session-punctuation
- M5 size=13 terms=m-or-training-as-learning-system, m-simulating-or-training-as-learning-system, ukrn-s, training, research, open; sample=M-open-learning-system, M-apm-solutions, M-or-training-as-learning-system, M-or-training-as-learning-system.v1, M-simulating-or-training-as-learning-system
  - M5.1 size=5; sample=M-open-learning-system, M-or-training-as-learning-system, M-or-training-as-learning-system.v1, M-another-university
  - M5.0 size=4; sample=M-simulating-or-training-as-learning-system, M-or-training-as-learning-system, M-or-training-as-learning-system.v1, M-simulating-or-training-as-learning-system
  - M5.3 size=3; sample=M-ukrns-wp-codex-eval-brief, M-ukrns-wp-preregistration, M-ukrns-wp-refactor-brief
  - M5.2 size=1; sample=M-apm-solutions
- M6 size=8 terms=codex, cli, irc, agency, agent, claude; sample=M-agency-unified-routing, M-drawbridge-multi-agent, M-make-agency-work-properly, M-understand-fucodex, M-codex-agent-behaviour
  - M6.0 size=4; sample=M-make-agency-work-properly, M-codex-agent-behaviour, M-codex-irc-execution, M-improve-irc
  - M6.1 size=2; sample=M-agency-unified-routing, M-drawbridge-multi-agent
  - M6.2 size=2; sample=M-understand-fucodex, M-repl-wins-over-cli
- M7 size=1 terms=; sample=M-the-perfect-crime
  - M7.0 size=1; sample=M-the-perfect-crime
- M8 size=1 terms=hud, lines, stack, frame, view, decision; sample=M-stack-hud-refactor
  - M8.0 size=1; sample=M-stack-hud-refactor
- M9 size=1 terms=pattern, far, discovery, written, improves, learned; sample=M-alfworld-pattern-discovery
  - M9.0 size=1; sample=M-alfworld-pattern-discovery
- M10 size=1 terms=reader, evaluation; sample=M-ukrns-wp-reader-kate
  - M10.0 size=1; sample=M-ukrns-wp-reader-kate
- M11 size=6 terms=webarxana, m-web-arxana-missions, m-web-arxana-ui-improvements, ought, interface, graph; sample=M-mission-scopes-into-substrate-2, M-arxana-roundtrip, M-web-arxana-ui-improvements, M-webarxana, M-web-arxana-missions
  - M11.0 size=5; sample=M-mission-scopes-into-substrate-2, M-arxana-roundtrip, M-web-arxana-ui-improvements, M-web-arxana-missions
  - M11.1 size=1; sample=M-webarxana

## Pattern Clusters

- P0 size=126 terms=reader, paper, claim, ukrns, design, project; sample=collaboration-coherence/isolation, collaboration-coherence/magical-thinking, collaboration-coherence/messy-with-lurkers, collaboration-coherence/misunderstanding-power-laws, collaboration-coherence/navel-gazing
  - P0.4 size=32; sample=math-strategy/preemptive-objection-clearance, system-coherence/argue-empirically-not-persuasively, system-coherence/argument-as-coverage-checklist-vs-system-specification, system-coherence/match-region-weight-to-load
  - P0.1 size=31; sample=system-coherence/bind-open-questions-to-closure-mechanisms, system-coherence/correct-effect-claims-with-relevant-denominators, system-coherence/facet-before-aggregating, system-coherence/place-capabilities-at-the-scale-of-their-absence
  - P0.0 size=27; sample=collaboration-coherence/isolation, collaboration-coherence/magical-thinking, collaboration-coherence/messy-with-lurkers, collaboration-coherence/misunderstanding-power-laws
  - P0.3 size=22; sample=plos-npt-with-small-n/absence-as-evidence, plos-npt-with-small-n/abstract-n-method-frame, plos-npt-with-small-n/barrier-enabler-strategy-display, plos-npt-with-small-n/coding-reliability-stated
- P1 size=73 terms=futon-theory, storage, coordination, proof, futon1a, gate; sample=coordination/ARGUMENT, coordination/artifact-registration, coordination/assignment-binding, coordination/bounded-execution, coordination/capability-gate
  - P1.1 size=27; sample=futon-theory/agent-contract, futon-theory/baldwin-cycle, futon-theory/curry-howard-operational, futon-theory/event-protocol
  - P1.0 size=25; sample=futon-theory/all-or-nothing, futon-theory/counter-ratchet, futon-theory/durability-first, futon-theory/error-hierarchy
  - P1.2 size=13; sample=coordination/ARGUMENT, coordination/artifact-registration, coordination/assignment-binding, coordination/bounded-execution
  - P1.4 size=5; sample=futon-theory/mission-dependency, futon-theory/mission-lifecycle, futon-theory/mission-scoping, futon-theory/progress-signal
- P2 size=92 terms=agent, aif, peripheral, eight-gates, agents, energy; sample=agency/self-attribution, agent/budget-bounds-exploration, agent/commitment-varies-with-confidence, agent/coordination-has-cost, agent/environment-over-optimization
  - P2.1 size=37; sample=agency/self-attribution, agent/budget-bounds-exploration, agent/commitment-varies-with-confidence, agent/coordination-has-cost
  - P2.0 size=20; sample=aif/belief-aware-risk-term, aif/belief-state-operational-hypotheses, aif/candidate-pattern-action-space, aif/decomposed-prediction-noise
  - P2.3 size=16; sample=eight-gates/an-push, eight-gates/cai-pluck, eight-gates/elbow-immediate, eight-gates/ji-press
  - P2.4 size=11; sample=pattern-discipline/peripheral-as-sokoban, peripherals/canonical-typed-event-vs-side-channel, peripherals/constrained-execution-envelope, peripherals/hot-reload-as-default-fix-path
- P3 size=63 terms=programme, trainers, research, training, institutional, governance; sample=corps/carrying-ones-own-question, corps/five-arrows, corps/letting-the-trace-teach, corps/seeing-the-surrounding-conditions, corps/standing-in-the-circle
  - P3.0 size=32; sample=equity/ARGUMENT, equity/confident-adaptation-as-published, equity/confident-adaptation-merged, equity/confident-adaptation
  - P3.2 size=9; sample=or2/bioliteracy-open-value, or2/data-intensive-infrastructure-gap, or2/open-practice-beyond-hypothesis, or2/shared-futures-beyond-atomised-incentives
  - P3.1 size=8; sample=repository-transition/counterstrawman-indicators, repository-transition/counterstrawman, repository-transition/diagnostics, repository-transition/identity-space
  - P3.4 size=8; sample=t4r/dtp-fit, t4r/exec-summary, t4r/legacy, t4r/rationale
- P4 size=32 terms=hdm, ai4ci, reasoning, mathematical, cues, kernel; sample=ai4ci/ai-core-function, ai4ci/ai-method-role, ai4ci/collective-intelligence-evaluation, ai4ci/collective-intelligence-framing, ai4ci/dual-layer-proof-patterns
  - P4.2 size=13; sample=ai4ci/ai-core-function, ai4ci/ai-method-role, ai4ci/collective-intelligence-evaluation, ai4ci/collective-intelligence-framing
  - P4.3 size=7; sample=hdm/deep-storage-to-active-graph, hdm/human-machine-dialogue, hdm/hyperreal-capital, hdm/non-capture
  - P4.0 size=6; sample=f6/graph-enhanced-evaluation, f6/negative-space-duality, f6/pattern-as-strategy, f6/proof-as-social-process
  - P4.1 size=5; sample=aif/shared-kernel-predictive-forward-model, f6/bootstrap-loop, f6/learning-event-detection, f6/stratum-bridge
- P5 size=42 terms=devmap-coherence, devmap, stack-coherence, clause, prototype, readme; sample=aif/measurement-window-hygiene, code-coherence/dead-code-hygiene, contributing/devmap-contribution-protocol, contributing/stack-scan-logging-protocol, devmap-coherence/baseline-freeze
  - P5.2 size=16; sample=code-coherence/dead-code-hygiene, contributing/devmap-contribution-protocol, contributing/stack-scan-logging-protocol, devmap-coherence/devmap-scope-discipline
  - P5.1 size=8; sample=devmap-coherence/ifr-f0-sati, devmap-coherence/ifr-f2-viriya, devmap-coherence/ifr-f3-piti, devmap-coherence/ifr-f4-passaddhi
  - P5.0 size=7; sample=devmap-coherence/prototype-alignment-embedding, library-coherence/library-embedding-refresh, library-coherence/pattern-differentiation-alarms, meta/designing-thoughts-not-text
  - P5.3 size=7; sample=devmap-coherence/prototype-alignment-bridge, devmap-coherence/prototype-alignment-role, devmap-coherence/prototype-alignment-tension, devmap-coherence/prototype-maturity-lifecycle
- P6 size=40 terms=operator, shape, eoi, structure, hinge, live; sample=aif/admissibility, aif/no-self-certification, aif/off-continuity-null-discriminates, aif/two-layer-calibration, budgeted-action-selection/mana-gated-work
  - P6.1 size=12; sample=budgeted-action-selection/mana-gated-work, invariant-coherence/drain-channel-shape, p4ng/agent-as-mana-unit-was-the-wrong-unit, structure/backwards-induction
  - P6.0 size=9; sample=code-coherence/subsumption-claim-discipline, invariant-coherence/bounded-disposition, invariant-coherence/protocol-family-naming, invariant-coherence/reachable-from-boot
  - P6.2 size=7; sample=aif/no-self-certification, aif/off-continuity-null-discriminates, aif/two-layer-calibration, futon-theory/crime-relocates-to-a-scarcer-witness
  - P6.4 size=7; sample=aif/admissibility, collaboration-coherence/determined-fork-proto-psr, futon-theory/derive-exits-on-a-minted-sorry, orchestration/consent-gate
- P7 size=10 terms=scan, focus, depositing, org, effort, consulting; sample=depositing/grant-to-pitch, depositing/scan-talk-frame, depositing/upstream-patch-signal, or2/progress-cylinder-assurance, or2/spread-effort-assurance
  - P7.0 size=4; sample=depositing/grant-to-pitch, depositing/scan-talk-frame, depositing/upstream-patch-signal, scan-coherence/mission-anchored-scan
  - P7.1 size=4; sample=workday/agenda, workflow-coherence/sphere-equilibrium, workflow-coherence/weekly-rhythm, workflow-coherence/wip-cap
  - P7.2 size=2; sample=or2/progress-cylinder-assurance, or2/spread-effort-assurance
- P8 size=54 terms=string, musn, realtime, evidence-shape, int, server; sample=agency/bounded-lifecycle, agency/delivery-receipt, agency/identifier-separation, agency/invariants, agency/loud-failure
  - P8.0 size=17; sample=musn/aif-live-scores, musn/declare-scope, musn/expensive-move-consent, musn/fulab-report-block
  - P8.3 size=13; sample=fulab/fulab-patterns, library-coherence/from-argument-to-pilot, library-coherence/llm-argument-scan, pattern-coherence/applicability-signals
  - P8.4 size=12; sample=realtime/authoritative-transcript, realtime/branch-parallelism, realtime/learn-as-you-go, realtime/listener-leases
  - P8.2 size=8; sample=agency/bounded-lifecycle, agency/delivery-receipt, agency/identifier-separation, agency/invariants
- P9 size=64 terms=description, iching, interpretation, target, return, dynamics; sample=iching/hexagram-01-qian, iching/hexagram-02-kun, iching/hexagram-03-zhun, iching/hexagram-04-meng, iching/hexagram-05-xu
  - P9.1 size=41; sample=iching/hexagram-03-zhun, iching/hexagram-04-meng, iching/hexagram-05-xu, iching/hexagram-06-song
  - P9.0 size=12; sample=iching/hexagram-09-xiaochu, iching/hexagram-17-sui, iching/hexagram-19-lin, iching/hexagram-26-dachu
  - P9.2 size=4; sample=iching/hexagram-21-shihe, iching/hexagram-39-jian, iching/hexagram-40-jie, iching/hexagram-43-guai
  - P9.3 size=4; sample=iching/hexagram-01-qian, iching/hexagram-02-kun, iching/hexagram-11-tai, iching/hexagram-12-pi
- P10 size=38 terms=vsatlas, stories, vsatlatarium, story, define, constellation; sample=vsatelier/annotation-as-commitment, vsatelier/cluster-as-agenda, vsatelier/decision-provenance, vsatelier/federated-cosmoses, vsatelier/projection-independence
  - P10.0 size=16; sample=vsatelier/federated-cosmoses, vsatlas/askew-layer, vsatlas/audience-shift, vsatlas/economic-resonance-layer
  - P10.4 size=8; sample=vsatlatarium/anthology-as-filter, vsatlatarium/arc-as-typed-relation, vsatlatarium/cached-layout-computation, vsatlatarium/cluster-as-story
  - P10.1 size=7; sample=vsatlas/accountable-poc, vsatlas/current-strength-gap, vsatlas/minimal-linking-pilot, vsatlas/offer-ladder
  - P10.3 size=6; sample=vsatelier/annotation-as-commitment, vsatelier/cluster-as-agenda, vsatelier/decision-provenance, vsatelier/projection-independence
- P11 size=16 terms=right, noble, liberation, mundane, mindfulness, wrong; sample=liberation/mundane/right-action, liberation/mundane/right-concentration, liberation/mundane/right-effort, liberation/mundane/right-intention, liberation/mundane/right-livelihood
  - P11.2 size=8; sample=liberation/noble/right-action, liberation/noble/right-concentration, liberation/noble/right-effort, liberation/noble/right-intention
  - P11.0 size=5; sample=liberation/mundane/right-action, liberation/mundane/right-intention, liberation/mundane/right-livelihood, liberation/mundane/right-speech
  - P11.1 size=2; sample=liberation/mundane/right-concentration, liberation/mundane/right-mindfulness
  - P11.3 size=1; sample=liberation/mundane/right-effort
- P12 size=12 terms=exotic, vision, basecamp, naturality, programming, exotic-programming-curriculum; sample=exotic/construction-discipline, exotic/dual-track-generation, exotic/dual-windowing, exotic/full-lift-registry, exotic/graded-naturality
  - P12.0 size=5; sample=exotic/dual-track-generation, exotic/full-lift-registry, exotic/live-sync-source-truth, exotic/template-driven-initialization
  - P12.2 size=5; sample=exotic/construction-discipline, exotic/dual-windowing, exotic/graded-naturality, exotic/hybrid-execution-semantics
  - P12.1 size=2; sample=exotic/semi-robust-baselines, system-coherence/single-seed-results-need-multi-seed-validation
- P13 size=65 terms=math-informal, api, prove, object, proof, math-formalization; sample=agent/hypothetical-proof-architecture, agent/reduction-to-kernel, career-coherence/free-solo-vs-rope, math-formalization/ae-integral-zero, math-formalization/coercion-bridge
  - P13.0 size=22; sample=agent/reduction-to-kernel, math-informal/argue-by-contradiction, math-informal/check-the-extreme-cases, math-informal/dualise-the-problem
  - P13.3 size=14; sample=math-formalization/ae-integral-zero, math-formalization/coercion-bridge, math-formalization/complex-polynomial-bound, math-formalization/connected-union-via-common-point
  - P13.1 size=12; sample=math-informal/complexity-classification, math-informal/exhaustion-as-theorem, math-informal/failure-mode-characterization, math-informal/quotient-by-irrelevance
  - P13.4 size=11; sample=math-formalization/construction-cost-asymmetry, math-informal/construct-an-explicit-witness, math-informal/construct-auxiliary-object, math-informal/local-to-global
- P14 size=7 terms=enrichment, layer, complexity, indentation, churn, layers; sample=enrichment/ARGUMENT, enrichment/churn-as-signal, enrichment/extend-not-rewrite, enrichment/indentation-as-complexity, enrichment/layer-as-evidence
  - P14.1 size=4; sample=enrichment/ARGUMENT, enrichment/layer-as-evidence, enrichment/layered-ingestion, enrichment/rational-reconstruction
  - P14.0 size=2; sample=enrichment/churn-as-signal, enrichment/indentation-as-complexity
  - P14.2 size=1; sample=enrichment/extend-not-rewrite
- P15 size=3 terms=campaign, standard, campaign-coherence, dependent, standard-verify, escrow; sample=campaign-coherence/campaign-as-temporary-institution, campaign-coherence/cross-mission-escrow, campaign-coherence/shared-standard-has-no-single-owner
  - P15.0 size=3; sample=campaign-coherence/campaign-as-temporary-institution, campaign-coherence/cross-mission-escrow, campaign-coherence/shared-standard-has-no-single-owner
- P16 size=16 terms=sidecar, docs, portal, chain, store, facts; sample=devmap-coherence/ifr-f3a-piti-audit, portal/first-class-query-interface, portal/portal-command-surface, portal/portal-sidecar-interop, sidecar/append-only-semantic-audit
  - P16.3 size=9; sample=sidecar/append-only-semantic-audit, sidecar/chain-builder-softness-accounting, sidecar/explicit-promotion-to-facts, sidecar/fact-lifecycle-event-types
  - P16.1 size=4; sample=devmap-coherence/ifr-f3a-piti-audit, portal/first-class-query-interface, portal/portal-command-surface, portal/portal-sidecar-interop
  - P16.0 size=2; sample=sidecar/bridge-triple-escalator, sidecar/typed-kolmogorov-arrows
  - P16.2 size=1; sample=sidecar/artifact-entity-mention-grounding
- P17 size=257 terms=exotype, edn, resources, lift, exotype-program-manifest, exotype-xenotype-lift; sample=iiching/TEMPLATE, iiching/exotype-000, iiching/exotype-001, iiching/exotype-002, iiching/exotype-003
  - P17.1 size=155; sample=iiching/TEMPLATE, iiching/exotype-001, iiching/exotype-003, iiching/exotype-005
  - P17.2 size=68; sample=iiching/exotype-000, iiching/exotype-002, iiching/exotype-004, iiching/exotype-007
  - P17.3 size=29; sample=iiching/exotype-013, iiching/exotype-030, iiching/exotype-034, iiching/exotype-044
  - P17.0 size=4; sample=iiching/exotype-014, iiching/exotype-061, iiching/exotype-167, iiching/exotype-224

## Strongest Cluster-Level Warrant Links

- M0 -> P2 weight=118
- M0 -> P13 weight=94
- M0 -> P1 weight=86
- M0 -> P6 weight=45
- M0 -> P8 weight=31
- M0 -> P0 weight=30
- M0 -> P5 weight=20
- M0 -> P4 weight=17
- M0 -> P16 weight=13
- M5 -> P2 weight=12
- M5 -> P0 weight=10
- M2 -> P2 weight=9
- M1 -> P13 weight=8
- M1 -> P2 weight=8
- M1 -> P4 weight=6
- M1 -> P0 weight=6
- M0 -> P14 weight=5
- M6 -> P8 weight=4
- M0 -> P10 weight=4
- M0 -> P3 weight=3
- M2 -> P8 weight=2
- M6 -> P1 weight=2
- M0 -> P12 weight=2
- M0 -> P7 weight=2
- M0 -> P9 weight=2

## Direct Pattern Warrants By Mission Cluster

For the upward cascade, this section is stronger than pattern-cluster proximity: it lists the concrete `futon3/library` patterns actually cited by missions inside each mission cluster.

- M0 terms=stack, pattern, evidence, m-substrate-metric, clj; cited-patterns=211; citing-missions=76
  - `futon-theory/stop-the-line` count=13; cited-by=M-action-cost-modelling, M-aif-head, M-bounded-in-flight-state, M-coordination-rewrite
  - `invariant-coherence/reachable-from-boot` count=9; cited-by=M-bounded-in-flight-state, M-interim-director-proxy-metric-inventory, M-live-geometric-stack, M-reachable-from-boot
  - `math-informal/reduce-to-known-result` count=9; cited-by=M-P3-rational-reconstruction, M-P7-rational-reconstruction, M-P8-rational-reconstruction, M-f6-arxiv
  - `futon-theory/structural-tension-as-observation` count=8; cited-by=M-aif-head, M-capability-star-map, M-categorical-code, M-coordination-rewrite
  - `peripherals/surface-earns-inhabitation` count=8; cited-by=M-a-sorry-enterprise, M-futonzero-prelim-practice, M-recommendation-bindings, M-stack-inhabitation
- M1 terms=peripheral, essays, parity, emacs, plan; cited-patterns=29; citing-missions=6
  - `f6/pattern-as-strategy` count=2; cited-by=M-f6-agents, M-f6-recursive
  - `agency/single-routing-authority` count=1; cited-by=M-peripheral-phenomenology
  - `agent/evidence-over-assertion` count=1; cited-by=M-peripheral-phenomenology
  - `agent/pause-is-not-failure` count=1; cited-by=M-peripheral-phenomenology
  - `collaboration-coherence/messy-with-lurkers` count=1; cited-by=M-peeragogy-rewrite
- M2 terms=date, m-operational-readiness, stability, conversation, moved; cited-patterns=14; citing-missions=3
  - `agent/sense-deliberate-act` count=1; cited-by=M-pilot-appearance
  - `aif/expected-free-energy-scorecard` count=1; cited-by=M-pilot-appearance
  - `aif/predictive-coding-belief-update` count=1; cited-by=M-pilot-appearance
  - `aif/structured-observation-vector` count=1; cited-by=M-pilot-appearance
  - `futon-theory/stop-the-line` count=1; cited-by=M-pilot-appearance
- M5 terms=m-or-training-as-learning-system, m-simulating-or-training-as-learning-system, ukrn-s, training, research; cited-patterns=23; citing-missions=2
  - `system-coherence/argue-empirically-not-persuasively` count=2; cited-by=M-or-training-as-learning-system, M-simulating-or-training-as-learning-system
  - `aif/belief-state-operational-hypotheses` count=1; cited-by=M-simulating-or-training-as-learning-system
  - `aif/candidate-pattern-action-space` count=1; cited-by=M-simulating-or-training-as-learning-system
  - `aif/evidence-precision-registry` count=1; cited-by=M-simulating-or-training-as-learning-system
  - `aif/expected-free-energy-scorecard` count=1; cited-by=M-simulating-or-training-as-learning-system
- M6 terms=codex, cli, irc, agency, agent; cited-patterns=5; citing-missions=4
  - `realtime/structured-events-only` count=2; cited-by=M-codex-agent-behaviour, M-codex-irc-execution
  - `futon-theory/all-or-nothing` count=1; cited-by=M-improve-irc
  - `futon-theory/single-source-of-truth` count=1; cited-by=M-agency-unified-routing
  - `realtime/loop-failure-signals` count=1; cited-by=M-codex-agent-behaviour
  - `realtime/loop-recovery-actions` count=1; cited-by=M-codex-agent-behaviour
- M11 terms=webarxana, m-web-arxana-missions, m-web-arxana-ui-improvements, ought, interface; cited-patterns=4; citing-missions=1
  - `exotic/live-sync-source-truth` count=1; cited-by=M-web-arxana-ui-improvements
  - `hdm/deep-storage-to-active-graph` count=1; cited-by=M-web-arxana-ui-improvements
  - `invariant-coherence/state-snapshot-witness` count=1; cited-by=M-web-arxana-ui-improvements
  - `system-coherence/present-graph-topology-not-adjacency-lists` count=1; cited-by=M-web-arxana-ui-improvements

## M0 Breakdown By Dominant Cited Pattern Cluster

M0 is too broad to read as one feature. This breakdown refactors M0 into explicit refined basins of the form `M0P<n>`, assigning each citing M0 mission to the pattern cluster it cites most often. Pattern clusters are secondary vocabulary labels here, not the primary upward edge.

- M0P2: missions=20; examples=M-war-machine-pilot, M-aif-head, M-portfolio-inference, M-weird-modernism, M-vsatarcs-writer, M-capability-star-map
  - top dominant patterns: `aif/expected-free-energy-scorecard`, `peripherals/surface-earns-inhabitation`, `aif/term-to-channel-traceability`, `social/scope-bounded-handoff`
- M0P1: missions=15; examples=M-futon1a-rebuild, M-coordination-rewrite, M-mission-peripheral, M-structural-law, M-futonzero-capability, M-forum-refactor
  - top dominant patterns: `futon-theory/stop-the-line`, `futon-theory/all-or-nothing`, `coordination/par-as-obligation`, `futon-theory/single-source-of-truth`
- M0P13: missions=11; examples=M-interim-director-proxy-metric-inventory, M-P8-rational-reconstruction, M-diagramprover, M-P3-rational-reconstruction, M-P7-rational-reconstruction, M-superpod-mark3
  - top dominant patterns: `math-informal/reduce-to-known-result`, `math-informal/construct-an-explicit-witness`, `math-strategy/compose-independent-lemmas`, `math-informal/split-into-cases`
- M0P6: missions=8; examples=M-bounded-in-flight-state, M-trip-journal, M-self-documenting-stack, M-interest-network-coupling, M-the-futon-stack, M-single-entry-point
  - top dominant patterns: `invariant-coherence/reachable-from-boot`, `invariant-coherence/state-snapshot-witness`, `invariant-coherence/drain-channel-shape`, `structure/block-as-futonic-revolution`
- M0P0: missions=7; examples=M-writing-ethics, M-expressions-of-interest, M-patterns-done-right, M-categorical-code, M-essay-corpus-substrate, M-interim-director
  - top dominant patterns: `pattern-discipline/patterns-as-categorical-objects`, `writing-coherence/meet-the-reader-where-they-are`, `writing-coherence/name-what-you-drop`, `plos-npt-with-small-n/transferability-not-generalisability`
- M0P8: missions=6; examples=M-transport-adapters, M-pattern-ingest, M-cyder, M-evidence-viewer-refinements, M-invariant-queue-unstuck, M-single-locus
  - top dominant patterns: `realtime/learn-as-you-go`, `realtime/structured-events-only`, `agency/single-routing-authority`, `orchestration/state-in-substrate-deltas-in-messages`
- M0P4: missions=3; examples=M-f6-eval, M-artificial-stack-exchange, M-self-improvement-loop
  - top dominant patterns: `f6/negative-space-duality`, `f6/self-play-loop`, `f6/graph-enhanced-evaluation`, `f6/learning-event-detection`
- M0P5: missions=3; examples=M-three-column-stack, M-stack-stereolithography, M-war-machine-tuning
  - top dominant patterns: `stack-coherence/maturity-evidence-audit`, `stack-coherence/ready-blocked-triage`, `devmap-coherence/prototype-maturity-lifecycle`, `stack-coherence/readme-devmap-sync`
- M0P7: missions=1; examples=M-daily-scan
  - top dominant patterns: `scan-coherence/mission-anchored-scan`
- M0P14: missions=1; examples=M-futon-enrichment
  - top dominant patterns: `enrichment/churn-as-signal`, `enrichment/extend-not-rewrite`, `enrichment/indentation-as-complexity`, `enrichment/layer-as-evidence`

## Temporal Levels

Temporal anchors use the same coarse mission date rule as `piano_roll.py`: latest dated status stamp, falling back to `**Date:**`.
If a mission has no in-document date, the audit falls back to the mission file's first Git commit date and records that provenance as `:git-first-commit`.

- L0 M4: median=2026-01-31 first=2026-01-30 last=2026-02-06 dated=3
- L1 M1: median=2026-02-20 first=2026-01-26 last=2026-05-24 dated=27
- L2 M9: median=2026-02-20 first=2026-02-20 last=2026-02-20 dated=1
- L3 M6: median=2026-02-27 first=2026-01-31 last=2026-03-29 dated=8
- L4 M3: median=2026-03-08 first=2026-03-08 last=2026-03-08 dated=1
- L5 M5: median=2026-04-16 first=2026-03-28 last=2026-05-07 dated=13
- L6 M8: median=2026-04-25 first=2026-04-25 last=2026-04-25 dated=1
- L7 M0: median=2026-04-29 first=2026-01-30 last=2026-06-10 dated=161
- L8 M2: median=2026-05-25 first=2026-02-20 last=2026-06-07 dated=4
- L9 M7: median=2026-05-27 first=2026-05-27 last=2026-05-27 dated=1
- L10 M11: median=2026-05-30 first=2026-04-12 last=2026-06-08 dated=6
- L11 M10: median=None first=None last=None dated=0

## Temporal Edge Audit

This audit reads explicit `M-*` references in mission docs as candidate mission->mission edges, classifies the local wording, and flags only the strongest temporal problem: a completion-prerequisite edge whose target mission is later than the source mission in piano-roll time.

- Candidate mission-reference edges: `824`.
- Edge kinds: `{'completion-prerequisite': 13, 'cross-reference': 638, 'elaboration': 45, 'retrospective-warrant': 128}`.
- Verdicts: `{'needs-dates': 3, 'ok': 729, 'ok-as-elaboration-or-reference': 91, 'suspicious-forward-prerequisite': 1}`.
- Suspicious forward prerequisites: `1`.

- `M-peripheral-model` (2026-02-10) -> `M-forum-refactor` (2026-03-08), context: - S-validate: Coordination outcome validation — needs forum (M-forum-refactor)

## Reading For M-stack-stereolithography

Mission clusters are candidate feature basins. Pattern clusters are warrant constellations. The cluster-level links are not mere proximity: they are explicit applied-pattern citations, so they can be read as a first approximation to `warranted-by` edges in a capability semilattice.

Temporal levels should be read as a completion-prerequisite sanity check: an earlier mission can be fully elaborated by a later mission, but it is suspicious for the earlier mission's completion to require a later mission as a prerequisite.

The hierarchy is intentionally shallow in this first pass: each side has coarse clusters plus within-cluster subclusters recorded in the EDN membership rows. This is enough to compare with `M-capability-star-map.graph.edn` without pretending the clusters already are the canonical star map.
