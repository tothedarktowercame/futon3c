# WS-B: Causal rule layer — Codex handoff spec (v1, 2026-08-02)

Mission: M-diagramprover (`holes/missions/M-diagramprover.md`,
§Programme of Work, WS-B and WS-B.0). Builds on the landed WS-A kernel
(`src/futon3c/diagramprover/{graph,matcher,rewrite,rule}.clj`).

**Two stages, dispatched as separate bells.** This document specifies
both; implement ONLY the stage named in your bell. Commit incrementally
— a commit is durable, a job result is not (~30-min job cap).

## Regime constraints (WS-B.0, decided — do not revisit)

The kernel is convex-DPO / plain SMC. Copy/discard are EXPLICIT
generators (edge values `:copy` 1→2, `:discard` 1→0), NOT quotiented
structure. Containment:
- All diagrams enter through the ingest functor below, which emits a
  CANONICAL form; no function in this layer may produce a non-canonical
  diagram (canonical: each variable's fanout is a right-nested comb of
  `:copy` edges, branches ordered by consumer variable id; fanout 1 =
  no copy edge; fanout 0 into a `:discard`).
- No rewriting *search* in this layer: surgery is a deterministic
  transformation, d-separation is a reachability algorithm. Do not
  write comonoid rewrite rules; that is month-2 (MPZ) territory.

## Stage 1 (this bell): ingest, surgery, d-separation, implications, gap marker

### Out files
- `src/futon3c/diagramprover/causal/dag.clj` — DAG model + JSON ingest
- `src/futon3c/diagramprover/causal/diagram.clj` — DAG → canonical string diagram
- `src/futon3c/diagramprover/causal/surgery.clj` — interventions
- `src/futon3c/diagramprover/causal/dsep.clj` — d-separation + implications
- matching `test/futon3c/diagramprover/causal/*_test.clj`

### dag.clj
Ingest the interchange JSON format of
`docs/memory-causal-graph-spec.json` and
`docs/lean-proof-pipeline-causal-spec.json` (READ-ONLY inputs):
- `(load-spec path)` → `{:variables {id {...}} :arrows [{:from :to ...}]
  :leak-edges [...] :interventions [...] :sensors [...]}` — keywordized,
  validated (unknown variable refs throw; cycle check via topological
  sort throws with the cycle named).
- Leak edges are REMOVABLE arrows: `(with-leaks dag)` /
  `(without-leaks dag)` return DAG variants. Leak `from` fields are
  free-text sources — model each as an exogenous latent node named by
  the leak id (K1/L1 etc.) with an arrow to its stated target.
- Sensors are NOT arrows; keep them in metadata only. `observes` never
  enters the adjacency structure.
- Plain adjacency helpers: `parents`, `children`, `ancestors`,
  `descendants`, `exogenous?`.

### diagram.clj
`(dag->diagram dag)` → WS-A kernel graph, canonical form:
- One generator edge per variable (edge value = variable id), inputs =
  its parents' wires, output = one wire, fanned out to its children
  via the canonical copy comb; childless non-outcome variables get
  `:discard`.
- `(canonical? g)` predicate — checks the comb shape and branch
  ordering; every public function returning a diagram must satisfy it
  (assert in tests, not at runtime).
- Round-trip: `(diagram->dag g)` recovers the adjacency structure;
  test `(= dag (diagram->dag (dag->diagram dag)))` on both real specs.

### surgery.clj
All operate on the DAG level and re-render diagrams via `dag->diagram`:
- `(do-intervention dag x)` — cut incoming arrows of x (JKZ surgery on
  the diagram = replace x's generator by an exogenous point; at DAG
  level = drop parent edges). Returns DAG.
- `(cut-outgoing dag x)` — for backdoor-criterion checks.
- `(remove-node dag x)` / `(with-leaks dag)` / `(without-leaks dag)`
  compose with it.

### dsep.clj
- `(d-connected? dag xs ys zs)` — reachability with collider logic
  (Bayes-ball or the dagitty `dConnected` ancestor-set method,
  `diagramprover-refs/dagitty/jslib/graph/GraphAnalyzer.js:960` is the
  reference — read it, port the semantics, not the code style).
- `(d-separated? dag xs ys zs)` = complement.
- `(connecting-paths dag xs ys zs {:limit n})` — at least one witness
  path when d-connected, for receipts ("with the connecting paths
  named"). Exhaustive enumeration not required; witness + count is.
- `(implied-independencies dag {:max-conditioning k})` — enumerate
  testable implications (pairwise non-adjacent variables, minimal
  separating sets up to k), the input to the D2 falsification pass and
  the model-test service. k defaults to 2; document the truncation.
- `(backdoor-adjustment? dag x y zs)` — zs blocks all backdoor paths
  x←…→y, no descendant of x in zs.

### Gap marker (pinned test, REQUIRED)
In `diagram_test.clj`: construct two diagrams equal modulo
cocommutativity of `:copy` (same DAG, two branch orders — build the
second by hand, bypassing `dag->diagram`) and assert
`matcher/find-iso` does NOT identify them, with a comment block:
"Quotient gap, WS-B.0 route (b). This failure is definitional, not a
bug. MPZ extension (arXiv:2204.04274) acceptance = this test flips."

### Stage-1 test expectations (all must pass)
1. Ingest both real specs: correct variable/arrow counts (20/34 memory
   with 4 leak edges; 20/31 lean), cycle-free, unknown-ref rejection on
   a corrupted copy.
2. Round-trip dag→diagram→dag identity on both specs; `canonical?`
   holds on every rendered diagram.
3. d-separation on the three canonical 3-node motifs: chain a→b→c
   (a⊥c | b, NOT a⊥c | ∅), fork a←b→c (same), collider a→b←c
   (a⊥c | ∅, NOT a⊥c | b, NOT a⊥c | descendant-of-b). These are the
   ground-truth cases; hand-derived, no oracle needed.
4. Backdoor: the standard confounded x←u→y, x→y case — {u} is a valid
   adjustment set, ∅ is not.
5. Surgery: after `(do-intervention dag x)`, x has no parents and
   d-connections through former backdoors are gone (test on motif 4).
6. Leak edges: memory spec `without-leaks` removes exactly L1–L4's
   latents; `with-leaks` restores; d-connectivity of a leak-target pair
   changes accordingly.
7. Gap marker test as specified above.
8. `implied-independencies` on the collider motif returns exactly
   {a⊥c | ∅} (k=2), and on the memory spec returns a non-empty list
   (count asserted, spot-check one entry by hand).

## Stage 2 (NEXT bell — do not implement now): receipts.clj
Q1/Q2/Q3 on the memory spec + R-series first question on the lean spec,
receipts as EDN with named paths. Specified fully in the mission and the
spec JSONs' `requested_receipts`; formalization guidance (esp. Q3's
two-topology encoding) will accompany that bell.

## Gates (same as WS-A)
clj-kondo clean; `futon4/dev/check-parens.el` OK; focused
`clojure -X:test :patterns '["futon3c.diagramprover.*"]'` green
(existing 10 kernel tests + new); no new deps; pure modules,
Drawbridge-loadable; no I/O outside `load-spec`.

## Acceptance bar (stage 1)
All 8 test groups pass; REPL transcript in the report: load the memory
spec, render canonical diagram, run `without-leaks` +
`do-intervention` on V07, show a d-separation verdict with a named
path. Bell claude-10 back with summary + commit shas + test output +
transcript.
