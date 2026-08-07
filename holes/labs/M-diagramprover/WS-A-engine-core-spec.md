# WS-A: Engine core — Codex handoff spec (v1, 2026-08-02)

Mission: M-diagramprover (`holes/missions/M-diagramprover.md`, §Programme
of Work). This slice is the generic string-diagram rewriting engine core:
a Clojure reconstruction of chyp's kernel.

## Goal

Typed open hypergraphs + convex matching + double-pushout rewriting, with
**rule application as a relation**: applying a rule to a graph returns a
lazy seq of ALL legal rewrites (match + result), not one. Rule sets are
data, pluggable. No causal semantics in this slice — that is WS-B, which
builds on this API.

## Source material (READ-ONLY)

`/home/joe/code/diagramprover-refs/chyp/chyp/` — Apache-2.0, (C) Aleks
Kissinger. Port surface ~2k lines:

- `graph.py` (1136) — `Graph`, `VData`, `EData`: vertices carry
  `in_indices`/`out_indices` port lists; edges are hyperedges with
  ordered `source`/`target` vertex lists; graph has ordered `inputs`/
  `outputs` (the boundary). Key ops: `add_vertex`, `add_edge`,
  `remove_vertex`, `remove_edge`, `merge_vertices`, `explode_vertex`,
  `is_boundary`, `domain`/`codomain`.
- `matcher.py` (522) — `Match` (vertex_map + edge_map, grown
  incrementally via `try_add_vertex`/`try_add_edge`), `Matches`
  (backtracking iterator), `match_graph`, `match_rule`, `find_iso`.
  Note the match validity predicates: `is_total`, `is_surjective`,
  `is_injective`, `is_convex` — convexity is what makes DPO valid here.
- `rewrite.py` (109) — `dpo(rule, match)`: pushout complement (remove
  matched edges, explode boundary vertices, remove interior vertices),
  then embed rhs. Read carefully around `explode_vertex`: chyp raises
  `NotImplementedError` for non-(1,1) boundary vertices ("rewriting
  modulo Frobenius") — we keep exactly the same restriction.
- `rule.py` (58) — a rule is (lhs, rhs) with equal boundary.
- `term.py` (198) — term ↔ graph conversion; port ONLY if time allows
  (nice for tests, not load-bearing).

Do NOT port: `state.py`, `proofstate.py`, `layout.py`, `parser.py`,
`checker.py`, GUI. Do not add Frobenius/non-left-linear support.

## Out files (create)

- `futon3c/src/futon3c/diagramprover/graph.clj`
- `futon3c/src/futon3c/diagramprover/matcher.clj`
- `futon3c/src/futon3c/diagramprover/rewrite.clj`
- `futon3c/src/futon3c/diagramprover/rule.clj`
- `futon3c/test/futon3c/diagramprover/*_test.clj`

## Design constraints

1. **Immutable data.** chyp mutates; we don't. `Graph` is a plain map
   `{:vdata {int VData} :edata {int EData} :inputs [int] :outputs [int]
   :vindex int :eindex int}`. Every op returns a new graph. VData/EData
   as maps, not records, so rule sets can attach extra keys (WS-B will
   add causal annotations).
2. **Relations, not functions.** `(matches dom cod)` and
   `(rule-applications rule g)` return LAZY seqs — the backtracking
   iterator in `Matches.__next__` becomes a lazy-seq producer. Every
   match found by chyp must be found by us (same completeness).
3. **Values as edge/vertex payloads.** `EData.value` (the generator
   name) and `VData.vtype` port over as `:value` / `:vtype` keywords/
   strings. Matching requires equal `:value` and arity-compatibility
   exactly as `try_add_edge` does.
4. **Pure kernel.** No I/O, no atoms, no defs of mutable state. Must be
   loadable over Drawbridge into the live JVM without side effects.

## API (target signatures)

```clojure
(g/make-graph)                          ;; empty graph
(g/add-vertex g {:vtype ... :value ...}) ;; -> [g' v-id]
(g/add-edge g s-ids t-ids {:value ...})  ;; -> [g' e-id]
(g/domain g) (g/codomain g)             ;; boundary types
(m/matches dom cod)                     ;; lazy seq of Match maps
(m/find-iso g1 g2)                      ;; Match or nil
(r/make-rule lhs rhs)                   ;; throws if boundaries differ
(rw/dpo rule match)                     ;; -> {:graph g' :match m'}
(rw/rule-applications rule g)           ;; lazy seq of (dpo rule m)
                                        ;;   for every convex match
```

Match map: `{:vertex-map {int int} :edge-map {int int}
:domain g :codomain g}`.

## Test expectations (write these; all must pass)

1. Graph construction round-trip: build the example from chyp's README
   (or any 2-in/1-out generator composed with a 1-in/1-out), check
   vertices/edges/boundary.
2. Matching completeness: a graph with two occurrences of generator `f`
   → `(matches lhs g)` yields exactly 2 matches; a symmetric lhs yields
   the symmetry-doubled count (derive expected count by hand, document
   it in the test).
3. Convexity: construct the standard non-convex situation (path leaving
   and re-entering the matched subgraph) and assert the match is
   rejected.
4. DPO correctness: apply a rule `f;g → h` to a chain `f;g;k`, assert
   result is iso to `h;k` via `find-iso`.
5. Non-applicable rule → `rule-applications` returns `()` (empty, not
   nil, not throw).
6. Boundary preservation property: for any successful dpo, `(domain g')
   = (domain g)` and `(codomain g') = (codomain g)`.
7. Differential (bonus, if straightforward): run chyp from Python on
   the same example and compare match counts.

## Gates

- `clj-kondo` clean on all new files.
- `futon4/dev/check-parens.el` on all new files.
- `clojure -X:test` passes (all existing + new tests).
- No new deps beyond what futon3c already has (pure Clojure).

## Acceptance bar

The four modules load over Drawbridge, all tests pass, and the README
example of chyp can be reproduced in a REPL session: build two graphs,
make a rule, enumerate its applications on a host graph, rewrite, and
check the result by iso. Include that REPL transcript in the completion
report.

Bell the dispatching Claude back with: summary, commit shas, test
output, and the REPL transcript.
