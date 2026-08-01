# CODEX-HANDOFF — Rung 4: k-step coupled propagation with exploration mass

Mission: `holes/missions/M-memory-retrieval.md` WS1, executing the Rung 4
charter in `holes/excursions/E-dynamic-queries.md` §Rung 4 (chartered dark
by Joe 2026-07-24). Prepared 2026-07-27 by claude-6 (Claude owner).
**Delivery: Agency bell from claude-6. When done, bell claude-6 back with a
summary + commit SHAs.** The Claude owner reviews the diff after the code
lands (author ≠ reviewer).

## Goal

Implement the first genuinely dynamical rung: iterate the coupled updates

    x_{t+1} = Φ(x_t, Δ_{θ_t}, q)        (Rung 1's propagation step)
    θ_{t+1} = Ψ(θ_t, x_{t+1}, q)        (operator update, floor-guarded)

for up to k steps under an explicit budget, over **frozen corpora only**,
in a **fresh namespace** beside `dynamic_queries.clj`. Rungs 1–3 code
paths are untouched. **The spec of record is `E-dynamic-queries.md`
§Rung 4 — read it in full before coding.** This packet adds file-level
scope; where they could disagree, the charter wins.

## Files

`:in` (READ-ONLY — do not modify):
- `holes/excursions/E-dynamic-queries.md` — the charter (§Rung 4).
- `src/futon3c/peripheral/dynamic_queries.clj` — Rung 1/3 code; Rung 4
  calls into it (e.g. reuse the single-step propagation) but never edits it.
- `test/futon3c/peripheral/dynamic_queries_test.clj` — invariant style.
- `holes/labs/M-typed-memories/phase4-wm-corpus.edn` — the frozen corpus.
- `scripts/run_dynamic_queries_demo.clj`,
  `scripts/run_dynamic_queries_rung2_demo.clj`,
  `scripts/run_dynamic_queries_rung3_demo.clj` — demo conventions.

`:out` (create):
- `src/futon3c/peripheral/dynamic_queries_rung4.clj`
- `test/futon3c/peripheral/dynamic_queries_rung4_test.clj`
- `scripts/run_dynamic_queries_rung4_demo.clj`
- `holes/labs/M-typed-memories/rung4-collapse-battery.edn` — the synthetic
  battery corpora (planted target + decoy relation with early accidental
  corroboration), preregistered in the file itself (a header map naming
  target, decoy, and expected floor-off/floor-on behavior).
- `holes/labs/M-typed-memories/rung4-results.edn` — frozen replay results.

## Core function (shape guidance, not a straitjacket)

```clojure
(coupled-propagation
  {:projection      <Phase 4 dark-adapter projection>   ; as Rung 1
   :pattern-activation <initial θ_0>                    ; as Rung 1
   :relation-weights   <fixed weight table>             ; as Rung 1
   :k                  <max steps, pos-int>
   :exploration-floor  <ε ≥ 0; explicit, reported — never defaulted silently>})
;; =>
{:typed-ranking        [...]
 :per-step-trace       [{:step n :contributions [...] :x-entropy _ :theta-entropy _
                         :path-diversity _ :challenge-reachable? _ :theta _} ...]
 :termination          :fixed-point | :cycle | :budget-exhausted
 :exploration-floor    ε
 :control-rankings     {:endpoint [...] :rung1-typed [...]}   ; named counterfactuals
 :candidate-set-preserved? true
 :selected-mission     nil
 :live-ordering-changed? false}
```

## Binding requirements (charter restated as checklist)

1. **Boundary**: Phase 1–4 admissible dark subgraph only; candidate set
   preserved exactly; `:selected-mission nil`;
   `:live-ordering-changed? false`; frozen/synthetic corpora only — **no
   live-store reads**.
2. **Exploration-mass floor**: θ keeps mass ≥ ε on every admitted relation
   type at every step; ε is an explicit reported parameter.
3. **Per-step audit**: contribution rows (Rung 1 discipline), entropy of
   x_t and θ_t, path diversity, challenge-memory reachability, explicit
   termination classification. No silent truncation.
4. **k=1 identity**: with k=1 and the floor inactive, output ranking ==
   Rung 1's ranking exactly (regression identity test required).
5. **Control arms**: fixed endpoint order AND Rung 1 one-step typed
   ranking present as named counterfactuals in every trace.
6. **θ semantics unearned**: iterated θ is a search heuristic, not a
   posterior; no learning beyond the Rung 2 one-outcome ratio; the
   Phase 6 calibration gate (n ≥ 20) is untouched.
7. **Determinism** under stable inputs.

## The confirmation-collapse battery (acceptance core)

- **Floor-off ablation MUST exhibit collapse** (θ concentrates on the
  decoy relation; planted target hidden). If you cannot produce the
  failure, the battery is not probing anything — redesign the corpus,
  do not weaken the assertion.
- **Floor-on run MUST recover the planted target** within the step
  budget, or report a reasoned non-recovery in the trace.
- Rung 1 single-step ranking runs as control on every battery case.
- Independently witnessed challenge memories remain reachable at every
  step of every run (asserted per step, not just at the end).

## Acceptance checklist

- [ ] Executable demo over `phase4-wm-corpus.edn` + the synthetic battery
      (`clojure -M scripts/run_dynamic_queries_rung4_demo.clj` completes,
      printing floor-off collapse and floor-on recovery side by side).
- [ ] k=1 identity test green.
- [ ] Collapse/recovery pair demonstrated in tests, not only the demo.
- [ ] Per-step traces deterministic and fully explained.
- [ ] `rung4-results.edn` parses as EDN; battery prereg header present.
- [ ] Rungs 1–3 files untouched (`git diff --stat` shows only `:out` files).
- [ ] Focused suite green: existing dynamic-queries + memory tests still
      pass (`clojure -X:test` for the touched namespaces at minimum).
- [ ] `clj-kondo` 0 errors 0 warnings on new files.
- [ ] `futon4/dev/check-parens.el` clean on new .clj files.
- [ ] Bell claude-6 with summary + commit SHAs.

## Explicitly out of scope

Live-store reads; any effect on live ordering; multi-coordinate
θ-learning promotion; consuming Rung 3 entropies as outcome
probabilities; relaxing the Phase 6 gate; edits to Rung 1–3 namespaces,
tests, demos, or fixtures.
