# Excursion: G incorporates ΔT — pull structural-pressure into the EFE decomposition itself

**Type:** Excursion (E-prefix; bounded scope-out from a mission; multi-cycle pilot work; see [[project_e_prefix_excursions]] for the convention).
**Status:** CYCLE 2 DRAFTED — implementation landed locally + live WM cache recomputed; awaiting claude-1 review / operator ratification before Cycle 3.
**Date:** 2026-05-27
**Author + end-to-end owner:** claude-1 (`:war-machine-pilot` peripheral, emacs-repl surface paired with Joe). Codex-5 likely co-pilot for the implementation cars.
**Parent mission:** `~/code/futon3c/holes/missions/M-action-cost-modelling.md` (this excursion executes v1 of car #4's work; v0 was the post-AIF tie-breaker shipped 2026-05-27 INSTANTIATE).
**Trigger:** Joe's operator-paired observation (emacs-repl, 2026-05-27 evening, post-M-action-cost-modelling DOCUMENT): *"7 G-tied options is still sounding a bit bad to me, like we ran this whole mission b/c 5 G-tied options seemed improbable and we have ended up with 7 G-tied options — but then we did a tie-breaker post-AIF?"* The critique is correct: the v0 tie-breaker is a post-AIF bandage. v1 incorporates the structural signal into G itself.

## Why this exists

M-action-cost-modelling's INSTANTIATE shipped car #4 as a **post-AIF tie-breaker**: after the WM's `judge` produces `:ranked-actions`, a re-sort step (`futon2.report.war-machine/apply-anamnesis-tiebreak`) discriminates tied groups of `:address-sorry` actions by ΔT-anamnesis-concentration over the sorry's related missions. The UI now visibly says *"7 G-tied options · broken by ΔT-anamnesis-concentration (5 distinct levels)."*

The honest reading of that result:

- The 7 tied actions have **identical G-decomposition** (G-risk 0.41, G-amb 0.015, G-info 13.99, G-survival 0.77 — all entries the same). The WM is scoring at the **action-class level** (`:address-sorry`), not per-target — so G cannot discriminate by construction.
- The tie-breaker is a re-sort layer **outside** G. It works (the UI now ranks honestly), but the WM head itself remains blind to the structural neighbourhood of each action target.
- Article 3.8.1's claim ("the WM on the next tick sees a richer manifold and ranks more accurately") is true *of the visible ranking* but false *of G itself*. That's a subsumption-claim discipline failure mode — VSATARCS-as-stack-ARGUE catches us on the very thing we just authored. Recorded honestly in `M-action-cost-modelling §7.6`.

This excursion ships **v1**: pull the structural-pressure signal **into** the EFE decomposition as a new named term. After v1, G itself discriminates per-target. The bandage becomes obsolete because G subsumes it. This is also the empirically-grounded answer to the same critique that motivated the original mission (5-tied at G=−4.422) — the proper fix is to make G richer, not to add a re-sort layer.

## Scope

### IF
You want the WM's EFE-based ranking to discriminate per-action-target rather than per-action-class, AND you want the discrimination to be visible inside G's decomposition rather than as a downstream re-sort.

### HOWEVER
Adding terms to G without principled grounding produces ad-hoc tuning. The new term must satisfy R5 (principled EFE terms with named breakdowns) and have a clearly-articulated semantics that survives `aif/niche-construction.flexiarg`'s false-floor discipline (the term reads non-zero only when the substrate has the typed edges that support it; false-floor stays visible).

### THEN
Define `G-structural-pressure(a)` as a candidate-local structural term derived from the target's mission-neighbourhood, and include it inside the named EFE breakdown rather than as a post-hoc re-sort. For v1, the empirical signal source is the same one the shipped tie-breaker already uses: anamnesis-concentration over the sorry's related missions, computed as `Σ (1.0 - mission-T)` from `futon3c.aif.mission-delta-t/delta-t-mission`. Adjust:

```
G(a) = λ_risk·G-risk + λ_amb·G-amb − 0.4·G-info + 1.2·G-survival - λ_struct·G-structural-pressure
```

with `λ_struct` calibrated so the new term's magnitude is comparable to the existing terms (the existing G values for the tied bucket sit at ~0.4 / 0.015 / 13.985 / 0.773; the anamnesis-concentration signal sits at 0.0–2.2 with a tail). Calibration follows the same scale-asymmetry awareness `aif/expected-free-energy-scorecard.flexiarg` HO-05 already flagged.

### BECAUSE
Action selection that scores `:address-sorry` actions identically irrespective of target is action-class scoring, not action scoring. The aliveness-synthesis (`M-action-cost-modelling §3.8` + §3.8.1) names the multi-projection aliveness signature as the discriminator: G should consume it. A post-AIF tie-breaker proves the signal exists and works empirically; v1 makes G itself the consumer. That makes the WM's belief-update path see structural-pressure as a first-class observation, not as a downstream re-sort.

## R1-R12 touch points

| R | Touch | What this excursion does |
|---|---|---|
| R1 — Explicit belief state | indirect | Belief state already includes typed-substrate readings via the watcher; v1 doesn't change R1, but R1's coverage IS the foundation that lets ΔT be computed |
| R2 — Observation channel schema | **partial** | Adds `:structural-pressure-per-action` as a new **candidate-local derived field**, not a new global observation channel; lands in the per-candidate scoring schema adjacent to `compute-efe` |
| R3 — Predictive-coding belief update | unchanged | Belief-update mechanism stays as-is |
| R4 — Predictive forward model | unchanged | v1 doesn't change forward-model shape |
| R5 — **Principled EFE terms** | **PRIMARY** | Adds `G-structural-pressure` as a new named term in the G-breakdown; follows `aif/expected-free-energy-scorecard.flexiarg` discipline (named, weighted, auditable, term-breakdown stored per candidate) |
| R6 — Action selection + abstain | indirect | Selection mechanism unchanged; only the score-per-candidate changes |
| R7 — Adaptive precision | unchanged | `λ_struct` may eventually adapt under R12, but v1 ships as static |
| R8 — Per-tick trace | **partial** | G-breakdown trace needs `G-structural-pressure` added; existing trace schema accommodates this naturally |
| R9 — Named validation properties | **partial** | New invariant: *"`:address-sorry` actions with different target-mission-neighborhoods do not all share identical G."* The old WM violated this (silently — no test); v1 satisfies it by construction. Test added to `futon2/test/futon2/aif/`. |
| R10 — Live operation | unchanged | The new term computes inside the existing tick path; no operational change |
| R11 — **Hierarchical / multi-agent composition** | **DEEPENING** | The ΔT signal that feeds `G-structural-pressure` is computed over edges that VSATARCS-writer authors (via car #2's file→mission derivation and any future VSATARCS-edits to substrate-2). Pulling ΔT into G means WM is now READING from VSATARCS's writes via the substrate-mediated channel — the **communication** that satisfies R11 per `M-action-cost-modelling §7.3`. Without v1, the R11 channel exists in principle but the WM never reads the VSATARCS-authored signal during its scoring. With v1, the channel closes. |
| R12 — Dual-loop / hyperparameter inference | indirect | `λ_struct` becomes a hyperparameter R12 can later adapt under the dual-loop. v1 ships with static `λ_struct`; R12 adaptation is a deferred follow-on. |

The excursion's PRIMARY R-touchpoint is R5 (new principled EFE term). Its DEEPENING contribution is R11 (WM reads VSATARCS-authored signal during scoring — empirical loop-closure). Secondary touches at R2, R8, R9.

## Acceptance criteria

A1. `G(a)` for `:address-sorry` actions targeting different sorries with different related-mission ΔT profiles yields **different G values** at the WM's `judge` step — verified by Drawbridge eval against the live ranked-actions after reload.

A2. The G-decomposition trace (visible in §3.4 trace affordance and in the WM API response) includes `G-structural-pressure` per action.

A3. The post-AIF tie-breaker in `apply-anamnesis-tiebreak` becomes **functionally redundant** — i.e., the ordering produced by G alone matches the ordering the tie-breaker would have produced. (The tie-breaker can stay in code for one release as belt-and-braces with a deprecation note; remove cleanly after one stable cycle.)

A4. A new R9 invariant test asserts: *"For `:address-sorry` actions, identical G-decompositions across different targets is a substrate-blindness signal (R5 violation), not a valid ranking state."*

A5. `λ_struct` is calibrated so that the relative ordering of the 7 currently-tied sorries matches v0's tie-breaker ordering (sanity check: aif-head first, handler second, r3d third, wm-ui-hud fourth, then the 0.0 tier).

A6. UI's tied-bucket display no longer fires (since the tie no longer exists at G level for these 7 actions); the `<details>` per-action trace still works and surfaces G-structural-pressure as a named term. The "broken by ΔT-anamnesis-concentration" header text disappears organically.

## Cycles plan (operator-paced)

- **Cycle 1**: Define schema extension (R2-partial) — add `:structural-pressure-per-action` as a candidate-local derived field; identify integration point in the `judge` → `compute-efe` seam. Decide `λ_struct` initial value via empirical fit against current ranked-actions.
- **Cycle 2**: Implement `G-structural-pressure` computation inside the `judge` function; consume from `delta-t-mission` (already shipped car #1). Update G-breakdown emission (R8).
- **Cycle 3**: Add the R9 invariant test (A4). Run the test suite; verify acceptance criteria A1, A2 against live substrate.
- **Cycle 4**: Calibrate `λ_struct` against A5 (the v0 tie-breaker ordering serves as ground-truth for v1's calibration; same data, different mechanism). Document final `λ_struct` value with rationale.
- **Cycle 5 (optional)**: Deprecate `apply-anamnesis-tiebreak`; remove after one stable cycle per A3.
- **Cycle 6 (optional)**: R12 follow-on — make `λ_struct` itself a slow-loop-inferred hyperparameter via the existing `:wm-outer-loop` (`futon2/scripts/wm_outer_loop.clj`). Per-G-term weight inference is a natural R12 extension.

## Cycle 1 proposal (2026-05-27, codex-5)

### 1. Schema extension site

Cycle 1's main clarification is negative: `:structural-pressure-per-action` should **not** be added to `futon2.aif.observation/observation-channels`.

Why:

- `futon2.aif.observation` is the WM's fixed tick-global world-reading schema (`:loop-health`, `:mission-health`, `:sorry-count-norm`, etc.). R2's force there is *stable shape across ticks*.
- Structural-pressure is **candidate-local**. Two `:address-sorry` actions in the same tick can and should carry different values.
- Putting a per-candidate field into the global observation vector would blur the line between "what the stack looked like this tick" and "what this candidate implies structurally if taken." That would weaken the existing R2 discipline rather than extend it.

So the right schema site is:

- enrich candidate action maps between `futon2.report.war-machine/judge`'s `wm-candidates` and `futon2.aif.efe/rank-actions`
- thread `:structural-pressure-per-action` on the candidate map
- have `futon2.aif.efe/compute-efe` consume that field and emit `:G-structural-pressure` in the named breakdown

Concretely, the seam is:

- `~/code/futon2/scripts/futon2/report/war_machine.clj` at the `wm-candidates` → `efe/rank-actions` handoff
- `~/code/futon2/src/futon2/aif/efe.clj` at the `compute-efe` return-schema / G-total assembly

This keeps the architecture clean:

- R2 global observation stays fixed
- R4 prediction stays the current `next-observation` + `next-belief` contract
- R5 gains one more named term on the candidate-scoring side

### 2. Initial `λ_struct`

Proposed initial value: **`λ_struct = 0.35`**.

Empirical fit reasoning:

- Current tied bucket base decomposition is approximately:
  - `G-risk ≈ 0.41`
  - `G-ambiguity ≈ 0.015`
  - `G-info ≈ 13.99`
  - `G-survival ≈ 0.77`
- Current v0 discriminator values are:
  - `2.2`
  - `1.0`
  - `0.7`
  - `0.2`
  - `0.0`
  - `0.0`
  - `0.0`

If the term enters as a subtraction, `λ_struct = 0.35` yields contributions:

- `2.2  -> -0.77`
- `1.0  -> -0.35`
- `0.7  -> -0.245`
- `0.2  -> -0.07`
- `0.0  ->  0.0`

That does three useful things at once:

- preserves the exact v0 ordering
- gives a visible spread large enough to break the tie decisively
- keeps the new term in the same order of magnitude as the existing positive terms, rather than immediately dominating the scorecard

The earlier rough `0.5–1.0` range is usable as a later calibration band, but it is too aggressive as the first shipped value. Cycle 2 should start at `0.35`, then Cycle 4 can re-fit if real cross-class behaviour suggests drift.

### 3. Exact `judge` / G integration point

The integration point is:

- `judge` computes / attaches `:structural-pressure-per-action` **before** `efe/rank-actions`
- `compute-efe` reads that field and emits:
  - `:G-structural-pressure`
  - updated `:G-total`

This is the exact place the v1 change belongs because it preserves the current layering:

- `judge` remains the live orchestration site with access to substrate-backed target context
- `compute-efe` remains the single point where named G terms are assembled into `:G-total`
- `apply-anamnesis-tiebreak` stays in place for now as belt-and-braces, but v1 makes it redundant by construction

### 4. Scope-shaping surprises from Cycle 1

Two findings changed the design wording enough that they should be explicit before Cycle 2:

1. **The live v0 tie-breaker is not currently consuming raw `-ΔT`.**

   The shipped tie-breaker sums `1.0 - mission-T` per related mission. That is the signal the operator saw, and it is the correct empirical calibration target for v1's first implementation. So Cycle 2 should consume the same signal inside G first, then only later consider replacing it with a different structural-pressure function if that earns its keep.

2. **The sign must be explicit.**

   Lower `G-total` is more preferred. Therefore higher structural-pressure should reduce `G-total`, not increase it. The excursion now states the term as a subtraction so the implementation cycle doesn't drift into a sign mistake.

3. **R8 has a hidden follow-on.**

   The current ranked-action trace compaction only whitelists the existing G terms. If Cycle 2 adds `:G-structural-pressure` to the live ranking but not to trace compaction, A2 will appear to fail even though the ranking is correct. That trace whitelist extension therefore belongs in Cycle 2's definition of done, not as an afterthought.

## Cycle 2 implementation note (2026-05-27, codex-5)

Cycle 2 is now landed in code:

- `futon2.report.war-machine/judge` enriches candidate action maps with `:structural-pressure-per-action` before `efe/rank-actions`
- `futon2.aif.efe/compute-efe` consumes that field, emits `:G-structural-pressure`, and subtracts `λ_struct · G-structural-pressure` from `:G-total`
- `futon2.aif.trace/strip-ranked-action` now preserves `:G-structural-pressure`

Focused verification:

- local tests: `41 tests / 114 assertions / 0 failures`
- live WM scheduler cache recomputed after reload
- top formerly-tied `:address-sorry` bucket now splits inside `G-total` itself:
  - `mission-aif-head-not-served` → `G-structural-pressure 2.2`, `G-total -5.0126`
  - `handler-closure-route-rebinding` → `1.0`, `-4.5926`
  - `r3d-per-entity-attribution` → `0.7`, `-4.4876`
  - `wm-ui-hud-mode-rationale-hardcode` → `0.2`, `-4.3126`
  - the three `0.0` items remain tied at `-4.2426`

Important honest note:

- the post-AIF tie-breaker still exists in code, but after Cycle 2 it no longer carries the ranking for the top differentiated items
- the residual three-way `0.0` tier is legitimate under the current substrate inventory; no fake discrimination was introduced there

## OUT of scope

- Refactoring G's overall structure beyond adding one new term.
- Extending the tie-breaker to handle non-`:address-sorry` action classes (still blocked on extending `delta-t-mission` to consume `code/v05/file→mission` per-edge for file-edit actions, etc.; that's downstream work).
- Touching VSATARCS code directly. v1 ONLY makes the WM-side a consumer of substrate signals that VSATARCS writes into substrate-2.
- §3.4 trace affordance v0.1 deferred items (predicted-observation per-action; structural-weight breakdown beyond ΔT).

## References

- `~/code/futon3c/holes/missions/M-action-cost-modelling.md` §3.8.1, §7.3, §7.6 (parent mission; v0 post-AIF tie-breaker landed and the v1 critique surfaced)
- `~/code/futon3c/src/futon3c/aif/mission_delta_t.clj` (the ΔT function v1 consumes inside G)
- `~/code/futon2/scripts/futon2/report/war_machine.clj` (the `judge` site where v1's new term lands; currently houses the v0 post-AIF tie-breaker which v1 makes redundant)
- `~/code/futon3/library/aif/expected-free-energy-scorecard.flexiarg` (the discipline for adding new principled G-terms)
- `~/code/futon3/library/aif/niche-construction.flexiarg` (false-floor discipline — `G-structural-pressure` reads zero where the substrate lacks the typed edges, which is the legitimate signal not a bug)
- `~/code/futon2/docs/futon-aif-completeness.md` §R5, §R11 (R-criteria definitions; R11 entry currently still reads N/A and needs the corrected stance per M-action-cost-modelling §7.3)
- Companion follow-on: M-action-cost-modelling §7.5 names `library/futon-theory/five-substrate-cycle.flexiarg` and the R11 entry rewrite as sibling work
