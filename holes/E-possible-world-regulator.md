# E-possible-world-regulator — a regulator that tunes the War Machine across possible futures

**Excursion** (E-prefix: a bounded scope-out, owned end-to-end by one agent — claude-1).
**Date:** 2026-06-08. **Status:** IDENTIFY (charter — concept + framing + the score-axis; not yet building).
**Parent:** `E-efe-education` (the precision-learning charter) — this excursion is *how* it tunes the delicacy
**without shooting in the dark.**
**Couples (reuse, do not fork):**
- `M-futon-forward-model` (futon7) — the business/cadence evaluator, reused as **one axis** (not the objective).
- `M-capability-star-map` (futon0) — the structure the WM navigates; the rollout reads its `graph.edn`.
- `C-pudding-prover` (futon7) — the **source-of-truth for what "useful" means** (witness ⟂ cadence; §8.2).
**Provenance:** Joe, emacs-repl 2026-06-08, after the live `gap-weight 6.0` saturation finding —
*"experimental regimes that tune the delicacy rather than shooting in the dark; the equivalent of Playwright
for exploring possible futures; an evaluator — a regulator in Mama Claude's terms — with the Ashby
requisite-variety feature that lets us discriminate between the settings."*

## 1. IDENTIFY — the tension

We tune the WM's EFE parameters (`gap-weight`, `body-weight`, `ascent-weight`, `applicability-penalty`, the
blend precision) **by hand, one at a time, live, on the serving nervous system.** This morning's activation hit
it concretely: `gap-weight 6.0` **saturates** (top-5 tie at the ceiling) — caught only by reading the live
ranking *after* committing the setting. There is **no instrument that explores parameter settings as possible
worlds and discriminates between them *before* we commit one.**

Ashby names the missing piece. The EFE is a **generator**: its parameters are variety knobs. To regulate it you
need an **external** evaluator carrying **at least as much variety as the disturbances that matter** — and
crucially **not the EFE's own `G-total`**: that scalar is the quantity the parameters *define*, so scoring a
setting by it is circular and has zero discriminating variety with respect to itself.

## 2. The question — and that the question is itself a knob

**Primary question (Joe):** *which build-out strategy gives the WM the best **bundle** of capabilities +
sustainability — and of those, which are the most **automatable**, i.e. which can we bring online NOW?*

**The focus is the War Machine, not the money.** Income/sustainability is `M-futon-forward-model`'s aegis; here
it enters as **one axis in the bundle**, never the objective. The objective is: **tune the WM so it is ready to
do *useful* work ASAP**, where —

- **"Useful" ≡ C-pudding-prover-defined** (referenced, not re-derived): advancing capability-registry /
  valuable-path (~25) entries along the **witness → cadence** ascent (§8.2; the *witness* ⟂ *cadence* axes) —
  producing **witnesses** that move base-camps toward *real* capabilities. `M-futon-forward-model` is the
  **cadence instrument** (Beta-binomial over the witness stream → `P(it keeps happening)`).

**The objective is a parameter.** *"Best capability bundle?"* / *"most-automatable-now?"* / *"best income?"* are
**different regulators** that yield **different optimal tunings**. The harness takes the **question (the
score-function) as an input**; it bakes in none. This is requisite variety at the meta level: the regulator
*family* is parametrized by the question. Joe: *"we could ask other related questions and get different answers."*

## 3. The architecture — "Playwright for possible futures"

| Playwright | This regulator |
|---|---|
| pins a browser, drives it deterministically, asserts on the DOM | pins a **corpus snapshot + seed**, drives the EFE deterministically, asserts on the **trajectory's score-vector** |

- **World = an EFE parameter vector** `{gap-weight, body-weight, ascent-weight, applicability-penalty, blend-precision, …}`.
- **Rollout = a deterministic WM-selection sim, OFF the live JVM.** A counterfactual replay over a *pinned*
  mission-corpus snapshot + fixed seed — **no Drawbridge reload, no serving-JVM mutation, no consent locus
  tripped.** Replayable precisely because it forbids wall-clock / randomness (the workflow-determinism discipline).
- **Regulator = a multi-dimensional score-vector** (requisite variety — NOT a scalar):
  1. **useful-work** — the trajectory's witness-yield on capability-registry / valuable-path entries (C-pudding-prover).
  2. **automatable-now** — fraction of the engaged trajectory the WM can drive **autonomously** under the live
     guardrails (`autonomous-admissible?` vs `needs-operator`) — literally "which can we bring online now."
  3. **sustainability** — ONE axis, from M-ffm: E[£-lift] / ROI coverage of the trajectory.
  4. **anti-saturation / discrimination** — within-domain top-k variance (the `gap-6.0` detector).
- **Discriminate = Pareto-compare worlds on the vector under the chosen question; pick AND justify the setting.**

*futon5 framing:* generator ∘ evaluator is a composed morphism; the sweep explores the generator's
parameter-domain; an eigendecomposition over the score-response surface (scope-out, §5) finds *which* knobs
actually move the chosen outcome — requisite variety is about covering the disturbances that **matter**, likely few.

## 4. The honesty discipline (the §12 gamification guard, applied to tuning)

Explore freely with counterfactual rollouts; **score against grounded anchors, never the model's own *expected*
projections.** Tuning the EFE to please a model the EFE feeds is the hall-of-mirrors trap (`M-futon-forward-model`
§12: "don't make the model winnable"). Concretely:
- **sustainability axis** → realized **ledger** clicks (M-ffm §8.4 invariant "realized overrides expected"),
  not the model's ticks.
- **useful-work axis** → C-pudding-prover's grounding taxonomy (**☑ verified / substrate**, not **◇ frontier /
  claimed**); a witness counts only if it is real.

## 5. Scope

**IN:** the param-sweep harness · the deterministic off-JVM rollout · the **question-parametrized** score-vector
· the first two questions (best-bundle, most-automatable-now) · the **`gap-weight` sweep as the first validating
test case** (must locate the ~2.5 sweet spot AND flag 6.0 as saturated — the live finding is the unit test).

**OUT (named):**
- the **live EFE re-tuning itself** — stays `E-efe-education` + **Joe's consent locus**; the harness *recommends*,
  the operator *commits*.
- **income modelling** — `M-futon-forward-model`'s aegis; reuse its evaluator, don't fork it.
- the **futon5 eigendecomposition** of the response surface — a later refinement.

## 6. Reuse (read before building — I-4)

- M-ffm evaluator: `futon7/holes/M-futon-forward-model.{mint,roi-results}.edn` · `.manifold-reader.bb` ·
  `futon3c/src/futon3c/logic/business_coupling_invariants.clj` — the sustainability axis (do NOT rebuild).
- The live EFE + opts: `futon2/src/futon2/aif/efe.clj` · `futon2/scripts/futon2/report/war_machine.clj`
  (`live-star-map-efe-weights`, `live-gap-view-efe-weights`) — the param surface to sweep.
- `M-capability-star-map.graph.edn` (futon0) — the structure the rollout ranks over.
- C-pudding-prover §8.2 capability registry + the witness ⟂ cadence axes — the useful-work axis.
- The WM guardrails (`futon3c/src/futon3c/wm/guardrails.clj` `autonomous-admissible?`) — the automatable-now axis.

## 7. MAP findings (2026-06-08, claude-1) — the seam exists; this is wiring, not writing

**The rollout seam is clean and pure.** `efe/compute-efe` is declared *pure* ("same (state, action, opts) →
same output", `futon2/src/futon2/aif/efe.clj:343`) and **every tunable weight lives in `opts`** —
`:gap-weight :graph-body-weight :graph-ascent-weight :graph-applicability-penalty :info-weight :survival-weight
:structural-pressure-weight :time-pressure`, plus the structure (`:capability-graph :pre-registered-goal
:mission-gap-view`). `efe/rank-actions state candidates opts` (`:419`) just maps `compute-efe` and sorts by
`G-total`. **So a parameter sweep = pin `(state, candidates)` once, re-rank under injected weights — pure,
cheap, deterministic. No new EFE surface needed.**

**The live judgement pipeline** (`war_machine.clj:3409–3457`) is the template to mirror:
- **pinned snapshot = `(wm-state, enriched-candidates, wm-missions)`** — `wm-state` (`:3409`,
  `{:observation :belief :sorrys :missions :patterns :anticipation :curvature-signal}`) + `wm-candidates`
  (`ap/compose-proposers` of bootstrap/pattern/mission/sorry/tension proposers, `:3417`) then
  `enrich-candidates-with-structural-pressure` ∘ `interest-net/enrich-candidates`. *This is the heavy part —
  built by one scan, pinned once.*
- **rollout = `rank-actions` → `apply-anamnesis-tiebreak` → `filter-live-open-mission-ranked-actions` →
  `(filterv can-execute?)` → engaged = first autonomous-admissible in EFE order** (the guardrails
  `guarded-selection` pick — deterministic; sidesteps any softmax in `policy/select-action`). *Nice
  convergence: that same step yields the **automatable-now** axis (the guardrails classification) for free.*

**Weight injection path (no builder change):** `live-star-map-efe-opts` / `live-gap-view-efe-opts` (`:166`, `:223`)
`(merge base-opts live-WEIGHTS {…structure})` — the live-weight consts **override** `base-opts`. So inject swept
weights by overriding *after* the builders: `(merge (live-star-map-efe-opts (live-gap-view-efe-opts {})) swept-weights)`
— swept weights win; graph / gap-view / domain-gate structure is inherited live. Trivial.

**Two thin shims (the only new surface):**
1. **expose-snapshot** — extract `war_machine.clj:3409–3426`'s `(wm-state, candidates)` build into one pure fn
   that *both* the live judgement and the harness call (one extract-fn refactor → no drift). Touches the
   live-judgement path ⇒ careful Drawbridge reload, not a restart.
2. **rollout-under-opts** — replicate `:3438–3457` given a pinned snapshot + a swept-weights map; return
   `{:ranked :admissible :engaged}`.

**Determinism checklist (verify at DERIVE):** (a) `fm/predict` pure (implied by compute-efe's purity claim —
spot-check no RNG); (b) `apply-anamnesis-tiebreak` deterministic; (c) engaged = first-autonomous-admissible
(deterministic by construction — already chosen over `select-action`). For Playwright-grade replay, also pin
the **graph + gap-view + domain-view** as data (so a serialized EDN snapshot replays with zero live-JVM reads).

**Build options:** **A (in-JVM, first cut)** — pin the snapshot once via one scan, sweep weights via *pure*
`rank-actions` calls over Drawbridge (no reload, no mutation; cheap because the scan is amortized). Gets the
`gap-weight` verdict immediately. **B (off-JVM, Playwright-grade)** — serialize the pinned snapshot to EDN,
run the sweep in a standalone process. Recommend **A → harden to B**; the snapshot-EDN is itself the
reproducibility artifact.

## 8. DERIVE (2026-06-08, claude-1) — score-vector · question-input · snapshot schema

**Determinism gate CLEARED:** no `rand`/`shuffle`/RNG in `forward_model.clj`, `efe.clj`, or the report ranking
helpers (`apply-anamnesis-tiebreak` `:403`, `filter-live-open-mission-ranked-actions` `:299`). The rollout path is
deterministic by construction; engaged = first-autonomous-admissible (sidesteps `policy/select-action`).

### 8.1 The unit of evaluation — Bundle(W)

A **strategy = a weight-vector W**; its **bundle = the top-k autonomous-admissible actions** in
`rank-actions(snapshot, W)`. *Principled:* `rank-actions` scores each candidate independently (`map compute-efe`),
so greedy-remove-and-re-rank ≡ top-k exactly — a myopic bundle with **no fabricated field-dynamics** (a true
multi-step rollout needs a field-transition model we don't honestly have — named scope-out §5). `k` default 5.

### 8.2 The score-vector S(W) = {:useful :automatable :sustainability :discrimination}

Per bundle action with target mission `m` (graph = `M-capability-star-map.graph.edn`, goal = `live-star-map-goal`):

1. **useful-work-ASAP** (primary; C-pudding-prover-defined) —
   `u(m) = single-cycle-leaf?(graph,m) × (w_c·c_joint(m) + w_a·ascent(m))`
   - `single-cycle-leaf?` (`efe.clj:79`; applicable ∧ exactly-1-open-hole) = **yields a witness THIS cycle** (the
     §13.2 advanceability gate — a 986-line mega-mission scores 0 here).
   - `c_joint(m)` (`centrality.json`) = recency-immune valuable-path centrality (the "right work", ☑-grounded).
   - `ascent(m)` = `mission-ascent-progress(graph,goal,m)` (`efe.clj:99`) = graded credit toward the WM goal.
   - `:useful` = mean `u(m)` over the bundle. *Encodes "useful work ASAP" = right-direction × witness-now.*

2. **automatable-now** (primary for Q2) — from the rollout guardrails classification:
   `:automatable = |autonomous-admissible ∩ bundle| / k`, plus `:first-autonomous-rank` (rank of the first
   autonomous-admissible in the full ranking — low = the WM engages real autonomous work immediately, not after
   NAGging through operator-gated items). Free from the rollout.

3. **sustainability** (secondary, SPARSE — M-ffm, ledger-anchored) —
   `:sustainability = Σ E[£-lift](m)` over bundle targets with a mission→feature mapping into `roi-results.edn`
   (ledger-anchored, **not** model ticks), 0 otherwise. **Honest caveat:** roi-results is keyed by *feature*, not
   mission; most WM missions → 0. A real mission→feature bridge is its own work; v1 accepts sparse coverage (a
   tiebreaker, not the driver — Joe's reframe).

4. **discrimination / anti-saturation** (health guard) —
   `:discrimination = effective-#-distinct-preferences over the top-k G-totals` (`exp(H(softmax(−G/τ)))`, or count
   of distinct G-totals / max−min spread). gap-weight 6.0 → top-5 tied → ≈1 → **saturated, flagged**. A
   feasibility floor, not a goal.

### 8.3 The question = a scalarization over S (pluggable)

A **question Q = `(fn [S] number)` + a constraint `:discrimination ≥ d_min`:**
- **Q-best-bundle** ("best bundle of capabilities + sustainability"): `max :useful + λ_s·:sustainability`
  s.t. `:discrimination ≥ d_min`.
- **Q-most-automatable-now** ("which can we bring online now"): `max :automatable`, `:useful` lexicographic
  tiebreak, s.t. `:discrimination ≥ d_min`. *Online-now, but useful — not automatable busywork.*
- More questions = more objective fns. *"Different questions → different answers" = requisite-variety-at-the-meta-
  level, made operational.*

**Harness output:** sweep W over a grid → `{W → S(W)}`; for Q, rank W by its objective; emit **best-W + the Pareto
front + the full table** (Joe sees the tradeoffs, not just a winner). **Validation run:** sweep `gap-weight ∈
{0, 2, 2.5, 4, 6}` (others fixed) ⇒ expect `:discrimination(6.0)` low and `:useful/:automatable` peaking near
~2.5 — the live finding becomes the harness's acceptance test.

### 8.4 The pinned-snapshot schema

```clojure
{:as-of <str, passed in — NOT (Date/now)>  :scan-id <str>
 :wm-state  {...}                 ; observation/belief/sorrys/missions/patterns/anticipation/curvature
 :candidates [ {:type :target ...} ... ]   ; post structural-pressure + interest-net enrich
 :wm-missions [...]               ; for filter-live-open-mission
 :structure {:capability-graph {...} :pre-registered-goal :wm-overnight-unsupervised
             :mission-gap-view {m->gap} :mission-domain-view {m->domain}}
 :grounding {:centrality {m->c_joint} :valuable-path #{m...} :roi-map {m->{:expected-roi-gbp n}}}
 :live-weights {...}}             ; the current setting = the baseline datum in the sweep
```
- **v1 (Option A, in-JVM):** held in an atom — live objects, no serialization.
- **v2 (Option B, EDN replay):** caveat — `:wm-state :patterns` may carry non-EDN (compiled regexes) → a
  serializable projection (strip/rehydrate) is the off-JVM hardening step.

### 8.5 Honesty mapping (each axis → its grounded anchor)
useful → ☑ valuable-path centrality + single-cycle witness-gate (not *claimed*) · automatable → live guardrails
(not *asserted*) · sustainability → ledger-anchored roi-results (not model *ticks*) · discrimination → pure
property of the ranking. **No axis reads the model's own expected projections** — the §4 gamification guard, met.

## 10. Live acceptance — first real run (2026-06-08, claude-1) — the instrument did its job

Build reviewed (clj-kondo 0/0; check-parens clean; sweep-test 5/20; war-machine-test regression 13/58 — all
pass), FF-merged to futon2 main (`28e156a`), reloaded into the serving JVM (gap-reader preserved), judge run
populated `!last-wm-inputs`, then `(gap-weight-acceptance (pin-wm-snapshot))` + a finer sweep. **The harness runs,
is deterministic, and on its FIRST live run produced findings that overturn the morning hand-guess and expose two
axis-coverage gaps. The acceptance-as-coded returns FALSE — informatively.**

Live sweep (89 candidates, valuable-path 25):

| gap | useful | automatable | discrimination | engaged |
|---|---|---|---|---|
| 0 | 0.0 | 0.2 | **3** | M-canon-fingerprint-store (math) |
| 0.5 | 0.0 | 1.0 | **1** | M-futonzero-mvp (local) |
| 1–6 | 0.0 | 1.0 | **1** | M-futonzero-mvp (local) |

**F1 — the tuning verdict (REFUTES the morning ~2.5 guess).** Saturation is NOT specific to gap 6 — it appears at
**any gap ≥ 0.5**: the top-5 `G-total`s become *exactly equal* (`-4.7773397773843325` ×5). Cause: the gap-SCORE
**ceiling-cluster** (≈5 missions clamped at 1.0) gets an identical boost at any positive weight, so no weight
breaks their tie. **No gap-weight in (0, 6] both realigns AND preserves discrimination** — it is a near-binary
switch (0 = math, no realign; ≥0.5 = local, saturated). **So the fix is NOT the weight; it is the gap-score
distribution** — exactly claude-3's de-biasing → growth-surface (Campaign-BSL R1-CORRECTION fix (a)). The
regulator's first act was to stop a plausible hand-tune that would not have worked.
  - **F1 DISPATCHED → claude-3 (bell `invoke-1780938516200-158-2f61a56e`, 2026-06-08).** The cause is the
    `[0,1]` CAP normalization piling the top at gap-score 1.0 (a ceiling-cluster); the fix is a spread-preserving
    top-end normalization in the `mission_fold_learn.py --emit-view` emitter (rank/percentile/soft-squash — claude-3's
    call), without re-introducing size-domination or changing the domain partition. **Acceptance:** re-emit
    `mission-fold-view.edn`; claude-1 re-runs the harness gap sweep to confirm discrimination > 1 at the realigning
    weights. Data re-emit only — no EFE/JVM touch; claude-1 owns the live re-validation.
  - **F1 VALIDATED (claude-1 sandboxed sweep, 2026-06-08).** claude-3 re-emitted with **empty-MASS** normalization
    (Σ missing-slots, linear below MASS_CAP=120 — no hard `→1.0` cap). Sandboxed sweep (new gap-view injected into
    the pinned snapshot; **live cache untouched** — activation is Joe's locus): ceiling-cluster **25 → 0**
    (max 0.875); discrimination across gap `[0 .5 1 2 6]` = **3, 4, 4, 5, 5** (was 1 at every gap ≥ 0.5);
    top-5 G-totals at gap 6 now **fully distinct** (`-9.24 -8.54 -7.49 -6.54 -6.39`, was `-4.777 ×5`).
    **Saturation FIXED — the gap-weight is a clean dial again** (discrimination *climbs* with weight). New engaged
    action at gap ≥ 1 = `M-emacs-cursor-peripheral` (a 1-hole LEAF + high growth-surface — useful-positive AND
    high-expansion).
  - **F1 ACTIVATED + closed (Joe-greenlit, 2026-06-08).** Live `mission-fold-view-cache` reset → the live WM
    judgement now reads the de-saturated gap-view (live cache max **0.875**, **0 ceiling**, 176 local missions);
    live ranking refreshed. The `.edn` is gitignored (`data/*`, regenerable) so the durable artifact is claude-3's
    **emitter** (`mission_fold_learn.py` empty-mass normalization + 9/9 contract tests incl. the no-ceiling F1
    regression) — committed by claude-3 at **futon6 `be47eb6`** (branch `hypergraph/2026-06-08-wholeness-demo`;
    `mission_fold_learn.py` + `mission_fold.py` + tests, first commit of all three, 513 lines — claude-1 verified
    `MASS_CAP=120` + the empty-mass `gap_score`). The `.edn` stays gitignored/regenerable via `--emit-view`. *This is
    the Campaign-BSL R1 chord landing on the live WM — the de-biased gap-reader, validated by the regulator it spawned.*

**EXCURSION STATUS (2026-06-08): IDENTIFY→MAP→DERIVE→BUILD→REVIEW→MERGE→LIVE-RUN→F2-fix→F1-fix all closed.**
The harness is built, reviewed, merged (futon2 `dfca8d7`), and live; it scores 3 live axes honestly (useful,
automatable, discrimination) + 1 sparse (sustainability). It earned its keep on run #1 (refuted the hand-guess,
localized the saturation to the gap-score, validated claude-3's fix). **Open follow-ups (named, non-urgent):**
F3 (mission→feature bridge for the sustainability axis); the ascent-term graph-limitation; and — now that the
gap-weight is a clean dial again — the *original* E-efe-education delicacy-tune (pick the weight via the harness's
question-objectives), which is now meaningful rather than saturation-blocked.

**F2 — the useful axis is DEAD on the live field (a DERIVE spec-gap, mine).** The capability-star-map graph holds
**31 mission nodes; the WM ranks over 89 candidates.** Bundle targets not in the 31 → `mission-applicable? false`
→ `single-cycle-leaf? false` → `:useful 0` everywhere. The graph nodes *do* carry `:open-hole-count`; the
candidates simply aren't graph nodes. **Fix:** compute `single-cycle-leaf?` / hole-count from the mission
**registry** (which has the counts) rather than only the graph — or extend the graph to the candidate set.
  - **F2 RESOLVED (claude-1, `dfca8d7`, 2026-06-08).** `mission-open-hole-count` made registry-first (live
    `:wm-missions`, 80 candidates; graph fallback); the useful single-cycle gate reads it. **Validated live:**
    `:useful` now non-zero (`0.00926` at gap ≥ 0.5 vs `0.0` at gap 0) — `M-emacs-cursor-peripheral` (1 hole) reads
    `leaf true`, multi-hole missions read false. Gates green (clj-kondo 0/0, check-parens, test 5/20). The axis is
    now a TRUE signal: `:useful 0` means "no single-cycle-advanceable work in the bundle," not a measurement gap.
    *Residual:* the **ascent** term is still graph-only (0 for non-star-map missions) — a separate follow-up; for
    now `c_joint` carries the useful signal for off-graph candidates.

**F3 — sustainability 0-coverage (confirms the sparse caveat).** `roi-map` matched **0** of 89 missions
(feature-keyed roi-results + crude name-match, incl. the dead `normalize-feature-key` regex). Dead on live, as
flagged; needs the real mission→feature bridge.

**Disposition.** codex-1's BUILD is correct and faithful (review PASS); the gaps are not code bugs but
(F1) a system finding the harness exists to surface, (F2) a spec-gap in my DERIVE axis definition, (F3) the known
sparse axis. The acceptance assertion is too fixture-shaped (assumes non-degenerate axes + saturation-uniquely-at-6);
it should be relaxed to "discrimination strictly decreases as gap crosses the ceiling-cluster threshold." **Net:
the instrument works and earned its keep on run #1.**

## 9. Next steps
1. *(IDENTIFY + MAP + DERIVE done; BUILD reviewed + merged + live-run.)*
2. **Build = a Codex handoff** (R11 scope-bounded): the two shims (pin-snapshot, rollout-under-opts) + the
   sweep harness + `S(W)` + the two question-objectives + the gap-weight acceptance run. Gates: clj-kondo,
   `check-parens`, futon2 tests, + the acceptance run reproduces the saturation finding. Architecture + the
   review gate stay here (author ≠ reviewer).
   - **DISPATCHED 2026-06-08** → codex-1 (bell `invoke-1780934280083-154-1a7d8c27`), branch
     `wm-outing/2026-06-08-regulator-sweep`. v1 de-risk: **single-repo (all futon2)**; the `:automatable` axis
     uses a documented futon2 proxy for the futon3c guardrails classification (real-guardrails wiring = a named
     fast-follow), avoiding a futon2↔futon3c dependency tangle. Awaiting bell-back (summary + gate results + shas).
3. **ARGUE/VERIFY** the swept gap-weight → hand the recommended value to `E-efe-education` for the live re-tune
   (Joe's consent locus).
