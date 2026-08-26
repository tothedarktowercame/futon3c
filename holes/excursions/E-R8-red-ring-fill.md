# E-R8-red-ring-fill — filling the placeholder at step ⑨

**Opened:** 2026-08-26 · claude-13 at Joe's direction.
**Revised:** 2026-08-26, same day, after slice 1 and two Codex investigations
overturned the premise twice. **Read the revision history below before the
slices** — it is the substantive part.

## Premise as opened, and how it changed

**As opened**, in Joe's words:

> *a loop is NOT born instrumented for its gain — but does have a placeholder
> where it can gain that instrumentation.*

`sec-system` step ⑨ is that placeholder: *"Fold any new realized fold outcome
into the engineering selection gain g"* — typed, correctly positioned between ④
(load the previous trace) and ㉞ (persist it), and **conditional**, so with no
outcome it is a silent no-op rather than a crash. The excursion opened on the
claim that **the placeholder has never been filled.**

**That claim was wrong, and so were its two successors.** The diagnosis was
corrected three times in one afternoon, each correction narrowing the target:

| # | claim | why it was wrong |
|---|---|---|
| 1 | *"R8's realised term has no producer."* | `fold_realized.clj` is the producer — built, tested, scale-verified by zai-5 (18/18, expected ΔG −0.6 → realized −0.6), armed 2026-07-08. |
| 2 | *"The producer has never fired."* | It fired **88 times**, 2026-07-02..07-06, in `futon2/data/wm-trace/`. I had measured `data/wm-full-loop/` — the archive of a runner that does not enact. |
| 3 | *"The placeholder was never filled."* | It was filled for five days. **An upgrade on 2026-07-08 replaced the working producer with a better one that has never had an input.** |

Each correction came from checking a claim already written down, not from a new
sweep. That is why the open questions below are specific.

## The verified mechanism

Two runners, not one. `scripts/wm_outer_loop.clj` reads traces and does not
enact — its `:require` list is `trace`, `intrinsic-values`, `shell`, `string`.
`scripts/wm_scheduled_run.clj:25` requires `futon2.aif.enact` and calls
`(enact/close-loop! judgement …)` at line 108 under `(live-wire?)`. That is the
enacting path, and it writes to `data/wm-trace/`.

    07-02 … 07-06   realized-outcome-of (coverage→ΔG) produces 88 outcomes,
                    each {:policy … :expected-G -0.2 :realized-G -0.5 :tick …}
    07-08  d36086f  A5 grounded feed added (flag-gated, dark)
    07-08  b624242  all built-dark flags armed default ON
    07-09           first run after the switch — 0 realized outcomes
    …               0 in every trace since, through 07-21 (the last)

The migration was a **correctness improvement** — `realized-outcome-grounded`
reads `bound − inhabited` over reviewed substrate endpoints, *"never coverage-ΔG
mixed with substrate state"*. And its own docstring says *"INERT UNTIL DATA: γ
stays starved until real fold-variance flows."* Nobody came back to check
whether data had started flowing.

**Why the silence was total** — three layers, each verified at source, each
individually defensible:

    grounded-deposit → deposit-for-mission → a3/deposits-by-id
      ↓  ONE deposit rejected :prompt-not-reconstructable
         ⇒ corpus-wide THROW, not a skip            actuator_a3.clj:149
      ↓  (catch Throwable _ judgement) ⇒ unchanged   enact.clj:255
      ↓  no :realized-outcome written
      ↓  step ⑨ "fold ANY NEW …" no-ops             sec-system ⑨
      ↓  γ starved ⇒ :selection-gain pinned at 1.0
      ↓  τ_eff = 1/g frozen ⇒ ⑯'s temperature never adapts

A strict corpus load is right for an evidence store. A catch-all around
enactment is right so a read failure cannot break the loop. A conditional fold
is right when there is nothing to fold. **Composed, they give seven weeks of
ticks that report success while the gain cannot move.**

## Slice status

| slice | status |
|---|---|
| **1 — reachability** | **DONE.** codex-22, `56e0281a`, reviewed and confirmed at source. Verdict NO for `wm_outer_loop`; the enacting runner is `wm_scheduled_run`. Displaced by the finding that ⑨ had already fired. |
| **1b — the missing input** | **DONE.** codex-22, `6dae1b9a`, reviewed. First unavailable link is `deposit-for-mission`; classification *a measurement nobody produces*, refined on review to **two faults at different depths** (below). |
| **2 — the null control** | **BLOCKED, and not for the stated reason.** The retro-trip note said *"there is nothing to freeze"*; there are 77 parseable mismatches, varying (`0.0`×18, `−0.3`×56, `−0.09`×3). But **all 77 carry one policy**, `M-bayesian-structure-learning`, so there is no action sequence for the null control to differ. The limit is **policy diversity**, not the mismatch. |
| **3 — operator-side ⑧ and ㉛** | **OPEN, unchanged, independent.** Per-channel precision and the Morning Brief queue; both recording changes, not measurement design. |
| **4 — record repair** *(new)* | Make `deposits-by-id` degrade rather than throw, or repair the rejected deposit. Cheap, and it **exposes** slice 5 rather than hiding it. **Touches the enactment path — Joe's call.** |
| **5 — the measurement gap** *(new)* | Nothing produces `:clean` / `:box-bindings` for this mission, so even a clean corpus load yields `bound = 0` and `realized-score` `nil` (`fold_realized.clj:163`, `(when (pos? bound) …)`). This is what R8 has actually been red about since 07-08. |

## The modular formalisation plan

Joe, 2026-08-26: *"rather than a monolithic Lean build, we build a formal model
modularly, so R8 and the touching components get formalized first, and we make
the repairs; then we go on to the next module."* Agreed, with three constraints
that come from the mechanism above.

**1. The module is the chain property, not the component.** Every layer above
typechecks individually, so formalising `deposits-by-id`, then `close-loop!`,
then step ⑨ separately **would prove all three correct and miss the defect.**
The unit is the property that spans them:

> step ⑨ occurs ⟹ a durable realized outcome exists, carrying both legs

which is `APMCycleMachine.lean:1184`'s `durableObservation = true ∧
contentDigest ≠ ""` generalised, and which crosses `actuator_a3`, `enact`,
`fold_realized` and one prose step.

**2. Module 1 is R8 *and* R14.** R14 is not adjacent, it is the chain's terminal
consumer — `:selection-gain` *is* R14, "Selection Gain as Commitment
Temperature". Three signs they are one module: both ruled by WR-27, which
carries `@holds-open R8 R14`; both armed by the same commit `b624242`, one flag
named *"R14 live-wire migration"*; and one measurement exposed both.

**3. The emitter's target is the producer-selection table.** For APM, *"the
runtime is not permitted to carry an independently maintained phase table."*
Here the independently maintained table is `*live-wire?*` and
`*selection-gain-grounded-feed?*` — two Clojure booleans deciding which producer
feeds `:realized-outcome`. Emitting **which producer is live under which
discharged precondition** is what makes 07-08 impossible: that commit swapped
producers with identical return shapes and an undischarged precondition.

**Order.** Module 1 R8+R14 → module 2 **R5** (*"the evaluate stage reports what
the criterion set does NOT cover, with the same discipline it applies to a poor
score"* — module 1's property one level up, so largely instantiation) → module 3
**R2**. R6 needs a candidate proposer that does not exist; R7 is `:holds true`
with no promotion test, so it is preservation rather than repair.

**Cost.** Not a constraint. The behavioural models need no Mathlib —
`APMCycleMachine → ExperimentalDesign → ExperimentPreregistration` elaborates
with the Mathlib import stripped, as does `BV.lean`. Seconds per file on a box
running at 7% of 32 cores.

**Scope, stated so it is not oversold.** A model rules out the
composition-and-liveness class — exactly the 07-08 class. It does not settle
whether `bound − inhabited` is the right quantity (semantics), cannot report
that the substrate holds no deposits today (empirical), and would not have
prevented the wrong-corpus error that hit three parties today.

## Deliberately out of scope

**`G(π)`.** Filling ⑨ moves the *engineering* gain `g`; it does not produce
`G(π)`. `fold-eval` #1 is *"deliberately not passed through the rollout/EFE
vocabulary: no predicted outcome distribution is present"*, evaluation (b) is
unbuilt, and codex-8's audit found no `P(s′ | π, s)` and no state-indexed
`G(s′)`. Conflating the two is the error this excursion exists to avoid.

## Joe's calls, outstanding

1. **Slice 4** — repair the record or make the corpus load degrade. Touches the
   enactment path.
2. **Slice 5** — the `:clean`/`:box-bindings` gap; a measurement-design question.
3. Whether **slice 2** is worth revisiting once more than one policy has flown.

## Stopping conditions

- Slice 2 stays blocked while the corpus carries one policy. Recording that is
  the result; do not retry it against the same corpus.
- If slice 4 lands and slice 5 still yields `bound = 0`, the ring stays red
  **with a working instrument attached** — a better-evidenced red than today's,
  and the correct place to stop.

## Related

- `futon2/holes/NOTE-step9-reachability.md` — slice 1 and the 88 outcomes.
- `futon2/holes/NOTE-what-stopped-2026-07-08.md` — the migration.
- `futon2/holes/NOTE-grounded-feed-missing-input.md` — slice 1b and the three layers.
- `p4ng/empirics-futon/NOTE-modular-formalisation-order.md` — the plan above, in full.
- `p4ng/empirics-futon/NOTE-a-lean-model-of-the-wm.md` — scope of what a model catches.
- `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md` — where slice 3 comes from.
