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
    [CORRECTED 2026-08-29, claude-15: the last :realized-outcome is
     2026-07-06T12:04Z (record 12 of wm-trace-2026-07-06.edn); from 13:04Z
     no record carries :enactment at all. The loop stopped ENACTING two days
     BEFORE d36086f/b624242. Cause open — see README-census-v1.md, final
     section. The 88 above is correct; "07-09" and "first run after the
     switch" are not.]
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

## Per-slice requirements, in the family vocabulary

*Added 2026-08-26 after `M-formal-war-machine` §2.1 mapped the APM machine's 35
predicates to seven WM requirement families. Each open slice now carries the
family it instantiates, the requirement, and — the point of the exercise — **the
naive fix that would recreate the defect.***

### Slice 5 — family 5 (provenance containment) + family 2 (non-empty handle)

**Requirement.** A producer must declare its domain, and must never return a
bare `nil` that conflates *out of domain* with *no measurement*.

    APM: validControllerMemoryUse:326 —
         surfacedIds.all (fun id => id ∈ accessibleIds ∨ id ∈ searchReceiptIds)
    WM:  the mission being grounded ∈ the producer's declared domain,
         and the result is typed: value | :domain-mismatch | :no-data

**❌ The naive fix that recreates the defect.** Add
`futon6-d/mission/bayesian-structure-learning` to `reviewed-candidate-cleans`.
That yields a **five**-entry hardcoded map with the identical failure mode, and
it is worse than it looks: **there is no CLean on disk for that mission**
(checked — 31 `.clean.edn` files, none matching), so the "fix" would also
require authoring a CLean for a mission that may legitimately not touch
substrate-2 at all.

**✅ The requirement-satisfying fix.** The vocabulary already exists **one
function above**, in the same namespace. `actuator_a3/a3-live-test:395`:

    :regime (cond (zero? bound) :domain-mismatch
                  (< inhabited bound) :discriminating
                  :else :all-inhabited)

`bound = 0` is a *named regime*, `:domain-mismatch` — "mission does not touch
substrate-2", per its own docstring. `realized-outcome-grounded` reaches the
same condition at `fold_realized.clj:163` and returns `nil`.

So: **derive the domain** (a mission is in-domain iff a reviewed CLean resolves
for it) and **return `:domain-mismatch`** instead of `nil`. No list is edited;
adding a CLean adds a mission automatically.

**And it exposes the real design question**, which the whitelist hid: the two
producers have *different domains*. `realized-outcome-of` covers any enacted
decision; `realized-outcome-grounded` covers missions with a substrate dial.
Selection between them should be **per-mission, by domain** — not global, by a
boolean flag. That is precisely what the module-1 emitter's producer-selection
table should carry.

**Acceptance.** For a mission with no CLean, ⑨ receives `:domain-mismatch` and
records it; for one with a CLean, a number. Neither is `nil`. No hardcoded
mission list is consulted anywhere on the path.

### Slice 4 — family 3 (digest agreement across a boundary)

**Requirement.** A durable record's reconstruction must not depend on state that
can change after the record was made.

    APM: validStudentTerminalCandidate:240 —
         receiptCandidateDigest = candidateDigest   (both inside the record)
    WM:  a deposit's prompt digest must be checkable from the deposit alone

**❌ The naive fix.** Repair the eight rejected deposits by recomputing their
shas against today's prose. They pass today and die on the next flexiarg edit —
and editing pattern prose is normal, encouraged work.

**✅ The requirement-satisfying fix.** Make the comparison internal to the
record: store the prose, or pin `pattern@git-sha` and read the historical blob.
The APM predicate compares two fields *of the same structure*; the WM compares a
stored field against a mutable tree, which is the whole defect.

**Acceptance.** A deposit that validated on the day it was made still validates
after any edit to `futon3/library`. Demonstrate on
`ft-bayesian-structure-learning-003`, whose two drifted proses
(`aif/expected-free-energy-scorecard` 08-23,
`structure/interest-event-vocabulary` 08-15) are the live case.

**Separate, cheap, and not the same question:** `fold_escrow/load-deposits`
degrades by design — *"valid deposits still serve"* — and
`actuator_a3/deposits-by-id:149` throws on any rejection. Whether that
strictness is load-bearing is a one-line decision for its author, independent of
the fix above.

### Slice 3 — family 2 (non-empty handle), operator side

**Requirement.** Unchanged, and now with a family: ⑧ per-channel precision and
㉛ the Morning Brief queue are both *"a step counts only if it left something
durable"* applied to the operator loop. The 2026-08-26 case is concrete —
codex-12's completion summary contradicted its own commits, and nothing recorded
that as a precision signal on the agent-self-report channel.

### What all three share

Every naive fix above **adds an entry** — one more mission, one more repaired
deposit, one more observation. Every requirement-satisfying fix **removes the
need for entries**, by deriving what was enumerated or internalising what was
external. That is the test to apply to slice work from here: *does this fix
scale by editing a list?* If yes, it is the whitelist again under another name.

## Global findings — the backlog these slices generated

Three findings from this diagnosis outlive the excursion and are tracked as
tickets, so modular work can proceed without losing them
(`holes/tickets/`, the draw pile `gen-wip-cards.py` reads into Cascade Live):

| ticket | requirement it proposes |
|---|---|
| `T-wm-wrong-corpus-26082026` | a recorded null result must name the corpus it is null about, as a path |
| `T-fixture-becomes-registry-26082026` | a producer declares its domain; a substitution may not shrink it silently |
| `T-evidence-pinned-to-mutable-prose-26082026` | durable evidence must not depend on a mutable tree |

**These are global requirements, not slice work.** Each was found while chasing
one ring and each constrains every module that follows. The second is already
the second clause of module 1's formal property; the other two want a home in
whatever the model says about evidence.

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

## The outline formalisation

**`mathlib4/DarkTower/WarMachine/GainChain.lean`** (2026-08-26, `d6824aeb` by
codex-18, reviewed and extended at `159a9c77`). Module 1 stated in Lean: the
four family predicates `threadedIdentity`, `inhabitedHandle`,
`durableBeforeFold`, `declaredDomain`, plus `typedAbsence`,
`dischargedPrecondition`, `domainNotNarrowed` and `gainAdvances`; the chain
property `gainChainSound`; and `foldCompliant`, which an out-of-domain mission
must satisfy even though it cannot move the gain. Six theorems named after the
dated incidents, including a positive witness modelled on the 88 pre-07-08
outcomes. Standalone — no Mathlib import — `lake env lean` exit 0, zero
`sorry`. It is an outline: no emitter, no Clojure contract, no mutation tests.

## Related

- `futon2/holes/NOTE-step9-reachability.md` — slice 1 and the 88 outcomes.
- `futon2/holes/NOTE-what-stopped-2026-07-08.md` — the migration.
- `futon2/holes/NOTE-grounded-feed-missing-input.md` — slice 1b and the three layers.
- `p4ng/empirics-futon/NOTE-modular-formalisation-order.md` — the plan above, in full.
- `p4ng/empirics-futon/NOTE-a-lean-model-of-the-wm.md` — scope of what a model catches.
- `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md` — where slice 3 comes from.
