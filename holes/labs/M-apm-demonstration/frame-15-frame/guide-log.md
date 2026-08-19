# f15-guide running log — frame 15, m93J06

Kept live so the frame's instrument findings survive a lost wake. Written by
`f15-guide`; not a store write.

## Open state verified before dispatching (2026-08-19)

- Bound: `conductor/status` returns `bound? true`, phase `guided-solve`, v9.
- Solver checkout `/home/joe/code/apm-frames/frame-15-m93J06-solver`, branch
  `exp/frame-15-m93J06-solver`, HEAD `a92ffb6c` — cut from
  `:reg/environment-revision`, NOT from apm-lean's current
  `repair/m99J06-val-ae`. No void-frame (f13) work inherited. Confirmed by
  `git -C <worktree> rev-parse HEAD`.
- `problems/m93J06/lean/Main.lean` in the worktree is byte-identical to the
  apm-lean anchor (md5 `ee1438589c49835b52d7d7f47ac5891d`; blob
  `793cfe3b51305ee997c7ba60a17553633999063e`). One `sorry`, line 265.
- `Soundness.lean` absent from the worktree, as declared. Probe commit
  `9ba574a6` touched only that file (+97, 1 file), so the frozen statement is
  untouched by the probe — verified by blob equality, not by trusting the pin.
- Bundle present: `problem.md`, `problem.tex`, `proof-outline.md`,
  `informal-solution.md`, `status.json`.
- Roster: `f15-solver`, `f15-student`, `f15-scribe`, `f15-proctor`, `analyst-3`
  all registered.

## Registration pin findings (carried from the ack)

- `:lean-revision 4331becf` = mathlib4, per `conductor_open.clj` `:lean-repo`.
  Ground control corrected my reading; recorded so the correction is not lost.
- `(:problem :regime) e7b9ec02` resolves in NO repository. Copied verbatim into
  all fourteen registrations; has never anchored anything. Declared
  `:dead-regime-pin` by ground control rather than silently fixed, so this frame
  stays comparable to its predecessors.
- The apm-lean anchor is `:reg/environment-revision a92ffb6c`. Two objects, one
  word "pin" — now disambiguated in the brief and containment witness.

## Prior for `:memory-contributes-to-close` — RULED

The honest prior is **ZERO attested instances on a sound problem**. f9/f10
recorded `memory-use steps = 0` (no instrumentation ran); the f10 "route memory"
reading was a prose attribution by a reader, not an attested step. **f15 is a
first measurement, not a replication.** Adjudicate on per-id `USED`/`IGNORED`
attestation only; never on resemblance, in either direction.

## Dispatch 1 — solver

    action-id : f15-dispatch-solver-1        v9 -> v10
    job-id    : invoke-1787153737431-5077-eb8f0615
    park-id   : park-d2befad8-efcf-4d96-9ecf-fae5fd9abba9  (engine-parked,
                surface `problem`, deadline 1787156438227 ≈ 16:20:38Z)
    cascade   : dispatch-recall-a7b88744-17ea-4bcd-b533-57139c93161d

### MEASUREMENT — D60 reproduced live, and it is worse here than in f13

The issued lexical query was:

    exponent OR closing OR conjunct OR existence

Four prose/meta words. Not one of `ode`, `flow`, `lipschitz`, `picard`,
`holder`, `gronwall`, `contdiff` — i.e. not one term that names this problem's
mathematics. f13's issued query (`finding OR equality OR clause OR strong`) had
the same shape, so this is the second consecutive frame in which
`default-query-term-limit` = 4 plus PROBLEM-CORPUS-IDF ordering spent every slot
on vocabulary shared by every problem in the corpus. The defect is not
intermittent.

### Offers: 5 surfaced, 1 lexical, 4 by pattern

| memory-id | via |
|---|---|
| `e-codexpilot-prove-eLpNorm-to-essential-supremum-by-superlevel-and-probability-monotonicity` | `:content-match` |
| `e-2ad2b4fe-6157-4dde-bffb-37d91290b45a` | `:pattern` |
| `e-codexpilot-apply-radial-R3-integration-through-a-one-dimensional-profile` | `:pattern` |
| `e-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation` | `:pattern` |
| `e-codexpilot-derive-local-L1-from-weak-L2-by-layer-cake-and-optimized-splitting` | `:pattern` |

`:memory-use/status :pending-outcome`, `used-ids []`, `rejected-ids []`,
`withheld-ids []` at dispatch. The single `:content-match` hit is a measure-theory
memory (eLpNorm → essential supremum); the four pattern-routed ones are radial
R³ integration, a measurable-space instance diamond, and layer-cake weak-L²
splitting. **On their face none is about ODEs.** That judgement is mine and is
NOT the measurement — the solver's per-id `USED`/`IGNORED` disposition is, and
the packet demands it explicitly and forbids flattering the count.

Exactly as the registration predicted: the lexical route underperformed and the
pattern cascade carried the load. Note the pattern cascade carried it toward
*integration theory*, because that is what the depositing corpus is made of.

## Pending

- Await job `invoke-1787153737431-5077-eb8f0615` (engine park; server-side turn
  enqueue on completion — surface `problem` is not a buffer surface).
- On wake: read the solver report, record the attempt, then decide guidance
  (typed `:answer` to a REPORTED residual only) vs advance.

---

## Solver attempt 1 — REVIEWED AS A GATE, verified independently

Commit `a68fa7e3` on `exp/frame-15-m93J06-solver`. Scope clean: only
`problems/m93J06/` (Main.lean, proof-outline.md, status.json). Worktree clean.

**What I checked myself, not taking the report's word:**

1. **Re-ran the compile.** `lake env lean problems/m93J06/lean/Main.lean`,
   EXIT=0, exactly two `declaration uses sorry` (lines 202, 241) and four
   linter warnings. Reproduces the solver's verbatim output exactly.
2. **Diffed the frozen statement.** Old `Main.lean:147-169` vs new `263-285`:
   IDENTICAL. The only deleted lines in the whole diff are two lines of the
   boundary-note prose and the bundled `sorry`. No definition above the theorem
   was touched.
3. **Grepped the diff for `axiom`, `native_decide`, `implemented_by`, `unsafe`,
   `admit`** — none.
4. **Ran `#print axioms` PER DECLARATION**, which the solver did not:

        apm_m93j06                        [propext, sorryAx, Classical.choice, Quot.sound]
        apm_m93j06_flow_exp_bound         [propext, Classical.choice, Quot.sound]
        apm_m93j06_not_holder_unique      [propext, Classical.choice, Quot.sound]
        apm_m93j06_global_exists_unique…  [propext, sorryAx, Classical.choice, Quot.sound]

   **Conjunct 5 (`apm_m93j06_flow_exp_bound`) is closed AXIOM-CLEAN on its own.**
   That is new mathematics delivered by this frame and it is not visible from
   the theorem-level axiom print, which `sorryAx` swamps.

### Accounting for the Analyst — state it this way or it will be over-credited

| conjunct | status | honest reading |
|---|---|---|
| 1 Picard–Lindelöf | NAMED RESIDUAL | uniqueness half COMPILED via `ODE_solution_unique_univ`; existence half is the residual |
| 2 C¹ flow regularity | NAMED RESIDUAL | only the trivial `t = 0` identity case discharged; punctured case is the residual |
| 3 linear growth | closed MODULO residual 1 | no clause-specific hole, but `choose`s trajectories from residual 1 — **not** an independent close |
| 4 Hölder non-uniqueness | CLOSED, free | citation of the pre-existing lemma, as registered. Not work. |
| 5 exponential Lipschitz | **CLOSED, AXIOM-CLEAN** | the one genuinely new closed conjunct |

**The `sorry` count went 1 → 2 and that is not a regression.** The acceptance bar
is a disjunction and it is the SECOND disjunct that is met: the single bundled
theorem-level `sorry` became named per-clause residuals, each a standalone lemma
carrying its nearest-API note and its empty searches. Anyone quoting "sorries
went up" without that decomposition is misreading the artifact.

Both residual lemmas are honest localizations, not weakenings: residual 1 states
existence ONLY (the ∃! wrapper is proved from it), and residual 2 states the
punctured version (the full conjunct is proved from it plus `flow_zero`).

### MEMORY MEASUREMENT — dispatch 1: 5 surfaced, 5 IGNORED, 0 USED

The solver returned a per-id disposition with a specific, checkable reason for
each: `eLpNorm` finite-exponent, additive `eLpNorm` approximation, radial R³
integration, measurable-space instance diamonds, weak-L² layer-cake. All five
are measure/integration theory; the problem is ODE flows on ℝ. The reasons match
what the ids say they are — this is not a blanket dismissal.

**This is a REAL OUTCOME, not a null result, and it must be reported as one.**

## Dispatch 2 — guidance, and a second D60 datapoint

    action-id f15-guide-solver-1   REFUSED (see below)      v10 -> v11
    action-id f15-dispatch-solver-2                         v11 -> v12
    job-id    invoke-1787154576940-5079-19a9b984
    park-id   park-59f7cae0-d688-43f0-b420-e7becb6bdb2a

Scoped to residual 1 ONLY (it gates conjuncts 1 and 3 together), with the route
change stated as: BUILD the constructor, do not search for it a third time —
the solver's empty searches and the file's own boundary note independently
agree Mathlib has none. Conjunct 5's axiom-clean close declared banked so it is
not regressed. Residual 2 explicitly out of scope for this attempt.

### D40 — RE-MEASURED AT PIN e1925203, STILL LIVE

`guide-solver` with `bell-type` "suggest" over HTTP:

    409 :guidance-type-invalid — "guide-solver requires a valid typed-bell performative"
    error/context {:bell-type "suggest"}

The string is never decoded to a keyword. So **the true count of typed guidance
bells this frame is 0**, and guidance was delivered as a `dispatch-solver`
packet labelled as such, per ground control's standing instruction. Record the
guidance-bell count as 0 — that is not an evasion, it is the only reachable path.

### :refusals-are-traceable — CONFIRMED, and D43 needs REFINING

The refusal IS in the persisted trace as an `:action-refusals` receipt
(`:refusal/action-id "f15-guide-solver-1"`, `:refusal/tool :guide-solver`,
step-index 20), carrying `:error/code :guidance-type-invalid` and its message.
f10's D6 — refusals existing only in the guide's prose — is fixed.

**But D43 as written ("receipts say only 'Tool execution failed'") is now only
half true, and the surviving half is sharper.** The receipt carries the code and
message; what it DISCARDS is `:error/context`. `record-action-refusal!` selects
exactly `[:error/component :error/code :error/message]` from the error, so the
offending value — here `{:bell-type "suggest"}`, the single most diagnostic fact
about this refusal — is present in the live HTTP response and ABSENT from the
trace. `:refusal/args` sanitizes it to `{:arg/type :string}`. An analyst reading
only the trace can see THAT a guidance bell was refused and WHICH rule refused
it, but not what was actually sent.

### D60 — second datapoint, and the packet text does not move the query

    dispatch 1: exponent OR closing OR conjunct OR existence
    dispatch 2: exponent OR closing OR residual OR existence

One word changed. Dispatch 2's packet is saturated with `Picard`, `Lipschitz`,
`ODE`, `continuation`, `compact exhaustion`, `flow` — and NONE of it reached the
query. The same five measure-theory memories surfaced again, by the same routes
(1 `:content-match`, 4 `:pattern`). So the retrieval is not merely
under-selective, it is **insensitive to the dispatch text**: two packets with
very different mathematical vocabulary produced near-identical prose-word
queries and an identical memory set.

---

## Solver attempt 2 — RESIDUAL 1 CLOSED. Gate review, verified independently.

Commit `27546a85` ("m93J06 build global Picard continuation"), +512/−3 on
Main.lean, scope clean (only `problems/m93J06/`), worktree clean.

**What I checked myself:**

1. **Re-ran the compile.** EXIT=0, exactly ONE `declaration uses sorry`, line
   612. Reproduces the solver's output.
2. **First 146 lines byte-identical to base** (md5 `66dff80d312b18f73e7ff1d7a803fef6`
   both sides). This is the strongest form of the "definitions untouched" check:
   `apm_m93j06_Solves` and `apm_m93j06_IsFlow` are provably the frozen ones, so
   the theorem cannot have been made easier by weakening what it quantifies over.
   `Solves f y₀ y := y 0 = y₀ ∧ ∀ t, HasDerivAt y (f t (y t)) t` — a genuine
   solution on ALL of ℝ, not a local one.
3. **Frozen statement diffs clean** (old 147-169 vs new 634-656). The change is
   purely additive: the only 3 deleted lines in the whole diff are two lines of
   boundary-note prose and the old bundled `sorry`.
4. **All 14 new declarations are `lemma`s.** No `def`, `structure`, `instance`,
   `axiom`, `native_decide`, `implemented_by`, `unsafe`, or `set_option`.
   Consequence for a required measurement: **no new definitions were introduced,
   so the "definition must take a non-trivial value in a concrete case" rule is
   INAPPLICABLE this frame** — not passed, not failed.
5. **Ran `#print axioms` per declaration myself:**

        apm_m93j06_global_exists_of_continuous_lipschitz  [propext, Classical.choice, Quot.sound]
        apm_m93j06_global_exists_unique_of_…              [propext, Classical.choice, Quot.sound]
        apm_m93j06_flow_exists_of_continuous_lipschitz    [propext, Classical.choice, Quot.sound]
        apm_m93j06_flow_exp_bound                         [propext, Classical.choice, Quot.sound]
        apm_m93j06_isIntegralCurve_abs_add_one_of_…       [propext, Classical.choice, Quot.sound]
        apm_m93j06                                        [propext, sorryAx, Classical.choice, Quot.sound]

### Status: FOUR of five conjuncts closed axiom-clean

| conjunct | status |
|---|---|
| 1 Picard–Lindelöf global ∃! | **CLOSED, axiom-clean** |
| 2 C¹ flow regularity | NAMED RESIDUAL (the only `sorry`, line 612) |
| 3 linear growth ⟹ flow | **CLOSED, axiom-clean** (no longer modulo residual 1) |
| 4 Hölder non-uniqueness | CLOSED by citation (free, pre-existing) |
| 5 exponential Lipschitz bound | **CLOSED, axiom-clean** |

`sorry` count 1 → 2 → **1**, and the surviving one is a NAMED per-clause lemma
rather than the original bundled theorem-level hole.

### What was actually built, and why it is the frame's result

The construction is the compact-exhaustion/gluing argument the frozen file's own
boundary note said Mathlib lacks, and it is legible: for each radius `a` obtain a
solution on `Ioo (-a) a`; prove nested symmetric solutions agree; then define
`y t := γ (|t| + 1) t` and prove that this diagonal IS a global integral curve.
The `|t| + 1` diagonal is what avoids needing a direct-limit construction.
Uniqueness is then Mathlib's `ODE_solution_unique_univ`. Nine supporting lemmas,
all axiom-clean.

The Lean kernel is the gate here and it is a strong one: since the frozen
definitions are byte-identical and `apm_m93j06` typechecks against the unchanged
statement with `sorryAx` reachable ONLY through conjunct 2, conjuncts 1/3/4/5
are proved outright. My job was to confirm nothing outside the kernel was gamed
— statement edits, added axioms, `native_decide`, hidden sorries — and none was.

### MEMORY MEASUREMENT — dispatch 2: 5 surfaced, 5 IGNORED, 0 USED

Same five memories, same routes, reasons again specific and checkable. Running
total across the solver phase: **10 offers, 0 USED, 10 IGNORED.**

The conjunct-1 construction is exactly the kind of transferable work the store
exists to accumulate — and NOTHING in the store contributed to it.

## Dispatch 3 — the last residual

    action-id f15-dispatch-solver-3                      v12 -> v13
    job-id    invoke-1787155612296-5082-1844680c
    park-id   park-df0ea42f-a748-497e-a739-351d9dfcc02c

Scoped to residual 2 alone (line 612), with conjuncts 1/3/4/5 declared banked.
Closing it makes the whole frozen theorem axiom-clean. Fallback shapes stated
explicitly (Lipschitz dependence + localised differentiability step; close under
a stated extra hypothesis; split into named pieces) so a partial is committed
rather than discarded. Halt-and-report instruction repeated for the case where
the conjunct turns out false.

---

## Solver attempt 3 — **m93J06 IS CLOSED.** Full gate review.

Commit `9ff4f866` ("solve m93J06 flow regularity"). Chain: `a68fa7e3` (conjunct
5) → `27546a85` (conjuncts 1 and 3) → `9ff4f866` (conjunct 2 + full closure).

**Everything below I ran myself. I did not take the solver's report for any of it.**

1. **Recompiled.** `lake env lean problems/m93J06/lean/Main.lean` → EXIT=0,
   **0 `declaration uses sorry`, 0 errors**, four cosmetic linter warnings.
2. **`grep -cE "\bsorry\b|\badmit\b"` over the whole file → 0.**
3. **Frozen statement SHA-256 reproduced independently:**
   `15df85c8bd0c9110014df51b491e50e949508a7e98b53aab5ad22f05e112352d`,
   identical to the same line range at base `a92ffb6c`. Matches the solver's
   claimed hash.
4. **First 146 lines byte-identical to base** (md5 `66dff80d…`). Every frozen
   definition — `apm_m93j06_Solves`, `apm_m93j06_IsFlow` and the early lemmas —
   is provably untouched.
5. **Purely additive diff**: +960/−3 on Main.lean (`git diff --numstat`; the `--stat` column reads 963 because it totals changed lines, insertions PLUS deletions — my earlier figure misread that column), the 3 deletions being two
   lines of boundary-note prose and the original bundled `sorry`.
6. **Declaration audit of every added top-level line**: no `def`, `axiom`,
   `instance`, `structure`, `class`, `abbrev`, `notation`, `macro`, `opaque`,
   `partial`, `unsafe`, `local`, `open`, `set_option`, `attribute`, or `@[…]`.
   All additions are `lemma`s. Also exactly ONE `theorem apm_m93j06` in the file
   — no shadowing declaration.
7. **`#print axioms apm_m93j06` run by me:**

        'apm_m93j06' depends on axioms: [propext, Classical.choice, Quot.sound]

   **AXIOM-CLEAN.** No `sorryAx`.

### Verdict on the artifact axis

m93J06 is CLOSED: zero executable sorries, axiom-clean, frozen statement
mechanically unchanged, in 3 solver dispatches. `:problem-closed-on-artifact` is
confirmed on its FIRST disjunct (the strong one), not the residual-reduction
fallback.

Why the kernel makes this a strong gate: the frozen definitions are
byte-identical, the statement hash matches, and no axiom or elaboration-affecting
declaration was added — so there is no route by which the theorem could have been
made easier outside the kernel. Everything else is Lean's problem, and Lean
accepted it.

### What was proved, per conjunct

1. **Global Picard–Lindelöf** (∃! solution on all of ℝ) — built from scratch:
   uniform local Picard radius `1/(2(L+1))`, compatibility of nested symmetric
   solutions, and the diagonal `y t := γ (|t| + 1) t` glued into a global
   integral curve. This is the compact-exhaustion/continuation constructor the
   frozen file's own boundary note said Mathlib does not contain.
2. **C¹ dependence on initial data** — local uniqueness, strict order
   preservation of the flow, compact flow tubes, local Lipschitz dependence, the
   divided-slope variational equation, and the derivative formula
   `exp (∫ s in 0..t, ∂ᵧ f s (φ s a))`. The solver proved the STRONGER statement
   that every time slice is C¹, not merely slices near 0.
3. Flow existence — falls out of 1 by choosing the unique trajectory pointwise.
4. Hölder non-uniqueness — citation, free, as registered.
5. Two-sided exponential Lipschitz bound — forward Grönwall plus the reversed
   field for negative time.

### MEMORY MEASUREMENT — dispatch 3: 5 surfaced, 5 IGNORED, 0 USED

**SOLVER-PHASE TOTAL: 15 offers delivered, 0 USED, 15 IGNORED.**

The same five measure-theory memories were surfaced all three times, with a
specific and checkable refusal reason each time. The problem closed anyway, and
the mathematics that closed it — a global Picard construction and a variational
C¹-dependence argument — is exactly the kind of transferable technique the store
exists to hold.

## Cascade measurements (registration asked for three; here they are)

From the recorded offers in `v14.edn`:

- **offers by route** (per dispatch): 5 `:leaf`, 48 `:why-hop`, 52
  `:co-incidence` = **105 offers per dispatch**, 315 across the three.
- **patterns per problem**: **1**. Exactly one seed pattern hangs off the five
  recall-surfaced memories; the cascade reached 10 distinct patterns from it.
- **cascade truncation**: `:truncated? true`, `expanded-available 132` against
  `cap 100` — **32 expansions dropped per dispatch**.

### THE OFFER COUNT IS NOT WHAT IT LOOKS LIKE — read this before quoting it

**105 "offers" per dispatch, but only the 5 `:leaf` ones were ever shown to the
solver.** `:memory-use/surfaced-ids` carries 5; the solver's packet listed 5; the
solver attested on exactly 5, three times. The 100 cascade-expanded offers are
computed in `cascade-receipt-offers`, which runs from `memory-offers` inside
`record-solver-attempt!` — i.e. **at RECORD time, after the solver has already
finished**. They are a post-hoc trace artifact, not something anybody was
offered.

So "315 offers, 15 dispositioned" would be a false reading of this frame, and
`:offer-disposition-populated` must be adjudicated with that distinction stated.
The solver could not have dispositioned the other 300: it never saw them.

`:cascade-seeds-from-recall` is CONFIRMED — the expansion is seeded from the
patterns attached to the recall-surfaced memories, exactly as the registration
predicted, and not from the problem's own touch-set.

## New defect — a long action reports the session as UNBOUND, not busy

`record-solver-attempt` took **~7 minutes**, because `memory-offers` expands the
cascade at record time and each substrate query costs ~2.5s (measured), over
3 receipts × 5 seeds and their pattern/problem neighbourhoods.

While it was running, a second action against the same binding returned:

    409 {"ok":false,"error/code":"conductor-session-unbound"}

The session was NOT unbound — the status endpoint reports `bound? true` and the
first action landed correctly (v13 → v14, phase `intervene`). A busy binding is
reported with the same error code as a dead one. **This is dangerous**: the
documented response to `:conductor-session-unbound` is to re-bind or take over,
and doing that against a still-running action would race the cycle's own state.
It needs a distinct code (`:conductor-action-in-progress`).

Replay protection did work — the action-id was not double-executed.

---

## Phase progress: guided-solve → intervene → promote-solver

    f15-record-solver-attempt-1   v13 -> v14   (attempt/f15/solver-0, 3 commits,
                                                axiom-clean? true, sorries 0,
                                                closer-hops 3)
    f15-deposit-1                 v14 -> v16   e-1929b416-4264-4fd5-a22d-403b8ed02560
    f15-dispatch-scribe-1         v16 -> v17   job invoke-1787158354719-5086-ad53417c
                                                park park-145b5236-6aec-482a-abe6-645fc8dc55d1

### :scribe-card-pinned-resolves — CONFIRMED

The machine injected

    :scribe-card-path "/home/joe/code/futon3c/holes/labs/M-apm-demonstration/role-cards/scribe-v2.md"

resolved from the registration's pinned blob `02441d9d…`, with no guide
declaring it authoritative by hand. D8's fix reached the running image. That is
the second of the two "did the source fix actually land" tests this frame was
sequenced to make. All three solver job ids were passed through to the scribe.

### STRUCTURAL FINDING — the guide is FORCED to write an unpromotable memory

`:intervene`'s only non-advance tool in store-mode is `:write-substrate`
(`autoconf`, problem.clj:143-153), and the only conductor operation that reaches
it is `deposit!`, which advances the phase as its last act. **So the single
deposit is mandatory: there is no way out of `:intervene` except by writing
exactly one memory.**

And by D41 that memory can never be promoted — `promote-artifact` requires
reviewer == acting-identity (always the guide) AND reviewer ≠ depositor, and
`write-substrate` stamps the conductor as author. So the machine COMPELS the
guide to write into the store and simultaneously GUARANTEES the write can never
become findable.

This is not a new defect so much as D41's consequence made concrete and
unavoidable: it is not that a guide *may* waste a deposit, it is that every
frame in store-mode must produce exactly one dead memory. Mine is
`e-1929b416-4264-4fd5-a22d-403b8ed02560` (the diagonal globalization technique).
I told the scribe it exists, told it why it is dead, and asked it to re-author
the technique in its own judgment if it clears the shelf bar — rather than skip
it as "already recorded".

---

## Scribe review and promotion — I judged, I did not stamp

Scribe returned two deposits, ordered by its own assessment of promotion value,
plus an honest lane report.

### My review of deposit 1 — `e-4f6b5d49…` "globalize-compatible-local-objects-by-pointwise-diagonal"

Read the content myself. It is BETTER than my own dead deposit: it abstracts the
method away from ODEs ("compatible family on an exhaustion → pointwise diagonal
with a dominating index"), keeps the concrete instance, and — the part that
matters — records the non-obvious working step: *to prove a local property of
the diagonal at `t`, freeze one radius larger than a neighbourhood of `t` and
rewrite the varying-radius diagonal to that fixed member*. It also states its
own failure condition (uniqueness fails, or overlap agreement needs extra
choices). Generalizes. **APPROVE.**

**But I REASSIGNED its pattern.** The scribe proposed
`math-strategy/proof-architecture`. I checked the alternatives rather than
accepting it:

- `math-strategy/exhaustion-as-theorem` — **a false friend.** Despite the name
  it is about exhausting a *technique class* to prove a barrier is fundamental,
  nothing to do with compact exhaustion of a domain. Rejected as a home.
- `math-strategy/construction-before-estimates` — "when a proof needs an object
  assembled from infinitely many pieces, define the whole object and prove it
  well defined BEFORE proving anything about its size", context "a construction
  given by cases over an infinite family". That is exactly this memory: prove
  overlap compatibility first, then define the diagonal, then rewrite locally
  before taking derivatives. It is also `@why math-strategy/proof-architecture`,
  so it sits strictly UNDER the scribe's proposal — a more specific home that
  preserves cascade reachability to the general one.

Promoted onto `construction-before-estimates`. Verified in the store afterwards:
`attachment-status reviewed`, `patterns ["math-strategy/construction-before-estimates"]`,
`reviewer f15-guide`.

### My review of deposit 2 — `e-00c94a8d…` "differentiate-scalar-flows-through-divided-slopes"

Read it myself. Names the layers and the reduction between each pair —
no-crossing → quotient `q = (φ(s,b) − φ(s,a))/(b−a)` → divided slope `A` with
`q' = A·q` → positivity from strict order preservation → log-integrate to
`q(t) = exp ∫A` → compact tube for a uniform bound → dominated convergence →
continuity → assemble `ContDiff`. Crucially it states its own scope limit: the
scalar order/no-crossing step is essential and higher dimensions need a matrix
variational equation. **APPROVE on the scribe's proposed pattern**
`math-strategy/proof-architecture` — here the fit is exact, because the memory
IS a layer architecture. Verified `reviewed` in the store.

### D62 — promoted ONE AT A TIME, with gaps. 2/2 clean.

Against frame 13's 3/3 failure when three promotions ran back-to-back in one
shell loop. Each promotion was confirmed to have reached `:reviewed` (not stuck
at `:proposed`) BEFORE the next was issued. This is a second datapoint for
f13-guide's hypothesis, offered as such and not as proof.

`:reviewed-attachment-gained` — **CONFIRMED, and it is GUIDE-reviewed.** Per the
registration's qualifier this does NOT demonstrate independent review: depositor
`f15-scribe`, reviewer `f15-guide`, which is the only reachable path (D41/D42).

### Card/machine mismatch — the reviewer's verdict vocabulary does not exist

The scribe card gives the reviewer three verdicts: **approve / reassign /
reject**. The machine's `promotion-verdicts` is `#{:approve :challenge :reject}`.
So `reassign` — the verdict I actually needed and used — **is not expressible**;
it survives only as "approve with a different `pattern-id`", which records in the
trace as a plain approval and loses the fact that the reviewer overrode the
proposer's pattern choice. And `:challenge` has no meaning in the card at all.
A frame that reassigns is indistinguishable in the trace from one that rubber-
stamps, which is precisely the distinction the review protocol exists to make.

### Scribe's gate refusal, recorded because the receipt cannot

First deposit attempt was refused: it used subject type `commit`, while the
ArtifactRef contract requires `git-commit`. **The refused attempt still minted
an evidence id, `e-54eafdd3-be5a-4918-96c2-db6c2c30c09f`** — an orphan id for a
memory that does not exist. `record-memory!` mints before validating ("the
evidence id is minted before validation or I/O and is returned on every
failure"), which is deliberate, but it means refused deposits leave id-shaped
debris that a later reader can mistake for a lost memory.

### Lanes recorded: solve (ran, 2 memories), arc (ran, 0), trajectory (ran, 0), challenge (not run)

The scribe ran `arc` on the solver transcript and found real error→fix spans —
equality orientation for `ODE_solution_unique_univ`, explicit interval-
integrability endpoints, converting the diagonal divided slope to `deriv`,
fully qualifying filter/measure APIs — and deposited NONE of them, judging them
too thin individually to clear the transfer bar. `trajectory` likewise ran and
deposited nothing, on the ground that the expensive stretches were already
represented by the two solve memories. I accept both judgements: an empty lane
honestly reported beats a padded one, and the scribe said which lanes ran rather
than reporting yield as if it were coverage.

## THE SCRIBE REPORTED THREE `USED` AND NONE OF THEM COUNTS

The scribe's attestation block reads:

    USED e-1929b416…  (my deposit) — read as an independent comparator
    USED e-4f6b5d49…  — verification search confirmed it is statusless/patternless
    USED e-00c94a8d…  — verification search confirmed it is statusless/patternless

**Not one is an instance of `:memory-contributes-to-close`, and a naive
aggregator would score this frame 3 USED.**

- `e-1929b416…` was not retrieved. **I named it in the scribe's packet and told
  it to read it.** That is guide-directed reading through the dispatch channel,
  not the memory machinery working.
- `e-4f6b5d49…` and `e-00c94a8d…` are **the scribe's own deposits, minutes old**,
  "used" to check its own writes were statusless. That is self-verification, not
  memory use.

The attestation channel accepts `USED` for memories that were never retrieved,
including ones the attester authored itself in the same session. Nothing checks
the claimed id against the surfaced set. **Any aggregate `USED` count in this
series must be filtered against `:memory-use/surfaced-ids` before it means
anything.** Recorded because this is exactly the failure mode the frame was
warned about — inferring use from resemblance — arriving from the direction
nobody was watching: not a false positive of judgement, but a category error
about what "used" means.

**Solver-phase measurement stands unchanged: 15 offers, 0 USED, 15 IGNORED.**

## Student dispatched — the clean transfer test

    f15-dispatch-student-1   v23 -> v25
    job    invoke-1787159146808-5088-10991dba
    park   park-45a6b939-0776-4342-bc18-45ea650981e1
    budget 60 min wall clock, attempt cap 3

- **Eligible set: 495 memories, and BOTH promoted memories are in it.** My own
  unpromotable deposit is correctly absent.
- **Containment verified by hand**: student checkout
  `frame-15-m93J06-student-deefb9b8…` is at `a92ffb6c`, its `Main.lean` is the
  265-line frozen file with ONE `sorry`, and `git log` shows the solver's three
  commits are NOT in its history. It cannot see the solution.
- The packet does not leak the route: no diagonal, no divided slopes, no commit
  shas, no statement of whether anyone else succeeded. Same acceptance bar, same
  partial-credit shapes, same per-id attestation demand with the same
  anti-inflation language.

This is the measurement the whole frame was built for: the student's ONLY route
to the solver's knowledge is the two promoted memories.

---

## Student attempt — gate-reviewed, and the transfer measurement is IN

Commit `a793f7e` on `exp/frame-15-m93J06-student-deefb9b8…`, single commit on
base `a92ffb6c`.

**Verified by me, independently:**

1. **Containment**: `git log` in the student tree contains NONE of the solver's
   three commits (`a68fa7e3`, `27546a85`, `9ff4f866`) — grep count 0. It could
   not see the solution.
2. **Definitions untouched**: first 146 lines md5 `66dff80d…`, identical to base.
3. **Frozen statement unchanged**: SHA-256 of the statement block is
   `15df85c8…`, the same hash as the frozen original and as the solver's.
4. **Scope**: one file, +97/−1, the single deletion being the bundled `sorry`.
5. **Declaration audit** (including `private`/`protected`/attribute prefixes):
   four theorems and one private theorem. No `def`, `axiom`, `instance`,
   `set_option`, `attribute`, `notation`.
6. **Recompiled**: EXIT=0, exactly THREE `declaration uses sorry` (167/176/184),
   matching the three named residuals.
7. **`#print axioms` run by me** (the student's own run was a temporary append,
   reverted before commit, so this needed re-doing):

        'apm_m93j06'       [propext, sorryAx, Classical.choice, Quot.sound]
        'apm_m93j06_conj5' [propext, Classical.choice, Quot.sound]

### Result: real, verifiable progress in 60 minutes from a fresh agent

| conjunct | student | note |
|---|---|---|
| 1 | NAMED RESIDUAL | `apm_m93j06_conj1_residual`, full conjunct statement |
| 2 | NAMED RESIDUAL | `apm_m93j06_conj2_residual` |
| 3 | NAMED RESIDUAL | `apm_m93j06_conj3_residual` |
| 4 | CLOSED | cited the pre-existing lemma, correctly, and did not re-derive it |
| 5 | **CLOSED, AXIOM-CLEAN** | `apm_m93j06_conj5` + private `…_gronwall_forward` |

One opaque bundled `sorry` → three localised per-clause residuals, each carrying
its nearest-API note and empty searches, plus one conjunct closed axiom-clean.

**And it closed conjunct 5 by a DIFFERENT ROUTE than the solver.** The student
used Mathlib's `norm_le_gronwallBound_of_norm_deriv_right_le` with
`gronwallBound_ε0`, applied to `s ↦ φ s a − φ s b`, handling `t < 0` via the
reversed-time flow. The solver used `dist_le_of_trajectories_ODE`. Two
independent axiom-clean proofs of the same conjunct, from two agents that could
not see each other. That is a genuine replication of conjunct 5.

## THE HEADLINE MEASUREMENT: promoted → surfaced → used BREAKS AT *SURFACED*

The student's attestation, verbatim in substance:

- `psr_search "Lean ODE Picard-Lindelof global solution Gronwall flow"` returned
  pattern candidates `futon-theory/interface-loop`,
  `math-formalization/coercion-bridge`, **`math-informal/local-to-global`** —
  every one reported `no-reviewed-attachment`. Attested **IGNORED**, with the
  reason that they were generic hooks with no Lean/ODE/Mathlib content.
- `memory_search` on tags `m93J06` / `frame-15` returned **EMPTY**.

**The two memories I had promoted forty minutes earlier were never surfaced to
the student at all.** Not surfaced-and-unused: never offered.

I verified the mechanism rather than guessing at it:

    math-informal/local-to-global              →  0 memory attachments
    math-strategy/construction-before-estimates →  2 edges (ours among them, :reviewed)

**The student's semantic search found the RIGHT pattern name and that node was
empty.** The diagonal memory's own tags literally include `:local-to-global`,
and the pattern it hangs off is `construction-before-estimates`. Nothing
reconciles a memory's tags with the pattern it is shelved on, so the reader
searched the correct aisle and the book was on a different one.

And the cascade could not bridge the gap either: the relations out of
`construction-before-estimates` are its own clause nodes plus
`math-strategy/proof-architecture`,
`math-strategy/isolate-computational-kernel-before-transport`, and
`math-informal/construct-an-explicit-witness`. **There is no edge to
`math-informal/local-to-global`.**

The second failure is independent and simpler: the student searched by TAG for
the problem id, but memories carry the problem as a SUBJECT
(`:evidence/subject {:ref/type :problem :ref/id "m93J06"}`), not as a tag. A
subject query finds 12 m93J06 memories in the store including both of ours; the
tag query the student naturally reached for finds none.

### The uncomfortable part, stated plainly

**I made this worse by reviewing well.** I moved the diagonal memory off the
scribe's proposed `proof-architecture` onto `construction-before-estimates`
because it is the more precise home — and it is. Neither pattern is the one the
student's search reached. Had I seen the student's query first I would have
chosen `math-informal/local-to-global`, which is where a reader looking for this
technique actually goes. **The reviewer picks the shelf by content; the reader
searches by need; the machine has no step that reconciles them.** A more careful
review produced a less findable memory, and nothing in the loop can detect that.

This is the sharpest form of the frame's result: it is not that the memories
were unhelpful. It is that the pipeline has an unmeasured join between
promotion and retrieval, and this frame is the first to instrument both ends at
once and see the gap.

---

## Adjudicated: **:closed**

    f15-record-students-1  v25 -> v26    (attempt/f15/student-0, 3 residuals,
                                          axiom-clean? false, commit a793f7e)
    f15-write-use-1        v26 -> v341   ALL 315 offer ids dispositioned
    f15-adjudicate-1       v341 -> v343  outcome :closed, residual-sorries 0,
                                          axiom-clean? true
    f15-dispatch-scribe-2  v343 -> v344  post-adjudication mine

`:offer-disposition-populated` — the write-use pass completed over every offer
id, 315 of 315, so F3's subset check has the material it needs at trace
derivation. It ran, and that is the capability becoming satisfiable rather than
merely declared.

### COST FINDING — dispositioning offers costs 287 MB and 315 versions

`write-uses!` issues one `saved-step` per offer, and every `saved-step`
checkpoints a FULL state snapshot. So dispositioning this frame's offers wrote
**315 state files, ~870 KB each and growing, 287 MB total**, took ~5 minutes,
and advanced the version counter from 26 to 341.

The version inflation is the part that will bite someone: a frame's version
number now says almost nothing about how much conducting happened — 315 of my
341 versions are one bulk bookkeeping action. And the disk cost scales with the
CASCADE size, not with what was actually offered to anyone: 300 of those 315
offers were never shown to a single agent (see the offer-inflation finding
above). **We paid 287 MB to disposition 300 offers that nobody was ever made.**

---

## THE MACHINE'S OWN RETRIEVAL PROBE — the cleanest number this frame produced

`:rprobe` records available-vs-retrieved per dispatch. Ten probes in the cycle:

| dispatch | available | retrieved |
|---|---|---|
| solver ×3 | **6** | 5 |
| student | **495** | **0** |
| scribe/other ×6 | 6 or 0 | 5 or 0 |

Two facts, both measured by the harness rather than inferred by me:

**1. The solver's eligible pool was SIX memories.** Not 495 — six. Recall
surfaced 5 of those 6, so dispatch-time retrieval was working nearly perfectly
*against a shelf that was almost empty*. The frame's solver-phase result is
therefore NOT "retrieval failed to find the good memory": it is "there were six
memories to choose from, five were shown, none was about ODEs." Those are very
different findings and only the probe separates them.

**2. The student had 495 eligible and retrieved ZERO.** The machine's own
instrument says dispatch-time recall returned nothing at all — not a bad ranking,
not a truncated list, an empty one. `:rprobe/retrieved-ids []` against
`:rprobe/available-ids` containing both `e-4f6b5d49…` and `e-00c94a8d…` by name.

So the promoted memories were in the eligible set, named in the probe, and
surfaced to nobody. The student then tried on its own initiative — its packet
carries a `memory-pull-invitation-v2` telling it to use `memory_search`,
`pattern_memory`, `evidence_graph`, `psr_search` mid-session — and its searches
returned only unattached patterns. **It did everything the invitation asked.**

### Correction to an earlier suspicion of mine

I flagged that the scribe was handed `:student-job-ids []` and started to write
it up as a defect. It is not one: that empty list is scribe dispatch **1**, at
`:promote-solver`, before the student had run — correctly empty. Scribe dispatch
**2** carries `["invoke-1787159146808-5088-10991dba"]`. My first regex match
took the earlier occurrence. No defect; the machine is right.

---

# CLOSE — `completed? true`, phase `null`, v356

    f15-promote-3      v344 -> v345   e-56ae9c01 onto the NEW pattern
                                      math-formalization-CA/ode-gronwall-api
    f15-promote-lane-1..4  v345 -> v349
    f15-close-1        v349 -> v356   cycle closed

**Promotions 3/3 clean, each one at a time with a gap, each verified `:reviewed`
in the store before the next was issued.**

## Scribe pass 2 — it authored a pattern, and the watcher ingested it

One deposit, `e-56ae9c01…`: when to choose `dist_le_of_trajectories_ODE` versus
`norm_le_gronwallBound_of_norm_deriv_right_le`, plus the shared reversed-time
construction for `t < 0`. It is the *replication* memory — the two independent
routes to conjunct 5 — and it exists only because the student worked blind and
landed on a different API than the solver.

I asked it to propose the pattern **a reader would search for**, not only the
best content fit, and it did exactly that: content fit
`math-informal/reduce-to-known-result`, reader-search home the new
`math-formalization-CA/ode-gronwall-api`, recommendation to take the reader
home. It authored the file, the watcher ingested it (10 relations live), and it
carries `@see-also math-informal/local-to-global` — the node the student's
search actually reached. Promoted onto the reader home.

**Caveat I must state:** the cascade traverses `pattern/has-semantic-why` ONLY
(`why-targets-fn`, conductor.clj). A `@see-also` edge does NOT feed it. So the
new pattern's link to `local-to-global` helps tools that read see-also and does
nothing for the cascade.

### I DECLINED to re-home the diagonal memory, deliberately

The scribe's ruling was that `math-informal/local-to-global` is the right home
for `e-4f6b5d49…` and that precision should not override the query path. **I
agree with the ruling and did not act on it.** Re-promoting an already-`:reviewed`
edge onto a second pattern hits the exact predicate D62 makes dangerous: the
attach lands before the review, and a refusal after the attach poisons the
memory permanently. The memory is currently good. Trading a live, reviewed
memory for a better shelf position, on a promotion path known to be non-atomic,
is not a trade I will make inside a frame. **Recommendation for ground control:
re-home `e-4f6b5d49…` to `math-informal/local-to-global`, or add a `@why` (not
`@see-also`) edge, once promotion is atomic.**

## The machine's own verdict: `:launchable? false`, three failures

    :malformed-cycle-attempts
    :f7-missed-available-artifact
    :f9-capability-not-realized

### 1. `:malformed-cycle-attempts` — MY ERROR, and it is uncorrectable

`attempt?` requires `:cycle/store-revision` as a 40-char sha. **I did not supply
it** in either the solver or the student attempt record. I inferred the attempt
shape from frame 13's persisted state, which also lacks the field — f13 halted
before close, so its malformed attempts were never validated and the gap was
invisible. The rehearsal frame a01A06 DID carry it.

Mine, and I am recording it rather than leaving it for the Analyst to find.

**But note the harness shape too**: attempt records are validated ONLY at close,
330-odd versions after they are written, with no route back — `record-solver-attempt`
is a `:guided-solve` tool and the phase is long gone. A validate-on-record would
have cost one second and caught it. As it stands the machine accepts a malformed
attempt silently and reports it when nothing can be done.

### 2. `:f7-missed-available-artifact` — the machine found the retrieval gap too

    :available-artifact-ids     513
    :need-probe-retrieved-ids    15

F7 requires available ⊆ retrieved. **498 available artifacts were never
retrieved.** This is the harness's own instrument independently reaching the
conclusion I reached by hand from the student's report and the `:rprobe` numbers.
Three independent routes to the same finding: the student's attestation, the
retrieval probe, and F7.

### 3. `:f9-capability-not-realized` — exactly TWO capabilities fail

All eight probes were RECORDED (`:f9-capability-probe-missing` correctly absent).
Computed from the trace:

| capability | holds? | evidence |
|---|---|---|
| frame-containment-witnessed | ✓ | probe recorded and passed |
| created-frame-worked | ✓ | scaffold `7117a38b…` ≠ closing `952b184b…` |
| unique-disposition | ✓ | exactly 1 disposition id |
| offer-use-disposition | ✓ | **315 of 315 offers dispositioned** |
| need-retrieval | ✗ | 513 available ⊄ 15 retrieved |
| promotion-importable | ✓ | 3 of 3 |
| promotion-need-taggable | ✓ | 3 of 3 |
| measurement-populated | ✗ | **8 of 17 required fields** |

**`:offer-disposition-populated` is CONFIRMED** — F3 passes, the first time in a
cascade-enabled frame.

### `:measurement-populated` IS UNSATISFIABLE BY CONSTRUCTION — new defect

`record-measurement` takes NO arguments, derives everything itself, and is not
in the conductor's operations map — only `close!` calls it. It computes 8 fields:

    terminal disposition · residual executable sorries · axiom cleanliness ·
    locked-lemma exposure · attempts or closer hops · memories promoted ·
    scribe lane coverage · arc-lane yield

The registration requires 17. The nine it never computes:

    statement defects at review · review escape rate ·
    promoted then surfaced then used · contract leaks · duplicate declarations ·
    promotion coverage · unconsumed promotions · import-only edges ·
    rewrite rule offered and used

**No conductor action can supply them.** A required capability that no reachable
action satisfies — the same shape as D31 and D40: declared, and unreachable.

And the sharpest instance: **"promoted then surfaced then used" — the exact
measurement this whole frame was built to make — is a required field the machine
does not compute.** I measured it by hand. Every frame that ever declared this
capability has failed it for this reason, and it would read as a frame defect
rather than a harness gap unless someone said so.

## `:guidance-count-exact` — CONFIRMED

Measurement reads `"attempts or closer hops" 0`. The true count of typed
guidance bells is 0: D40 refused the only typed route, and both guidance packets
rode inside `dispatch-solver` openings, which the subtrahend cancels exactly
(3 jobs − 3 openings = 0). D2's repair holds — f9 read −100 and f10 −101.

**But note the hazard this creates**: because D40 forces guidance into the
dispatch channel, the guidance metric is structurally BLIND to guidance that was
actually given. This frame gave two substantive guidance packets and the honest
recorded count is 0. Both statements are true and they must be reported together.

---

## POST-CLOSE — a replayed park resume, and why it was nearly destructive

After the cycle closed I received a park resume for
`invoke-1787160573399-5090-aa4c6b08` (scribe pass 2) carrying the scribe's full
report as if it were new. **I had already consumed that result**: I noticed the
park had been released while the job showed `state: done`, fetched the job
result directly, and acted on it — promoted `e-56ae9c01…`, recorded the promote
lanes, closed the cycle, belled ground control.

**The resume payload carries nothing marking it as a replay.** It is
byte-identical in form to a first delivery. A guide that acted on it naively
would have re-issued `promote-artifact` for a memory that is already `:reviewed`
— which is exactly the operation D62 makes dangerous, on a memory that is
currently good. The failure mode is not a wasted action; it is a poisoned
memory, and the trigger is simply having been efficient enough to poll the job
directly instead of waiting.

Mitigation used: verify state before acting on any resume. Final state confirmed
after the replay, nothing re-run:

    cycle           bound? false, closed at v356
    e-4f6b5d49…  :reviewed  math-strategy/construction-before-estimates
    e-00c94a8d…  :reviewed  math-strategy/proof-architecture
    e-56ae9c01…  :reviewed  math-formalization-CA/ode-gronwall-api

Recommendation: park resumes should carry the park-id and a
delivered-already/ack marker, so a continuation can tell a first delivery from a
replay without reconstructing what it did last turn.

---

## Close ACCEPTED by ground control (claude-2), independently re-verified

claude-2 re-ran the load-bearing checks rather than accepting them: worktree
clean at `9ff4f86`, 0 sorries, `lake env lean` exit 0, `#print axioms
apm_m93j06 → [propext, Classical.choice, Quot.sound]`, and the same
`md5 66dff80d…` on the first 146 lines at base and head. Both axes confirmed.
Recorded as the first genuine solve since f10, and the first ever on a problem
verified sound in advance.

### One number of mine was wrong — corrected above

I reported the solver diff as **+963/−3**; the true figure is **+960/−3**.
`git diff --numstat` gives 960 insertions and 3 deletions; the `--stat` column
renders **963** because it totals changed lines (insertions PLUS deletions), and
I read that column as insertions. claude-2's independent count was right.
Corrected in the diff-audit entry rather than left standing, since the Analyst
reads these as receipts.

Declaration counts reconcile exactly: 12 declarations matching `apm_m93j06` at
base, 40 at head → **28 new**, matching claude-2's count.

### Carried forward by ground control

- **Re-homing `e-4f6b5d49…` to `math-informal/local-to-global` is QUEUED and
  blocked on D62** until promotion is atomic. My refusal to make that trade
  inside the frame was accepted.
- `:malformed-cycle-attempts` accepted as my error; validate-on-record accepted
  as the fix for the harness half.
- **`:measurement-populated` unsatisfiable** taken as the most consequential
  defect in the report.
- Two rules adopted verbatim: aggregate `USED` counts must be filtered against
  `:memory-use/surfaced-ids`; and "315 offers, 15 dispositioned" is a false
  reading because only the 5 leaves were ever shown.

### Frame cost, recorded for the series

    f15-guide   75,234,338      f15-solver  17,659,846      f15-scribe  1,286,232
    TOTAL       94,180,416 tokens  /  960 LOC, 28 decls, 4 conjuncts, :closed

The guide is ~80% of frame cost, consistent with f13: **conducting costs more
than solving.** This is the first frame in the series where that denominator
divides into a non-zero yield.

**FRAME 15 CLOSED AND ACCEPTED.** f15-guide does not persist beyond this frame;
everything that should outlive it is in the three reviewed memories, this log,
and the close report.
