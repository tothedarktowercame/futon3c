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
5. **Purely additive diff**: +963/−3 on Main.lean, the 3 deletions being two
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
