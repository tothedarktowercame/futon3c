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
