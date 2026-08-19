# f13-guide working notes — m99J06

Cycle `m99J06-a0a723b1aef69731014d0b797f7f8ff0da1a8be419e8f064649dd606e8dbff0f`.
Not frozen material. Written so the analysis survives a pouch eviction.

## Solver dispatch 1

- action-id `f13-guide/dispatch-solver/1`, at version 9 -> 10.
- job `invoke-1787138965830-5047-ae5ec610`, recipient `f13-solver`.
- park `park-c016b9a7-6172-45ec-b363-b6f47a6a0aab`, deadline +3600s.
- **`[dispatch-recall-outcome=completed-with-memories]`** — first non-empty
  recall in the f9..f13 series. Two memories surfaced, both f12 harvest:
  - `e-1b72bb47-1575-4a08-ba3b-2a40735c2b86`
    propagate-local-api-mismatches-to-global-theorem-semantics (rewrite-rule)
  - `e-7c6631c9-caf0-4ee1-a1ce-2c7b7c6b88a6`
    audit-elaborated-regularity-semantics-before-proof-search (technique)
- The packet demands an inhabitation witness as first deliverable, WITHOUT
  supplying the argument below. Deliberate: whether the solver finds it (with
  or without the two surfaced memories) is the measurement.

## HYPOTHESIS (mine, NOT COMPILED): `apm_m99J06_H01Model H` is uninhabited

If this holds, `theorem apm_m99j06` is VACUOUSLY TRUE for every `H`, and the
frame confirms `:problem-closed-on-artifact` while refuting `:problem-solved`
— the f12 pair, by a different mechanism.

Argument. Everything `apm_m99J06_isH01Pair u du` requires constrains `u` only
on `[0,1]`:
- `AbsolutelyContinuousOnInterval u 0 1` is stated over `uIcc 0 1` only
  (Mathlib/MeasureTheory/Function/AbsolutelyContinuous.lean);
- `MemLp u 2 (volume.restrict (Icc 0 1))` sees only `[0,1]`;
- `HasDerivAt u (duRep x) x` for a.e. `x` in `[0,1]` is LOCAL, so for a point
  of `(0,1)` it constrains `u` only on a small neighbourhood inside `(0,1)`;
- `u 0 = 0`, `u 1 = 0`.

So take `u = 0` and `u' = Set.indicator {2} (fun _ => 1)`, both with `du = 0`.
`u'` is identically `0` on a neighbourhood of every point of `[0,1]` (the ball
of radius 1/2 avoids `2`), so both are `isH01Pair _ 0`.

`M.realizes` then gives `w w' : H` with `M.val w = u`, `M.val w' = u'`,
`M.weakDeriv w = M.weakDeriv w' = 0` — and `val w = u` is POINTWISE function
equality on all of `ℝ`.

`M.inner_eq` integrates over `Icc 0 1` only, where `u = u' = 0`, so
`⟪w,w⟫ = ⟪w,w'⟫ = ⟪w',w'⟫ = 0`, hence `‖w - w'‖² = 0`, hence `w = w'`, hence
`M.val w = M.val w'`, hence `0 = u' 2 = 1`. Contradiction.

The defect class: `realizes` pins `val` POINTWISE on all of `ℝ`, while
`inner_eq` measures only on `[0,1]`, so the structure demands an isometry
separate points that its own inner product identifies.

**Corroboration from the file's own header.** The "Statement repairs" note says
the former model "stored weak derivatives as literal functions even though its
inner product observed them only almost everywhere, making the hypothesized
structure inconsistent", and repaired it by moving `weakDeriv` into `Lp`. That
is EXACTLY this defect — and the repair was applied to `weakDeriv` and not to
`val`, which is still a raw `H → ℝ → ℝ`.

Status: HYPOTHESIS. Not compiled by me (card forbids scratch-file evals).
The solver owes a compiled witness in either direction. If it returns (A) with
a genuine model, this note is wrong and that is the better outcome.

If the solver misses it, this is the content to deliver at `:intervene` as an
`:answer` to its reported residual — content is permitted there because it
changes the route.

## Machine findings so far

- **The engine-owned park did not fire.** `park-dispatch` only posts when the
  cycle context carries `:agent`; `/api/alpha/parked` was empty after the
  dispatch, so I parked by hand. Report at close.

## Update, 11:34 — the solver is reproducing the argument unaided

`problems/m99J06/lean/Inhabitation.lean` appeared in the solver's worktree
(untracked, in progress) within ~2 minutes of dispatch. It builds
`test_isH01Pair` / `test_H01Model` as a local copy and is proving

    example {H} [NormedAddCommGroup H] [InnerProductSpace ℝ H] :
      IsEmpty (test_H01Model H)

by exactly the route in the hypothesis above, with the same `spike` witness
(`fun x => if x = 2 then 1 else 0`) — and slightly cleaner: instead of
`w = w'` it shows `⟪w,w⟫ = 0` and `⟪ws,ws⟫ = 0` separately, so `w0 = ws = 0`
and then `M.val 0 = 0` versus `M.val 0 = spike`.

**No guidance was given.** The solver got there from the packet's demand for a
witness alone. The measurement is intact.

### Review point that makes or breaks the witness (mine, for adjudication)

The witness is stated about a COPY (`test_H01Model`), not about
`apm_m99J06_H01Model`. Diffed the copied block against Main.lean lines 32-57
modulo the `apm_m99J06_`/`test_` prefix: **textually identical except one line
break** (`abbrev X_L2 :=` on one line rather than two). So the copy is
faithful and an `IsEmpty` result transfers.

But at review I require the CLOSE in `Main.lean` to be stated against the real
`apm_m99J06_H01Model` — a green `Inhabitation.lean` alongside an unchanged
`sorry` is not a close, and a close routed through the copy is not a proof of
the frozen theorem.

## Corrections to the notes above

- The engine-owned park finding STANDS, but my evidence was sloppy:
  `GET /api/alpha/parked` with no params returns `[]` with
  `more-pending: true`. The real query is `GET /api/alpha/parked?agent=f13-guide`,
  which shows exactly ONE park — mine (`park-c016b9a7-…`, mode `within-turn`).
  So: no engine park, my hand park is alive.
- Job telemetry reads `execution {:executed? false :tool-events 0}` while the
  solver is demonstrably writing files. The live signal is the agent record's
  `invoke-activity-at` (11:34:08, 21s quiet), not the job's execution counters.
  Do not read `executed? false` as wedged.

## RETRIEVAL FINDING — the OR-join fired, the query did not

From the offered receipt `e-4b862952-6668-4db9-9aea-e5b65cab1a7a` (authoritative;
the job event log truncates the prompt at ~1511 chars, which is why my first
report said "two memories" — **five were surfaced**, not two, and not merely
eligible).

The term extractor found the right vocabulary. `:term-sources`:

- problem-md: finding, strong, **sobolev**, uniqueness, **weakly**, dense,
  sufficient, weak, limit, equivalent, **subspace**, interval, converges,
  dimensional
- proof-outline-md: equality, **galerkin**, apm_m99j06_model_ext, combinations,
  **finite-dimensional**, gives, identities, **inner_eq**,
  representative-level, **riesz**
- stdin-packet: clause, lake, report, **vacuous**, verbatim, **apm_m99j06**,
  **apm_m99j06_h01model**, blocked

The query actually issued was

    finding OR equality OR clause OR strong

i.e. the first term of each of the three sources plus the second term of the
first. Mechanism, read from source, not inferred:

1. `dispatch_with_recall.clj:552` round-robins the three source term-lists.
2. `default-query-term-limit` is **4** (line 21), applied at line 557.
3. Round-robin position 5 is `galerkin`. The cap cuts at 4.

So every distinctive term — galerkin, sobolev, riesz, inner_eq,
apm_m99j06_h01model — is one slot or more past the cap. `text-keywords`
(line 320) sorts each source rarest-first *by problem-corpus IDF*, and against a
corpus of mathematics problems the rare words are the PROSE words. The source
comment at line 424 already names this: "problem-corpus IDF selects artifact
vocabulary and INVERTS relevance — e-retrieval-miss-a01A12-slit-wedge".

**Correcting my orientation on the anchor band.** The briefing said
`anchor-df-band [3 150]` "is not doing selection". True — but not for the reason
given. The band lives in `query-anchor-term-memory-df`, which runs ONLY when
`--anchor-source memory-df`. The default is `:problem-idf` (line 562) and the
receipt confirms `:anchor-source :problem-idf`. The band was never on the path.
Same for the df-scoping fix `6bfe5808`: it only affects the memory-df path.

**And the wave-2 rung as built would not have repaired this dispatch.**
`--anchor-source memory-df` changes `required-term` only — the term that gets a
ranking boost (`rank-with-anchor-boost`, line 689). The query terms are
unchanged. Flipping it yields the same `finding OR equality OR clause OR strong`.
The damage is in the query; the built fix targets the anchor.

**What actually delivered the memories.** `:memory-use/surfacing-via`:
- `e-1b72bb47…` (the one the solver used first) — `:content-match`
- the other four — `:pattern`

So lexical recall contributed exactly ONE memory, and it ranked **27th of 30**
in the lexical seed (score -9.88); 27 of the 30 seed hits were
`:evidence-type :coordination` (chat turns, invoke receipts), not memories. The
cascade contributed four. The read path returned the right thing largely by
luck of the four generic words appearing in it.

STORE-MODE: this is an OBSERVATION, not a change. I have not touched retrieval
and will not this frame.

## SOLVER RESULT — job done 11:36:43, verdict (B) UNINHABITED

Commit `25186f291b25c98a0c5e6e2280a951ab107be277` on
`exp/frame-13-m99J06-solver`. Solver's own report: close is **VACUOUS**, no
sorry/admit/added axiom, worktree clean, no residuals.

Per-memory attestation returned unprompted and complete over all five:
- USED `e-1b72bb47…` — propagated the whole-function/interval-semantics
  mismatch through `realizes` and `inner_eq` to model emptiness.
- USED `e-7c6631c9…` — audited the model's inhabitation BEFORE PDE/Galerkin
  proof search.
- IGNORED the three `e-codexpilot-…` (typeclass preconditions; ContDiff — not
  in this statement; pair-refutation-with-repair — contract forbade repair).

### My review (gate, not rubber stamp)

- `git diff a92ffb6..HEAD`: +77/-1, one file. The five frozen definitions and
  the `apm_m99j06` signature are **byte-identical** to the pin (checked by
  extracting the block and comparing with the inserted lemma removed:
  `FROZEN BLOCK IDENTICAL: True`).
- `grep sorry|admit|^axiom|sorryAx` over Main.lean: **NONE**.
- The witness is stated against the REAL `apm_m99J06_H01Model`
  (`theorem apm_m99J06_H01Model_isEmpty`, Main.lean:83-152), not a `test_` copy —
  the copy file was deleted. The close is
  `letI : IsEmpty (apm_m99J06_H01Model H) := …; exact isEmptyElim M`.
  My stated review bar is met.
- Independent compile + axiom audit: bg job `bg-1787139511615-1`.

## VERIFIED — I re-ran the acceptance checks myself (bg-1787139511615-1)

    --- ACCEPTANCE COMPILE (verbatim command) ---
    problems/m99J06/lean/Main.lean:69:6: warning: unused variable `hddu`
    problems/m99J06/lean/Main.lean:158:17: warning: unused variable `hf`
    ACCEPTANCE_EXIT=0
    --- AXIOM AUDIT (guide copy, repo untouched) ---
    'apm_m99j06' depends on axioms: [propext, Classical.choice, Quot.sound]
    'apm_m99J06_H01Model_isEmpty' depends on axioms: [propext, Classical.choice, Quot.sound]
    AXIOM_EXIT=0

Zero `sorry` diagnostics. Frozen block byte-identical to the pin. Close is
`letI : IsEmpty (apm_m99J06_H01Model H) := apm_m99J06_H01Model_isEmpty;
exact isEmptyElim M` — against the REAL structure. Every claim in the solver's
report reproduced.

## RETRACTION — the engine-owned park DID fire

My earlier finding was WRONG and I am retracting it in full.

Evidence: `v10.edn`, written 11:29:27 (dispatch time, before my hand park at
11:30:18), already carries `:park/id "park-c016b9a7-6172-45ec-b363-b6f47a6a0aab"`
on the `:dispatch-solver` step, and `:park/error` is ABSENT. The engine created
that park. My hand POST at 11:30:18 with the same (agent, session, awaiting)
was an **idempotent upsert** — it returned the SAME id and replaced the
engine's 2700s deadline with my 3600s one (park deadline reads 12:30:18 =
11:30:18 + 3600, not 12:14:25 = 11:29:25 + 2700).

Confirmed live: the `:dispatch-scribe` action auto-parked as
`park-6f011667-9bf4-43bd-9724-0ba29d633f64` with no action from me.

What actually happened: D57 (`GET /api/alpha/parked` returns nothing without
`?agent=`) hid the engine's park, I read the empty list as "not parked", and
reported a defect that does not exist. D57 is real; "the conductor does not
park its dispatches" is not. Worth keeping as a lesson: a masking defect
manufactures a plausible second defect downstream of it.

Live consequence worth knowing: a hand park by an agent whose engine already
parked SILENTLY REPLACES the engine's deadline and wake payload. Benign here
(longer deadline, richer checklist); not benign if someone shortens one.

## Conductor actions can exceed a 2-minute client timeout

`record-solver-attempt` took ~3.5 min wall-clock; my `curl` timed out at 2m and
the action completed server-side regardless (v10 -> v11). The `action-id`
dedupe (`:conductor-action-duplicate`) makes a retry SAFE, so the correct
response to a client timeout is poll-then-retry, never assume failure.

## Cycle progress

- v11 `:intervene` — solver attempt recorded (`attempt/f13/solver-0`, commit
  25186f29, axiom-clean, 0 residual sorries, 1 closer hop).
- v13 `:promote-solver` — deposited
  `check-pointwise-realizes-against-the-metric-support-before-proving`
  (statusless, patternless). NOTE: authored by `f13-guide`, so **I cannot
  promote it** (D41). It is on the shelf for a future frame's reviewer.
- v14 — scribe dispatched, job `invoke-1787140125766-5056-613d10d4`,
  auto-parked `park-6f011667-…`.

Promotion targets when the scribe reports: pattern-ids proven to work in f12 are
`math-strategy/structural-obstruction-as-theorem` (the emptiness IS the theorem
— exact fit) and `math-formalization/notation-semantics-traps` (pointwise vs
a.e.). Reviewer must be `f13-guide`; depositor must be `f13-scribe`.

## The promote -> recall loop closed across frames

The two memories the solver USED (`e-1b72bb47…`, `e-7c6631c9…`) are exactly two
of f12's promoted artifacts, from f12's own `:promo/artifact-id` records. So
f12 promoted them, f13's dispatch recall surfaced them, and f13's solver
attested using them to reach its result. That is the full loop, end to end,
for the first time in the series.

## DEFECT — `promote-memory-attachment!` is NOT ATOMIC and poisons on failure

Sequence, all verified:

1. Scribe deposited three memories, all authored `f13-scribe`, all
   `:evidence/type :memory`, all with NO status and NO patterns on the entry.
   I checked each entry before promoting.
2. `promote-artifact` x3, reviewer `f13-guide` (≠ depositor), verdicts
   `approve`, pattern-ids `math-formalization/notation-semantics-traps` and
   `math-strategy/structural-obstruction-as-theorem` (both proven in f12).
   **All three refused.**
3. The refusal receipts say only `:error/message "Tool execution failed"` —
   **D43 exactly**. I re-issued one with a fresh action-id and read the full
   HTTP body, which DOES carry the finding:
   `failure: promotion-attachment-not-statusless, attachment-status: proposed`.
4. Live hyperedge read: all three now carry `:attachment-status :proposed`
   **and the pattern-id I supplied**. No `memory-attachment-review` evidence
   exists in the store for any of them (only f12's, from 08:05).

So the attach at `memory_lifecycle.clj:333-340` posts pattern + `:proposed`
BEFORE the review evidence is written (346-370) and the review applied. When
the later step fails, the edge is left in exactly the state lines 321 and 328
refuse. **The promotion is then permanently unretryable through that path, and
one failed promotion poisons the memory.** The two operator-visible symptoms —
a harmless pre-flight refusal and a destructive half-applied one — are
indistinguishable in the receipt.

The three edges were NOT born `:proposed`: each carries the specific pattern-id
I passed, so the attach step is what wrote the status. My own guide deposit,
never promoted, is untouched.

Not diagnosed: WHY the post-attach step failed. No review evidence was written,
so it failed at or before that append. f12 promoted four memories through this
same path successfully at 08:05 today, so something between f12 and f13
changed — `:append-retry-live` (f1322302, c035df5c, 99176882) is the declared
departure that landed in that window and is the first place to look.

### What I did about it
Dispatched the scribe a second time (`dispatch-scribe/2`, v19) for ONE fresh
virgin memory, told it the mechanism plainly, and asked it to correct me if
re-recording is the wrong move. I will promote that one with the full HTTP
response captured. If it fails identically I stop burning memories and record
`:reviewed-attachment-gained` as refuted with the mechanism; if it succeeds I
ask for the remaining two.

Frame state so far: THREE proposed attachments, ZERO reviewed.

## CORRECTION — the promotion gate is NOT broken; it lost a race

`e-62615b79-8ae2-470b-8b28-465dd87f50c7` (fresh scribe deposit, virgin edge,
verified statusless/patternless by the scribe AND by me before the call)
**promoted cleanly on the first try**:

    attachment-status : :reviewed
    patterns          : ["math-strategy/structural-obstruction-as-theorem"]
    reviewer          : f13-guide
    witness-status    : :independently-witnessed
    review evidence   : e-1044dc37-8ed8-4c37-9549-171448c9f6b7
                        (:review/event :memory-attachment-review,
                         :review/verdict :approve, provenance cycle = this one)

So **`:reviewed-attachment-gained` is CONFIRMED** in exactly the qualified form
the prediction names: a GUIDE-reviewed attachment of a scribe-authored,
statusless, patternless deposit. Not independent review — as predicted.

What actually distinguishes the four calls:
- promotions 1,2,3 ran BACK TO BACK in one shell loop; all three failed after
  the attach;
- promotion 4 ran ALONE, several minutes later; it succeeded end to end.

`review-attachment!`'s own docstring states a read-after-write postcondition —
it "refuses success until that read observes the new review version". Leading
hypothesis: bunched promotions lose that race against the substrate's hyperedge
query cache, and losing it leaves the edge attached-but-unreviewed, which lines
321/328 then refuse permanently.

**This is a hypothesis, not a proof.** Evidence for: 3/3 bunched failed, 1/1
spaced succeeded, and the documented postcondition is exactly a read-after-write
check. Not ruled out: something else about the first batch. Test in progress —
scribe dispatched a third time (`dispatch-scribe/3`, v21) to re-record the other
two rules as fresh memories; I will promote them ONE AT A TIME WITH A GAP. If
both succeed spaced, that is evidence for the race and I will report it with
that caveat.

What stands regardless of the cause:
- the promotion is **non-atomic** — attach is posted before review is applied;
- a failure after the attach **poisons the memory permanently** for that path;
- the refusal receipt says only "Tool execution failed" (**D43**), so the
  operator cannot distinguish a harmless pre-flight refusal from a destructive
  half-applied one. That is the part that turns a retryable blip into lost work.

I retract "the promotion gate is broken" from my previous report. It is not.
