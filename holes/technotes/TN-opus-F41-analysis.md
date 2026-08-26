# F41 analysis — two machine defects, and a measurement that was running blind

Claude (claude-12), 2026-08-26, written live while watching the frame.
Campaign `jit-all-open-nontopology-v1`, frame f41, problem **a97J06**.
Status: attempts 1-3 are complete and analysed below; **certification is
still pending**, so the closing tally and scribe-reduce verdicts are not yet
recorded.

Companion to `TN-fable-F32-F35-bank-review.md`. The running working record is
`TN-spec-delta-cannot-judge-and-trace-checker.md`; this note cites it rather
than repeating it.

## Why f41 was worth watching closely

f41 draws **a97J06**, the sibling of f40's **a97J05**. f40 was the campaign's
first fully clean frame: its attempt 2 closed a97J05 at 0 sorries using four
memories, all four fingerprinted against the base blob, and the frame
produced five approvals after a run of 0-for-31. Those five approved
memories — about power-series coefficient decay, naming
`hasFPowerSeriesAt_iff`, `Metric.ball_mem_nhds`,
`FormalMultilinearSeries.coeff_ofScalars` — went onto the shelf.

So f41 is the best test the campaign has had of **tier-A condition 3**,
cross-problem transfer: a different problem in the same domain, with
approved, transferable memories available, written under the v2.3 cards, and
naming APIs — which matters, because the fingerprint standard can adjudicate
an API name and cannot adjudicate a procedural memory (spec-delta §11).

Verified before attempt 1: all four of f40's used memories were on f41's
shelf (39 accessible, later 41).

## Defect 1 — the campaign halted on a correct rejection

**Fixed and verified.** Full detail in spec-delta §16.

At 08:21:01Z, with a97J06 already solved by the solver at 0 sorries in 3
rounds, the regulator went `:failed` on
`:promotion-review-projection-invalid` and stayed down. A 141-problem
campaign stopped.

The cause was a good review. f41's promotion proctor **rejected** a candidate
because its pattern attachment was incoherent — "that pattern explicitly
concerns a theorem's sharp failure regime rather than a proof method failing
on one problem" — which is `promotion-proctor-v3.md` rule 5 carried out
exactly ("an incoherent attachment is never `:approve`"). It named the
pattern it judged correct instead.

`memory_lifecycle.clj:429` then required the review's pattern set to match
the edge's for every verdict except `:reassign`, and threw. But four lines
above, `attachment-status` is `:proposed` for `:reject`, and the edge's
patterns are rewritten only on `:reassign` — so on the reject path those
pattern ids are never applied to anything. **The invariant that halted the
campaign governs a value that path does not use.**

A non-fatal route for the same condition already existed
(`apply-existing-attachment-review!`, `memory_lifecycle.clj:246`, returns a
finding); `promotion_review_store.clj:196` bypassed it and called the
throwing function directly.

Fixed in `912e2b30` (Clojure) and `e1bbe29560` / `0e9c1991fe` (Lean model).
The Lean pair is worth noting: the first commit modelled approval as list
equality, and the second corrected it — the Clojure `exact-patterns?`
compares count and set, so it accepts a reordering, and the model had stated
a constraint *stronger* than the machine enforces. The model was corrected
toward the implementation rather than the reverse.

After the fix, promote-solver completed `{reassign 3, reject 1}`, the reject
resting at `:attachment-status :proposed` — unpromoted, which is the safe
state — and the frame proceeded.

### The general rule this produced

> Where a defect has a natural blast radius, the machine must not exceed it.

An invalid review is a defect in one candidate. Its correct consequence is
that the candidate is not promoted, not that 141 problems stop. Joe's
requirement that unreviewable material be impossible is met by leaving the
candidate `:proposed` and recording the finding; it never required halting
the queue, and halting is a large part of why the machine "can hardly run".

**Corrected after codex-10's review** (spec-delta §16.1). That statement
merges two cases with opposite correct responses. A `:reject` or `:reassign`
naming different patterns is a *completed valid judgement* — never a defect,
proceeds, nothing to repair. A failure of the projection apparatus itself is
not a candidate-level outcome at all, and dispositioning it as one is
void-and-advance scoped to a candidate, which Joe rejected after F32. The
rule needs both halves: the machine must not exceed a defect's natural blast
radius, and must not fall short of it either. The shipped
`publishing-review?` gate inherits the conflation, and the pending Lean
contract types `projectionInvalid` as a complete nonpublishing disposition;
codex-10 is holding that contract until this is settled, which I endorse.

## Defect 2 — the student's work is invisible to the machine

**Found, not yet fixed.** Full detail in spec-delta §17.

f41 attempts 1 and 2 both recorded `:outcome "partial"` with
`:candidate/head` equal to `:base-revision`, `Main.lean` untouched at 182
lines and 5 sorries. Through the receipts, the student sat for 34 and 25
minutes and wrote no Lean, twice.

It did not. The workspace holds `problems/a97J06/lean/Scratch.lean`, 8110
bytes of real Lean, seven lemmas, two of which the student reports compile
sorry-free. The file is ignored:

    $ git check-ignore -v problems/a97J06/lean/Scratch.lean
    .gitignore:13:Scratch*.lean	problems/a97J06/lean/Scratch.lean

An ignored file cannot be committed, so the candidate head cannot move, so
the attempt scores as barren. **The student role card does not mention
scratch files at all.** Proving leaves in a scratch file and assembling
afterwards is an ordinary way to write Lean; the name it reached for is the
one apm-lean excludes.

### The memory evidence this was hiding

Identifiers named by the memories the student used, counted in the scratch
work and differenced against the base file as §3.1 requires:

| identifier | Scratch.lean | base Main.lean |
|---|---|---|
| `integral_exp_mul_complex_Ioi` | 2 | 0 |
| `Real.fourier_real_eq_integral_exp_smul` | 1 | 0 |
| `intervalIntegral.integral_Iic_add_Ioi` | 1 | 0 |

Novel to the base, so fingerprinted. The student's own account says more than
the counts: it checked every lemma the memories named against the pinned
Mathlib and reported **"Zero phantom names"**; it credited `e-bd971ae9` with
predicting the exact cast-normalization friction it then hit; and it
attributed the three remaining sorries to "mechanical shape-matching I did
not finish within the 30-minute budget" — no mathematical obstruction.

The scratch file is preserved outside the workspace so attempt 3 cannot reset
it away.

### Why this is the costly one

The machine could not distinguish **"the student did nothing"** from **"the
student did the work where I do not look."** Those are opposite findings. One
says the memory system is not helping; the other says it is helping and the
instrument is blind. Two attempts were filed under the first while being the
second.

The witness standard is right to read committed artifacts — prose
attribution is design signal, never outcome data (retrieval-whitepaper-v3
§3.1). But a standard is only as good as its reach, and where a student can
work outside that reach, every USE claim in the attempt reads as unwitnessed
and the campaign under-measures the thing it exists to measure.

## Condition 3 is still open

This must not be over-read, and the temptation is real, because the
fingerprint table above looks like the result.

**Every memory involved in that table is same-problem** — f41's own scribe
and guide deposits about a97J06. Fully witnessed they would still not bear on
condition 3, which requires a fingerprinted use of a memory mined from a
*different* problem.

The tally so far:

| attempt | used | cross-problem uses | witnessed? |
|---|---|---|---|
| a1 | 4 / 39 | 1 — `e-73ac922d`, f37-guide, mined from a96A08 | no artifact committed |
| a2 | 5 / 41 | 0 — three f41-scribe, two f41-guide, all a97J06 | scratch only, uncommitted |
| a3 | 4 / 42 | 1 — `e-3411c0c2`, f33-guide, mined from a94A07 | committed, but **0 of 16 identifiers present** |

**Condition 3 is not met in f41.** Attempt 3 is the one that finally produced
a committed artifact (`:candidate/head` a35590e6, outcome `not-closed`), so
it is the only attempt where the cross-problem question could be answered at
all — and the answer is no. `e-3411c0c2`
(`entire-injective-affine-route-second-pass-spellings`, about entire
injective affine maps) names sixteen identifiers, and not one of them appears
in the artifact. That is consultation, not transfer: the student read a
memory from a94A07, and nothing from it reached the proof. It is the same
shape as f40's `e-e9f9c621`, where 0 of 38 identifiers were present.

The identifier that *is* novel in the artifact —
`integral_exp_mul_complex_Ioi`, in the proved `half_line_exp` — comes from
f41's own scribe deposits. Fingerprinted, and irrelevant to condition 3.

**f40's four approved memories were used zero times across all three
attempts**, having been on the shelf throughout. That is the plainest fact
f41 produced, and the one most worth explaining: the shelf offered exactly
the kind of material the design predicts should transfer between siblings,
and the student never reached for it.

### The student routed around the gitignore by itself

Attempt 3's commit adds `Reports/f41-student-attempt-3-scratch.lean` — 23
lines, a path the ignore rule does not match. So the student found the
workaround unaided, which is evidence the §17 fix should name the mechanism
rather than prescribe a workflow. But it preserved 23 lines out of a
184-line `Scratch.lean`, and `Main.lean` never moved from 182 lines and 5
sorries. Partial self-mitigation, not a solution.

**None of f40's four memories has been used in either attempt**, though all
four have been on the shelf throughout. Attempt 1's one genuine cross-problem
use names real Mathlib identifiers and so is adjudicable in principle, but
that attempt committed nothing, so there is nothing to difference against the
base — consultation, not load-bearing use (spec-delta §12).

What f41 establishes so far is that **the measurement was running blind**,
not that transfer has been demonstrated.

## What to change, in order

Nothing here is dispatched yet: role cards pin by git blob, so editing one
mid-frame churns a running manifest. These wait for f41 to certify.

1. **Student card: say where work is collected from**, and that
   `Scratch*.lean` is gitignored and will be discarded. Name the mechanism
   rather than forbidding the habit — a student told this can keep its
   workflow and still land the result. (A student that knows it must land
   work in `Main.lean` to be seen may also budget differently, which is the
   point.)
2. **Attempt receipt: record uncommitted or ignored Lean in the workspace**,
   with a compile check, so the condition is reported rather than silently
   identical to an idle turn.
3. **Reconsider the 30-minute budget** against what attempt 2 actually did.
   It reached a state it described as two mechanical rewrites from closing.
   That is a different failure from being stuck, and only the receipt fix
   above makes the difference visible frame to frame.

## Open items carried forward

- `:findings` is returned by `promotion_review_store/persist!` and never read
  by `live_promotion.clj:539` — latent, fires only when a projection
  genuinely fails.
- `:challenge` was added to `attachment-verdicts` during the defect-1 fix.
  Consistent with the lifecycle and tested, but new behaviour introduced
  during a stop-the-line repair, so it is a new failure surface.
- The Clojure enforcement gap: unresolved review passes advanced f39 and f40.
- Two unaudited agent-minted-id sites: guide deposit, terminal submission.

## What I checked (so this is auditable)

- Read `912e2b30` in full; ran `memory-lifecycle-review-test` and
  `promotion-review-store-test` (19 tests, 109 assertions, 0 failures) and
  `promotion-pipeline-test` (9 tests, 29 assertions); clj-kondo clean on all
  three changed files.
- Built the Lean model myself with the named targets only —
  `lake build DarkTower.APMCycleMachine DarkTower.APMCycleContractEmitter
  DarkTower.APMCampaignTraceChecker DarkTower.APMQualification` — 764 jobs,
  success, `Mathlib.olean` still absent, so no Mathlib compile.
- Confirmed the regulator resumed by **ticks** (29225 → 29617 → 30451), not
  by `:regulator/status`, which read `:running` for nine minutes while the
  loop was not ticking.
- Read the proctor's submission and the attempt receipts directly from
  `data/apm-role-submissions/` and the frame's `live/` directory; resolved
  memory provenance against the substrate on :7073.
- Verified the shelf contents and the scratch file by hand in the student
  workspace, including `git check-ignore -v` and
  `git status --porcelain --ignored`.

Instrument fix made along the way: `24356587` stops `apm-frame-pulse.py`
rendering an in-flight attempt as `0/N used`, which is indistinguishable from
the zero-uptake result it was built to catch.
