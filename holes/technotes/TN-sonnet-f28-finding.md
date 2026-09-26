# TN: F28 / a01A12 — the solver→memory channel is still dead; a second channel covered for it

Author: Claude (Fable 5, session referred to as "sonnet"), 2026-08-24T10:32Z.
Live babysitting observation of campaign `jit-all-open-nontopology-v1`
(the 141-problem run), frame `f28`, problem `a01A12`, prompted by Joe asking
whether the repaired memory-transmission design was actually demonstrating
memory reaching the Student.

Apparatus under review: futon3c `master` at `c105693b`. Campaign directory
`data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f28/`.
Companion to `TN-opus-f27-review.md`, which diagnosed the same class of break
on f27 and built "Fix 1" (guide deposits reaching the student, commit
`4faf7677`). This note is evidence that Fix 1 is live and doing real work —
and evidence that the channel it was built to route *around* is still broken.

## Method

1. Read the Student dispatch prompts for `student-attempt-1` and `-2` (job
   events, `:type "prompt"`) for the literal `:memory-snapshot` map sent to
   each attempt.
2. Read `:receipt/memory-use` off both attempts' ledger entries
   (`ledger.edn`) for `:surfaced-ids` / `:used-ids` / `:queries`.
3. Read the role card (`role-cards/zai-student-v2.md`) to establish what
   `:accessible-memory-ids` actually authorizes and what counts as a
   compliant report.
4. Traced the write side: `memory_snapshot.clj` (`publish!`,
   `candidate-visible?`) and `countdown_control.clj`
   (`publish-promotion!`, `publish-guide-promotion!`) to find what is
   supposed to populate a snapshot.
5. Read all three snapshot files on disk for f28
   (`snapshots/f28-solver-memory.edn`, `f28-guide-1-memory.edn`,
   `f28-guide-2-memory.edn`) and the ledger's `:solver-promotion` receipt.
6. Pulled the actual Agency job transcripts for the Scribe deposit
   (`apm-role-1249dc4f…`) and a subsequent Repair job
   (`apm-role-8e208d3d…`) that diagnosed a live bug in the deposit path.
7. Checked `student-attempt-3`'s dispatch prompt (currently running,
   started `2026-08-24T10:12:37Z`) for its `:accessible-memory-ids`.

Everything below is receipt- and file-read, not inferred from projection
buffer summaries.

## Finding 1 — attempts 1 and 2 had zero memories available, and it was not benign

```
attempt-1 :memory-snapshot :accessible-memory-ids []
attempt-2 :memory-snapshot :accessible-memory-ids []

attempt-1 :receipt/memory-use :surfaced-ids [] :used-ids []
          :queries ["SurjOn principal fourth root cpow slitPlane onto wedge
                     slit range upper half plane"]
attempt-2 :receipt/memory-use :surfaced-ids [] :used-ids [] :queries []
```

Per the role card, `:accessible-memory-ids` is "the complete memory
authority for this attempt… Do not query, read, or use any memory id
outside that list." So attempt 1's single query was necessarily going to
surface nothing — the accessible set was empty before the query was ever
formed. Attempt 2 did not query memory at all, which the role card treats
as a distinct, penalized failure mode ("an equal cost to 'memory surfaced
nothing' with no queries listed").

My first-pass read of this (in conversation, before checking the write
side) was that an empty accessible set for a fresh problem's first pass is
expected — no prior agent has solved `a01A12` before, so there is nothing
yet to promote. **That reading was wrong.** The Solver *had* just closed
real mathematical content on this exact problem, in this exact frame,
before either Student attempt ran (`:promote-solver` phase completes before
`student-attempt-1` begins). The question is not "does prior work exist" —
it plainly does — but "did it reach the snapshot," and the answer is no.

## Finding 2 — root cause: a wire-format bug silently dropped the Scribe's real deposit

The Scribe (`f28-scribe`, job `apm-role-1249dc4f…`) is dispatched at
`:promote-solver` with an explicit contract: return a report containing
"string `:depositor` and non-empty vector `:candidates`" — this is a real
agent turn, not a mechanical extraction. Its own transcript, followed
immediately by a Repair job (`apm-role-8e208d3d…`), shows what happened:

> "Found the real cause: `submitted-report` reads `(:receipt evidence)` and
> parses that string as EDN — my previous payload put the fields directly
> in `evidence` with no `receipt`, so the lanes never reached the
> validator. Repairing now."

`live_promotion.clj`'s deposit-report parser only looks inside a `:receipt`
key; the Scribe's first submission put its fields at the top level of
`evidence`, so the parser saw nothing and the deposit was treated as empty.
The Scribe's actual first-pass content, recovered from its own repair
message, was four real candidates grounded in the Solver's rounds:

```clojure
{:memory-id "e-14c2c205…", :pattern-ids ["math-formalization-CA/complex-arg-of-cpow-root"],
 :source-attempts ["solve-round-1-dispatch-d0456e40" "solve-round-2-dispatch-073236f5" "verify-receipt-666f1ce6"]}
{:memory-id "e-4b95d2fd…", :pattern-ids ["math-formalization-CA/surj-via-oriented-root-preimage"], …}
{:memory-id "e-56018477…", :pattern-ids ["math-formalization-CA/schwarz-disk-automorphism-formula"], …}
{:memory-id "e-925c0ab3…", :pattern-ids ["math-formalization-CA/schwarz-disk-automorphism-formula"], …}
```

These are not placeholder content — they name the specific lemma-level
obstacles the Solver worked through across its checkpointed rounds (the
`cpow` branch-argument fact, the surjectivity-via-oriented-preimage
technique, the Schwarz-rigidity route to the disk-automorphism formula).
Exactly the kind of residual the role card says a memory should carry.

## Finding 3 — the repair did not actually reach the solver-promotion channel

This is the part worth flagging as still open, not closed. After the
Repair job resubmitted correctly (`:receipt` properly wrapped, submission
id `9ff363aa…`), I expected `snapshots/f28-solver-memory.edn` to update.
It did not:

```
snapshots/f28-solver-memory.edn  →  :snapshot/memories []   (unchanged)
```

The ledger carries exactly one `:solver-promotion` receipt
(`receipt-id 33aef703…`, `:receipt/reviewed-memory-ids []`,
`:receipt/snapshot-id 7f296d88…` — the same empty snapshot both Student
attempts 1 and 2 were bound to). No second `:solver-promotion` receipt
exists. Whatever the Repair job actually submitted, it never resulted in a
published, reviewed snapshot on this channel. `:promote-solver` runs once
per frame and the campaign had already advanced past it by the time the
repair landed — there is no visible re-entry point for a late repair to
feed back into that phase's receipt. **The solver→memory channel is still
effectively broken for this frame**, independent of whatever fixed the
Scribe's serialization bug in the code going forward.

## Finding 4 — a second, independent channel is what actually reached the Student

`student-attempt-3` (running now) carries a real, non-empty
`:accessible-memory-ids`. Tracing where those four ids come from:

```
snapshots/f28-guide-1-memory.edn  →  :snapshot/memories []        (after guide-intervention-1)
snapshots/f28-guide-2-memory.edn  →  4 memories, :attachment-status :reviewed  (after guide-intervention-2)
```

`f28-guide-2-memory.edn`'s candidates carry `:depositor "f28-guide"`,
`:reviewer "f28-promotion-proctor"`, `:attachment-status :reviewed`, and
`:source-attempts [1 2]` (referencing Student attempts 1 and 2, not Solver
rounds — a different provenance encoding than the Scribe's repaired
payload, so this looks like the Guide independently re-derived the same
class of pattern from what the Student had already surfaced, not a replay
of the Scribe's fixed submission). Three of the four `:pattern-ids` are
identical in substance to the Scribe's original four
(`complex-arg-of-cpow-root`, `surj-via-oriented-root-preimage`,
`schwarz-disk-automorphism-formula`); the fourth candidate combines two of
those pattern-ids under one memory.

This is `countdown_control.clj`'s `publish-guide-promotion!` — the
promotion-review cycle a Guide intervention runs through
(`live-promotion/drive!`, independent review by `f28-promotion-proctor`,
published as the **union** of the prior snapshot with approved
candidates). This is structurally the same design as "Fix 1 / option A" in
`TN-opus-f27-review.md` (commit `4faf7677`, built in response to f27's
*guide → student* channel being severed by a frozen snapshot binding).
Here on f28 it is not a repair-in-progress — it is running in production,
under the same code, on the very first frame of a new 141-problem
campaign, and it is what actually got a Student attempt a non-empty
memory set for the first time in this campaign's life.

## Net assessment

Joe's read was right: an empty `:accessible-memory-ids` on attempt 1/2 was
not "nothing to promote yet" — it was a live serialization bug eating a
real, content-rich deposit. That specific bug looks fixed going forward
(the Repair job's diagnosis is concrete and its resubmission was correctly
shaped). But the channel it belongs to (`:promote-solver` → solver-memory
snapshot) has not, in this frame, actually produced a non-empty published
snapshot — the fix landed too late for `:promote-solver`'s single pass, and
nothing re-drives that phase. What rescued attempt 3 was a *different*
channel (`Fix 1`'s guide-promotion path) picking up equivalent content
after the fact. That is redundancy working, not the original defect being
closed. A frame with no Guide intervention — e.g. a Student who closes the
proof on attempt 1 — would still get nothing from the Solver's own
promote-solver deposit today, and there would be no second channel to
compensate.

## Finding 5 — attempt 3 is a real, working demonstration of the memory design

`student-attempt-3` finished (job `apm-role-4a112f6d…`, submission
`0847ec06…`, outcome `partial`) while this note was being written. Its
`:receipt/memory-use`:

```clojure
:surfaced-ids ["e-0d0d3806…" "e-98785d73…" "e-b1c4fa0e…" "e-bc26b67e…"]
:used-ids     ["e-0d0d3806…" "e-98785d73…"                "e-bc26b67e…"]
:queries []
```

All four accessible memories were fetched before starting, per the role
card's instruction. Three were used substantively; the fourth
(`e-b1c4fa0e`, the ~700-line Schwarz-rigidity automorphism-classification
route) was assessed and correctly deferred as out of scale for the
remaining budget rather than blindly attempted. The attempt's own account:

> "What closed: the `hrange` sorry that both prior attempts left open. I
> proved `apm_a01A12_conformalMap_mapsTo` and
> `apm_a01A12_conformalMap_surjOn`… File went from 3 sorries to 1; full
> Mathlib compile passes."

This is the sorry neither attempt 1 nor attempt 2 (which had no memory
access) got past. The report also does exactly what the role card asks for
on partial transfer, not just success: it names three specific ways the
memory content did not port cleanly to this workspace's Mathlib
(`v4.29.0-rc8`) — `Complex.cpow_ofReal` does not exist here and had to be
rebuilt from `cpow_def_of_ne_zero`/`exp_add`/`exp_mul_I`; an `exact`-style
application against `Real.rpow` hit a `whnf` unification timeout, fixed by
pre-stating an explicit `have`; the memory's one-shot `nlinarith` finishers
needed splitting into two steps in this environment. That is a precise,
falsifiable account of *how* memory helped and *where* it needed adaptation
— not a vague "memory was useful" claim.

Net: once a memory snapshot actually reaches the Student (via whichever
channel), the design does what it was built to do. The open problem
documented above is entirely on the *supply* side (getting a snapshot
populated at all), not on the Student's use of one once it exists.

## Open at time of writing

- Whether the Scribe/Repair's corrected submission (`9ff363aa…`) is
  recoverable into a real second `:solver-promotion` receipt, or whether
  the promote-solver phase needs an explicit "late deposit" re-entry point,
  is unresolved. Not yet raised with codex-10 as its own item — the
  checkpoint-stall and cgroup-memory findings from earlier tonight were
  bellled live; this one has not been.
- Why `f28-guide-1-memory.edn` came back empty (no deposit after
  guide-intervention-1, one after guide-intervention-2) is not established
  here — plausibly the Guide had less to work with after only one Student
  attempt, but that is a guess, not a checked fact.
