# Proposal: measure memory-access mode (spoonfed / open-search / both), deferred

From: Claude (Fable 5, "sonnet"), 2026-08-24T12:52Z, arising from live
babysitting of `jit-all-open-nontopology-v1` (f28/f29) and discussion with
Joe. **Status: deferred by Joe's own call — "this gets pretty complicated
for a system that just barely works." Revisit after ≈10 more frames
complete**, once there's baseline throughput/stability data to design
against rather than guessing. This note exists so the idea and its known
confound aren't lost between now and then.

Evidentiary basis: `TN-sonnet-f28-finding.md` and `TN-sonnet-F29-finding.md`
(same lab, `holes/technotes/`).

## The observation that prompted this

Two live findings tonight, both grounded in the actual code and job
transcripts, not speculation:

1. **A real full-text search system exists in this codebase and is unused
   by the APM pipeline.** `peripheral/memory_recall.clj`'s
   `propose-patterns-by-query` (backed by `substrate/evidence-text-search`,
   real relevance scoring, a fallback pass) is called only from
   `dispatch_with_recall.clj` and `peripheral/real_backend.clj`. Neither
   `countdown_control.clj` nor `live_promotion.clj` — the two namespaces
   driving every promotion in this campaign — reference it at all.
2. **The Student has no search tool, full stop.** Checked against a real
   job transcript (f29 `student-attempt-1`, 49 tool calls): the roster is
   `run_shell`, `edit_file`, `write_file`, `read_file`, `memory_read` (×3,
   one per pre-authorized id), `run_readonly`. No query/search tool
   anywhere. The role card's "search the store with the vocabulary of your
   obstacle" is something the Student narrates into a self-reported
   `:queries` string, not a capability it is actually given.

So today's design is pure "spoonfeed": the promotion pipeline pre-selects
an exact, reviewed set of memory ids and the Student may only read those,
by id. Nothing in the loop lets either the Scribe (at deposit time, to
check for existing related memories before minting a new one) or the
Student (at solve time, to chase a specific term) actually search.

## The proposed experiment

Since the connective tissue (`propose-patterns-by-query`) already exists
and works elsewhere, wiring it in is a plumbing job, not new research
infrastructure. Once wired, it's cheap to compare access modes across a
frame's 3 Student attempts:

- **spoonfed** — today's design: fixed `:accessible-memory-ids`, no search.
- **open search** — a live query tool (`propose-patterns-by-query` or
  equivalent), no pre-selected snapshot restriction.
- **both** — the spoonfed set plus the search tool.

## The confound Joe flagged, and it's not hypothetical

If the three conditions are fixed to attempt-ordinals 1/2/3 in that order
every frame, the comparison is broken, because attempt-ordinal *already*
carries an independent advantage: `scribe-v2.md` states outright that
*"Student attempts are first-class input… the student's account of what
did not work… is the arc and trajectory lanes' raw material,"* and this is
not just policy — both frames observed tonight show it happening:

- **f28**: attempts 1-2 got zero accessible memories (a separate bug, since
  fixed); `guide-intervention-2` then deposited 4 memories explicitly
  tagged `:source-attempts [1 2]` — mined directly from attempts 1 and 2's
  own failure accounts — and attempt 3 (which received them) closed the
  sorry the first two left open.
- **f29**: `guide-intervention-1` (run after attempt 1) deposited a memory
  that explicitly *corrects* a mistaken plan (a Gauss-Lucas route) attempt
  1 proposed. Attempt 2 inherits that correction regardless of which
  access-mode condition it's assigned.

So attempt 3 is never a clean read of "condition = both" — it always also
carries whatever got mined from rounds 1-2's struggle, on top of its
assigned access mode. Fixing condition-to-ordinal in a constant order makes
"attempt 3 did best" collinear with "attempt 3 is attempt 3," which are two
different claims that a fixed-order design cannot separate.

## Two ways to break the confound, not yet chosen between

1. **Randomize condition-to-ordinal mapping per frame.** Free in
   frame-count (still 3 conditions per problem, no extra frames), but needs
   enough frames accumulated before a regression can separate the ordinal
   effect from the condition effect — and the ordinal-mining effect itself
   becomes a second thing worth reporting, not just noise to discard.
2. **One condition per whole frame**, compare across frames. No
   within-frame confound at all, but costs 3x the frames for equivalent
   power, and mixes in the huge problem-difficulty variance already
   observed (f28's `solve` phase: 1h19m; f29's: 22m) rather than
   controlling for it.

Given the queue is running ~140 more problems regardless of this decision,
(1) is probably the better fit once it's time to build this — it costs
nothing in frame-count and turns the confound into a second measurable
quantity. Not decided; Joe's call when this is revisited.

## What's independently worth doing regardless of the experiment

Two things this investigation surfaced are real defects on their own,
separate from whether/when the access-mode experiment gets built:

- **Pattern-id namespace fragmentation** (full detail in
  `TN-sonnet-F29-finding.md`, Finding 3): four different prefixes observed
  across two frames tonight (canonical `math/…`, f28's
  `math-formalization-CA/…`, f29-scribe's `math-formalization/…`,
  f29-guide's `math-formalization-CV/…`), none matching, including within
  the same frame. Everything that matches pattern-ids does exact string/set
  equality — no canonicalization anywhere. This alone would suppress
  cross-frame reuse even before any FTS wiring, and is cheap to fix
  (constrain Scribe dispatch to the existing `math/*` taxonomy, or a
  normalization pass) independent of this proposal.
- **Wiring `propose-patterns-by-query` into the Scribe's deposit step**
  (not the Student side) is lower-risk and valuable on its own even without
  running the access-mode experiment: it directly implements what
  `scribe.md` already asks for ("we already have this… do not create a new
  memory") but currently has no mechanical support for.

Neither of these needs to wait for the ≈10-frame checkpoint; both are
scoped, bounded fixes that don't touch the experimental-design question
above.

## Companion observation (added 2026-08-24T18:50Z): does guide-intervention actually help?

A second, cheaper-to-answer question surfaced while watching f30 live, and
it uses exactly the same passively-logged data this proposal already
depends on — no new instrumentation, just more frames to look at the
pattern across.

f30's `student-attempt-2` (job `apm-role-82aafdcb…`) closed the theorem
cleanly: 0 errors, 0 sorries, correct axioms, submitted and accepted at
17:23:05. The cycle ran on regardless (fixed 3-attempt schedule, confirmed
in `generated_contract.clj`'s `expected-transitions` — a linear,
pre-registered phase order; nothing short-circuits it on early success).
`guide-intervention-2` (job `apm-role-fdcbd2e9…`) then explicitly recorded
knowing this — *"Attempt 2 was a success: the Student closed
`apm_a01j06`. I recompiled its source blob myself — 0 sorries…"* — and
made a genuine, well-reasoned attempt to improve attempt-3: it found a
guide-1 memory had been rejected at review for miscategorization, costing
attempt-2 "~10 compile-fix cycles" it shouldn't have needed, deposited a
corrected replacement, and recorded 4 new reusable failure classes.

Measured outcome: `student-attempt-3` took **45m44s**, longer than
attempt-2's **37m11s**, for the same result (0 sorries, same axioms, no
evidence of a cleaner or shorter proof). So this round's intervention,
despite being well-targeted and well-reasoned, didn't produce a measurable
speed or quality improvement — on this one data point.

One data point proves nothing either way; it's exactly the kind of claim
that needs ≈10 frames of the same measurement to say anything real about.
The check is cheap once there are enough frames: for every
`guide-intervention-N` that precedes a `student-attempt-(N+1)` following
an *already-successful* `student-attempt-N`, compare attempt durations and
outcomes before/after the intervention. If interventions after a success
never help, that's worth knowing (maybe stop early on success after all,
contra the "guide improves round 3" justification this note opened with).
If they sometimes help substantially, that's worth knowing too (maybe the
issue is intervention quality, not the always-run-3 design). Either answer
informs the access-mode experiment above, since both are really the same
underlying question: what, if anything, does an intervention between
attempts actually buy.

## Revisit trigger

Come back to this proposal after ≈10 more frames of
`jit-all-open-nontopology-v1` have completed — by then there should be
real throughput/stability numbers (via the babysitter's per-phase/per-frame
timing) to judge whether the system is stable enough to add experimental
complexity, and possibly evidence of whether the namespace-fragmentation
fix alone (if done in the meantime) already improves outcomes without
needing the full access-mode comparison. The guide-intervention-efficacy
question above should be checked at the same time — it's free to answer
once there are enough frames, using data already being logged.
