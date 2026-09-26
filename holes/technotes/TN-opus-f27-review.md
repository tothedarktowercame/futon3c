# TN: F27 / m94A03 — the math is a replication, the experiment is a null

Author: Claude (Opus 5), 2026-08-23T22:37Z. Read-only review of campaign
`jit-m94A03-retry-v3-f27`, prompted by Joe: "I've spent most of the day just
trying to get it to run. What I don't know is whether all the effort is
leading to anything."

Apparatus under review: futon3c `master` at `a7a5e395` (campaign manifest pins
`11a2f940`); problem `apm-lean` base revision `9b573944`, blob `10570eb8`.

Companion to `TN-fable-F25-review.md` / `TN-fable-F27-review.md`, which cover
*why runs need repairs*. This note covers a different question: **given that
f27 ran, is the output worth anything?** Short answer: the mathematics is
worth more than the projection buffer says, and the experimental arm measured
nothing. Those are separable, and only one of them is a problem.

## Method — what was actually checked, not receipt-trusted

1. Read all 12 phase files in `live/`, plus `manifest.edn`, `preparation.edn`,
   `snapshots/f27-solver-memory.edn`, `analyst/state.edn`, and the projection
   buffer (`*problem: f27-m94A03*`, 25 events).
2. Read both diffs: solver `9b573944 → 3d8107d5` (+186/−3) and the student's
   uncommitted working-tree diff (+229/−66).
3. Audited both diffs for statement tampering (below).
4. **Recompiled both files independently** with `lake env lean` plus an
   appended `#print axioms apm_m94a03`. Not a receipt re-read — a fresh
   elaboration.
5. Checked the formal claim against the mathematics by hand.
6. Probed the futon1b substrate on `:7073` for the three deposited memories.

## Finding 1 — the result is an independent replication (upgrade, not a caveat)

m94A03 was closed **twice, by two agents, on two materially different routes**:

| | route to the dimension-two logarithm membership | weak-gradient vehicle |
|---|---|---|
| solver (`3d8107d5`) | `log ∈ L¹(B²)` via `Real.abs_log_mul_self_lt`, then `MemLp.mono_exponent` | `apm_m94a03_logNormGradient_weak_of_memLp` |
| student (attempt 1) | `log ∈ L²(B²)` via the same bound applied to `√r` | `apm_m94a03_logNormGradient_weak_ball` |

Both compile clean under independent re-elaboration:

```
exit 0 · 0 errors · 0 sorry-warnings · 22 linter warnings
'apm_m94a03' depends on axioms: [propext, Classical.choice, Quot.sound]
```

The formal claim is also the *right* claim. `f(x) = log‖x‖` on `B(0,1) ⊂ ℝⁿ`
has `|∇f| = ‖x‖⁻¹`, and `∫₀¹ r^(n−1)·r^(−2) dr` converges iff `n > 2`. So
`¬H¹(2) ∧ H¹(3) ∧ H¹(4)` is correct and matches the harvested informal
solution.

**Statement integrity: clean in both.** Every deletion in the student diff is
comment prose or a `sorry` — it edits no pre-existing declaration at all. The
solver diff makes exactly one non-comment edit to existing code: it weakens
`apm_m94a03_integral_logNormRegularized_tendsto`'s hypothesis from `MemLp … 2`
to `MemLp … 1`. That *strengthens* the helper (weaker hypothesis, same
conclusion) and is sound. `apm_m94a03_InH1UnitBall`,
`apm_m94a03_IsWeakGradientOn`, and the theorem statement are untouched in both.

An independent replication with a divergent proof route is the strongest
evidence a single frame can produce. **Nothing in the projection buffer tells
you it happened.** The buffer reports hashes, phase, and "Solver rounds
completed: 2 / 50".

## Finding 2 — the student's proof was destroyed, and had no home to be saved to

The student worktree `/home/joe/code/apm-frames/f27-m94A03-student` was reset
to base at **22:34 today**, mid-review. HEAD had never moved off `9b573944`;
the proof lived only as an uncommitted working-tree modification. After the
reset: `git status` clean, 1049 lines, 2 sorries.

The campaign directory does not archive role source. Grepping the whole
campaign for `logNormGradient_weak_ball` returns exactly one hit — the *prose
name* inside `live/student-attempt-1.edn`'s `:new-lemmas`. The source itself
was nowhere.

By 22:45 the worktree had been retired outright (`apm-frames/f27-m94A03-student`
no longer exists). It survives only because this review had compiled a copy.
Recovered to:

```
data/apm-campaigns/jit-m94A03-retry-v3/jit-m94A03-retry-v3-f27/recovered/
  student-attempt-1-Main.lean   (1212 lines, 0 sorries — the destroyed proof)
  solver-3d8107d5-Main.lean
  student-lean-output.txt / solver-lean-output.txt
```

Note this directory is gitignored (`.gitignore:15 data/*`), so it is durable
against worktree cleanup but not against machine loss. **The single most
valuable artifact f27 produced came within one review of vanishing with no
trace but a lemma name in a receipt.**

## Finding 3 — the experimental arm has three open circuits

The frame's headline reads "3/3 student attempts succeeded, 2 guide
interventions." All three links that would carry an experimental signal are
severed, and each break is visible in the data.

**(a) Solver → memory, severed at promotion.** All 3 solver candidates
rejected `pattern-attachment-missing` by `f27-promotion-proctor`. The
proctor's own reasons concede the substance: *"The fact is not already in the
pinned base and is findable from the residual vocabulary, but no proposed
pattern or pattern file is bound to this candidate."* Rejected on schema, not
content. Consequence: `snapshots/f27-solver-memory.edn` has
`:snapshot/memories []`.

**(b) Guide → student, severed by a frozen snapshot.** All three student
attempts carry the *identical* `snapshot-id`/`snapshot-digest`
`e54c994e5603…`, pinned at the solver-promotion receipt `af784fbe`. The guide
deposited after attempt 1 (`e-c0e8a22c`) and after attempt 2 (`e-d1d58caf`);
neither ever entered the student's accessible set. Three for three:

```
:accessible-memory-ids []  :surfaced-ids []  :used-ids []  :queries []
```

The deposits themselves are fine — all three memories are durable and readable
at `GET :7073/api/alpha/hyperedge/hx-mem-…`, verified during this review. The
break is entirely on the snapshot/retrieval side.

Compounding it: the guide role card `claude-guide-v2.1.md` describes the live
`futon3c.apm.conductor` binding system as the only effectful route, which this
campaign does not use. The guide found `!bindings` empty system-wide, correctly
diagnosed the mismatch, and fell back to writing straight to the substrate.
It never had a channel to the student to begin with.

**(c) Attempts 2 and 3 are not attempts.** The worktree is never reset
*between* attempts and the student's work is never committed, so attempts 2
and 3 opened `Main.lean`, found attempt 1's own finished proof, and verified
it. The student says so plainly in its own failure account:

> "I expected to find an open residual to attack; instead I found a completed
> proof. What I actually did: verified rather than re-derived. … If the
> measurement wanted a from-scratch attempt, this attempt does not supply one."

**Net: n = 1 real student attempt, 0 interventions with a causal path, 2
tautological re-verifications.** There is no variance for the memory treatment
to explain and no channel through which it could have acted. This is a
plumbing null, not a scientific one — which is good news, because plumbing is
fixable and a real null would not be.

## Finding 4 — solve round 2 was a format artifact, not work (already fixed on master)

Rounds 1 and 2 of `:frame-solve` carry the **same** `final-head 3d8107d5`. No
additional proof work happened. Round 1 was rejected
`:live-proof-terminal-invalid / [:axioms-not-permitted]` while reporting
exactly the three standard axioms as a parsed vector:

```clojure
:axioms ["propext" "Classical.choice" "Quot.sound"]     ; round 1 — REJECTED
:axioms "'apm_m94a03' depends on axioms: [propext, Classical.choice, Quot.sound]"  ; round 2 — accepted
```

The validator at collection time accepted only the raw `#print axioms`
string form. **Correction after checking history:** master commit `2439b3c6`
(21:04:01 — the same minute round 1 was collected) added the vector-form
normalization, with test `json-vector-axioms-normalize-to-permitted-symbols`.
The serving JVM was running pre-fix code when round 1 landed. No further
change needed; "2 / 50" still overstates f27's work by one round.

## Finding 5 — nothing is merged, and the stale metadata caused the degeneracy

`apm-lean` master still carries the 236-line, 3-sorry `problems/m94A03/lean/Main.lean`
and a `status.json` asserting `sorry_count_total: 3` with the 2026-08-19
`closer_hop 3` obstruction unresolved. The closed 1232-line proof exists only
on `exp/countdown-f27-m94A03-solver`.

That stale `status.json` is what dispatched the student at an already-closed
residual — the student diagnosed it correctly (Main.lean mtime 22:04 vs other
bundle files 18:42) and the scribe logged it, but it is filed as
"procedural." It is not procedural: it is the proximate cause of two of the
three attempts being void.

## What the apparatus got right

Worth recording, because it is unusual and it is why this review was cheap:

- **The receipts are honest.** The scribe's `trajectory` lane self-reports
  `ran-empty`; the guide's close-frame audit names all three promotion
  rejections and marks attempt 3 "verification-only"; the student volunteers
  that its own attempt does not constitute a measurement. Nothing was
  laundered into a success narrative. That is the property that makes the
  ledger worth keeping.
- **The close-frame audit did independent verification**, not receipt-reading:
  it git-logged the final-head and grepped for `sorry` in the solver
  workspace. Correct instinct, right layer.

## Fixes — status as of 2026-08-24T00:05Z

Done directly (Joe: "not by handoff") on branch `fix/f27-review`, worktree
`/home/joe/code/futon3c-opus-f27`, commit `409569e4`. Tested in that
worktree's own process; nothing loaded into the :6768 JVM.

| # | fix | status |
|---|---|---|
| 4 | reset the Student worktree to base before each original fresh attempt (`workspace-lifecycle/reset-to-base!`, called from `live-learning-phases/prepare-student-workspace!`; repairs keep the tree) | **done** `409569e4` |
| 2 | archive the Student's problem file, named by git blob, beside the phase state before the receipt is minted; receipt carries `:receipt/source`; the missing-observation receipt archives too | **done** `409569e4` |
| 3 | promotion deposit gate rejects any candidate with empty `:pattern-ids` (`:candidate-patterns-missing`) with one bounded schema repair, and the deposit prompt states the library rule | **done** `409569e4` |
| 5 | accept the parsed-vector axioms form | **already on master** `2439b3c6` |
| 6 | merge `exp/countdown-f27-m94A03-solver` into `apm-lean` master; refresh `status.json` | **open** — `apm-lean` checkout is on someone's `repair/m97A06-energy-regularity` branch; not touched |
| 1 | guide deposits reach the Student (option A below, Joe's call 2026-08-23) | **done** `4faf7677` |

Gates on `409569e4`: clj-kondo 0 errors / 0 warnings; `check-parens` OK;
105 tests / 455 assertions across the affected and adjacent namespaces
(`promotion-pipeline`, `live-promotion`, `workspace-lifecycle`,
`workspace-lifecycle-policy`, `live-learning-phases`, `live-proof-phases`,
`countdown-control`, `learning-loop-dry-run`, `live-job-driver`), 0 failures.

Behavioural note for anyone resuming an older campaign on this code: a
persisted Student request without `:base-revision` now fails closed at
activation (`:student-workspace-base-unknown`) instead of silently skipping
the reset. Re-prepare the frame; do not patch the state file.

Second commit `4faf7677` (fix 1): 144 tests / 605 assertions across the
same namespaces plus `frame-cycle-handlers`, `live-job-driver`,
`queued-frame-adapter`, `memory-snapshot`, `frame-cycle-contract-v2`,
`generated-contract`. 3 failures, all in
`bank-handler-rejects-a-different-frames-verify-receipt`, which **fails
identically on master** with master's own `frame_cycle_handlers.clj` and
test file (verified by stashing both and re-running) — pre-existing, not
introduced here, and not touched.

### Fix 1 — what was built (option A)

- Store-mode Guide report may carry `:candidates`
  (`memory-id`, `content-digest`, non-empty `pattern-ids`, `source-attempts`),
  gated at the terminal by `promotion-pipeline/validate-guide-deposit`;
  candidates in harness-mode are refused (`:guide-candidates-outside-store-mode`).
- Before the Guide receipt exists, the candidates go to the promotion Proctor
  (`live-promotion/drive!` gains a `:review-pending` entry; state at
  `live/guide-intervention-N-review.edn`) and the approvals are published as
  the **union** of the prior reviewed snapshot at
  `snapshots/<frame>-guide-<N>-memory.edn` — every prior memory re-validated
  and re-checked against the substrate. The Guide receipt then carries
  `:receipt/snapshot-id/-digest/-path/-reviewed-memory-ids/-promotion-reviews`.
- `live-job-driver` lets a receipt provider defer certification behind a
  further job (`:status :awaiting-terminal`).
- `frame-cycle-handlers/latest-snapshot-receipt`: Student attempt k binds to
  the most recent Guide union, else the Solver promotion; the handler check,
  `build-request`, and `countdown-control`'s snapshot verification agree.
  Binding is still exact and content-addressed; review is still independent;
  the Lean-generated memory policy is untouched.
- The projection shows the reviewer's job while a deposit is under review.
- **Guide card v2.2 (DRAFT)** — `role-cards/claude-guide-v2.2.md` — adds the
  `:candidates` output and says plainly that under the campaign machine the
  deposit is substrate-write + `:candidates`, not a conductor binding.
  `queued-frame-adapter` pins v2.2 for JIT campaigns; the per-frame one-off
  manifests (`f21/f22/f23-one-off-manifest-v1.edn`) still pin v2.1 by blob
  and are Joe's to re-pin.
- Not changed: the contract's global-invariant label
  `:student-snapshot-equals-promoted-snapshot` now reads as "equals the
  latest *reviewed* snapshot"; the EDN label is documentation, not code.

### Fix 1 — the design question as it was put (kept for the record)

The intended design is in the guide card (`claude-guide-v2.1.md`): *"In
store-mode you may deposit memories between attempts … approved memories
join the student's eligible set (witnessed union with the open snapshot)."*
The implementation has no union: `build-request` binds every Student attempt
to the promote-solver receipt's snapshot, `frame-cycle-handlers` enforces
`:frame-cycle-student-memory-snapshot-mismatch` against that same receipt,
and the Lean-generated memory policy pins `:exact-snapshot-binding true` and
`:independent-review true`. A guide deposit therefore needs an **independent
review between guide-intervention-k and student-attempt-k+1** before it can
enter a snapshot at all — there is no such stage, and the guide-intervention
receipt is minted before any review could run.

Two ways to close it:

**A (recommended).** In store-mode, the guide-intervention phase becomes a
promotion: guide job → typed report carrying `:candidates`
(`memory-id`, `content-digest`, non-empty `pattern-ids`, `source-attempts`)
→ `promotion-proctor` independent review (reuse `live-promotion/drive!` from
its `:independent-review` stage) → publish the **union** of the prior
snapshot with the approved candidates → mint the guide receipt with
`:receipt/snapshot-id/-digest/-path/-reviewed-memory-ids`. `build-request`
binds student-attempt-k+1 to the *latest* snapshot receipt (guide-k if it
published one, else promote-solver); the handler check accepts the same.
Binding stays exact and content-addressed, review stays independent, so the
generated memory policy is unchanged. Cost: one multi-stage driver in
`live-learning-phases/run-live!` for the guide kind, ~40 lines in
`build-request`/handlers, tests, and a **guide card v2.2** (output contract
gains `:candidates`) — the card is frozen by blob, so this is a regime
boundary and needs a new manifest pin.

**B.** No new stage: admit into the next attempt's snapshot only guide
deposits that are *already* independently reviewed in the substrate
(`memory-snapshot/candidate-visible?`), and record the excluded ids with
reasons in the Student request. Honest, cheap, and in practice still empty —
the guide cannot dispatch a reviewer from inside the campaign machine — so it
converts the silent null into an attributed one and nothing more.

A closes the loop; B only labels it. A changes what the Student can see
mid-frame, which is the experimental variable itself, so it is Joe's call.

## Open at time of writing

- The analyst post-close obligation is pending (`analyst/pending`,
  close-receipt `50435c9f…`, tenure 1/2).
- The projection buffer is one event stale: it shows 25 events ending at
  `scribe-reduce` and `waiting-for-terminal-result / guide`, while
  `live/close-frame.edn` is already `:live-job-certified`.
