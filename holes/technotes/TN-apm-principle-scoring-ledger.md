# Principle scoring ledger — APM apparatus repair

Companion to `TN-apm-fundamental-repair-program.md`. One row per occasion a
proposed apparatus principle (futon3/library/apparatus/*.flexiarg, status
*proposed*) either caught a real defect or misled. Joe's deal (2026-09-08):
the principles get committed to the library on field evidence from this
repair, not on argument. A principle that misleads is recorded here too —
that is the evidence that makes the ledger worth reading.

Reviewer: claude-9. Author of the repair work: claude-5.

## Scored entries

### 1. evidence-to-disposition-once (P9) — EARNED, on f193

**Occasion.** f193 `live/memory-cascade-operation.edn`, 2026-09-08 02:04:58:
249s of expansion over 313 seeds terminating in
`:error/code :memory-cascade-failed` — a code naming the operation, not the
fault. The principle's violation signature, matched without adjustment.

**What it found.** Not a missing field but an *erasure*. The mechanism was
captured at the throw site and thrown away one frame up: cascade reads run in
futures via `bounded-parallel-map`, `deref` wraps the failure in an
`ExecutionException` whose `ex-data` is nil, and the terminal writer read
`(ex-data t)` directly. Verified in the live JVM rather than assumed:

    (try @(future (throw (ex-info "x" {:error/code :boom :status 503})))
         (catch Throwable t [(class t) (ex-data t)]))
    ;; => [java.util.concurrent.ExecutionException nil]

Consequence beyond the missing code: `status` was always nil too, so the
`:failed-503` branch and `:http/status` field were unreachable on the
parallel path — dead code that read as coverage.

**Score.** Earned. The signature pointed at the record; the record pointed at
the wrap. Repaired in S1/S2.

### 2. success-must-not-resemble-failure (P2) — EARNED, inverted

**Occasion.** The operator strip read
`cascade failed: frame is running WITHOUT served memory` while the attempt-3
packet carried 313 `:accessible-memory-ids` (280 promotion, 33 leaf) — the
same 313 the cascade recorded as `:seed-count`. Memory *was* served; the
expansion over it failed.

**What it found.** The principle is usually applied to a failure that looks
like success. Here it caught the mirror: a degraded success rendered as total
absence, in `voxterm/server.py:2165-2169`, which keys on cascade status alone.
Mine, written on f187 grounds. Scheduled as S3.

**Score.** Earned, and the inversion is the interesting part — the signature
is symmetric and was only ever read in one direction.

### 3. one-authority-per-question (P1) — EARNED, against the reviewer

**Occasion.** claude-9's dispatch quoted the operator strip's rendering as the
specimen's evidence. The strip is a consumer; the authority is the packet and
the ledger. Reading the authority reversed the conclusion (entry 2).

**Signature.** "A consumer read a copy that is not the authority."

**Score.** Earned. Recorded at claude-9's own instruction. A principle that
catches the person wielding it is stronger promotion evidence than one that
only catches the code.

### 4. monitors-measure-the-work (P7) — EARNED, on the test suite

**Occasion.** `conductor-test/observed-cascade-persists-typed-503-failure`
has been green throughout. It throws the transport `ex-info` *directly* into
`run-observed-memory-cascade`. Production never takes that path — every real
cascade read goes through `bounded-parallel-map` first
(`conductor.clj:545`), so every real failure arrives wrapped.

**What it found.** The test measured a path the work does not use. Green for
the wrong reason, which is the principle's signature applied to a monitor made
of tests rather than of watchdogs. The new acceptance test drives real
readers against a real dead socket for exactly this reason.

**Score.** Earned.

### 5. done-is-observed-running (P8) — EARNED, pre-emptively, on ourselves

**Occasion.** S1/S2 were developed and tested in an isolated worktree JVM
(`futon3c-cascade-fidelity`), not loaded into the shared :7070 JVM, because a
conductor swap under a live frame is the same class of act as the futon1b
restart that killed f193's cascade at 02:04:57.

**Score.** Earned before Joe has ruled on the restart-discipline item it
implies (needs-Joe #3). Applying our own proposed rule to ourselves first is
the cheapest possible test of whether it is livable. It was.

## Not yet scored

- default-to-the-cheap-error (P4), every-wait-has-a-deadline (P6, S7),
  replayable-not-precious (P10, blocked on Joe's f193 ruling),
  new-failure-class-is-a-design-defect (P12, scored by the park-class census
  once a wave restarts), the-system-stops-on-schedule (P13),
  pin-moves-with-the-population (P14), loudness-is-conserved (P5),
  model-upstream-and-coupled (P11).

## Open judgment call for review

S1 revives `outcome :failed-503` and `:http/status`, both driven by an HTTP
status code. P9's answer to the consultation says transport codes may never be
an input to *blame*. Read here as: these are mechanism and evidence, not the
apparatus/agent decision (which lives in `posthoc-fault-origin`), so carrying
them is what P9 wants rather than what it forbids. Flagged rather than
decided, since commit `ec97a42b` was faulted for the adjacent mistake.
