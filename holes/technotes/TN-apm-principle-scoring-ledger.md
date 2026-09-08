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

### 6. new-failure-class-is-a-design-defect (P12) — MISLED US, then earned

**Occasion.** needs-Joe #3, as claude-9 and claude-5 proposed it: a rule that
no shared JVM may be restarted while a durable operation is in flight, gated
on an in-flight check. Joe rejected it and inverted it: "futon1b should be
durable enough. Work that's getting sent in there should get queued if it's
not available, and then sent, and then processed."

**What we got wrong.** We proposed a catch layer made of operator discipline
— a new rule a human must remember, whose failure mode is a human forgetting.
P12 says a new failure class is a design defect, and we had just spent the
night applying that to code while proposing its opposite for ourselves. The
operator did the one-layer-down elimination we were supposed to do: the
obligation is client-side durability, not restart etiquette. Became S8.

**Score.** The principle earned; its authors did not apply it. Recorded
because a ledger that only lists successes is not evidence of anything.

**Corollary, still open.** Joe kept the quiet-time requirement for the
*futon3c* JVM and said quiet times are increasingly hard to find — he named
it an open problem with no answer. The same elimination applies one level up:
if a quiet time must be hunted, manufacture it instead. Sketch (design note
first, nothing built unbriefed): the coordinator exposes a bounded quiescent
window at frame boundaries, so reloads land on a schedule rather than on
luck. That is P13 applied to reloads mechanically.

### 7. every-wait-has-a-deadline (P6) — partial, observed at the halt

**Occasion.** The clean halt at 02:54:34 surfaced the coordinator sitting in
`:status :awaiting-substrate` with a live `:coordinator/delayed-retry`:
`:attempt 1`, `:max-attempts 3`, history `:hyperedge-unreachable` (02:30:59)
then `:memory-snapshot-visibility-not-obtained` (02:47:03).

**What it found.** The wait carried `:not-before-ms` and an attempt count,
so it looked bounded. S7 later showed it was not: nothing bounded how far
ahead that instant could be, and the watchdog reads the mover's own field as
the deadline.

**CORRECTION (entry 12 is the scored version).** I first wrote here that the
02:47 failure was "a visibility lag, not death", inferring it from the error
code `:memory-snapshot-visibility-not-obtained` and from three healthy probes
I ran at 03:00. The durable transport certificate says otherwise:
`:history [{:attempt 0, :operation :write, :acquired-outcome :timeout}]` and
the coordinator's own entry records `:transport/acquired-outcome :unavailable`
for attempt 1. Both attempts were transport failures against a futon1b that
was up. The code names the STEP the failure happened in, not the fault.

**Score.** Partial. Sharpens S7/S8 rather than settling them: the fault is not
a missing deadline but a retry budget that treats transient visibility lag and
substrate death identically — which is only distinguishable now that S1/S2
make errors carry their mechanism.

### 8. done-is-observed-running (P8) — EARNED LIVE, in my own repo

**Occasion.** The S3 strip fix was committed, and the running service still
served the false text. It had been up since 01:00:48 and never saw the file.
Caught by querying the live strip after committing rather than assuming the
commit was the deployment.

**Score.** Earned, minutes after S6 was written up as the comparator for
exactly this gap. The declared/loaded/running split is not a futon3c
peculiarity; it is anywhere a process outlives an edit.

### 9. one-authority-per-question (P1) — EARNED AGAIN, against its user

**Occasion.** My first S3 acceptance test re-implemented the strip's branch
instead of calling `apm_status()`. That is the same signature — a consumer
reading a copy rather than the authority — that I had logged against the
strip an hour earlier, committed by the person logging it, while fixing it.

**Score.** Earned. A principle that catches both of us in one night on the
same signature is discriminating rather than decorative.

### 10. pin-moves-with-the-population (P14) — MISLED, mildly

**Occasion.** 87 absolute-path references across 33 test files looked like
the reason branch work cannot see a clean gate. Measuring said otherwise: of
202 failing assertions, 146 sit in files with no absolute path at all, and
only one of five assertion-shaped pins was a defect.

**Restatement.** Counting occurrences of a pattern is not measuring the
fault. Only running the thing and attributing the failures is.

### 11. monitors-measure-the-work (P7) — EARNED, and it needed mutation

**Occasion.** S4. The watchdog had twenty tests and was blind through the
f193 night regardless. Both reasons were structural: the tests handed
`watchdog-observation` its inputs as literals, so the file-reading arity
production uses was never touched; and every one checked that the alarm
FIRES, so nothing measured the healthy case.

**What the principle demanded that a green suite did not.** Commissioning by
induced failure, both directions, then mutation to prove the commissioning
itself discriminates:

| mutation | caught by |
|---|---|
| role turn not declared an external wait (the shipped 0c0868ed regression) | both role-turn cases |
| cursor frozen | healthy-progress case |
| cursor advances every observation | **the stall case stops firing** |

The third is the finding. False progress is precisely what kept the watchdog
quiet all night, and its signature is an alarm that stays silent — invisible
to every test that only checks firing. Confirmed against the loaded code in
:7070, not only in the test JVM.

**Score.** Earned, and it is the strongest entry so far, because the
principle predicted a specific missing test and that test then caught a
specific historical failure.

### 12. evidence-to-disposition-once (P9) — EARNED AGAIN, against me, on the same signature I had just repaired

**Occasion.** I reported to the reviewer, and wrote into this ledger and the
plan doc, that f193's 02:47 substrate failure was an indexing-visibility lag
rather than a dead substrate. My whole basis was the error code
`:memory-snapshot-visibility-not-obtained` plus three healthy probes run
thirteen minutes later against a different query.

**What the authority said.** The durable transport certificate
(`live/transport-certificates/1788835623826-1-…edn`, emitted 02:47:03):
`:history [{:attempt 0, :operation :write, :acquired-outcome :timeout,
:evidence :not-obtained}]`, and the coordinator records
`:transport/acquired-outcome :unavailable` for attempt 1. Timeout, then
unreachable. Not a lag.

**Why it fooled me.** `:memory-snapshot-visibility-not-obtained` names the
*step* the failure occurred in — the visibility check — not what failed in it.
That is character-for-character the signature I had spent the night removing
from `:memory-cascade-failed`, and I read the new one the same wrong way four
hours later, having written the fix myself.

**Score.** Earned, and it is the entry that most justifies the trial. A
principle worth promoting has to catch the person who already knows it.

### 13. every-wait-has-a-deadline (P6) — restated by S7

**Occasion.** The `:awaiting-substrate` wait had a deadline and was still
unbounded: nothing constrained how far ahead `:not-before-ms` could be, and
the watchdog reads that same field AS the deadline, so a far-future wake is a
deadline that can never be exceeded.

**Restatement earned.** *A deadline the waiting party can set arbitrarily far
ahead is not a bound.* Same family as S4's flicker mutation — the monitor
fooled by the very field the thing it monitors controls.

**Corollary from the deviation.** The reviewer's spec asked for the
already-past deadline to be caught too. It should not be: after a long stop
every pending retry is overdue and refusing them turns a restart into a park
storm, while a past wake's worst case is an immediate retry. *The dangerous
direction of an unchecked bound is rarely symmetric; bound only the direction
that hurts.*

### 14. success-must-not-resemble-failure (P2) — EARNED on a vocabulary member nothing can produce

**Occasion.** S8. `:visibility-lag` is a declared member of
`observation-outcomes`, is handled in `needs-retry?` and `evidence-compatible?`,
and is checked for in `live_promotion.clj`. **No production path emits it.**
The only classifier can return `:success`, `:timeout` or `:unavailable`.

**What it found.** The taxonomy declares a distinction the machine cannot
make, so code that branches on it reads as coverage and is dead. The
retry ladder appears to treat lag differently from death; it cannot.

**Score.** Earned. Cousin of S5's closed-enum problem from the other side:
S5 is an enum that quietly reopens, this is an enum with an arm nothing
reaches.

## Not yet scored

- default-to-the-cheap-error (P4),
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
