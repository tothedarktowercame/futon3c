# Principle scoring ledger — APM apparatus repair

Companion to `TN-apm-fundamental-repair-program.md`. One row per occasion a
proposed apparatus principle (futon3/library/apparatus/*.flexiarg, status
*proposed*) either caught a real defect or misled. Joe's deal (2026-09-08):
the principles get committed to the library on field evidence from this
repair, not on argument. A principle that misleads is recorded here too —
that is the evidence that makes the ledger worth reading.

Reviewer: claude-9. Author of the repair work: claude-5.

**If you are reading this to decide whether to commit the principles:** the
summary table below is the answer, and *What the trial changed* at the end is
what the machine does differently now. Everything between them is the
working. Two of the thirteen principles misled, three had no occasion, and
both facts are on the table rather than in the prose.

## Summary — what each principle was worth

Twenty-five entries across ten repairs (S1–S10 plus the S5 slices), one
scheduled pause, and three loads observed against running code. Twenty-one
score a principle; four record findings no principle covered. "Misled" means
the principle pointed at the wrong thing and the ledger says so; that is the
column that makes the rest credible.

| Principle | Occasions | Earned | Partial | Misled | Strongest single exhibit |
|---|---|---|---|---|---|
| success-must-not-resemble-failure (P2) | 3 | 3 | – | – | A delivered guide turn filed as a fault, and **discarded before persist** (f177, f194) |
| done-is-observed-running (P8) | 4 | 3 | 1 | – | Two repairs were committed, green, and **not loaded**; the JVM said `:not-loaded` when asked |
| one-authority-per-question (P1) | 4 | 3 | 1 | – | Three call sites asked a cancel the same question and all three answered it wrong |
| evidence-to-disposition-once (P9) | 2 | 2 | – | – | The cascade's own error record erased the mechanism that caused it |
| monitors-measure-the-work (P7) | 2 | 2 | – | – | The progress watchdog could not fail; mutation showed it silent under an induced stall |
| every-wait-has-a-deadline (P6) | 2 | – | 2 | – | A wait that looked bounded (attempt count, `:not-before-ms`) with no horizon on the instant itself |
| new-failure-class-is-a-design-defect (P12) | 1 | misled, then earned on the same occasion | | | Told us to mint no new park class for f193; the census held at **44** through the whole program |
| the-system-stops-on-schedule (P13) | 1 | 1 | – | – | The pause was cheaper than any of the three live loads it replaced |
| default-to-the-cheap-error (P4) | 1 | 1 | – | – | Argued successfully *against* a reviewer directive to delete a fallback |
| pin-moves-with-the-population (P14) | 1 | – | – | 1 | Sent me after 87 "path pins"; measurement found 1 real defect in 5 |
| loudness-is-conserved (P5) | 0 | – | – | – | no occasion |
| replayable-not-precious (P10) | 0 | – | – | – | no occasion |
| model-upstream-and-coupled (P11) | 0 | – | – | – | no occasion |

Two findings had **no principle to file them under**, which is the useful
kind of gap:

- **A repair citation must name a checkable defect, not a neighbourhood.**
  f177 was closed as covered by `f4ae8d5a` with the note "watch f188+ for
  recurrence". It recurred once more, on f194, and that recurrence discarded
  an authenticated submission. Two independently adjudicated frames, one
  defect, park census unmoved at 44.
- **A fence's own acceptance must be observation against live data.** P8
  governs repairs and says nothing about the tests that guard them — and a
  fence is exactly the artifact whose failure is silent. Demonstrated twice
  in one hour, the second time by me.

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

### success-must-not-resemble-failure (P2) — earned, and it named the fix

**Occasion.** S5 slice 1, f194. The wrapper collected an authenticated guide
submission, cancelled the job that produced it, and got HTTP 409
`invoke-job-already-terminal`, state `done`. `job-port/cancel!` reports
`:ok` only for HTTP 200, so the collection branch read a job that had
finished *early* as a cancel that had *failed*, returned before `persist-fn`,
and escalated a delivered turn to an apparatus park.

**What it found.** P2 supplied the fix, not just the diagnosis. The question
"is this job still running?" has a success answer that arrives as a 4xx. Once
the disposition is named after what the response establishes
(`:already-terminal`) rather than after its HTTP shape, the branch is obvious.

**Score.** Earned, strongly — the principle's phrasing was the design.

### one-authority-per-question (P1) — earned, with a scope it did not settle

**Occasion.** Same seam. Three call sites (`supersede-unaccepted!`,
`recover-orphan-attempt!`, the collection branch) each ask a cancel result the
same question, each by reading `(:ok ...)`, and each would mis-answer it
identically.

**What it found.** P1 says the question gets one authority, which argues for
converting all three. Evidence argues against doing it blind: the other two
have fired **zero** times campaign-wide, and neither has a live record to pin
a test against. Fixed the one with real occurrences; the other two adopt the
classifier in the enum slice, where the disposition type makes the conversion
mechanical.

**Score.** Earned for locating the shared question. Silent on how far to
carry a repair in one step — the live-pin rule decided that, not P1.

### An adjudication is not a repair (no principle covers this)

**Occasion.** f177 was adjudicated as covered by `f4ae8d5a`, with
"watch f188+ for recurrence". `f4ae8d5a` moved the session-identity capture
ahead of the cancel — a real repair of a *different* fault in the same
handful of lines. It never touched how the cancel's answer is read. The
frame-park record credited it with a region rather than a defect, and the
class recurred on f194.

**Gap.** The library has no principle saying an apparatus-repair citation
must name the defect it closes and be checkable against it. "Same
neighborhood as commit X" is how a fault gets marked handled without being
handled — and the recurrence note in the record shows the author half-knew.
Candidate for a fourteenth principle; not drafted here.

### The f194 park was destroying evidence, not mislabelling it

**Severity note on S5 slice 1.** The collection branch computes the
terminal-collection record, *then* cancels, *then* returns on `:ok false` —
**before `persist-fn`**. So the authenticated submission was not merely filed
under the wrong name; it was **discarded**. On both frames it reached.

This changes what the seam cost. A mislabelled success is a reporting defect
and the evidence survives in the record. This was an evidence-loss defect
wearing a reporting defect's appearance, and the wrapper's own park record is
what made it look like the milder thing.

**Bearing on P2.** "Success must not resemble failure" reads like a legibility
principle. Here the resemblance had teeth: the arm that success fell into was
an early return, so the resemblance itself is what deleted the work.

### P1 x the live-pin rule — resolution by sequencing, not by ranking

**Occasion.** S5 slices 1-2. P1 says the three call sites asking a cancel the
same question get one authority, which argues for converting all three at
once. The live-pin rule says a test pins one live record verbatim — and the
other two sites have **zero** firings campaign-wide, so converting them in
slice 1 would have bought authored-constant tests, the exact failure mode the
live-pin rule exists to prevent.

**What it found.** The two did not have to be traded off. Slice 1 fixed the
site with real occurrences and a live record; slice 2 introduced the closed
disposition type, which made the other two adoptions mechanical and
type-checked rather than speculative. P1 was satisfied one slice later at no
cost to the pin rule.

**Score.** Both earned. The useful entry is the interaction: when a principle
of structure and a principle of evidence disagree about *scope*, they often
disagree only about *order*. Check for a sequencing that satisfies both
before ranking them.

### Singletons reduce to the join — measured

**Occasion.** `frame-park-decisions.edn` carried f177 and f194 as separate
entries in the same class, adjudicated a day apart, each on its own. Both are
one seam, closed by one commit.

**What it found.** The end-to-end note's claim that apparent singletons
collapse once the join is named has an instance here — two independently
adjudicated frames, one defect, census unmoved at 44 — but n=2 is a weak
instance and should be cited as one. Two entries merging is consistent with
the claim and also consistent with coincidence. Recorded because the
temptation was to inflate it: the first version of this entry said "three
frames" and rested on a miscount (below), and a claim about collapsing
singletons is exactly the kind that wants its evidence bigger than it is.

**Score.** Weak support, honestly denominated. Not offered as a load-bearing
exhibit for the promotion.

### The activating finalizer hazard — and what the ledger says about it

**Occasion.** S5 slice 3. The single-finalizer guard treated `"activating"`
as already-terminal, so a job completing before its running transition
landed would return the ledger unchanged: no terminal state, no result, no
delivery, no event.

**The queued-forever link, tested rather than assumed.** The proposal was
that this explains historical accepted-but-never-anything seats. Measured
against the live ledger (3973 jobs, 2026-09-08T11:19Z): **zero jobs in
`activating`**. My first check — counting `activating` in job event logs —
returned 0 and meant nothing, because the activating transition is a bare
`assoc-in` that appends no event. A negative result from an instrument that
cannot produce a positive one.

**So the honest statement:** the hazard is real in code, has no observed
instance, and *this ledger cannot rule it out either*, because a job
stranded in `activating` leaves no event trace. That last part is the
finding worth keeping — the state is invisible to the log, so the diagnostic
that would confirm or refute the link does not exist yet. If a
queued-forever seat recurs, the check is the job's `:state` field directly,
not its events.

### A fence that checks a vocabulary against a vocabulary is not checking anything

**Occasion.** S5 slice 3, then one hour later against itself.

I replaced a conformance test that compared `job-state`'s sets *to each
other* — which is why it stayed green while the producer wrote an
undeclared state. My replacement compared the consumer's sets to the
producer's **declared** sets, and passed while the producer wrote
`"deduped"`, which none of its own predicates declared. The same defect,
one level up, committed by the person who had just named it.

**What it found.** "Check against the producer" is not a sufficient
instruction, because the producer has two faces: what it declares and what
it writes. Only the second is the world. The working fence pins the live
ledger's state census and the set the finalizer is actually called with.

**Score.** The gap is in P8 (done is observed running). P8 governs repairs;
nothing said the same of **fences**. A fence's own acceptance should be
observation against live data, not agreement with a declaration — and a
fence is exactly the artifact whose failure is silent.

**Cost of the miss.** `terminal-invoke-state?` answered false for
`"deduped"`, and the invoke skip-guard uses it to refuse re-running a
finished job. The guard would have re-run a deduped job the queue reached.
I had declined to delete the `"succeeded"` phantom an hour earlier on
exactly this reasoning — the reasoning was right, and I was applying it to
the harmless half of the problem while the harmful half sat undetected in
the same set.

### The S5 loads, observed running (P8 discharged)

**Scheduled pause, 2026-09-08.** Joe authorised a halt rather than three
live swaps. Coordinator `jit-queue:jit-all-open-v3` drained to `:stopped`
at 11:31:52Z with a quiescence witness (epoch 12, ticks 29905, tick-claim
nil), verified in the durable state file and not only in the stop! return.
An f196 student turn dispatched at 11:30:40 was left to finish, which is
what draining means.

**Slice 1** (`bf695656`). Loaded by `(require … :reload)` from master.
Induced with f194's own 409 verbatim against the LOADED code:
`:already-terminal`, terminal-state `"done"`, `:ok true`; a genuine 500
still `:failed`. Then the whole collection path: `:terminal-collected`,
submission **persisted**, persist-fn reached — the early return that
discarded the evidence is gone. f194's decision record flipped from
`:decision/not-done` to `:decision/verified-live`.

**Slice 2** (`98dc7a8a`). Table 8 cells over an 8-cell product, total, no
unreachable arm, no nil fall-through, f194's cell `:delivered-by-submission`
— driven against loaded code over the real state list including `:deduped`
and `:delivered`.

**Slice 3** (`3a0a2373`). `http.clj` reloaded and routes probed before
anything else (`/agents`, `/parked`, `/invoke/jobs` 200; compact 409, not
404). Then measured against the live ledger through loaded code: 3975 jobs,
**zero states undeclared by the producer, zero unclassifiable by the APM
consumer**. A real deduped row, `invoke-1788837954677-14476-727fe1c1`, now
answers `skip-guard-would-skip? true` — the duplicate execution is
prevented, on a real row rather than a literal.

**Not done:** no live `activating` job was constructed. Building one means
racing a real dispatch on a live Agency, which is not worth it for a hazard
with zero observed instances; the predicate and the finalizer guard's
expression were verified directly instead. Stated rather than glossed.

**Resumed** 11:34:34Z, epoch 12→13, ticks advancing (29906→29916 in ~40s),
f196 picked up its in-flight student turn as `:live-job-dispatched` with no
error codes. Park-class census unchanged at 44 across the whole pause.

**Bearing on P8.** Every one of these checks was cheap once the machine was
stopped, and three of them (routes, the ledger census, the real deduped row)
could not have been run safely against a live frame. The principle asks for
observation against running code; the pause is what made the observation
honest rather than approximate.

### the-system-stops-on-schedule (P13) — EARNED, at the pause itself

**Occasion.** The scheduled repair pause, 2026-09-08. Three repairs were
finished and could not honestly be called done, because loading them needed
a stopped machine. Joe authorised a halt; the coordinator drained to
`:stopped` with a quiescence witness in 19 seconds, the in-flight student
turn was left to finish, and the campaign resumed on the next epoch with
that turn intact.

**What it found.** The principle's value here was not the stop, it was
having decided in advance what the stop was *for*. Three separate live
loads had been planned around avoiding a pause; the pause turned out to be
cheaper than any one of them and made checks possible that were otherwise
unavailable (see the loads entry).

**Score.** Earned. Also the cleanest demonstration in this ledger that a
halt is an instrument, not an admission.

### default-to-the-cheap-error (P4) — EARNED, by argument against a directive

**Occasion.** S5 slice 3. The brief said to replace `classify`'s
`:else :unknown` with the closed vocabulary. I kept it.

**What it found.** `:unknown` is the cheap error: callers act on it
(`campaign-reconcile` filters it, the driver raises
`:live-job-state-unclassified`), and it is the correct answer for a vanished
job or a peer running newer code. The hazard was never the fallback — it was
that a state the producer *writes* and the consumer merely *forgot* was
indistinguishable from a genuinely foreign one. The fence closes that gap;
deleting the fallback would have removed a real safety net to satisfy the
letter of an instruction.

**Score.** Earned, and useful precisely because it argued against what the
reviewer had asked for.

## Not yet scored

Three of the thirteen, with no occasion in this repair that tested them:

- **loudness-is-conserved (P5)** — nothing in this program changed how much
  the machine says, only what it says.
- **replayable-not-precious (P10)** — was blocked on the f193 ruling; the
  ruling came (preserve as partial, no re-run), which settled the frame
  without exercising the principle.
- **model-upstream-and-coupled (P11)** — the futon1b failures at 02:30 and
  02:47 are the natural test and remain unexplained, so scoring it would be
  guessing.

A principle unscored after fourteen occasions is not thereby weak; it means
this repair did not touch the kind of failure it is about. That is worth
saying plainly rather than padding the ledger to thirteen rows.

## Four times I measured with the wrong instrument

Worth its own section because it is one mistake, not three, and it is the
mistake most likely to survive review — a negative result looks like
diligence.

1. **The event-log count.** Asked whether any job had ever stranded in
   `activating`, I counted `activating` in job event logs and got zero. That
   transition is a bare `assoc-in` and appends no event, so the count could
   never have been anything but zero.
2. **The `check-ignore` on a directory.** (claude-9's, reported against
   himself.) `data/apm-campaigns` answered "not ignored" while the file
   beneath it was ignored by `data/*`. The instrument was asked about the
   wrong object.
3. **The atom deref.** Reading the live ledger I dereferenced the var, got
   the atom, and keyed into the atom — producing an empty census that looked
   like a clean bill of health. Caught only because 3975 jobs reporting zero
   states was implausible on its face.

4. **The nearest-match regex.** Asked how often the wrapper-reconciliation
   fault fired, I counted occurrences of its error code in a 300KB serialized
   blob and attributed each to the nearest following `:frame/id`. That
   reported "seven times across f177, f178 and f194". The true figure is
   **two**: two distinct job ids, two frames (f177, f194), two adjudications.
   The string repeats because one event is copied into a residual, a decision
   record and a queue entry; f178 never carried this fault at all. The count
   was inflated 3.5x and travelled into a reviewer's draft of a promoted
   pattern before a recount caught it.

**The lesson, stated so it can be checked:** a negative result is only
evidence if the instrument could have produced a positive one, and a *count*
is only evidence if you have checked what the thing being counted is. Three
of these four were about proving absence; the fourth was about proving
abundance, and it is the one that nearly reached Joe.

**Where it got caught:** not by review. claude-9 accepted the number and
built it into the pattern's evidence line; I caught it only when redlining
that line, because the pattern demands checkable claims and I went to check
its own. The gate that worked was the standard the artifact set for itself.

All four were caught before they reached a conclusion Joe would have acted
on, but the margins differed sharply. The third was caught by implausibility
rather than method, and the fourth had already passed a reviewer and been
written into a pattern file. Both are luck, and are recorded as luck.

## What the trial changed

Six things are true of the machine now that were not true this morning.
Five I verified myself; the sixth is cited.

1. **A delivered turn is no longer thrown away.** The wrapper collected an
   authenticated submission, cancelled the job that produced it, got a 409
   because the job had already finished, and returned *before persisting* —
   on both frames it reached. Now persisted; verified against the running JVM
   with f194's own 409.
2. **Thirteen finished jobs can no longer be re-run.** `"deduped"` was a
   real ledger state that no predicate declared, and the invoke skip-guard
   would have re-executed any of them the queue reached. Verified on a real
   row through loaded code.
3. **The progress watchdog can fail.** It was silent under conditions it
   existed to catch. Now commissioned by mutation in both directions — it
   fires on an induced stall and stays quiet on a healthy one.
4. **A wait for the substrate now ends.** `:awaiting-substrate` had an
   attempt count and a resume instant but no horizon on that instant; it is
   bounded at 30 minutes with the deadline published.
5. **The park-class census did not move.** 44 before, 44 after — ten
   repairs, four frames adjudicated, no new failure class invented to
   describe any of them.

6. **Warm substrate reads got about a thousand times faster.** futon1b
   hyperedge queries: 1.286s cold to 1.2ms/0.75ms warm (omitted-param),
   1.032s to 0.82ms (type-only); read permits 2 to 4, so four concurrent slow
   reads now serve together and `/health` stays under 3.2ms where two calls
   previously timed out past a second. *Not my measurement:* it is codex-2's
   work under Joe's direction, reviewed and restart-verified by claude-9 —
   futon1b `796f40d7` (TN-futon1b-cost-profile-2026-09-08.md), `4f456929`,
   `2ef1886b`, plus claude-9's probes against the restarted server at ~11:08Z.
   Cited rather than reproduced, and flagged as such because the rest of this
   document is measurements I took myself.

Item 5 is the result to read first. The others say defects were fixed; that
one says the fixes were repairs rather than renamings.

## Open judgment call for review

S1 revives `outcome :failed-503` and `:http/status`, both driven by an HTTP
status code. P9's answer to the consultation says transport codes may never be
an input to *blame*. Read here as: these are mechanism and evidence, not the
apparatus/agent decision (which lives in `posthoc-fault-origin`), so carrying
them is what P9 wants rather than what it forbids. Flagged rather than
decided, since commit `ec97a42b` was faulted for the adjacent mistake.
