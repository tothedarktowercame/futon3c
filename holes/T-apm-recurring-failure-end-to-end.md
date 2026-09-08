# Why jit-all-open-v3 does not converge: the recurring failures end to end

Written 2026-09-08 00:1x, with the campaign deliberately halted at Joe's
instruction ("up on blocks until we've thought through the errors that keep
recurring properly").  Halt is clean: `stop!` returned `:status :stopped`,
`enabled? false`, ticks frozen at 24341, no tick claim.  This is not a fault
stop and not a watchdog halt.

## What the record actually says

126 decided frame parks across the campaign (`frame-park-decisions.edn`).

* **111 of 126 (88%) carry `:decision/apparatus-repair-required? true`.**
* **124 of 126 are `:decision/frame-voided? false`** (2 nil, 0 true).

Nothing was thrown away, and almost nothing was the mathematics.  The campaign
is not failing to do proofs.  It is failing to *record* that it did them, and
failing to keep running while it does.

Class distribution is not a flat tail.  It is two events plus a tail:

| group | count | what it is |
|---|---|---|
| `:preflight-blocked-by-declared-provider-usage-limit` | 62 (49%) | ONE event |
| `:student-terminal-session-id-erased` | 13 (10%) | wrapper cancellation |
| ~40 further classes | ~51 | one or two occurrences each |

**The 62 are a single incident.**  All 62 are dated 2026-09-06, and 59 of them
fall in one hour (14:00Z), on consecutive frames from f114.  A provider quota
was exhausted and the campaign kept dispatching frames into a provider that was
refusing, parking each one, for an hour.  It did not stop.

## The through-line

Every remedy in this system converts a loud failure into a quieter one, and
nothing restores the loudness.

1. The quota event burned 62 frames fast and visibly.  The remedy (d6beeec0)
   reclassified that terminal as bounded `:awaiting-substrate` so it would wait
   instead of consuming the queue.  Correct in itself.  It converted a
   fast-burn failure into a **silent wait**.
2. Role-turn faults that used to void frames were converted to parks that
   preserve evidence and stay re-enterable (`role-terminal-repair-park`, Joe
   2026-09-06, restating the post-F32 rejection of void-and-advance).  Correct
   in itself.  It converted lost work into **accumulated parked work**.
3. Findings that used to escape the rescue chain were given rescue routes
   (f28523b6), and misattributed apparatus faults stopped charging roles
   (2feb07e1).  Both correct.  Both make failures **survivable**.

Each step is good engineering.  The cumulative effect is a system that
converges on *not dying* rather than on *working*, because every failure mode
ends in a state the campaign can sit in indefinitely.

## Why nobody sees it

`watchdog-observation` (`src/futon3c/apm/durable_coordinator.clj:475`) builds
the semantic-progress cursor from the coordinator's regulator state, which
contains no frame fields:

```clojure
:frame-id     (or (:frame-id state) (:frame/id state) ...)
:phase        (:phase state)
:attempt-ordinal (or (:attempt-ordinal state) (:submission/attempt state))
:active-job-id   (or (:job-id intent) (:retry/id delayed-retry))
```

`coordinator.edn` has only `:regulator/*` and `:coordinator/*` keys, so every
frame field resolves to `nil`.  The live cursor confirms it:

```clojure
{:frame-id nil, :phase nil, :attempt-ordinal nil,
 :last-committed-event-id nil,
 :active-job-id "jit-tick-5e07a189..."}
```

The one populated field is the coordinator's own tick id, which changes every
tick **by construction**.  So the progress clock resets every tick and the
watchdog can never fire, however long a frame is dead.  The queue state holds
the truth the whole time (`[:active :frame :frame/id]` = f193); the watchdog
never looks there.

So the three properties compose into the failure Joe hit tonight: failures end
in indefinitely-sittable states; the stall detector is blind; and the operator
strip reports `state: ok` with no alert.  The campaign can be dead and green.

## The current instance, f193/m00A02

* memory-assert transport timeouts to futon1b, 7 occurrences 22:43-23:00:58Z,
  `org.httpkit.client.TimeoutException: idle timeout: 30000ms` (timeout is
  hardcoded at `peripheral/memory_write.clj:245,287`).  Reads measured 1.3-1.6s
  while the campaign was paused, so that is the idle floor, not the loaded one.
* Retries exhausted 3/3.  Frame's `live/` last written 23:03:28Z.
* All three f193 seats idle, no running jobs, since 23:00:12Z at the latest.
* Coordinator kept ticking, persisting a fresh `:jit-problem-queue/tick` intent
  each tick and reporting `:regulator/last-result {:status :intent-persisted}`
  forever.  It survived a `resume!` at 23:12Z.
* Strip reported `state: ok`, `alert: None`, `last_progress_s: 7` throughout.

The only surviving evidence was one hour-old error string in `phase_detail`,
which reads as a recurring error and is in fact a tombstone.

## What to fix, at the seam rather than the consumer

**A. Make the two-authority join canonical.**  Two authorities answer "did the
role deliver": the Agency invoke ledger (did the process finish) and the
typed-submission store (did the role deliver its contract).  Established in
`holes/T-typed-submission-wrapper-cancellation-evidence.md`.  Every bug fixed
tonight is a place that joined them differently:

* `live_job_driver.clj:1149-1160` synthesizes `:state :done` from a collected
  submission -- one implementation of the join;
* `inferred-repair-origin` did not consult the collection at all until
  2feb07e1 -- a second site that needed it;
* `live_learning_phases.clj:449-463` gates the session-id finding on
  `(= :done (:state job))` -- a third;
* `terminal-collection-record` stores the raw pre-cancel `:terminal-state` -- a
  fourth view;
* the watchdog cursor looks at neither -- a fifth.

One function should answer "what is the authoritative disposition of this role
turn", and every consumer should call it.  The ~40 singleton park classes are
the signature of that function not existing.

**B. Make the watchdog see frames.**  Read frame identity from queue state, and
never use the coordinator's own tick id as the activity token -- that field
guarantees false progress.  This is what would have surfaced f193 at 23:05
instead of 00:05.

**C. Give indefinite waits a deadline.**  `:awaiting-substrate` currently has no
upper bound.  A wait that cannot expire is indistinguishable from a hang, which
is exactly what B then fails to catch.  The quota fix and tonight's stall are
the same defect seen a day apart.

**D. futon1b assert latency.**  Separate, genuinely external: assert exceeding
30s under load against a 1.3-1.6s idle read floor, and an 853ms per-request
overhead measured for a query that scans and returns nothing.  This one is
infrastructure, not protocol, and belongs in futon1b.

## What is NOT established

* Why the intent never resolves.  `:regulator/last-result` is perpetually
  `:intent-persisted` and the stall survived a resume.  I did not determine
  whether f193 is in `:awaiting-substrate` specifically or livelocked for
  another reason.  **Not found.**
* Whether the strip's `last_progress_s` reads the projection writes.  Only
  established that it reports progress while the frame's phase state is an hour
  stale.
* Whether the ~40 singleton classes reduce to A, or whether some are genuinely
  distinct.  They have not been individually re-read.

## Order

C and B before A: a bounded wait plus a working detector make the remaining
failures visible, and visibility is the precondition for judging whether A is
sized correctly.  D in parallel, different repo, no contention.
