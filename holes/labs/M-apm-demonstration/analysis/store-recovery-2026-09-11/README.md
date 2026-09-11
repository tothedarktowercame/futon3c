# Store waits and durable APM recovery

Implemented 2026-09-11 at Joe's request, with APMV2 and APMV3 disabled.
No shared runtime was reloaded or restarted and no APM/Lean/agent experiment
was dispatched. This packet is for review before deployment, not permission to
resume V3. V2 is retired and must remain disabled.

## Historical diagnosis: what the records establish

`store_journal_probe.py` reads the existing futon1b service journal for
2026-09-10 12:00Z through 2026-09-11 00:44:30Z. Its retained JSON contains the
command, raw journal SHA256, metadata-only observations and limitations.
157,439 log lines include 32 completed HTTP requests over 25 seconds and
292 SQLite `SQLITE_BUSY` index failures. These counts are not all APM requests.
The probe excludes duplicate expensive-read timing lines from its HTTP count.

Notable HTTP handler durations:

| Completion time | Operation | Duration | Recorded outcome |
|---|---|---|---|
| 22:02:54Z | POST hyperedge | 391.537s | 503: projection source moved after quiescence |
| 22:44:35Z | POST memory/projection | 364.222s | Same projection consistency refusal |
| 22:50:30Z | POST memory/projection | 650.335s | Client disconnected |
| 22:50:30Z | Three POST memory/assert requests | 270.781s, 90.068s, 173.603s | Clients disconnected |
| 22:50:30Z | GET F218's specific evidence ID | 66ms | Client already disconnected |

The last row uses the exact evidence ID retained by F218's timed-out tick.
Other reads of that ID completed in 50–218ms. A later matching URI is not a
uniquely correlated client attempt: historical GETs lacked trace IDs.

The server has four HTTP workers and a 16-task queue. Projection construction
runs synchronously, under the shared projection monitor, waits for indexing
quiescence, hydrates components, compares the source watermark, and rebuilds
when it moved. The long requests and source-moved refusals establish actual
projection cost/contention, not merely guessed overload. They can occupy the
same workers needed by short evidence reads. **Worker queueing and/or pauses
before handler entry are strongly indicated, but not separately measured in
the old records.** Journal receipt timestamps can also lag message production.

SQLite failures concern the auxiliary text index. They are real concurrent
contention, but these records do not prove that SQLite caused the XTDB
projection timeout. There is no historic worker-queue duration or projection
lock duration with which to apportion the total. Heap/GC causality is likewise
not established. The diagnosis is now narrower than “store slow,” but not a
demonstrated permanent performance repair.

## Implemented recovery behaviour (futon3c)

- Evidence GETs receive unique `x-trace-id` values, also retained in typed
  exceptions. Timeout/unavailability carries read operation and
  `evidence:not-obtained`; HTTP 429/502/503/504 is explicitly retryable
  unavailability. HTTP 401/500 and other unexpected statuses remain loud,
  non-transport errors. HTTP 404 retains actual absence semantics.
- Promotion catches only explicitly typed transport ExceptionInfo, feeding the
  existing bounded retry state machine. Unclassified/integrity exceptions are
  not relabelled. No deadline, retry limit, or validation gate was increased.
- The retry retains the newest successfully persisted checkpoint, rather than
  blindly returning to the caller's older phase. A retry following process
  restart preserves its attempt/history and does no work before its wake.
- An unsuccessful hold write cannot report “retry scheduled.” Failure to make
  the recovery state durable is surfaced as `promotion-hold-persistence-failed`.
- Candidate persistence already checks canonical identity and observes the
  entry/edge before issuing another write. A new lost-response test establishes
  that a landed pair is recovered with one actual write, not duplicated.

This strengthens the existing transactional steps. It is not a claim of
exactly-once execution across arbitrary process death between external role
dispatch and local persistence. That separate dispatch/receipt protocol and
its reviewed guards remain in force.

## Store instrumentation — futon1b `a4bbf61`

- The bounded request executor now measures time from task submission to worker
  admission. A thread-local task ID joins that measurement to the request log;
  requests without a client trace receive a task-derived log trace ID.
- Cheap health exposes main HTTP worker totals/active count, queued request
  count, remaining queue capacity and completed task count. The existing
  expensive-read permit count/waiter count remains a separate observation.
- Projection logs identify operation ID, waiting for the projection lock,
  acquired lock and wait duration, indexing-quiescence wait, selection count,
  hydration elapsed time and whether source movement forces another attempt.
  No proof, candidate body, or database query parameter value is added.
- Existing worker/queue bounds, rejection policy, source-watermark checks,
  index consistency and write validation are unchanged.

Admission timing begins when HttpServer submits an exchange runnable; it does
not include kernel backlog or pre-submission delays. Health cannot tell an APM
request's exact rank. A global queue length is not a per-request position.

## User-visible state — Voxterm `2128b42`

The strip reads durable coordinator registration/state as EDN, so a deliberate
stop takes precedence over the stale watchdog. The current phase shows
**stopped**, with **phase age at stop**, not an increasing execution duration.
A still-running role is explicitly marked draining. Without a matching role,
positive durable tick evidence is labelled **coordinator tick claimed**, not a
claim that a thread is executing. Unknown evidence stays unknown.

The UI distinguishes expensive-read permits from HTTP workers and request
queue length. Saturation produces a store-wide warning such as “7 requests
queued; 4/4 HTTP workers busy.” It explicitly says APM queue position is unknown.
Promotion retry has the label “waiting to retry store operation.” Missing
instrumentation is `?`, never a fabricated empty queue.

The queue probe uses the already provisioned independent health listener
`http://127.0.0.1:7072/health`, configurable by `FUTON1B_HEALTH_URL`, rather than
joining the busy main request queue at :7073.

## Validation

- `futon3c.apm.live-promotion-test`: 55 tests, 285 assertions.
- `futon3c.apm.promotion-candidate-store-test`: 17 tests, 67 assertions.
- `futon3c.evidence.futon1b-backend-test`: 22 tests, 94 assertions.
- New executor test: one test, seven assertions; actual one-worker queue
  saturation, rejection, measured queue wait and completion. No database or
  listener is started.
- `futon1b-server` compiled in a separate process without starting a server or
  opening a store. Full projection/XTDB performance qualification is not claimed.
- Voxterm lifecycle tests: five tests; plus existing timeline-feed, watchdog,
  cascade, statement-repair and parked-frame checks passed. JavaScript passes
  `node --check`.
- Clojure changes pass clj-kondo (no errors/warnings) and workspace parentheses
  checks. Diffs checked. An existing backend classification test was found
  sleeping through the production retry window; that assertion now explicitly
  sets retry duration to zero. Its dedicated retry tests remain enabled.

Controls include timeout, unavailable status vs actual absence, preservation
of a newer checkpoint, restart from serialized hold state, pre-wake inactivity,
bounded exhaustion, integrity refusal, failed hold persistence and lost write
response without duplicate effects. These are offline controls, not a successful
live canary or a throughput claim.

## Deployment and next decision

Review these three commits together. Loading source alone will not instrument
an already-created HTTP executor: the executor change requires recreating the
listener under the normal store lifecycle. No such recreation was done here.
Do not use a second store JVM, unbounded worker queue, disabled consistency
guard, or increased APM deadlines as deployment shortcuts.

After approved deployment, collect the new measurements before making a
performance intervention. If projection hydration/invalidations dominate,
the structural next step is an explicitly reconciled projection operation
with an observable queued/building/published/refused state, and a reviewed
source-consistency contract. If queue admission dominates, that operation must
avoid monopolizing request workers while preserving the same publication
checks. Neither simply adding workers nor ignoring source movement is justified
by this packet. No V3 canary is authorized by this note; V2 never resumes.
