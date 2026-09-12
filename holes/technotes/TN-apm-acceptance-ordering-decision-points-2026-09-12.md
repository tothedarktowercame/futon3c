# APM accepted-work ordering decision points, 2026-09-12

## Scope

Static source audit: futon3c master
`c449416826ece78c8d3be6e8a7e4d9be6d7ca095`.  Every source citation below is
pinned as `c449416826ece78c8d3be6e8a7e4d9be6d7ca095:<path>:<line>`.
Historical facts come from the named durable files; no service was queried.
Times are quoted only where those records supply them.  The per-frame EDN
records do not timestamp every internal write, so ordering between such writes
is established by persisted predecessor/successor fields and code order, not an
invented clock time.

The acceptance artifact in this machinery is the authenticated typed
submission plus its content-addressed `:terminal-collection` record
(`src/futon3c/apm/live_job_driver.clj:789-800`).  A wrapper reaching `done` is
process evidence, not the role-output acceptance authority.

## f177 — terminal job beats wrapper cancellation

Timeline (records: `data/apm-campaigns/jit-all-open-v3/queue-state.edn`, parked
f177; `holes/labs/M-apm-demonstration/frame-park-decisions.edn`, f177): the
Student submission was authenticated; the producer independently reached
`:done`; wrapper reconciliation then attempted cancellation and Agency returned
HTTP 409, `invoke-job-already-terminal`, state `done`; the collector returned
`:live-job-wrapper-reconciliation-failed`; the decision was recorded at
`2026-09-07T15:56:00Z`.

Historical decision point, still visible as the final genuine-failure branch
on current master: submission is read before job observation
(`src/futon3c/apm/live_job_driver.clj:1047-1064`), cancellation occurs at
`:1091-1099`, and a non-OK reconciliation is classified at `:1130-1132` before
the already-built collection is persisted at `:1133-1137`.  `job-port/cancel!`
originally made only HTTP 200 successful (`src/futon3c/apm/job_port.clj:24-32`).
Current master first recognizes the exact 409 as `:already-terminal` success
(`src/futon3c/apm/live_job_driver.clj:335-372`).

Accepted was the already-observed typed submission; the branch had that value,
the original job identity, and a 409 response proving the producer was
terminal.  It did not consult/persist the constructed collection before fault
return and interpreted HTTP status rather than the response semantics.
Classification: **MISSING-JOIN**, not RACE: both acceptance and terminality
were available at classification time.

## f194 — authenticated Guide submission discarded on the same 409

Timeline (same two record files, f194): the Guide's second intervention was
delivered and its submission authenticated; the job reached `done` between the
collector's poll and cancel; cancel returned the verbatim HTTP 409
`invoke-job-already-terminal`; the old branch returned before `persist-fn`, so
the collection was discarded and the frame parked.  Decision time is
`2026-09-08T11:00:00Z`.  At the recorded repair pause the coordinator was
quiescent at `2026-09-08T11:31:52Z`; the repaired shared path was replayed at
`2026-09-08T11:32Z` and returned `:already-terminal`, `:ok true`, then persisted
the submission.

The decision points and available information are exactly f177's
`live_job_driver.clj:1047-1137` and `job_port.clj:24-32`.  Accepted was the
authenticated Guide submission.  The raw 409 also named the same job and
terminal state.  Classification: **MISSING-JOIN**.  Commit `bf695656` supplied
the current `cancellation-disposition`; amendment 17 (`f4ae8d5a`) shares this
branch at `live_job_driver.clj:1111-1129`, but solved the earlier, separate
problem of capturing a live session id before cancellation.  It did not make a
409 semantically successful, which is why f194 recurred.

## f218 Guide leg — accepted submission, deliberate cancellation, unrelated stop

Timeline (records:
`data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f218/live/guide-intervention-1.edn`,
`holes/technotes/TN-F218-watchdog-recovery-2026-09-11.md`, and the f218 decision):
the Guide tool event persisted authenticated submission
`2445c6727e603f2b2aed7f66342c5d0ed1886462cf9a092ebd4334a2d6ff6c4b`;
Agency records cancellation at `2026-09-10T22:48:41Z` with reason “typed-submission
wrapper reconciliation”; a later coordinator tick's futon1b read blocked for
30 seconds; the watchdog stopped the campaign at
`2026-09-10T23:20:50.130436312Z`; recovery at `2026-09-11T00:38:35Z` showed the
coordinator advancing; the closure disposition was recorded at
`2026-09-11T10:39:43.407940+00:00`.

The cancellation decision is current `live_job_driver.clj:1089-1137`:
authenticated submission explicitly outranks stale wrapper state, collection
is constructed, cancellation is reconciled, and collection is persisted.
Accepted was the typed submission/terminal collection, and the branch did
consult it.  No cancellation-ordering fault decision was found for this leg.
Its frame headline was instead the later independent watchdog decision:
pending intent becomes an external wait in
`src/futon3c/apm/durable_coordinator.clj:616-654`, and watchdog halt invokes
durable stop in `src/futon3c/apm/semantic_progress_watchdog.clj:180-200`.
Classification: **OTHER**.  Cancellation-after-acceptance behaved as intended;
the substrate timeout and watchdog deadline caused the stop.  A replay must
still protect this ordering because future consumers could wrongly treat the
cancelled wrapper as negative role evidence.

## f227 — repair exhausted before retained-session recovery became durable

Timeline (records:
`data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f227/live/guide-intervention-2-review.edn`,
f227 parked entry in `queue-state.edn`, and f227 decision): original promotion
review job
`apm-role-5a1aa8b0dfa0f1cd1f4634d20cb262ad87f7c182d9ea6bcc506cdd844d064817`
returned a terminal report without a typed submission, a repair successor was
dispatched, and the durable phase exhausted with
`:live-job-terminal-repair-exhausted`.  The parked fault preserves the original
job as terminal/done but explicitly records `:typed-submission-missing` and one
repair attempt.  The retained session was used later to create the authenticated
submission
`856f8294163582224d19a6c00aeb35b750c6030788da2c0385b3afcc7e1e02fe`
and the recovery was recorded at
`2026-09-12T13:31:25.296810759Z`.

Decision point: `live_job_driver.clj:1238-1271` chooses the submission from the
current state's collection/current provider observation; repair archives the
predecessor and replaces `:ticket` at `:1503-1563`; exhaustion at `:1450-1493`
classifies from current validation/findings and repair counters.  The
archive retains predecessor job, terminal collection and findings
(`:1504-1522`), while exhaustion classifies the observations durable at that
time.  The queue then turns this code into a frame park through
`src/futon3c/apm/queued_frame_adapter.clj:296-340`.

Accepted was the recovered authenticated typed submission, but it was not yet
durable when exhaustion classified the frame.  At classification time the
controller had the original terminal report, job id, and retained session id;
those identified where recovery could be requested, but the report itself
affirmatively said the typed submission was missing.  It consulted the durable
job report and current repair result; there was no accepted submission record
to join.  Classification: **RACE**.  This incident does not establish a direct
violation of the candidate invariant at the controller's durability boundary:
the useful completion existed as recoverable session state before it existed as
an accepted typed submission.  It does expose the adjacent ordering in which
repair exhaustion and session release could make that recovery impossible.

## PROPOSED repair directions

| Decision point | Classification | PROPOSED direction |
|---|---|---|
| f177/f194 cancel response before collection persistence | MISSING-JOIN | Retain current `cancellation-disposition`: 409/already-terminal succeeds only after exact job identity and terminal state are checked; persist accepted collection before any later lifecycle classification. |
| f218 accepted submission followed by wrapper cancellation | OTHER | Preserve the current two-authority model: collection says role output accepted; wrapper state says execution stopped. Never infer role failure from the latter. Keep substrate/watchdog repair separate. |
| f227 repair exhaustion | RACE | Add a bounded retained-session recovery/recheck before exhaustion and before session release. If that step creates or finds an authenticated submission, rerun the normal collection path; do not treat session prose alone as acceptance. |
| amendment-17 session capture (`f4ae8d5a`) | related, already fixed | Keep capture-before-cancel at `live_job_driver.clj:1111-1129`; do not use session presence as a substitute for submission acceptance. |

## PROPOSED replay-test plan

All pins should be extracted verbatim from the production records named below,
stored as fixtures with provenance/digest, and driven through `live-job-driver/drive!`
and the queue adapter—not tested as isolated predicates.

1. **f177:** pin the parked `:fault/result` cancel response from
   `queue-state.edn`.  Drive an authenticated submission, job poll `done`, and
   the exact 409 through the shared collection branch.  Assert one durable
   terminal collection and no wrapper fault.
2. **f194:** pin the complete f194 cancel response and authenticated Guide
   submission shape from `frame-park-decisions.edn`.  Drive the race as poll
   `running`, submission present, cancel 409/done.  Assert
   `:cancellation/disposition :already-terminal`, then replay from persisted
   state and assert idempotent certification.
3. **f218:** pin the Guide typed-submission/collection shape from
   `guide-intervention-1.edn` and cancellation facts/times from the F218 TN.
   Drive submission present → cancel 200/cancelled → persist → replay with the
   wrapper cancelled.  Assert the accepted collection remains authoritative;
   separately inject the pinned futon1b timeout into the coordinator/watchdog
   replay and assert it cannot rewrite the Guide result.
4. **f227:** pin the original job/dispatch/submission ids from
   `guide-intervention-2-review.edn` and the parked repair history from
   `queue-state.edn`.  Drive the pinned original terminal report with
   `:typed-submission-missing`, successor repair dispatch, retained-session
   recovery producing the pinned authenticated submission, then exhaustion.
   Exercise both orderings around the exhaustion boundary: recovery durable
   first must collect and must not emit
   `:live-job-terminal-repair-exhausted`; exhaustion durable first may remain a
   typed fault, but must retain the session until the bounded recovery decision
   completes.  A companion pin whose session produces no submission must still
   exhaust.
