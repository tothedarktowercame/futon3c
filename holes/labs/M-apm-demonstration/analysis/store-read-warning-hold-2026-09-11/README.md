# Slow store reads: finish the frame, then repair before advancing

Joe commissioned this behavior on 2026-09-11. This packet changes the
coordinator's treatment of read latency; it does not establish that the
underlying query performance has been repaired.

## Executable behavior

A campaign opts in with `store-read-health.edn` under its campaign root:

```clojure
{:policy/version 1 :repair-agent-id "codex-16"}
```

`set-alight-problem-list!` reads this authority at each queue tick. An explicit
`:store-read-health` in launch `:authority` takes precedence. The checked
policy binds the real substrate and evidence HTTP clients during preparation,
frame execution, and retirement. Their per-request HTTP/body deadline is
30,000ms. Elapsed time over 5,000ms creates a warning, including frame/problem,
URL, wire trace ID, configured deadline, elapsed milliseconds, outcome and
time. Read values and exceptions are preserved. HTTP response decoding is
included in the measurement. Individual warning files are atomically persisted
under `<frame-directory>/store-read-warnings/<digest>.edn`; no response body
or exception message is logged. Futures inherit the binding.

A returned response still goes through all attachment, identity, independent
review and certificate checks. A warning never substitutes for evidence. A
30-second transport timeout still fails the read and follows the existing
bounded transport recovery. The existing 30-minute coordinator tick deadline
is unchanged: this change is not yet resumable publication across ticks.
External role processes do not inherit JVM dynamic bindings; this packet
covers coordinator-owned HTTP reads through the two instrumented clients,
not every tool call made by a Student or Guide.

After a terminal frame has retired, the queue reads and validates its warning
files. With warnings, it durably records the completed frame, clears `:active`,
retains the cursor/resumption queue, and stores `:store-read/hold`. It does not
mint or prepare a successor. The hold is persisted *before* Agency dispatch.
A deterministic `store-repair-<hold-id>` job announces and activates a bell to
the configured repair agent. Replays observe an existing running or terminal
job instead of reactivating completed work. The accepted job ID is persisted.
An unavailable Agency leaves the durable hold in place and surfaces a dispatch
error; it cannot advance the queue. Corrupt/unreadable warning files fail
closed. Removing policy does not erase or bypass already retained warnings.

The coordinator reports `:batch-paused` and settles its tick. Voxterm displays
"frame completed; queue held" with count, maximum latency and repair job,
instead of a stale halt or a newer queued frame. Ordinary `resume-paused`
rejects this hold. Completion of the bell alone never releases it.

## Repair agent runbook

The bell contains the exact queue-state path, coordinator ID, hold ID, terminal
receipt ID, warning count and up to five example warnings. The queue holds the complete records. Read those records, correlate their wire
trace IDs with futon1b logs, diagnose and repair the cause, then retain code
commit and validation artifacts. Measure the relevant operation after repair;
a successful unrelated probe or a bigger timeout is not repair evidence.
Never rerun the completed mathematical work, erase warnings, weaken evidence
checks, or restart V2/topology/JVMs. If diagnosis cannot be completed or repair
cannot be verified, retain the hold and report the blocker.

After the coordinator tick has settled, the trusted repair operator calls:

```clojure
(futon3c.apm.jit-queue-coordinator/release-store-read-hold!
 {:registry-path "data/apm-coordinators/registry.edn"
  :coordinator-id "jit-queue:jit-all-open-v3"
  :receipt {:hold/id "EXACT-HELD-ID"
            :repair/status :verified
            :repair/commit "EXACT-40-HEX-COMMIT"
            :validation/evidence ["PATH-TO-RETAINED-VALIDATION"]}})
```

This API acquires the coordinator tick lock, requires a quiescent coordinator,
and validates the exact current hold and repair receipt shape. It records the
operator's assertion with the complete hold in `:store-read/repairs`, restoring
any independent pause/failure status. **It does not authenticate a claimed
review, inspect git, or interpret test output.** Those checks are the trusted
operator's responsibility before calling it; this is not autonomous acceptance
of arbitrary agent output.

If the release returns `:ok true` and `:queue/status nil`, resume through:

```clojure
(futon3c.apm.durable-coordinator/resume!
 "data/apm-coordinators/registry.edn" "jit-queue:jit-all-open-v3"
 "Verified substrate repair for hold EXACT-HELD-ID; evidence PATH")
```

If another pause or failure status was restored, leave it in force. If the
process died after release but before resume, inspect `:store-read/repairs`
for the exact retained receipt and confirm quiescence before resuming; do not
invent another hold or discard history. The next frame remains the queue's
responsibility. This repeats when a subsequent frame records genuine warnings.

## Validation

Tests exercise 5,000ms vs 5,600ms with an injected monotonic clock through both
real HTTP-client boundaries, without sleeping for seconds or stressing the
live store. They check returned values, preserved exceptions, wire identity,
hard-deadline cancellation, future context propagation, disk readback and
corruption, actual queue completion before hold, no successor, dispatch replay,
failed dispatch, explicit repair release, running-coordinator refusal and
preservation of an independent manual pause. Existing visibility/admission,
client, queued-frame adapter and coordinator tests remain required.

Deployment observations are separate from these simulated latency tests. No
synthetic warning is inserted into a live frame and no fake repair is claimed.
The motivating historical timing evidence is in the sibling
`f224-store-timeout-2026-09-11` packet.

## Executed deployment

Canonical futon3c commit `ed791340` was loaded from its own classpath on
2026-09-11 at 19:01:02Z. The campaign policy was atomically enabled for subsequent
V3 queue ticks; an already executing tick keeps its existing context. No JVM
or APM loop was restarted. Voxterm commit `3faf4f6` was deployed by restarting
only the UI service. `enabled.edn` records the policy and active F225 identity.

One labelled diagnostic read of the previously slow endpoint completed in
1,381ms with the effective 30,000ms deadline. `diagnostic.edn` records it;
`server-trace.log` correlates the same wire ID with 0ms admission wait,
1,377ms expensive-read callback and 1,378ms server request duration. This is
availability/correlation evidence, not a stress test or proof of a query repair.
No synthetic warning was written to a production frame. At readback F225 was
running and F226 remained queued to resume; no production warning hold existed
at that observation. The automatic warning/hold/dispatch cycle is tested but
has not yet been observed end-to-end on a genuine production slow read.
