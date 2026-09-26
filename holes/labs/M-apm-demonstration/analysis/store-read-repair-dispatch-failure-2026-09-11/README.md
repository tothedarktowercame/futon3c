# Failed automatic store-repair dispatch

The retained Agency job failed before executing any tool or command. Its terminal
message reports a thread-store conflict: the targeted Codex session already had
an active writer. Delivery of the failure also failed because the monitor caller
was not a registered seat. See `observed-job.json` for actual observations.

The current V3 queue is paused with no active frame after F225/m93J03, retaining
hold `bad82e9694d2ba65a3e0c6f9769f5bffd3863413fa38fcdeb273e0016bf05cdc`.
Two successful reads took 6248 and 6007 ms, exceeding the 5000 ms warning threshold
but not the 30000 ms hard deadline. The coordinator reports complete, while the
watchdog record still reports watching with coordinator-enabled true. The queue
hold and the failed dispatch must not be mistaken for completed repair.

This packet is diagnosis only. No namespace reload, session reset, queue resume,
replacement dispatch or database-performance repair was performed. The ongoing
seat can consume the retained warning directly; future automatic repair dispatch
needs an idle owned seat or queue delivery into the active session, with durable
failure reporting for the monitor. Do not bypass the single-writer invariant.

## Implemented handoff repair

`store-read-hold/dispatch!` now resolves the exact registered session and submits
an `apm-store-repair` typed followup. It never announces or activates a second
invoke job. The existing ready endpoint withholds delivery while the agent is
invoking, and the Emacs poller delivers into its existing session. Delivery-time
validation requires the exact retained queue hold; missing/unreadable/released
holds fail closed. Existing inbox-zero followup semantics are unchanged.

Executed tests: store-read-hold 4/15, followup-queue 6/24,
followup-validity 5/14, followup HTTP 2/9 (17 tests / 62 assertions total).
Lint, check-parens and diff checks pass. These test routing and hold validation,
not database-performance repair.

The correlated server traces show zero HTTP admission queue wait in both reads.
The first has 6024 ms expensive-read execution within 6232 ms server duration;
the second has 3146 ms execution within 6005 ms server duration, with roughly
2.9 seconds before permit acquisition. Both expensive reads report four holders.
This locates query cost plus database permit contention; it does not by itself
establish a safe worker/permit tuning change or a repaired query.
