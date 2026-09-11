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
