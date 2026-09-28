# P2c-2-0 — durable promise-history outbox

This design keeps the park and followup `/tmp` files authoritative. The held
switch to history authority remains held (`holes/missions/M-象-2000.md:970-976`).

## Present transition boundary

`park!` mutates the in-memory park state, atomically persists it, then calls
`history/record!` for `:promise/park-made` (`agency/parked_on.clj:368-428`). A
completion removes/retracts records, persists, and only afterwards records
dependency termination, budget exhaustion, wake and release
(`parked_on.clj:326-355`). Followup `update-state!` likewise persists first,
then derives and records requeue/final transitions (`followup_queue.clj:45-67`);
enqueue enters through it at lines 69-93. Therefore history-before-state cannot
happen today, as the crash report records (`holes/missions/M-象-2000.md:1466-1475`).

`record!` increments process-local submission order, allocates a random UUID,
drains captured edits, advances `/tmp/futon3c-promise-history-chains.edn`, then
submits a daemon task (`promise_history.clj:20-26,46-75,77-124`). The task
appends once. Failure is only counted and printed; it is not retried
(`promise_history.clj:28-34,116-125`). The executor queue and counters are
process-local, and startup loads neither, so SIGKILL silently loses queued
work. Worse, the chain head is already durable, leaving a detectable missing
transition. `await-writes!` is diagnostic only (`promise_history.clj:127-132`).

Atomic state replacement itself is now sound: a same-directory temporary file
is forced before `ATOMIC_MOVE` (`atomic_file.clj:19-41`). Corrupt input is
preserved and counted (`atomic_file.clj:43-75`). The remaining defect is the
gap between that replacement and the history task.

## Smallest outbox protocol

Put `:history-outbox {evidence-id prepared-entry}` in each existing authority
file, `/tmp/futon3c-parked-on.edn` and `/tmp/futon3c-followups.edn`. Do not add a
sibling file: replacing state and a sibling cannot be one filesystem commit.
Mark this root as persistence metadata, excluded from `promise-capture/clean`
and snapshot replay just as `:just-released` is excluded today
(`promise_capture.clj:3-15`).

For each mutation, while holding the existing store operation:

1. Prepare every immutable format-3 entry, including evidence id, captured
   edits, sequence and predecessor. Add it to the new state and reserve its
   head in memory, without writing the chain sidecar.
2. Atomically persist **state plus pending entries**. A persist error must
   propagate; it must not be swallowed as parked `persist!` currently does
   (`parked_on.clj:88-94`).
3. Return/perform the existing runtime action, while the single writer drains.
   Append the exact prepared entry. Success, or `:duplicate-id` followed by an
   exact readback, means delivered; any mismatch is a conflict and stays pending.
4. Persist the fixed chain head idempotently, then remove the pending entry and
   atomically persist the authority file again. A crash anywhere after step 2
   leaves enough information to repeat steps 3-4.

On startup, load the authority file, rebuild reservations from its pending
entries plus the chain sidecar, enqueue every pending entry in sequence order,
and expose pending/drained/conflict counts. Recovery waits for this drain in
the crash test before running P2b comparison; ordinary Agency boot can drain
asynchronously, preserving today's non-blocking service.

## Identity and chain replay

History IDs are **not deterministic today**: `record!` uses `UUID/randomUUID`
(`promise_history.clj:83-90`). The packet should derive
`promise-history:<sha256>` from a canonical transition identity: store,
promise/followup id, type, event time, semantic details and captured-edit
digest. The complete prepared entry is persisted, so restart never recomputes
identity from changed state. This follows the existing check writer's rule:
read deterministic id, accept a duplicate only when its identity fields match,
otherwise conflict (`promise_outcome.clj:104-150`).

The current chain allocator increments before enqueue (`promise_history.clj:50-75`).
Re-calling it would allocate another sequence and can break the chain. The
outbox instead freezes `:history/promise-sequence` and `:history/predecessor`.
Drain never calls `next-link!`; `commit-head!` installs that already prepared
head only if equal to, or directly after, the durable head. Re-draining the
same row therefore neither increments sequence nor changes predecessor.
Multiple pending rows reserve from the preceding prepared row, not merely the
last acknowledged sidecar head. Existing `check-chains` then continues to
detect missing or mismatched predecessors (`promise_history.clj:162-199`).

## Live risk and first packet

Parked state serves every agent. First land backward-compatible outbox helpers
and the extended file schema. At a quiet point, record the live park count and
file hash, reload `promise-history`, then `followup-queue`, then `parked-on` in
one proof-eval call from master. Verify the park count and semantic snapshot
are unchanged, both files parse, pending drains to zero, and LIST/readback has
one exact evidence id per drained row. Do not restart or change authority.

The first implementation packet covers only the three existing transitions:
park-made, the completion batch (dependency/wake/release), and
followup-enqueued. Reuse `promise_crash_test.clj:43-65,82-104`: after SIGKILL
at its existing post-persist boundary, restart, drain, and require P2b equality.
Add kills after append/before acknowledgement and after chain commit/before
outbox removal. Finally drain the same persisted outbox twice and assert one
evidence row, one sequence number and an empty outbox. Later packets can move
the remaining ready-queue, followup lease/terminal and maintenance transitions
onto the same protocol.
