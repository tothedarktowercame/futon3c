# C251 — invoke-jobs ledger durability discovery

Date: 2026-08-31

Discovery only. No persistence format or code changed.

## Current atomicity

The rewrite is **not atomic**. `persist-invoke-jobs-ledger!` is exactly:

```clojure
(spit (invoke-jobs-store-path) (pr-str ledger))
```

at `src/futon3c/transport/http.clj:315-322`. `spit` opens the one authority
path for replacement, truncates it, and writes the 134.6 MB serialization in
place. There is no temporary sibling, atomic rename, file-channel `force`,
directory sync, checksum, backup, or writer lock. The live path is on ext4,
but ext4 cannot make an application-level truncate-plus-multiwrite sequence
atomic.

A process death during the write can therefore leave the sole file empty,
truncated, or syntactically incomplete. On the next load, the catch at
`:326-338` prints an error and silently substitutes the empty default ledger.
That turns a torn file into apparent loss of all 6,000+ jobs while leaving the
torn authority in place.

There is also an ordering race. Ledger state changes are atomic in memory,
but persistence happens after `swap!`/`swap-vals!` and is not serialized
(`:384-404`). Two threads can obtain states N and N+1, then finish their
full-file writes in the opposite order, leaving older N on disk. Concurrent
in-place writers can also overlap at the file descriptor level.

Finally, persistence exceptions are caught and only printed. The mutation
helper returns normally with the newer in-memory ledger. Callers can therefore
acknowledge an accepted or terminal transition that never reached disk.

## Guarantee promised versus guarantee held

The mission and source call this ledger durable, require accepted state to be
persisted before acknowledgement, require terminal state before delivery, and
define restart recovery (`M-codex-irc-execution.md:188,238-250`; source comments
around `http.clj:354-365`). The implementation actually provides:

- **Orderly process exit after a successful write:** usually recoverable from
  the last writer, assuming no later writer reordered it. There is no explicit
  close hook, but `spit` closes its writer before returning.
- **SIGTERM/kill during ordinary operation:** no promise. A kill outside a
  write leaves the last completed userspace write; a kill during a write can
  tear the only file. `SIGKILL` cannot run cleanup.
- **Power or kernel loss:** no durable-commit promise. Neither file contents
  nor directory metadata is forced to stable storage before acknowledgement.
- **Write error:** the process continues and callers are not told persistence
  failed.
- **Concurrent mutations:** disk ordering is not guaranteed to match the
  atom's ordering.

Thus “survives JVM restart” is demonstrated only for a complete parseable
file. Crash atomicity and acknowledged durability are not established.

## Direct consumers that a migration would break

The serving reader expects one EDN map. Direct out-of-process consumers do too:

1. `src/futon3c/apm/countdown_control.clj:505-529` reads the whole map to
   assemble a campaign delivery ledger.
2. `scripts/memory_outcome_sweeper.py:36,645` parses the file with
   `edn_format.loads`.
3. Historical/research tooling reads it directly, including
   `holes/labs/M-memory-retrieval/arm_attribution_backfill_20260801.clj` and
   `derive-panel-nomination-sweep-20260801.py`.
4. Recovery tests replace `invoke-jobs-store-path` with a single EDN fixture
   and assert the map-shaped restart behavior.
5. Documentation and operator recipes name `FUTON3C_INVOKE_JOBS_FILE` as the
   canonical single job store.

An API-backed migration must preserve a snapshot/export reader during the
transition or update these consumers together. Merely changing the server
writer would strand offline evidence tooling.

## Candidate schemes

### Atomic whole-map snapshot

Write a sibling temporary file, flush/`force` it, atomically rename it over the
authority, and sync the parent directory; serialize writers under one lock.

- Preserves the current EDN shape and all direct readers.
- Makes each completed replacement crash-atomic and ordered.
- Still writes 134.6 MB per mutation and still needs explicit error
  propagation. It fixes correctness urgently, not scaling.

### Append-only framed log plus atomic snapshots

Append one checksummed, sequence-numbered transition record and force it before
acknowledgement. Periodically write an atomic snapshot and compact only after
the snapshot and directory entry are durable.

- Write cost follows the transition, preserves complete history, and recovery
  can stop loudly at a torn final frame.
- Requires a precisely versioned transition algebra, replay/idempotency rules,
  compaction protocol, and a compatibility snapshot/export for direct readers.
- Plain newline EDN without length/checksum framing is insufficient: embedded
  output text and a torn tail must be distinguishable.

### Per-job atomic files

Write each job through temp+rename and keep indexes separately.

- Bounds ordinary job updates and localizes corruption.
- Cross-job indexes (`job-order`, `trace->job`, sequence allocation) cease to
  be one transaction; directory scans and thousands of files become part of
  recovery. Multi-record invariants require a journal anyway.

### Embedded transactional store

SQLite in WAL mode (or an equivalently mature embedded transactional store)
can atomically update jobs, events, trace identity, and active indexes with
ordered commits and explicit synchronous policy.

- Supplies transactions, crash recovery, indexed queries, and bounded writes
  without implementing a database protocol locally.
- Adds a schema, JDBC/native dependency, migration/export tooling, backup and
  checkpoint operations, and an explicit choice of SQLite `synchronous` mode.
  WAL commit is not a power-loss promise unless that policy is selected and
  tested.

## Recommendation and decision owner

First, independently of the final backend, repair the present writer to use a
serialized atomic temp-file replacement with `force`, directory sync, and
loud error propagation. That is a bounded correction making the existing
durability claim less false while retaining every reader; it does not solve
cost.

For the scalable format, choose **SQLite WAL with a committed EDN snapshot
export for compatibility**, rather than growing a bespoke append-log database
inside `http.clj`. The ledger already has secondary indexes, concurrent state
transitions, recovery rules, and offline consumers—the conditions under which
home-grown replay and compaction become a second state machine to prove.

This backend decision belongs to Joe. It selects the acknowledged durability
level (`synchronous` policy), operational dependency, migration/rollback plan,
and whether the canonical authority remains human-readable EDN or becomes a
transactional store with EDN as a projection. The urgent atomic snapshot
repair can be commissioned separately without pre-deciding that migration.
