# P2c fix 1 — atomic promise-state files

Date: 2026-09-27

## Change

`futon3c.agency.atomic-file/write!` writes UTF-8 bytes to a temporary file in
the target directory, calls `FileChannel.force(true)`, and replaces the target
with `ATOMIC_MOVE` plus `REPLACE_EXISTING`. Park and followup persistence now
share this implementation.

Both loaders use `atomic-file/load-edn-map!`. A missing file remains a silent
empty state. A parse failure or non-map root is atomically renamed to
`<path>.corrupt-<epoch-ms>`, logged to stderr, counted in
`atomic-file/stats`, and followed by an empty in-memory state. `/health` exposes
the same counter and last event under `agency-file-corruption`.

Preserving the corrupt source and making the event inspectable makes an empty
boot recoverable and visible. Refusing to boot would give stronger protection
against operating with missing promises, but it would also remove Agency's
ability to serve unaffected queues. I prefer the requested visible degraded
boot provided the health signal is monitored and operators recover the
quarantined file before treating the empty store as authoritative.

## Crash-test changes

- A truncated park file is preserved and counted; startup returns empty and
  replay still reports the missing active park.
- A truncated followup file now boots, preserves and counts the file, and
  replay reports the missing queued/dedupe state instead of a parser crash.
- SIGKILL after the new temp file is forced but before rename leaves the old
  target parseable and unchanged. Restart agrees with the old history/state.
- The park-made, park-released, and followup-enqueued persist/history gaps still
  disagree. This packet does not change authority or transition ordering.

## Other file writers inspected

- `agency/turn_queue.clj` writes a sibling temp file and atomically replaces
  the target, but uses plain `spit` and does not force file or directory data.
- `agency/promise_history.clj` writes its chain sidecar through a temp file and
  atomic move, but uses plain `spit` without a force.
- `agency/roster_store.clj` uses a temp file plus atomic move; its payload write
  is also plain `spit` without a force.
- `agency/inbox.clj` uses `Files/write` plus atomic move without a force.
- `agency/registry.clj` writes `/tmp/futon-bell.edn` with plain `spit`; it is a
  transient notification file rather than an authoritative state store.
- The invoke-jobs ledger in `transport/http.clj` already uses a same-directory
  temp file, `FileDescriptor.sync`, atomic replacement, and directory force.

None of these writers changed in this packet.

## Validation

- P2c crash tests: 7 tests, 31 assertions.
- Parked-on tests: 19 tests, 83 assertions.
- Followup-queue tests: 6 tests, 24 assertions.
- Promise-history tests: 8 tests, 46 assertions.
- clj-kondo clean; check-parens OK.
