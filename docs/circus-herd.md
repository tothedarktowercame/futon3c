# Circus herd route

`GET /api/alpha/circus/herd?since=<ISO instant>&until=<ISO instant>&agents=<csv>`
returns parallel operator-session lanes and indexed agent-origin interactions.
The default window is the last six hours; explicit windows are `(since, until]`.
Lane turns are oldest first, and `first` / `last` are their timestamps in that
window. `cursor` is the latest turn or bell timestamp served, or `since` for an
empty result. Pass it as the next `since`. Agent filtering keeps selected agents'
lanes and interactions where either sender or recipient is selected.

Only `session-heads/*.json` receipts are enumerated; their mtimes cheaply exclude
inactive sessions. Each head's predecessor chain stops at `since`. No turn-store
listing occurs on the request path. Heads/links must already be populated (the
existing `backfill-strand!` handles legacy strands). Chronological writes are
assumed by predecessor traversal. A timestamp cursor cannot recover a later
backfilled record older than the cursor; request a full window after backfill.

Turn projections use the strand route's exact cache and event invalidation.
Unlike an explicit strand request, herd does not force recomputation of the newest
two turns, so opening the overview after the strand reuses those projections.
`threads_n` counts projected 馬 threads; `quotes_n` counts projected quote edges.
`ma_status` is the projected 馬 verdict status.

Agent-origin writes now append an id/time receipt to
`bell-index/<urlencoded recipient>.json`, using atomic replacement and the same
per-recipient JVM and file locks for writes and backfill (also across JVMs). Deterministic-id writes also
index once (and repair a missing receipt on retry). A crash between a random-id
record write and index update needs the explicit backfill. The index contains
ids/timestamps, not duplicated record bodies. A pending recipient session is
omitted; its agent remains available as the interaction endpoint. Known sender
`caller_session` and recipient `session_id` are returned. There is no ingress
job lookup or remote query on this route; private transport job state is not an
additional route dependency. Updated record sessions are read without reindexing.
`kind` retains the turn's surface, defaulting to `agent` if absent.

## One-off bell backfill

Run from futon3c after review. Dry-run (no writes):

```sh
clojure -M -e "(require '[futon3c.xiang.turn-store :as t]) (prn (t/backfill-bells! (t/store)))"
```

Explicit write, safe to repeat:

```sh
clojure -M -e "(require '[futon3c.xiang.turn-store :as t]) (prn (t/backfill-bells! (t/store) :write? true))"
```

Use `(t/store \"/path/to/turn-store\")` to select a non-default directory.
No backfill runs automatically at namespace load or on herd requests.

Invented response:

```json
{"lanes":[{"session":"session-a","agent":"agent-a",
 "first":"2026-01-01T02:00:00Z","last":"2026-01-01T02:00:00Z",
 "turns":[{"id":"turn-invented","created_at":"2026-01-01T02:00:00Z",
 "ma_status":"read","threads_n":1,"quotes_n":1}]}],
 "bells":[{"at":"2026-01-01T03:00:00Z","from_agent":"agent-a",
 "from_session":"session-a","to_agent":"agent-b","turn_id":"turn-bell",
 "kind":"bell"}],"cursor":"2026-01-01T03:00:00Z"}
```
