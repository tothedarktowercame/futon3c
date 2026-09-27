# P2c — promise cache/history crash tests

Date: 2026-09-27

## Method

Each case starts a separate JVM with both promise paths redirected into a fresh
temporary directory. Promise evidence is written through a file-backed test
implementation of the real evidence protocol in the same directory. The child
touches a boundary marker only after the named operation reaches the boundary;
the parent observes that marker and calls `Process.destroyForcibly` (SIGKILL).
A second fresh JVM loads the persisted park and followup files, reads the
persisted evidence, and calls `promise-replay/compare-state`.

The boundary is deterministic. Polling waits for the marker; elapsed time does
not choose when the process is killed. No live port, live evidence store, or
real `/tmp/futon3c-*.edn` path is used.

The three requested transitions have the same reachable order:

1. mutate the authoritative atom;
2. `spit` the `/tmp` state;
3. call `history/record!`, which allocates and queues the asynchronous write.

Consequently, history-written-before-state-persist is not a reachable boundary
for park-made, park-released, or followup-enqueued in the current code. The
suite does not fabricate that ordering.

## Outcomes

| Case | Restart result | Exact current difference | Classification |
|---|---|---|---|
| park-made: state persisted, history not submitted | Disagrees; readable | `:no-history` for the park; replay lacks `:records`, `:index`, and `:coalesced` entries present in the authoritative file | History loses the park transition. The live promise remains in `/tmp`; no duplicate. |
| park-released/fulfilled: state persisted, first completion history call not submitted | Disagrees; readable | No chain issue, but replay retains the park `:records` and `:index` entries that `/tmp` removed | History permanently lags fulfilment/release. The authoritative state does not lose or duplicate an active promise. |
| followup-enqueued: state persisted, history not submitted | Disagrees; readable | `:no-history` for the followup; replay lacks `:queued` and `:dedupe` entries present in `/tmp` | History loses the enqueue transition. The live followup remains in `/tmp`; no duplicate. |
| truncated park file copied from a clean real-shaped case | Disagrees; parser returns empty state | Park loader catches the EDN error and silently substitutes empty state; replay still has the park's `:records`, `:index`, and `:coalesced` | The authoritative cache loses the active promise on restart, but comparison detects it. |
| truncated followup file copied from a clean real-shaped case | Restart cannot read state | `RuntimeException`, `Invalid token: :`; history remains readable | No silent promise classification is possible because followup initialization fails. |
| SIGKILL after `await-writes!` with an empty writer queue | Agrees; readable | `:issues []`, `:differences []` | Control passes. |

The generated UUIDs vary; tests pin the reason and exact differing state paths,
not a particular identifier.

## Proposed repairs (not implemented)

### Persist/history gap

Changing to history-before-state merely reverses the vulnerable window. The
smallest complete repair while retaining `/tmp` authority is a durable outbox
inside the authoritative state transaction: persist the state plus the exact
pending history payload atomically, drain that outbox to evidence, then
atomically mark/remove the delivered item. Restart must drain the outbox before
declaring comparison complete. A stable transition/idempotency id makes a crash
after evidence append but before outbox acknowledgement harmless.

This applies to park-made, park release/fulfilment, and followup enqueue. It
does not switch authority to history.

### Truncated state files

Both state writers should write a sibling temporary file, flush it, and replace
the target with `Files.move(..., ATOMIC_MOVE, REPLACE_EXISTING)`, with directory
sync where the platform supports it. The park loader should also stop silently
turning parse failure into an empty authoritative state; retain the last valid
file or fail startup with a typed corruption result. Followup already fails
loudly but still needs atomic replacement to avoid the corruption.

## Running

```sh
clojure -M:test -n futon3c.agency.promise-crash-test
clojure -M:test -n futon3c.agency.promise-replay-test
```

Observed crash-suite runtime was about 26 seconds. No production fix, reload,
or live write was performed.
