# APM transport conformance programme

Status: active; campaign quiesced before Park 1.

This programme recovers the transport/publication specification from the
mostly-working Clojure apparatus and its durable traces.  It is not a greenfield
redesign.  Successful traces are positive examples; f63 and f64 are preserved
counterexamples.  The Lean model, Clojure implementation, and run-certificate
validator must describe the same transition system.

## Linear park chain

| Park | Work | Exit evidence | Next wake |
|---|---|---|---|
| P1 | Recover current states, observations, decisions, and certificates | Trace-to-transition inventory for f63/f64 | Inventory reviewed and internally consistent |
| P2 | Formalize the recovered transition system in Lean | Compiled model plus classification, preservation, and progress theorems | Lean gates and axiom audit pass |
| P3 | Align Clojure with the model's wire vocabulary | Typed boundary results and conformance tests | Clojure suites and generated-vocabulary comparison pass |
| P4 | Emit and validate transition certificates | Replay validator accepts conformant traces and rejects mutations | Certificate fixtures pass on both surfaces |
| P5 | Repair demonstrated nonconformances | f63/f64 regression fixtures pass without weakening fail-closed checks | Full offline qualification passes |
| P6 | Run a stopped-machine canary, then resume | Canary certificate validates; watcher and durable state agree | Campaign restart authorized |

No later park begins before the prior exit evidence exists.  The campaign stays
stopped through P1--P5 and is restarted only at P6.

P4 is implemented at the promotion publication boundary. Each attempt is
validated and atomically appended under the promotion state's sibling
`transport-certificates/` directory; invalid records remain durable evidence.
`scripts/apm-replay-transport-certificates.clj` revalidates the directory and
its retry sequence offline. P5 remains responsible for correcting the f63
visibility-lag classification and the f64 regression policy.

## P5 repair evidence

Implemented 2026-08-30. Memory-snapshot visibility now returns an obtained
Boolean separately from a typed failure to obtain that Boolean. Callback
timeouts and unavailability retain their operation, acquired outcome, evidence
validity, candidate count, and parallel-execution fact; an obtained `false`
continues to produce `:memory-snapshot-review-not-visible` and is never retried
as transport.

Promotion publication carries typed observation failures into the existing
bounded delayed retry state without replacing the last valid state. History
retains the concrete snapshot failure and observation classification. Before a
certified state is persisted, its transition certificate is validated against
the source/loaded identity and decision model. Invalid certificates remain
diagnostic artifacts but produce `:transport-certificate-nonconformant` and an
apparatus hold instead of advancement.

The projection watchdog reports a quiescent delayed retry as `:waiting`, with
wake, attempt/maximum, and last failure. It suppresses pre-wake liveness noise
but no longer labels that state healthy-running. The f63 exception/later-visible
and f64 timeout/retry/success shapes are pinned by the focused suites and the
append-only certificate replay fixtures. P6 still owns the stopped-machine
canary, namespace reload, loaded-resource confirmation, and campaign restart.

## P1 recovered semantics

### Existing intended path

1. A role produces a promotion candidate.
2. An independent review is persisted.
3. The controller projects the reviewed attachment into the substrate.
4. Snapshot publication freshly verifies memory, review, and current attachment.
5. A successful snapshot is atomically written and read back.
6. A transport-classified failure preserves the last valid state and schedules
   a bounded delayed retry.  The reviewer is not redispatched.
7. Retry exhaustion parks the frame with its evidence and history preserved.

### f63 counterexample

The two memories and their reviews were eventually present, but fresh snapshot
visibility returned `:memory-snapshot-review-not-visible`.  In
`memory-snapshot/publish-cumulative!`, every exception from the visibility
function was caught and converted to Boolean false.  This erased the difference
between an authoritative negative observation and failure to obtain an
observation.  The generated contract marks evidence failure non-retryable, so
f63 exhausted its single publication repair and parked.

### f64 counterexample

The review-edge projection timed out and retained
`:error/component :transport`; `live-promotion/transport-failure?` therefore
scheduled a bounded delayed retry.  The retry history records attempt 0 failing
with `:promotion-review-projection-failed` and attempt 1 succeeding.  During the
delay the projection watchdog suppressed ordinary liveness checks and reported
the frame healthy although no agent was active.  The running JVM also retained
old sequential cascade and snapshot implementations after the parallel repairs
had been committed, causing aggregate latency not represented by the per-read
timeout.

### First recovered invariants

1. Failure to obtain an observation is not evidence of absence.
2. A successful negative observation, malformed evidence, visibility lag, read
   transport failure, and write transport failure are distinct outcomes.
3. Every retry state has a bounded attempt count, a wake time, retained last
   valid state, and an observable retry history.
4. A waiting retry is quiescent but not healthy-running; monitoring must expose
   its wake and last failure.
5. Aggregate work has a bound independent of candidate count, or the bound and
   candidate count are both certified.
6. A run certificate identifies both source implementation SHA and loaded SHA.
7. Park, retry, advance, and close decisions must be justified by a constructor
   in the shared typed outcome vocabulary.

### Recovered outcome axes

The model must not use one flat `transport?` Boolean.  At minimum it records:

- operation: read, write, publication, or post-publication verification;
- result: success, timeout, unavailable, malformed, authoritative absence, or
  visibility lag;
- evidence status: obtained, not obtained, or invalid;
- retry state: attempt, maximum, delay, wake, and history;
- implementation identity: specified, source, and loaded versions.

P2 will formalize these recovered distinctions without adding behavior not
witnessed by the implementation or an explicitly registered repair decision.
