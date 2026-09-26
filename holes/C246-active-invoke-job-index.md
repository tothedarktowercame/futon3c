# C246 — active invoke-job read index

Date: 2026-08-31

## Repair

`active-invoke-job-counts` now reads a process-local set of active job IDs and
looks up only those jobs. Its warm cost is proportional to active jobs (101 in
the C243 sample), rather than all durable history (6,195 jobs).

The index is deliberately derived rather than durable. On restart it is built
from the authoritative ledger after inflight recovery. Existing lifecycle
mutation helpers rebuild it from the committed in-memory ledger value; stream
events, which cannot alter lifecycle state, carry the same ID set to the new
immutable ledger value without scanning history. If any direct or concurrent
mutation makes the cached ledger identity differ, the next reader rebuilds
before answering. Thus a stale index is never accepted merely because one
exists.

This choice keeps the durable format and recovery semantics unchanged. Its
maintenance cost is one history scan after a lifecycle mutation (about 3.9 ms
over the C243 population), while the existing full-map serialization remains
roughly 2.2 seconds. An incremental transition index could remove that small
write-side scan later, but would add more correctness surface without changing
today's dominant write cost.

## Falsifier

`active-invoke-job-counts-consistency` computes indexed and authoritative
full-scan results at the same observation time and reports both maps plus the
indexed/history population sizes. Sharing the time is necessary because
delivered-job age otherwise advances between sequential calls and creates a
false mismatch.

The focused control uses 500 terminal jobs and three active jobs, verifies
restart reconstruction, changes one active job to terminal through the normal
mutation helper, and verifies equality again.

Canonical invocation:

```sh
clojure -M:test -n futon3c.transport.active-invoke-index-test
```

Result: exit 0, 1 test, 4 assertions, 0 failures, 0 errors.

`clj-kondo` and `check-parens.el` also exit 0 on the touched source and test.

## Explicit non-changes

- No ledger records were pruned or archived.
- No publication was made asynchronous.
- No durable field or ledger version was added.
- The monolithic full-map rewrite on mutation is unchanged. It should be the
  next separately commissioned case because changing it changes atomicity,
  recovery, and shutdown durability rather than merely query cost.
