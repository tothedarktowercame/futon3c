# F225 slow-read repair: use the maintained memory attachment projection

The complete hold and its two warnings are retained in `diagnostic.edn`.
F225/m93J03 already completed; none of this work reruns or republishes its proof.

## Diagnosis

The two wire trace IDs match `incident-server.log`. Both had zero HTTP executor
admission wait. The first took 6,024 ms inside the expensive-read callback
(6,232 ms server total); the second took 3,146 ms there, with approximately
2,859 ms elsewhere, principally waiting to enter that callback. Both callbacks
started with four expensive readers active. Client totals were 6,248/6,007 ms.
These records distinguish HTTP queueing from database admission/query work;
they do not identify a particular competing agent or an XTDB GC pause.

The exact URL shape comes from the default `memory-snapshot/candidate-visible?`
lookup, which checked every inherited memory using a general endpoint query.
F225's starting solver snapshot contains 408 memories. The general query scans
hyperedge endpoint membership and hydrates selected rows. Four concurrent
visibility checks repeatedly consume the expensive-read permits.

## Repair

Snapshot visibility now requests memory attachments through the already serving
`/api/alpha/memory/projection`. Futon1b maintains this current-state index
synchronously on attachment writes and retractions, with revision/generation
tracking; it is not a TTL cache. Its endpoint groups retain full edge properties
and deterministic edge-ID ordering. The consumer rejects malformed, historical,
wrong-endpoint, unversioned and potentially truncated projection responses.
It still fetches the full memory and review evidence separately and applies all
existing current-state, reviewed-status, pattern, reviewer, author, subject,
reason and residual checks. No compact evidence body substitutes for those reads.

The projection uses a read-only POST. That client previously lacked read-health
observation. GET and read-only POST now share the same asynchronous whole-response
HTTP deadline, tracing and warning observation. The 5,000 ms warning threshold
and 30,000 ms frame HTTP deadline are unchanged. Returned slow evidence still
gets checked; failed reads still fail. No generic endpoint cache or scan fallback
was added. At 100 returned attachments the consumer refuses possible truncation.

This repairs the query choice at the actual snapshot consumer. It does not claim
that arbitrary XTDB endpoint scans or all possible concurrent workloads are fast.
Other generic endpoint consumers retain their existing behavior and monitoring.

## Executed validation

`probe.clj` ran in an isolated client JVM against the existing store, using only
read APIs. It compared the two exact affected endpoint lookups, including edge
IDs, types, endpoints and all properties, and asserted complete baseline results.
The original requests took 1,506/1,460 ms under diagnostic load; their replacement
lookups took 45/42 ms. Eight full default visibility checks in two waves of four
returned true in 168–346 ms; no warnings were produced. `diagnostic.edn` is the
machine-readable result. The concurrent console output in `probe.log` has some
interleaved lines; the EDN atom-backed receipt is intact and read back successfully.
The reproduction script now serializes console printing; measured logic is unchanged.
This bounded test is not a claim to have reproduced the earlier peak workload.

`freshness.clj` ran from futon1b in a disposable in-process XTDB node, not either
live service. It executed the existing generation-on-write test, then changed a
reviewed attachment to proposed and retracted it. Fresh projections reflected
both changes, with advancing generation: 10 assertions passed. Futon1b inspected
source pin: b454d0fa7ceeddcec96bb41fe13e645be8220e26. No futon1b source or service
configuration was changed.

Sequential futon3c namespace tests: client 5/30; read-health 5/46;
memory-snapshot 24/112; memory-recall 17/70 — 51 tests, 258 assertions passing.
The new snapshot test initially stubbed the obsolete evidence protocol rather
than the bounded HTTP boundary and asserted false where the contract permits nil;
that failed run is retained. The corrected tests stub the actual bounded read,
verify separate memory/review requests, and test all existing rejection gates.
The pre-existing default lookup test was corrected to use that same actual port.
Lint, Emacs check-parens and diff whitespace checks pass.

Reproduce from futon3c: `clojure -M <packet>/probe.clj` while this exact hold exists;
from futon1b: `clojure -M:node <absolute-packet>/freshness.clj`.
Run each test namespace with `clojure -M:test -n <namespace>` individually.

Deployment and exact hold release/resume receipts will be retained separately.
The repair is deployed only by reloading the two canonical futon3c namespaces.
No JVM, V2, topology or completed proof restart is required.

## Executed deployment and queue release

Repair commit `17dca50e3ec80850cc71b1a2f0ccbb5c95804d7a` was loaded at
20:31:38Z from the two canonical classpath files. `live-validation.edn` records
both exact candidates passing full visibility in 219/186 ms, without warnings.
`release.edn` records successful release of the exact commissioned hold through
`release-store-read-hold!`, with no independent pause restored.

One `durable-coordinator/resume!` call returned `:started`. Its printed result
included a live promise and the full coordinator history, so it is not readable
EDN; the exact bytes are retained in the local path with SHA256 identified in
`resume-summary.json`. The summary extracts only the observed start acknowledgement;
no second resume call was made to obtain a different receipt.

`resumed-observation.edn`, read from durable files at 20:33:06Z, confirms F226 /
m93J06 active, coordinator running at tick 78,335 (previously 78,322), hold absent,
and exact warning and hold identity retained in `:store-read/repairs`. F225's
original terminal receipt remains the most recent completed record. This confirms
queue resumption, not completion of the next frame or elimination of every store
performance risk. No JVM, V2 or topology loop was restarted.
