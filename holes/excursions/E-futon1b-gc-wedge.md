# E-futon1b GC wedge: discovery and proposed repair

Date: 2026-08-23 UTC  
Scope: discovery only. PID 1082659 was not signalled, killed, restarted, or
otherwise repaired during this investigation.

## Ruling

This is not a semaphore leak and it is not evidence that `-Xmx4g` has become
intrinsically too small for the corpus. The two permits are held by two real
HTTP reads which entered `with-expensive-read!` about 40 seconds after this JVM
started and never returned. Both are blocked waiting for XTDB's internal
pgwire response while executing the evidence keyset-page query. Later reads
wait three seconds on the fair semaphore and shed correctly.

The retained Java heap belongs principally to XTDB/Arrow schema and page
metadata. It is not a 3.7 GB Clojure response vector: after a live-object
histogram, 9,307,005 `org.apache.arrow.vector.types.pojo.Field` objects remain,
with 9.45 million `ArrayList`s, 9.31 million unmodifiable lists, 5.81 million
`FieldType`s, 517,810 `xtdb.metadata.PageMetadata$PageIndexKey`s, and thousands
of live XTDB scan/parser/plan objects. The direct Arrow buffers themselves are
small in the Java histogram. This is repeated Arrow field-tree metadata for
XTDB pages/plans.

The allocation/query seam implicated by both the object population and the
live stacks is `futon1b/futon1b_evidence.clj:201-223`,
`fetch-newest-projected-page`. It constructs a new XTQL form for every keyset
page with cursor values embedded as literals, and projects the nested
`evidence/subject` union on every page. `bounded-window` repeatedly calls it
for post-filtered reads. The accompanying thousands of `DynamicClassLoader`,
ANTLR SQL parser and generated XTDB scan-function instances are consistent
with thousands of distinct compiled query shapes rather than one stable
parameterised plan. A dominator path would require a heap dump; none was taken
on this shared, memory-pressured service. The histogram establishes the
retained owner (XTDB Arrow field/page metadata), and the two live stacks
establish the currently wedged producer/query shape.

## Evidence from the running JVM

### Heap ownership

Command (completed successfully):

```text
$ jcmd 1082659 GC.class_histogram
1082659:
 num     #instances         #bytes  class name (module)
-------------------------------------------------------
   1:       1103013     1161480808  [B (java.base@21.0.11)
   2:      11554197      537467616  [Ljava.lang.Object; (java.base@21.0.11)
   3:         61190      391361112  [Ljdk.internal.vm.FillerElement; (java.base@21.0.11)
   4:       9453435      226882440  java.util.ArrayList (java.base@21.0.11)
   5:       9308448      223402752  java.util.Collections$UnmodifiableRandomAccessList (java.base@21.0.11)
   6:       9307005      223368120  org.apache.arrow.vector.types.pojo.Field
   7:       5808224      185863168  org.apache.arrow.vector.types.pojo.FieldType
   8:       3691922      118141504  clojure.lang.Symbol
   9:       4865178      116764272  clojure.lang.PersistentHashMap$BitmapIndexedNode
  10:       2573606      102944240  clojure.lang.PersistentVector
  11:       2507890       80252480  clojure.lang.PersistentArrayMap
  12:       1221007       48840280  clojure.lang.PersistentHashMap
  13:       1182435       37837920  clojure.lang.PersistentHashSet
  14:       1102199       35270368  clojure.lang.MapEntry
  15:       1092110       26210640  java.lang.String (java.base@21.0.11)
  21:        517810       12427440  xtdb.metadata.PageMetadata$PageIndexKey
  38:         13668        1421472  clojure.lang.DynamicClassLoader
  49:          9972         717984  xtdb.operator.scan$eval21982$fn$reify__21987$fn__22063
  65:          5586         446880  xtdb.antlr.SqlLexer
Total      71340190     3751260896
```

`GC.class_histogram` reports live objects after its collection, so these are
retained, not merely the recent allocation rate. The object-array plus
`Field`/`FieldType`/list rows account for about 1.40 GB. Byte arrays contribute
another 1.16 GB overall, although a class histogram alone cannot assign every
byte array to its owning graph. The `Field`/`FieldType`/list triad is an
unambiguous Arrow schema object graph.

Command:

```text
$ jcmd 1082659 GC.heap_info
1082659:
 garbage-first heap   total 4194304K, used 3970740K [0x0000000700000000, 0x0000000800000000)
  region size 2048K, 132 young (270336K), 11 survivors (22528K)
 Metaspace       used 216047K, committed 476800K, reserved 3211264K
 class space    used 52548K, committed 151744K, reserved 1048576K
```

Command:

```text
$ jcmd 1082659 VM.flags
-XX:ConcGCThreads=6 -XX:+ExitOnOutOfMemoryError
-XX:MaxDirectMemorySize=3221225472 -XX:MaxHeapSize=4294967296
-XX:NativeMemoryTracking=summary -XX:+UseG1GC
```

Command:

```text
$ jcmd 1082659 VM.native_memory summary
Total: reserved=9238275KB, committed=6201875KB
       malloc: 1271355KB #2109386
       mmap:   reserved=7966920KB, committed=4930520KB
-                 Java Heap (reserved=4194304KB, committed=4194304KB)
-                     Other (reserved=834406KB, committed=834406KB)
```

Command:

```text
$ tr '\0' '\n' </proc/1082659/environ | rg 'MALLOC_ARENA_MAX|FUTON'
FUTON1B_PORT=7073
FUTON1B_STORE_DIR=migration-store-21
MALLOC_ARENA_MAX=2
FUTON1B_HEALTH_PORT=7072
```

This rules out the previously documented missing-`MALLOC_ARENA_MAX` failure.
The immediate pressure is the 3.97 GB Java heap, not an invisible glibc arena
rise.

### The two permits

Command (the full dump was inspected; these are the decisive frames):

```text
$ jcmd 1082659 Thread.print
"pool-2-thread-3" #177 [1083953] ... cpu=240183.11ms elapsed=344133.58s ... runnable
   java.lang.Thread.State: RUNNABLE
        at sun.nio.ch.Net.poll(Native Method)
        at org.postgresql.core.v3.QueryExecutorImpl.processResults(QueryExecutorImpl.java:2175)
        at org.postgresql.jdbc.PgPreparedStatement.execute(PgPreparedStatement.java:182)
        at xtdb.jdbc.XtConnection$XtPreparedStatement.execute(XtConnection.kt)
        at next.jdbc.result_set$reduce_stmt.invokeStatic(result_set.clj:724)
        at xtdb.api$plan_q$reify__4976$fn__4977.invoke(api.clj:124)
        at xtdb.api$q.invokeStatic(api.clj:172)
        at futon1b_xt$safe_q$fn__5356.invoke(futon1b_xt.clj:38)
        at futon1b_evidence$fetch_newest_projected_page.invokeStatic(futon1b_evidence.clj:218)
        at futon1b_evidence$bounded_window.invokeStatic(futon1b_evidence.clj:248)
        at futon1b_evidence$query_evidence_response.invokeStatic(futon1b_evidence.clj:343)
        at futon1b_server$evidence_route$fn__6350.invoke(futon1b_server.clj:476)
        at futon1b_server$with_expensive_read_BANG_.invokeStatic(futon1b_server.clj:410)

"pool-2-thread-4" #178 [1084033] ... cpu=242386.84ms elapsed=344131.40s ... runnable
   java.lang.Thread.State: RUNNABLE
        at sun.nio.ch.Net.poll(Native Method)
        at org.postgresql.core.v3.QueryExecutorImpl.processResults(QueryExecutorImpl.java:2175)
        at org.postgresql.jdbc.PgPreparedStatement.execute(PgPreparedStatement.java:182)
        at xtdb.jdbc.XtConnection$XtPreparedStatement.execute(XtConnection.kt)
        at xtdb.api$q.invokeStatic(api.clj:172)
        at futon1b_xt$safe_q$fn__5356.invoke(futon1b_xt.clj:38)
        at futon1b_evidence$fetch_newest_projected_page.invokeStatic(futon1b_evidence.clj:218)
        at futon1b_evidence$bounded_window.invokeStatic(futon1b_evidence.clj:248)
        at futon1b_evidence$query_evidence_response.invokeStatic(futon1b_evidence.clj:343)
        at futon1b_server$evidence_route$fn__6350.invoke(futon1b_server.clj:476)
        at futon1b_server$with_expensive_read_BANG_.invokeStatic(futon1b_server.clj:410)

"pool-2-thread-2" #176 [1083945] ... waiting on condition
   java.lang.Thread.State: TIMED_WAITING (parking)
        - parking to wait for (a java.util.concurrent.Semaphore$FairSync)
        at java.util.concurrent.Semaphore.tryAcquire(Semaphore.java:415)
        at futon1b_server$with_expensive_read_BANG_.invokeStatic(futon1b_server.clj:408)
        at futon1b_server$evidence_route.invokeStatic(futon1b_server.clj:472)
```

The permit holders are therefore genuine but permanently stalled work, not
lost releases. `finally (.release expensive-read-permit)` cannot run until
`xt/q` returns or throws. The dump also showed all named
`pgwire-connection--pool-1-thread-*` workers parked idle on their executor
queue while these client sockets wait for results. That is a broken query
lifecycle, not merely a scan that is slowly consuming CPU.

The full dump also records the collector saturation:

```text
"G1 Conc#5" ... cpu=46436226.56ms ... runnable
"G1 Conc#4" ... cpu=46441041.63ms ... runnable
"G1 Conc#3" ... cpu=46435573.35ms ... runnable
"G1 Conc#2" ... cpu=46441049.25ms ... runnable
"G1 Conc#1" ... cpu=46436406.34ms ... runnable
"G1 Conc#0" ... cpu=46435442.22ms ... runnable
```

## Proposed repair

This should be implemented as one recovery change-set, then deployed by a
Joe-approved restart. A code reload cannot recover the two existing JDBC calls
or release their permits.

1. **Make the evidence page query one bounded, parameterised shape.** In
   `futon1b/futon1b_evidence.clj`, replace `fetch-newest-projected-page`
   (currently lines 201-223) with parameterised SQL whose text is stable and
   whose cursor and limit are JDBC parameters. Provide fixed variants for
   “first page” and “after cursor”; do not embed each cursor value in a newly
   compiled XTQL form. Project scalar filter/sort columns by default. Project
   `evidence/subject` only when `subject-type` or `subject-id` post-filtering is
   requested. Keep the existing 1,000-row page ceiling and cursor ordering.

2. **Give every database read a real JDBC deadline.** Add a timed SQL helper in
   `futon1b/futon1b_xt.clj` beside `safe-q`, using next.jdbc's supported
   `:timeout` option (`PreparedStatement.setQueryTimeout`) and a connection
   network timeout slightly above it. Use it for the evidence projected page
   and hydration query first, then migrate the other expensive routes. Set the
   server-side query deadline to 60 seconds initially. A timeout must close or
   invalidate the connection, throw through `safe-q`, and reach
   `with-expensive-read!`'s `finally`; wrapping `xt/q` in a Clojure future is
   insufficient because JDBC socket reads need not honor interruption.

3. **Bound a whole request, not only each page.** In `bounded-window`, enforce
   a maximum number of scanned projected rows/pages for filters that cannot be
   pushed down. On exhaustion return a cursor plus an explicit incomplete/
   continuation indication rather than looping inside one HTTP request. Text
   search should continue to use the FTS sidecar to select ids and hydrate only
   its bounded result; it must not fall back to an unbounded evidence scan.

4. **Reject oversized hyperedge windows and stop caching them.** In
   `futon1b_server.clj:599-614`, reject `limit > 1000` with a 400 and require
   callers to advance `after`. In `futon1b_graph.clj:786-801`, cache only
   windows at or below that ceiling. The observed `limit=10000` request is
   pushed down before hydration, but it still means up to 10,000 point
   hydrations and a materialised response retained in the 32-entry cache. It
   is not the query holding the two permits in this dump, but it is an unsafe
   shape and can independently recreate pressure.

5. **Do not raise `-Xmx` as the repair.** `futon1b/README.md` records about
   976 MB live for 140,296 documents on 2026-08-17 and explicitly calls 4 GB
   ample. Here the live set is 3.75 GB because repeated XTDB/Arrow metadata is
   retained. A larger heap would postpone admission failure, lengthen GC, and
   allow more repeated field trees. After the parameterisation and deadline
   changes, restart once, replay representative evidence/hyperedge traffic,
   and measure `GC.class_histogram` and post-GC occupancy. Raise the ceiling
   only if that new bounded steady-state measurement demonstrates a genuinely
   larger corpus live set.

### Recovery sequence (proposal only; not performed)

1. Land and test the bounded/parameterised queries and JDBC cancellation.
2. Quiesce callers so they cannot immediately recreate the two scans.
3. Joe authorises a controlled restart of `futon1b`.
4. Verify cheap health, then one evidence page and one <=1,000 hyperedge page.
5. Confirm both semaphore permits return, post-GC heap is well below 4 GB, and
   no Arrow `Field` population grows monotonically across repeated pages.

## Make the next failure legible

`with-expensive-read!` should maintain an in-memory holder registry containing
request id, route, sanitized query shape, thread, acquisition time and scanned
page/row counts. Cheap `/health` should expose permit count, waiter count,
oldest-holder age and heap/GC pressure without taking a permit. Log one
structured start/finish/timeout record per admitted read and emit an alert when
both permits are held for 60 seconds or when rejection rate is nonzero for five
minutes. The existing vitality service should poll this cheap health surface,
not an HTML consumer, and alert on old holders, sustained >85% post-GC heap,
or continuous G1 concurrent CPU. These measurements distinguish overload,
hung JDBC, and a leaked permit without another thread dump.

## Non-actions

No runtime repair, signal, restart, heap dump, source-code change, or network
load test was performed. The only repository change from this investigation is
this report.

## Repair performed (2026-08-23, ams-claude session 012zxpti)

Code: futon1b `4cd17bc` (repair), `8aba53c` (validate hyperedge window
before the permit; metaspace in /health), `bf875b0` (API-CONTRACT).
Regression: `clojure -M:node -m test-evidence-deadline` (22 checks);
test-json, test-a1a2 (42/42), test-candidate-query, test-text-search all pass.

What landed, against the five proposals:

1. Page query is one `(fn [p-type … p-cursor-at p-cursor-id p-limit] …)`
   XTQL form — XTQL `limit` accepts a param (xtdb.xtql.plan 2.1.0) — so the
   compiled text is stable across cursors; two variants (first/after-cursor)
   × the set of present filters. `evidence/subject` is projected only for
   subject filters. `/count`'s scan and hydration share the same path.
2. `futon1b-xt/timed-q`: the node's own JDBC connection with
   `setNetworkTimeout` (timeout+5s) and `setQueryTimeout`. **Measured:
   XTDB 2.1.0 pgwire does not act on pgjdbc's cancel**, so the effective
   deadline is 65 s for the 60 s default; once the socket drops the
   server-side scan stops within ~3 s (probe: process CPU → 0, no operator
   threads). Expiry → 504 `:query-deadline-exceeded`, permit released.
3. `bounded-window` scans ≤ 20,000 projected rows per request; past that it
   returns `:incomplete true` + cursor. The wedged shape
   (`type=coordination&subject-type=portfolio&subject-id=global`, 157,336
   coordination rows) now answers in 14.8 s with 0 matches / 20,000 scanned
   instead of never.
4. `/hyperedges` `limit>1000` → 400 (outside the permit); cache only ≤ 1000.
5. `-Xmx` untouched.

Observability: holder registry + start/end/timeout log lines per admitted
read (`[futon1b-expensive-read]`); cheap `/health` carries permits, waiters,
holders with age, oldest-holder-ms, admission stats, heap, metaspace, GC.

### Restart — not the controlled one

While taking the before-census for `restart-futon1b-detached.sh`
(`/health?deep=true`, 240 s timeout) the old JVM (PID 1082659, 3.97 GB /
4 GB) hit `OutOfMemoryError` at 11:06:02 and `ExitOnOutOfMemoryError` ended
it. My census request was almost certainly the last straw. systemd restarted
the unit at 11:06:13 (PID 3639998) against source already on disk
(`4cd17bc` written 11:00:50), so the live service runs the repair;
`8aba53c`'s cosmetic change is not live until the next restart. The script's
before-census was unobtainable from the wedged JVM by construction — that is
a gap in the script for this failure mode, left as-is.

Post-restart verification (PID 3639998):
- `/health` up after ~35 s; permits 2/2, holders [].
- evidence `limit=100`: 1.38 s; hyperedges `limit=1000`: 28.8 s;
  `limit=10000`: 400; `/count?type=coordination`: 9.5 s → 157,336.
- `GC.class_histogram` across 18 distinct-cursor/filter pages: Arrow `Field`
  1,509,994 → 1,509,883 → 1,511,572 (flat; wedge had 9,307,005).
  `PageIndexKey` constant 361,146. Post-GC heap **993 MB** (README baseline
  ~976 MB). `DynamicClassLoader` 53,212 → 56,253 (~130/request) with
  Metaspace 471 MB used — the watch-metric, now in `/health`.
- `scripts/fts-status.py`: store 159,492 = index 159,492, delta +0.
  Fresh writes stamped 11:09–11:10 (write path confirmed).

### Follow-ups (not done)

- The futon3c caller issuing the wedged shape must honour `:incomplete` /
  `:next-cursor`; otherwise it will read 0 entries as "none". Better: push
  `subject-type`/`subject-id` down into the XTQL where on the nested map so
  the scan bound is rarely hit.
- `restart-futon1b-detached.sh` aborts when the before-census cannot be
  taken, which is exactly the wedged state; decide whether an explicit
  opt-in (`…ALLOW_NO_CENSUS`) is acceptable.
- Watch `:metaspace-used-mb` for monotonic growth.
