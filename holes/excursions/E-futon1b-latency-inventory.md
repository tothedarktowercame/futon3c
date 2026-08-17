# E-futon1b latency inventory

Measured 2026-08-17 by codex-5. Discovery only: no production source was
changed and no service was restarted. The multi-watcher remained stopped.

This inventory hunts one specific compound failure shape: an unindexed table
scan below a bounded-looking loop. It deliberately does not repeat the known
`fetch-entity` / `retract-flexiarg!` finding that motivated the hunt.

## Active-mission audit

After the first inventory commit, the report was checked against the nine
commits on `futon1b` branch `origin/master-dionysus-20260817` at
`1d98a0036a5b7c0d860db22c3bb0821f45ecf995`: its BEFORE leg, mission,
candidate-index contract, source diffs, and types-cleanup excursion. Labels
below mean:

- **SURVIVES** — not measured or described by that mission/branch;
- **ALREADY-KNOWN** — the branch already establishes the result;
- **OBSOLETED** — the branch contains a source fix, even if the older running
  process still exhibits the condition.

The BEFORE leg identifies a different service PID (`900542`) from this host's
live PID (`2280223`). None of its absolute timings are compared with mine.

## Result summary, ordered by scan-count product

| result | full scans per top-level operation | quiet measurement | loaded measurement |
|---|---:|---:|---:|
| **SURVIVES — confirmed:** paginated pattern entity census recomputes a full type count on every page | **8** at current counts | 287 ms for the extra count scan; **>=2.296 s** lower bound for eight scans | 10.74–13.24 s per page; **~43–53 s** for four pages |
| **OBSOLETED in source; observable in old live process:** projection rebuild dies when the watermark moves | up to 40 rebuild opportunities for a 40-edge repoint, but observed count not instrumented | cached projection read 1.18–1.84 ms; rebuild itself not isolated | three of four 40-edge repoints previously produced the watermark-moved 503; one observed 503 occupied ~19 s |
| **SURVIVES — killed hypothesis:** the global memory-projection mutation lock is itself an 8x multiplier | lock acquisitions = 8, but no eightfold wall-time product | three no-op mutations: 35.8 ms, 73.8 ms, 1.270 s | 8 concurrent no-ops: each 17.03–17.28 s, batch wall 17.28 s, not ~136 s |
| **ALREADY-KNOWN — killed hypothesis:** other single-id GET routes share `fetch-entity`'s name-scan fallback | 1 point query per id (evidence chains loop, but remain point queries) | implementation uses `xt/id` equality | no scan signature found; loaded point/no-op read can still inherit store-wide delay |

## SURVIVES — entity pagination pays a second full scan on every page

The Dionysus mission targets evidence content/attribute candidate selection
and re-check. It does not mention `entities-query`, the pattern entity census,
or the redundant per-page entity count. Neither its graph/server diff nor its
BEFORE leg changes or measures this path. This is the one positive finding in
this inventory that survives the active mission audit.

### Observed anomaly

The pattern census is a human-facing operation used during the math-library
split/retraction work. It was expected to be a bounded paginated read, but a
quiet scan component previously measured in hundreds of milliseconds while a
loaded page took 10–13 seconds today.

### Call path, scan, and loop

1. `futon3c.watcher.multi/fetch-pattern-entity-ids` calls `fetch-type` for
   `pattern/library` and `pattern/clause`.
2. `fetch-type` loops over `/api/alpha/entities?type=...&limit=5000`, following
   `:next-cursor` until empty
   (`futon3c@1ce6e282cf7482c04cb8b03f458307d331f50647`,
   `src/futon3c/watcher/multi.clj:581-620`).
3. `futon1b@5838929efe59183c623706493c3e7520bef33923`,
   `futon1b_server.clj:539`, dispatches each page to
   `futon1b-graph/entities-query`.
4. `entities-query` (`futon1b@5838929efe59183c623706493c3e7520bef33923`,
   `futon1b_graph.clj:361-386`) executes **two** queries from
   `:entities` per page: the requested ordered page and a separate unbounded
   type-total query. The latter exists only to return the same `:count` on
   every page.

Current counts were `pattern/library=1,353` and `pattern/clause=10,181`.
At the hard page cap of 5,000 that is one library page plus three clause pages:
four page calls x two entity-table scans = **eight scans** per
`fetch-pattern-entity-ids` call. The loop looks like four harmless HTTP calls;
the endpoint looks like one harmless page; their product is the trap.

### Measurements

Quiet anchor: the pre-existing isolated measurement recorded alongside the
pagination implementation was **287 ms for the extra full type-count scan**
(`futon3c@0ba49db4186bd1d9dfc86f66732ca6cee86f30d4`,
`holes/excursions/E-apm-A3-ingest-efficiency.md`, H4). Eight such scans are a conservative
**2.296 s lower bound**; it excludes page materialization and HTTP transfer.

Under shared write load, the same first-page calls measured:

```text
pattern/library page 1: 13.235518 s, 312,029 bytes, count 1,353
pattern/clause  page 1: 11.487706 s, 3,071,776 bytes, count 10,181
pattern/library repeats: 10.900732 s, 10.743967 s, 11.809412 s
```

At four pages, the measured loaded range projects to **42.98–52.94 seconds**
for the top-level two-type census. Severity is eight unindexed scans, not four
HTTP calls. The 37–46x ratio is between a loaded full-page call and the quiet
count-scan component, **not** a same-call comparison; it is retained only as a
lower-bound contrast. The repeated identical loaded page calls establish the
10.74–13.24 s regime without pretending the cross-host Dionysus BEFORE
numbers are comparable.

### Reproduction

Read-only; safe against the live store:

```bash
curl -sS -w 'wall_s=%{time_total} bytes=%{size_download}\n' \
  -o /tmp/pattern-library-page.edn \
  'http://127.0.0.1:7073/api/alpha/entities?type=pattern%2Flibrary&limit=5000'

curl -sS -w 'wall_s=%{time_total} bytes=%{size_download}\n' \
  -o /tmp/pattern-clause-page.edn \
  'http://127.0.0.1:7073/api/alpha/entities?type=pattern%2Fclause&limit=5000'

rg -o ':count [0-9]+|:next-cursor "[^"]+' /tmp/pattern-*-page.edn
```

Follow each emitted cursor with `&after=<URL-encoded cursor>` to reproduce the
four-page top-level operation. The source proof is the two `fxt/safe-q` calls
inside `entities-query`; no timing inference is needed to establish scan count.

## OBSOLETED in source — watermark-moved rebuild failure

This is not a surviving finding. Commit
`futon1b@1755d13ddd8dbfa26d005d6b4a111047e02b55ec` retries the complete
selection/hydration build when its watermark moves (up to five attempts)
instead of dying on the first movement. The branch mission also records the
root observation: 295 consecutive failures on a quiet 22 GB Dionysus store.

`refresh-memory-projection-component!` normally does bounded point hydration.
If the source watermark moves during that window, it calls
`initialize-memory-projection!`, which selects every current `memory/assert`
id, hydrates every selected component, and runs under the global projection
lock. A 40-edge repoint calls the refresh path 40 times. Thus the static upper
shape is 40 opportunities to rebuild, but it would be dishonest to report 40
actual full scans without runtime instrumentation.

Measured negative/positive boundary:

```text
quiet cached POST /api/alpha/memory/projection: 1.184–1.840 ms
loaded failed rebuild observed during this work: ~19 s, HTTP 503
reason: :memory-projection-source-moved-after-quiescence
prior observed batch: 3 of 4 forty-edge repoints provoked the same reason
```

This host nevertheless still exhibits the old failure because PID 2280223
started **2026-08-14 10:17:24**, before commit `1755d13` landed at
**2026-08-14 17:06:40 +01:00**. Thus the runtime numbers are valid observations
of stale deployed code, not evidence of an unfixed source defect. This rejects
"ordinary cached projection reads are slow," but creates no new fix packet.

Read-only reproduction for the fast side:

```bash
curl -sS -w 'http=%{http_code} wall_s=%{time_total}\n' \
  -o /tmp/projection.edn -X POST -H 'Content-Type: application/edn' \
  --data '{:endpoints ["t00A05"] :limit 50}' \
  http://127.0.0.1:7073/api/alpha/memory/projection
```

## SURVIVES — killed hypothesis: the global lock is not an 8x multiplier

The lead was literal: `with-memory-projection-mutation` is `(locking node
(f))` at `futon1b@5838929efe59183c623706493c3e7520bef33923`,
`futon1b_graph.clj:171-174`. To measure lock cost without changing a row, I fetched an existing
`memory/assert` edge and POSTed the identical edge. The server returned
`:no-op? true`; this is a throwaway timing action with no store mutation.

Sequential no-ops measured 73.8 ms, 35.8 ms, then 1.270 s as load rose.
During the loaded regime, eight concurrent identical no-ops each reported
17.03–17.28 s, while the whole batch completed in 17.28 s. If the lock were
serializing eight equally expensive critical sections, batch wall time would
have approached 136 seconds. Instead one slow critical section dominated and
the queued no-ops drained within ~245 ms. The global lock can add queueing, but
it is not the scan-times-loop product responsible for the observed minutes.

Reproduction (the payload must be an exact existing edge):

```bash
seq 1 8 | /usr/bin/time -f 'batch_wall_s=%e' \
  xargs -P 8 -I{} curl -sS -w 'item={} wall_s=%{time_total}\n' \
  -o /tmp/noop-{}.edn -X POST -H 'X-Penholder: api' \
  -H 'Content-Type: application/edn' --data-binary @/tmp/existing-edge.edn \
  http://127.0.0.1:7073/api/alpha/hyperedge
```

Verify every response contains `:no-op? true` before accepting this result.

The Dionysus types-cleanup note mentions this lock as a safety boundary, but
does not measure contention or identify it as a latency multiplier. The
negative measurement therefore survives rather than duplicating that mission.

## ALREADY-KNOWN — killed hypothesis: single-id routes are point reads

The untested hunch was that other single-id routes resolve like
`fetch-entity`. After the measurements above identified the candidate surface,
source inspection found:

- `GET /api/alpha/hyperedge/{id}` -> `hyperedge-by-id` -> one `xt/id`
  equality query (`futon1b@5838929efe59183c623706493c3e7520bef33923`,
  `futon1b_graph.clj:591-599`).
- `GET /api/alpha/evidence/{id}` -> `fetch-by-id` -> one `xt/id` equality
  query (`futon1b@5838929efe59183c623706493c3e7520bef33923`,
  `futon1b_evidence.clj:80-82`).
- evidence `/chain` loops, but each iteration is that point lookup; it does not
  scan by name or external id
  (`futon1b@5838929efe59183c623706493c3e7520bef33923`,
  `futon1b_evidence.clj:401-414`).

These forms use `(from ... [*])` syntactically, but the `xt/id` predicate is
the indexed point path; this is exactly why code shape alone was not used as a
finding. Loaded point reads can still stall behind store work—the no-op probe
spread from 35.8 ms to 17 s—but there is no unindexed-scan x item-loop
signature here.

This is already part of the Dionysus mission's established cost model:
`futon1b@1d98a0036a5b7c0d860db22c3bb0821f45ecf995`,
`holes/M-evidence-landscape-index.md` ("Facts on the ground") records indexed
single-`xt/id` lookup as the accepted point path and contrasts it with the
40-second multi-id disjunction. The inventory adds no new finding here.

## Measurement limitation: runtime topology disagreed with the packet

Drawbridge `:6768` reached PID 2286732 (futon3c), but live substrate `:7073`
was PID 2280223 (`clojure.main -m futon1b-server`) and
`futon1b-server/!node` inside the Drawbridge JVM was `nil`. A wrapped direct
probe returned `next.jdbc ... Connectable ... nil`, confirming that timing
futon1b internals through that `/eval` would measure no live node. I did not
restart or rewire either process. Component timing therefore used the public
HTTP boundary plus the already-recorded 287 ms isolated count scan. Exact
process check:

```bash
ss -ltnp | grep -E '7073|6768'
ps -fp "$(ss -ltnp | sed -n 's/.*:7073 .*pid=\([0-9]*\).*/\1/p')"
```

That topology discrepancy matters for reproducibility and should be resolved
before a follow-up packet requires live-JVM private-var instrumentation.
