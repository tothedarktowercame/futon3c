# XTDB 2 store: memory and latency measurements, 2026-07-30

Measurements taken from a single long-running futon1b XTDB 2 instance immediately
**before** an operator-initiated restart, so the degraded state is captured rather
than lost. Recorded for the JUXT conversation on Wed 2026-08-05.

**Status: observations, not a diagnosis.** One instance, one uncontrolled
workload, no isolation of cause, no comparison build. Everything below is "what
this process did today", and the confounds are listed at the end. Nothing here
should be read as a defect claim against XTDB 2.

## The instance

| | |
|---|---|
| Process | `java -Xmx4g` with `--add-opens java.base/java.nio=org.apache.arrow.memory.core` |
| Started | Wed 2026-07-29 08:05:31 |
| Uptime at measurement | 1 day 06:13 |
| Serves | evidence/entity/hyperedge HTTP API on :7073 (also :7072) |
| Threads | 126 |

Workload over the measured window was an agent loop: a few hundred evidence
writes (memory entries, receipts, turn-round records averaging 2–16 KB, a few at
40 KB), by-id reads, `text-search` queries, and a number of `?limit=N` list
reads. Modest by database standards — this is not a load test.

## Memory: growth over ~4.75 hours

Two snapshots, same process, same day.

| metric | 09:30Z | 14:15Z | change |
|---|---|---|---|
| RSS | 4.98 GB | **7.76 GB** | +2.78 GB |
| VmSize | — | 10.99 GB | |
| G1 heap total | 2.33 GB | 4.19 GB | grown to the `-Xmx4g` ceiling |
| G1 heap used | 845 MB | 2.07 GB | +1.2 GB |
| **Metaspace used** | **331 MB** | **957 MB** | **×2.9** |
| Metaspace committed | 482 MB | 1.65 GB | ×3.4 |
| Metaspace reserved | 1.41 GB | 2.23 GB | |
| Class space used | 96 MB | 336 MB | ×3.5 |
| Young GCs | 832 | 1515 | +683 |
| Total GC time | 43.4 s | 173.0 s | +129.6 s |
| Full GCs | 0 | **0** | — |

Two things stand out.

**1. Metaspace tripled.** 331 MB → 957 MB used, 482 MB → 1.65 GB committed, in
under five hours of moderate query load. Class space alone went 96 MB → 336 MB.
Growth in *class* metadata rather than object heap is the signature of classes
being generated and retained — in a Clojure system the obvious candidate is
per-query or per-request compilation, where each distinct query shape produces
fresh classes that are never unloaded. We have not confirmed that mechanism; it
is the hypothesis the numbers suggest, and it is the question most worth putting
to the XTDB team.

**2. RSS exceeds `-Xmx` by 3.76 GB, and only part is explained.** The largest
anonymous mapping is 3.96 GB (the heap reservation); metaspace committed accounts
for a further 1.65 GB; the next largest single mapping is 351 MB. Arrow off-heap
buffers are expected here and are not by themselves surprising. The point for
capacity planning is that `-Xmx4g` sizes less than half of this process.

**GC is not the problem.** Zero full GCs across 30 hours, and 173 s of total GC
against 30 h of uptime is ~0.16 %. An earlier hypothesis that the store was
GC-thrashing was refuted by these numbers. Note however that 130 s of that 173 s
accrued in the last 4.75 hours — the GC *rate* rose sharply as metaspace grew.

## Latency: by-id is flat, list scans degrade

Same endpoints, same instance, at the two snapshots.

| operation | 09:30Z | 14:15Z | change |
|---|---|---|---|
| `GET /evidence/<id>` (by id) | 0.18 s | 0.18 s | flat |
| `GET /entity/<id>` (by id) | 0.25 s | 0.40 s | |
| `GET /evidence/<missing-id>` (404) | 0.20 s | 0.31 s | |
| `GET /evidence?limit=1` | 1.76 s | **2.57 s** | +46 % |
| `GET /evidence?limit=50` | — | 8.73 s | |
| `GET /evidence?limit=100` | 9.36 s | — | |
| `GET /evidence?limit=200` | 12.45 s | **23.96 s** | **+92 %** |
| `POST /evidence` (small) | 0.10 s | 0.18 s | |
| `POST /evidence` (40 KB) | 0.11 s | — | |
| `POST /hyperedge` | 0.42 s | — | |

The shape is consistent across both snapshots and is the practically important
finding for us:

- **Point lookups by id are fast and stayed fast** — 0.18 s, unchanged across the
  memory growth.
- **Writes are fast and roughly size-independent** — a 40 KB document posts in
  the same time as a tiny one.
- **List reads carry a large constant and scale with `limit`.** Even `limit=1`
  costs 1.8–2.6 s, which suggests the cost is in establishing the scan rather
  than in returning rows. And the per-row slope roughly doubled over the session
  while point lookups did not move.

## What we changed on our side

The degradation shaped our own code, and the fix is worth reporting because it
was a client-side error rather than a store defect: our recall path was issuing a
single conjunctive `text-search` query built from up to 36 terms. Measured hit
counts against term count on this instance: 1 term → 5 hits, 3 → 3, 7 → 2, 12 →
1, 29 → **0**. Long conjunctions reliably returned nothing. We now cap at 3 terms
and fall back through 2-term pairs to singles. Separately we stopped issuing
`?limit=400`/`?limit=500` diagnostic queries once they were measured at 25–40 s
each.

## One unexplained event

During a batch of ~36 writes at ~10:05Z, a single `POST /evidence` exceeded a 30 s
client timeout, leaving a half-applied batch. It was **not** reproduced: small and
40 KB writes both returned in ~0.1–0.18 s before and after. Reads at the time were
1.8–2.6 s, so worker contention during a multi-second scan is a plausible but
unverified explanation. We raised the client POST timeout to 120 s. Recovery was
possible only because the writer uses deterministic ids and an existence check, so
re-running resumed cleanly — a client-side property, not something the store did.

## Confounds and limits

- **Single instance, single workload, no control.** No A/B, no comparison build,
  no isolation of which query shapes drive metaspace growth.
- **The final measurement was taken under machine-wide memory pressure**: an
  unrelated batch sweep (7 JVMs, ~7.9 GB) was running concurrently, with the host
  at 23/30 GB used and 549 MB free. The 14:15Z latency figures are therefore an
  upper bound and the comparison to 09:30Z is not clean. The *memory* figures for
  the store process are unaffected by this.
- **Corpus size was not measured**, so the list-scan cost has no denominator here.
- We have not tested whether restarting restores the latency profile; the restart
  that follows these measurements is the natural test, and re-running the same
  table afterwards would make the before/after complete.

## Questions worth asking JUXT

1. Is per-query or per-request class generation expected in XTDB 2, and if so is
   there a cache or unloading path that we may be defeating with high query-shape
   variety?
2. Is a multi-second floor on a `?limit=1` list read expected, and is it the scan
   setup rather than row retrieval?
3. What is the recommended `-Xmx` to total-RSS ratio for an Arrow-backed store of
   this shape, given we observe RSS at ~1.9× `-Xmx`?
4. Is a long conjunctive full-text query expected to return zero rather than
   degrade gracefully to a ranked partial match?
