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

## Correction: the agent hot path is not list-scan bound (2026-07-30, late)

An earlier reading of this document implied that the 2–24 s list-read cost was
what our retrieval path pays. **It is not**, and the correction matters for
which question is worth JUXT's time. Stage timings taken on the live dispatch
path:

| stage | measured |
|---|---|
| memory projection | **9–14 ms** — an in-memory indexed projection, not a store list scan |
| receipt list read (the only list-shaped read on this path) | 1.10–1.50 s, under a 5 s client cap |
| full-text ladder | ~15.8 s total; individual searches 0.75–3.71 s |

So the dominant cost on our hot path is **full-text search**, not list scans,
and the list-read figures earlier in this document — while accurate as
measurements of the endpoint — should not be read as describing what our agent
loop actually pays. A 30 s dispatch budget was breached not by store list
latency but by client-side fan-out, which we removed rather than papering over
with a larger timeout.

The question this sharpens for JUXT is therefore about **full-text search
latency and result-set behaviour**, not list-scan cost. Note also that the FTS
surface in question is our own SQLite sidecar rather than XTDB (see the
withdrawn question at the end of this document), so the part of the ladder cost
attributable to XTDB specifically has not yet been isolated.

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

## After the restart (operator-initiated, ~14:25Z)

The restart is the natural test of which part of the degradation is
*accumulated* and which part is *structural*, and it separates them — though not
as cleanly as the pre-restart numbers implied. Fresh process, ~15 minutes warm,
and by then the unrelated batch sweep had also finished, so the host went from
23/30 GB used to 10/30 GB.

| metric | 09:30Z (warm JVM, quiet host) | 14:15Z (warm JVM, loaded host) | post-restart (fresh JVM, quiet host) |
|---|---|---|---|
| RSS | 4.98 GB | 7.76 GB | **1.82 GB** |
| Metaspace used | 331 MB | 957 MB | **122 MB** |
| Metaspace committed | 482 MB | 1.65 GB | 148 MB |
| G1 heap total | 2.33 GB | 4.19 GB | 1.19 GB |
| G1 heap used | 845 MB | 2.07 GB | 668 MB |
| `GET /evidence/<id>` | 0.18 s | 0.18 s | 0.15 s |
| `GET /entity/<id>` | 0.25 s | 0.40 s | 0.39 s |
| `?limit=1` | 1.76 s | 2.57 s | **2.14 s** |
| `?limit=50` | — | 8.73 s | **5.66 s** |
| `?limit=200` | 12.45 s | 23.96 s | **13.65 s** |
| `text-search`, 1 term | — | 1.01 s | 0.81 s |

**The finding that matters: the restart did not make list reads fast. It
returned them to a baseline that was already slow.** `?limit=200` went
24.0 s → 13.7 s, against 12.5 s at 09:30 on a 25-hour-old process. `?limit=1`
went 2.57 s → 2.14 s, against 1.76 s. A brand-new process with a warm cache and
a quiet host is still spending ~13 s on a 200-row list read and ~2 s on a
one-row list read.

So the list-scan cost decomposes into:

- **a large structural baseline that survives restart** — this is the part worth
  asking JUXT about, and it is not a leak;
- **a session-accumulated component of roughly 10 s at `limit=200`** — which the
  restart clears.

**Correction to the pre-restart reading.** Before the restart we were inclined to
attribute that accumulated component to the metaspace growth. The post-restart
data does not support that inference, because the restart and the sweep's
completion coincided: the fresh process was measured on a host with 20 GB
available rather than 7.3 GB. Host memory pressure and JVM-internal accumulation
are therefore **not separated** by this experiment, and the honest statement is
that the 14:15Z figures reflect both. Separating them needs a restart taken while
the host is *still* loaded, or a load run against a *warm* process on a *quiet*
host.

**What is not confounded** is the memory growth itself, which was measured on the
store process directly: metaspace used went 331 MB → 957 MB over 4.75 hours and
resets to 122 MB on restart. That is a property of the process, not of the host.

**Point lookups never moved**, across a 30-hour-old degraded process and a fresh
one: 0.15–0.18 s throughout. Whatever the list path is doing, the id path does
not do it.

## Six hours after the restart: RSS regrows, metaspace does not

A third snapshot of the *same fresh process*, taken 6 h 01 m after the
operator restart, on a day of unusually heavy agent-loop traffic.

| metric | pre-restart 09:30Z (25 h old) | pre-restart 14:15Z (30 h old) | post-restart +15 min | **post-restart +6 h** |
|---|---|---|---|---|
| RSS | 4.98 GB | 7.76 GB | 1.82 GB | **6.09 GB** |
| Metaspace used | 331 MB | 957 MB | 122 MB | **191 MB** |
| Metaspace committed | 482 MB | 1.65 GB | 148 MB | **481 MB** |
| Class space used | 96 MB | 336 MB | — | **47 MB** |
| Young GCs | 832 | 1515 | — | **1483** |
| Total GC time | 43.4 s | 173.0 s | — | **178.8 s** |
| Full GCs | 0 | 0 | — | **0** |

**This dissociates the two growth curves, and it weakens the hypothesis
this document opened with.** In six hours RSS regrew by **+4.27 GB**
while metaspace used grew by only **+69 MB**. The pre-restart session
had RSS +2.78 GB accompanied by metaspace +626 MB over a comparable
4.75 h. So the process can put on multiple gigabytes of resident memory
essentially *without* class-metadata growth: whatever drives RSS is
predominantly heap or off-heap (Arrow buffers are the obvious
candidate), **not** the per-query class generation we proposed as the
leading explanation. Question 1 to JUXT stands as a question, but it
should no longer be framed as our leading hypothesis for the RSS
figures; it is at most an explanation for the *metaspace* series, and
that series is the smaller effect.

**The GC rate is the sharper anomaly.** 1483 young GCs and 178.8 s of
GC in **six hours**, against 1515 young GCs and 173.0 s in **thirty
hours** before the restart — a roughly fivefold increase in young-GC
rate per unit time. Full GCs remain zero and total GC is still only
~0.8 % of wall-clock, so this is not a pause problem; it is an
allocation-rate signal. The honest reading is that today's workload is
genuinely much heavier than the days preceding the earlier snapshots
(a continuous dispatch loop, two full-corpus censuses of ~90 by-id
reads each, and one `limit=5000` hyperedge query), so **allocation rate
is confounded with workload and this comparison does not isolate a
store property.** It is reported because the fivefold gap is large
enough to be worth a question, not because it is controlled.

What survives from the earlier sections unchanged: point lookups by id
stay flat (~0.15–0.18 s) across every process age measured, and zero
full GCs across all snapshots.

## Confounds and limits

- **Single instance, single workload, no control.** No A/B, no comparison build,
  no isolation of which query shapes drive metaspace growth.
- **The final measurement was taken under machine-wide memory pressure**: an
  unrelated batch sweep (7 JVMs, ~7.9 GB) was running concurrently, with the host
  at 23/30 GB used and 549 MB free. The 14:15Z latency figures are therefore an
  upper bound and the comparison to 09:30Z is not clean. The *memory* figures for
  the store process are unaffected by this.
- **Corpus size was not measured**, so the list-scan cost has no denominator here.
- The restart test was run (see above) but is **partially confounded**: the
  unrelated batch sweep finished at about the same time, so the fresh process was
  measured on a quiet host while the degraded one was measured on a loaded host.
  JVM accumulation and host pressure are not separated by this data.

## Questions worth asking JUXT

1. Is per-query or per-request class generation expected in XTDB 2, and if so is
   there a cache or unloading path that we may be defeating with high query-shape
   variety?
2. Is a multi-second floor on a `?limit=1` list read expected, and is it the scan
   setup rather than row retrieval? Note this floor **survives a restart** — a
   freshly started process still takes ~2.1 s — so it is structural rather than
   accumulated.
3. What is the recommended `-Xmx` to total-RSS ratio for an Arrow-backed store of
   this shape, given we observe RSS at ~1.9× `-Xmx` after 30 hours (and 0.45×
   on a fresh process)?
4. Is a long conjunctive full-text query expected to return zero rather than
   degrade gracefully to a ranked partial match?

**Withdrawn before the meeting (2026-07-30).** A fifth question was drafted
here about hyphenated identifiers appearing to be indexed atomically. It has
been removed rather than asked, because the premise was wrong on
investigation: **the full-text surface in question is not XTDB at all.** It is
an application-controlled SQLite FTS5 sidecar (`futon1bi.text-index`,
`tokenize='unicode61'`), where hyphens are ordinary separators. The observed
zero-result queries were bounded-result starvation plus body-only indexing —
`:evidence/body` is indexed, the id column is `UNINDEXED`, and top-level
subjects are not indexed. All of that is ours to fix and none of it is a
question for JUXT. Recorded here so the mistake is visible rather than
silently deleted; question 4 above concerns genuine XTDB text-search
behaviour and still stands.
