# E-memory-latency — slow pattern-conditioned memory queries

**Status: BUILD-TEST COMPLETE; MONITOR (2026-07-23). Owner: Joe + Zaif/WM
implementers.**

**Origin:** Phase 3 live acceptance for
`M-shared-memory-control-build-test`. Correctness acceptance passed; this
excursion records the remaining latency problem before the same read seam is
used by the War Machine.

## Summary

Pattern-conditioned memory retrieval is correct and bounded, but a top-five
`psr_search` can still take long enough to disrupt an interactive agent turn.
The final Phase 3 probe retrieved the intended pattern at rank 1, but reported:

- slowest single candidate endpoint recall: **4,753 ms**;
- sum of the five candidate endpoint-recall measurements: **about 17,778 ms**;
- three memory records checked by the candidate-construction lane.

An earlier implementation of the paraphrase fallback issued several FTS
queries sequentially and took about **44 seconds** for `psr_search`. Replacing
that fanout with one bounded token-disjunction query preserved rank-1
retrieval and removed the avoidable series of FTS calls. It did **not** remove
the larger serial candidate-enrichment cost.

These figures use different clocks. The 44-second observation is the elapsed
tool call seen in the invoke trace. The 17,778 ms figure is a sum of
per-candidate recall measurements reported inside the final search result; it
is not a separately measured end-to-end wall time. Bell-job duration also
contains model deliberation and must not be treated as storage latency.

The immediate conclusion is therefore deliberately narrow:

> Query quality is adequate for Phase 3, but the current top-\(k\) enrichment
> path repeats an intrinsically expensive endpoint-membership scan. It cannot
> become the WM reference path without instrumentation and one bounded
> multi-endpoint substrate operation.

## 2026-07-23 implementation outcome

The plan below has now been implemented through the live build-test stage.
The current search path no longer performs a corpus membership scan per
memory or per candidate:

```text
primary/fallback FTS
  -> one compact batch projection for proposal validation
  -> one compact batch projection for top-k enrichment
```

Futon1b exposes `POST /api/alpha/memory/projection` with hard bounds of 20
distinct endpoints and 100 components per endpoint. Futon3c propagates one
trace id through FTS, proposal validation, and candidate enrichment, and
reports local scoring, proposal, enrichment, substrate, and total clocks.
Search returns only the compact hook-bearing memory projection. `psr_select`
uses the same projection to identify current memories and then point-fetches
the complete evidence bodies that it records as surfaced.

### The database-backed batch experiments were insufficient

Three implementations were measured rather than accepted on shape alone:

1. One endpoint-membership scan followed by OR-filtered whole-document
   hydration took about **15–20 seconds** live. One instrumented run spent
   4.95 s selecting endpoints, 12.25 s hydrating edges, and 2.59 s hydrating
   evidence.
2. Joining a bounded selected-id relation to the full hyperedge table took
   10.63 s once, then exceeded 103 s on the next run before the client
   disconnected. The independent health listener remained responsive; the
   hardened accept loop did not reproduce the earlier zombie failure.
3. Bounded indexed point hydration fixed the second table scan, but every
   request still paid the original multi-second corpus membership scan. That
   could not meet the 500 ms direct-projection target.

These failures justified L4, but not a TTL workaround. Futon1b now builds a
bounded, revisioned projection of current `:memory/assert` components before
opening either listener. Verified memory assertions and both retraction paths
serialize the store mutation with a synchronous point refresh of that
projection. Other hyperedge traffic does not invalidate it. The projection
contains compact hooks, never full selection bodies, and has a hard
5000-component startup ceiling. Explicit valid-time or system-time reads
bypass it and retain the bounded bitemporal database path.

The live restart built revision 1 from **9 memory components / 16 endpoints**
in **12,280.8 ms**. This cost is paid before readiness rather than on an agent
turn.

### Short live acceptance series

Corpus and host: local Futon1b `migration-store-21`, projection revision 1,
9 components, one caller, no deliberate concurrent load.

- Ten direct five-endpoint projection requests (one populated endpoint plus
  four misses, limit 9) completed in **1.765–4.572 ms** caller wall time.
  The first response reported 0.613 ms service time and returned the same
  three mathematics memories as before.
- Five top-five `psr_search` repetitions completed in **1.825–3.521 s**.
  `math-formalization/tactic-algebra-interference` remained rank 1 and carried
  all three reviewed hooks on every checked result. Candidate enrichment was
  44–48 ms; the remaining dominant work was primary/fallback FTS
  (proposal totals 1.68–3.02 s).
- Five `psr_select` repetitions returned the same three complete bodies in
  **242–861 ms** outer wall time. After the first process-cold call, the four
  observations were 242–295 ms.

These are acceptance probes, not a latency distribution. They clear the
initial targets for the observed cases without supporting a general p95 or
concurrency claim.

Validation gates:

- Futon1b A1/A2: **39/39 PASS**.
- Futon1b A3/A4/A5, including projection overwrite and retraction coherence:
  **59/59 PASS**.
- Futon3c focused memory/backend suite: **45 tests, 196 assertions, 0
  failures/errors**.
- `check-parens.el`: clean on every changed Clojure file.
- `clj-kondo`: zero errors and zero warnings on every changed Clojure file.

Deployment evidence:

- Futon1b implementation commit: `63a170e` (`Add coherent bounded memory
  projection`).
- Futon3c implementation commit: `e8c7364` (`Batch and instrument memory
  recall`).
- Final Futon1b restart: 2026-07-23 12:11 BST, PID 233414. Startup rebuilt
  revision 1 from 9 components / 16 endpoints in 12,187.0 ms; both main and
  independent health endpoints returned 200. A traced post-restart projection
  returned the same three memories in 7.239 ms caller wall / 0.711 ms service
  time. The service was active with about 1.0 GB current / 1.09 GB peak memory.

### Restart-catch-up correction

The first post-restart Phase 3 smoke found a correctness defect hidden by the
latency result: revision 1 had been built while XTDB was still replaying its
durable log. It contained the superseded `e-6bcbb51e…` but omitted current
correction `e-aa9c729b…`. Fast stale recall is not acceptance.

Futon1b commit `f887aa9` (`Gate memory projection on XTDB progress`) now records
the XTDB completed/submitted/processed transaction watermark. A projection is
published only when that watermark is stable across selection and hydration;
later drift forces a coherent bounded rebuild. A regression writes below the
normal synchronous refresh hook and proves that the next read self-heals.

After deployment the startup build required four attempts while XTDB caught up,
then published 14 components / 31 endpoints. The corrected smoke completed in
3.323 s total search time, ranked
`math-formalization/tactic-algebra-interference` first, used current correction
`e-aa9c729b…` as its proposal support, returned it among the three reviewed
hooks, and counted the superseded original among two state exclusions. The
shared top-five projection took about 85 ms caller wall. Futon1b remained
39/39 and 59/59 green; the temporal suite is now 4 tests / 13 assertions.

### Monitoring still required

Preserve trace ids and gather a longer fixed-query series over normal traffic.
At minimum, record cold/warm classification, projection revision/component
count, FTS primary/fallback time, proposal-validation substrate wall time,
candidate-enrichment substrate wall time, total search time, admission
outcomes, and concurrency. Report p50/p95 only after a stated sample size;
separate 503 admission from latency and correctness failures.

Also watch startup build duration and component count, projection revision
advancement after memory lifecycle writes, process memory/PSI, accept-loop
exceptions, and response correctness after every Futon1b restart. Re-run the
same measurements when the WM domain is admitted or corpus size changes
materially. The remaining obvious latency target is FTS, not endpoint
membership.

## The observed cases

All three probes used:

```text
query: normalize a denominator before using field_simp
top_k: 5
expected pattern: math-formalization/tactic-algebra-interference
```

| Probe | Candidate construction | Checked memories | Expected-pattern rank | Observation |
|---|---:|---:|---:|---|
| Initial | Full conjunctive FTS only | 0 | Absent from top 5 | Fast enough to run, but semantically missed. |
| First correction | Bounded sequential token fanout | 3 | 1 | Correct, but `psr_search` took about 44 seconds. |
| Final correction | One bounded token disjunction | 3 | 1 | Correct; slowest endpoint recall 4,753 ms and sum across five candidates about 17,778 ms. |

The final query strategy was:

```clojure
{:query-strategy :bounded-token-disjunction
 :fallback-tokens ["normalize" "denominator" "field_simp"]
 :checked-memory-count 3}
```

The supporting corrected memory was
`e-aa9c729b-fe1e-435d-ba5d-bb19087f27a7`. Its reviewed attachment promoted
the intended pattern to rank 1 without an embedding fallback.

The machine-readable record is
`holes/labs/M-typed-memories/phase3-trial-results.edn`.

## Where the time accumulated before this fix

The pre-fix `psr_search` path had four read stages:

```text
local notions-index scoring
  -> memory proposal (FTS, plus fallback FTS when needed)
  -> serial endpoint validation for each proposed memory
  -> serial recall/enrichment for each of the top-k patterns
```

### 1. Candidate construction can make two FTS calls

`memory_recall/propose-patterns-by-query` first sends the full query to
`/api/alpha/evidence/text-search`. If that produces no reviewed-memory
proposal, it sends one bounded OR query built from at most four informative
tokens.

This is now at most two FTS calls, not one call per token. Both calls remain
serial, because the fallback depends on the result of the primary query.
Before this fix, `proposals-from-rows` also validated each retained memory
with a separate endpoint query. The final probe checked three memories, so
candidate construction could pay three endpoint scans before top-\(k\)
enrichment began. It now validates those memories in one bounded projection.

### 2. Proposal validation and top-\(k\) enrichment were both serial

Before this fix, after merging local lexical candidates and reviewed-memory
proposals, `tool-psr-search` used `mapv` over the top candidates. For each
candidate it called `recall-by-endpoint` before moving to the next candidate.

With three checked proposal memories and `top_k = 5`, the whole path can pay
for eight endpoint scans. The final probe's approximately 17.8-second sum
accounts only for the latter five candidate recalls, not the proposal
validation calls. Consolidating both lanes is the first optimization target.

### 3. One endpoint recall was itself multi-stage

The old database-backed `recall-by-endpoint` path:

1. requests `:memory/assert` hyperedges for one pattern endpoint;
2. overfetches up to three times the requested result count;
3. filters domain, review status, and lifecycle state;
4. fetches each surviving evidence entry;
5. constructs compact memory projections.

The 3× edge overfetch is required for correctness. Phase 3 demonstrated why:
a superseded edge otherwise consumed a pre-filter slot and hid its current
correction. It is bounded (`max-limit = 100`) and should not be removed merely
to improve a benchmark.

The remaining N+1 shape is the graph request followed by evidence-entry
fetches for each surviving memory. Direct isolation below shows that these
point reads are secondary to endpoint selection for the current corpus; they
should still be removed from the compact search projection.

### 4. Timeouts are safety ceilings, not latency targets

The substrate client currently gives these reads a 60-second timeout. That
prevents indefinite blocking, but it also permits a single slow remote read to
occupy most of an agent turn. Lowering the timeout without consolidating the
work would convert slowness into recall failures; it is not the first fix.

## What is known, inferred, and still unknown

### Known

- Candidate enrichment is sequential in `tool-psr-search`.
- The old token fallback performed sequential FTS calls; the final
  token-disjunction performs one fallback FTS call.
- Endpoint recall overfetch is bounded and necessary for lifecycle
  correctness.
- The final query returns the correct reviewed memory and excludes the
  superseded memory.
- The final five candidate-recall measurements sum to about 17,778 ms.
- Proposal construction performs one endpoint-validation query per retained
  FTS memory; the observed three-memory case can therefore make three more
  endpoint scans before the five candidate recalls.
- On 2026-07-23, five direct FTS repetitions took **0.66–1.16 s**, while five
  direct endpoint repetitions with `limit=9` took **3.04–4.49 s**.
- Endpoint selection, not evidence hydration, dominates the current case:
  `limit=1` still took **2.50–5.60 s**, an endpoint miss with no hydration took
  **2.85–5.00 s**, and seven individual evidence GETs took **39–67 ms** each.
- The same diagnostic window showed no cgroup pressure explanation: Futon1b
  used about 5.2 GB against `MemoryHigh=10 GB`, with zero `memory.events high`
  and zero current PSI.

### Plausible but not yet isolated

- A single XTDB membership scan over all requested endpoints will cost
  materially less than the current three-plus-five repeated scans.
- FTS catch-up or concurrent ingestion may explain part of the observed
  endpoint variance.
- Removing evidence-entry N+1 will improve the tail by hundreds of
  milliseconds, but not by the several seconds needed on its own.

### Unknown

- The p50/p95 latency of each individual substrate operation.
- How much caller-observed time is bounded HTTP queueing/transport versus XTDB
  execution, evidence hydration, and projection. Expensive-read admission
  itself does not wait: Futon1b admits immediately or returns retryable 503.
- Whether a warm query differs materially from a cold query.
- Whether candidate recalls contend with FTS catch-up or other expensive
  reads.
- How the path behaves at WM corpus size and under concurrent Zaif runners.

No throughput, success-rate, or scaling claim should be made from the three
Phase 3 trials or the short live diagnostic series. The latter isolates the
dominant operation but is not a cold/warm latency distribution.

## Reproduction

### Direct FTS timing

```bash
time curl -sS -G \
  --data-urlencode 'q=normalize OR denominator OR field_simp' \
  --data-urlencode 'limit=15' \
  http://127.0.0.1:7073/api/alpha/evidence/text-search >/tmp/memory-fts.edn
```

### Direct endpoint timing

```bash
time curl -sS -G \
  --data-urlencode 'end=math-formalization/tactic-algebra-interference' \
  --data-urlencode 'type=memory/assert' \
  --data-urlencode 'limit=9' \
  http://127.0.0.1:7073/api/alpha/hyperedges >/tmp/memory-edges.edn
```

To distinguish selection from hydration, repeat with `limit=1`, then with a
known-absent endpoint. Time the returned evidence ids individually through
`/api/alpha/evidence/{id}`. The 2026-07-23 isolation found that the miss and
`limit=1` remained multi-second while point GETs remained tens of
milliseconds.

### Full path

From a runner configured with `:memory-domain :mathematics`:

```text
psr_search "normalize a denominator before using field_simp" top_k=5
```

Record the result's candidate-construction audit and every candidate's
`:memory-recall :elapsed-ms`. Also record an outer monotonic wall clock around
the tool call. Do not infer storage time from the whole bell-job duration.

Run cold and warm repetitions separately, with a small fixed count. Preserve
the exact query, `top_k`, corpus revision, substrate URL, and concurrency in
the result.

## Proposed build-test sequence

### L1 — Instrument the stages

Add monotonic elapsed measurements for:

- local pattern-index scoring;
- primary FTS;
- fallback FTS;
- every proposal edge-validation query, including checked-memory count and
  aggregate;
- each candidate's hyperedge query;
- evidence-entry hydration;
- projection;
- total `psr_search` wall time.

Include a query/trace id propagated to Futon1b. Report server service time
separately from caller-observed wall time so bounded executor queueing and
transport are visible. Record admission outcome rather than “admission wait”:
the current semaphore uses `tryAcquire` and rejects contention immediately.

**Exit:** the sum of named stages approximately accounts for total tool time,
and cold/warm runs can be compared without using invoke-job duration.

### L2 — Consolidate all endpoint membership scans

Add one explicitly bounded multi-endpoint substrate operation. It must:

- accept a bounded set of endpoint ids and a bounded per-endpoint result limit;
- scan endpoint membership once and retain the matched requested endpoint in
  each result;
- hydrate each distinct edge at most once, then group deterministically by
  requested endpoint;
- preserve temporal basis, type, lifecycle overfetch, and explicit overload
  outcomes;
- serve both proposal-memory validation and top-\(k\) pattern recall.

Do not use parallel single-endpoint scans as the first fallback. The isolated
single scan already takes 3–5 seconds; multiplying it across the two
expensive-read permits would trade caller latency for substrate contention and
recreate the workload-amplification shape addressed by the 2026-07-22
hardening.

**Exit:** proposal validation and top-five enrichment each perform one bounded
substrate operation, with no per-memory or per-pattern endpoint query and with
identical candidate order and audit reasons.

### L3 — Remove evidence hydration N+1

Return the compact fields needed by `psr_search`—memory id, hook, lifecycle
state, witness status, volatility, and review status—in the batch projection.
Keep full evidence bodies at the `psr_select` seam.

This is a secondary optimization for the measured corpus: point evidence GETs
were tens of milliseconds while an endpoint miss was still multi-second. Its
main value is removing avoidable tail work and making the search contract one
coherent projection, not explaining the present 3–5-second floor.

**Exit:** search does not fetch full evidence entries one by one; selection
still retrieves the complete bodies and records exactly what surfaced.

### L4 — Materialize the current compact projection after measurement

Batching was insufficient on the live XTDB corpus, so the implemented L4 is a
coherent revisioned current-state projection rather than a short-lived TTL
cache. It advances synchronously with memory assertion/retraction writes;
explicit valid/system time bases bypass it. Domain and lifecycle filtering
remain canonical in Futon3c, and full selection bodies are not cached.

## Proposed latency gates

These are initial engineering targets, not claims about current performance:

- direct current endpoint projection: p95 at or below **500 ms** locally;
- top-five `psr_search`: p95 at or below **5 seconds** locally;
- selected-pattern full-body recall: p95 at or below **2 seconds** locally;
- all overload and timeout outcomes remain explicit data;
- candidate order, lifecycle filtering, domain filtering, and memory-use audit
  remain unchanged.

Measure at least cold and warm conditions and report corpus size and
concurrency. Revise the targets if the instrumentation shows an unavoidable
substrate floor, but do not declare acceptance from a single fast run.

## Non-fixes

The following would make the timing look better while weakening the memory
contract:

- removing lifecycle overfetch, which can hide current corrections;
- skipping review/domain filtering;
- reducing `top_k` only for the benchmark;
- using stale bodies at selection;
- converting timeouts into empty-memory success;
- adding an embedding fallback to hide lexical or storage latency;
- parallelizing several single-endpoint corpus scans before a batch substrate
  operation exists;
- running unbounded parallel reads.

## Relationship to Phase 4

Phase 4 asks the War Machine to use the same memory seam in dark mode. That is
useful for latency work because WM queries will expose a different domain and
corpus while remaining non-operative. Phase 4 should not fork a faster
WM-specific memory implementation.

The recommended order is:

1. add L1 instrumentation;
2. run the existing mathematics probes as a cheap baseline;
3. implement L2 batch/compact recall;
4. rerun mathematics probes;
5. admit dark WM memories through the same code;
6. compare domains without changing live mission ordering.

## Cross-references

- `holes/missions/M-shared-memory-control-build-test.md` — Phase 3 acceptance
  and Phase 4 gate.
- `holes/labs/M-typed-memories/phase3-trial-results.edn` — machine-readable
  trial and timing ledger.
- `src/futon3c/peripheral/memory_recall.clj` — bounded proposal and endpoint
  recall implementation.
- `src/futon3c/peripheral/real_backend.clj` — serial top-\(k\) enrichment in
  `tool-psr-search`.
- `src/futon3c/substrate/client.clj` — HTTP calls and 60-second read timeout.
- `futon1b/futon1b_server.clj` and `futon1b/futon1b_graph.clj` — admission,
  endpoint query, bitemporal filtering, and hydration.
