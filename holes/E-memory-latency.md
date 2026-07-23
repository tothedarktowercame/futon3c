# E-memory-latency — slow pattern-conditioned memory queries

**Status: IDENTIFY (2026-07-23). Owner: Joe + Zaif/WM implementers.**

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

## Where the time can accumulate

The current `psr_search` path has four read stages:

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
After FTS, however, `proposals-from-rows` also validates each retained memory
with a separate endpoint query. The final probe checked three memories, so
candidate construction could pay three endpoint scans before top-\(k\)
enrichment begins.

### 2. Proposal validation and top-\(k\) enrichment are both serial

After merging local lexical candidates and reviewed-memory proposals,
`tool-psr-search` uses `mapv` over the top candidates. For each candidate it
calls `recall-by-endpoint` before moving to the next candidate.

With three checked proposal memories and `top_k = 5`, the whole path can pay
for eight endpoint scans. The final probe's approximately 17.8-second sum
accounts only for the latter five candidate recalls, not the proposal
validation calls. Consolidating both lanes is the first optimization target.

### 3. One endpoint recall is itself multi-stage

`recall-by-endpoint`:

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

### L4 — Add conservative caching only after measurement

If batching is insufficient, consider a short-lived cache of the compact
current reviewed projection. Cache identity must include domain, pattern,
valid/system time basis, and corpus revision. Challenge, supersession,
retraction, and review changes must invalidate or advance the projection.

Do not cache full selection bodies merely to improve search benchmarks.

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
