# C243 — invoke-jobs ledger publication cost

Date: 2026-08-31

Discovery only. No ledger, publication, or durability code changed.

## Measured population

The live file is `/tmp/futon3c-invoke-jobs.edn`. During the first sample it
contained 6,195 jobs and occupied 134,652,281 bytes; concurrent Agency work
advanced it to 6,197 jobs during the second read. The first population was:

- 5,803 `done`, 269 `failed`, 93 `delivered`, 22 `cancelled`, 6 `running`,
  and 2 `queued` jobs;
- 101 jobs matched the status projection's active/nonterminal set (including
  delivered-but-unconsumed jobs);
- 97,115 retained events across the later 6,197-job sample, with as many as
  263 events in one job.

On the 134.6 MB snapshot, a fresh JVM measured 2,845.7 ms to `slurp` and
`clojure.edn/read-string`, then 2,220.2 ms to `pr-str` the whole ledger. The
combined 5.07 seconds accounts for C235's greater-than-five-second stack.

## What is serialized

The durable format is one monolithic EDN map. `persist-invoke-jobs-ledger!`
uses `(spit path (pr-str ledger))` at `transport/http.clj:284-292`, so every
persist rewrites the full 134.6 MB, not a delta. Both mutation helpers persist
the complete post-update map (`:349-367`).

Publication itself does **not** rewrite the ledger on every warm call.
`active-invoke-job-counts` calls `ensure-invoke-jobs-ledger!` (`:1142-1149`).
Only when the process-local ledger atom is nil does `ensure` read, recover,
and immediately persist the entire ledger (`:341-347`). C235 caught this cold
initialization path. Once initialized, status publication scans the in-memory
jobs without persisting them. Every actual ledger mutation still rewrites the
whole history.

## What the counting query scans

`active-invoke-job-counts` reduces over `(vals (:jobs ledger))`, not an active
index (`transport/http.clj:1149-1172`). Thus its CPU cost scales with total job
history, although only 101 of 6,195 sampled jobs contributed to the result.
The pure scan was small at the present size (about 3.9 ms over all jobs versus
1.1 ms over the active subset); parsing and full serialization dominate the
cold path. This is nevertheless the same population-shape defect as reading a
large index to report a small live set, and its warm cost grows with history.

## Publication frequency and fan-out

`publish-agents-status!` computes `registry-status` synchronously and then
starts `broadcast-agents-ws!`; the broadcast future computes
`registry-status` again (`agency/registry.clj:272-290,318-335`). Each publish
therefore performs two all-history active-job scans, one on the caller and one
in the broadcast future.

Synchronous publication is requested by:

- every `report-external-invoke!` call (`registry.clj:1350-1422`), including
  runner close through `clear-external-invoke!`, scheduler snapshot status,
  every WS status frame, and every HTTP agent-status report;
- every successful proxy-agent refresh (`transport/http.clj:3013-3019`).

Federation also requests the coalescing asynchronous publisher from two
roster paths (`agency/federation.clj:859,1067`). Additionally, direct
`registry-status` consumers perform the same all-history count without going
through publication.

For the WM click specifically, phase updates use `update-agent!` and do not
publish; close clears the external invoke and publishes once. Therefore it is
incorrect to say every click necessarily pays the five-second serialization.
A click pays that cost when its status query is the process's first ledger
access (or something reset the ledger atom); a warm close pays the two scans.

## Repair decision left open

The evidence separates two costs:

1. Whole-file durability on initialization and every job mutation dominates
   today. A delta/log or compacted snapshot design could bound that cost, but
   changes durability and recovery semantics.
2. Status counting scans historical jobs to report the active subset. A
   maintained active index or bounded projection could remove history scaling
   without making publication asynchronous.

Async publication is not recommended as the primary repair: it would hide the
caller latency while leaving full serialization and shutdown durability
unresolved. Choose the durable representation and active-index policy in a
separate commissioned repair.

## Read-only measurement invocations

The measurements used `stat` and a fresh `clojure -M -e` process that only
`slurp`ed, `clojure.edn/read-string`-parsed, counted, and `pr-str`-rendered the
file in memory. It did not require `futon3c.transport.http`, because doing so
in a second JVM could run inflight recovery and mutate the live ledger.
