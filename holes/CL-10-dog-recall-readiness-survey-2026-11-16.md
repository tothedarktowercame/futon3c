# CL-10 — why 狗 recall misses prompt render

Survey date: 2026-11-16 UTC. This was read-only. The HTTP probes used invented
queries and no memory or operator text is reproduced here.

## Finding

狗 starts at the earliest server-side point it currently has: immediately after
the turn record is written, on the `:recorded` event
(`xiang/turn_service.clj:1193-1195`, `agency/dog_provider.clj:428-456`). The
Emacs-path prompt normally reads the cache only 1.5–2.9 seconds later. The live
futon1b search takes 2.6 seconds for a selective one-term query and 10–11
seconds for broad three/five-term OR queries at limit 50. 狗 runs its 驴 and
keyword searches serially (`dog_provider.clj:366-372`). Thus `not-ready` is the
expected result, not a listener race.

## 1. Where the search time goes

The route admits the whole search under one of four expensive-read permits,
waiting at most 3 seconds for admission (`futon1b_server.clj:722-751,812-825`),
then calls `futon1b-text/search` (`futon1b_server.clj:1503-1509,1563-1577`).
Inside search:

1. SQLite FTS5 finds and BM25-ranks candidate ids. The tag predicate is part of
   this same indexed SQL query (`futon1b_text.clj:974-1016`). It is not a
   post-search linear filter.
2. Every candidate is nevertheless fetched from XTDB and rechecked against
   the authoritative body and tags. Fetches run in waves of four
   (`futon1b_text.clj:1057-1080,1090-1132,1134-1164`).
3. Every survivor is fetched from XTDB a second time for hydration
   (`futon1b_text.clj:1166-1183`). The source comment records about 0.3 seconds
   for one equality fetch and identifies hydration as the historical dominant
   cost (`:1173-1179`).
4. Those point reads share the process-wide XTDB permit pool (eight today;
   `futon1b_xt.clj:15-28,62-89`). The four outer HTTP permits do not make one
   query faster; they only bound concurrent expensive requests.

Read-only timings against `:7073`, all with `scope=all` and
`tags=source/claude-code`:

| invented query | limit | hydrate | checked/results | elapsed |
|---|---:|---:|---:|---:|
| `workflow` | 50 | yes | 19/19 | 2,639 ms |
| `workflow OR coordination OR evidence` | 50 | yes | 50/50 | 10,192 ms |
| `workflow OR coordination OR evidence OR memory OR planning` | 50 | yes | 50/50 | 10,703 ms |
| same five terms | 50 | no | 50/50 | 5,333 ms |
| same five terms | 10 | yes | 10/10 | 4,391 ms |
| same five terms | 10 | no | 10/10 | 3,665 ms |

The OR expansion matters indirectly: it fills the candidate/result limit, so
the request performs 50 authoritative point rechecks and 50 hydration reads.
Tag filtering is indexed and is rechecked for correctness, but removing the tag
cannot eliminate the point-read phases. `hydrate=false` approximately halves
the broad limit-50 time, directly implicating the second XTDB fetch. Reducing
the limit reduces reads but does not make the current path fit the prompt
window. The probes do not show permit admission as the main cost: they returned
200, and elapsed time scaled with checked/hydrated hit count. Under contention,
permit waiting can add up to another 3 seconds.

狗 currently needs hydrated `:evidence/body` to obtain the stable memory handle
and short hook (`dog_provider.clj:267-304`), so it cannot simply request the
existing bare response (`:306-314`).

## 2. Record-to-render timing

The record is written and `:recorded` is notified before quote writing, reply
target resolution, and the inline 小象 draft (`turn_service.clj:1193-1229`).
For `:soon`/`:later`, that draft wait is bounded at 2,000 ms. Only after
`record-turn!` returns can transport assemble the agent surface; surface
assembly calls `prompt-line/render!` (`transport/http.clj:5391-5404,5448-5456`).
`render!` immediately calls the registered providers and writes the returned 狗
payload (`prompt_line.clj:272-295`). 狗's provider only reads the cache; on a
miss it returns a freshly stamped `not-ready` and merely ensures background
work is running (`dog_provider.clj:1301-1326`).

For the 12 most recent operator records carrying `:dog_recall` at survey time,
`dog_recall.at - created_at` ranged from **1,517 to 2,943 ms**, median **2,456
ms**. Every one was `not-ready`; the individual deltas were 1,517, 2,206,
2,258, 2,267, 2,287, 2,409, 2,502, 2,579, 2,584, 2,591, 2,630, and 2,943 ms.
This stamp is made in the cache-miss branch, so it is the render observation
time, not the later background completion time (`dog_provider.clj:1311-1318`).

The useful compute window is therefore about 1.5–2.9 seconds. The inline draft
incidentally supplies most of it; it is not a recall synchronization contract.

## 3. Options

### A. Parallelize 狗's two searches

Run the 驴-seeded and keyword requests concurrently, preserving the existing
alternating merge. Cost: two simultaneous expensive-read/XTDB workloads and
more contention. Expected wall time becomes roughly the slower request instead
of their sum: about 2.6–10.7 seconds at limit 50 in this sample. This helps but
does not make broad queries ready within 1.5–2.9 seconds.

### B. Lower the limit

Limit 10 reduced the five-term hydrated probe from 10,703 to 4,391 ms. Combined
with parallel search, expect roughly 1.4–4.4 seconds from the measurements in
this packet and the operator's preceding probes. Cost: lower recall coverage;
because 狗 ultimately retains only three memories this is reasonable, but
false positives among the first ten can hide a useful result. It is still not
a readiness guarantee.

### C. Remove the duplicate hydration fetch

The recheck projection already fetches `evidence/body`, tags, subject and other
fields (`futon1b_text.clj:1057-1067`), but `hydrate=false` discards that body and
the hydrated path fetches it again. A bounded response mode returning the
already-certified recheck projection would remove the second read without
weakening the store-side check. Expected five-term/limit-50 time from the probe
is about 5.3 seconds; at limit 10 it was 3.7 seconds. Cost: a new explicit
response contract. Useful, but alone still misses the normal render window.

### D. Make memory recall an indexed projection

The durable fix is a dedicated futon1b memory-recall projection containing only
the public fields 狗 needs: the existing `cc:` memory id/current version,
project/slug, short hook, tags and FTS score. Maintain it with the evidence
index, so an interactive read does one indexed query rather than 10–100 XTDB
point reads. The earlier 狗 measurement documented in
`dog_provider.clj:19-22` was p50 75 ms, max 101 ms; that is the target order of
magnitude. Two such reads in parallel should remain well below the observed
1.5-second minimum window. Cost: a declared derived index/projection with
checkpoint and staleness health, plus an exact public response schema.

### E. Start before the turn record exists

The current server cannot start earlier: `:recorded` is emitted immediately
after the record write. Starting from raw Emacs/evidence text would move work
ahead of record construction, but requires a provisional key (evidence id or
client turn id), later binding it to the turn record, and reproduces query/text
selection logic across the client/server boundary. It could gain the evidence
delivery interval, but it does not cure a 4–11 second search and adds lifecycle
races.

### F. Wait while building the prompt

A cache wait would block the agent-dispatch HTTP path that assembles the
surface. A wait long enough for the present broad search is 4–11 seconds. It
must **not** be added to Joe's synchronous Emacs record request. A separately
scheduled dispatch could wait after that request has returned, but this only
moves latency into agent start and consumes a request/worker unless expressed
as asynchronous dispatch readiness. A small wait (for example 200 ms) is useful
only after the search is made reliably sub-200-ms.

## Recommendation and pinning test

Implement D in futon1b, then run the two 狗 projections in parallel with limit
10 (A+B). Do not add a prompt-path wait first. This attacks the multiplicative
per-hit point reads, preserves authoritative `cc:` identities, and fits the
measured minimum window with margin. C is a safe intermediate improvement but
not closure by itself.

Pin it without a flaky sleep-based unit test:

* Futon1b test: seed invented memories, invoke the new projection, assert the
  exact ranked public shape and assert via an injected query spy that the read
  makes zero per-hit XTDB fetches after index publication.
* 狗 test: use two request functions held on barriers; assert both requests are
  simultaneously in flight, release them, then render and assert the cache
  supplies items rather than `not-ready`.
* One bounded live acceptance: for ten new operator turns, record
  `recorded→published` and `recorded→render` and require publication to precede
  render. Report p50/p95 rather than encoding wall-clock timing in the unit
  suite.

