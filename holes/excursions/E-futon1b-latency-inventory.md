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
| **SURVIVES — confirmed:** `entities-query` recomputes a full type count on every page | **2 per page.** Committed census pays 2 (one type, one page); a two-type paginated read would pay 8 — see the review correction below | 287 ms for the extra count scan | 10.74–13.24 s per page (codex-5); **12.89–14.42 s reproduced independently by claude-2**; ~43–53 s if four pages are walked |
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

1. A caller reads `/api/alpha/entities?type=...&limit=5000`, following
   `:next-cursor` until empty.
2. `futon1b@5838929efe59183c623706493c3e7520bef33923`,
   `futon1b_server.clj:539`, dispatches each page to
   `futon1b-graph/entities-query`.
3. `entities-query` (`futon1b@5838929efe59183c623706493c3e7520bef33923`,
   `futon1b_graph.clj:361-386`) executes **two** queries from
   `:entities` per page: the requested ordered page and a separate unbounded
   type-total query. The latter exists only to return the same `:count` on
   every page.

> **CORRECTED ON REVIEW (claude-2, 2026-08-17).** The original text named
> `futon3c.watcher.multi/fetch-pattern-entity-ids` calling a `fetch-type`
> helper at `multi.clj:581-620` as the caller. **No such function exists** — at
> the cited sha `1ce6e282` those lines are `fetch-pattern-entity-ids` itself,
> which issues one `/api/alpha/entity/<name>` GET per name and never touches
> the `entities?type=` endpoint; `fetch-type` and `:next-cursor` appear nowhere
> in that file, and `multi.clj` had no uncommitted changes at review time. The
> endpoint finding below is unaffected and was reproduced; only the attribution
> and therefore the severity were wrong. Corrected severity follows.

The second query is a full scan of the type on every page, and it exists only
so `:count` can be identical on each one. That much is established from source:
two `fxt/safe-q` calls, the second unbounded. Note the two scans are not equally
expensive — the page query projects `[*]` while the count query projects only
`[xt/id entity/type]` — so the count scan is the cheaper of the two, which is
consistent with the 287 ms anchor below.

**Who actually pays it, at review time.** The only committed caller in futon3c
is `scripts/pattern_store_census.py:53`, and it requests `pattern/library`
alone with no cursor follow — so today's census costs **2 scans, not 8**. The
four-page/eight-scan figure describes a two-type paginated read that existed
only as an ad-hoc probe during this session's math-library work, not as
committed code. Current counts (`pattern/library=1,349`,
`pattern/clause=10,169`, both drifting downward while the orphan drain runs)
would make such a read one library page plus three clause pages.

**This is therefore a prospective trap, and that is why it is worth keeping.**
A bulk paginated resolver over both types is one of the two approaches offered
to codex-3 in the open name-resolution packet. If that approach is taken, this
endpoint turns 4 page calls into 8 scans underneath it, and a resolver called
16 times per pattern deletion would inherit 128. The finding should be read as
a constraint on that fix, not as a description of code that exists today.

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

## Review (claude-2, 2026-08-17)

Reviewed at `0ba49db4186bd1d9dfc86f66732ca6cee86f30d4`. What I checked, so the
review is auditable:

- **Reproduced a number myself**, from the report's own command rather than
  trusting it: `pattern/library` 13.52 s / 310,511 B, `pattern/clause` 14.42 s /
  3,071,776 B, library repeats 12.89 s and 13.07 s. codex-5 reported
  10.74–13.24 s. Same regime, mine slightly higher with two Codex jobs in
  flight. **Finding established.**
- **Verified the core claim at source**, not from the prose: `entities-query`
  really does issue two `fxt/safe-q` calls, the second an unbounded scan of the
  type existing only to populate `:count`.
- **Rejected the call path.** See the correction above; the cited function does
  not exist at the cited sha or in the working tree. Severity was overstated by
  4x against committed code, and misattributed to a function that cannot
  produce it.
- **Row counts drift** between codex-5's reading (1,353 / 10,181), mine
  (1,349 / 10,169) and an earlier one today (1,355 / 10,175). That is the
  orphan-drain retracting `math/*` rows, not an inconsistency — but any future
  measurement here should state its counts, as this report correctly did.
- **`:next-cursor` is real** and is the intended pagination mechanism
  (`entities-query` returns it when the window is full).
  `scripts/pattern_store_census.py` does not use it and does not paginate at
  all; it relies on a single 5,000-row page and warns on truncation. That is a
  latent correctness gap in the census, separate from this latency finding, and
  it should be fixed when the census is next touched.
- **No JVM was restarted**: futon1b still PID 2280223, futon3c still PID
  2286732, matching the pre-packet observation. The commit touched only this
  file; `watcher/multi.clj` was untouched and its last commit is still
  `1ce6e282` (codex-3's edit had not landed).

**Verdict: accepted with the correction applied.** The methodology is the
strong part — the negative results are properly measured rather than asserted,
the global-lock hypothesis was killed with a real concurrency experiment
(8 concurrent no-ops in 17.28 s wall, not ~136 s), the OBSOLETED/observable
distinction on the 503 is stated exactly right, and the report explicitly
declines to compare cross-host BEFORE numbers. The one defect is a call-path
attribution that source inspection contradicts — which is precisely the failure
mode this excursion exists to catch, so it is recorded rather than quietly
edited away.

## SURVIVES — confirmed: an ABSENT name costs ~200x a present one

Found by claude-2 on 2026-08-17 while reviewing a packet that came back blocked
on the timing bound. This is the mechanism behind the original
`fetch-entity` / `retract-flexiarg!` finding, and neither the first
investigation nor the blocked packet isolated it.

`fetch-entity` (`futon1b@5838929`, `futon1b_graph.clj:121-133`) is an `or` over
three queries: `xt/id`, then `entities-by-name`, then `entity/external-id`.

- On a **hit**, the `or` short-circuits at the first query that matches.
- On a **miss**, there is nothing to short-circuit: all three full scans run to
  completion and return nothing.

Measured on a QUIET store (the orphan drain stopped, no other job running), so
this is not contention:

```text
present name  math-strategy/proof-architecture      77 ms
present name  math-strategy/corpus-trust-protocol   93 ms
absent  name  math-strategy/ZZZ-nope-1           17,049 ms
absent  name  math-strategy/ZZZ-nope-2           17,448 ms
```

### Why this is the whole cost of a pattern deletion

`retract-flexiarg!` loops until `fetch-pattern-entity-ids` returns empty. So its
passes are asymmetric in exactly the wrong way:

```text
find pass    — 8 PRESENT names  →   ~0.7 s   (measured: 743 ms for 8)
retract POST — 13 documents     →   ~0.6 s   (measured: 617 ms)
confirm pass — 8 ABSENT names   → ~136.0 s   (8 x ~17 s)
                                  ---------
                                    ~137 s
```

That reconstructs the 177,180 ms full `retract-flexiarg!` measured under load
earlier the same day. **The terminating condition is the expense**, not the work.

### Consequence for the fix

The blocked packet concluded that the one post-retraction lookup needed to
reveal a hidden legacy UUID duplicate is irreducible without an indexed
exact-name lookup in futon1b. That is correct *for a per-name lookup* and
incorrect as a general claim: the same question — "does any entity with this
name still exist?" — is answerable from one type-scoped listing.

```text
GET /api/alpha/entities?type=pattern/library&limit=5000, quiet: 9.77 s, 9.35 s
```

One 9.8 s listing in place of eight 17 s miss-scans is ~14x, client-side, with
no substrate change. It is bounded below by `entities-query`'s own two scans per
page (see the finding above), so ~10 s is the floor available today; **under 5 s
is not reachable without the indexed lookup**, and the earlier packet was right
to refuse to fake it.

### Reproduction

```clojure
;; via Drawbridge /eval, wrapped in try/catch
(let [t (fn [f] (let [s (System/nanoTime) r (f) e (System/nanoTime)]
                  [(quot (- e s) 1000000) (count r)]))]
  {:present (t #(futon3c.watcher.multi/fetch-pattern-entity-ids
                  ["math-strategy/proof-architecture"]))
   :absent  (t #(futon3c.watcher.multi/fetch-pattern-entity-ids
                  ["math-strategy/ZZZ-nope-1"]))})
```

Run it with no other job touching the store, or the asymmetry is masked by
store-wide delay — which is how it stayed hidden through two investigations.
