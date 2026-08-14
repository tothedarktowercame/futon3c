# E-apm-A3-ingest-efficiency — the A3 series as handoffs

**Created 2026-08-13 (claude-2) at Joe's request:** *"let's focus on getting
the A3 series into a list of handoffs that get the ingest to be efficient …
I liked your previous estimate of 'something plausibly under half an hour'
and if that's blocked we need to change it."*

Sub-items of **A3** on the locked list (`E-apm-halftime-pre-go-live.md`). No
top-level additions; this file only decomposes A3.

## The target, stated as a number

| quantity | measured |
|---|---|
| flexiarg files | 1,148–1,192 (1,095 in `futon3`) |
| facets per pattern | mean **6.64** (927 of 1,148 have all 7) |
| documents per file | 1 + F + F → mean **14.3**, max 15 |
| **corpus total** | **16,396 documents** |
| current ceiling | **0.76 docs/sec** (~1.3 s/doc, 4-way concurrent) |
| ingest at that rate | **~6 h** concurrent, ~14 h sequential |
| **rate needed for 30 min** | **9.1 docs/sec** — a **12×** gap |

**The target is not known to be impossible.** One measured batch of 4 wrote
in **274 ms** with all four rows verified present — 14.6 docs/sec, above the
rate required. The same shape minutes later took 11,965 ms. Closing that gap
is H1 and is what the whole series hangs on.

## What is already ruled out (do not re-investigate)

Measured on the live store, 2026-08-13, after the boot at 17:39 UTC:

| hypothesis | verdict | evidence |
|---|---|---|
| batching helps | **no** | 8 individual 24,228 ms vs batch-of-8 24,017 ms |
| cost is per HTTP call | **no** | batch of 8 = 1 call, same time |
| cost scales with payload | **no** | 20 KB doc 3,420 ms vs tiny 3,317 ms |
| type registration | **no** | brand-new type 3,000 ms vs existing 2,995 ms |
| the rescue ladder | **no** | no `:rescue` key in batch responses |
| the post-commit read-back | **no** | reads are 89–287 ms, ~3% of the cost |
| HTTP/validation overhead | **no** | invalid POST rejected in 11 ms |
| writes cannot overlap | **no** | 4 concurrent 5,356 ms vs 13,352 ms sequential |

Concurrency saturates: 4→1,339 ms/doc, 8→1,322, 16→1,307.

*Caveat on provenance: the ~3.3 s figures were taken while codex-3 was running
ingests against the same store. The clean re-measurement is ~3.0 s/doc. The
274 ms outlier was clean.*

---

## H1 — ✅ ANSWERED 2026-08-14 (codex-4) — verified by claude-2. **30 min is back on.**

**The mechanism: an unindexed name scan, not transaction indexing.**
`ensure-entity-id` calls `entities-by-name` for every entity that omits an
explicit `:id` (`futon1b_graph.clj:241`), and that query pulls `[*]` from
`:entities` and filters on `entity/name` — a full scan of a 5,876+ row table,
per document.

**The repro flips on demand** (claude-2's own measurement, not the report):

| | ms | per doc |
|---|---|---|
| 1 entity, explicit `:id` | 78 | 78 |
| 1 entity, omitted `:id` | 3,003 | 3,003 |
| 4 entities, explicit | 148 | 37 |
| 4 entities, omitted | 12,144 | 3,036 |
| repeat explicit (idempotency) | 45 | — |

**Relations have the same two regimes.** `resolve-rel-endpoint` first tries
`(fxt/present? node :entities x)` — a lookup by `xt/id` — and only falls back
to the name scan when that misses. Endpoints that are real ids take the fast
path; endpoints that are merely names take the scan.

**Measured end-to-end, one realistic file (1 pattern + 7 clauses + 7
relations), all explicit ids:**

```
8 entities batched, explicit ids : 310 ms
7 relations batched, id endpoints: 761 ms
TOTAL per file                   : 1,071 ms   (was ~28,000 ms)
re-ingest of the same file       :   275 ms, census UNCHANGED (idempotent)
```

**1,148 files → 20.5 minutes sequential**, before any concurrency. Target met.

**Why the fix is identity-preserving, not a new scheme.** Pattern entities
already use the qualified name as their id — `agency/bounded-lifecycle` has
`:id "agency/bounded-lifecycle"`, and so does `baldwin/two-claims-not-one`,
which today's new path wrote (`ensure-entity-id` found the legacy row and
reused its id). Passing `:id` explicitly produces the ids the system already
produces; it just skips the scan that rediscovers them.

*Honest limit, from codex-4 and confirmed: the one anomalous 274 ms
**omitted-id** batch was never reproduced. The slow regime's mechanism is
proven; that single fast outlier is not explained. It does not affect the fix.*

*Not a poll/flush tunable — this eliminates `FileChannel.force`, indexer flush
interval, FTS SQLite write lock, and the (4-wide) `query-permits` semaphore as
the fixed per-document cause.*

*Consequence for H2: **not needed.** Deferring the transaction was aimed at a
cost that turns out to be a table scan. Do not dispatch it.*

## H7 — FIX: make `ingest-flexiarg!` supply explicit ids (from H1)

Pass `:id` = the qualified name for the pattern entity and each clause entity
in `futon3c/src/futon3c/watcher/file_ingest.clj` `ingest-flexiarg!`. Relations
already pass `(:id pattern-entity)` / `(:id clause-entity)`, which then hit
`present?` and skip the scan.

**Decision for Joe (small but real):** for a pattern that does **not** already
exist in the store, this changes the minted id from a fresh UUID to the
qualified name — making new patterns consistent with the 5,876 existing ones.
For every pattern that already exists, the id is unchanged.

## H1 (original packet) — DISCOVERY: what makes a verified entity write cost ~3 s?

**Discovery only. No code changes.** Split from implementation deliberately.

Deliverable: the mechanism named, plus a **repro that flips between the fast
and slow regimes on demand**. If it is a tunable (poll interval, flush
duration, permit pool, log sync), say which and what it costs to change.
Start from the ruled-out table above rather than re-deriving it.

Gates it unblocks: everything below. **This is the one that decides whether
half an hour is reachable.**

## H2 — DISCOVERY: can the indexing wait be deferred safely?

`xt/execute-tx` submits *and waits*; `xt/submit-tx` + `xt/await-tx` exist in
XTDB 2.1.0. Joe's standing design question — *"use a future so that we don't
wait inline, but certify after the fact"* — applies to the **transaction**,
not the read-back (the read-back is only ~3% of the cost).

Deliverable: a written recommendation with **measured backlog growth**, not a
patch. Hard constraint: writes outpacing indexing is what made the store
unbootable on the morning of 2026-08-13 (see
`futon1b/TN-futon1b-boot-incident-2026-08-13.md`). Any proposal must say what
bounds the backlog and how a caller learns a write failed after being told it
succeeded.

## H3 — ✅ DONE 2026-08-14 (codex-3, commit `77c5a60`) — verified by claude-2

`build-entity` now threads the docs already built in the same batch into
`ensure-entity-id`, so a repeated name resolves to the id already minted.
All copies then share one `:xt/id`, and `execute-tx` collapses them to one row.
The response deliberately keeps its input length and order — `ingest-flexiarg!`
zips returned entities **by position**, so collapsing them would break the
caller.

Verified by claude-2, not accepted from the report: read the diff; ran
`clojure -M:node -m test-temporal` → 14 tests, 60 assertions, 0 failures; read
the added tests to confirm they assert real behaviour (3 copies → 1 distinct
id; repeat batch → same ids; explicit `:id` wins over a later implicit
duplicate; 1 stored row).

*Noted, not blocking: the dedup scan is O(n²) in batch size — each item filters
every previously built doc. Fine at 8; the 5,001-row test still passed. The
per-item `entities-by-name` node query probably dominates anyway.*

## H4 — ✅ DONE 2026-08-14 (codex-3, same commit) — verified by claude-2

`/api/alpha/entities` takes an `after` cursor, orders by `xt/id`, returns
`:next-cursor` when the window is full, and `:count` is now the **true type
total** rather than the returned-row count. Test fixture writes 5,001 entities
and enumerates them 5,000 + 1 with both pages reporting 5,001.

**Not yet live — this needs a substrate restart** (~6m30s boot). Batch it with
any other pending restart rather than restarting twice.

*Noted, not blocking: `:count` now costs a second unbounded scan of the type on
**every** call (~287 ms for `pattern/library`), on what is a hot read path.
`hyperedges-query` offers `include-total?` to waive exactly this; the entity
version does not.*

*Superseded description of the original defect, kept so it is not re-found:*

## H3 (original) — `entities/batch` did not deduplicate within a batch (A3.2)

`build-entity` → `ensure-entity-id` resolves by querying the node, but nothing
in the batch is committed while items are built, so N copies of one name each
get a fresh UUID. Measured: same name 3× in one batch → **3 distinct ids, 3
rows**. Across separate calls idempotency is intact (3 writes → 1 row).

Acceptance: same name N× in one batch yields **one** row and one id, repeated
across calls too; A4 unaffected (invalid item still 400 before any write);
existing tests still 37/37 and 33/33.

## H4 — FIX: the entity endpoint cannot enumerate its own largest type (A3.1)

`entities-query` takes only `limit`, has **no cursor**, `max-result-limit` is
5000, and there are 5,876 `pattern/library` entities — so a maxed request
comes back silently short and looks complete. Its `:count` is the *returned*
count, not the true total.

Acceptance: an `after` cursor, and a `:count` that is the true type total when
unfiltered — i.e. match `hyperedges-query`, which already does both correctly.
Add a test that enumerates a type with more rows than `max-result-limit`.

## H5 — ✅ DONE 2026-08-14 (claude-2). The first non-truncated count.

Full enumeration via H4's cursor: 5,876 and 15,637 rows paged, both matching
their declared `:count`. **COMPLETE**, not a sampled page.

### `pattern/library` — 5,876 rows, **1,318 distinct patterns**

| rows per pattern | patterns |
|---|---|
| 1 | 260 |
| 2 | 358 |
| **7** | **700** |

Nothing has 3–6. Checks: 260+358+700 = 1,318 ✓ and
260·1 + 358·2 + 700·7 = 5,876 ✓. **Surplus: 4,558 rows.**

### `pattern/clause` — 15,637 rows, **9,378 distinct clauses**

| rows per clause | clauses |
|---|---|
| 1 | 3,119 |
| 2 | 6,259 |

3,119 + 6,259 = 9,378 ✓; 3,119 + 12,518 = 15,637 ✓. **Surplus: 6,259 rows.**

**Total surplus ≈ 10,817 of 21,513 rows — about half the store is copies.**

### Both document shapes reconciled

| | n |
|---|---|
| distinct pattern names (library) | 1,318 |
| distinct parents implied by clauses | 1,229 |
| patterns WITH clause entities | 1,224 |
| patterns with NO clause entity | 94 |
| clause-parents with NO library entity | 5 |

**Corrections this forces.** The earlier "1,194 distinct / 383×1 / 111×2 /
505×6 / 195×7" was read off the truncated 5,000-row page and is wrong in every
column. The true shape is 1,318 distinct with a clean 1/2/7 split.

**Two findings, recorded not worked:**
- The 5 clause-parents with no library entity include
  `math-strategy/clarification-meta` — a pattern deleted on 2026-08-13 whose
  **clause entities survive**. So the delete/rename orphan sweep misses clause
  entities as well as the legacy hyperedge (A1).
- The other 4 are malformed names such as
  `data-mining/checkpoint-the-long-run/evidence (2026-06-25` — clause names
  containing a parenthesised date, i.e. a projection artefact that became an
  entity id.

### Original packet text

## H5 (original) — MEASUREMENT: the true duplication census (blocked on H4)

Every A3 figure on the locked list ("5,000 rows / 1,194 distinct names / 811
duplicated") was read off a truncated `limit=5000` page and is void.

Deliverable: distinct names, rows per name, and **the distinction that decides
everything — real duplicates vs bitemporal versions**. Must cover **both
document shapes**: content lives in `:entity/props` for older patterns and in
separate `pattern/clause` entities for newer ones (15,637 of those).

## H6 — THE A3 FIX: one copy, one convention, watcher-maintained

Joe, 2026-08-14: *"we should make sure that what we finally have in the store
is one copy with one naming convention, and that those will be kept up to date
by the multi_watcher."*

### A3's central question is now ANSWERED: real duplicates, not versions

5,876 rows carry **5,876 distinct `:entity/id` values** for **1,318 distinct
names** — every id occurs exactly once. So these are genuine separate rows,
not bitemporal versions of one row. **4,558 pattern rows and 6,259 clause rows
are surplus.** Retraction is the right instrument.

### Three id conventions coexist today

| `pattern/library` (5,876 rows) | n |
|---|---|
| plain UUID | 3,934 |
| qualified name | 1,242 |
| **stringified `#uuid "…"` literal (malformed)** | **700** |

| `pattern/clause` (15,637 rows) | n |
|---|---|
| qualified name | 9,345 |
| plain UUID | 6,292 |

The 700 malformed ids are stored as the *string* `#uuid "0030a6e9-…"` rather
than an id. The same wart appears in relations (`:relation/src "#uuid \"…\""`).

### The blocker a naive drop would hit

| | n |
|---|---|
| distinct pattern names in store | 1,318 |
| ids from `@flexiarg` headers | 1,103 |
| ids derivable from file paths | 1,131 |
| union of the two (a file exists either way) | 1,179 |
| **in store, NO file** | **232 — would be destroyed by drop+reimport** |

### ⚠ CORRECTED AGAIN 2026-08-14 — the real number is **94**, and 93 are stubs

Both 255 and 232 were wrong: **the scan globbed only `*.flexiarg`**. There are
**9 `.multiarg` files** (`p4ng-orpatterns.multiarg`, `vsat-vsatlas.multiarg`,
`pacspine.multiarg`, …) holding **98 patterns** under `@arg <id>` headers.
Counting flexiarg paths + `@flexiarg`/`@arg`/`@multiarg` headers across both
extensions gives **1,324 file-backed ids** and leaves **94** store-only.

Of those 94:

| | n |
|---|---|
| **title-only stubs** (no components at all) | **93** |
| has clause entities | 1 (`ukrns/ARGUMENT`) |

The 93 are not library patterns: 71 are devmap rows `f0/…`–`f7/…` (`f0/p0` is
`:type :devmap/prototype`, source `futon3/devmap`, and is also a colliding key
in **B5**'s index), 19 are the `math/*` buckets of **A4**, 2 are named
`null/…`. **So there is essentially nothing to export — the job is one
pattern, not 232.**

### 🔴 NEW BLOCKER — `.multiarg` is watched but never ingested as patterns

`flexiarg/src-exts` is `#{"flexiarg" "multiarg"}`, so multiarg files pass
`supported-ext?` and are watched. But `dispatch!` routes only
`(= "flexiarg" ext)` to `ingest-flexiarg!` — `.multiarg` falls through to the
generic `:else` code-graph branch. **The 98 multiarg patterns are therefore
never written as `pattern/library` entities by the current path.**

Consequence for H6: a re-ingest will not refresh them, so the retract sweep
must either exclude them or — better — `dispatch!` must route `.multiarg` to a
multi-pattern ingest first. Until then those 98 patterns cannot be
watcher-maintained, which is exactly the standard Joe set.

*Superseded: an earlier pass said 255 by deriving ids from paths alone
(`parentdir/filename`). Matching against the authoritative `@flexiarg` header
as well drops it to **232**. Also found: **35 flexiarg files carry no
`@flexiarg` header at all**.*

*Not `iiching`, despite 255 ≈ 256. Checked: `iiching` is 257 in store / 257
with files (1 store-only), `iching` 64/64 (0 store-only). The near-power-of-two
is a coincidence.*

**These are real patterns, not artefacts.** `p4ng` has **58 patterns in the
store and exactly 1 file on disk** — names like `p4ng/attribution-forward`,
`p4ng/borrow-the-situation`, `p4ng/bridge-before-portal`. The library
directory `futon3/library/p4ng/` exists; the files simply were never written.
Composition of the 232: p4ng 53, or 26, math 19 (= **A4**), f2 14, pacspine 12,
f3 12, vsatlas 11, fulab 11, f6 10, f0 9, then a long tail across ~29
namespaces.

**A pattern with no file cannot be kept up to date by the multi_watcher** —
the watcher's input is files. So the 255 are not merely a data-loss risk;
they are patterns that can never satisfy Joe's requirement while they remain
fileless. (19 of them are the `math/*` buckets of **A4**.)

### Sequence

1. **Export everything first.** Full index of both types to disk — Joe:
   *"index everything, save it to disk, and then we would have something we
   could restore from."* Independent of the store backup.
2. **Export the 255 fileless patterns as `.flexiarg` files**, or get Joe's
   ruling that a subset is genuinely dead and may go. This is the step that
   turns drop+reimport from lossy into safe, and it is a **decision point,
   not an implementation detail**.
3. **Retract** all `pattern/library` and `pattern/clause` rows.
4. **Re-ingest from files** with H7's explicit ids → exactly one row per
   pattern, every id a qualified name.
5. **Verify:** row count == distinct name count; 100% qualified-name ids; no
   `#uuid`-string ids remain; spot-check content against the pre-drop export.
6. **Confirm the watcher maintains it** — edit, rename and delete one pattern
   and check the store follows. Note the two known sweep gaps: delete leaves
   the legacy hyperedge (A1) and leaves clause entities (H5).

### Original packet text

## H6 (original) — drop and reimport (blocked on H1, H3, H5)

Joe's plan: *"drop all six (or however many) reimport once, and make sure that
further imports are idempotent."* Not startable until H1 makes the reimport
affordable, H3 makes batch imports idempotent, and H5 says what to drop.
Index and save to disk first — Joe: *"obviously I don't want unnecessary data
loss."*

## Order

H1 and H2 are independent discovery — run concurrently, they answer different
questions. H3 and H4 are small independent fixes that can run alongside.
H5 waits on H4. H6 waits on H1, H3, H5.
