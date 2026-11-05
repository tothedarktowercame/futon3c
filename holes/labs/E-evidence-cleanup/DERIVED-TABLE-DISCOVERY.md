# Discovery: an XTDB `derived` table in futon1b

Read-only investigation, 2026-11-05. Source checkout: `/home/joe/code/futon1b`
master. No live writes or unbounded live reads were made.

## 1. The evidence write path

`POST /api/alpha/evidence` parses the request, applies the penholder gate, and
calls `futon1b-evidence/write-evidence!` [futon1b_server.clj:896-910]. The
destination is the XTDB table `:evidence`: the rescue writer ultimately issues
`[:put-docs table-spec doc]` through `xt/execute-tx` [migration/ingest.clj:97-119].

The evidence document ID is caller-supplied `evidence/id` (namespaced form wins)
or a random UUID string [futon1b_evidence.clj:25-30,68-76]. The validator
requires type, claim type, and nonblank author; normalizes string/keyword types;
checks the closed origin stamp and attributed author; checks the harness; and
defaults `at`, body, and tags [futon1b_evidence.clj:32-96]. Before mutation it
also rejects missing `in-reply-to` and `fork-of` targets and duplicate IDs
[futon1b_evidence.clj:212-246]. Documents pass through the migration shape
transform and a two-stage rescue ladder, then are verified by an ID existence
query [futon1b_evidence.clj:212-221,248-272; migration/ingest.clj:88-119].

After a verified put, the only evidence-specific post-write hook is
`futon1b-text/on-append!`, asynchronously feeding the SQLite FTS/attribute/tag
sidecar [futon1b_evidence.clj:261-270; futon1b_text.clj:631-640]. That sidecar
stores body text, author/time/session attributes, tags, and a document cache
[futon1b_text.clj:225-270; futon1b_sidecars.clj:36-57]. Evidence writes do **not**
call the entity index, hyperedge endpoint index, or file-derived scope index;
those hooks belong respectively to entity mutations, hyperedge mutations, and
run artifact indexing [futon1b_entindex.clj:112-128;
futon1b_hxindex.clj:260-278; futon1b_scopeindex.clj:1-10].

Recommendation for `:derived`:

- Keep validation, deterministic/caller IDs, shape transformation, rescue,
  exact readback, valid-time support, and an append/batch route.
- Do not call evidence FTS, evidence attribute/tag, entity, hyperedge, or scope
  hooks. The point is that ordinary evidence search never sees these rows.
- Add only indexes justified by derived reads: family census and source-act
  lookup. Declare any SQLite candidate index in `futon1b-sidecars`, whose
  registry is the authority for ownership, maintenance, serving gate, and
  fallback [futon1b_sidecars.clj:17-28].

## 2. Hyperedges as the implementation pattern

Hyperedges already occupy their own `:hyperedges` XTDB table. Their builder
normalizes the type and endpoints, requires at least one endpoint, accepts an
explicit ID or derives `hx:<type>:<sorted-endpoints>`, and preserves labels and
props [futon1b_server.clj:73-123]. The single writer transforms, detects an
identical no-op, supports valid-time, uses the rescue ladder, verifies presence,
then updates the endpoint/type index and invalidates cached windows
[futon1b_server.clj:134-201].

The HTTP surface has `POST /api/alpha/hyperedge` and point
`GET /api/alpha/hyperedge/{id}` [futon1b_server.clj:878-894], batch POST (maximum
500; batchable valid-time puts share one XTDB transaction and one index update)
[futon1b_server.clj:203-274,1034-1043], bounded type/end listing
[futon1b_server.clj:1078-1131], and type census [futon1b_graph.clj:2252-2274].
Point reads use ID equality [futon1b_graph.clj:1032-1041]. Type+end reads use a
SQLite candidate index but re-check every candidate in XTDB; that route returns
no cursor [futon1b_graph.clj:1233-1288].

This is the right structural pattern to copy, not a reason to encode derived
rows as hyperedges. Build a separate validator/writer and `:derived` table, with
single, batch, point, bounded source/family, and census routes. Keep its fields
first-class so XTQL can join them to `:evidence` and `:hyperedges`. If live
family/source scans prove too costly, add a declared candidate sidecar which
always re-checks XTDB, as the hyperedge route does. XTDB 2.1.0 has no user
secondary indices; the sidecar registry records that limitation
[futon1b_sidecars.clj:7-25].

## 3. ERASE and pre-erase snapshot

The pinned engine is XTDB 2.1.0 [deps.edn:4-8]. It supports
`[:erase-docs :table id]`; the repository's transaction probe states that erase
removes every historical version and demonstrates it against `:hyperedges`
[p2_queries.clj:80-90]. A measured design note confirms an erased row disappears
even under `FOR ALL SYSTEM_TIME`, leaving no SQL-visible tombstone
[holes/DESIGN-hyperedge-scope-sidecar-2026-09-26.md:385-388].

There is no existing erase HTTP/API path. `POST /api/alpha/documents/retract`
uses `:delete-docs`, admits only entities/hyperedges/relations, and performs
their index cleanup [futon1b_graph.clj:418-529; futon1b_server.clj:980-984].
Hyperedge `hx/op=retract` is also `:delete-docs`, not erase
[futon1b_server.clj:134-155]. Evidence is deliberately absent from both paths.

An XTDB-only evidence erase would leave stale FTS rows. The text append path
upserts one ID by deleting its old FTS/attribute/tag rows and inserting a new
copy [futon1b_text.clj:233-270], but exposes no deletion hook. More importantly,
`reconcile!` computes only `store IDs - index IDs` and adds missing rows; it does
not delete `index IDs - store IDs` [futon1b_text.clj:679-698]. Thus the migration
needs one validated operation that (a) erases the named evidence IDs and verifies
absence across all system time, then (b) deletes the same IDs from `ev_fts`,
`ev_fts_map`, `ev_attr`, `ev_tags`, and `ev_doc` in one SQLite transaction. It
must record a durable migration receipt before erase, because neither XTDB nor
the current sidecar repair loop can reconstruct the removed rows.

The live store directory measured 82,550,111,658 bytes (`du -sb`), about 77 GiB.
Its current FTS database was 12,629,708,800 bytes with an 11,391,832-byte WAL;
the XTDB local log's `LOG` was 28,707,966,582 bytes (read-only filesystem
inspection, 2026-11-05). The node configuration puts both local transaction log
and local object storage under the store directory [migration/ingest.clj:46-53].
The filesystem is ext4 on an LVM logical volume (read-only `df -T`).

Plain live `cp` or `rsync` is not a consistent snapshot: it can observe XTDB
log/storage and SQLite database/WAL at different instants. The safest procedure
is a brief service stop, then copy the entire `migration-store-21` directory and
verify its byte inventory before restart. If the JVM must remain running, use an
atomic LVM snapshot of the containing logical volume, mount it read-only, and
copy the whole store from that single crash-consistent instant; keep the SQLite
WAL/SHM alongside the DB. This repository contains no supported XTDB online
backup or checkpoint command (searches for `snapshot`, `backup`, and
`checkpoint` found only query/index checkpoints and static export artifacts),
so a file-by-file live copy should not be represented as recoverable authority.

## 4. Write cost and pacing

The best checked-in post-fix measurement reports evidence POST median 84 ms and
p90 221 ms [TN-slow-repl-turns-2026-10-28.md:68-91]. Current load is materially
slower: parsing the last hour of `futon1b-zone` request-end records at 22:21Z
found 537 successful evidence POSTs (8.95/min), median 892 ms, p90 1,718 ms,
p99 2,482 ms, range 161-3,459 ms. These are end-to-end concurrent service
latencies, not isolated storage benchmarks. The server's own historical comment
puts ordinary evidence arrival around 30/min and documents long corpus reads
holding shared permits [futon1b_server.clj:690-718].

Do not migrate 600k rows as single POSTs: at the checked-in median that is about
14 hours, and at the current median about 149 hours, before contention. Copy the
hyperedge batching design: at most 500 transformed valid-time documents in one
transaction, per-item results, fallback to isolated retries, and exact bounded
readback [futon1b_server.clj:211-274]. Start at one 200-row batch followed by a
pause, measure request latency/queue health and sidecar lag, then increase only
while ordinary traffic remains within its existing thresholds. Persist a
family/id cursor so the migration is resumable and idempotent.

## 5. Proposed derived contract and one-family migration

### Row

```clojure
{:xt/id                         "derived:<family>:<stable-writer-id>"
 :derived/id                    "derived:<family>:<stable-writer-id>"
 :derived/family                :artifact-activation
 :derived/writer                "futon3c.agency.artifact-activation"
 :derived/writer-version        "<git-sha-or-schema-version>"
 :derived/source-act-ids        ["evidence-or-act-id" ...]
 :derived/subject               {:ref/type ... :ref/id ...}
 :derived/at                    "<ISO-8601>"
 :derived/body                  {...}}
```

Use the source writer's already-deterministic ID where one exists; otherwise
hash `[family writer-version canonical-source-ids canonical-body]`. An identical
replay is a no-op and a differing replay at the same ID is a 409 conflict.
`:derived/at` is also the XTDB valid-from time. Keep writer and version separate:
recomputation can create a new bitemporal version without pretending the old
reading never existed. Source act IDs are explicit join keys, not IDs parsed
from copied prompt text.

For efficient XTQL source joins, atomically write a normalized companion row per
source relationship in `:derived-source-links`, with ID
`derived-source:<derived-id>:<source-act-id>` and scalar columns for both IDs.
This avoids depending on list-membership pushdown: the evidence code records
that XTDB 2.1 list membership is not working and SQL `IN` scans
[futon1b_evidence.clj:156-186]. The derived document remains the canonical
record; link rows are verified projections and are erased with it.

### Routes

- `POST /api/alpha/derived`: validate and idempotently write one document plus
  source-link rows in one transaction; exact readback.
- `POST /api/alpha/derived/batch`: maximum 500, grouped by valid-time, with one
  result per input and isolated fallback/retry.
- `GET /api/alpha/derived/{id}`: ID equality point read, optional temporal axes.
- `GET /api/alpha/derived?source-act-id=...&family=...&limit=...&after=...`:
  join `:derived-source-links` to `:derived`, bounded and cursor-paged.
- `GET /api/alpha/derived?family=...&limit=...&after=...`: bounded family window.
- `GET /api/alpha/derived/census?family=...`: exact family count; if this scan
  is too costly, serve from a declared/rechecked family candidate index.

Every list route must refuse a full cursorless window rather than silently
truncate, matching the hard 1,000-row hyperedge window cap
[futon1b_server.clj:1078-1131]. Derived rows get no default full-text route.

### Migrate and verify one family

1. Freeze the family's classifier and writer/schema version. Take the store
   snapshot above. Record source evidence count, IDs, total body bytes, min/max
   time, and a stable digest over sorted `[id body-hash]`.
2. Page the evidence family by bounded `(at,id)` cursor. Transform each row to
   the derived shape without copying irrelevant evidence wrappers. Batch-write,
   checkpoint only after exact point/sample and count verification, and pace
   against live POST latency.
3. Verify before erase: equal ID cardinality, zero missing/extra source IDs,
   equal per-row canonical body hashes, equal source-act joins, family census,
   and temporal spot checks (first/last plus a deterministic random sample).
   Retain a manifest containing every source ID and both hashes.
4. In reviewed chunks, use the new evidence-erase operation that also deletes
   FTS rows. After each chunk verify: no evidence version under all system time,
   no rows in any five FTS tables, derived point reads present, source joins
   present, and manifest counts reconcile. Stop on the first mismatch.
5. Final verification repeats the whole-family count/digest and checks ordinary
   evidence text search no longer returns the family. Keep the snapshot and
   manifest until the next full store backup and a restoration drill succeed.

At discovery time the read-only FTS sidecar contained 824,483 rows in each of
`ev_fts`, `ev_attr`, and `ev_fts_map`, plus 2,241,700 tag rows. Its checkpoint
was `2026-11-05T20:31:19.495332901Z` and captured XTDB basis tx 28,148,978,399
(Python SQLite URI `mode=ro`, 2026-11-05). These numbers are migration baselines,
not proof of current completeness; the sidecar's own contract says coverage is
the captured basis, not the append counters [futon1b_text.clj:1260-1300].
