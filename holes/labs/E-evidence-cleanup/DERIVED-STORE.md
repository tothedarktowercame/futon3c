# Moving derived records out of evidence

Joe, 2026-11-05: the copies already in evidence "need to be moved out of the evidence store ...
they are derivatives of evidence and not evidence, and therefore they need to be handled properly.
I question if they should be in XTDB or in a sidecar. The whole design remains open. But querying
for 'hearo' and 'Lionheart' etc. should return real turns not derivative turns."

## What the two searches hit (FTS sidecar, all time, 2026-11-05 22:20Z)

"hearo" 88 rows, "Lionheart" 85. Real acts among them: Joe's turns (4 / 4), claude-8 replies (5 / 4),
turn-commits (2 / 0), one parked-resume dispatch. The rest are derived:

| Family | hearo | Lionheart |
|---|---:|---:|
| artifact-activation (weak activation) | 25 | 25 |
| zai/kimi `:turn-start` prompt copies | 23 | 22 |
| 象-1 `:turn-round` (Circus seat output) | 9 | 10 |
| promise-history | 7 | 0 |
| 象 settled-turn (`:xiang-turn`) | 4 | 5 |
| context-retrieval | 3 | 2 |
| 馬 observation | 3 | 3 |
| dev `:invoke` prompt preview | 2 | 3 |
| minus40 `:memory-injected` | 0 | 7 |

## Size, all time (824,411 rows indexed)

| Family | Rows | Body bytes |
|---|---:|---:|
| promise-history | 50,767 | 375 MB |
| artifact-activation | 22,290 | 270 MB |
| zai/kimi turn-round | 127,193 | 210 MB |
| zai/kimi turn-start | 9,141 | 156 MB |
| dev invoke (start/complete/preview) | 245,232 | 137 MB |
| **chat turns + replies (the acts)** | **44,855** | **102 MB** |
| context-retrieval | 77,790 | 60 MB |
| 象 settled turn | 3,328 | 33 MB |
| 馬 observation/card | 1,312 | 7 MB |
| minus40 memory-injected | 657 | 3 MB |

The participant acts are about a tenth of the indexed bytes.

## Classification is by role, not only by tag

`:turn-round` is the Zai harness's per-round output. For a working seat (zai-2 doing a bell) the
final round is that agent's reply, an act. For a Circus seat (象-N, 猴-1, 馬, 驴, 狗) it is machinery
output, derived. So the classifier needs the seat's role (M-repl-circus animal table), not just tags.
Dispatch records (`:invoke-start`, bell/whistle `:mesh-edge` between agents) are acts; mesh edges
written by Circus readers (ma-reading, lu-rerank, ma-occurrence, turn-capture) are not.

## Plan

0. **Search returns acts (reversible, now).** The text-search endpoint gets an act allow-list as its
   default scope, with an explicit `scope=all`. An allow-list, so a new derived writer cannot leak
   back into search results.
1. **Derived store (design, Joe's assessment).** Recommendation below.
2. **Migration, per family.** Writers already stopped copying (writers 1-4, promise lifecycle step 5
   pending). For each family: copy to the derived store, verify counts and a sample, then remove from
   evidence (XTDB `ERASE`, which drops history, so a store snapshot first) and from the FTS sidecar.
   Erase is irreversible; each family's erase is confirmed with its counts before it runs.

## Recommendation for step 1: XTDB, in its own table, not a sidecar

- Derived records are joined to their source acts (`turn-evidence-id`, `turn-record-id`,
  `in-reply-to`). In futon1b they can be joined by XTQL with the acts and the hyperedges, which is
  what the GCC/XTQL direction asks for; a sidecar puts that join in application code.
- Readings are recomputed (象 `interpretation_version`, 馬 prompt versions). XTDB keeps the versions
  bitemporally; a sidecar would need its own versioning.
- Promise state already moved this way (hyperedges in futon1b), so there is one pattern, not two.
- The case for a sidecar: write load on futon1b (reads there are already slow: 0.3 s per point read),
  and volume (turn-round and invoke rows are 350 MB). Answer: many of those rows need not be stored at
  all, or only as counts; decide that per family in step 2, rather than choosing a store to absorb them.

A derived table is not full-text indexed with evidence, so search over acts stays clean by
construction; a separate index can be added for derived text if a reader needs it.

## Decision (Joe, 2026-11-05)

"OK, let's go w/ XTDB for now, that works with the GCC + XTQL direction ... and if we need to improve
XTDB itself or its tuning, we can do that." Derived records go to their own table in futon1b's XTDB.
Next: a read-only discovery of how futon1b writes evidence (table, transaction path, FTS ingest
hooks, ERASE support), then the derived table and its routes, then per-family migration.

## Step 0 done: search returns acts (2026-11-05 ~22:40Z)

codex-12: futon1b `93c9053` (`act-scope-rules` allow-list in futon1b_text.clj, applied in SQL before
the limit; responses carry `:scope` and `:scope-removed`; `scope=all` keeps the old behaviour) and
futon3c `904ccb14f` (Emacs header shows scope and excluded count; `my/evidence-landscape-search-all`).
Review fix, claude-8 futon1b `c967112`: `start-server!` registers `text-search-route` by value and does
not keep the server object, so the new route cannot reach the live server before a restart. `search`
now treats a missing `:scope` as `:acts`; zai-memory search passes `:all`; the route is registered by
var from the next restart. Reloaded `futon1b-text` and `zai-memory-1b` live. Live, last 24 h:
"hearo" 13 acts (joe, claude-8, claude-6, parked-resume), 54 removed; "Lionheart" 6 acts, 33 removed.
Until futon1b's next restart, `scope=all` over HTTP still returns acts (the old route drops the
parameter). Old `:invoke-start` rows from before writer 1 still carry 600-char prompt previews and
count as acts; they go with the dev-invoke family in the migration.

## Discovery and table build (2026-11-05 ~22:45Z)

codex-13 `384fea4b30` (DERIVED-TABLE-DISCOVERY.md). Checked by claude-8: XTDB 2.1.0 pinned
(deps.edn), `:erase-docs` probe in p2_queries.clj:80-90, evidence POST path futon1b_server.clj:896-910.
Findings that shape the plan: no erase path exists and the FTS reconciler only adds, never removes, so
the migration needs one operation that erases from XTDB and deletes the same ids from the five FTS
tables, with a receipt first; evidence POSTs currently take ~0.9 s median, so the move must be batched
(≤500) and paced; the store is 77 GiB and a consistent copy needs futon1b stopped, because the volume
group has no free extents (`vgs`: VFree 0), so an LVM snapshot is not possible.
Decided (claude-8): the row shape and `:derived-source-links` companion rows as proposed. Part A
(table, writer, single/batch POST, point GET) to codex-13 `invoke-1793917402049-13410-bccaae98`.
New routes register at server start, so they go live at the next futon1b restart, which can be the
same stop as the snapshot (and also brings `scope=all` over HTTP).
Part A done: codex-13 futon1b `b78f1fa976` (futon1b_derived.clj: validated writer, derived row and
its source links in one transaction, no-op replay, 409 on a different body at the same valid time,
later valid time = new version, batch ≤500 with per-item fallback; routes by var). claude-8 review:
test_derived 2 tests / 23 assertions pass (rerun); the FTS check counts sidecar rows directly.
Carried forward: (1) classify + verify do 2+ point reads per row, too slow for a 600k-row migration
at live read speeds, so the migration tool needs a bulk existence check; (2) a new version with
different source ids left old links current; fixed in part B (codex-13,
`invoke-1793917737924-13418-d3b6c7ee`, with list/census routes).
Part B done: codex-13 futon1b `ed91c86c9` (source-act lookup through the link rows, family window,
cursor paging, cap 1000, census; a new version ends dropped links in the same transaction and verify
checks they are gone). claude-8: test_derived 3 tests / 36 assertions pass (rerun). In memory: point
read 10 ms, 200-row batch 4.5 s. Census scans the whole table; fine now, revisit at 600k rows.
Step C (migration tool, in-JVM, batched existence checks, resumable, manifest under
~/code/storage/derived-migration/) to codex-13 `invoke-1793918042938-13422-4c65658a`; step D
(evidence erase: copy check, receipt with the full documents before erase, `:erase-docs`, delete from
the five FTS tables) to codex-14 `invoke-1793918043740-13423-909553ed`.

## Steps C and D reviewed; first family copying (2026-11-05 ~23:30Z)

Step C: codex-13 futon1b `ae1f0ba` (futon1b_migrate_derived.clj: family spec as data, cursor on
(at, id), manifest/batches/state/complete files, readback check per batch, dry run, verify!).
claude-8 review fixes, all in futon1b:
- `9d19ad9` The transform rehashed the stored `:text`. Rows written before f75fb0fff already carry
  `:text-sha256` of the FULL text (computed before truncation), so for truncated rows the copy would
  have held the hash of a 64 KiB prefix; it also added `:text-chars`, which the writer never stores.
  Now: keep the stored hash; hash only complete unhashed text; a truncated unhashed row keeps
  `:text-truncated? true`. verify! held every source row in memory (270 MB for this family); it now
  keeps a count and its 20-row sample. verify! deduplicates the manifest, since a crash between the
  manifest append and state.edn replays a batch.
- `d3da60e` Live, the `[*]` page took 103 s per 200 rows (every matching body carried through the
  sort): ~3 h for the family. Now a keys-only page (7.3 s per 1,000) and one `_id IN` hydrate
  (25 s per 1,000; point reads were 0.28 s each). Dry run: 34 s per 1,000-row batch.
- `46076d8` The pressure check read `:queue-depth`, which the snapshot does not have
  (`:requests/queued`), and one busy moment ended the run; it now waits up to 30 checks.
Tests: test_migrate_derived 2 tests / 28 assertions (each new assertion fails on `ae1f0ba`).
Live: loaded from master; run `aa-1` started ~23:20Z as a future in futon1b (`user/derived-aa1`,
check with ~/notes/evidence-cleanup/check-aa1.clj); writes take 1.4-2.3 s per 1,000-row batch, but
batches land ~2.5 min apart (source reads under live load), so ~1 h for 22,290 rows.

Step D: codex-14 futon1b `30f6b7b` (futon1b_evidence_erase.clj: refuses without a manifest entry, a
derived copy of the same family and matching source and derived body hashes; durable receipt with
the full documents before `:erase-docs`; history check after; FTS sidecar delete; outcome written).
claude-8 review fix `6513666`: the sidecar delete ran `DELETE FROM ev_fts WHERE id = ?` per id, a
full FTS5 scan each (0.6 s at 825k rows live), ~5 min of held sidecar write lock per 500-id batch;
and the history check ran two queries per id (~0.55 s each live), ~9 min per batch. Both are now one
`IN` statement per batch. New tests: the history check must see a past-valid-time-only row and a
replaced row; the manifest written by migrate-derived drives an erase end to end. test_evidence_erase
5 tests / 27 assertions; test_text_search ALL PASS. Not loaded live (`futon1b-text` changed; load
with the erase, after the snapshot).
Erase waits for: the futon1b stop and snapshot (Joe), then Joe seeing the aa-1 counts.
