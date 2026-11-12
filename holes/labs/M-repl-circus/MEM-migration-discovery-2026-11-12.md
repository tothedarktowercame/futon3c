# MEM: minus40 (:7093) → futon1b (:7073) migration — discovery and cut-over plan

kimi-2, 2026-11-12. Joe: memories "should probably be migrated into the same
store so that we don't have multiple unneeded JVMs"; "memory migration is the
priority" (M-repl-circus §DERIVE). Read-only discovery; counts, ids and code
locations only. Counts are from the two FTS sidecars, read-only SQLite
(:7093: `~/code/data/minus40/scratch-store/fts5-evidence.db`; :7073:
`~/code/futon1b/migration-store-21/fts5-evidence.db`, the live store dir per
`systemctl --user show futon1b-zone.service -p Environment`).

## 1. What :7093 holds

Not only memories. Evidence docs 13,227; hyperedges 118,971; entities 1,541.

- **Memories**: evidence type `memory` = 4,864 docs (claim-type assert 6,062
  incl. some non-memory asserts; challenge 19, discharge 2 = sorry traffic).
  Hyperedges (sidecar rows): `memory/fact` 17,752, `memory/mentions` 8,240,
  `memory/link` 1,752, `memory/version` 1,502, `memory/assert` 588,
  `memory/relation` 428, `memory/supersede-proposed` 64, `memory/sorry` 57,
  `memory/alias-proposed` 38. (The server's `/api/alpha/census?type=` reports
  about half these — 8,872 fact, 4,118 mentions, 294 assert, 754 version;
  the sidecar carries two rows per edge. Unconfirmed which the migration
  should reproduce; replay through the write API regenerates the server-side
  truth and the sidecars follow.)
- **The 象 lab batch** (this is why http.clj points at :7093): `:xiang/scope`
  32,900, `:xiang/act` 22,042, `:xiang/open` 14,890, `:xiang/answers` 4,114
  hyperedges; 4,864 `e-xiang-*` docs.
- **驴 situations**: `:lu/occurrence` 14,294 edges; `lu-situation` docs 7,158
  (`e-lu-*`).
- **Faults**: `:m40/fault` 272.
- **Entities/crystallise**: 1,541 `m40/*` entity nodes.
- Embeddings are NOT in the store: `~/code/data/minus40/embeddings/<model>/
  {ids.json,vectors.npy,meta.json}` (embed_facts.py header).
- Store size on disk: 3.7 G (log 2.4 G, storage 1.1 G, fts5 sidecar 245 M).

Write rate (docs/day by `:evidence/at`): 2026-11-05 745, 11-06 359,
11-07 397, 11-08 213, 11-09 652, 11-10 460, 11-11 538 → order 500 docs/day.

## 2. Readers and writers of :7093

Base URL is read from `MINUS40_BASE` env (default `http://127.0.0.1:7093`)
in `minus40/scripts/m40.py:41`; `brief.py`, `call_hook.py`, `crystallise.py`
share `hook.BASE`/their own BASE (unconfirmed whether all honour the env).

| client (file:line) | R/W | endpoint | how often | if pointed at :7073 |
|---|---|---|---|---|
| minus40 `ingest_hook.py:314-353` | W | POST `/api/alpha/evidence`, `/api/alpha/hyperedge` | per Claude hook event (memory file mtime) | works — same server code; writes land on the main store |
| minus40 `m40_mcp.py` (MCP server in `~/.claude.json`) | R+W | evidence/hyperedges/memory assert | per agent tool call | works; this is my seat's m40_* tool channel |
| minus40 `m40.py:49,176,401` | R | GET hyperedges, text-search | per CLI/agent call | works if :7073's FTS5 sidecar indexes memory docs (it indexes everything it sees; §3) |
| minus40 `brief.py:308,470` | R+W | hyperedges, evidence | per session start (Claude SessionStart hook in `~/.claude/settings.json`) | works |
| minus40 `call_hook.py:105` | W | POST evidence | per hook event | works |
| minus40 `crystallise.py:81-297` | R+W | entity, hyperedge(s) | manual/nightly runs | works |
| minus40 `relations.py`, `embed_facts.py`, `backfill_facts.py`, `fault_intake.py`, `judge.py`, `bench_corner_store.py` | R/W | various | batch/manual | fault_intake also reads :7070 (out of scope) |
| futon3c `agency/dog_provider.clj:12,35,64` | R+W | GET text-search per operator turn; POST evidence+hyperedge per buried memory | per operator turn / per bury | works; dual-write-safe (bury ids are content-derived slugs) |
| futon3c `transport/http.clj:12259` (SCOPE_EDGE_BASE) | R | the 象 lab-batch scope edges | per scope read | **BLOCKER until the 象 batch is copied** — the edges exist only on :7093 |
| futon3c `xiang/lu_memory.clj:43` + `scripts/lu_situations.py` | R+W | evidence + `:lu/occurrence` | per settled turn | works once lu docs/edges are copied; writes are idempotent ids? (unconfirmed) |
| futon2 `aif/repair_obligation.clj` (:7093) | R | (unconfirmed endpoint) | rare | works after copy |
| `~/.claude/settings.json` hooks (brief, call_hook, ingest_hook) | — | via the above | per session/turn | follow their scripts' BASE |

## 3. Compatibility with :7073

- **Same code**: both services run `clojure -M… -m futon1b-server` with
  WorkingDirectory `~/code/futon1b` — the same checkout, so the same version
  by construction.
- **Routes**: :7073 already serves `/api/alpha/evidence(+text-search)`,
  `/api/alpha/hyperedge(s)`, `/api/alpha/memory`, `/api/alpha/entity`
  (futon1b_server.clj:924,1463,1713,1717,1725). No route is missing.
- **FTS**: :7073's FTS5 sidecar indexes what the server writes; memory docs
  are ordinary evidence docs (`e-m40-*`), so text-search will index them like
  :7093's does. Unconfirmed: whether :7093's sidecar carries any
  memory-specific indexing options (its `fts_meta`/`ev_attr` tables look
  stock).
- **Id collisions**: none for memories — `e-m40-*` docs on :7073: **0**;
  `memory/*`, `:xiang/*`, `:lu/occurrence` hyperedge types on :7073: **0**.
  One real overlap: 1,142 of 4,864 sampled `e-xiang-*` doc ids already exist
  on :7073 (partial dual-write history) → replay must be idempotent on
  evidence id (skip or accept-same-content).

## 4. Volume

Migration adds ≤13,227 evidence docs and ~60k distinct hyperedges to a store
holding 846,671 docs / 4.9 M edges: +1.6% docs, +1.2% edges, 3.7 G against
108 G. Write rate ~500 docs/day is noise to :7073 (its watcher traffic is
larger by orders of magnitude).

## 5. Cut-over plan

One go: nothing. Everything can be gradual because ids are stable and
writes are small.

1. **Snapshot-copy :7093 → :7073 by replay through the write API.** Read
   :7093 docs in bounded pages (200/page, the fetch_since discipline),
   POST each to :7073 `/api/alpha/evidence` keyed by its own id, then the
   hyperedges the same way (server recomputes sidecars). Idempotent: skip
   ids already present with identical content (the 1,142 e-xiang overlap is
   the test case). Check: counts match per type; a sample of 20 memory docs
   byte-equal.
2. **Dual-write the memory writers for one day** (ingest_hook, call_hook,
   m40_mcp, dog bury, lu_situations): BASE envs accept a second URL, or a
   tiny proxy fans out. Check: next-day doc counts move in lockstep;
   `m40_find` answers identical on both.
3. **Switch readers** one config point at a time: `MINUS40_BASE` env carries
   the URL for the minus40 scripts (m40.py:41) — but each futon3c client has
   its own def (`dog_provider.clj:64`, `lu_memory.clj:43`,
   `http.clj:12259` SCOPE_EDGE_BASE env); flip them to read :7073 after the
   copy verifies. Check: dog recall answers unchanged on a replayed day of
   turns; `http.clj` scope reads return the lab batch from :7073.
4. Keep :7093 read-only for two weeks (say), serving as the rollback and the
   diff reference; nightly count check.
5. Stop `minus40-store.service`; leave the store dir untouched for a further
   stated period.

Clients that cannot tolerate a gap or dual-write: none found — every writer
uses content-derived or session-derived ids. The 象 lab batch (SCOPE_EDGE_BASE)
is the one client with a hard dependency on :7093-only content; its switch is
step 3, gated on step 1's copy.

## Unconfirmed / not done in the hour

- Which of the minus40 scripts honour `MINUS40_BASE` vs hardcode BASE
  (m40.py does; brief.py/call_hook.py/crystallise.py import `hook.BASE` —
  trace `call_hook.py`'s BASE before the switch).
- futon2 `repair_obligation.clj`'s exact endpoint and frequency.
- Whether :7093's FTS sidecar has memory-specific config.
- Whether `lu/occurrence` writes are idempotent under replay.
- The census-vs-sidecar factor-of-two on hyperedge counts (two row tables?).
