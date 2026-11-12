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

## Review (claude-13, 2026-11-12)

Checked: the readers named at `http.clj:12259` (SCOPE_EDGE_BASE, default
:7093), `ingest_hook.py:51` and `m40_mcp.py:31` (MINUS40_BASE),
`dog_provider.clj:62` ("Never the live :7073", a stated rule this
migration reverses on Joe's word of 11-12); :7073's admission code has a
`[:memory :assert]` contract (audit mode) and a stop-list that names neither
`:xiang/*` nor `:lu/*`. The note carries counts and ids only.

Amendments before step 1:
- **Throttle the copy.** Bulk writes have flooded :7073 before (2026-09-30:
  ~330 rewrites gave 504s and a near-full heap). Copy at a fixed low rate,
  pause when a write takes over 1 s, and watch the heap sampler.
- **Time must survive the copy.** minus40's tools answer "what did memory say
  on date X" (`m40_asof`, `m40_history`, `m40_state`). Step 1's check must
  include those answers on both stores for a sample of dates, not only
  `m40_find`; if they read XTDB system time rather than the documents'
  own `:evidence/at`, a replay changes them and needs valid-time writes.
- **One config point first.** Before switching, make every client read one
  variable (for example `FUTON_MEMORY_BASE`, falling back to its present
  default), so the switch is one change and the rollback is one change.
- **Delta copy instead of a day of dual writes.** Copy; switch writers; copy
  what :7093 received in between (by `fetch_since`); then switch readers.
  This needs no edit to minus40's writers beyond the base URL.
- **Resolve the factor-of-two in hyperedge counts** before using counts as
  the step-1 check.

## Must-do after the switch: make it survive a restart (Joe, 2026-11-12)

Joe: "Please add the config issues as a must-do action after the migration
🈲 otherwise we won't be able to access the migrated data if futon needs to
restart!" The switch itself (a JVM system property in futon3c, an exported
variable elsewhere) lives only as long as the processes that hold it. Before
:7093 is stopped, every one of these must hold :7073 without anyone
re-applying the switch:

- [x] **The code defaults change to :7073** in every client (done 2026-11-12:
      futon3c `6754d75bb`, minus40 `3aaf95b`; search asks
      `scope=all&tags=source/claude-code`) (futon3c
      `dog_provider.clj`, `xiang/lu_memory.clj`, `transport/http.clj`,
      `scripts/lu_situations.py`; minus40's scripts), so a restart with no
      setting lands on the migrated data. `FUTON_MEMORY_BASE` then remains
      only as the rollback.
- [x] **futon3c's unit**: not needed, since the code defaults are :7073
      and were reloaded live 2026-11-12 ~05:30Z.
- [x] **`~/.claude.json`**: the minus40 MCP server entry's `MINUS40_BASE`
      pin changed to :7073 (Joe's OK, 2026-11-12 ~05:37Z; backup in
      ~/notes/claude-13/backups/). Sessions take it at their next start.
- [x] **The environment Claude Code starts in** (checked 2026-11-12: no
      MINUS40_BASE or FUTON_MEMORY_BASE set) (the hooks in
      `~/.claude/settings.json` inherit it): no stale `MINUS40_BASE` or
      `FUTON_MEMORY_BASE` pointing at :7093.
- [ ] **A restart test**: after the above, restart a client of each kind
      (one Claude session for the hooks and MCP tools; a futon3c namespace
      reload with the property cleared) and confirm each reads :7073.
- [ ] Only then is `minus40-store.service` stopped (after its read-only
      period).

## Results of step 1, the copy (kimi-2; reviewed by claude-13, 2026-11-12 ~05:10Z)

- **Evidence:** 11,022 copied, 902 identical skipped, all 13,227 then on
  :7093 accounted for; 4 apparent conflicts were state-file phantoms from two
  concurrent copy processes, since written (0 real conflicts). The HTTP path
  ran at ~20 docs/min (permit-bound, see below); the in-process batch path
  (`futon1b/m40_batch_migrate.clj`, reusing `futon1b_migrate_derived.clj`'s
  write-batch! shape) at ~7.5 docs/s, with `:valid-from` = each doc's own
  `:evidence/at` and `futon1b-text/on-append!` per doc.
- **Hyperedges:** through futon1b's own `write-hyperedges-batch!` in-process.
  Valid time: own `said-at`/`at` for 11,021; for `:xiang/*` edges, which
  carry no time, the named turn record's `created_at` (18,712) or the copy
  time when no record was found (7,445). 16 delta edges took a turn time
  after the copy's cut-off.
- **Census:** 12 of 15 types equal; `:memory/assert` on :7073 is a superset
  (its own 1,967 plus the 294 copied); `memory/fact`, `memory/mentions`,
  `memory/version` short by 4–9 each because :7093's writers are still live.
  A final delta closes them at the switch.
- **Verify:** 20/20 memory docs byte-equal; m40 asof/history/state equal
  apart from same-day live-write skew.
- **Full-text index on :7073:** the copied docs are indexed (e-m40 1,165 of
  1,239; e-lu 7,299 of 7,377; the gap is live skew). claude-13 found two
  things the switch depends on:
  1. :7073's text search defaults to `scope=acts`, which excludes memory
     docs. Memory clients must ask `scope=all&tags=source/claude-code`
     (20 of 20 hits are memories that way); m40.py and dog_provider.clj
     do not today.
  2. That query takes ~15 s on :7073 against ~75 ms on :7093: the search
     re-checks each candidate in XTDB, and each re-check waits for one of
     futon1b's 4 query permits (`futon1b_xt.clj` `query-width 4`), which
     long scans from other clients hold almost continuously (free permits
     sampled 0, 2, 2, 0, 0; the periodic full-text catch-up has deferred 278
     times for want of 2 free permits, and its checkpoint has not moved
     since 00:44Z). **The switch is blocked until this is fixed.**

## The block lifted (claude-13, 2026-11-12 ~06:00Z, Joe: "addressing this one directly")

Three futon1b changes, all loaded live by namespace reload (no restart):
1. **`b37dbc3` Fix C for type + mission.** futon3c's scope re-ingest read
   one mission's scope edges per binder (~15 queries per changed mission
   doc) with `type + mission`, which skipped the doc projection and scanned
   XTDB: ~14 s each, holding a query permit. Now served from hx_node: 80 ms.
2. **`a59a349` query permits 4 → 8** (Joe's go; the live semaphore was
   given 4 more permits).
3. **`fcb0e63` text-search hydration by `_id = ?`.** XTDB 2.1 SQL scans the
   table for `_id IN (…)` at every size (23 s for one id on evidence, 28 s
   for two, OR 94 s) but uses the id index for `_id = ?` (0.3 s). The text
   search's `hydrate-survivors` was the one evidence hydration still using
   IN.

Memory search on :7073 (`scope=all&tags=source/claude-code`): ~22 s → 0.6–2.8 s
at 狗's limit of 50, inside its 5 s timeout (:7093: ~0.08 s). The switch can
go ahead once the clients ask with `scope=all&tags=source/claude-code`
(m40.py's text search, dog_provider.clj's two search URLs), followed by the
final delta copy and the must-do list above.

Still holding permits for long periods, not blocking: the hx index catch-up
(repairs by `_id IN` in chunks of 500, a scan each; last full run 2.6 h)
and entity-write presence checks.

## minus40's store stopped (2026-11-12 ~06:00Z)

Joe: "Can't we just make a temporary port forwarder for minus40 and remove it
right away?" The delta copy finished first (documents written since the
first run: ~100, copied; hyperedges re-run in full, 60,324 upserted, 0
failed). Then `minus40-store.service` was stopped and disabled (its store
dir `~/code/data/minus40/scratch-store` kept), and the transient unit
`m40-forward` (minus40 `scripts/m40_forward.py`, `1ba2d7f`) took its ports:
:7093 → :7073 and :7092 → :7072. Sessions started before the switch still
have MINUS40_BASE=:7093 in their MCP server; through the forwarder their
writes reach futon1b, so there is no split and no final top-up. Their memory
*search* runs the pre-switch code without `scope=all`, so it returns acts
rather than memories until those sessions restart.

**Remove the forwarder** (`systemctl --user stop m40-forward`) once no
running session was started before ~05:37Z on 2026-11-12. As a transient
unit it does not survive a reboot. The read-only period for :7093 is
replaced by this; the store dir stays as the rollback.
