# SUBSTRATE dedup plan — one entity per mission (+ excursions)

kimi-1 for claude-13 and Joe, 2026-11-12. Follows Joe's "let's sort out the
duplicates … we generally don't need duplicates of anything" and the
inventory (`GCC-substrate-inventory-2026-11-12.md`, `9c6a94af6`). READ-ONLY
throughout: :7073 GET routes (limit ≤ 200) and file reads; nothing written
or signalled. PUBLIC-safe: ids, counts, code locations.

All counts from a full page of `mission/doc` (1,200 entities) and
`excursion/doc` (600) taken from :7073 earlier today, plus per-endpoint
probes. 89 external-ids have >1 `mission/doc` entity (97 extra entities).

## 1. The label pairs and where each writer derives its label

| Watcher label (roots.clj) | Ingester label (path-derived) | Dup external-ids |
|---|---|---|
| `futon4-elisp-d` | `futon4-d` | 27 pairs + 2 triples |
| `futon6-py-d` | `futon6-d` | 22 pairs + 3 triples |
| `futon5-d2` | `futon5-d` | 2 (watcher side UUID-keyed legacy) |
| `futon3-d` | `futon3-d` in **two repos** | 1 (`M-coordination-rewrite` — same external-id under `futon3-d` and `futon3b-d`; possibly two real documents, needs a look before merging) |
| scratch worktrees | `futon3c-baseline-d`, `futon3c-desktop-save-d` | 2 |
| same label, **case-split slug** | watcher keeps header case, ingester lowercases | 28 (`futon2-d` 9, `futon3c-d` 11, `futon0-d` 6, `futon3a-d` 1, `futon4-d` 1; e.g. `futon2-d/mission/G-over-cascades` vs `futon2-d/mission/g-over-cascades`) |
| UUID legacy | keyed | 4 (`M-cutover-probe` is two UUIDs, no props at all) |

Writer label sites:

- **Watcher**: `futon3c/watcher/roots.clj:25-39` `default-watch-roots`
  (`{:path … :label "futon4-elisp-d"}`); used by `multi.clj` and stamped on
  every watcher vertex. The ns docstring (roots.clj:5-11) is an explicit
  identity ruling: "a checkout is identified by its watcher label … two
  checkouts of one repo carry distinct labels".
- **Mission-scope ingester**: `futon3c/scripts/mission_scope_ingest.clj:294-303`
  `repo-name-from-path` + `mission-doc-id`: basename of the `…/code/<repo>/`
  path + `"-d"`. Never consults the watcher table.
- **Slug case**: watcher vertex-id keeps the parsed mission id verbatim
  (`file_ingest.clj:952` `vertex-id = (str label "/mission/" mission-id)`);
  ingester runs `slug` (lowercase, `mission_scope_ingest.clj:49-55`).
- Two legacy one-off scripts hardcode the path-derived labels:
  `scripts/ingest_mission_code_paths_to_futon1a.clj:42-50`,
  `scripts/ingest_excursions_to_futon1a.clj:38-46`.

## 2. Which label wins — evidence

**Winner: the watcher label (roots.clj).**

- *Declared authority*: roots.clj's docstring is the only place a label
  discipline is stated, and it says watcher labels. The ingester's
  `basename+"-d"` is a local convenience, not a decision.
- * Code: `futon4-elisp-d`/`futon6-py-d`/`futon5-d2` appear only in
  roots.clj; the path-derived forms appear only in mission_scope_ingest and
  the two legacy scripts above. inbox_zero (witness, sweeper,
  followup_validity) keys on the watcher table.
- * Data: the entire code/v05 layer (namespace/var/file vertices and
  edges) is watcher-labelled. On the mission entities themselves the
  ingester id currently carries the edge mass — probed:
  `futon4-d/mission/formal-arxana` 83 edges (all mission-scope/*),
  `futon4-elisp-d/mission/formal-arxana` 5 (code/v05/mission-doc 1,
  file→mission 4); `futon5-d/mission/categorical-code` 78 vs
  `futon5-d2/…` 7; `futon6-d/mission/differentiable-math` 12 vs
  `futon6-py-d/…` 3 — but every one of those edges is re-emitted by its
  writer (§4), so edge mass is not a reason to keep an id.
- * No stake: Giraffe keys on repo basenames, not labels; futon2/futon4
  sources reference neither label form (grep, unconfirmed for Mojira's
  data files under ~/notes).

Case rule: the canonical slug is the ingester's lowercase `slug` (980 of
the keyed mission entities already use it; the case-preserving watcher
form exists for only ~28 ids).

## 3. What depends on each entity of a pair

- **Hyperedges** (probes above): ingester id carries `mission-scope/*`;
  watcher id carries `code/v05/mission-doc` + `code/v05/file→mission`.
  Both populations are writer-emitted and idempotent.
- **Entity identity rule** (`futon1b_graph.clj:562-572` `ensure-entity-id`):
  an entity PUT takes the requested `:id`; else resolves by
  `:entity/name` (prefer matching type, then smallest id); else mints a
  UUID. A PUT **replaces the whole document**. Name-keyed ids therefore
  resolve directly; UUID ids resolve via the name bridge in routes
  (`resolve-end-id`).
- **Readers that break TODAY because of the dups**:
  `GET /api/alpha/entity?source=…&external-id=…`
  (`futon1b_graph.clj:540-560` `entity-by-external`) throws L1
  `:external-id-ambiguous` (409) when a dup pair shares the source — the
  point-lookup route the GCC load would use is already wedged for these
  89 ids.
- Giraffe/WM/Mojira/Arxana: no id-level dependency found (unconfirmed for
  ~/notes data files).

## 4. Merge plan

### 4a. Writer changes (land BOTH before any re-ingest, or the merge re-forks)

1. `mission_scope_ingest.clj` `mission-doc-id` (:298): resolve the label by
   longest-prefix path match against `futon3c.watcher.roots/watch-roots`
   instead of `basename+"-d"`. A few lines; makes the ingester follow the
   authoritative table forever, including future repos (futon7, futon1b).
2. `file_ingest.clj` `ingest-mission-doc!` (:952): apply the same `slug`
   (extract it to a shared ns) to `mission-id` when building `vertex-id`.
   Removes the 28 case-split pairs on re-ingest.
3. Same fix in `ingest-excursion-doc!` — see 4c.

### 4b. Mission merge steps (89 external-ids)

Per affected mission document, in order:

1. **Baseline counts**: entity count by type; the 89 dup external-ids
   listed (I left the full list in `/tmp/dups.edn` this session — regenerate
   from a fresh page before acting).
2. **Watcher re-emit**: one `POST /api/alpha/mc/sync-mission` per affected
   md (the route `ingest-mission-doc!` already funnels through,
   `file_ingest.clj:916-927`) → the surviving id
   `<watcher-label>/mission/<slug>` gains the watcher props (status, phase,
   title, summary, …). Where a keyed entity already exists under that id
   (ingester's), the PUT replaces it and **keeps** only watcher props —
   so:
3. **Scope re-ingest**: run the scope ingester per affected mission (its
   stale-retraction machinery is per-mission and safe). It re-emits the
   `mission/doc` entity with `mission/id` + `mission/path` (merging props
   onto the same id — confirm `ensure-entity!` merges props rather than
   replacing, `mission_scope_ingest.clj:406-410` suggests it keeps existing
   type; **verify merge-not-replace before running**) and re-emits the
   `mission-scope/*` hyperedges. Because those edges' `hx/id` is
   `hx|mission-scope|<scope-id>` (no label embedded) and futon1b writes
   hyperedges as `put-docs` keyed by `hx/id` (`futon1b_server.clj`
   write path), **the edges repoint in place** — no hand-editing.
4. **Retract the losers**: `POST /api/alpha/documents/retract` for each
   old-label / cased / UUID mission entity doc (the 97 extra ids).
5. **Verify**: `mission/doc` count drops 1,200 → ~1,103; zero external-ids
   with >1 entity; `entity-by-external` returns 200 for all 89; spot check
   `M-formal-arxana` → one entity `futon4-elisp-d/mission/formal-arxana`
   with BOTH `mission/status` and `mission/path`; its hyperedge count ≈
   83 + 5 (scope + code edges on one id).

Special cases: `M-coordination-rewrite` (futon3-d vs futon3b-d) — check
whether two real documents exist before merging; `M-cutover-probe` — two
empty UUID entities, just retract both and let the watcher re-create.

### 4c. Excursions (second section)

- **UUID ids** (all 600 excursion entities are UUID-keyed): name
  resolution already works (`ensure-entity-id` resolves by
  `:entity/name`), and edges use the name endpoint, so nothing is broken.
  If Joe wants name-keyed ids for symmetry with missions: add
  `:id (first (:endpoints vertex-doc))` at `file_ingest.clj:1048` (one
  line, exactly the T-9e pattern missions already use), then per entity:
  retract the UUID doc, re-ingest the file. Order matters — retract
  first, because the name-resolution rule would otherwise keep adopting
  the UUID id forever. 600 retractions; zero edge repoints needed.
  Cheap alternative: leave the UUIDs; document that excursion identity is
  the name. Joe's call.
- **Stray stubs** (the scope lane writes `mission/doc` entities for E- and
  C- documents because `multi.clj:74` `mission-doc-stem-pattern` includes
  them): **755 `mission/doc` entities with `E-…` external-ids** (591
  duplicate a real `excursion/doc`; 164 have no excursion counterpart —
  repos not watched or predating the watcher) and **9 with `C-…`**.
  Writer fix: in the scope ingester, when the stem is `E-`/`C-`, do not
  ensure a `mission/doc` entity (either skip the doc entity or emit the
  correct type). Then retract the stubs — but list the 164 unmatched ones
  first; retracting them leaves those E- ids with no entity at all, which
  is either correct (kind not ingested there) or a watcher-coverage gap to
  hand to the futon1b/futon7 coverage question from the inventory.

## 5. Risks

- **Re-fork mid-merge**: if only one writer is fixed, the next file event
  (watcher, 5 s poll) or next scope-lane pass recreates the duplicate.
  Both writer changes must land before step 4b.2. This is the main risk
  and it is fully controllable.
- **ensure-entity! replace-vs-merge**: if the ingester's entity write
  replaces props rather than merging, step 4b.3 would erase watcher props
  (status/phase) on every scope re-ingest — and this may already be the
  mechanism that stripped status off scope-tree entities. Verify on one
  mission (read entity props before/after a scope re-ingest) before the
  batch.
- **Readers holding old ids**: `hyperedges?end=<old-id>` returns 0 after
  retraction; the two GCC lab notes and any peripheral backend caching
  ids degrade softly (name resolution still finds the survivor). No
  hard-dependency on the losing ids found in code.
- **Scope-lane state**: re-ingest runs off detected trees in
  `futon6/data/mission-scope-trees/` via the off-cycle drainer
  (`multi.clj:88-92`); if the lane is disarmed, run the ingester manually
  per mission — do not arm/disarm as a side effect.
- **futon5 UUID missions** (`M-categorical-code`, `M-differentiable-code`):
  the watcher's PUT passes `:id`, which wins over name resolution, so the
  UUID legacy doc is NOT adopted — it must be explicitly retracted or it
  survives as a third copy.
- **409s persist until the end**: `entity-by-external` stays ambiguous for
  a pair until its loser is retracted; do the merge per-pair to
  completion rather than all re-emits then all retracts.
