# E-futon1a-archivist — substrate-2 write-path governance (the archivist revival)

**Status:** IDENTIFY (charter seed, 2026-06-30). Spun out of **C-cascade-real** as the governing
mechanism for its **Clause 3** (canonical / unambiguous contents). Owner: **TBD** (Joe to assign —
substrate-2-adjacent; claude-2 owns the data model, so a *different* agent for the governance is
fine, coordinating with claude-2).

## The tension (why this exists)

C-cascade-real's whole point is *a clear understanding of what's in the system*. That is impossible
with **ambiguous contents** — and futon1a (substrate-2, :7071) currently has them. Live finding
(claude-4, 2026-06-30): the **same mission wears three node-id schemes** across writers —

| writer | scheme | example |
|---|---|---|
| capability / mission-scope ingest | **bare** `M-*` (role `:mission`) | `M-capability-star-map` |
| O3 lineage (clock_lineage) | `mission:M-*` | `mission:M-autoclock-in` |
| the mine (joint-memes) | `mission/M-*` | `mission/M-typed-holes` |

**CORRECTION (claude-2, 2026-06-30):** the "2/15 exist" figure was a **wrong-key artifact** — the
probe queried `mission:M-*`/bare `M-*`/`mission/M-*` (all 0-node islands). The **canonical scheme
is `<repo>-d/mission/<id>`, `:entity/type :mission/doc` — 708 nodes already exist** (census:
708 canonical · 162 bare-`M-*` alias · 118 `mission|M-*` legacy · 0 each for O3's and the mine's
schemes). So the missions DO exist; the dimensions don't compose only because **O3 and the mine
emit non-canonical keys**. The gate's job is to make that impossible: **reject `mission:M-*`,
`mission/M-*`, and `*-desktop-save-d`** (backup-checkout drift) at write time; canonical =
`<repo>-d/mission/<id>`. claude-2 owns the model + supplies the descriptor-registry entry + the
reject-list; this excursion owns the **gate**.

## Root cause (the management gap)

futon1's `README-archivist.md` had real governance: **Charon** (a strict ingest-gatekeeper
penholder that *enforces* contracts), a **model-descriptor registry + queue** (uncovered types
surface), and a **new-type request process** (canonical-by-default; justify divergence; declare
xtdb-canonical vs filesystem-working-copy). futon1a kept the *bones* (penholder allow-list enforced
in `system.clj`; `seed_futon1_descriptors` / `model/verify` / `repair_legacy_descriptors` ported)
but **lost the enforcement on the agent write path**: `/meta/model/registry`, `/meta/model/queue`,
penholder/charon endpoints all **404**; the hyperedge write path is gated by penholder **auth**,
not by a **canonical-id / type contract**. Empirically: as penholder `"api"` you can write
`mission:M-*` with zero pushback. Ungoverned writes → scheme drift.

## Goal

Bring the archivist discipline to futon1a so **non-canonical is a write-time gate failure, not a
weeks-later discovery**:
1. **Canonical identity/type contract** — register, per entity kind, the one canonical node-id
   scheme + type (e.g. *missions = bare `M-*`, role `:mission`*) in a descriptor registry that is
   the source of truth.
2. **Write-path gate (Charon-style)** — the hyperedge ingest path rejects or *queues* writes whose
   entity-ids/types are non-canonical or uncovered, instead of silently accepting them.
3. **Re-surface registry + queue** — the `/meta/model/registry` + `/meta/model/queue` views, so
   "what's governed / what's pending" is queryable.
4. **`futon1a/README-archivist.md`** — the doc + the new-type request process, ported/updated.

## First instance (already on C-cascade-real's critical path)
The **mission identity contract** with claude-2: ratify the one canonical mission/entity scheme +
confirm `mission-scope-ingest` as the live-trackable populator; then **fix O3** (clock_lineage) to
use it. That single canonical scheme + one write-path check is the seed of the broader governance.

## Acceptance (draft)
- One canonical scheme per entity kind, registered + documented.
- A write attempting a non-canonical mission id is rejected/queued (demonstrated).
- O3 lineage references the canonical mission node (composes with capability/scope/mine).
- `verify-live` (cascade-real) composition becomes meaningful because identities unify.

## Discipline / guardrails
- **Never restart the futon1a JVM** (I-0); reload via Drawbridge.
- Identity changes are migrations — value-time/retract aware (reuse the D3 valid-time + retract
  machinery); don't orphan existing edges.
- Coordinate with **claude-2** (M-populate-substrate-2 / the data model) — this excursion owns the
  *gate*, not the data content.

## Status (2026-06-30 — claude-2 executed migration + cutover; LIVE on :7071)

All four acceptance criteria met:
- Canonical scheme registered + documented (descriptor `:mission/doc`, id-field `:entity/name`,
  pattern `^(<13-repo allow-list>)/mission/(?!M-)[A-Za-z0-9-]+$`, queue `["^M-[^/]+$" "^mission[|]"]`).
- Non-canonical write **demonstrably rejected on the live `/entity` path**: `mission:M-*` → 400,
  `futon3c-desktop-save-d/mission/*` → 400; canonical → 200; bare `M-*` → 200 queued+logged
  (gate-queue recorded all dispositions).
- O3 lineage references canonical nodes (claude-4 verify-live `:consistent? true`).
- Populations unified: 246 UUID-keyed `:mission/doc` merged/rekeyed onto canonical (futon3c
  `431aca7`); each carries `:migrated-from`.

**Key fix (futon1a `a12b7a5`):** the gate was wired only into `open-world/ingest!` (the `/ingest`
route), but live writers use `POST /entity → run-write!`, which never called it — so the gate was
INERT on the real path. Added `open-world/gate-entity-id!` + an L4 hook in `run-write!`. Gate code
`f7fe27a`/`29a19ca`, watcher keying `66126ea`, reloaded via Drawbridge (JVM not restarted, I-0).

## Deferred cleanup (revisit at end of excursion — Joe, 2026-06-30)

Tooling: `futon3c/scripts/archivist_cleanup.bb` (penholder `joe`, gated `run-erase!`, default dry-run).

1. **246 UUID tombstones** — provably superseded (canonical node carries `:migrated-from`); the
   cleanup tool's eligible set. Evict when ready:
   `bb scripts/archivist_cleanup.bb --reason "…" --execute`.
2. **30 `mission-doc/*` stubs** (name `M-5`, `M-1`, …) — fallback stubs, **no canonical mapping**;
   need a slug→mission map or a decision to drop as garbage. NOT auto-evicted (no successor).
3. **75 + 2 non-allowlist nodes** (`futon3c-desktop-save-d`, `futon5-d2`, `futon5-health-main-d`,
   `futon2-arguing-worlds-d`, `mission-d`, …) — backup/branch checkout drift. Separate cleanup.
4. **`/meta/model/queue` HTTP endpoint** — 404 (not routed in this build); in-mem
   `gate-queue/snapshot` records correctly. Wire the view route (slice-1 follow-on).
5. **dot-drift canonical name** `futon3c-d/mission/substrate-metric.R2-curvature-report` (a `.` in
   the id) — gate rejects future dotted writes; this existing one is a normalization target.
