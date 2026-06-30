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

Only **2/15** of the mine's top missions exist as nodes under *any* scheme. So the dimensions
don't compose at the **identity** layer (below the type layer the cascade-real L1 gate checks),
and the cascade can't be a trustworthy self-model while one thing wears three names.

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
