**Status:** INSTANTIATE in progress (2026-05-01) for the inventory-snapshot sibling; siblings to Codex.
**Family:** `evidence-per-turn` (proposed; alternatively under a new family `observability-stewardship`)
**Algorithm trial run:** iteration-3 of `~/code/algorithms/next-invariant.md`. Closes pipeline-tracer `track-4-2-snapshot-as-evidence`.
**Home-repo:** futon3c

# M-state-snapshot-witness: Periodic point-in-time projection as queryable evidence

## 1. IDENTIFY

### Motivation

`track-4-2-snapshot-as-evidence` (pipeline-tracer; M-invariant-queue-extend) has been the rank-top open tracer in the candidate queue. Original ask: *"Either: extend `:family-fired` body with full inventory snapshot maps, OR ship a separate `:event :inventory-snapshot` shape. Decision + implementation lands as a checkpoint."*

The decision (this mission's IDENTIFY): **separate `:event :state-snapshot` shape, namespace-discriminated subtype `:event :inventory-snapshot`** for the first sibling. Reasons:
- Conflating snapshots with `:family-fired` mixes per-event deltas with cycle-boundary state. Two distinct event types are clearer for downstream consumers (HUD, time-travel queries, inventory diff).
- Per-event snapshot would be wasteful (most events don't change inventory).
- A separate event type slots cleanly into the canary apparatus's existing tagging conventions.

### Theoretical anchoring

- **State-snapshot-witness shape (just authored).** See `futon3/library/invariant-coherence/state-snapshot-witness.flexiarg`. Per-cycle full-state projection as one evidence entry; complements per-event deltas.
- **Reachable-from-boot composition.** Snapshot creation is part of the bootstrap construction path. The snapshot evidence becomes the structural witness that boot-time construction worked.
- **Companion to `family-canary`.** The canary emits per-family deltas; the snapshot emits per-cycle state. Together they reconstruct full history.

### Scope in

- **Sibling invariant `state-snapshot-witness/inventory`** under family `evidence-per-turn`. The first worked example.
- **Implementation:** `futon3c.logic.snapshot/snapshot-inventory!` reads `docs/structural-law-inventory.sexp`, projects each family + sibling-invariant to a flat map, emits one `:event :inventory-snapshot` evidence entry through the boundary.
- **Cadence:** boot-time (wired into `bootstrap.clj`); operator-on-demand via `snapshot-inventory-now!`.
- **Tagging:** `[:invariant-queue :state-snapshot :inventory]`. Subject `:ref/type :pattern :ref/id "state-snapshot/inventory"`.
- **Tests:** test/futon3c/logic/snapshot_test.clj covering empty-input, real-stack-pass, evidence-emit-shape.
- **Inventory entry:** `state-snapshot-witness/inventory` at `:status :operational-when-enabled`.
- **Tracer closure:** emit `:pipeline-tracer-closed` for `track-4-2-snapshot-as-evidence` with resolution recording the decision + implementation.

### Scope out

- **`state-snapshot-witness/registry`** (family-check-fns at boot) — handed to Codex.
- **`state-snapshot-witness/repo-refs`** (git-state per repo) — handed to Codex.
- **`state-snapshot-witness/hud-render`** (HUD last-render) — handed to Codex.
- **Hourly periodic snapshots** — boot-only is enough for the first sibling.
- **Snapshot diff tooling** — separate concern; snapshots are queryable, diffs are operator-driven.

### Completion criteria

1. `state-snapshot-witness/inventory` registered in inventory at `:status :operational-when-enabled` with full triples.
2. `futon3c.logic.snapshot` namespace exists with `snapshot-inventory!` boot-callable.
3. Bootstrap.clj wires it after the registrar block.
4. Tests cover at least the emit-shape and the no-throw-on-clean-stack paths.
5. **Tracer `track-4-2-snapshot-as-evidence` closes via `tracer/emit-tracer-closed!`** with resolution recording the decision + commit reference.
6. HUD widget shows MOVING (open: 5, closed: 1, canary fires: ≥10) instead of STUCK.
7. One Codex handoff opened scoping the three remaining siblings.

### Relationship to other missions

- **Closes a pipeline-tracer:** track-4-2 from M-invariant-queue-extend. Tests the queue-flow signal.
- **Pattern home:** `futon3/library/invariant-coherence/` gets its fifth shape.
- **Algorithm trial run:** iteration-3 of `~/code/algorithms/next-invariant.md` against the rank-top tracer rather than the rank-top priority-queue candidate. Different queue surface, same algorithm.
- **Foundation:** depends on M-reachable-from-boot (snapshot creation belongs in the bootstrap construction path).

### Owner and dependencies

- **Owner:** Joe (architectural authority on snapshot scope) + claude-11 (worked example for inventory) + Codex (siblings via handoff).
- **Primary repo:** futon3c.
- **Touches:** docs/structural-law-inventory.sexp, src/futon3c/logic/, dev/futon3c/dev/bootstrap.clj.

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

INSTANTIATE in progress for the inventory-snapshot sibling.

## Checkpoints

### 2026-05-01 — mission opened with shape-first IDENTIFY (tracer-closing run)

- IDENTIFY complete; state-snapshot-witness shape named and lifted into `futon3/library/invariant-coherence/state-snapshot-witness.flexiarg`.
- Decision: separate `:event :inventory-snapshot` shape (vs extending `:family-fired`).
- Three-sibling scope: inventory (worked example, this mission), registry / repo-refs / hud-render (Codex handoff).
- Pending: implementation, test, inventory entry, bootstrap wire-in, tracer closure, HUD verification.
