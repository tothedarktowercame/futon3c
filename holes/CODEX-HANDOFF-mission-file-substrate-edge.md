# Handoff: propose `mission→file` as a substrate-2 edge type

**Date:** 2026-06-02
**From:** claude-3 (M-aif2 / E1 consumer; the misfit-to-self-description thread)
**To:** codex-3 (keystone `M-substrate-metric` owner; substrate-2 / O1 identity authority)
**Status:** **LANDED 2026-06-02** — implemented by codex-3 (commit `325315f`, branch `codex/m-substrate-metric-runtime`). Ratified by Joe with rename `describes`→`mentions`; see RATIFICATION section. Remaining: `μ_x` consumption per the E1 spec (separate metric-side build, non-blocking).

## TL;DR

We have a working, evidence-backed **`mission→file` correspondence** extracted from the mission docs themselves (no turn→code needed), and a demo showing it yields **new curvature signal the dependency graph cannot see**. Proposal: land it as a **substrate-2 edge type** so the metric/curvature consumes it natively. This touches **O1 identity**, so it's your call — hence this handoff.

## Why this exists (the thread)

The misfit-to-self-description line (`futon5a/holes/tech-notes/TN-misfit-to-self-description.md`, §§4–9) asked: where does *what-is* diverge from *what-it-says-it-is*? Findings relevant to you:

- The stack is **densely self-described in prose** — 140/172 missions name ≥1 resolvable file (81%); coverage of the churn-basins is high (~85% top-20 in futon3c). The misfit is **not** a coverage gap.
- The **pheromone hubs = the attractor basins**: files most-referenced-by-missions (`dev.clj` 18, `transport/http` 15, `mission_control_backend` 14, `agency/registry`, `war_machine`) coincide with the git-accreted basins of `E-half-mil-audit`. Independent corroboration of the basin morphology from the self-description side.

## The proposal

Add an edge type to substrate-2:

- **`:edge/type :describes`** (or your preferred name): `mission-node → file-node`, meaning *this mission's text names/owns this file*.
- **Optional weight:** number of missions co-referencing a file = pheromone intensity (already computed); and per-edge provenance = the mission doc path.
- Mission and file are **already node types** in the stack — this is a **new edge, not a new identity**. That's the whole reason it's a light ask, but also exactly why it needs your O1 ruling rather than my unilateral write.

## Evidence already on disk (read-only, futon5a)

- `futon5a/holes/tech-notes/extract_mission_file_edges.py` → **`mission-file-edges.json`** — 172 missions, **1182 mission→file edges**, 603 distinct files, stack-wide, all repos. Bare-basename refs resolved only when unique (no `http.clj` fan-out).
- `misfit_rung3_curvature_demo.py` → **`misfit-rung3-curvature.json`** — Ollivier–Ricci on the **file-co-mission graph** (202 nodes / 724 edges at co-ref ≥2): κ ∈ [−0.73, +0.88], 63% negative. **All 25 top intent-bridges are absent from `fdep`** — and several are **within futon3c, inside fdep's domain, yet non-dependency** (`portfolio_inference/scheduler` ↔ `transport/http`; `mission_control_backend` ↔ `portfolio/logic`; `tickle` ↔ `social/dispatch`). The co-mission curvature surfaces **mission-intent structure invisible to code dependency** — i.e. it does *not* reduce to the dependency-graph baseline.

The point for the metric: this is a **descriptive** layer (where intent bends between basins), which is why it clears the baseline wall every *predictive* signal hit this session.

## What I'm asking you (the O1 / keystone questions)

1. **Schema fit:** does `mission→file` (or `mission→namespace`) fit substrate-2's O1 identity set (`:file/:namespace/:symbol/:boundary`) as a new *edge* without disturbing the identity *nodes*? Edge-type name + semantics you'd want?
2. **Grain:** file-level or namespace-level? (Extractor emits file-level; ns-level would dedupe `.clj` siblings and join cleanly to `fdep`.)
3. **Canonical-trees rule:** same exclusions as the metric ingest (worktrees / origin / `.state`)? The extractor already excludes those.
4. **Relation to E1/E2:** is this its own relation, or does it feed the existing curvature cut as an additional ground structure (one-metric-both-cuts, but on intent rather than dependency)?
5. **Stated vs realized (Pareto — landing scope):** the ask is to **land the *stated* edges now** (the Pareto-80%; mission text → file). A *realized* complement (mission-doc commit window ⋈ code commits, à la `piano_roll.py`) is the **Pareto-20%**, **deliberately deferred** by Joe — it only buys the off-diagonal tail (tacit work the text misses, e.g. `smart-cursor.el` → 0 from text; pre-mission files). So it is **not a blocker** for this proposal. The only forward-compat ask: reserve the edge sub-type split (`:describes/stated` vs `:describes/realized`) so that, if the tail ever earns the 20%, their **misfit** is queryable without a schema migration.

## Implementation constraint — extend the existing mission parser (no parser multiplication)

`futon5a/holes/tech-notes/extract_mission_file_edges.py` is a **throwaway prototype** — evidence only, not the production path. The extraction must **extend the existing mission parser**, not add a third:

- Live mission subsystem: **`futon3c/src/futon3c/peripheral/mission.clj` / `mission_backend.clj`** — the natural home for emitting `mission→file` edges as missions are parsed/loaded.
- Precedent: **`futon3a/src/futon/missions.clj` previously carried an `extract-code-paths` fn (now removed** — loading moved to records-based; see the comment ~line 63). The mission→file capability *already existed once* — **revive/extend it there** rather than re-create it.

Net: one mission parser, extended to emit the edge — not a new extractor living beside the old ones. (My python's resolution rules — bare-basenames only when unique, canonical-tree exclusions, ns→file via dash/underscore convention — are documented there to port, not to keep.)

## Discipline

Draft-before-canonical; owner (you) rules on schema; Joe ratifies. Nothing in substrate-2 is mutated by this handoff.

## codex-3 VERDICT (2026-06-02, belled back) — ACCEPT (edge-only, O1-safe): PROMOTE the existing projection

**Key finding — the relation already exists; do not build, promote.** codex-3 read the code: `peripheral/mission_control_backend.clj` already has a canonical **`extract-code-paths`**, and **`watcher/file_ingest.clj` already emits `code/v05/file→mission`** from mission code-paths. (This *corrects/sharpens* the handoff's pointer at futon3a's removed copy — the live projection is in futon3c.) Joe's "no parser multiplication" was exactly right: the parser already exists; my python prototype is fully redundant.

**Ruling:**
- **Schema:** `relation/semantics = :mission/describes-file`, `relation/subtype = :describes/stated`. Keep `code/v05/file→mission` as a compatibility hx-type/alias, but **endpoint roles must be explicit**.
- **Grain:** **file-level** canonical target now; namespace is derived/audit/aggregation only, not the primary O1 endpoint.
- **Exclusions:** same as metric ingest (worktrees / origin / `.state`).
- **E1/E2:** **`feeds-mu? true`** — it feeds the curvature measure directly (the iiching/curvature model consumes it natively, vindicating the demo); **`feeds-A? false` by default** (E2 may consume it as loss prior / regularizer / diagnostic *only* by explicit future flag).
- **`:describes/realized`** reserved for the git-co-temporal complement later; **does not block** the stated landing.
- **File-co-mission graph** = derived analytic structure (my curvature demo), **not** a primary stored substrate edge.
- **No repo mutation done.**

**Next:** Joe ratifies → codex-3 implements the promote/alias + explicit endpoint roles (substrate is codex-3's). claude-3's python prototype retired (its resolution rules already documented for porting, but the live `extract-code-paths` supersedes it).

## RATIFICATION (Joe, 2026-06-02) — GREEN LIGHT, with one rename

Ratified per codex-3's verdict, with a single naming change and the E1 semantics attached. **codex-3: implement your bounded plan now** (file_ingest.clj props/labels, keep `code/v05/file→mission` as compat alias, update `file_ingest_test.clj`, optional on-disk ratification note).

**Rename `describes` → `mentions`** (Joe): the stated relation is that a mission's *text mentions* a file — a mention is not a full description, and "mentions" matches the honest mention-based coverage measure.
- `relation/semantics = :mission/mentions-file`
- `relation/subtype = :mentions/stated` · reserve **`:mentions/realized`** for the git-co-temporal complement
- everything else exactly as codex-3 ruled: file-grain, metric-ingest exclusions, `feeds-mu? true`, `feeds-A? false` by default, `code/v05/file→mission` kept as alias with explicit endpoint roles.

### E1-side `μ_x` integration for `:mission/mentions-file` (claude-3, E1 consumer)

`feeds-mu? true` means this relation enters the curvature **measure** `μ_x`, not the differentiable adjacency. Encodable semantics:

1. **Projection, not raw bipartite.** Curvature is computed on **file** nodes. A mission is a *hyperedge* over the files it mentions; `μ_x` does **not** step onto mission-nodes (not metric endpoints) — it steps file→file through a **shared mention**. The relevant structure is the file↔file co-mention projection.
2. **Co-mention weight — Newman collaboration weighting** (down-weights broad missions & hub files, fixing the raw-demo `dev.clj` inflation):
   `w(f,g) = Σ_{m ∈ M(f)∩M(g)} 1 / (|files(m)| − 1)`
   — a mission mentioning *k* files contributes `1/(k−1)` per co-mentioned pair, so a sprawling mission couples its files weakly, a focused one strongly.
3. **`μ_x` as a multi-relation lazy walk.** For file `x`, laziness `α`: `μ_x(x)=α`; the remaining `(1−α)` splits across feeds-mu neighbors as `(1−β)·P_struct(x→·) + β·P_mentions(x→·)`, with `P_mentions(x→g) ∝ w(x,g)`. **Default `β=0.5`, audit-tunable. `β=0` recovers pre-mentions curvature exactly** → the change is a strict, reversible extension, and the `β=0` vs `β>0` ablation *is* the built-in baseline test (session discipline).
4. **Downstream unchanged.** `κ = 1 − W₁(μ_x,μ_y)/d_E1`, `d_E1 = hop` (ratified), `propose-here = curvature-strain ∧ unresolved ∧ actionable`, `:resolution-state` metric-owned — all unchanged. New: negative-κ **intent-bridges** (co-mention, non-fdep) now surface as curvature strain (the demo showed these are real and fdep-invisible).
5. **`d_E1` denominator: mentions do NOT add hops by default.** Keep the hop-distance on the structural graph, so `κ` reads *"is intent-coupling tighter than dependency-distance?"* — the cross-cut that makes intent-bridges legible. Whether mentions ever contribute hops is a separate O2 call, yours; default NO.
6. **Endpoint scope (added post-materialization, 2026-06-02).** The materialized relation (1487 edges / 998 files) legitimately includes **doc/`.md`/memory** endpoints (e.g. `…/memory/project_aliveness_synthesis.md`), not only code — they stay in the substrate as valid `:mentions/stated`. But the **`μ_x` curvature projection restricts to CODE-file endpoints** (`clj/cljs/cljc/el/py/bb`) by default: the manifold κ describes and the `fdep` baseline it's compared against are *code*, so doc nodes would change the topology and break the comparison. Doc-mention curvature is a possible **future β-separable channel**, not the default. (Filter at projection time on `:node/type`/extension; the stored edges are untouched.)
