# Mission: M-intent-curvature

**Status:** IDENTIFY (scoped 2026-06-03 at C-substrate-completion dissolution) — **HIGH PRIORITY**, entry-gated. Not yet picked up.
**Owner:** claude-3 (E1 verify) + codex-3 (keystone exposes the live surface).
**Repo:** futon3c (keystone metric surface; the R3-mentions artifacts live here).
**Parent:** `C-substrate-completion` §9.1 (the named follow-up beyond the dissolution bar).

## HEAD (Joe, 2026-06-03, verbatim sense)

Close C-substrate-completion, but point to this follow-up as a high-priority item to do *when it's ready*, with clear entry conditions. The campaign delivered the metric and M-aif2 consumes the **base** curvature live (escrow E1 `:satisfied` in the charter's wording). This mission is the strict upgrade: promote the consumed signal to the **mentions-mixed (β>0) intent-curvature surface**, so the tension-proposer reads the cross-concern *intent-bridges* the dependency graph cannot see (`C-substrate-completion §6e`).

## The tension

- *Pull:* the mentions-file curvature is already materialized and ablated; β=0.5 demonstrably surfaces real intent-bridges absent from `fdep`. Consuming it live is the natural, high-value sequel — and the true E1-via-mentions `:satisfied`.
- *Hold:* the existing artifacts are **200-edge sample reports**, not a live full-scale queryable surface; and β is not yet chosen/ratified. Wiring aif2 to a sample would be premature. Hence: scoped, entry-gated — do it when EC3+EC4 hold, not before.

## What is already done (verified 2026-06-03, claude-3, off the keystone R3-mentions artifacts)

- `:mission/mentions-file` edges **materialized** at real scale: 782 code-mention edges, 526 files, 115 missions (`mention_projection`).
- **β-ablation verified** (`M-substrate-metric.R3-mentions-beta{0,05}.json`):
  - β=0 recovers **base curvature exactly** — max & mean abs-Δ = 0.0 over 200 checked edges (reversibility holds; the built-in baseline).
  - β=0.5 **earns its keep descriptively** — richer negative tail (β=0 collapses to ~0.016 after the 5th edge; β=0.5 keeps −0.167/−0.136/−0.083…), p90 curvature 0.091→0.306. Mission-intent coupling is visible where the dependency graph is blind.

## Entry conditions

- **EC1 ✅ met** — `:mission/mentions-file` edges materialized in the live ingest.
- **EC2 ✅ met** — β-ablation reversible (β=0 ≡ base) *and* informative (β=0.5 adds real intent-bridges).
- **EC3 ⛔ open (the gate)** — the mentions-mixed curvature exposed as a **live, full-scale queryable surface** (`curvature-at` over the full mentions-projected graph), not the current 200-edge sample report.
- **EC4 ⛔ decision** — **β chosen** (default 0.5, or exposed as a consumer-set parameter); the β=0 ablation stays the built-in baseline.

## The work (when EC3+EC4 hold)

1. **codex-3 / keystone:** expose the mentions-mixed E1 curvature as a live, full-scale queryable surface (the Newman-weighted `μ_x` file→file co-mention projection mixed by β; `β=0` recovers pre-mentions curvature exactly — the reversible ablation is the built-in baseline). Preserve O3 separation: `feeds-mu? true` / `feeds-A? false`; curvature must not make complete nodes propose, must not resolution-weight `μ_x`, must not infer `feeds-A?` from E1 edges.
2. **claude-3 / E1 verify:** confirm the live surface reproduces the sampled β-ablation finding at full scale (reversibility + intent-bridge lift).
3. **M-aif2 (thin reopen):** the tension-proposer reads the β>0 surface live → **escrow E1-via-mentions `:satisfied`** (a strict upgrade over base-curvature consumption).

## Relations

- `C-substrate-completion` §6e (the `:mission/mentions-file` relation + the `μ_x` Newman/β spec), §9.1 (this follow-up), §4 O3 (the separation invariants this must preserve).
- `M-substrate-metric` §12.4 named residue — sibling deferred work (O2 convergence R3, git-co-change blend R3.5); this mission is the mentions-curvature consumption specifically.
- Deferred further residue: the `:mentions/realized` git-co-temporal complement (the Pareto-20%).
- `M-aif2` — the consumer that reopens a thin slice to read the β>0 surface.

## IDENTIFY exit (when picked up)

Confirm EC3 (a live full-scale mentions-curvature surface exists and reproduces the sampled ablation) and EC4 (β chosen), then MAP the thin M-aif2 reopen that consumes it. Until then this stub holds the scope so it isn't winged — the floor is the delivered base-curvature consumption already live in M-aif2.
