# Ψ-v2 design — pattern-level receipt-informed conductances (WS3, DERIVE)

Author: claude-6 (owner architecture), 2026-07-28. Paper-first per the
logic-model-first discipline; the replay harness implements THIS design.
Mission: `holes/missions/M-memory-retrieval.md` WS3.

## Position on the ladder

| Rung | θ granularity | Update source | Status |
|---|---|---|---|
| S6 (live) | one coefficient per **memory** | receipts `(1 + α·used/offered)` | deployed |
| **Ψ-v2 (this)** | one coefficient per **pattern** (+ per edge type when >1 exists) | receipts, attributed through the recall graph | dark design |
| Rung 2 (dark) | per pattern, one-outcome | beta-binomial posterior ratio, witnessed transition | implemented dark |
| Theory target | per **relation type** θ_r | coupled Ψ over outcomes | E-dynamic-queries |

Ψ-v2 moves the coefficient from the node (memory) to the *edge structure
recall actually traverses* (the pattern star, and the edge type when the
graph has more than one — post-backfill). This is the smallest step that
makes θ a property of the graph rather than of items, i.e. the first
honest "conductance".

## State

- θ_p for every pattern p having ≥1 current+reviewed attachment.
- θ_r per edge type r. Today r ∈ {`:pattern-attachment`} only, so θ_r is
  **declared degenerate** until the cohort-2 backfill (supersedes /
  resolves edges) lands; the harness must report the edge-type census it
  saw and mark θ_r inactive rather than silently fitting one point.

## Credit assignment (the design's one non-obvious decision)

Receipts do not record which pattern surfaced each memory
(`:memory-use/inclusion-reasons` is empty in live rows; verified
2026-07-28). Attribution is therefore reconstructed, deterministically:

1. For memory m in a receipt row, take m's current+reviewed pattern
   attachments (from the frozen graph export).
2. Restrict to attachments whose pattern id/description terms intersect
   the row's recorded `:recall-query` terms (the lexical route recall
   actually used).
3. If the restriction is empty (route not reconstructable), fall back to
   ALL of m's current+reviewed attachments.
4. Split one unit of offered-credit (and used-credit, if m ∈ used-ids)
   **uniformly** across the attributed patterns. Fractional credit is
   fine; invented precision is not — no similarity weighting.

Every row's attribution mode (`:matched` | `:fallback-all` |
`:unattributable`) is reported in the audit output. Memories with zero
reviewed attachments contribute nothing to θ (they were surfaced by the
FTS net, not the graph — that is S6's territory, not Ψ-v2's).

## Update rule

For pattern p with accumulated fractional counts (offered_p, used_p):

    θ_p = 1 + α · (used_p / offered_p)   if offered_p ≥ n-min-coeff
    θ_p = 1                              otherwise (per-coefficient abstention)

- α = 0.5 (matches deployed S6; comparability beats novelty here).
- Cold start is neutral by construction (abstention → 1).
- Bounded: θ_p ∈ [1, 1+α] before flooring; reasoned non-use lowers the
  ratio — correct, per the S6 policy, not a bug.
- When θ is used as a ranking-weight vector it passes through the
  Rung 4 lower-bounded-simplex machinery with explicit floor ε — the
  exploration-mass discipline is inherited, not re-derived.

## Calibration gates (two, and they are different)

- **Harness activation**: n-min-coeff = 5.0 fractional offered-credits
  for a θ_p to leave neutral *inside the dark replay*. Exploratory
  threshold, preregistered here.
- **Live promotion**: the Phase 6 standard — n ≥ 20 independently
  witnessed outcomes per coefficient — plus Interface-1 coordination
  (next `:recall-system` version tag, cohort boundary). The harness
  MUST emit `:promotion :below-calibration-minimum` while any live
  coefficient is under-observed; with current receipt volume the
  expected verdict is exactly that. Validating the contract without
  earning promotion is the Rung-2 precedent, deliberately repeated.

## Evaluation protocol (the harness)

1. **Frozen export**: all `:pattern-outcome` receipts by `ground-control`
   (bounded query, read-only) + the attachment graph (reuse the WS2
   export or re-export with the same discipline). Date-stamped.
2. **Join** offered↔outcome halves on `:job-id`. Unjoined halves are
   reported, not dropped silently.
3. **Three arms per joined row, leave-one-out** (θ and per-memory stats
   fitted on all OTHER rows — no self-scoring):
   - (a) no-Ψ: the surfaced order as recorded in the offered half;
   - (b) S6 scalar: per-memory `(1 + α·used/offered)` re-rank;
   - (c) Ψ-v2: pattern-level θ re-rank per this design.
4. **Metrics**: MRR of used-ids within the surfaced list + hit@1, per
   arm. Rows with empty used-ids are counted and characterized
   (`recall-empty` / `surfaced-not-usable` / reasoned non-use) but do
   not enter MRR — no metric laundering.
5. **Determinism**: identical output on rerun; ties broken by memory id.

## Explicitly out of scope

Live wiring (Interface 1); any store write; consuming Rung 3 entropies;
similarity-weighted credit (needs a validated likelihood model first);
relation-type θ fitting while the census has one edge type.
