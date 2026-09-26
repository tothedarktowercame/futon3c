# CODEX-HANDOFF — WS3: Ψ-v2 dark replay harness

Mission: `holes/missions/M-memory-retrieval.md` WS3. Prepared 2026-07-28
by claude-6 (Claude owner). **Delivery: Agency bell from claude-6. When
done, bell claude-6 back with a summary + commit SHAs.** Owner reviews
after landing (author ≠ reviewer).

## Goal

Implement the dark replay harness for the Ψ-v2 design — **the design is
fixed and lives in `holes/labs/M-memory-retrieval/psi-v2-design.md`;
read it first and implement it as written.** Where this packet and the
design doc could disagree, the design doc wins. The harness compares
three ranking arms (no-Ψ / S6 scalar per-memory / Ψ-v2 pattern-level)
on frozen receipt data, leave-one-out, and reports honest numbers at
whatever the current receipt volume supports. **Expected promotion
verdict: `:below-calibration-minimum` — that is the correct output, not
a failure.**

## Files

`:in` (READ-ONLY — do not modify):
- `holes/labs/M-memory-retrieval/psi-v2-design.md` — THE SPEC.
- `holes/labs/M-typed-memories/connectivity_meter.bb` — reuse its
  bounded-query + retry + write-once conventions (and its export if
  fresh enough; otherwise re-export with the same discipline).
- `scripts/wire_math_memory_patterns.clj` — attachment/review shapes.
- `scripts/dispatch_with_recall.clj` — the S6 receipt-stats formula to
  replicate for arm (b); replicate, do not import.

`:out` (create, under `holes/labs/M-memory-retrieval/`):
- `psi_v2_replay.bb`
- `receipts-export-20260728.edn` — frozen receipt export (date-stamped,
  query URLs + limits + counts recorded; re-stamp if run another day)
- `psi-v2-replay-results-20260728.edn`
- `ws3-results-note.md` — short honest note (≤ 50 lines)

## Binding requirements (design doc restated as checklist)

1. **Read-only**: bounded GETs only (`&limit` on every query; single
   5s-backoff retry on 503/`:expensive-read-busy`); zero store writes.
2. **Join** offered↔outcome halves on `:job-id`; unjoined halves
   counted and reported.
3. **Credit assignment** exactly as designed: reviewed attachments →
   query-term restriction → uniform fractional split; attribution mode
   per row (`:matched`/`:fallback-all`/`:unattributable`) in the audit
   output; zero-attachment memories contribute nothing to θ.
4. **Update rule**: α = 0.5; per-coefficient abstention below
   n-min-coeff = 5.0 fractional offered-credits; θ_p ∈ [1, 1.5].
5. **Edge-type census reported**; θ_r marked `:inactive-degenerate`
   while the census has one edge type. Do not fit it.
6. **Three arms, leave-one-out** — θ and per-memory stats fitted
   excluding the scored row. No self-scoring.
7. **Metrics**: MRR of used-ids + hit@1 per arm over rows with
   non-empty used-ids; empty-used rows counted and characterized, not
   entered into MRR.
8. **Promotion verdict**: emit `:promotion :below-calibration-minimum`
   unless every active coefficient has ≥ 20 witnessed outcomes (it will
   not — report the actual per-coefficient counts).
9. **Determinism**: identical results EDN on rerun (given the frozen
   export); ties broken by memory id.

## Acceptance checklist

- [ ] `bb psi_v2_replay.bb` runs end to end; reruns replay from the
      frozen export (write-once, like the WS2 meter).
- [ ] Results EDN parses; contains per-arm metrics, per-coefficient
      offered/used counts, attribution-mode census, edge-type census,
      unjoined-half counts, empty-used-row characterization, promotion
      verdict.
- [ ] A tiny in-script fixture test: 3 synthetic receipt rows with
      known attachments where the correct Ψ-v2 ranking is computable by
      hand — assert it (the P3-validation discipline, applied here).
- [ ] `ws3-results-note.md` states the numbers plainly, including n,
      and claims nothing beyond them.
- [ ] `clj-kondo` 0 errors on the new .bb file; `check-parens` clean.
- [ ] `git diff --stat` shows only this packet's `:out` files.
- [ ] Bell claude-6 with summary + commit SHAs.

## Explicitly out of scope

Everything in the design doc's out-of-scope list: live wiring, store
writes, Rung 3 entropies, similarity-weighted credit, θ_r fitting on a
one-type census.
