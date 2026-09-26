# Memory: indicator exhaustion plus `lintegral_iSup_ae`

- Requested memory level: `tactic`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a97J01`
- Git commit: `9435997fcf4e083eb3c0a2574a77f3d462923be7`
- Evidence IDs: `e-pull-offer-f4668f1f-6771-4c57-9f64-f0188af1d558`, `e-pull-offer-01a0d924-2ebb-4fa3-927f-d098d60f3ad4`, `e-pull-offer-8a245105-69ad-4620-aeac-480e0a0ab29f`, `e-pull-offer-1a66f8fd-ff4b-42b8-81b9-239abe6a4355`, `e-pull-offer-8eeb9336-3f42-4af9-ae74-d6747c756f94`

## Memory

To turn uniform bounds on measurable truncations `A n` into a global
lintegral bound, define an ambient-measure sequence

```lean
let g : ℕ → α → ℝ≥0∞ := fun n x =>
  (A n).indicator (fun x => ENNReal.ofReal (max (f x) 0)) x
```

Then prove measurability, `A n ⊆ A (n+1)`, and hence the a.e. successor
inequality required by `lintegral_iSup_ae`. Bound every `∫⁻ x, g n x`, rewrite
the supremum of the integrals using MCT, and prove pointwise that `⨆ n, g n x`
is the untruncated positive part. In `a97J01`, `exists_nat_ge` supplies a
single natural number dominating both `|x|` and `f x`.

The direction that often needs explicit handling is the pointwise equality:
each indicator is at most the target, while for a nonnegative value one chosen
truncation contains the point and attains the target exactly.

## Demand-side vocabulary

This grounds the hungry pulls `integral indicator truncation`, `integral
monotone convergence`, `Lebesgue integrable monotone-convergence`, `set
integral indicator function truncation bounded measurable set Mathlib Lean`,
and `lintegral monotone convergence indicator measurable set truncate nonneg
integral ENNReal`. Every literal term is a promotion tag.

## Honesty boundary

This is one compiled instance (`n=1`), including symmetric positive and
negative applications inside the same theorem. Those two applications are
not independent cross-problem evidence and do not justify `n=2`.
