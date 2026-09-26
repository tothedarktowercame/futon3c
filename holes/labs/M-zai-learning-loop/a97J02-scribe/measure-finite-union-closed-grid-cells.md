# Memory: measure a finite union of closed grid cells by a.e. disjointness

- Requested memory level: `tactic`
- Lane: mathematics
- Confidence: one compiled problem instance (`n=1`)
- Problem: `a97J02`
- Git commit: `c3763609b5753bdea1a3d1172aed6f6bd55f4655`
- Jobs: `invoke-1785874642221-31-4f1e14a4`, `invoke-1785874703950-33-68989489`, `invoke-1785878598130-35-9f6c865c`
- Evidence ID: `e-pull-offer-5b8d797e-13a2-4724-ad36-f28b7a963954`

## Memory

Closed adjacent intervals are not pairwise disjoint, but they are pairwise
`AEDisjoint volume`. Use `measure_biUnion_finset₀`, not the strictly disjoint
finite-union theorem.

For distinct grid indices, order them. If `n < m`, then `n ≤ m - 1`; any
point in both cells is forced into the singleton `{n/N}`. The reverse ordering
is symmetric. Close the null-intersection goal with `measure_mono_null` and
`measure_singleton`. Each cell contributes `1/N` by `Real.volume_Icc`, so the
finite sum is `card/N`.

This applies whenever finitely many measurable pieces overlap only on
explicitly null boundaries.

## Provenance vocabulary

The mixed phase-A query was `grid partition interval union measure squeeze
theorem limit compact closed`. It returned the useful
`math-informal/monotone-approximation` plus unrelated results, so it is
provenance rather than hunger.

## Honesty boundary

One compiled grid problem supports the tactic (`n=1`). No second compiled
problem was found, so there is no cross-problem recurrence claim.
