# Pattern library additions — codex-scribe, frame f39 promote-solver (a97A01)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization-CA/dense-class-cancellation-for-oscillatory-weak-convergence

Trigger: the goal is `Tendsto (fun n => ∫ f * gₙ) atTop (nhds 0)` for an
arbitrary `L∞` (or `L¹`) test function `f` against an oscillatory family
`gₙ` (Rademacher-like signs, characters, oscillating indicators) that has
no almost-everywhere pointwise limit, so dominated/Vitali convergence and
`tendsto_Lp_of_tendsto_ae` routes are unavailable. Move: (1) prove exact
cancellation `∫ s * gₙ = 0` on a dense class of step functions `s` — for
dyadic oscillations each level-`m` cell refines into paired equal-length
odd/even level-`n` subcells whose contributions cancel exactly once
`n > m` and `n > 1`; (2) prove the uniform bound
`‖∫ h * gₙ‖ ≤ ∫ |h|` (from `|gₙ| ≤ 1` pointwise, via
`integral_congr_ae`/`norm_integral_le_abs_integral` style lemmas); (3)
approximate `f` in `L¹` by a dyadic step function and split the integral
by the triangle inequality with the `ε / 2 + ε / 2` budget. Reason it
works: convergence needs to be checked only on the dense class, and the
uniform bound is exactly what makes the extension to arbitrary `f` legal
— the same bound used once, not a circle. This is the `L¹`-dense-step
analogue of the Hilbert-space dense-span extension
(`math-formalization-FA/inner-product-space-api`): the dense-class plus
uniform-bound structure replaces orthogonality, and proving
orthogonality separately would require the same cellwise cancellation,
so do the cancellation directly and skip the bundling.

## math-formalization-CA/dyadic-refinement-of-indicators-by-finite-cell-sums

Trigger: an integral over a coarse dyadic cell `Ico (j/2^m) ((j+1)/2^m)`
must be related to finer level-`n` cells, e.g. to pair odd/even subcells
for cancellation. Move: prove the pointwise indicator refinement
`(Ico (j/2^m) ((j+1)/2^m)).indicator 1 t = ∑ r ∈ Finset.range (2^(n-m)),
(Ico ((a+r)/2^n) ((a+r+1)/2^n)).indicator 1 t` with `a = j * 2^(n-m)`,
by `funext` + `by_cases` on membership: on the cell, the covering
`bUnion` gives one inhabited subcell and disjointness (`div_lt_div_iff_of_pos_right`
on cast endpoints plus `omega`) kills the others; off the cell every
subcell is disjoint. Then push the sum through the integral with
`integral_finset_sum` and cancel pairwise via a parity sum lemma
(`Finset.sum` over an even-length range of alternating ±c is zero).
Reason it works: `2^n = 2^m * 2^(n-m)` makes the refinement an exact
finite partition with no measure-theoretic residue — half-open `Ico`
cells tile without overlap, so the bookkeeping is arithmetic
(`nlinarith`/`omega`) once the cast discipline `(Nat.cast : ℕ → ℝ)` is
fixed. Mathlib carries no specialized dyadic-cell tiling lemma for cast
endpoints, so this refinement is always hand-proved; budget for it
rather than searching.
