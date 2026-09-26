# Pattern library additions — codex-scribe, frame f47 promote-solver (a97A01)

Created because the reviewed mathematics memory corpus returned no matches
for the mined rules (search receipt d06575bb44de89dc10080a94672e5f8d1688b6e20cb898fe4ee16ffe3090e307,
2026-08-27). Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment. The first two ids re-coincide with
the f39 file for the same domain so identity stays stable if both are ingested.

## math-formalization-CA/dense-class-cancellation-for-oscillatory-weak-convergence

Trigger: the goal is `Tendsto (fun n => ∫ f * gₙ) atTop (nhds 0)` for an
arbitrary `L∞`/`L¹` test function `f` against an oscillatory family `gₙ`
(Rademacher-like signs, characters, oscillating indicators) with no
almost-everywhere pointwise limit, so dominated/Vitali convergence routes are
unavailable. Move: (1) exact cancellation `∫ s * gₙ = 0` on a dense class of
step functions `s`; (2) the uniform bound `‖∫ h * gₙ‖ ≤ ∫ |h|` from `|gₙ| ≤ 1`
(`norm_integral_le_integral_norm` + `integral_congr_ae`); (3) approximate `f`
in `L¹` and split with the `ε/2 + ε/2` budget, choosing the tail index by
`max (m + 1) <lowest exceptional level>` so every later `gₙ` cancels. Reason:
convergence is checked only on the dense class, and the uniform bound is what
makes the extension legal — dense-class plus uniform-bound replaces
orthogonality; do the cellwise cancellation directly and skip the bundling.

## math-formalization-CA/dyadic-refinement-of-indicators-by-finite-cell-sums

Trigger: an integral over a coarse dyadic cell `Ico (j/2^m) ((j+1)/2^m)` must
be related to finer level-`n` cells. Move: prove the function-level indicator
refinement by partitioning: extract the general grid lemma
`Ico (a 0) (a N) = ⋃ i ∈ Finset.range N, Ico (a i) (a (i+1))` for any
`Monotone a` (`Ico_subset_biUnion_Ico` one way, monotonicity the other), match
the affine grid `a r = (j * 2^(n-m) + r)/2^n`, rewrites the endpoints with
`2^n = 2^m * 2^(n-m)`, then convert set equality to an indicator-sum identity
by `funext` + `by_cases` on membership: inside, `Finset.sum_eq_single` picks
the one inhabited subcell and disjointness kills the rest; outside, every
subcell indicator vanishes. Push the sum through `integral_finset_sum` and
cancel pairwise with an alternating-parity sum lemma
(`Finset.sum` over an even-length range of alternating ±x is 0, by induction
peeling two `Finset.sum_range_succ` at a time). Mathlib carries no dyadic
tiling lemma for cast endpoints; budget to hand-prove it.

## math-formalization-CA/cast-discipline-dyadic-endpoint-order

Trigger: a goal compares `((k : ℕ) : ℝ) / (2 ^ n : ℕ)` endpoints — proving
cells disjoint, equal, or nested. Move: never manipulate the casted reals
algebraically. Transfer the order: `(div_lt_div_iff_of_pos_right (by positivity)).mp`
turns `a/2^n < b/2^n` into `(a : ℝ) < (b : ℝ)`, `exact_mod_cast` returns to
`ℕ`, and `omega` closes the arithmetic — including the two-sided sandwich
that forces two cell indices equal. For membership exclusions in a
finite union use the same transfer per cell rather than a global
interval-arithmetic search. Reason: `Nat.cast` endpoints carry proof
obligations (`k ≤ 2^n`, `1 ≤ k`) that `omega` consumes once the inequality is
back in `ℕ`; staying in `ℝ` forces `field_simp`/`nlinarith` detours that fail.

## math-informal-CA/rademacher-system-weak-null-pairing

The Rademacher functions `r_n = sign(sin 2ⁿπt)` (±1 on alternating dyadic
cells of length 2⁻ⁿ) pair to zero against every `L¹([0,1])` function in the
limit: `∫ f · r_n → 0`. The pairing is weak-* nullness of the sign system,
not a pointwise statement — `r_n` has no a.e. limit. Any source that encodes
the sign as constant (say +1 on both odd and even cells) makes the family
constant a.e., so the claimed limit is false for any `f` with `∫ f ≠ 0`; the
repair is the genuine alternating sign. Generalizable shape: before proving a
weak-nullness claim, check the family is genuinely oscillatory in the source
statement itself — a constant or eventually-aligned family refutes it by the
same integral witness.
