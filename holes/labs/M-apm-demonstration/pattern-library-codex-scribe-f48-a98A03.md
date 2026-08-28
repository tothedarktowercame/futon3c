# Pattern library additions — codex-scribe, frame f48 promote-solver (a98A03)

Created because the reviewed mathematics memory corpus returned no matches for
the mined rules (search receipt
125daa9fd9758e0f0c530e82ec18524387c20bb972f76fe7b5e13fb75ca53e36, 2026-08-27).
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids below
are pattern ids for attachment.

## math-formalization-CA/monotone-surjection-continuity-for-compact-interval-maps

Trigger: the goal is `ContinuousOn F (Icc a b)` for a function built as a
monotone envelope (supremum extension, distribution function of a measure,
cumulative sum) and a direct ε-δ or `ContinuousOn`-constructor attack has no
purchase. Move: transport to the subtype `{x // x ∈ Icc a b}` with
`continuousOn_iff_continuous_restrict`; on the subtype prove two cheap facts —
`Monotone F` (from monotonicity of the envelope on ℝ) and
`Function.Surjective F` onto the subtype (from surjectivity of the core map
composed with an interval-membership witness) — then close with
`Monotone.continuous_of_surjective` (Mathlib,
`Topology/Order/MonotoneContinuity.lean`; requires `DenselyOrdered` codomain,
satisfied by ℝ) and coerce back with `.subtype_val`. Reason: on a densely
ordered codomain, monotonicity plus surjectivity is exactly the hypothesis
that kills jump discontinuities, so no estimate is needed; the only work is
moving the endpoint-membership obligations into the subtype value.

## math-formalization-CA/first-differing-digit-order-for-based-expansions

Trigger: two real numbers are given as base-`b` digit streams
(`ℕ → Fin b` under `Real.ofDigits`) and the goal is an order or equality
comparison between their values — e.g. proving a digit-to-value map is
monotone. Move: locate the first differing index `n` (`Nat.find` on
`Function.ne_iff`, prefix agreement from `Nat.find_min`); rewrite both values
with `Real.ofDigits_eq_sum_add_ofDigits … (n + 1)` and
`Finset.sum_range_succ`; equate the prefix sums with `Finset.sum_congr`
(hypothesis `∀ i < n, a i = b i` discharged by `simp` under the digit
definitions); compute the two differing `Real.ofDigitsTerm` values by `simp`;
bound the two tails with `Real.ofDigits_le_one` and `Real.ofDigits_nonneg`;
finish the resulting affine inequality in the common prefix and the common
positive factor `(b ^ (n + 1))⁻¹` with `nlinarith`. Reason: the first
differing digit carries a full place-value weight while the swapped tails are
confined to `[0, 1]` in tail units, so the comparison reduces to one
`nlinarith` goal after the split; never attempt to compare the streams
pointwise or through the `Real.fromBinary` bundled form, whose coercion
traps `rfl`.

## math-formalization-CA/cantor-staircase-by-composition-not-by-name

Trigger: a construction needs the Cantor function / devil's staircase
(continuous, monotone, locally constant off `cantorSet`, measure-zero
derivative support), and searches confirm Mathlib has no named
`cantorFunction`. Move: do not rebuild the staircase from ternary iterates;
factor it as a composition of pieces Mathlib already has — the canonical
homeomorphism `cantorSetHomeomorphNatToBool : cantorSet ≃ₜ (ℕ → Bool)` and
the binary value map `Real.fromBinary : (ℕ → Bool) → unitInterval` (giving
`Real.fromBinary_continuous` for core continuity and
`Real.fromBinary_surjective` for the surjectivity half), then extend to ℝ as
`sSup (core '' {z | (z : ℝ) ≤ x})`. The supremum extension is monotone by
`csSup_le_csSup`, and agreement with the core on `cantorSet` is what feeds
the monotone-surjection continuity route. Reason: the staircase exists in
Mathlib as a composition even though it exists under no name; a file boundary
comment asserting the object is absent is a claim about names, not about
compositions, and refuting it costs two `#check`s.

## math-formalization-CA/tagged-sum-riemann-via-mvt-estimates

Trigger: Riemann integrability is stated by a custom tagged-partition
predicate (`∀ ε > 0, ∃ δ > 0, ∀ fine partition, ∀ tags in cells,
|tagged sum − I| < ε`) and the goal is a fundamental-theorem identity for
`f` whose derivative exists on the open interval. Move: run the predicate as
an oracle — extract `δ` from it against the error `|f 1 − f 0 − I|`, produce
one sufficiently fine uniform partition, obtain per-cell mean-value tags from
Mathlib's `exists_hasDerivAt_eq_slope` (wrapped so it returns exactly
`f b − f a = f' t * (b − a)`), telescope the adjacent increments over `Fin n`
to `f 1 − f 0`, and feed the tagged family to the predicate's own estimate
for a contradiction via `lt_irrefl`. Reason: the predicate quantifies over
all tagged fine partitions, so it must be consumed by exhibiting one
particular partition and tag family; the mean-value theorem supplies tags
whose sum telescopes to the endpoint difference, and no integral theory
(`∫ … ∂volume`, `HasIntegral`) is needed on that branch at all.
