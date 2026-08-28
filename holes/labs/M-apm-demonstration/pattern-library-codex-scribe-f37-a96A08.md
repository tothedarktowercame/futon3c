# Pattern library additions — codex-scribe, frame f37 promote-solver (a96A08)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/partial-fraction-to-removable-sinc-representation

Trigger: a real integrand is trigonometric over a rational denominator that
splits into distinct linear factors with removable singularities, and the raw
quotient is undefined at those points so continuity/integrability arguments on
it stall. Move: partial-fraction the denominator and use the angle-addition
identity of the trig factor so each term collapses to a scaled `Real.sinc` at
an affine argument (Mathlib already carries `Real.sinc`, `Real.sinc_of_ne_zero`,
`Real.continuous_sinc`); prove the identity only off the two bad points, then
lift it to `=ᵐ[volume]` with `Filter.eventually_eq` of the finitely many point
masses (`volume.ae_ne`). Reason it works: the sinc normal form is continuous
on all of ℝ, so integrability and interval-integral manipulations can be run
on the extension while every conclusion transfers back to the original
function by a.e. congruence — the removable points never need a value.

## math-formalization/integrability-of-continuous-extension-by-isBigO-tails

Trigger: you must show a continuous function on ℝ is Lebesgue-integrable and
the only decay information available is a comparison against a known
integrable profile on the two half-lines. Move: build the comparison as a
norm bound `‖f x‖ ≤ C * ‖(1 + x ^ 2)⁻¹‖` on `x ≤ -2` and `2 ≤ x` (the
`abs_sin_le_one`-style bound supplies the constant), then apply
`Continuous.locallyIntegrable.integrable_of_isBigO_atBot_atTop` with
`Asymptotics.isBigO_iff` witnesses on both tails and
`integrable_inv_one_add_sq.integrableAtFilter` as the reference integrability.
Reason it works: Mathlib's API already packages the two-tail cutoff argument;
providing explicit constants via `filter_upwards` on `eventually_le_atBot` /
`eventually_ge_atTop` is the only file-specific content, and the interior
compact contributes local integrability for free.

## math-formalization/reduce-symmetric-integral-to-canonical-limit-by-affine-reparametrization

Trigger: a library or construction-target lemma computes a canonical
improper limit (e.g. the Dirichlet sinc integral `∫₀^R Real.sinc → π / 2`),
but the target integral has affinely reparametrized arguments (scaled and
shifted), so the lemma does not apply verbatim. Move: on each symmetric
truncation `Icc (-R) R`, rewrite the Lebesgue integral as an interval
integral (`MeasureTheory.integral_Icc_eq_integral_Ioc` and
`intervalIntegral.integral_of_le`), push the constant scaling out with
`intervalIntegral.smul_integral_comp_mul_sub` (the `c • f x + c'` form
matches affine arguments directly) and split with
`intervalIntegral.integral_add`; each piece is then literally the canonical
integral at a shifted endpoint, so the limit is the library value scaled.
Reason it works: affine reparametrization commutes with the interval
integral up to the constant Jacobian that the API already exposes, so no
change-of-variables theorem is needed — only endpoint bookkeeping (`ring`
closes it).

## math-formalization/evaluate-real-integral-by-aecover-symmetric-tendsto

Trigger: the goal states a Lebesgue integral over all of ℝ as a value, and
you can compute the symmetric truncation limit but not an antiderivative
over ℝ. Move: prove `Integrable` of (an a.e.-equal continuous form of) the
integrand, compute `Filter.Tendsto (fun R => ∫ x in Icc (-R) R, f) atTop`
toward the value, and conclude with
`MeasureTheory.aecover_Icc Filter.tendsto_neg_atTop_atBot
Filter.tendsto_id).integral_eq_of_tendsto`. Reason it works: `aecover_Icc`
turns the exhaustion of ℝ by symmetric compact intervals into an a.e.
covering filter, so integrability plus the one limit yields the global
integral — the standard "improper = symmetric limit" bridge, already in
Mathlib under this exact spelling.

