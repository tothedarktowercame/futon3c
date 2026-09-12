# Pattern library additions — zai-scribe f197, m00A06 scribe-reduce

Created because no existing math library pattern fits these mined rules.
Ingest via `scripts/apm-ingest-coined-pattern-files.sh`; ids below are pattern
ids for attachment.

## math-formalization-CA/a.e.-vs-pointwise-domination-transfer
When an a.e. congruence under a restricted measure is needed to transfer
integrability and the filter lemmas you expect do not exist, abandon the a.e.
route and transfer by pointwise domination: state |g| <= |h| for every point
(possibly after a case split at the singular set), then use `Integrable.mono`
with `Filter.Eventually.of_forall`. Strictly simpler and compiles where
`filter_upwards` machinery fails.

## math-formalization-CA/linear-functional-via-submodule-extension
An integral formula that is only well-behaved on nice functions (integrals are
junk on non-integrable ones) cannot be bundled as a linear map on the whole
function space. Restrict: prove the nice functions form an explicit Submodule
(closure under add and const_smul, including manual compact-support arguments
where lemmas like a const_smul support rule are absent), prove additivity and
homogeneity only there, then extend with `LinearMap.exists_extend`. The frozen
statement only needs agreement on the nice subspace.

## math-formalization-CA/integral-api-argument-shapes
Set-integral additivity/smul helpers do not exist under their natural names;
go through `integral_add`/`integral_const_mul` on the restricted measure, with
`setIntegral_congr_fun` to normalise integrands first. `integral_add_compl`
takes the MeasurableSet as its FIRST argument, and without an explicit
`(s := ...)` the endpoint metavariables stay unresolved and exact-style terms
fail to unify. `integral_congr_ae` removes a constant summand like `phi 0 = 0`
without splitting the integral.

## math-formalization-CA/measurable-representative-for-discontinuous-sign
A kernel like `|x|^-1 * x` is discontinuous at 0, so continuity-based
measurability automation fails and no `measurable` lemma for the sign function
is findable. Use a piecewise measurable representative (`Measurable.ite` with
`measurableSet_lt`; note plain `measurable_if` does not exist) and repair the
mismatch by a.e. congruence off the singleton singular set.

## math-formalization-CA/instance-diamond-integral-rewrite-residual
When a set-integral rewrite fires but leaves a residual equality of two
structurally identical integrals that `rfl`/`congr`/`ac_rfl` all refuse, the
two sides carry non-unifiable instance arguments (e.g. a new integrability
class layer changing how NormedSpace/measurable-space instances elaborate).
Do not grind the residual: restructure the proof so both sides elaborate from
the same term — factor integrability into a named lemma, use congruence
lemmas that rewrite inside the integrand, and apply the splitting lemma with
fully explicit arguments.
