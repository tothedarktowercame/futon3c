# Pattern library additions — zai-scribe, frame f46 scribe-reduce (a96J08)

Created because no existing library pattern fits the mined rules below. This
file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment. Rules that fit existing library
patterns (math-formalization-CA/measure-integration-api,
math-formalization-CA/series-evaluation-api) are attached there and not
re-coined.

## math-formalization/defeq-endpoints-no-rewrite
Trigger: a change-of-variables or reflection step leaves endpoint terms that
are definitionally equal but syntactically different (negation distributed
over inversion, cast placement), and repeated `rw` attempts (e.g. rewriting
the negated-inverse form to the inverse-of-negated form) each fail with
pattern not found. Move: stop rewriting. If the surrounding lemma application
elaborates, defeq closes the goal without any endpoint rewrite; the final
proof needs no rewrite at all. Repeated occurrence-matching failures on two
visibly-equal endpoint shapes are the signature.

## math-formalization/named-type-arg-elaboration-mismatch
Trigger: calling a generic algebra lemma (geometric HasSum and friends) with
an explicit named ring/type argument like `(K := (1:ℂ))` fails with
'Application type mismatch ... of sort Type but expected of type Type ?u'.
Move: drop the named argument and let unification infer the base structure
from the other arguments; the mismatch is an elaboration artifact of the
explicit cast, not a wrong lemma.

## math-formalization/complex-eq-neg-self-zero-route
Trigger: over ℂ (or any non-ordered field), needing `a = 0` from `h : a = -a`.
linarith cannot help (no order); `module` may normalize the goal wrongly (to
`1 = 0`) by ignoring the hypothesis over the module structure; grep for a
`eq_neg_self`-shaped group lemma may come up empty in the snapshot at hand.
Known candidate routes when the lemma is absent: `h ▸` into
`add_right_cancel`, or `by rw [h]; ring`-style manipulation via
`eq_inv_iff` analogues — several hand constructions die on rw
occurrence-matching, so budget the attempts. Currently an open question, not
a settled recipe.

## math-formalization/linear-combination-with-explicit-inverse-product
Trigger: normalizing a geometric-series value over ℂ — a goal of the shape
`(1 - r⁻¹)⁻¹ = -(r * (1 - r)⁻¹)` after `field_simp` leaves an unsolved
normalization, and `mul_eq_one_iff_eq_inv` does not apply (it is stated for
Groups; the complex field is out of scope). Move: prove the missing
inverse-product fact explicitly (`e^{-x} * e^{x} = 1` via `exp_add`) as a
`have`, obtain the unit equation from `eq_div_iff` rather than the Group
lemma, and close with `linear_combination` using the hypothesis (expect a
sign/shape guess or two on the coefficient). Rewrite `(r - 1)` to `-(1 - r)`
before `div_neg` if `field_simp` leaves the negation on the wrong side.
