# Pattern library additions — zai-scribe, frame f37 scribe-reduce (a96A08)

Created because no existing math library pattern fits the mined rules (the
tail-threshold rule was already on the shelf as memory e-0d5fcb3e and is not
re-coined here). This file is ingested explicitly by
`scripts/apm-ingest-coined-pattern-files.sh`; ids below are pattern ids for
attachment.

## math-formalization/filter-tendsto-const-mul-api-shape
Trigger: you are chasing a limit of the form `(fun x => c * f x) atTop/atBot`
and reach for the generic product tendsto lemmas. Move: the generic
const-times-id lemmas elaborate against a `nhds` target and leave a filter
mismatch goal; the atTop/atBot-specific forms exist as IFFs
(const-mul-atTop-of-pos / const-mul-atBot-of-pos-or-neg) and must be applied
through `.mpr`, with the additive endpoint shift supplied by the separate
map_add_atTop lemma. When const-addition at the atBot end is not packaged as a
Tendsto lemma at all, reparametrize the endpoint affine variables instead of
fighting for the missing lemma.

## math-formalization/state-integral-splits-as-standalone-haves
Trigger: a rewrite like splitting an integral then substituting a hypothesis
is run inline inside a term whose body is itself an integral expression.
Move: the rewrite can be consumed by a wrong subterm, leaving a pattern
mismatch far from the real error. State the split as a standalone `have`
with an explicitly written integrand function before rewriting, so the
rewrite target is unique.

## math-formalization/fieldsimp-ring-for-affine-endpoint-congruence
Trigger: closing an affine-endpoint congruence (two expressions for the same
shifted/scaled limit endpoint) with plain `field_simp`. Move: plain
`field_simp` can leave bare commutation goals (`mul_comm`-shaped) that it
does not discharge; call `field_simp` with the nonzero side conditions as
explicit arguments, then finish with `ring`. Repeat offenses of plain
`field_simp` on the same goal are the signature of this gap.

## math-formalization/recheck-sign-when-assembling-limit-constants
Trigger: the final statement equates a computed limit to a numeric constant
built from signs of the pieces (outer negations, orientation flips from
reparametrized endpoints). Move: recheck the sign of every constant against
the assembled expression before submitting; a dropped outer negation does not
surface as an arithmetic error but as an inexplicable failed congruence
rewrite at the end of the proof, costing a full compile cycle to even see.
