# Pattern library additions — zai-scribe, f76 scribe-reduce (b95J02)

Created because no existing math library pattern fits the mined rules below.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids below
are pattern ids for attachment. Rules that fit existing library patterns
(math-formalization/probe-constant-namespace-qualification,
math-formalization/probe-division-lemma-variant-names) are attached there and
not re-coined.

## math-formalization/abbrev-vs-def-instance-synthesis-in-application-position

Trigger: applying a structure-theorem or module-theory lemma to a type
ascribed through an abbreviation (e.g. a module induced on a type synonym by
an algebra evaluation structure), Lean reports "failed to synthesize" a
Module / Module.Finite instance *inside the application*, even though the
exact same instance synthesizes fine in an isolated `inferInstance` or
`example` with the statement spelled out. The abbreviation unfolds at
`instances` transparency only in some elaboration positions, producing
instance-argument diamonds that hand-written `haveI`s do not unify with.
Move: pass the instances explicitly by `@`-application, with each one
`inferInstanceAs`-cast to the *non-abbreviated* spelled-out def form of the
type (the abbrev spelling fails in argument position where the def spelling
succeeds); where diamonds persist across the module structure itself, set
`set_option backward.isDefEq.respectTransparency false` before the theorem —
Mathlib's own files in this area set exactly that option. Symptom signature:
instance synthesizes at statement level but not at application level.

## math-formalization/compare-ideal-to-linear-kernel-via-restrict-scalars

Trigger: wanting to state that the kernel of a linear map into a polynomial
ring equals the (principal) ideal spanned by some polynomial. The kernel of an
R-linear map is a submodule over the scalar ring, while the ideal is a
submodule over the polynomial ring itself — the two live in different
submodule lattices and a direct equality statement is a type error, and
`Submodule.map` between the lattices repeatedly fails to elaborate
(cross-ring mismatch). Move: state the equality against
`(Ideal.span {p}).restrictScalars R`, route every membership through
`Submodule.restrictScalars_mem`, and when a rewrite under the coerced
application fails on a Quotient.mk/mkQ mismatch, replace it with an explicit
`rw [show (liftQ …) ((mkQ …) q) = f q from rfl]` since the application is
definitionally rfl.

## math-formalization/degree-bounds-via-withbot-sup-not-natdegree-fold

Trigger: bounding the degree of a polynomial built as a finite sum over
polynomial pieces. The natural first move — `natDegree_sum_le` plus
`Finset.sup_lt_iff` — fails to rewrite because the goal displays
`Finset.fold max 0 …` rather than `Finset.sup`, and the two are not
syntactically interchangeable for `rw`. Move: switch the whole bound to the
`degree` (WithBot) layer: `natDegree_lt_iff_degree_lt`, `degree_sum_le`
(which does apply to the displayed fold), and `Finset.sup_lt_iff` in
`WithBot`, using `bot_lt_coe` for a strictly-positive bound. When a degree
argument stalls on a fold/sup display mismatch, change degree layers rather
than fighting the display form.
