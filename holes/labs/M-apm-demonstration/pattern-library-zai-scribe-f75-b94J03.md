# Pattern library additions — zai-scribe, frame f75 scribe-reduce (b94J03)

Created because no existing math library pattern fits the mined rules below.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids below
are pattern ids for attachment. Rules that fit existing library patterns
(math-formalization/notation-semantics-traps,
math-formalization/defeq-endpoints-no-rewrite,
math-formalization/transport-splitting-field-structure-through-uniqueness) are
attached there and not re-coined.

## math-formalization/splits-rootset-api-takes-mapped-polynomial

Recent Mathlib's `Polynomial.Splits` takes no ring-hom argument — it is
`Splits f` with the field implicit — and `card_rootSet_eq_natDegree` applies to
`Splits (p.map (algebraMap F K))`, not to the unmapped `p`. Scratch written
against the older shape (`IsAlgClosed.splits_codomain`-era signatures) fails
wholesale with unrelated-looking errors before any mathematics happens. When a
splitting/root-count proof stalls at the statement level, check which
`Splits`/`rootSet` generation the snapshot is in and rewrite the statement to
`p.map (algebraMap F K)` before building the body. Deprecations in this area
(`splits_codomain` → `splits`) still compile, so a deprecated warning is a
generation tell, not a failure.

## math-formalization/two-element-mulequiv-from-dichotomy

When a two-element group identification must be built by hand (e.g. an
automorphism group ≃* Multiplicative (ZMod 2) with no packaged Mathlib
equiv), the reliable route is: prove the dichotomy lemma first (every element
of the source is one of two named witnesses, via the algebra: image of the
generator squared forces ±, then a re/im decomposition pins the element); then
define the map by `split`-ing on the dichotomy — `split`, not `if_pos`/`if_neg`,
whose rewrites fail on classical Decidable instances — and close
multiplicativity case-by-case, wrapping the identity witness with `one_mul` /
`mul_one` since `1` and `refl` are not syntactically equal. Reach for this
whenever the group is visibly {id, one explicit involution} and the library has
only the involution (e.g. `conjAe`), not the identification.

## math-informal/avoid-adjoin-rootset-membership-via-algebra-equiv-route

Trying to prove a concrete field is the splitting field by showing a generator
(e.g. a non-real root, or I) lies in `adjoin` of the root set is a from-scratch
trap: the library anchor is only `adjoin_I`-shaped and needs the generator
itself inside the adjoin, with no transport lemma. The compiling alternative is
the algebra-equiv route: enumerate the candidate splitting-field models with a
`nonempty_algEquiv_or`-style dichotomy, refute the wrong branch by counting
roots in it, install the concrete model with `IsSplittingField.of_algEquiv`,
then transport automorphism computations across with `AlgEquiv.autCongr` and
carry Galois structure with `IsGalois.of_algEquiv`. Never construct adjoin
membership witnesses when an equiv between the two models can be produced.
