# Pattern library additions — zai-scribe, frame f65 scribe-reduce (b01J04)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/evaluate-a-polynomial-on-a-structured-algebra-element-by-structural-induction

When a proof needs the value of a polynomial applied to a structured algebra
element (a diagonal-shaped endomorphism, a product over an index family), and
the library packages no entrywise evaluation lemma, do not search harder for
the packaged name: prove the identity by structural induction on the
polynomial itself, choosing the induction principle whose constructors match
addition and monomials (not addition, multiplication, and constants), and
handle the monomial case with the structure's power lemmas and pointwise
product application. Two recurring instances: entrywise polynomial evaluation
on diagonal-shaped elements, and building a squarefree annihilating
polynomial as a product of distinct-root linear factors whose squarefreeness
follows from pairwise relative primality of the factors.
