# Pattern library additions — zai-scribe, frame f66 scribe-reduce (b03J01)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/derive-degenerate-prime-branch-structurally-not-by-counting

When a uniqueness statement about prime-indexed subobjects is proved by
counting congruences plus divisibility of an index, the counting route only
forces the count to be one in branches where the prime actually divides the
ambient size. In the branch where it does not divide, the count is one only
structurally — every such subobject has trivial size hence equals the trivial
one, and uniqueness holds by equality-with-trivial, not by counting. Do not
search for one uniform proof term; case-split on divisibility and give the
degenerate branch its own structural argument.

## math-formalization/falsify-generic-arithmetic-helper-by-small-counterexample

Before proving a generic arithmetic helper lemma assembled from the
constraints a proof happened to use (divides a product, congruent to one
modulo a prime factor, coprime to that prime), test it against small numbers.
The constraint set is often not determining: a coprime cofactor of the
product can itself satisfy the congruence. If a counterexample exists, the
missing hypothesis is usually a genuine structural fact about the numbers at
hand (a fixed small gap between the two factors); add it and re-test before
returning to the tactic proof, rather than fighting an unprovable statement.
