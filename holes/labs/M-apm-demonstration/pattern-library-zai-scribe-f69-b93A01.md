# Pattern library additions — zai-scribe, frame f69 scribe-reduce (b93A01)

Created because no existing math library pattern fits the mined rules.
Ingested by `scripts/apm-ingest-coined-pattern-files.sh`; ids below are
pattern ids for attachment.

## math-formalization/embed-via-faithful-action-on-coset-space-sum

To prove a finite group embeds in a symmetric group of a specific degree,
prefer acting faithfully on a disjoint sum of coset spaces (each quotient by
a subgroup of known index) and transporting through a faithful-action-to-
Equiv.Perm helper, over constructing explicit permutations (disjoint cycles
from a generator, conjugation homs). The coset-action kernel is computable
(normal-core / intersection arguments) with no normality hypothesis needed,
while explicit permutation construction needs a generator-lifting API that
Mathlib does not provide and costs hundreds of lines to hand-roll.

## math-strategy/check-math-before-porting-a-divisibility-route

Before porting a divisibility/factorial non-embedding argument from a
smaller case, test the arithmetic on the smallest numeric instance. A
divisibility claim that holds for one pair of parameters can fail for the
next (a product of two primes can divide a factorial well below the group
order times anything); when it fails, the honest route is a structural
witness bound (order-of-element forces support size), not a harder
divisibility chase.
