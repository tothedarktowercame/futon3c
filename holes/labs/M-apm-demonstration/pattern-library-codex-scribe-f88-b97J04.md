# Codex Scribe pattern library — f88

These patterns were added because the reviewed mathematics-memory search found
nearby generalized-eigenspace and commuting-family patterns, but none covered
using uniqueness after restriction to identify a semisimple summand, or
transporting an additive semisimple–nilpotent decomposition to a multiplicative
semisimple–unipotent one.

## math-formalization/identify-a-semisimple-summand-by-restricted-uniqueness

- **Trigger:** A commuting semisimple–nilpotent decomposition must be identified
  on a generalized eigenspace, but direct expansion of powers or a binomial
  argument would create substantial noncommutative algebra bookkeeping.
- **Move:** Restrict both summands to the intrinsic maximal generalized
  eigenspace, compare the restricted decomposition with the scalar-plus-
  nilpotent decomposition there, and invoke uniqueness of a commuting
  nilpotent/semisimple sum.
- **Why it works:** The shifted original operator is nilpotent on its maximal
  generalized eigenspace, scalar multiplication is semisimple, and every
  commuting summand preserves that subspace. Uniqueness therefore identifies
  the restricted semisimple summand with the scalar operator, which is exactly
  the desired eigenspace statement.

## math-formalization/normalize-additive-jordan-data-to-a-multiplicative-decomposition

- **Trigger:** An automorphism has a commuting additive decomposition into a
  semisimple part and a nilpotent part, and the goal asks for a commuting
  semisimple–unipotent factorization with uniqueness.
- **Move:** First prove that the semisimple additive part is bijective. Turn it
  into a linear equivalence, define the unipotent factor by left-normalizing the
  original automorphism with its inverse, and translate every competing
  multiplicative factorization back to an additive one using
  `D = A * (B - 1)`.
- **Why it works:** Commutation makes the normalized difference from the
  identity a product of a commuting inverse with the nilpotent part, hence
  nilpotent. Conversely, the translated `D` commutes with `A` and is nilpotent,
  so additive uniqueness identifies `A`; cancellation then identifies `B`.
