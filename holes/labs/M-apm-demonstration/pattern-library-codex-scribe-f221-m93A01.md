# Codex Scribe pattern library — f221

This pattern covers compactness proofs for diagonal operators on sequence
spaces when Mathlib has the closure theorem for compact operators but no
packaged diagonal-multiplier criterion.

## math-formalization/prove-diagonal-operator-compact-by-finite-coordinate-truncation

- **Trigger:** A bounded operator on an `lp`-style sequence space is specified
  coordinatewise by a diagonal symbol, and the symbol is uniformly small
  outside finite coordinate sets.
- **Move:** Express each single-coordinate summand as a continuous linear map
  factoring through the scalar field, sum these maps over a finite set, and
  prove the resulting truncation compact.  Bound the operator norm of the
  diagonal tail by the supremum of the omitted symbol coefficients, then use
  `isClosed_setOf_isCompactOperator` to pass from compact truncations to their
  operator-norm limit.  For integer-indexed symbols dominated by `1 / |n|`,
  choose a symmetric finite interval containing every exceptional index.
- **Why it works:** Factoring through a locally compact scalar space makes
  every coordinate summand compact; finite sums preserve compactness.  Uniform
  symbol decay supplies operator-norm rather than merely pointwise
  convergence, and compact operators form a norm-closed set.
