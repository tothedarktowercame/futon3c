# Codex Scribe pattern library — f74

These patterns were added because reviewed mathematics-memory search found no
coherent parent for the certified finite-abelian-group moves.

## math-formalization/recover-invariant-factors-from-power-kernel-cardinalities

- **Trigger:** Two normalized cyclic-product presentations of the same finite
  commutative group must be shown equal, while product/cardinality alone does
  not determine their factors.
- **Move:** For every natural `k`, compare the cardinality of the kernel of the
  `k`th-power homomorphism.  On a cyclic factor of order `n` this cardinality
  is `gcd n k`, and on a finite product the cardinalities multiply.  Recover
  the last factor of each divisibility chain from equality of all resulting
  gcd products, cancel it, and recurse on `dropLast`.
- **Why it works:** Power-kernel cardinality is intrinsic and invariant under
  multiplicative equivalence, but its value on a normalized cyclic product is
  an explicit arithmetic profile.  Saturating the profile at a last factor
  detects which entries divide it; positivity permits cancellation and
  recursively determines the whole chain.

## math-formalization/split-a-maximal-cyclic-factor-by-character-extension

- **Trigger:** An induction on the cardinality of a finite commutative group
  needs a proper kernel and a cyclic direct factor whose order is the ambient
  exponent, but the library exposes no normalized invariant-factor theorem.
- **Move:** Choose an element whose order is the exponent and take its cyclic
  subgroup.  Identify that subgroup with the complex roots of unity, extend
  its faithful character to the ambient group, prove the extension remains in
  the original character image by the exponent equation, and invert the
  faithful character on that image.  The resulting retraction splits the
  ambient group as its kernel times the cyclic subgroup.
- **Why it works:** The extended character turns analytic divisibility of the
  circle group into an algebraic retraction.  A retraction has an explicit
  kernel-product equivalence, and a nontrivial cyclic target makes the kernel
  strictly smaller, supplying the descent needed for strong induction.
