# Codex Scribe pattern library — f89

These patterns were added because the reviewed mathematics-memory search found
patterns about restricting Sylow subgroups to normal subgroups and about
equicardinal fibers, but none covered lifting subgroup existence through a
finite quotient or conjugating a subgroup container so that it contains a
prescribed Sylow subgroup.

## math-formalization/lift-a-subgroup-from-a-finite-quotient-by-comap

- **Trigger:** A normal subgroup is known, and a subgroup of a desired prime
  order can be produced in the finite quotient, but the goal asks for a
  subgroup of the original group with the product cardinality.
- **Move:** Produce the quotient subgroup by Cauchy's theorem, comap it along
  the quotient map, preserve its index using surjectivity, and recover the
  comap's cardinality from `Subgroup.card_mul_index`.
- **Why it works:** A surjective homomorphism preserves the index of a subgroup
  under comap.  In the quotient the index is computable from the quotient and
  subgroup cardinalities, while in the ambient group the same index determines
  the cardinality of the lifted subgroup.

## math-formalization/conjugate-a-container-to-a-prescribed-sylow-subgroup

- **Trigger:** A finite group contains some subgroup of a useful cardinality,
  but the argument needs such a subgroup containing one fixed Sylow subgroup
  so that normality inside the container yields a normalizer bound.
- **Move:** Choose a Sylow subgroup inside the available container, map it into
  the ambient group, use conjugacy of ambient Sylow subgroups to send it to the
  prescribed subgroup, and conjugate the whole container by the same element.
  Prove normality of the prescribed subgroup in the conjugated container and
  convert it with `Subgroup.normal_subgroupOf_iff_le_normalizer` into containment
  in its ambient normalizer.
- **Why it works:** Conjugation preserves subgroup cardinality and inclusion.
  Sylow conjugacy aligns the smaller subgroup without losing the larger
  container, and normality in the container is exactly the API condition that
  places the container below the ambient normalizer.

## math-formalization/restrict-a-coset-core-size-by-permutation-range

- **Trigger:** A subgroup has small finite index and its normal core must be
  shown to have one of only a few possible cardinalities.
- **Move:** Realize the core as the kernel of the coset permutation action.
  Combine divisibility of the kernel cardinality into the subgroup cardinality,
  `Subgroup.card_mul_index`, and divisibility of the image cardinality into the
  factorial-sized permutation group; finish the finite arithmetic separately.
- **Why it works:** `Subgroup.index_ker` identifies kernel index with image
  cardinality, and Lagrange bounds that image by the full permutation group.
  The resulting simultaneous divisibility conditions are often much sharper
  than kernel containment alone.

## math-formalization/replace-native-decide-with-explicit-prime-valuation-reduction

- **Trigger:** A closed natural-number factorization or prime-valuation fact is
  discharged by `native_decide`, but the theorem must satisfy an axiom policy
  that rejects the native evaluation axiom.
- **Move:** Rewrite `Nat.factorization` using its prime specialization, expose
  one factor of the prime, simplify `padicValNat` of the product, and prove the
  remaining cofactor is not divisible by the prime with `norm_num`.
- **Why it works:** The factorization API reduces the closed computation to
  ordinary certified equalities and divisibility propositions, so the proof
  kernel checks the same numerical fact without trusting native evaluation.
