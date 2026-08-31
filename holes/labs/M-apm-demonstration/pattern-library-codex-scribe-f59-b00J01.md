# Codex Scribe pattern library — f59

These patterns were added because the reviewed mathematics-memory search for
the corresponding Galois-theory obstacles returned no coherent parent.

## math-formalization/transport-splitting-field-structure-through-uniqueness

- **Trigger:** A theorem is stated for an arbitrary splitting field, while the
  computable structure or automorphism group is known only for one convenient
  concrete or canonical splitting field of the same polynomial.
- **Move:** Use `Polynomial.IsSplittingField.algEquiv` to identify the two
  splitting fields, then transport automorphisms by `AlgEquiv.autCongr` and
  compose with the concrete group equivalence.
- **Why it works:** Splitting-field uniqueness supplies the algebra equivalence
  missing from the statement, and conjugation by that equivalence preserves
  the multiplicative automorphism-group structure.

## math-formalization/real-subalgebra-generated-by-a-nonreal-element

- **Trigger:** To prove that a real subalgebra of `ℂ` is all of `ℂ`, the
  available generator is a complex root whose imaginary part is nonzero.
- **Move:** Subtract the embedded real part and multiply by the inverse of the
  embedded imaginary part to put `Complex.I` in the subalgebra; then express an
  arbitrary complex number as its real part plus its imaginary part times `I`.
- **Why it works:** A real subalgebra already contains every embedded real, and
  closure under subtraction and multiplication converts any nonreal member
  into `I`, which generates `ℂ` over `ℝ`.

## math-formalization/compute-intermediate-fields-via-finite-group-subgroups

- **Trigger:** An abstract finite Galois extension has an explicitly identified
  small automorphism group, and the goal asks for all intermediate fields,
  their degrees, or their Galois status.
- **Move:** Classify subgroups of the concrete finite group, pull them back with
  `MulEquiv.comapSubgroup`, take `IntermediateField.fixedField`, and use the
  fixed-field/fixing-subgroup identities. Compute degrees from subgroup cards
  and the tower formula; decide Galois status from subgroup normality.
- **Why it works:** The Galois correspondence reverses inclusion and turns a
  finite subgroup enumeration into an exhaustive field enumeration, while
  index/cardinality and normality encode exactly the requested invariants.
