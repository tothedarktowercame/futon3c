# Codex Scribe pattern library — f185

These patterns cover positive proof moves in the certified solver head that
were not represented by a coherently fitting reviewed pattern.

## math-formalization/use-the-generalized-eigenspace-action-api-before-restricting

- **Trigger:** A commuting decomposition `T = S + N`, with `S` semisimple and
  `N` nilpotent, must show that vectors in a generalized eigenspace of `T` are
  eigenvectors of `S`.
- **Move:** Express membership using `Module.End.mem_genEigenspace`, establish
  that `T` commutes with `S`, and apply the packaged theorem
  `Module.End.apply_eq_of_mem_of_comm_of_isFinitelySemisimple_of_isNil`.
- **Why it works:** The theorem already encapsulates the restricted
  Jordan--Chevalley uniqueness argument and returns the scalar-action equality
  directly, avoiding explicit restriction maps and subtype bookkeeping.

## math-formalization/recover-a-split-monic-polynomial-from-root-multiplicities

- **Trigger:** Two characteristic polynomials over an algebraically closed
  field should be equal, and generalized-eigenspace dimensions give equality
  of every root multiplicity.
- **Move:** Convert pointwise root-multiplicity equality to equality of the
  root multisets with `Multiset.ext` and `Polynomial.count_roots`, then express
  each monic split polynomial as the product of its linear root factors using
  `eq_prod_roots_of_monic`.
- **Why it works:** Over an algebraically closed field both characteristic
  polynomials split and are monic, so their multisets of roots with
  multiplicity determine them completely.
