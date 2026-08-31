# Codex Scribe pattern library — f60

This pattern was added because the reviewed mathematics-memory search for the
corresponding finite-cardinality obstacle returned no coherent parent.

## math-formalization/count-finite-structured-objects-by-explicit-coordinate-equivalence

- **Trigger:** A finite structured type is defined by a decidable predicate on
  an ambient finite function type, while the natural homomorphism-and-kernel
  counting route needs a lifting or kernel theorem that the library lacks.
- **Move:** Replace the structured type by the subtype cut out by its defining
  predicate, split each ambient coordinate with an explicit equivalence, prove
  that the predicate depends only on one coordinate, and assemble an
  equivalence from the subtype to a product of a smaller structured subtype and
  the free coordinates.
- **Why it works:** An explicit product equivalence proves the constant-fiber
  decomposition directly.  Cardinality then follows from `Fintype.card_congr`
  and `Fintype.card_prod`, without constructing a surjective homomorphism or
  evaluating a large kernel.
