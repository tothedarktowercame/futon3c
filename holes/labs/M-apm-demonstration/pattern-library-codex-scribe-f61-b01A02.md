# Codex Scribe pattern library — f61

This pattern was added because the reviewed mathematics-memory search for the
corresponding finite-group obstacle returned no coherent parent.

## math-formalization/trivialize-conjugation-by-coprime-centralizer-index

- **Trigger:** A finite group has a normal cyclic subgroup, and the remaining
  obstacle is to show that the quotient action on that subgroup is trivial.
- **Move:** Use the conjugation homomorphism into the subgroup's automorphism
  group.  Bound the centralizer index both by the ambient group order and by
  the automorphism-group order; if those bounds are coprime, the index is one,
  so the normal subgroup lies in the center.  Then combine cyclicity of the
  central quotient with the available structural criterion for the ambient
  group.
- **Why it works:** The kernel of conjugation is the centralizer.  Injectivity
  of the induced quotient map makes its index divide the automorphism-group
  cardinality, while Lagrange makes the same index divide the group order.
  Coprimality forces the action image to be trivial without constructing or
  classifying semidirect products.
