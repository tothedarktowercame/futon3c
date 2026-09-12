# Codex Scribe pattern library — f87

This pattern was added because the reviewed mathematics-memory search returned
nearby splitting-field and Galois-correspondence patterns, but neither covered
inheritance of abelian Galois structure by an embedded simple subextension.

## math-formalization/inherit-abelian-galois-through-an-embedding

- **Trigger:** A simple finite extension is embedded in a splitting field whose
  automorphism group is commutative, and the goal needs the simple extension
  itself to be Galois or to have as many automorphisms as its degree.
- **Move:** Prove the generator's minimal polynomial splits in the ambient
  splitting field, use `IntermediateField.nonempty_algHom_adjoin_of_splits` to
  construct an algebra homomorphism from the simple adjoin, install the
  ambient `IsAbelianGalois` instance, and descend it with
  `IsAbelianGalois.of_algHom`.
- **Why it works:** An embedding into an abelian Galois extension places all
  conjugates of the simple generator in a normal abelian environment, and the
  library packages the resulting normality, separability, and commutativity as
  an `IsAbelianGalois` instance on the source. Then
  `IsGalois.card_aut_eq_finrank` converts that structure directly into the
  automorphism-cardinality equality.
