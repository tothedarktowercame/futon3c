# Codex Scribe pattern library — f189

The reviewed mathematics-memory search found patterns about small permutation
representations and conjugation actions, but no pattern for turning an
even-valued action on an abstract finite type into a surjection to the standard
alternating group on `Fin n`.

## math-formalization/upgrade-even-finite-actions-to-standard-alternating-quotients

- **Trigger:** A finite group action gives a homomorphism into permutations of
  an abstract finite type; every action permutation is even, and a theorem
  requires a surjective map to `alternatingGroup (Fin n)` with explicit kernel.
- **Move:** Use `MonoidHom.codRestrict` to restrict the action homomorphism to
  `alternatingGroup` on the original type.  Preserve the kernel by extensionality,
  compute the restricted range cardinality from `ker.card_mul_index`, and turn
  equality with the target cardinality into `range = top` using
  `Subgroup.eq_top_of_card_eq`; then obtain surjectivity through
  `MonoidHom.range_eq_top`.  Finally choose `Fintype.equivOfCardEq` to `Fin n`
  and compose with `Equiv.altCongrHom.toMonoidHom`; injectivity of this
  equivalence-induced map preserves the kernel.
- **Why it works:** Codomain restriction changes only the codomain subtype and
  therefore does not change which source elements map to one.  In a finite
  target, a subgroup of full cardinality is the whole group.  Alternating
  groups are invariant under equivalence of their underlying finite types, so
  the abstract action set can be standardized only after the group-theoretic
  kernel and range calculation is complete.
