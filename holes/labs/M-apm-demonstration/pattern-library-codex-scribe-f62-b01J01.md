# Codex Scribe pattern library — f62

These patterns were added because the reviewed mathematics-memory search for
the corresponding finite-group obstacles returned no coherent parent.

## math-formalization/equicardinal-fibers-from-equivariant-transitive-action

- **Trigger:** A finite map is equivariant for a group action and the action on
  the target is transitive, but the desired divisibility or product formula
  requires every fiber to have the same cardinality.
- **Move:** For two target points, choose a group element carrying one to the
  other and restrict its action on the source to an equivalence between the two
  fibers.  Then identify the source with the sigma type of its fibers and use
  `Nat.card_sigma` or the corresponding finite sum formula.
- **Why it works:** Equivariance sends a point over one target to a point over
  its translate, and the inverse group element supplies the inverse map.
  Transitivity therefore makes all fibers equivalent, while the sigma
  decomposition counts the source without needing a separately packaged
  constant-fiber theorem.

## math-formalization/prime-index-kernel-from-finite-abelian-decomposition

- **Trigger:** A prime divides the cardinality of a finite commutative group,
  and a subgroup of that prime index is needed but no direct existence theorem
  is available.
- **Move:** Decompose the group as a finite product of multiplicative `ZMod`
  factors, use prime divisibility of the product to select a factor whose
  modulus is divisible by the prime, project to that factor, reduce it with
  `ZMod.castHom`, and take the kernel of the resulting surjective homomorphism.
- **Why it works:** The selected coordinate maps surjectively onto `ZMod p`.
  `Subgroup.index_ker` converts surjectivity and the target cardinality into
  index `p`, avoiding classification-specific maximal-subgroup arguments.
