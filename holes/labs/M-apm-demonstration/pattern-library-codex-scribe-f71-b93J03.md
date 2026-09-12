# Codex Scribe pattern library — f71

These patterns were added because the reviewed mathematics-memory search for
the corresponding finite-dimensional linear-algebra obstacles returned no
coherent parent.

## math-formalization/reduce-infinite-operator-family-through-finite-span

- **Trigger:** A property must hold simultaneously for an arbitrarily indexed
  family of endomorphisms, but the available construction handles only finite
  families and the ambient endomorphism space is finite-dimensional.
- **Move:** Choose finitely many actual members of the family spanning its
  range, prove the property for that finite subfamily, and express the desired
  condition as membership in a linear subspace so it extends across the span.
- **Why it works:** Finite dimensionality makes the range span finitely
  generated.  Conditions such as annihilating a fixed vector are linear in the
  operator, so verification on spanning generators implies verification for
  every member of the original family.

## math-formalization/assemble-triangular-basis-from-invariant-subspace-and-quotient

- **Trigger:** An endomorphism preserves a subspace and is triangular on both
  that subspace and the induced quotient, but the goal asks for a triangular
  matrix on the whole space with a linearly ordered `Fin` index.
- **Move:** Assemble the two bases with `Module.Basis.sumQuot`, prove the four
  block cases directly, reindex the sum basis by `finSumFinEquiv`, and transport
  once more along the finrank equality if the target `Fin` size is stated in a
  different arithmetic form.
- **Why it works:** Invariance kills the lower-left cross block, the order on a
  sum index makes the opposite cross-block triangular condition impossible,
  and the two diagonal blocks are exactly the restricted and quotient maps.
  Reindexing changes coordinates without changing the vanishing condition.
