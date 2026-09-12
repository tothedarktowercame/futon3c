# Codex Scribe pattern library — f178

This pattern was added because the reviewed mathematics-memory search found
the adapted-complements classification pattern, but no parent for the final
change-of-scalars step from conjugacy of a generator action to linear
equivalence over the generated algebra.

## math-formalization/upgrade-linearity-by-commuting-with-algebra-generators

- **Trigger:** A map between modules over a finitely generated algebra is
  linear over the base ring and intertwines the action of each algebra
  generator, but the goal requires a linear map or equivalence over the whole
  algebra.
- **Move:** Express every algebra scalar as a base-scalar polynomial in the
  generators.  Prove scalar compatibility by additivity, base linearity, and
  the generator-commutation hypotheses; for an equivalence, package the
  forward map and derive the same properties for its inverse by injectivity.
- **Why it works:** The algebra action is determined by the base-ring action
  and the actions of its generators, so intertwining those operations forces
  compatibility with every scalar without constructing an algebra-linear map
  from coordinates afresh.
