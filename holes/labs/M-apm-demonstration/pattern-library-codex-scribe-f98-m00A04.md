# Codex Scribe pattern library — f98

This pattern was added because the reviewed mathematics-memory search found no
coherent parent for the distinction between a function's raw support and its
topological support in formal encodings of compactly supported test functions.

## math-formalization/audit-raw-support-versus-topological-support

- **Trigger:** A formal definition of a compactly supported continuous or
  smooth function requires `IsCompact (Function.support f)`, or a supposedly
  rich test-function space unexpectedly collapses.
- **Move:** Expand `Function.support f` as the nonzero locus. Continuity makes
  that locus open, while the compactness hypothesis makes it closed. On a
  connected noncompact ambient space, a nonempty clopen raw support would be
  the whole space and hence cannot be compact. Conclude that only the zero
  function satisfies the definition, and audit every closure or variational
  construction built from those tests.
- **Why it works:** In Mathlib, `Function.support f` is the raw set where
  `f x != 0`; `tsupport f` is its closure. Ordinary compact support is expressed
  using compactness of `tsupport`, not compactness of the raw support. Confusing
  the two silently strengthens the predicate enough to trivialize continuous
  test functions on connected noncompact spaces.
