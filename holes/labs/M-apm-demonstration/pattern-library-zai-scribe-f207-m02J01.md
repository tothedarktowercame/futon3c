# Zai Scribe pattern library — f207 (m02J01 student arc)

Added because the reviewed mathematics-memory search found no coherent parent
for the discovery that staged fixed-support machinery on the test-function
space serves *continuity proofs about operators*, not the mere construction of
test-function elements.

## math-formalization/staging-serves-continuity-not-membership

- **Trigger:** You must produce an element of a bundled smooth compactly
  supported function space, and the API offers a staged route through
  fixed-support subspaces plus inclusion constructors, which a residual note
  or memory says to use.
- **Move:** Read the structure definition first. When the structure is a plain
  record of a function plus smoothness and support conditions, and the support
  condition is against the whole space, the anonymous constructor closes each
  field directly; the staged subspace route is only needed when you must prove
  *continuity of a map* into the space. A residual comment claiming the staged
  route is required for bundling may be stale — verify against the structure
  before scheduling hours for staging.
- **Why it works:** The fixed-support stages exist to give a normed-space
  structure on which continuity estimates run. Membership in the full space
  never mentions those stages, so constructing an element needs only the
  record fields.
