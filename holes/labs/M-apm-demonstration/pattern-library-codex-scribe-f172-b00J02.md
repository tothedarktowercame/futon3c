# Codex Scribe pattern library — f172

These patterns were added because the reviewed mathematics-memory search for
the corresponding finite-algebra obstacles returned no coherent parent.

## math-formalization/count-homomorphism-kernels-by-coordinate-fibers

- **Trigger:** The kernel of a homomorphism on a finite matrix-like structure
  is too large for kernel reduction by `decide`, but membership in the kernel
  becomes an independent finite constraint on every coordinate after forgetting
  the structure.
- **Move:** Build an equivalence from the kernel to the subtype of underlying
  matrices satisfying the mapped identity equation, rewrite that subtype as an
  iterated dependent function of scalar fibers with `Equiv.subtypePiEquivPi`,
  and count each scalar fiber separately.
- **Why it works:** The equivalence exposes a constant finite fiber at each
  coordinate, so `Fintype.card_pi` replaces evaluation of one large structured
  kernel by small kernel-checked decisions and a product calculation.

## math-formalization/lift-invertible-matrices-through-residue-map-by-determinant

- **Trigger:** A surjective ring homomorphism does not come with a library
  theorem asserting that its induced map on general linear groups is
  surjective, while scalar representatives in the source can be chosen
  explicitly.
- **Move:** Lift the entries of the target invertible matrix, prove the lifted
  determinant is a unit by mapping the determinant and using the target's
  nonzero determinant, then package the matrix with
  `Matrix.GeneralLinearGroup.mk''`.
- **Why it works:** `RingHom.map_det` transfers the determinant calculation
  across the residue map.  A source-specific criterion turning nonzero residue
  into `IsUnit` supplies exactly the hypothesis needed by `mk''`, avoiding any
  missing general lifting theorem for `GL`.
