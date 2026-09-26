# Codex Scribe pattern library — f197

These patterns cover the certified construction of a regularized integral
functional when the desired formula is linear only on admissible test
functions, and the compact-support seminorm estimate for its singular and
nonsingular pieces.

## math-formalization/define-regularized-functional-on-predicate-submodule-then-extend

- **Trigger:** A functional is specified by integrals that are known to exist
  only for functions satisfying a linearity-stable admissibility predicate,
  while the theorem asks for a linear map on the ambient function space.
- **Move:** Package the admissible functions as a `Submodule`, define the
  integral formula as a linear map on that submodule using `integral_add` and
  `Integrable.integral_smul`, then obtain an ambient linear map with
  `LinearMap.exists_extend` and recover its formula on subtype elements by
  function congruence.
- **Why it works:** The subtype carries exactly the hypotheses needed to make
  the integral algebra valid. Algebraic extension postpones arbitrary values
  outside the admissible class without requiring the integral formula itself
  to be meaningful there.

## math-formalization/bound-a-regularized-singular-integral-by-splitting-local-and-support-pieces

- **Trigger:** A singular integral is regularized by subtracting a point value
  near the singularity, and a distribution-style goal requires a uniform
  zeroth-plus-first derivative seminorm bound for every fixed compact support.
- **Move:** Rewrite the local difference quotient as a bounded sign factor
  times `dslope`; bound `dslope` by the global derivative supremum using the
  convex mean-value estimate; bound the complementary kernel by one; and
  restrict the outer integral to the declared compact support before applying
  the constant-norm set-integral estimate.
- **Why it works:** Cancellation converts the singular local term into a
  first-derivative cost on a fixed finite-measure set, while compact support
  converts the nonsingular tail into a zeroth-order cost multiplied by the
  support measure. The two estimates combine with a support-dependent constant.
