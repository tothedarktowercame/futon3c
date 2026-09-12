# Codex Scribe pattern library — f219

These patterns cover transporting Hilbert-space closure-of-range results back
to a functional-valued operator and turning a positive lower norm bound into a
continuous linear equivalence with a prescribed closed target submodule.

## math-formalization/transport-annihilator-density-through-riesz-and-adjoint

- **Trigger:** A continuous bilinear pairing is represented as a map into a
  continuous dual, while the available closure theorem is stated for the
  Hilbert adjoint and orthogonal complements.
- **Move:** Compose the dual-valued map with the inverse Riesz equivalence to
  obtain a Hilbert-space-valued continuous linear map.  Translate membership
  in the custom annihilator into membership in the orthogonal complement of
  the adjoint kernel by expanding both definitions and using the Riesz
  evaluation identity.  Apply `ContinuousLinearMap.orthogonal_ker` (and
  adjoint involutivity when necessary), then carry closure membership forward
  through the continuous Riesz map and identify its image of the represented
  range extensionally.
- **Why it works:** Riesz representation converts evaluation by a functional
  into an inner product, so the transpose kernel becomes the Hilbert adjoint
  kernel.  The orthogonal-kernel theorem gives exactly the required closure,
  and continuity transports that closure back to the dual-valued range.

## math-formalization/package-a-bounded-below-map-as-an-equivalence-to-a-closed-target

- **Trigger:** A continuous linear map has a positive lower norm estimate and
  a separately proved equality between the closure of its range and a target
  closed submodule; the goal asks for a `ContinuousLinearEquiv` onto that
  target.
- **Move:** Derive injectivity from the lower bound applied to a difference.
  Convert the estimate with
  `antilipschitzWith_iff_exists_mul_le_norm`, then use
  `isClosed_range_iff_antilipschitz_of_injective` to make the range closed.
  Replace the closure in the density equality by the range, upgrade the set
  equality to a submodule equality with `SetLike.ext'`, and compose
  `ContinuousLinearMap.equivRange` with `ContinuousLinearEquiv.ofEq`.
- **Why it works:** A positive lower bound simultaneously rules out a kernel
  and controls inverse continuity.  Closedness turns density into surjectivity,
  while the range equivalence and `ofEq` handle the subtype bookkeeping
  without constructing an inverse map by hand.
