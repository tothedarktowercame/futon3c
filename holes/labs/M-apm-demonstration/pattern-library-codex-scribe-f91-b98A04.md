# Codex Scribe pattern library — f91

These patterns were added because the reviewed mathematics-memory search found
only unrelated analysis patterns and no match for extracting a prescribed
simple subquotient through Jordan--Hölder or reducing a simple subquotient of a
finite power to one of a single coordinate module.

## math-formalization/make-a-cover-occur-by-extending-and-comparing-composition-series

- **Trigger:** A simple quotient gives a cover `A ⋖ B` in a finite-length
  submodule lattice, and the goal is to identify it with a factor of a chosen
  full composition series.
- **Move:** Regard the cover as a one-step `LTSeries`, extend it at both ends to
  a full `CompositionSeries`, prove that the embedded endpoints remain
  consecutive by excluding an intermediate lattice element with `CovBy`, and
  apply `CompositionSeries.jordan_holder` against the chosen full series.
- **Why it works:** Extending a one-step series preserves its two marked
  elements.  A cover admits no element strictly between them, so their indices
  in the extension differ by one; Jordan--Hölder then transports that exact
  factor to one of the chosen series' successive factors.

## math-formalization/reduce-a-simple-subquotient-of-a-finite-power-coordinatewise

- **Trigger:** A submodule of `Fin n → M` surjects onto a simple module, but a
  later argument needs a submodule of one copy of `M` with the same simple
  quotient.
- **Move:** Project to the first coordinate.  If the projection kernel maps
  nontrivially to the simple target, restrict the quotient map to the kernel;
  simplicity makes it surjective, identify that kernel with a submodule of the
  tail, and recurse.  Otherwise the quotient map kills the projection kernel,
  so descend it through `LinearMap.ker.liftQ` and
  `LinearMap.quotKerEquivRange` to a surjection from the projection range in
  `M`.
- **Why it works:** A nonzero map into a simple module is surjective.  The two
  cases therefore either lower the number of coordinates or finish with a
  quotient of a submodule of one coordinate, giving a terminating induction.
