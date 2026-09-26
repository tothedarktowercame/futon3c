# Codex Scribe pattern library — f63

These patterns were added because the reviewed mathematics-memory search for
the corresponding commutative-algebra obstacles returned no coherent parent.

## math-formalization/maximal-ideal-classification-by-residue-polynomial-ring

- **Trigger:** A maximal ideal of a polynomial ring over a non-field base must
  be classified, while the useful irreducibility and principal-ideal APIs live
  only after passing to a residue field.
- **Move:** Contract the maximal ideal to the coefficient ring, identify that
  contraction with the kernel of a quotient map to a field, map the original
  ideal along the induced surjective polynomial homomorphism, classify its
  image as a principal maximal ideal, and recover the original ideal by
  comapping.
- **Why it works:** Surjectivity gives the map/comap identity up to the kernel;
  once the kernel is known to lie in the original ideal, the correction term
  disappears.  The residue polynomial ring is a PID/UFD where maximal
  principal ideals correspond to irreducible generators.

## math-formalization/nonmembership-in-generated-ideal-by-quotient-degree

- **Trigger:** A polynomial is visibly nilpotent modulo a finitely generated
  ideal, but proving that the polynomial itself is not in the ideal is awkward
  over the original coefficient ring.
- **Move:** Map coefficients to a residue field so that the constant generator
  vanishes, rewrite membership in the image ideal as divisibility by the
  remaining polynomial, and contradict divisibility using `natDegree`.
- **Why it works:** `Ideal.mem_map_of_mem` transports any hypothetical source
  membership.  In a polynomial ring over a field, membership in a singleton
  span is divisibility, and a nonzero divisor cannot have larger natural degree
  than its dividend.
