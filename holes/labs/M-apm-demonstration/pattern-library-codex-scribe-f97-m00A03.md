# Codex Scribe pattern library — f97

This pattern was added because the reviewed mathematics-memory search found no
coherent parent for the finite-span/Riesz-separation proof of spectral
finiteness for compact operators.

## math-formalization/contradict-compactness-with-an-invariant-finite-span-flag

- **Trigger:** An infinite family of distinct nonzero eigenvalues is bounded
  away from zero, while compactness must be contradicted without an existing
  theorem packaging spectral finiteness.
- **Move:** Enumerate the eigenvalues, choose eigenvectors, use distinctness to
  obtain linear independence, and form the increasing finite spans. Apply
  Riesz's lemma inside each successor span to choose unit vectors separated
  from the preceding span. Invariance of the finite spans and the eigenvalue
  lower bound turn that separation into pairwise separation of their images.
- **Why it works:** Finite spans are closed and proper by linear independence;
  the eigenvector equations make them invariant and make the shifted image of
  each successor vector land in the preceding span. The resulting uniformly
  separated image sequence has no Cauchy subsequence, contradicting relative
  compactness of the image of the closed unit ball.
