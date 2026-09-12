# Codex Scribe pattern library — f226

This pattern was added because the reviewed mathematics-memory search found no
coherent parent for propagating a locally persistent property across a compact
real interval by a uniform finite mesh.

## math-formalization/propagate-local-persistence-across-a-compact-interval-by-uniform-mesh

- **Trigger:** A property is known at one point, a single positive radius works
  uniformly along the compact interval, and the local theorem propagates the
  property from an anchor to every point strictly within that radius.
- **Move:** For a target displacement `t`, use `exists_nat_gt (|t| / δ)` to
  choose a positive natural `n`, set `d = t / n`, prove `|d| < δ`, and induct
  the property over the arithmetic grid `k * d` for `k ≤ n`.  Prove grid
  membership in `[min 0 t, max 0 t]` by splitting on the sign of `d`, so the
  same induction handles both orientations.
- **Why it works:** Uniformity prevents shrinking-step or Zeno behavior;
  Archimedean subdivision makes every successor step lie in the open local
  propagation interval, while the sign split keeps all anchors inside the
  compact region on which the uniform radius is valid.
