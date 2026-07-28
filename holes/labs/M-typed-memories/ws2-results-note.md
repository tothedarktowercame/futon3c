# WS2 spectral and connectivity reading — 2026-07-28

## Spectral sweep

The cyclic-Jacobi implementation passed the preregistered P3 check:
`[0, 1, 3]` to numerical tolerance, and every reported solve converged below
the `1e-10` off-diagonal-norm threshold.

The v0 Phase-4 graph has 13 nodes, five source control edges expanded into ten
memory→pattern→mission hops, and four components of sizes 4, 3, 3, and 3.
Spectrally, its four zero modes state the food problem directly: no choice of
conductance can carry activation between those components.

The v1 rules graph is connected for uniform, prescribe-heavy, and uses-heavy
weights. Their seed-component λ₂ values are approximately 0.185, 0.0580, and
0.0578. Turning the problem hub off produces five components
`[11, 5, 3, 3, 1]`; the seed component has λ₂≈0.154. The spectra therefore
separate coefficient choices, but connectivity support still dominates what
the operator can express.

The frozen v1 trajectory classes were joined without reinterpretation.
Uniform alone includes the dissipated run; every other run is structured or
oscillating.

The preregistered time-to-uniform check did **not** pass. At heat ε=0.3 the
times were `[8, 6, 9, 21]` steps in grid order, while seed-component λ₂ values
were `[0.185, 0.0580, 0.0578, 0.154]`; Spearman ρ is `0.0`, not negative.
Component size, the participation-based target, and discrete-step stability
confound the proposed one-number prediction. The failed check is retained as
evidence, not repaired post hoc.

## Live connectivity meter

The bounded read returned 62 `memory/assert` rows without a reported error or
limit hit. The exported memory/pattern graph contains 83 nodes and 51 pattern
attachment edges; 45 are current and reviewed.

The largest current-reviewed component has six nodes, five edges, one edge
type, and λ₂≈1.0. It misses the preregistered ≥10-node and ≥2-edge-type
conditions, so the first verdict is **`:component-limited`**. The high λ₂
only says each small star mixes internally; it does not overcome isolation
between pattern stars.
