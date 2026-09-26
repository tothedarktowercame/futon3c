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

## Owner-review addendum (claude-6, 2026-07-28): why ρ was 0.0 — a stability-regime diagnosis

Post-hoc analysis of the frozen `full-spectrum` data, clearly marked as
such. The preregistered prediction assumed the continuous-time diffusion
regime, where λ₂ governs the rate. But the sweep's update is explicit
Euler, x ← x + εΔx, whose modes evolve as (1−ελ)^t; stability requires
ε < 2/λ_max. At ε = 0.3, from the frozen spectra:

| θ | λ₂ | λ_max | max\|1−ελ\| | t observed |
|---|---|---|---|---|
| uniform | 0.185 | 8.51 | **1.55 (unstable)** | 8 |
| prescribe-heavy | 0.058 | 9.15 | **1.75 (unstable)** | 6 |
| uses-heavy | 0.058 | 7.56 | **1.27 (unstable)** | 9 |
| hub-off | 0.154 | 6.51 | 0.95 (stable) | 21 |

Three of four grid points sit past the stability boundary: their
per-step normalization turns the iteration into power iteration onto the
dominant Laplacian eigenvector, so their small t measures fast
*concentration*, not diffusion. Only hub-off is in the diffusive regime,
and its t = 21 is the slow diffusion λ₂ ≈ 0.154 predicts at ε = 0.1-like
effective rates. The λ₂ prediction was tested outside its regime of
validity; ρ = 0.0 is the arithmetic consequence, not evidence against
the spectral picture. Upstream note: the v1 "sustained-structured"
classes at ε = 0.3 should be read with this in mind — some structure is
the explicit scheme concentrating on the top eigenvector, which is a
choice of dynamics, not a property of the graph alone.

**Preregistered follow-up (written before running):** at ε = 0.1 all
four grid points are stable (max|1−ελ| = 0.982, 0.994, 0.994, 0.985 —
in each case attained at λ₂, so λ₂ is the slowest mode and governs).
Prediction: time-to-uniform at ε = 0.1, same definition, same 60-step
trace, ranks inversely with seed-component λ₂; Spearman ρ negative,
expected ≤ −0.8. Test: `ws2-owner-stability-check.bb`.

**Result (run after preregistration): CONFIRMED at the bar.** Times
`[46, 49, 52, 38]` in grid order; ρ = −0.8 exactly (n = 4, Σd² = 18;
the float prints −0.7999…98 — comparison done at 1e-9 tolerance so the
exact-equality case is not lost to rounding; equality was inside the
preregistered "≤"). One adjacent-rank inversion remains (hub-off
converged faster than uniform despite slightly lower λ₂ — its seed
component is half the size, which shifts the participation target); the
other three ranks are in perfect inverse order. Together with the ε=0.3
table: λ₂ predicts diffusion time *in the stable regime*, and the ρ=0.0
of the original check was a regime error, not a spectral failure.
Frozen result: `ws2-owner-stability-check-results.edn`. Consequence for
future sweeps and for any Rung 4-style iterated dynamics: **step size is
part of the operator** — ε must be reported against 2/λ_max, and
sweeps should either stay inside the stability region or declare that
they are studying the concentration (power-iteration) regime
deliberately.

## Meter-coverage check (claude-6, 2026-07-28, prompted by claude-4)

claude-4 flagged that a supersession chain (the liminf memory,
three generations) might be a second edge type the export missed.
Checked directly against the store (bounded queries): **no** `:memory/
retract` hyperedges exist and no hyperedges touch the chain by endpoint
— supersession currently lives *inside evidence bodies* (`:review/*`
fields; 3 such body-level references in the first 100 memory entries),
not as edges. So the one-edge-type conclusion **stands for the
hyperedge graph that pattern-mediated recall actually traverses**,
which is the operative graph for the dynamics question. The flag
converts into a food observation: supersession/resolves/distills
relations exist narratively (body-level) but not structurally (edges) —
claude-4's scribe instruction (cross-memory references become typed
edges from the next pass) is exactly what lifts them into the graph the
meter measures. Expect the meter to move when those land.
