# S4 scribe pass 14

- Mode: per-run cadence, drafts only; no store-write endpoint was called.
- Row: a01A05, durée run 5.
- Corrected turn-round: `e-codexroll-019fa2c1-t010`.
- Outcome receipt: `5a7a6fec-1783-4df9-9505-817eb5398920`.
- Solve-lane yield: 2 drafts.
- Trajectory-lane yield: 1 draft.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Total yield: 3 drafts.

The first solve draft records the axiom-clean weak-convergence proof: extend
pairing convergence from generators to their algebraic span, then to its
closure by an `ε/2` Cauchy–Schwarz estimate, and finally project an arbitrary
vector onto the closed span.

The second solve draft treats the false approximation statement as a reusable
counterexample construction. A triangular family spreads coefficients of size
`2^(-(m+1))` across `m` earlier coordinates. Each pairwise correlation meets
the required scale, but the aggregate error acquires an unbounded `sqrt(m)`
factor. The draft explicitly does not assert that a particular repaired upper
bound is provable.

The trajectory draft records the measurement discipline: a finite numerical
spot-check can refute a universal claim, but cannot identify its asymptotic
failure mode. The generalized workflow rule is marked as inference.

The target `gramSchmidt_approximation` remains an honest `sorry` and is retired
as wontfix under Joe's direction. No recall or search-infrastructure memory was
drafted.
