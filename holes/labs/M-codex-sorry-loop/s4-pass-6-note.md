# S4 scribe pass 6

- Mode: drafts only; no store-write endpoint was called.
- Turn reads: bounded GETs for `e-codexroll-019f9b12-t015` and `t016`.
- Receipt read: exact id `13167309-920e-44a7-b276-e5db3e3e26e9`.
- Frontier-lane yield: 1 draft.
- Solve-lane yield: 1 draft.
- Trajectory-lane yield: 1 draft.
- Arc-lane yield: 0; no local error→fix produced an axiom-clean gain.
- Total yield: 3 drafts.

The exact frontier is a Lebesgue-point plus `MemLp` bridge proving that the
Poisson-weighted convolution error tends to zero. It has
`:anchor :literature` and demand 2, gating `poissonConv_tendsto_ae` and the
campaign's sorried `poissonConv_tendsto_L2` predecessor.

The installed approximate-identity API distinguishes compactly supported
normalized bumps from the full-support Poisson kernel. The former's shrinking
support hypothesis cannot be discharged by the latter's tail decay. The
runner's proposed route is a dyadic-annulus Lebesgue-point estimate plus an
`L²` far-tail bound.

The Carleson-project Zulip thread on the Hardy–Littlewood maximal principle
anchors the frontier: substantial countable-family and weak/strong-type
maximal infrastructure lives outside core Mathlib.

The headline trajectory observation is the refusal. A new helper containing
`sorry`, or a proof through the already-sorried `L²` theorem, would only
relocate or propagate `sorryAx`. The runner therefore reverted its temporary
diagnostic print, left the scoped file unchanged, and made no commit. The exact
outcome receipt confirms unchanged HEAD, a clean tracked tree, and six sorries
before and after.

Capture limit: t015 is setup and t016 contains the complete investigation and
duplicated final report. No rationale beyond the runner's stated refusal and
the receipt-confirmed repository state was reconstructed.
