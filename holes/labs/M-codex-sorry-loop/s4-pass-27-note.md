# S4 scribe pass 27

- Mode: one proving run; drafts only; no store-write endpoint was called.
- Row: a00J05, solved axiom-clean.
- Turn-round: `e-codexroll-019fa2c1-t039`.
- Receipt: `c18f4fdb-edc2-47e8-86cb-4a76c9242ed1`.
- Commit: `2f24f492ae53cadf0345f07fbfcd0b54da6dfdf6`.
- Sorry count: 2 → 0.
- Solve-lane yield: 2 drafts.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 0.
- Total yield: 2 drafts.

Both evidence ids resolved, and the turn-round contains the cited
a00J05/Cauchy-transform/Liouville/cocompact content. The proof was read from the
exact commit.

The first solve draft puts the negative result in its hook and makes the
substitution explicit: `tendsto_integral_filter_of_dominated_convergence`
requires a countably generated source filter, and current Mathlib cannot
synthesize `(cocompact ℂ).IsCountablyGenerated`. Compact support supplies a
stronger route for this target: an eventual uniform `C/‖z‖` bound on the whole
integral error, followed by `squeeze_zero_norm'`.

The second solve draft records the entire-function contradiction separately:
eventual agreement outside a compact set transfers the nonzero limit of
`z*g(z)`; division by `z` makes `g → 0`; Liouville forces `g = 0`; uniqueness
of limits contradicts the positive-measure asymptotic.

## Missing-instance classification

The finding belongs in the solve lane in this pass, not in a standalone
frontier record.

It is a real missing Mathlib instance and a useful precondition audit:
filter-indexed dominated convergence cannot be instantiated on
`cocompact ℂ` through the named theorem. But the mathematical target is not
blocked, and the uniform compact-support estimate is both axiom-clean and more
direct. A separate frontier memory would duplicate the same trigger while
suggesting unresolved demand that this row does not have. The solve memory
retains the exact missing instance, its scope, and the verified replacement.

This is deliberately not generalized to all cocompact integral arguments. The
replacement depends on compact support and explicit uniform decay. A future
row without those properties could justify promoting the missing instance as
an independent frontier.

## Existing relations and subject handles

The prior promoted memories were fetched:

- `e-codexpilot-differentiate-a-cauchy-transform-locally-under-the-integral`
  covers the local differentiation stage and is attached to
  `math/measure-integration-api`.
- `e-codexpilot-extend-a-reciprocal-across-finite-punctures-then-apply-Liouville`
  supplies a distinct cocompact-Liouville architecture and is attached to
  `math/entire-and-singularity-api`.

Reused in this pass:

- `M-codex-sorry-loop`
- `a00J05`
- `math/measure-integration-api`
- `math/entire-and-singularity-api`

Minted: none.

Both pattern handles were confirmed in the read-only live graph export before
drafting. The subjects are also named throughout the bodies and hooks because
the text-search sidecar does not index `:subjects`.

## Recall exclusion

No recall or terrain memory was drafted. The packet records that the persisted
offered result has `:recall-reason :timeout`; therefore zero surfaced memories
do not measure coverage. The outcome receipt also contains an explicitly
unconfirmed ladder hypothesis. Neither is promoted here into a mathematical or
architectural diagnosis.

Both hooks state actionable conditions rather than restating their memory
names, and both drafts contain nonempty `:how-to-apply` sequences.
