# WS3 Ψ-v2 dark replay — 2026-07-28

The bounded export contains 16 ground-control `pattern-outcome` receipt
halves. Joining by job id produced two complete offered/outcome rows.
Twelve offered halves have no outcome half; no outcome half lacks an offer.
Those losses remain explicit in the join audit.

Only one joined row has a non-empty `used-ids` set, so the metric sample is
`n=1`. No-Ψ, deployed-S6 scalar, and Ψ-v2 each score MRR `1.0` and hit@1
`1.0`. These equal numbers say only that all arms retained the already-correct
top item in one row; they do not compare policy quality.

The other joined row is classified `:surfaced-not-usable` and is excluded
from MRR as preregistered. Attribution reconstructed one `:matched` row and
one `:fallback-all` row. No scored row was self-fitted: both memory and
pattern coefficients were recomputed leave-one-out.

The graph census contains 51 `:pattern-attachment` edges and no second edge
type, so relation-level θ_r is correctly marked `:inactive-degenerate`.
Twenty current reviewed pattern coefficients are reported. None reaches the
dark activation minimum of 5.0 fractional offered credits; the largest
observed counts are 2.0 offered/2.0 used. The explicit ε=0.01 simplex floor
therefore leaves the present replay neutral.

The three-row synthetic fixture supplies six offered and six used credits to
the planted good-route coefficient, yielding θ=1.5. Ψ-v2 deterministically
changes the hand-computable ranking from `[b1, g1]` to `[g1, b1]`; the script
asserts that result.

Promotion is **`:below-calibration-minimum`**. The harness contract works,
but one metric-bearing live row and under-five coefficient counts do not
earn a live policy claim.
