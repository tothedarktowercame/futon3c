# Pattern library additions — zai-scribe, frame f201 scribe-reduce (m02A02)

Created because no existing library pattern fits the mined rule below. This
file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
the id below is a pattern id for attachment.

## math-formalization/integer-discreteness-before-cast
Trigger: a real-valued inequality goal over cast integer atoms (squares or
sums of `(↑a : ℝ)^2` with `a : ℤ`) that `nlinarith`/`positivity` cannot close
even though each square is manifestly nonnegative — e.g. needing `(1:ℝ) ≤
↑a^2 + ↑b^2` from `a ≠ 0 ∨ b ≠ 0`. Move: nlinarith only manipulates the
polynomial atoms it sees; it cannot manufacture a discreteness (integrality)
lower bound from bare nonnegativity, and `sq_nonneg`-supplied facts often do
not match the cast atoms syntactically. Supply the integer-level fact first
(rcases on the integer cases, reduce `a ≠ 0` to `1 ≤ a.abs`-style integrality
or case-split on sign) and only then cast into ℝ, rather than feeding
positivity facts to the real arithmetic.
