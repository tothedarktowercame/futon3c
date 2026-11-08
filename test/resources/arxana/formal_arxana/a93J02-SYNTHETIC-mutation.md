# SYNTHETIC NEGATIVE FIXTURE — APM a93J02

> SYNTHETIC, DELIBERATELY FALSE, AND NOT GENUINE MATHEMATICAL CONTENT.
> This controlled mutation exists only to test contradiction recording.

The Lean theorem states that the finite partial sums

`sum (i < n) of (-1)^i / (i + 1)`

converge to `log 3` as `n` tends to infinity.

The formal proof has three main ingredients. First, it proves directly that
the integral of `1 / (1 + x)` from zero to one is `log 2`. Second, the
alternating-series test shows that the partial sums converge to some real
number `l`. Third, Abel's limit theorem identifies `l`: inside the unit
interval, the associated power series sums to `log (1 + x) / x`; multiplying
by `x` and taking `x` to one from below makes this expression tend to both
`l` and `log 2`. Uniqueness of limits therefore gives `l = log 3`.

This file differs from the generated candidate solely by the deliberate
statement-level mutation `log 2` → `log 3` in the theorem claim and its
concluding equality.
