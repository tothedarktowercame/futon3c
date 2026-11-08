# APM a93J02 — generated informalisation

> Model-generated candidate explanation. This is not authored prose and is
> not accepted mathematical authority.

The Lean theorem states that the finite partial sums

`sum (i < n) of (-1)^i / (i + 1)`

converge to `log 2` as `n` tends to infinity.

The formal proof has three main ingredients. First, it proves directly that
the integral of `1 / (1 + x)` from zero to one is `log 2`. Second, the
alternating-series test shows that the partial sums converge to some real
number `l`. Third, Abel's limit theorem identifies `l`: inside the unit
interval, the associated power series sums to `log (1 + x) / x`; multiplying
by `x` and taking `x` to one from below makes this expression tend to both
`l` and `log 2`. Uniqueness of limits therefore gives `l = log 2`.

This proof uses Abel's limit theorem to cross the boundary of the power
series' convergence disk. It does not formalise the authored informal
argument's dominated-convergence route.
