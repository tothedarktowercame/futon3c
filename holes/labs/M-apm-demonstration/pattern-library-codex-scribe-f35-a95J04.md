# Pattern library additions — codex-scribe, frame f35 promote-solver (a95J04)

Created because no existing math library pattern fits the mined rules.
The watcher ingests this file; ids below are pattern ids for attachment.

## math-formalization/pointwise-hassum-to-taylor-coefficients-via-fps-uniqueness
Trigger: a coefficient sequence is pinned to a function only by a pointwise
family `HasSum (fun n => a n * z ^ n) (f z)` on a disk, and you need the
coefficients identified with `iteratedDeriv n f 0 / n !` to export them to
power-series machinery. Move: package the coefficients as
`FormalMultilinearSeries.ofScalars`, upgrade the pointwise family to
`HasFPowerSeriesAt` through `hasFPowerSeriesAt_iff` (it is exactly the
filter-upwards restatement), then use uniqueness of the power series at a
point (`HasFPowerSeriesAt.eq_formalMultilinearSeries` between the packaged
series and the one from `AnalyticAt.hasFPowerSeriesAt`) and compare
coefficients. Reason it works: uniqueness of FPS expansions collapses any
pointwise sum hypothesis into coefficient equalities without building a
`HasFPowerSeriesOnBall` first.

## math-formalization/compact-thickening-upgrades-pointwise-analyticity
Trigger: you have `AnalyticAt` at every point of a compact set (e.g. the
closed unit disk) and need holomorphy on one uniformly larger region.
Move: the set of analyticity points of any function is open
(`isOpen_analyticAt`), so it is an open neighbourhood of the compact set;
`IsCompact.exists_thickening_subset_open` yields a thickening of the
compact inside it, and the thickening contains a strictly larger ball
whenever the compact is a closed ball. Reason it works: this converts a
pointwise condition on a compact into a single uniform domain — the
standard compactness step behind "removable obstructions on the boundary
give continuation past it".

## math-formalization/normalize-meromorphic-point-values-before-continuation
Trigger: a function is `MeromorphicOn` on a domain but its raw point
values at removable points are arbitrary, so `AnalyticAt` fails at those
points and continuation arguments on the raw function are unsound. Move:
pass to the normal form (`toMeromorphicNFOn`); its meromorphic order
agrees with the original's (`meromorphicOrderAt_toMeromorphicNFOn`),
analyticity at a point is exactly nonnegative order
(`MeromorphicNFAt.meromorphicOrderAt_nonneg_iff_analyticAt`), and the
normal form equals the original wherever the original was analytic
(`toMeromorphicNFAt_eq_self`). Reason it works: the normal form repairs
exactly the point-value defect and nothing else, so order-based reasoning
transfers verbatim.

## math-formalization/additive-principal-parts-from-order-germs
Trigger: you must subtract finitely many principal parts from a
meromorphic function (e.g. to bound Taylor coefficients of the remainder),
and a search for a ready-made additive decomposition API fails because the
divisor machinery only factorizes. Move: work locally —
`meromorphicOrderAt_eq_int_iff` gives a power-bounded germ for any integer
order, a Taylor expansion of that germ
(`AnalyticAt.exists_eq_sum_add_pow_mul`, truncated at the pole order)
yields an analytic part plus an explicit one-term principal part, and
`meromorphicOrderAt_add` then shows subtracting all the local principal
parts makes every order nonnegative; finish with the thickening upgrade.
Reason it works: the local germ carries exactly the data of its Laurent
tail, so the classical partial-fractions construction is a one-line
consequence of order normalization plus germ Taylor expansion; no global
decomposition theorem is needed.
