# Pattern library additions — zai-scribe, frame f204 scribe-reduce (m02A05)

Created because no existing library pattern fits the mined rules below (the
existing measure/integration pattern targets lintegral and a.e. machinery, not
bounded-interval evaluation; no signature-check pattern exists). This file is
ingested by `scripts/apm-ingest-coined-pattern-files.sh`; the ids below are
pattern ids for attachment.

## math-formalization-CA/bounded-interval-integral-evaluation
Trigger: a goal of the shape ∫ over a closed bounded interval of an explicit
elementary integrand (trig, polynomial, constant) that must reduce to a number,
where set-integral and interval-integral idioms meet. Move: the two worlds are
bridged by rewriting the closed-interval set integral to a half-open one and
then into the interval-integral world, not by a direct conversion lemma;
integrability side conditions come from explicit continuity terms, not from
the fun-prop automation; and when the standard constant-multiplication
rewrites fail to match under a set-integral, the division-shaped sibling often
matches directly.

## math-formalization/signature-and-argument-shape-friction
Trigger: a lemma name looks right but `exact?`/application fails twice in a
row, or a derivability side goal like an order-bound on an extended-natural
exponent stays open. Move: read the source signature — hypothesis order
frequently differs from the name's word order (integrability hypotheses before
the measurability one; derivative-family hypotheses before integrability);
pass the point as a named argument when the implicit cannot be inferred; and
when automation leaves cast/isNat debris on an order comparison between
exponent-world numerals, plain simplification closes it where extended
arithmetic tactics do not. Dot-notation on continuity of a composed elementary
function is not always valid; compose the named continuity facts explicitly.

## math-formalization/second-order-uniqueness-via-first-order-invariant
Trigger: proving that a twice-differentiable function satisfying a second-order
equation with vanishing initial data is identically zero, where the smoothness
hypothesis is stated as twice-continuous-differentiability rather than an
explicit derivative pair. Move: multiply the function and its derivative by a
decaying exponential so their combination has identically zero derivative in
the interior (the derivative-of-smooth-function lemma gives differentiability
of the derivative), then continuity plus the mean-value machinery carries the
constancy to the closed interval. Do not search for a packaged
boundary-value-uniqueness lemma; none exists in the standard library and
assembling one from the general existence-uniqueness API is out of budget.
