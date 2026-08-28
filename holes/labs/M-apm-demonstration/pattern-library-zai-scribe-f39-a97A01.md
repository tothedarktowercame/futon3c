# Pattern library additions — zai-scribe, frame f39 scribe-reduce (a97A01)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/probe-division-lemma-variant-names
Ordered-field division and multiplication monotonicity lemmas come in
zero-subscript and unprimed variants; the wrong variant fails as an unknown
identifier rather than a mathematical error, and the iff-forms of
multiplication monotonicity fail on instance synthesis where the one-sided
implication form applies directly. The name of a monotonicity lemma says
which side the FACTOR sits on — the reverse of which side the hypothesis
mentions — so probe the exact name and argument order before the rewrite.

## math-formalization/extract-exact-membership-inequalities
What membership in two half-open intervals gives is a pair of cross
comparisons pinning the shared parameter, not the endpoint ordering a
stronger claim would want. Stating the strengthening anyway makes the
auxiliary lemma unprovable, and the counterexample arrives from the
arithmetic automation itself — looking like a tactic limitation rather than
a false statement. Extract what the hypotheses literally give first, and
strengthen only if the extraction provably fails.
