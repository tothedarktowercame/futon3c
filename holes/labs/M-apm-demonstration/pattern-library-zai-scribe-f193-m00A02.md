# Pattern library additions — zai-scribe, scribe-reduce on m00A02 (f193)

Created because no existing library pattern fits the mined rule below.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids
below are pattern ids for attachment. Other mined rules from this frame
fit existing patterns (math-formalization/filter-tendsto-const-mul-api-shape,
math-formalization/probe-division-lemma-variant-names,
math-formalization/vanishing-derivative-limit-via-fixed-interval-slopes,
math-formalization/asymptotic-coefficients-via-filter-lhopital-remainder-comparison)
and are attached there, not re-coined.

## math-formalization/zpow-nat-pow-to-zpow-bridge
Trigger: a term-derivative goal stated over a natural-exponent encoding
(`c / y ^ i`, `a n / x ^ n`) must be fed to Mathlib's zpow-based derivative
API (`hasDerivAt_zpow`), and the obvious rewrites die. Concretely: (a)
`hasDerivAt_zpow` takes `(m : ℤ) (x) (h : x ≠ 0 ∨ 0 ≤ m)` positionally —
passing the nonneg disjunct first makes Lean try to synthesize the ℤ from
the disproof tactic and fails with 'unsolved goals ℤ'; (b) its derivative
value is spelled `((m : R) * x ^ (m - 1))` with an `Int.cast`, so a
hand-written value like `-(i : R) * …` or `(-i : ℤ) * …` fails unification —
restate the value with the cast visible and normalize with `Int.cast_neg`/
`mul_assoc`; (c) `← zpow_natCast` does NOT rewrite a nat-exponent `HPow`
occurrence in the goal into zpow form — the pow-with-nat and zpow-with-Int
instances do not match the rewrite pattern, so converting between the
natural encoding and the zpow shape needs an explicit congr/convert chain.
Budget for that normalization as its own subtask; it is plumbing, not the
mathematics.
