# Pattern library additions — zai-scribe, frame f47 scribe-reduce (a97A01)

Created because no existing math library pattern fits the mined rules.
This file is ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`;
ids below are pattern ids for attachment.

## math-formalization/parity-dot-notation-falls-back-to-rcases-omega
Parity predicates on ℕ (Even, Odd) are plain existential definitions, so
dot-notation lemma names projected off them (`.not_odd`, `.not_even`,
`.even_iff_not_odd`) fail as invalid-field or unknown-identifier errors even
when the fact is true. The reliable replacement is to rcases both parities
into their witnesses (⟨a, rfl⟩, ⟨b, rfl⟩) and close the resulting linear
contradiction with omega — which also closes a False goal outright, so a
trailing simp then reports 'No goals to be solved'. Probing dot notation on
an existential predicate costs a compile round; skip straight to the rcases.

## math-formalization/condition-rewrites-need-their-negation-proof-first
A conditional rewrite lemma (if_neg, if_pos, and their Exists/forAll
cousins) takes its side condition as an explicit argument, so rewriting
first and trying to `swap` into the side goal fails with 'goal index out
of bounds'. Restructure: state the side condition as a `have ¬P` (or `P`)
up front, prove it, then rewrite with it as an argument. This turns a
failed tactic dance into one sequenced proof step.
