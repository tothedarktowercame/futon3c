# Pattern library additions — zai-scribe, scribe-reduce on a97J07 (f42 frame)

Created because no existing math library pattern fits the mined rules.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids
below are pattern ids for attachment.

## math-formalization/probe-mul-monotonicity-nonneg-side
Trigger: multiplying two order bounds with the generic product monotonicity
lemma and the elaborator complains about the nonneg side goals. Move: the
generic `mul_le_mul` of this Mathlib revision demands nonnegativity of the
LEFT factors (the ones multiplied by, not the other multiplicands), which is
the reverse of what most proofs assume. Probe `#check` on the lemma before
the first application and order the two bounds so the nonneg obligations
discharge by transitivity with the bound itself; reach for `mul_comm` only
once, after the shape is confirmed.

## math-formalization/read-full-compile-output-before-axiom-audit
Trigger: the axiom report names `sorryAx` for a proof that greps clean of
any textual `sorry`. Move: elaboration errors (unknown identifiers from a
too-narrow `open` line, e.g. missing `Topology` / `Filter` for `Tendsto`,
`𝓝`) elaborate into placeholder sorries inside `have`s, so the axiom
report is a downstream symptom. Read the full compile output first, extend
the `open` line to the namespaces the proof actually uses, and only then
audit axioms.

## math-formalization/probe-constant-namespace-qualification
Trigger: a comment or hint names a Mathlib constant with a namespace prefix
and `#check` on that full name fails as an unknown identifier. Move: the
constant may be declared at the file's root namespace outside the named
section (e.g. a Complex-flavoured lemma living at the root). One `#check`
round trip on the unqualified root name settles it; do not rewrite the
proof around a lemma that exists under a different qualification.
