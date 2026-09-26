# S4 scribe pass 19

- Mode: per-run cadence, drafts only; no store-write endpoint was called.
- Problem: a93A03.
- Run 12 turn-rounds: `e-codexroll-019fa2c1-t019`, `t020`.
- Run 13 turn-round: `e-codexroll-019fa2c1-t021`.
- Receipts: `1e916185-46ae-4ee0-ab1c-01a9759928d2` and
  `df9722e5-dc6f-435e-8ec1-8286f76505e7`.
- Solve-lane yield: 1 draft.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 1 draft.
- Total yield: 2 drafts.

The missing-hypothesis draft records the diagnostic sequence, not merely the
repair: identify Banach--Steinhaus as the proof engine, inspect its
`CompleteSpace` requirement, test the omitted assumption with the incomplete
space `c₀₀`, and retain the repair only after the completed proof demonstrates
that it is load-bearing.

The solve draft records the repaired route. Weak convergence gives pointwise
boundedness of `innerSL`; Banach--Steinhaus gives a uniform operator bound;
`innerSL_apply_norm` turns it into uniform bounds on `‖h n‖`; that bound supplies
the `IsCoboundedUnder` premise required by `le_liminf_of_le`.

Cross-lane correction: the draft proposed a `:resolves` edge to
`le-liminf-is-cobounded-under-unbounded-above-gap`, correctly typed as a
`:draft-memory`. The target exists in
`M-zai-learning-loop/s1-pilot/memory-drafts-cohort1.edn`; promoted-store fetch
and text search could not see it, and a grep scoped to the M-codex lane could
not see it either. Receipt `be23ef8b-20a0-4c21-9a1a-7f3e45a9f589` reinstates
the edge and records the scope error.

Promotion recommendation: promote that M-zai draft after changing
`:status :gap-open` to a resolved status. Preserve its negative API analysis,
replace the `:untested-suggestion` marker with the validated Banach--Steinhaus
route, and cite commit `2f8bb4b4`, receipt
`df9722e5-dc6f-435e-8ec1-8286f76505e7`, and promoted solve-memory evidence
`e-codexpilot-derive-real-liminf-lower-semicontinuity-via-Banach-Steinhaus`.
This is not redundant: the solve memory records what works, while the gap
record explains why nonnegativity and the `liminf_eq`/`le_csSup` alternatives
do not supply `IsCoboundedUnder`.

No separate junk-value draft was added. The exact `csSup`/real-`liminf`
mechanism appears in that unpromoted M-zai draft, while the general family is
represented by the promoted `math-formalization/notation-semantics-traps`
pattern. This row is a fourth confirming instance. The useful follow-up is to
add this liminf instance to the existing semantics-traps pattern description.

Process-memory proposal: record that an absence claim is only as broad as the
indexes searched. A promoted-store fetch, promoted-memory text search, and
single-lane draft grep jointly say nothing about unpromoted drafts in other
lanes. For a promoted-memory target, require a successful evidence fetch; for a
`:draft-memory` target, verify the filesystem artifact, lane, name, and status
directly before accepting or rejecting the edge. This is a structurally
reusable provenance rule, not merely a report of this review error.

Zulip independently corroborated the missing hypothesis rather than supplying
the proof route: `116395-maths/banach-steinhaus.json` and the mathlib4
weak-topology thread both formulate the development with completeness. No
recall-infrastructure material was drafted.
