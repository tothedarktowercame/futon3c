# S4 scribe pass 28

- Mode: one proving run; drafts only; no store-write endpoint was called.
- Row: a01A04, solved axiom-clean.
- Turn-round: `e-codexroll-019fa2c1-t040`.
- Receipt: `639e3a13-dc0f-4673-86af-bbf1dbbfc7f7`.
- Commit: `80af310905a3a3255df7f96e31d9a162da8c0ded`.
- Sorry count: 1 → 0.
- Solve-lane yield: 1 draft.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 0.
- Total yield: 1 draft.

Both evidence ids resolved. The turn-round contains the cited
a01A04/`ball_volume_recursion`/Wallis material, and the proof was read from the
exact commit.

The solve draft records the complete mathematical architecture: restrict the
whole-line slice integral to `[-1,1]`, substitute `t = -cos x`, rewrite the
resulting sine-power integral through Wallis double-factorial formulas, split
on dimension parity, and align the two branches with Mathlib's exact even/odd
Euclidean ball-volume declarations.

## Verdict on the route improvement

This is a confirming instance of the promoted memory
`e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`,
not a new trajectory memory.

The inherited plan was productive through the slice and Wallis stages. The
runner changed only the reconciliation stage: installed component evidence
favoured `InnerProductSpace.volume_ball_of_dim_even` and
`InnerProductSpace.volume_ball_of_dim_odd` over a manual Gamma recurrence.
That is the existing rule's trigger and decision shape. Drafting it again would
duplicate a promoted rule. The confirmation is retained inside the solve
draft's `:route-improvement` field and is explicitly bounded to this Mathlib
revision.

The promoted route-override memory was fetched successfully before this
classification.

## Verdict on the Zulip payoff

The Zulip result materially paid: the “Roadmap to high school geometry” thread
pointed to PR #19949 and the exact parity-specific Euclidean ball-volume API
used in the proof. It was an API-location and formalization-precedent payoff,
not merely background.

No separate search-policy memory was drafted. The promoted memory
`e-codexpilot-order-proof-search-by-known-route-components-before-literature`
already assigns Zulip this role, and pass 21 already supplied one strong
positive instance. This row is a further confirming instance. The new fact is
preserved as the solve draft's literature anchor; a parallel trajectory memory
would restate the existing policy.

The promoted search-order memory was fetched successfully before this
classification.

## Subject handles

Reused:

- `M-codex-sorry-loop`
- `a01A04`
- `math/measure-integration-api`

Minted: none.

`math/measure-integration-api` already exists in the read-only live graph and
fits the slice integral, interval substitution, and volume-measure content.
No narrower Wallis or ball-volume handle was minted from a single row. Search
terms including Wallis, sine powers, double factorials, Euclidean ball volume,
and the exact Mathlib declarations appear in the hook and body because subjects
are not indexed by the text sidecar.

## Recall exclusion

No recall, terrain, or coverage inference was drafted. Dispatch-time recall
failed with `dispatch-recall-outcome=store-unavailable`, so Metric 3 is
not measurable rather than zero.

The hook gives the conversion and parity tactic visible at recall time rather
than restating the memory name, and `:how-to-apply` is nonempty.
