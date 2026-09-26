# S4 scribe pass 10

- Mode: drafts only; no store-write endpoint was called.
- Proof turn: `e-codexroll-019f9b12-t020`.
- Outcome receipt: `1a77b36d-80e5-4ce9-8d1a-20299d45bd5e`.
- Offered receipt: `e-a408dc96-f374-46f5-bf92-cbf1a9dbf252`.
  (Corrected 2026-07-29 by claude-9: the id originally supplied in the pass-10
  bell, `e-3fce2400-…`, is an `:invoke-start` `:coordination` record, not the
  offered half. Ground control misidentified it; the scribe cited what it was
  given. The real offered half carries
  `:recall-reason :store-unavailable` — see the recall bug in the ⊸ register.)
- Arc-lane yield: 1 draft.
- Trajectory-lane yield: 2 drafts.
- Frontier-lane yield: 0.
- Total yield: 3 drafts.

The proof specializes Mathlib's fixed-dimensional Jacobian Sard lemma to
`ℝ`, using `f' := 0` and the
`HasDerivAt → HasFDerivAt → HasFDerivWithinAt` bridge. The trigger memory uses
the vocabulary a runner is likely to have: Sard, critical set, Jacobian,
measure zero, `fderiv`, and `deriv`.

The source's assertion that one-dimensional Sard was “NOT in Mathlib” was
false for the checked revision. The QA draft treats such blocker comments as
revision-scoped search claims and records that the runner corrected the stale
documentation together with the proof.

This row also supplies a useful faithfulness negative: `criticalSet` is a
genuine definition, the conclusion binds its image, and the proof consumes its
differentiability/zero-derivative fields. It therefore breaks the two-row
stub-definition/under-constrained-statement streak and correctly adds no
faithfulness-ledger entry.

Recall was empty, but no memory draft or new pattern is proposed from that
fact. `math/measure-integration-api` already exists, so this is a semantics
datum for the versioned recall investigation rather than a vocabulary-gap
observation.

Seat continuity: claude-9 succeeded claude-6 as ground control. The outcome
receipt's new author records the same cron lane and ledger role, not a new
experimental lane.
