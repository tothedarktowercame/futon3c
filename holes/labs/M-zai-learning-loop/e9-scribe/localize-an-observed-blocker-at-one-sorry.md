# Draft: localize an observed blocker at one sorry

- Requested memory level: `process`
- Status: draft; awaiting operator review
- Confidence: single observed problem (`n=1`)
- Problem: `a96J04`
- Git commit: `1d014930e85e1b9a8a21dfc197cad7876baf468a`
- Evidence IDs: `e-4b7ea8b6-0d88-49a8-bb7c-b76fcbf30bbc`,
  `e-d7b604b2-24e4-4703-ad17-1bd756df25fa`,
  `e-e55d539e-492e-4ba5-9298-e9b8147edac6`,
  `e-47ffb25c-9cc6-4df5-848b-62a464bb1c61`,
  `e-afcdc7ee-2507-473b-95ac-8e74c2a7de67`

## Proposed memory

When a session reaches an observed library or theory bridge it cannot close:

1. compile and retain every proved reusable leaf;
2. place the smallest remaining `sorry` at the exact bridge, rather than
   scattering holes through downstream corollaries;
3. write a boundary comment naming the API searched, what was actually tried,
   the checked validation result, and the exact remaining proof chain;
4. distinguish “not found in this bounded search and revision” from “cannot be
   derived,” and name material routes that were not investigated;
5. commit the compiling partial and report its precise `sorry` count.

For a96J04 this yielded two compiled monotonicity helpers, exit 0, and one
`sorry` exactly at `ac_monotone_maps_null_to_null`. The comment records the
open-cover → interval-decomposition → finite-AC plan.

## Evidence comparison

The turn stream records the outer-regularity and source searches, alternative
measure-theoretic ideas, helper corrections, and final compilation. The
committed source, status, and outline agree with the final summary on both the
one-hole frontier and the unblock plan.

## Novelty relative to current memories

This is near, but not identical, to
`e-c924ebba-3fff-4a01-b281-d3e90a0a09bd`, which requires a compiling lemma
checkpoint before cap-risk assembly. It is also near
`e-codexpilot-treat-not-in-mathlib-comments-as-revision-scoped-search-claims`,
which scopes library-absence assertions to a revision and search. The proposed
addition is their proof-boundary intersection: the exact contents and
placement of a single honest residual `sorry` after useful leaves have been
proved.

## Boundary

This is one disciplined instance, not evidence that the practice improves
completion rates. In this case the countable-rational-basis route was not
investigated, so the boundary must say “observed blocker,” never
“proven-unroutable.”
