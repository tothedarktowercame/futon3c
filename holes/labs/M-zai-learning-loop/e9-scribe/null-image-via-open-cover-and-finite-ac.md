# Draft: null image via open cover and finite absolute continuity

- Requested memory level: `strategy`
- Status: draft; awaiting operator review
- Confidence: single observed problem (`n=1`); architecture incomplete
- Problem: `a96J04`
- Git commit: `1d014930e85e1b9a8a21dfc197cad7876baf468a`
- Evidence IDs: `e-d7b604b2-24e4-4703-ad17-1bd756df25fa`,
  `e-956a1b1b-0bd6-4366-9d3d-f7ddcff55ca4`,
  `e-47ffb25c-9cc6-4df5-848b-62a464bb1c61`,
  `e-afcdc7ee-2507-473b-95ac-8e74c2a7de67`

## Proposed memory

For a monotone absolutely continuous `f : [0,1] → ℝ`, organize the Lusin
N-property proof as four explicit interfaces:

1. Given `ε > 0`, obtain the absolute-continuity threshold `δ > 0` for finite
   pairwise-disjoint intervals.
2. Cover the null set `E` by an open set of measure below `δ`, then express the
   relevant open part as countably many disjoint intervals `(a k, b k)`.
3. Use monotonicity to place each interval image inside
   `[f (a k), f (b k)]`.
4. Apply the finite AC hypothesis to every finite partial family. Bound the
   countable sum of image lengths by the common finite-partial-sum bound and
   conclude `volume (f '' E) ≤ ε`; arbitrariness of `ε` gives zero.

Keep the interval-decomposition bridge separate from the endpoint-image and
finite-sum leaves. That makes the dependency frontier visible and lets the
monotonicity helpers compile independently.

## Evidence comparison

The runner explores outer-measure, pushforward-measure, and derivative routes,
then commits the cover → monotone image → finite partial sums architecture.
The proof outline and final summary agree on that architecture, while
`Main.lean` proves only the monotone image leaves and places the one `sorry` at
the remaining assembly.

## Failure boundary

This is a proof plan, not a completed theorem. The session did not formalize
the countable disjoint interval decomposition or the countable-limit passage,
and did not investigate a countable-rational-basis route. Promotion must
preserve that incomplete status rather than reporting the architecture as
validated end to end.
