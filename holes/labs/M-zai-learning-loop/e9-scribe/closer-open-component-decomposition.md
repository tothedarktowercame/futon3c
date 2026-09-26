# Draft: construct open interval components through rational witnesses

- Requested memory level: `lemma-location`
- Status: promoted addendum commissioned in the closer pass
- Confidence: single observed problem (`n=1`)
- Problem: `a96J04`
- Git commit: `33575db8ea2271641c75b1eda91c78ab72606150`
- Closer job: `invoke-1785770371655-945-fa70bee1`
- Decisive construction time: approximately 15 minutes

## Proposed memory

When a proof on `(0,1)` needs the decomposition of an open subset of `ℝ`
but no packaged Mathlib theorem is available, construct only the component
facts the downstream finite argument needs:

1. take the connected components of the open subset (restricted to `(0,1)`);
2. assign each nonempty open component a rational witness, making the family
   countable by injection into `ℚ`;
3. identify each component with the interval `(sInf C, sSup C)` using
   connectedness and openness;
4. pass finite families of these pairwise-disjoint intervals to the finite
   absolute-continuity hypothesis, then take the countable supremum/limit.

For a96J04 this local construction supplied the missing bridge and closed
`ac_monotone_maps_null_to_null` without changing its statement. It is the
constructive addendum to the earlier gap record: the decomposition was routed
by connected components and rational witnesses, rather than found as one
packaged theorem.

## Evidence and scope

The construction is present in apm-lean commit
`33575db8ea2271641c75b1eda91c78ab72606150`; the closer job is
`invoke-1785770371655-945-fa70bee1`. The main theorem compiles with zero
`sorry`, and its axiom report is `[propext, Classical.choice, Quot.sound]`.

## Honesty boundary

This is one completed construction (`n=1`), not evidence that it is the best
general API or that it recurs across problems. A directly packaged Mathlib
lemma was still absent from the APIs and source locations checked in this
revision; that is a revision-scoped search observation, not a timeless
nonexistence claim.
