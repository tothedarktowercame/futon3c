# Draft: monotone image interval containment

- Requested memory level: `tactic`
- Status: draft; awaiting operator review
- Confidence: single observed problem (`n=1`)
- Problem: `a96J04`
- Git commit: `1d014930e85e1b9a8a21dfc197cad7876baf468a`
- Evidence IDs: `e-0fafe91b-b9ab-4ddf-abb4-876ba9447d1e`,
  `e-f6f409e6-b6b8-47f3-bfec-3f6e63e79b23`,
  `e-b7709d4e-6537-415f-ab8a-e06912cf313b`,
  `e-e55d539e-492e-4ba5-9298-e9b8147edac6`

## Proposed memory

To prove image containment for a function monotone only on an ambient
interval, unpack the image witness and manufacture ambient membership before
applying `MonotoneOn`:

```lean
rintro y ⟨x, hx, rfl⟩
have hx01 : x ∈ Icc 0 1 :=
  ⟨ha.1.trans hx.1, hx.2.trans hb.2⟩
exact ⟨hf ha hx01 hx.1, hf hx01 hb hx.2⟩
```

Here `ha : a ∈ Icc 0 1`, `hb : b ∈ Icc 0 1`, and
`hx : x ∈ Icc a b`. The reusable result is
`f '' Icc a b ⊆ Icc (f a) (f b)`.

## Why this pattern matters

`MonotoneOn f (Icc 0 1)` requires both comparison points to inhabit the
ambient set. Membership of `x` in the smaller interval `Icc a b` is not
definitionally the required ambient membership. Endpoint transitivity closes
that type-level gap, after which the lower and upper endpoint inequalities are
direct applications of monotonicity.

## Evidence comparison

Rounds 24–25 check the actual `MonotoneOn` argument order. Round 28 identifies
and fixes the smaller-versus-ambient interval mismatch. Round 29 reports exit
0, and the exact helper survives in committed `Main.lean`; this draft is based
on the compiled artifact, not merely the runner's proposed code.

## Boundary

One compiling instance supports the tactic. It has not yet been observed
across multiple problems, so it carries no recurrence claim.
