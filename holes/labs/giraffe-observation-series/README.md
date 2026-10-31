# Giraffe observation-learning series

Working ledger for the branch that Fable will review after the series.

## Step 0 — interpret current structure

- `54b043392` derives cluster composition, prerequisite roots, dependent
  frontiers, and ticket linkage without inventing cluster names.
- `780f95e72` turns a difference between clocked work and discussed work into
  an unresolved `relationship-review`; it neither asserts an edge nor
  changes the clock.
- Historical verifier repair was independently reviewed and merged as
  `130b272db`. Both the legacy pinned run and current clustered run verify.

## Step 1 — persist the observation

- A settled operator turn now writes one deterministic Evidence Landscape
  proposal per explicit M-/E-/T- reference that differs from its clock target.
- The proposal carries the operator evidence ID, exact quote, clocked work,
  discussed work, and turn-record ID.
- It is emitted only after the turn settles, alongside the existing settled
  Xiang packet. Missing clock or operator evidence fails closed.
- Alias resolution is deliberately not part of this slice. An alias such as
  “Giraffe” needs a separate resolver that reports its evidence basis.

### Demo made possible

Given clock `E-prove-memory-useful` and operator text explicitly naming
`M-giraffe`, Evidence Landscape receives a proposed, unresolved relationship
review linked to the operator turn. A subsequent step can record how that
tension was resolved.

## Next: Step 2

Define resolution acts that cite a relationship-review event:
`formalized-edge`, `reclocked`, `ticket-created`, `completion-revised`,
`incidental`, or `unresolved`. Do not infer a resolution merely because the
next clock differs.
