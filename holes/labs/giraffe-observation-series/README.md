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

## Step 2 — record the resolution

The turn action `POST /api/alpha/xiang/turns/:id/resolution` records resolution
acts that cite a relationship-review event:
`formalized-edge`, `reclocked`, `ticket-created`, `completion-revised`,
`incidental`, or `unresolved`.

- Every act requires `resolution-evidence-id` and a type-specific witness.
  For example, `reclocked` requires the new target and clock-decision ID;
  `formalized-edge` requires its relation and edge evidence ID.
- The cited review must be derivable from the authoritative turn record.
- The file record is updated before the Evidence Landscape append and remains
  the source of truth if that append needs retrying.
- A byte-identical retry is idempotent. A different resolution for the same
  review is refused; history cannot be silently rewritten.

### Demo made possible

The Step 1 mismatch can now be closed as `incidental`, citing a later operator
evidence row and rationale. Evidence Landscape receives a resolution whose
`in-reply-to` is the proposed review; the turn file carries the same resolution.

## Step 3 — ticket provenance and current relevance

The combined Giraffe pass now projects ticket provenance separately from
planning dependencies and publishes it inside the already sealed
`work-clusters.json` projection.

- `part-of`, `motivated-by`, and `relates-to` links remain lineage/context;
  they do not affect readiness or cluster membership.
- Source-pinned `Parent:` declarations are extracted alongside checked-reading
  links. Matching statements merge as two attestations; a source-only parent
  remains visible instead of disappearing.
- Source SHA drift fails the projection rather than being interpreted as no
  parent.
- Every ticket receives a current-relevance state. A typed witness with an
  Evidence Landscape ID can establish `current`, `obsolete`, or `superseded`.
  Without one, the ticket is `review-required`; a closed/cancelled origin gives
  the more specific reason `closed-origin-open-ticket`.

### Demo made possible

On the sealed 188-ticket population, 51 tickets have lineage/context, 45 have
an explicit origin, and all 188 require current-relevance review because no
typed relevance witnesses were supplied. Four open repair tickets have a
completed/cancelled origin and are specifically classified
`closed-origin-open-ticket`.

## Step 4 — record retrieved-pattern outcomes

The turn action `POST /api/alpha/xiang/turns/:id/pattern-outcome` records what
happened to a pattern retrieved for that specific turn: `applied`, `adapted`,
`rejected`, or `obvious`.

- The pattern ID must occur in the turn's candidate sidecar. A caller cannot
  manufacture a positive or negative observation for an unretrieved pattern.
- Outcomes are accepted only after the analyzed/drafted turn has a happened
  summary, so retrieval is compared with an observed result.
- Every outcome cites an assessment evidence ID. Application also cites use
  evidence and its effect; adaptation and rejection cite the unresolved text
  and its evidence; `obvious` records why the retrieval added no information.
- The file record is updated before Evidence Landscape. An identical retry is
  idempotent, while a conflicting label is refused.

### Demo made possible

For a settled turn whose sidecar retrieved `social/x`, the service can record
that it was applied and cite the concrete effect. An invented pattern ID is
refused, and an adaptation without an evidenced residual is refused. This
creates attested use observations without treating retrieval itself as use.

`T-evidence-pinned-to-mutable-prose-26082026` carries two matching attestations
for `part-of E-R8-red-ring-fill` (checked reading plus pinned source). The
population currently calls that excursion `unstarted`, not closed, so this
projection reports `no-current-relevance-witness`; it exposes rather than
silently repairs the lifecycle disagreement.

## Next: Step 4

Record retrieved-pattern outcomes (`applied`, `adapted`, `rejected`, `obvious`)
and the exact residual that justified adaptation or rejection.
