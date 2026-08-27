# APM authority seam repair

Date: 2026-08-27

Four live failures exposed values that were independently reconstructed on
opposite sides of a boundary: a JIT frame was checked against a countdown
manifest, a qualification report attested an older generated contract, the
Lean emitter and Clojure allowed-key set diverged, and a theorem name was
lowercased instead of read from the source declaration.

The Lean model now represents these as one `LaunchAuthorityBundle`. Validity
requires exact campaign and frame binding for the manifest, exact registered
and observed qualification digests for the generated contract, and equality
between the source-declared and audited theorem names. The emitter exposes
those requirements; the Clojure generated-contract validator requires the
same values. Existing runtime gates provide the corresponding observations,
while `scripts/regenerate-apm-contract.sh` remains the atomic emitter plus
qualification operation.

## f46 promotion incident

The f46 Promotion Proctor returned three approvals. Every review had non-empty
pattern IDs, reasons, residuals, complete candidate accounting, and the correct
reviewer identity. It correctly omitted `:review-evidence-id` and
`:attachment-status`: those are controller authority created by
`promotion-review-store/persist!`.

`promotion_pipeline/validate-review*` nevertheless demanded those fields
before calling the store, so the valid role output could never cross the
persistence boundary. The repair separates two predicates:

- `validate-returned-review*` checks the reviewer-authored judgement and does
  not require controller evidence;
- `validate-review*` checks the persisted projection and does require evidence
  identity plus reviewed attachment status for publishing verdicts.

Lean models the same distinction with `PromotionReviewStage` and
`PromotionReviewEvidence.ValidAt`. It proves both that a publishing returned
review can be valid without controller fields and that the same unpersisted
value cannot serve as a completed disposition.

Changing the generated contract digest is intentional. A held promotion state
re-enters its last valid independent-review state after a contract change,
reuses the completed reviewer job, persists the controller evidence, validates
the persisted form, and then continues to publication. No reviewer redispatch
or campaign-state rewrite is required.
