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

## Cumulative memory provenance and campaign lineage

A cumulative snapshot previously restamped every carried memory with the frame
and problem publishing that snapshot. That made the carrier appear to be the
depositor. The same-problem holdout consequently selected memories using a
field whose meaning changed at every publication.

The Lean model now distinguishes a memory's depositor origin from a snapshot
publication. A valid publication requires every origin to be complete and
requires republication of a memory ID to preserve that origin. The model also
states the precondition under which same-problem holdout is depositor-truthful.
The generated memory policy exposes these requirements to Clojure.

The runtime now enforces the corresponding rules:

- a provenance frame must equal the frame prefix of `:depositor`;
- complete provenance is preserved exactly;
- legacy provenance is reconstructed from the depositor frame and the durable
  frame-to-problem mapping, and is marked `:provenance/repaired? true`;
- duplicate memory IDs preserve the earliest carrier's entry;
- predecessor campaigns come only from an ordered `:campaign/priors` launch
  value or `lineage.edn`, never directory discovery;
- the ordered lineage is included in the memory snapshot and in the minted
  frame's conditions.

Ambiguous frame IDs across a declared lineage fail closed because a depositor
prefix could not identify one authoritative origin. The repair does not alter
campaign or coordinator state. To accumulate the intended shelf on the next
`jit-all-open-v2` frame, its launch authority must declare
`{:campaign/priors ["jit-all-open-nontopology-v1"]}`; that declaration is then
pinned into the frame manifest and snapshot.

## Transport failure disposition

Promotion projection previously sent a substrate timeout through the same
apparatus-repair path as invalid evidence. A single timed-out hyperedge write
therefore stopped the regulator even though no evidence judgement had been
made.

The Lean model now separates transport failures from evidence failures. Only
the transport class admits delayed retry, and a retry at its configured bound
is invalid. The emitted contract fixes the runtime policy at three attempts,
ten minutes between attempts, with durable history required. It also states
that evidence failures are not retryable.

Promotion state records the failed transport observation, attempt ordinal and
absolute retry deadline before returning control. Regulator ticks before the
deadline perform no substrate or reviewer I/O. A successful retry preserves
the history in the certified state; exhaustion enters the existing apparatus
repair path. Pull-mode parking uses the same absolute deadline instead of its
ordinary short continuation timer. Evidence failures continue directly to the
existing repair/refusal behavior.
