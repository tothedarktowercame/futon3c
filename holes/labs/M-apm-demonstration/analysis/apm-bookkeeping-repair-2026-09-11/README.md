# APM bookkeeping audit and shared repair, 2026-09-11

Joe requested a forward repair of recurring bookkeeping failures, rather than
another exception for F223. This packet examines the producer and consumer
paths for role service addresses, promotion evidence collection, promotion
recovery, and queue decision status. It does not claim a complete audit of
all APM mathematics, runtime performance, or historical transient errors.

## Retained-state census

`bb scripts/apm-audit-bookkeeping.clj data/apm-campaigns/jit-all-open-v3 200 223`
produced retained-census.json: 24 frames, 221 retained phase checkpoints and
eight queue parks. Each input is hashed. Files were read individually; this
is not an atomic snapshot or a count of every transient error.

| Frame | Retained queue classification |
| --- | --- |
| F200 | solver-session-mismatch |
| F202 | solver-remediation-required |
| F206 | live-job-terminal-repair-exhausted |
| F208 | solver-strategy-checkpoint-required |
| F209 | live-job-activation-failed |
| F218 | campaign-stepper-execution-failed |
| F220 | solver-human-intervention-frame-park |
| F223 | promotion-apparatus-repair-exhausted |

These are retained classifications, not fresh causal diagnoses of all eight.
F223's independently inspected commands and port probes are in the preceding
f223-promotion-routing-2026-09-11 packet. The queue marks seven decisions
recorded; this does NOT establish successful recovery or completed learning.
`reconcile-park-decisions` explicitly records decisions without executing them.

## Findings and implemented changes

1. **Addresses were not carried consistently into role commands.** The shared
   submission/search helpers defaulted to 7070 even when their dispatcher used
   another configured Agency address; reviewers composed trace URLs themselves.
   Proof, learning and promotion dispatch now pass their actual Agency base
   into the rendered prompt. Submission, search and the checked trace reader
   receive it explicitly. Shell quoting protects the supplied address. The
   trace reader verifies returned identity and distinguishes HTML HTTP errors.
   Candidate EvidenceEntry URLs retain their separate store authority.

2. **Promotion expected collection evidence it never constructed.**
   `successor-observation` consumes `:terminal-collection`; `agency-stage`
   previously returned only a report. F223's held review has no collection.
   Promotion now creates the existing typed-role collection record from the
   observed terminal and persisted submission, checks both job identities,
   and durably saves the collection before validating or publishing reviews.
   A failed collection write stops downstream publication. Its collection is
   therefore retained in subsequent failure checkpoints.

3. **Exhaustion had no explicit operator recovery operation.**
   `prepare-review-recovery` / `authorize-review-recovery!` now validate a local
   operator decision against the exact held-state digest, predecessor job,
   source/loaded-runtime identity and collection authority. This is a trusted
   operator port, not a role-submitted field or public authenticated HTTP API.
   A persisted `:review-successor-pending` state contains the complete original
   checkpoint before dispatch. It fixes the successor identity and ordinal.
   Failed final writes leave this pending state replayable with the same job
   identity through Agency's existing idempotent announce/activate contract.
   Original review, collection and retry count survive; another failed review
   does not regain the exhausted automatic budget. A consumed recovery ID
   cannot authorize a further attempt. The operator must serialize read/write
   with the frame driver; this function does not itself acquire that lock.
   The pending replay uses the prior frozen reviewer request.

4. **The park finding hid the reviewer's explanation.** Complete-disposition
   failures now retain verdict/reason/residual beside the existing finding and
   memory ID. The failing gate is unchanged. This improves future park evidence;
   it does not rewrite F223's historical finding or claim a Voxterm UI change.

## Operational scope and remaining limits

F223 is deliberately preserved, not marked recovered. Its old missing collection
must be obtained by a fresh, identified observation of the retained review and
submission before the recovery operation can accept it. Historical records are
not silently backfilled. Queue decision reconciliation still does not schedule
parked frames. Operator recovery must be run under the frame's serialized owner;
a queue-wide scheduler for retired frames is not introduced in this packet.

Existing automatic corrective-review branches remain distinct from the new
operator operation. Their older archive-before-dispatch flow merits the same
transactional treatment; this packet's replay tests establish the new operator
path, not a universal claim about all old APM retry mechanisms. No mathematical
review gate, retry counter, old receipt, registry or queue entry is relaxed.

## Validation

Relevant namespaces are run separately: promotion-recovery, live-promotion,
typed-role-submission, promotion-pipeline, live-proof-phases and
live-learning-phases. Regression coverage includes configured address propagation
into the exact archived activation packet, missing/stale collection, wrong job
identity, failed authorization and collection writes, failed final successor
write with stable replay identity, runtime drift, tampered pending state, and
reuse of a consumed recovery ID. See validation.txt for final counts and gates.
Deployment is separately receipted; source commit alone is not evidence of load.
