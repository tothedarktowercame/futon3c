# F224/F225 recurrence: receipt IDs routed as Agency jobs

The queue observed on 2026-09-11 has three awaiting-decision promotion parks:
F223/m93J01 (scribe-reduce), F224/m93J02 (guide-intervention-2-review), and
F225/m93J03 (guide-intervention-1-review). F226/m93J06 is active. These are
actual parks, not merely stale display entries.

## Findings

Both new parks exhausted one corrective review. F224 has one cannot-judge
candidate; F225 has two. Both checkpoints contain terminal collection evidence,
and their findings preserve the reviewers' reasons, so those parts of 17720cc6
are operating. The reviewer used Agency port 7070, but received HTTP 404 for
an input Student receipt ID treated as a job ID.

The controller path establishes the mismatch:

- live_learning_phases.clj constructs Guide :input-attempt-id from the Student
  phase's :receipt/id. It is not an Agency job identity.
- promotion_candidate_store/controller-source-attempts mixes that receipt with
  job identities in one untyped vector, :source-attempts.
- My 4ca4eb8a review-read-instruction generated an invoke/jobs URL for every
  element of that mixed vector. That assumption was incorrect and was deployed
  with the later repair. The controller also did not pass the Guide's own
  validated ticket job into Guide candidate materialization; the receipt
  provider has the ticket available but passes only request/report.

The earlier producer ambiguity was real, and my URL-generation repair made the
wrong interpretation explicit rather than correcting it. The audit/tests did
not exercise this actual Guide producer-to-reviewer path. Thus the claim that
routing was repaired across roles was too broad.

## Executed checks

probes.json records four read-only Agency GETs. Both input receipt IDs return
404. Both actual Guide ticket IDs return 200 with matching returned identities:
F224's Guide has 12 retained events; F225's has 48. Both job records have cancelled
terminal state; this does not erase their events, nor establish that the claimed
mathematical experiments occurred. No candidate claim was independently reviewed
in this investigation, and neither park was cleared.

The failed receipt IDs are ebf2d3c20f127a1012e102e0001c44232837a7a332f47d49ca2a3b56a338db75
(F224) and a13c68effa57febbc7b6b5ce9cf4df9be9454a4c4c3bccaa696056cba036fb81
(F225). Exact actual Guide job IDs and observation timestamps are in probes.json.
Both Guide runs occurred after the 15:18 deployment.

## Required correction, not implemented in this investigation

Carry typed provenance from the producer: an Agency job reference, a phase
receipt reference, and an unresolved historical reference must remain distinct.
Only an explicitly job-typed reference receives an Agency job URL; do not infer
type from a hash's shape. Bind the Guide's own job from its validated ticket and
retain the input Student receipt separately. Resolve receipts through their
controller-owned receipt-to-job relation when that is what review requires.
Preserve existing candidate/receipt identities; do not rewrite old evidence as
if it had always had corrected provenance.

Before another live deployment, replay the complete producer/materializer/
review-packet path for Solver, Student/Scribe and Guide deposits, including
these two concrete failure cases. Merely testing URL formatting did not cover
that path. This packet makes no source, runtime, queue or receipt mutation.
