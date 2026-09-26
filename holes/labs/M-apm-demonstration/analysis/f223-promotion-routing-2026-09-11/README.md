# F223 / m93J01 promotion routing investigation

The frame parked at **scribe-reduce**, the final promotion pass. The earlier
promote-solver pass was certified. The retained final state reports
promotion-pass-incomplete, repair attempt 1 of 1; the queue records
promotion-apparatus-repair-exhausted and awaiting-decision.

## Observed cause

Both independent-review attempts queried source job traces against the
futon1b service on port 7073. Agency's invoke-job endpoint is on port 7070.
The initial reviewer first tried an evidence lookup, then an invoke-job
lookup on the same wrong port. Its successor repeated the wrong-port lookup.
The retained commands and 404 responses are in evidence.json, extracted only
from completed command events (no submission credentials or private reasoning).

Initial review: apm-role-61104d221b22a9a7bcb142fa58a4eacc9493f7d4689608fc444a01d1aa5e789c.
Successor: apm-role-0c06abd6c9348f917c772d532c3256dee628d05f770b776b1d9ff31dbcc74f22.

Five candidate verdicts were cannot-judge because the claimed failed and
successful proof moves could not be checked against those traces. None carried
review materialization. The pipeline reports review-evidence-not-materialized
before testing cannot-judge; this generic finding obscures the retrieval reason.
This incident establishes a routing failure, not database saturation.

Read-only probes reproduce 404 on 7073 and HTTP 200 with exact matching job IDs
on 7070, with 84 and 63 events respectively. Both source jobs have cancelled
terminal status. This does not erase their recorded events, but neither their
availability nor event count establishes any candidate's mathematical claim.
The independent reviewer must inspect the actual events and any truncation.
Response hashes identify this observation, not immutable endpoint snapshots.

## Bounded repair

live_promotion.clj now supplies the configured Agency invoke-job endpoint,
explicit source-attempt lookup URLs, URL-encoded IDs, and instructions to
check returned identity/events and report the exact failed URL/status.
Initial review, polling instructions and successor review share this guidance.
EvidenceEntry read references remain separate. No pinned role card, acceptance
invariant, retry budget or historical verdict changes.

Validation: live-promotion-test, 56 tests / 291 assertions, zero failures/errors;
clj-kondo zero errors/warnings; check-parens OK; git diff --check clean.
The new regression checks configured address, trailing slash, encoded IDs,
deduplication and preservation of cannot-judge when evidence is unavailable.

## Remaining recovery

This packet is a source repair, not live deployment or unpark. F224 was not
interrupted. Load the canonical repaired namespace at a safe controller boundary.
F223 then requires an explicitly recorded apparatus-recovery decision and a
new independent review with correct trace access, preserving both old attempts.
Do not reset the exhausted counter, edit the retained review into approval,
or mark promotion complete from these probes. If the current recovery protocol
cannot express that successor, extend and test that protocol first.

## Follow-up: shared trace reader and recovery constraint

The command history establishes the address carryover: evidence lookup on 7073,
then invoke/jobs on 7073, then the successor repeats the latter. It does not
establish the reviewer's private reason for choosing it.

Added scripts/apm-read-job.py and supplied its invocation through the shared
`typed-role-submission/command`, so all roles using that prompt receive a trace
reader rather than having to compose an endpoint. The reader uses the canonical
Agency default (7070), supports an explicit --agency-base, URL-encodes IDs,
checks returned identity, bounds each read to 30 seconds, and reports HTML HTTP
errors with URL/status. It makes no automatic retry or claim of witness validity.
This complements the promotion-specific configured URLs in 4ca4eb8a.

Validation: four Python tests (success/routing, wrong identity, HTML 404,
timeout/no retry); typed-role-submission-test 16 tests / 73 assertions; kondo
zero warnings/errors; parentheses OK; diff check clean. Actual read-only reader
invocation returned the exact first source ID and 84 events.

Recovery remains blocked by an explicit transition constraint:
`live-promotion/drive-step!` refuses an exhausted promotion pass, and
`problem-queue-supervisor/reconcile-park-decisions` explicitly records but does
not execute dispositions. The necessary structural extension is an operator-
authorized recovery record bound to the parked state digest, previous terminal
review ID, repair source identity and a one-use successor authorization. Validate
those bindings, archive the predecessor, and use the existing independent-review
successor path without resetting the old retry count. Test mismatched/stale
records, duplicate consumption and failed persistence before dispatch. Neither
source reload alone nor attaching a queue decision will recover F223.

No live reload, review dispatch, counter reset, or queue mutation in this packet.
