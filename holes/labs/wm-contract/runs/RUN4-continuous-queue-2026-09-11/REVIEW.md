# Queue review — 2026-09-11, Codex-17

Reviewed 4d4599fd. Not accepted for activation yet.
Existing focused tests independently pass: 5 tests / 35 assertions.

Executed review-repro.clj in a disposable JVM and roots:
- resume on a held queue with its scheduler still registered throws :already-started.
- It has already overwritten the durable :held state with :stopped / :operator-resume-required.
- A directory at queue-state.edn returns a fresh :not-started state/cursor 0 rather than refusing.

Additional inspected concerns requiring gates:
- start!/resume! state mutations do not share tick!/stop!'s JVM/OS lock.
- valid-state? admits arbitrary in-flight maps, missing fields, non-ISO timestamps,
  and cursor movement without completed-entry evidence.
- config-digest omits execution authority roots and full cohort binding; replacing
  server/controller roots can retain the queue digest and cursor.
- A catch after successful started-state publication uses the earlier state,
  potentially losing its recorded click identity.
- On restart, the persisted in-flight click is not used for waiting; only a new
  :trial-started return invokes await-click!.
- Current positive progress tests redefine series/step!. The only actual service
  test uses deliberately malformed config; it does not establish composed success.
- stop! validates mutable source files first: source drift may prevent an operator
  stopping local scheduling. Scheduled exceptions outside the inner catch vanish.

Require focused corruption/race/recovery controls plus a materialized disposable
async service/queue lifecycle using actual controller, cohort, binding, recording,
visibility and terminal readers. Task core/environment may be isolated ports;
validator/step substitutes cannot establish that composed gate.

No live queue, capacity, click, restart or old evidence mutation occurred.

## Follow-up review of 5150c54a

Independent fast 6/44 and materialized 1/9 pass. Original resume now retains the
click and returns running; directory state refuses. Those corrections accepted.

Remaining recovery seam reproduced in lost-completion-repro.clj: with persisted
in-flight identity but no process-local completion promise, actual await-click!
returns not-tracked; queue holds :click-incomplete and never consults the durable
service reader (0 calls). A completed durable trial cannot recover this way after
JVM loss. Require exact-existing-start inspection that cannot become admission if
its marker disappears. Incomplete/corrupt evidence still holds.

The materialized positive fixture currently activates a cohort but replaces the
whole core with a map; no start/checkpoint/close API is invoked there and no cohort
count is asserted. Strengthen the actual cohort lifecycle gate rather than
claiming activation alone proves cohort consumption. Also cover recovery after
terminal publication and loss of the completion promise, plus unknown-incomplete
refusal with no new click.

## Review of 70a28055

Independent queue fast7/48 and materialized lost-promise1/11 pass. Recovery now
uses inspect-started! and exact-click checking under the controller lock; the
single-trial lost-promise correction is accepted within that scope.

Multi-trial inspection still needs correction. step*'s prepare-trial closure
requires a started file and attaches the same required-existing-click-id to
EVERY trial during controller preflight. An unstarted later trial therefore
refuses even when the requested earlier click is valid; two different started
clicks cannot both match one required click. Scope inspection to the matched
trial while preserving all manifest authority and prohibiting later dispatch.
An actual two-trial test is required, not a single-trial configuration exception.

The current materialized positive fixture still isolates the complete task core;
its real cohort activation does not itself establish core checkpoint consumption.
Keep the separate real full-core cohort gate explicit until those are composed.
No live activation accepted by this review.
