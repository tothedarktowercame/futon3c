# Typed-submission wrapper cancellation: where the evidence goes

Discovery only, 2026-09-07.  No behaviour was changed.

## Conclusion

The two observations called “destroyed evidence” have different causes.

* The session id is genuinely unavailable after cancellation, but the cancel
  handler does not delete a previously stored id.  It finalizes the invoke row
  with whatever `:session-id` that row already has; for a running asynchronous
  invoke that is normally `nil`, because the worker only obtains and persists
  its result session id when the invoke returns.  The wrapper's pre-cancel poll
  is therefore the last possible observer when it contains an id.  A job
  cancelled before that observation has no recoverable id at this layer.
  (`src/futon3c/transport/http.clj:1555-1562`,
  `src/futon3c/transport/http.clj:4436-4452`,
  `src/futon3c/transport/http.clj:5956-5963`,
  `src/futon3c/apm/live_job_driver.clj:965-975`; jobs
  `apm-role-8d45c0b99fd09ed2dd70b940761dbe4086ab1ed3c235c3db83dbebe92a819ac1`
  and
  `apm-role-fab5baab5c75d6b40e9c070892200e99fe34c91ad0122fa4cf2756ee26697615`.)
* `:terminal-state :running` is not a failed cancellation transition.  It is
  deliberately the raw, pre-cancel observation copied into the content-addressed
  terminal-collection record.  The Agency invoke row separately becomes
  `cancelled`.  The collection also durably records that the authenticated
  submission is available, and later validation is supposed to synthesize a
  `:done` job from that submission.  (`src/futon3c/apm/live_job_driver.clj:692-702`,
  `src/futon3c/apm/live_job_driver.clj:990-1006`,
  `src/futon3c/apm/live_job_driver.clj:1154-1167`; job
  `apm-role-fb063e8a1402e124efd792ad746fbe14a89fe9e3ee21dff162002f8bcc9feb83`.)

The wrapper should continue to cancel, and terminal consumers should continue
to distinguish the authenticated submission authority from the invoke
wrapper's lifecycle.  The useful upstream improvement would be to make that
distinction explicit in the collection schema, not to relabel the raw wrapper
observation or keep a redundant worker alive.

## 1. Who cancels, and why

`live-learning-phases` supplies `drive!` with a `:cancel-fn` that POSTs to the
Agency cancel endpoint with reason `"typed-submission wrapper reconciliation"`.
(`src/futon3c/apm/live_learning_phases.clj:1285-1297`,
`src/futon3c/apm/job_port.clj:24-32`.)

On each drive tick the controller-owned authenticated-submission store is read
before the external job.  If a submission exists while the wrapper is still
nonterminal, the submission branch invokes that cancel function.  Its stated
rule is that an authenticated completion outranks stale live wrapper state;
the same branch then persists the submission and collection before any orphan
or redispatch decision.  (`src/futon3c/apm/live_job_driver.clj:947-965`,
`src/futon3c/apm/live_job_driver.clj:990-1006`.)

The cancel endpoint first wins the single-finalizer race, then conditionally
interrupts the job's process and its supervising worker.  The documented
constraints are (a) finalize before killing so the worker's error path cannot
replace `cancelled` with `failed`, and (b) kill only when the named job is the
agent's sole executing job, so cancelling a queued/redundant job cannot destroy
another turn.  (`src/futon3c/transport/http.clj:5912-5925`,
`src/futon3c/transport/http.clj:5944-5999`.)  No additional design constraint
explaining why a submitted role must keep executing was found; the opposite is
implemented: the authenticated result makes continued wrapper execution
redundant (`src/futon3c/apm/live_job_driver.clj:990-995`).

## 2. Session-id: exact loss boundary and last observer

The invoke row begins with `:session-id nil` (`src/futon3c/transport/http.clj:1291-1315`).
The asynchronous worker extracts `sid` only after its invocation returns and
passes it to `finalize-invoke-job!` (`src/futon3c/transport/http.clj:4423-4452`).
That finalizer unconditionally associates its `sid` argument into the terminal
row (`src/futon3c/transport/http.clj:1527-1567`).  Cancellation calls the same
finalizer with `(:session-id job)` from the still-running ledger row
(`src/futon3c/transport/http.clj:5956-5963`).  Thus the exact durable boundary
is `finalize-invoke-job!` line 1561: it writes `nil` because the running row has
not yet received the worker's eventual result id.  It is more precise to call
this premature finalization than deletion of stored evidence.

Within the wrapper, `observed-job` is read before `cancel-fn`; that poll is the
last observer and commit `f4ae8d5a` now copies its id into `:job/session-id`
before cancellation.  Later `restore-session-identity` replays that copy.
(`src/futon3c/apm/live_job_driver.clj:626-645`,
`src/futon3c/apm/live_job_driver.clj:965-975`,
`src/futon3c/apm/live_job_driver.clj:1007-1025`; commit `f4ae8d5a`.)  For a job
cancelled before any poll exposes an id, a last observer was **not found**.

The two f192 jobs both now report `state "cancelled"`,
`terminal-code "operator-cancelled"`, the wrapper-reconciliation message, and
`session-id nil` (jobs
`apm-role-8d45c0b99fd09ed2dd70b940761dbe4086ab1ed3c235c3db83dbebe92a819ac1`
and
`apm-role-fab5baab5c75d6b40e9c070892200e99fe34c91ad0122fa4cf2756ee26697615`).

Preserving this id for every cancellation would require moving session identity
publication earlier than worker completion: the agent backend/registry would
have to publish the newly established id into the invoke ledger while the turn
is running, before the wrapper can cancel it.  Merely changing the cancel
handler to “preserve the old value” cannot help when that old value is nil
(`src/futon3c/transport/http.clj:1291-1315`,
`src/futon3c/transport/http.clj:4436-4452`,
`src/futon3c/transport/http.clj:5958-5963`).  What would break is **not found**:
no invariant forbidding early publication was found.  It would, however, add a
new cross-thread durable update and race that needs an ownership rule against
the first-terminal-wins finalizer (`src/futon3c/transport/http.clj:1542-1573`).

## 3. Why terminal collection says `:running`

The driver polls the job, computes `terminal?`, then enters the submission
branch while that observation is still live.  It calls cancellation, but builds
`terminal-collection-record` from the already-read `job`; that function copies
`:state job` to `:terminal-state`.  Persistence follows only after cancellation
succeeds.  (`src/futon3c/apm/live_job_driver.clj:965-999`,
`src/futon3c/apm/live_job_driver.clj:1026-1033`.)  Therefore the exact write of
`:running` is `terminal-collection-record` line 697, based on the snapshot read
at line 965.  The subsequent Agency cancel cannot revise the content-addressed
collection.

The f191 durable file contains collection id
`c74933adb953e87875b08a994c49ae137bf7caa24e81a3d9d3fa58346af37d09`,
`:terminal-state :running`, `:submission/available? true`, and submission id
`630f733418046ff3b3cd9de978b781dec086b525b614418131f39c14a86143eb`;
its wrapper reconciliation names original job
`apm-role-fb063e8a1402e124efd792ad746fbe14a89fe9e3ee21dff162002f8bcc9feb83`.
That job's Agency row is actually `cancelled`, while repair job
`apm-role-fa1c715385ab618684bde824c13e579747c89295fab7f1813ea6efbb91870989`
is `done` with session id
`zai-dca83955-982a-4ad9-98fa-d5bbf85ff2f9` (those two job ids; durable file
`data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f191/live/student-attempt-2.edn:1`).
The recorded `process-interrupt :no-active-control` does not cause the stale
snapshot: finalization happens before interrupt is attempted
(`src/futon3c/transport/http.clj:5958-5985`).

Preserving both facts does not require changing `:terminal-state` to `:done`.
It requires retaining the raw observed wrapper state and adding or naming a
separate reconciliation disposition such as “authenticated submission
collected; wrapper cancelled.”  Replacing `:running` with `:done` would erase
the fact that the invoke never completed and would make
`terminal-collection-record` cease to be a record of its input job; because
`:collection/id` hashes that body, this also changes collection identity
(`src/futon3c/apm/live_job_driver.clj:692-702`).  Keeping the worker alive until
it naturally reaches `done` would spend compute after the controller already
has authoritative output and would abandon the explicit stale-wrapper
reconciliation rule (`src/futon3c/apm/live_job_driver.clj:990-1006`).

## 4. Recommendation

Leave the cancellation behavior alone.  It correctly terminates redundant
execution, records a real `cancelled` Agency terminal, and obeys the
single-finalizer and job-grain process constraints
(`src/futon3c/transport/http.clj:5912-5925`,
`src/futon3c/transport/http.clj:5944-5999`).

Consumer-side interpretation is necessary because two authorities are being
joined: the invoke ledger says whether the wrapper process finished, while the
authenticated submission store says whether the role delivered its contract.
The driver already encodes the correct join by synthesizing `:state :done` only
for validation when a collected submission is present
(`src/futon3c/apm/live_job_driver.clj:1149-1167`).  The f191 failure was a
consumer losing that distinction, not evidence that the cancellation failed;
commit `2feb07e1` recognizes the contradiction when collected authority and
`:typed-submission-missing` coexist (`src/futon3c/apm/live_job_driver.clj:1263-1288`).

Two upstream refinements are defensible as separate changes: publish session
identity into the invoke ledger as soon as it exists, and make the collection's
raw-wrapper-state versus reconciled-role-disposition fields explicit.  Neither
requires removing cancellation.  Until early session publication exists, the
before-first-poll case is intrinsically unavailable to the wrapper and must not
be charged as role failure; `live-learning-phases` now only requires a session
id from a job that actually reached `:done`
(`src/futon3c/apm/live_learning_phases.clj:449-463`).
