# V3 decommission before V4

Joe commissioned retirement of V3 and resolution of the displayed M93J07 park.
The durable queue identifies that problem as F227; F228 is M94A03, at preflight.
The coordinator was stopped through its durable lifecycle API. `stop.edn` records
its disabled state and quiescence witness at tick 82098, with no active tick.

F227's original promotion reviewer returned prose but omitted the mandatory typed
submission. The original reviewer, in its retained session, recovered submission
856f8294163582224d19a6c00aeb35b750c6030788da2c0385b3afcc7e1e02fe for original job
apm-role-5a1aa8b0dfa0f1cd1f4634d20cb262ad87f7c182d9ea6bcc506cdd844d064817.
Recovery job: invoke-1789219392921-20351-1958f28b. The recovery does not itself
publish memories, complete a proof, or certify frame closure.

The queue previously had pause/resume but no permanent administrative retirement
state. The new `decommission` transition requires an exact archived queue identity,
archive path/hash, no outstanding park decisions or read hold, disabled coordinator,
no tick claim, no active jobs, and no remaining registered frame seats. The trusted
operator must actually observe those inputs and verify the archive before calling.
It clears active scheduling/resumptions but retains completed/parked history and
points to the full archived queue. Subsequent ticks return decommissioned without
reconciling decisions, creating frames or dispatching work; ordinary pause-resume
cannot reactivate it. No mathematical terminal receipt is invented.

Unfinished F227 learning and F228 preflight belong in the retained handoff, not in
V3's running queue. Their ledgers, workspaces, branch heads and accepted evidence
remain available for explicit V4 intake; they are not silently imported or marked
completed. Existing historic park decisions and store warnings remain preserved.

Validation: queue namespace 42 tests / 272 assertions passing; clj-kondo and Emacs
check-parens pass. Tests cover stale archive identity, missing observations, live
jobs/seats/tick, enabled coordinator, preserved history and no effects on replay.
Deployment/archive/seat-removal and queue readback receipts follow separately.

## Executed decommission

Canonical repair `6f02bb6b` was loaded from its own classpath. The new queue
transition was applied under the coordinator tick lock. The original reviewer
submission was independently read and passed the typed payload validator before
the append-only F227 park decision was reconciled. No phase result was forged.

The authoritative job-history scan showed no active V3 jobs. 272 idle/restored
frame registrations were removed through the registry lifecycle API; its durable
roster watch persisted removal. `pre-decommission.edn` and `seat-removals.edn`
retain the scope and receipts. `applied.edn` pins the reconciled queue archive at
`data/apm-decommission/jit-all-open-v3-2026-09-12/queue-reconciled.edn`, SHA256
`fd75207e7555eb1835639ab06fd8ba6ed2732a6799a34498bde414b48d6d0bf9`.
The original pre-decision queue is retained beside it. Both contain full historical
state; these runtime archives remain local, while their identity receipts are committed.

Execution correction: the first operator script passed a string to the Path-only
atomic persistence API after archive/seat removal. That queue write did not land.
Recovery inspected all completed effects and resumed only the final queue transition
with a Path, checking equality with the archived queue first. Its final write and
receipt landed before a subsequent malformed catch form failed compilation. Neither
seat removal nor the queue transition was rerun; verification read the durable results.

`verified.edn` at 13:34:06Z records the valid decommissioned queue, no active frame
or resumptions, no awaiting-decision parks, stopped/disabled coordinator with no tick
claim, no active V3 jobs, and zero remaining frame seats in both registry and saved
restart roster. Completed history, reconciled park history and store-warning repair
history exactly match the retained archive. `verify.clj` reproduces these read-only
checks. M93J07's interrupted learning phase and M94A03's preflight are retained work,
not newly claimed successes. No V4 installation, JVM restart, V2 or topology restart
occurred. Workspaces and historical ledgers are preserved for explicit future intake.
