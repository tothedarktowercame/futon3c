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
