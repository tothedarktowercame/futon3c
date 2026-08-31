# C91 — bounded testing service implementation and provisional health

Date: 2026-08-31

Implementation status: opt-in.  One transient service is created per
`bg.py launch-test` job under `futon-testing.slice`.  The slice has
`TasksMax=1024`, each ordinary job has `TasksMax=256`, and a fifth concurrent
submission is refused loudly.  Logs, receipts, and the small durable registry
live in `/tmp/futon-bounded-tests`; `test-kill` records cancellation separately
from the test/resource verdict.  This is boot-scoped retention by design.

This first run does **not** establish that the service is healthy.  It did
establish that its verdict can say no.

## Commands

```sh
systemctl --user set-property futon-testing.slice TasksMax=1024
python3 scripts/bg.py launch-test \
  "python3 scripts/bounded_test_pressure_fixture.py" \
  --agent wm-organization --label c91-pressure \
  --dir /home/joe/code/futon3c --tasks-max 16
python3 scripts/bg.py launch-test "sleep 3; echo durable-complete" \
  --agent wm-organization --label c91-durability \
  --dir /home/joe/code/futon3c
python3 scripts/bg.py launch-test "clojure -T:build ci" \
  --agent wm-organization --label c91-futon2-ci \
  --dir /home/joe/code/futon2
python3 scripts/bg.py test-status JOB_ID
python3 scripts/bg.py test-kill JOB_ID
systemctl --user show futon-testing.slice \
  -p ActiveState -p TasksCurrent -p TasksMax -p ControlGroup
```

## Five running criteria

### 1. Containment holds

Measurement: `futon3c-zone.service/pids.events:max` before and after each
terminal job.  The durability probe and real futon2 CI both recorded delta 0,
including while the CI job reached its own 256-task ceiling.

Failing condition: any positive Agency `pids.events:max` delta attributable to
a bounded test interval, or a test unit whose `ControlGroup` is not below
`futon-testing.slice`.

Current observation: pass on two runs; sample is too small for a health claim.

### 2. Verdicts are honest

Measurement: receipts where `inner-exit == 0`, resource state is bad, and
`outer-exit == 0`.

Failing condition: count greater than zero.  This is a class-1 infrastructure
defect.

Current count: 0.  Two deliberately informative false-green cases were
observed and both were rejected by the outer verdict:

- tiny-budget control: inner green, `pids.events:max` delta 2, outer exit 125;
- real futon2 CI: 1,023 tests / 6,155 assertions green, peak 256,
  `pids.events:max` delta 4 and native-thread `EAGAIN`, outer exit 125.

### 3. Durability holds

Measurement: the `sleep 3; echo durable-complete` submitter exited immediately;
the systemd-owned job subsequently completed, emitted its receipt, and exited
zero.  Submission-to-start was 43 ms.

Failing condition: a systemd-accepted job disappears when its `bg.py` submitter
exits, or reaches terminal state without a recoverable unit status and receipt.

Current observation: pass on one explicit caller-disappearance probe.

### 4. The budget is right

Measurement: `pids-peak` in every receipt.  Ordinary observations so far are
2 for the durability probe and 256 for futon2 CI.  The production sample is too
small for a p95.

Too-tight condition: p95 approaches 256 or any production job fails because of
the task budget.  Over-provisioned condition: after at least 30 production runs,
the observed maximum remains below half the limit.

Current observation: **256 is measured too tight**, because the first real CI
hit 256 and failed for resource reasons despite green tests.  Do not promote
the opt-in path to a default until a second trial limit is approved and
measured.  The clipped observation does not establish the required new limit.

### 5. Admission does not stall

Measurement: `submitted-at` to wrapper `started-at` in every receipt.  Observed
starts were 34 ms and 43 ms.  Four concurrent sleepers were admitted; a fifth
was immediately refused as `admission-cap`, then the four were explicitly
cancelled and recorded as cancellation rather than test failure.

Failing condition: start wait exceeds that job's eventual run duration.  This
implementation refuses at four rather than silently queueing, so refusal rate
must also be reported; it must never spill work into the Agency cgroup.

Current observation: no queued stall; one intentional fifth-job refusal.

## Retirement criterion

Evaluate after a rolling window of 30 completed, non-control jobs.  Retire the
service if containment-caused failures exceed genuine test-caused failures in
that window: it is then costing more executable truth than it buys.  Disable it
immediately, without waiting for 30 runs, if the dishonest-verdict count becomes
nonzero or containment pressure increments the Agency's `pids.events:max`.

Retirement means returning `bg.py` test submission to its prior path while
retaining receipts for diagnosis; it does not mean declaring the failed runs
green.
