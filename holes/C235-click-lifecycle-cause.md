# C235 — C230 lifecycle stall versus C231 fixture race

Date: 2026-08-31. Discovery owner: `wm-organization`. No runner-service code
was changed; `wm-verbs` owned that file during this investigation.

## Finding

C230 stalls before the guarded lifecycle `swap!`, inside `report-idle!`. A
live `jstack` of the throwaway rehearsal's `wm-runner-click` thread showed:

```text
runner_service/report_idle! line 77
  registry/clear_external_invoke!
  registry/report_external_invoke!
  registry/publish_agents_status!
  registry/registry_status
  http/active_invoke_job_counts
  http/ensure_invoke_jobs_ledger!
  http/persist_invoke_jobs_ledger!
  clojure.core/pr-str
```

The thread was `RUNNABLE`, consuming CPU in recursive map printing. It had not
reached `close-click!`'s `swap!` at lines 125–133, so the click-id guard was not
the cause. There was no missing Agency network listener on this path: the slow
operation was local registry publication and invoke-ledger serialization.

This proves a lifecycle operation can remain projected as `running? true` with
`last-result nil` after the runner has returned and written its output. It does
not prove the serialization can never terminate; it proves it exceeds the
five-second closure bound and sits ahead of the authoritative state update.

## Relationship to C231

The defects are causally related but not identical.

- C231 fixed a fixture ownership error: setup reset shared status and registry
  without joining the prior worker. A slow worker could therefore publish into
  the next test's registry. Its separate completion promise and exact join are
  the right repair for cross-fixture corruption.
- C230 exposes why the prior worker remains live: its close/fail path performs
  fallible, potentially expensive registry publication before clearing the
  lifecycle projection. C231 does not reorder or bound that publication, so it
  does not make a production click report completion promptly.

Thus the C231 fix should remain, but it does not cover C230. A separate repair
must decide the lifecycle/publication ordering and what to record if registry
publication fails.

## Namespace pattern

The ordering occurs three times in `runner_service.clj`:

1. `report-phase!`: apparatus lookup and `reg/update-agent!` precede the phase
   `swap!`;
2. `close-click!`: `report-idle!` precedes the closing `swap!`;
3. `fail-click!`: `report-idle!` precedes the failing `swap!`.

That is a namespace-level pattern, not one isolated line. Once the desired
ordering/error policy is fixed, a small source lint can reject a registry
publication before the corresponding lifecycle transition. Writing that lint
before the policy decision would merely encode today's ordering, so it is not
part of this discovery delivery.

## Evidence commands

```sh
clojure -M:test:test-all -i :slow -n futon3c.wm.chain-rehearsal-test
jstack <throwaway-jvm-pid>   # inspect the wm-runner-click frame
python3 scripts/bg.py launch-test \
  'clojure -M:test:test-all -i :slow -n futon3c.wm.chain-rehearsal-test' \
  --agent wm-organization --label c235-lifecycle-diagnosis \
  --dir /home/joe/code/futon3c --window measurement
```
