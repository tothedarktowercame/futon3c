# F218 watchdog recovery, 2026-09-11

Joe requested investigation and recovery of the idle APMV3 loop. This was an
operational recovery through the existing durable coordinator API, not a source
patch or service restart.

## Observed failure

Campaign: `jit-all-open-v3`; coordinator: `jit-queue:jit-all-open-v3`.
Authority files are `data/apm-campaigns/jit-all-open-v3/coordinator.edn`, its
`.watchdog.edn` sibling, and `data/apm-coordinators/registry.edn`.

Before recovery, the coordinator was stopped and durably disabled, at tick 66460.
Its last result was `:live-regulator-tick-threw`, with ExceptionInfo
`futon1b read timed out`, transport code `:futon1b-read-timeout`, and a 30000 ms
timeout reading evidence `e-apm-promotion-2588028e88b64f282e2b31e7ed5313b0`
from `http://127.0.0.1:7073/api/alpha/evidence/`.

The watchdog recorded `:external-job-deadline-exceeded` for tick job
`jit-tick-17c0bdb573b97be323a7633e5f04c95d5cc475fcf464b7f71f0cc779ba5d321b`,
deadline 1789082327294, and durable stop at 2026-09-10T23:20:50.130436312Z.
The semantic cursor remained F218 / guide-intervention-1 / event sequence 20.
The pending tick intent digest was
`b8eb15f5ad897234dddf289734b533942eef1e8b7e9d0645c9c03190254b0d70`.

The Guide job `apm-role-ec1ea5119e971343294e3f1ded6ff94af6ed67020ad185e3198ab7325f3a6df9`
had already submitted receipt
`2445c6727e603f2b2aed7f66342c5d0ed1886462cf9a092ebd4334a2d6ff6c4b`.
The job endpoint reported cancellation at 22:48:41Z with reason
`typed-submission wrapper reconciliation`; its tool event records a successful
typed submission. Thus the external deadline identifies the coordinator tick,
not a Guide that spent thirty minutes generating a response.

The exact timed-out evidence URL returned HTTP 200, 3329 bytes, in 0.233 seconds
during diagnosis. This establishes recovery of that dependency at observation
time; it does not establish why the store originally became unresponsive.

## Recovery executed

Via `scripts/proof-eval.sh` against the existing canonical JVM:

```clojure
(futon3c.apm.durable-coordinator/resume!
 "/home/joe/code/futon3c/data/apm-coordinators/registry.edn"
 "jit-queue:jit-all-open-v3"
 "Joe requested F218 recovery; timed-out evidence read now returns HTTP 200; Guide typed submission accepted. Recover expired tick intent through durable resume.")
```

The API returned `{:ok true :status :started}`. The expired intent was archived
in `:coordinator/superseded-intents` with disposition `:expired`, the original
digest and deadline, grace 120000 ms, and disposed-at 1789087115800.
No manual state edits, timeout changes, watchdog bypass, or JVM restart occurred.
Pre-existing dirty coordinator source/tests and compact-session changes were
not edited, committed, or reloaded by this recovery.

## Verification

At 00:38:35Z the durable coordinator was running at tick 66461. By 00:39:04Z it
was running at tick 66465, and the watchdog was `:watching` with semantic event
sequence 21. The new active job was
`apm-role-b318f6bba74c7414630df9b5af3af68dba60d51baa937542d36aec608c923dc6`.
Its live job endpoint identifies agent `f218-promotion-proctor`, created at
00:38:48Z and started at 00:38:53Z, state `running`, with a text event describing
independent candidate/pattern review. This is actual downstream work, beyond
merely enabling a scheduler. F218 completion is not claimed.

Checks were operational: exact dependency HTTP read, typed-submission/job
inspection, durable disposition inspection, and subsequent coordinator/watchdog
and downstream job observations. No source change required unit tests or Lean.
If the same store timeout recurs, investigate store latency and transport-failure
handling; this packet does not claim a permanent store-latency fix.
