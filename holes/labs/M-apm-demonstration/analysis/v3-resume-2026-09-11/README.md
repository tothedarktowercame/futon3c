# V3 resumed after store tuning

Joe authorized resuming the regular APM V3 loop on 2026-09-11. The initial
runtime observation found no running coordinator, a disabled registration,
`:regulator/status :stopped`, and no pending intent. No frame reset or manual
dispatch was needed.

Before resuming, reloaded the canonical checkout namespaces for evidence-store
recovery, live promotion, durable coordinator and JIT coordinator adapter.
The first two include repair commit `4b7b6e13`. The pre-existing uncommitted
watchdog repair passed 33 tests / 237 assertions, clj-kondo and Emacs parentheses
checks before reload. Its source hash and diff are retained here; the owner's
source and test files were not staged or modified by this resume packet.
`loaded-source.json` records all four source hashes and repository HEAD.

Resumed only `jit-queue:jit-all-open-v3` through `durable-coordinator/resume!`
against `data/apm-coordinators/registry.edn`, with reason
`:joe-authorized-resume-after-store-tuning`, store commit `a1ebfa1` and recovery
commit `4b7b6e13`. The API returned `:ok true :status :started`. Epoch 31 claimed
tick 66729 at 2026-09-11T02:38:41.780937410Z, with pending job identity
`jit-tick-8e69173f999296a005c5cf7779aceb9dd1f249b5635e84b4f7a4e1a8aa0fd28b`.
This is an internal coordinator identity, not an Agency invocation-job record.

Restarted the Voxterm user service to activate the previously tested display
repair `2128b42`; the futon3c JVM was not restarted. The watchdog refreshed and
the display changed from the stale supervisor-gone alert to `state: ok`,
`alert: null`, with running/enabled lifecycle and a claimed tick. Both sampled
display responses are retained as `status-*.json`.

F218/m03J03 remains in `guide-intervention-1` at the retained observations.
Actual thread inspection shows the coordinator publishing a memory snapshot
and awaiting bounded visibility checks against the store, rather than merely
claiming a tick with no executing thread. See `work-observation.edn`. Store
health exposed eight workers and zero queued requests in the sampled responses.
This establishes resumed execution and a healthy supervisor, **not completion
of F218 or proof of long-term reliability**. No consistency guard, timeout or
retry bound was weakened, and no other loop was resumed.
