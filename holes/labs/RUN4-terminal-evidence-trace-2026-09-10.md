# RUN4 terminal-evidence producer trace

Codex-10, 2026-09-10. Build/test preparation only; no live RUN4 invocation.

## Durable chain that exists

1. `futon3c.wm.runner-service/click!` atomically claims the one-serving-JVM
   slot and returns exactly `{:click-id id :started-at iso}` on acceptance, or
   `{:rejected :already-running :click-id existing-id}` while busy
   (`src/futon3c/wm/runner_service.clj:416-448`). This return is admission
   evidence only, not terminal evidence.
2. The worker calls `futon2.aif.full-loop-runner/run-opportunity!`; only after
   that call returns does `close-click!` persist a click/run binding
   (`src/futon3c/wm/runner_service.clj:377-396`, `:319-355`). A thrown worker
   call instead updates volatile service status to `:service-failed`; it does
   not persist a click/run binding (`:357-375`). Service idle therefore cannot
   establish a terminal outcome.
3. A click/run binding currently stores schema, click ID, the full-loop's
   internal attempt ID, raw full-loop outcome, an observational run ID, and
   run-record status/path (`runner_service.clj:210-315`). It is atomic-renamed
   and directory-fsynced (`:263-315`), but the `:durability` field is added only
   to the function's returned map after that fsync; it is not present in the
   durable binding bytes (`:292-304`). `:binding-status
   :verified` means only that run ID and click ID agree with the referenced run
   record (`:217-245`); it is not a task-success verdict.
4. The full-loop durable run record stores run ID, click ID, start time,
   selector seam, trace flag, route and (when present) `:run4/task-pin`
   (`/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj:279-307`). The
   pin is taken from selection/construction checkpoints (`:286-296`) and binds
   the exact task-pin identity selected at `:1000-1013`.
5. The full-loop in-memory result does contain the authoritative raw outcome,
   all five checkpoints, and closing data (`full_loop_runner.clj:2403-2583`).
   Grounded success is produced only after reviewer/build/grounding gates and
   closes as `:grounded-change` (`:3152-3289`). Initialization and other
   failures carry typed failure kind/stage in `:data` (`:3290-3445`). These
   fields are not presently copied into the durable run record or click/run
   binding, apart from the top-level raw outcome.

The identity join currently available is:

`series attempt reservation -> accepted click -> click/run binding -> verified
run record -> exact RUN4 task-pin identity`.

The outer series attempt ID is bound to the click by the RUN4 admission store;
the click/run binding's `:attempt/id` is the distinct internal full-loop attempt
ID and must not be substituted for it.

## Terminal-consumer boundary

A read-only consumer can safely establish a successful terminal result only
when every join above is strict, immutable source digests remain fresh, binding
durability is confirmed, the run record is present/verified, the exact pin
identity agrees, and the raw outcome is `:grounded-change`. Missing, truncated,
duplicate, mismatched, unavailable or refuted artifacts must yield
wait/reconciliation or refusal. Click acceptance, worker-thread completion,
service idle and `await-click! :completed` are never success.

The current durable artifacts cannot distinguish a confirmed binding fsync
from the post-rename/fsync-failure case after process restart. They are also
insufficient to classify every non-success
as task `:failed`, task `:blocked`, or infrastructure `:unsafe`. The binding
retains only `:outcome`; it drops the full-loop result's checkpoint judgments,
failure kind/stage, repair obligation, reviewer/build evidence and grounding
witness. Guessing from outcome names would invent terminal semantics.

## Smallest producer addition required

At the existing `result-summary`/`persist-click-run-binding!` call boundary
(`runner_service.clj:210-337`), persist a strict, versioned projection of the
already-returned full-loop result: exact pin identity, internal attempt ID,
outcome, required checkpoint terminal judgments, failure kind/stage, reviewer
and build gate results, and grounding witness. Bind that projection digest to
the click/run record and validate it before exposure. This is a natural
projection of existing runtime values, not a new verdict.

The remaining policy fork is the explicit mapping from those typed full-loop
terminal states to the series vocabulary (`:succeeded`, `:failed`, `:blocked`,
and infrastructure `:unsafe`). Until that mapping is commissioned, a consumer
may prove `:grounded-change -> :succeeded` but must leave all other outcomes
indeterminate/refused rather than advancing the series.
