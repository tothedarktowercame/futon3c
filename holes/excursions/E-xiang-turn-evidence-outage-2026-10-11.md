# E-xiang-turn-evidence-outage-2026-10-11

Read-only diagnosis, 2026-10-11.  I did not reload or restart either JVM,
change the Emacs setting, edit a turn record, or append to futon1b.

## Finding

The outage is not a dead turn-service scheduler or a moved store.  The zone
rebuild restarted the primary Emacs daemon with
`session-mode-turn-recorder` at its source default, `files`, rather than the
previous live value `jvm`.  File mode bypasses the JVM turn service when a
reading settles.  `scripts/session_turn_analysis.py complete` writes the
analysis file and sets `analysis_status` to `"analyzed"` itself; the Emacs
file-mode reaper only notices that outcome.  Neither path calls the JVM's
`settle-evidence!`.  Consequently neither the `e-xiang-turn-<record-id>`
append nor `compute-ports!` runs.  Happened summaries continue because Emacs
also implements that part of the file pipeline.

This is a rebuild-related loss of live Emacs configuration, not damage to the
records or evidence store.

## 1. The path which sets `analyzed`

For a representative 2026-10-10 record, `turn-Ei1bca.json`, creation was
17:13:46Z and both the record and its sibling analysis file were updated at
17:14:49Z.  Its dispatch has a `job_id` but no `scheduled_at`; it has
`happened_summary`, but neither `ports` nor turn evidence.

That shape is systematic:

| record day | analyzed | with JVM `analysis_dispatch.scheduled_at` |
|---|---:|---:|
| 2026-10-04 | 162 | 144 |
| 2026-10-05 | 73 | 73 |
| 2026-10-06 | 8 | 8 |
| 2026-10-09 | 93 | 0 |
| 2026-10-10 | 117 | 0 |
| 2026-10-11 (at census) | 25 | 0 |

`scheduled_at` is written only by JVM `record-turn!`'s `:soon` path.  Thus all
analyzed turns up to the crash were using the JVM recorder, and all analyzed
turns after the rebuild were not.

The live primary Emacs server (`/run/user/1000/emacs/server`, started Oct 8
23:30) reports:

```elisp
(session-mode-turn-recorder session-mode-turn-jvm-base-url
 session-mode-analysis-agent)
;; => (files nil "象")
```

There is no persisted override in the Emacs configuration or code tree.  The
source default in `emacs/session-turn-analysis.el` is `files`.  In that mode:

1. `session_turn_analysis.py complete` publishes the sibling analysis and
   directly sets `request["analysis_status"] = "analyzed"` (lines 341ff).
2. `turn_dispatch_reap.py --apply` treats an existing analysis as authoritative
   and merely reports it analyzed; it does not contact the xiang turn route.
3. The Emacs reaper runs its own completion hooks.  It never invokes
   `futon3c.xiang.turn-service/settle-evidence!`.

By contrast, JVM mode schedules `turn-service/reap!`; since commit `0ec902b6`,
that reaper explicitly calls `settle-evidence!` when it discovers an analysis
published by the Python seat.  The missing call is therefore a path switch,
not a regression in `settle-evidence!`.

## 2. Live service and scheduler

The Agency JVM (PID 39171) started Oct 8 23:30:33 UTC.  A read-only Drawbridge
inspection of `futon3c.transport.http/!xiang-turn-service` found a fully built
service with:

- record store `{:dir "/home/joe/.emacs-graph/session-turn-analysis"}`;
- non-nil `:evidence!` and `:schedule!` effects;
- a `java.util.concurrent.Executors$DelegatedScheduledExecutorService`;
- `isShutdown = false` and `isTerminated = false`;
- its daemon thread alive and waiting in
  `ScheduledThreadPoolExecutor$DelayedWorkQueue.take`;
- no outstanding turn jobs in service state.

The delegated wrapper does not expose `getQueue`, so an exact queue count is
not available through the public interface.  The worker's plain `await`
(rather than timed wait), absence of outstanding jobs, and successful
schedule/cancel probe establish that it is alive and effectively idle, not
wedged or rejected.

The cached service is also not a pre-evidence instance: its config contains
the real `transport.http` evidence closure, not the default no-op.  Declared
agent-origin turns created after the rebuild have ports, further direct
evidence that this scheduler executes work when the live JVM path gives it
work.

## 3. Relationship to the disk failure/rebuild

The rebuild explains the boundary exactly by resetting process-local Emacs
customization:

- the primary Emacs server also started Oct 8 23:30;
- `session-mode-turn-recorder` currently has the source default `files`;
- no persistent `(setq session-mode-turn-recorder 'jvm)` exists;
- the last pre-rebuild analyzed records (Oct 6) all carry the JVM marker,
  while the first post-rebuild population (Oct 9) carries none.

The alternative disk/path explanations are contradicted by live evidence:

- Agency has `HOME=/home/joe`, cwd `/home/joe/code/futon3c`, and record store
  `/home/joe/.emacs-graph/session-turn-analysis`; there is no `/mnt/old` path
  in the service.
- The directory is owned by `joe:joe`, mode 0700, and current records and
  analyses are being written there by the same user.
- Agency has `FUTON3C_EVIDENCE_BACKEND=futon1b`; ordinary operator evidence is
  still reaching futon1b.
- The service construction has a real `:evidence!` closure and the scheduler
  is healthy.

So the crash did matter, but by erasing an unpersisted runtime selection when
Emacs restarted, not by corrupting a disk or half-initialising Agency.

## 4. Restoration and backfill

The immediate restoration is to persistently configure the primary Emacs
daemon with:

```elisp
(setq session-mode-turn-recorder 'jvm)
```

and set that value in the live daemon at an operator-chosen boundary.  No JVM
restart, namespace reload, or turn-service rebuild is indicated: master
already contains `0ec902b6`, the service already has its evidence effect, and
its scheduler is alive.  A fresh ordinary turn should then show
`analysis_dispatch.scheduled_at`, followed after settlement by both `ports`
and `e-xiang-turn-<id>`.

A backfill from Oct 6 is safe if it replays settlement for already analyzed
records rather than republishing analyses:

- `turn-evidence-entry` uses deterministic id `e-xiang-turn-<record-id>`;
- `append-evidence!` explicitly accepts idempotent/duplicate-id receipts
  quietly;
- `compute-ports!` recomputes the session and replaces the record's `ports`
  value, rather than accumulating entries.

The backfill will intentionally write both futon1b and record files, so it
should wait for Joe's authorization.  Including Oct 6 is conservative: eight
analyzed Oct 6 records already have ports and some evidence, and the duplicate
contract makes replay quiet.  Oct 9 onward is the demonstrated missing set.

## Conclusion

Best-supported cause: the rebuilt Emacs came up in file recorder mode.  This
fully accounts for the sharp date boundary, direct Python `analyzed` writes,
continued happened summaries, healthy idle JVM scheduler, and simultaneous
loss of the only two `settle-evidence!` products.  Restoring and persisting
JVM recorder mode is the repair; rebuilding Agency would not address the
cause.
