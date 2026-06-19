# M-agents-queue

Date: 2026-06-13
Status: IDENTIFY — **PARKED** 2026-06-13 (Joe). Held: claude-1 on `/loop` manages
dispatch adequately for now; this is the more elaborate, self-organizing version for
later. Not started.

## The idea (Joe, 2026-06-13)

Replace **push-dispatch** (operator or claude-1 picks an idle agent and shoves work at
it; cf. `futon-dispatch`) with a **pull-based worker pool**: one shared queue of jobs;
participating agents are **self-reawakening** — when idle they claim the next job, do it,
and loop back to the queue. The operator (or any agent) just *enqueues*; the pool
self-organizes who does what.

Why it's "more interesting and easier to manage": load-balancing is automatic (whoever's
free pulls next), no agent-selection logic, resilient (a dead worker just stops pulling),
scales by adding workers.

## What already exists (reusable — the gap is small)

- `!on-idle` (registry.clj ~48) — fires when an agent finishes an invoke and goes idle.
  **This is the "send the worker back to the queue" hook, already wired.**
- `spawn-drainer!` / `signal-drainer!` (turn_queue.clj) — the worker loop: drain, then
  park on a lock until notified. Wake-on-new-work.
- tickle/conductor (`mark-idle! → !on-idle → tickle-queue → conductor dispatch`) — already
  an *idle-driven* dispatcher; half a pull-pool.
- durable queue (turn_queue.clj, EDN-persisted, msg-id dedup), kangaroo warm pouches
  (= the persistent workers, [[project_kangaroo]]), autorunner (self-reawakening pattern,
  [[project_claude_autorunner]], `peripheral/drive.clj`).

## What's genuinely new

1. **One unassigned pool queue** — jobs with no recipient yet (`:to :pool`). Today's
   `turn-queue` is keyed per-recipient (`:to agent-id`) — a push.
2. **Atomic claim** — an idle worker pops the next pool job and assigns it to *itself*
   (one worker, one job; reuse the existing `swap!`/dedup for atomicity).

Then the loop falls out of the existing pieces:
`operator → enqueue to :pool` · `worker idle → !on-idle → claim next → process → idle → …`
· `pool empty → park (signal-drainer!), wake on enqueue`.

## Invariants to pin BEFORE code (logic-model-first, cf. turn-delivery-invariants.md)

- **Exactly-once claim** — two workers never grab the same job.
- **No lost jobs** — worker dies mid-job ⇒ job returns to the pool (re-queue on failure).
- **Reply routing** — results flow back to the originator (operator's `*agents*` / a
  results channel), not lost in the worker.
- **Fairness / no starvation** + worker opt-in (claude-family are workers; codex stays a
  downstream handoff target, not an operator-pool worker).
- **Inspectable, not silent** — queue + claims are stack artifacts you can watch; a worker
  never ends a turn mid-tool-call ([[feedback_autorunner_inspectable_no_silent_turns]]).

## Relation

Evolution of [[project_agency_hardening]] + [[project_kangaroo]] with the autorunner as the
self-reawakening pattern. Would subsume the push-dispatch path: `futon-dispatch` becomes
"enqueue to pool" rather than "pick an idle agent." First artifact when un-parked: the
pool-queue logic model, then incremental build (pool queue → atomic claim → `!on-idle`
wire → futon-dispatch-enqueues-to-pool → reply routing).
