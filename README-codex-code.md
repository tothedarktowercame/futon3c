# Codex IRC Execution Contract (Design)

This document defines the required runtime model for Codex over IRC.

The problem being solved is not message relay. Relay already works.
The problem is guaranteeing that a user turn produces real work execution (or a truthful terminal failure), with auditable evidence.

## Contract

1. Every `@codex` invoke creates a durable `job-id` and a Clojure `promise` for final result.
2. IRC gets immediate `[accepted job-id]`.
3. A worker `future` starts and must drive that promise to a terminal state: `done`, `failed`, `timeout`, or `cancelled`.
4. For work-mode requests, `done` is legal only when runtime execution evidence is real: `executed? = true` or `tool-events + command-events > 0`.
5. If execution evidence is missing, terminal state is `failed no-execution-evidence` (never fake `done`).

## Runtime Flow

1. On inbound IRC mention, create a job record in durable store with:
   - `:job/id`, `:job/state`, `:job/prompt`, `:job/surface`, `:job/started-at`, `:job/trace-id`.
2. Create a `promise` for the same job id.
3. Start a worker `future` bound to that job id.
4. Worker runs Codex invoke and streams structured `on-event` updates into the job timeline.
5. First execution event emits `[running job-id]` to IRC.
6. Progress heartbeats/events emit `[progress job-id] ...`.
7. Terminal event emits exactly one of `[done ...]`, `[failed ...]`, `[timeout ...]`, `[cancelled ...]` and records delivery receipt for the trace id.
8. Promise is delivered exactly once.
9. Job state is immutable after terminal.

## Evidence Rules

1. `done` for work-mode must carry runtime evidence fields from structured events, not model prose.
2. Raw event stream is persisted as a job artifact with hash and counts.
3. IRC summary is a projection of persisted job state, not a source of truth.
4. `!job <id>` must show canonical status: `state`, `executed?`, `tool-events`, `command-events`, artifact path.

## Invariants

1. `accepted -> terminal` always.
2. `done(work-mode) -> execution evidence present` always.
3. `failed/timeout -> reason + artifact + delivery receipt` always.
4. Every terminal result is either delivered to the origin surface or marked explicitly as delivery-failed.

## User-Visible Behavior Goal

This model restores normal chat-agent semantics on IRC:

1. user submits turn,
2. agent acknowledges,
3. agent runs (with optional progress),
4. agent returns truthful final result.

No superficial "work claimed" replies are allowed to complete a job.

## Implementation Checklist

1. Introduce explicit invoke job state machine (`queued`, `running`, `done`, `failed`, `timeout`, `cancelled`).
2. Bind each job to a `promise` and enforce single terminal delivery.
3. Persist job timeline/events and terminal artifact metadata.
4. Gate `done` on execution evidence for work-mode prompts.
5. Add `!job <id>` status command backed by durable job state.
6. Add integration tests for:
   - accepted -> running -> done with real execution evidence,
   - accepted -> failed on no-execution-evidence,
   - accepted -> timeout with receipt,
   - restart recovery for in-flight jobs.
