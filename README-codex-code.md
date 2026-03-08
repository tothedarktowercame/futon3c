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

---

## Why This Wasn't Enough (M-codex-agent-behaviour, 2026-03-08)

The contract above solved the **plumbing**: durable job records, execution
evidence gating, delivery receipts. But Codex still didn't execute reliably
on IRC. It would write plans, prep local files, and claim completion — with
zero tool events. The plumbing faithfully recorded `executed?=false`, but
that was cold comfort when the user got "I prepped a file" on IRC.

### The gap was in the harness, not the agent

Codex works fine on the vanilla CLI. Ask a question, get an answer with tool
execution. The failure was in our invoke harness — specifically, the WebSocket
bridge path that IRC uses.

The local invoke path (`make-codex-invoke-fn` in dev.clj) already had
enforcement: detect when Codex claims work without execution evidence, retry
with a firmer prompt, cap at one retry. But the WS bridge
(`scripts/codex_ws_invoke_bridge.clj`) called Codex and returned the raw
result with no enforcement and no execution evidence tracking. The plumbing
contract gated `done` on evidence — but the bridge never produced the
evidence in the first place.

### Two failure modes

1. **Plans instead of executing.** Codex describes what it would do but
   fires zero tools. The text looks like work; the evidence says nothing
   happened.

2. **Ships artifacts to local buffers.** Codex does real work but writes
   output to its own context (`/tmp` files, local buffers) instead of
   emitting it through the coordination surface. From our side this looks
   identical to (1): we asked a question and got no real answer.

### What was added

**H-1: Q→A completeness.** The WS bridge now parses execution evidence from
Codex's NDJSON output stream (tool-events, command-events) and checks three
enforcement predicates after each invoke:

- `work-claim-without-execution?` — "I'll do X" with zero tool events
- `task-reply-without-execution?` — task-mode prompt, non-planning reply, no evidence
- `format-refusal?` — refuses feasible output format with invented limits

When triggered, the bridge retries once with an enforcement prompt that says
"execute one concrete first step now." If the retry also fails, a structured
error is returned instead of surfacing fake completion text.

**H-2: Diagnosability.** Every invoke outcome emits structured evidence:
`executed`, `tool-events`, `command-events`, `enforced-retry`. When
enforcement triggers, a separate `enforcement-retry` evidence entry records
the reason and initial result. A human reviewing the evidence trail can
diagnose what happened without reproducing the failure.

### Files

| File | What changed |
|------|-------------|
| `scripts/codex_ws_invoke_bridge.clj` | Execution counting, enforcement predicates, retry wiring, enriched evidence |
| `test/futon3c/agents/codex_enforcement_test.clj` | 16 tests / 103 assertions covering H-1 and H-2 |
| `holes/missions/M-codex-agent-behaviour.md` | Full mission record (IDENTIFY → INSTANTIATE) |

### Is this enough?

Probably. The enforcement retry matches the proven pattern from the local
invoke path, where it has been effective. The live IRC test (2026-03-08)
showed Codex executing correctly through the full path: IRC → Linode HTTP →
WS bridge → laptop Codex → tool execution → result back to IRC.

The remaining risk is that enforcement is duplicated across three places
(bridge script, dev.clj, test file). A follow-up (Option C in the mission
doc) would consolidate enforcement into the `codex-cli` adapter so all
invoke paths get it automatically. Until then, changes to enforcement
regexes need to be updated in all three locations.
