# M-codex-irc-execution — Guaranteed IRC Execution Contract for Codex

**Status:** ARGUE (2026-03-07)

## 1. IDENTIFY

- [x] **Motivation**
The gap is not message transport. IRC relay works in both directions. The real failure is semantic: Codex can emit convincing completion prose with zero runtime execution evidence (`executed?=false`, `tool-events=0`, `command-events=0`). That breaks trust and makes IRC feel like a simulation instead of a working agent surface.

The desired behavior is standard chat-agent behavior with guarantees:
1. user submits a turn,
2. system acknowledges and starts work,
3. optional progress arrives,
4. terminal result is truthful (`done` with evidence, or explicit failure/timeout).

- [x] **Theoretical anchoring**
1. `I-1 Agent Identity Is Singular`: codex-1 must behave consistently across surfaces.
2. `I-2 Transport Routes, It Does Not Create`: bridge relays, invoke engine does work.
3. `realtime/structured-events-only`: execution claims must come from runtime events, not prose.
4. `realtime/authoritative-transcript`: durable invoke/job record is source of truth; IRC lines are projections.

- [x] **Scope in/out**
In scope:
1. Define and enforce a mission/work execution contract for Codex over IRC.
2. Require execution evidence for mission/work completions.
3. Design durable job state model (`accepted -> running -> terminal`) with promise/future semantics.
4. Specify delivery guarantees and auditability.

Out of scope:
1. Replacing IRC transport.
2. Replacing Codex CLI.
3. Multi-agent scheduler redesign.
4. Full portfolio/governance redesign.

- [x] **Completion criteria**
1. No mission/work invoke may end as `done` without execution evidence.
2. Every accepted invoke reaches an explicit terminal state (`done|failed|timeout|cancelled`).
3. Terminal state records where result was delivered (or delivery failure).
4. A canonical job status query can show execution evidence fields and artifact pointer.
5. Restart/reload does not produce silent loss of in-flight terminal outcomes.

- [x] **Relationship to other missions**
Depends on:
1. `M-improve-irc` (bridge + invoke surfaces + delivery plumbing).
2. `M-futon3c-codex` (Codex invoke engine wiring).
3. `M-transport-adapters` (surface adapter assumptions).

Enables:
1. Reliable mission execution from IRC.
2. Portfolio/mission-control trust in IRC-origin evidence.

- [x] **Source material**
1. `holes/missions/M-codex-irc-execution.md` (this mission).
2. `README-codex-code.md` (contract statement).
3. `dev/futon3c/dev.clj` (Codex invoke engine and IRC relay integration).
4. `src/futon3c/transport/http.clj` (`/api/alpha/invoke` acceptance/response behavior).
5. `scripts/ngircd_bridge.py` (IRC accept/queue/done/fail projection).
6. `src/futon3c/agents/codex_cli.clj` (execution evidence extraction).
7. `test/futon3c/dev_irc_summary_test.clj`, `test/futon3c/transport/http_test.clj`.

- [x] **Owner and dependencies**
Owner: futon3c runtime/transport layer.
Dependencies: local Codex runtime, registry invoke path, IRC bridge process, Emacs/trace surfaces.

## 2. MAP

- [x] **Inventory existing infrastructure**
1. IRC bridge accepts mentions, queues invokes, posts terminal messages (`scripts/ngircd_bridge.py`).
2. Invoke API endpoint handles agent invocation (`src/futon3c/transport/http.clj`, `/api/alpha/invoke`).
3. Registry invoke path updates agent status and propagates invoke metadata (`src/futon3c/agency/registry.clj`).
4. Codex invoke engine emits runtime evidence (`dev/futon3c/dev.clj`, `src/futon3c/agents/codex_cli.clj`).
5. Delivery receipts exist (`/api/alpha/invoke-delivery` + recorder in `dev.clj`).

- [x] **Inventory existing data**
1. Invoke metadata already carries execution counters (`executed?`, `tool-events`, `command-events`).
2. Invoke trace artifacts already persist to `/tmp/futon-invoke-artifacts/*`.
3. Bridge already has `job-id` and queue depth, but no durable per-job state record.

- [x] **Identify ready vs missing**

| Ready (already present) | Missing (mission work) |
|---|---|
| IRC accepted/failed/done framing | Durable invoke/job state machine with formal transitions |
| Runtime execution evidence fields in invoke metadata | Promise/future contract exposed in job model |
| Delivery receipt endpoint + recorder | Canonical `!job <id>` status surface |
| Invoke trace artifact writing | Restart-safe in-flight/terminal reconciliation |
| Engine-level and HTTP-level no-evidence guards (initial versions) | Full model-level guarantee that accepted always reaches explicit terminal with auditable evidence |

- [x] **Answer survey questions (Q1–Q7)**

Q1. Where is execution evidence computed?
A1. `src/futon3c/agents/codex_cli.clj` in `run-codex-stream!` + `summarize-execution`.

Q2. Where is terminal `done` currently emitted to IRC?
A2. `scripts/ngircd_bridge.py` `_invoke_worker_loop` posts `[done ...]` on `response.ok`.

Q3. Where can we enforce non-fake completion independent of bridge text?
A3. `dev/futon3c/dev.clj` (`make-codex-invoke-fn`) and `src/futon3c/transport/http.clj` (`handle-invoke`) are authoritative enforcement points.

Q4. What caused recent false-positive completions?
A4. Mission/work prompts were sometimes classified as brief/task inconsistently; earlier guard keyed too narrowly to explicit `Mode: task`.

Q5. What hard truth is now confirmed by runtime behavior?
A5. No-evidence mission/work responses can now be forced to fail (`invoke-no-execution-evidence`) instead of silently accepted as `done`.

Q6. What remains unresolved after current patches?
A6. Design-level guarantee is still distributed across components; durable job lifecycle and canonical query surface are not yet complete.

Q7. What is the model-level missing piece?
A7. A first-class durable invoke job abstraction with explicit state transitions and single terminal delivery contract.

- [x] **Document surprises**
1. Surface-mode classification (`brief` vs `task`) materially affects guarantee behavior unless mission/work intent is separately recognized.
2. Bridge-layer patches can mask design flaws; enforcement must live in engine/API contracts.
3. Evidence plumbing already existed; missing piece is lifecycle semantics and durability, not raw telemetry.

## 3. DERIVE

- [x] Define job entity schema (`job-id`, state, timestamps, trace-id, evidence summary, artifact ref, delivery state).
- [x] Define state machine and legal transitions.
- [x] Define terminal-state immutability rule.
- [x] Define evidence gate semantics for mission/work completions.
- [x] Define promise/future semantics for accepted invokes.
- [x] Define `!job <id>` read model and output fields.
- [x] Define restart recovery semantics for queued/running jobs.
- [x] Record IF/HOWEVER/THEN/BECAUSE for key design choices.

### 3.1 Entity Types

1. `invoke-job`
   - Identity: `job-id` (string, globally unique, monotonic per node preferred).
   - Core fields:
     - `job-id`
     - `agent-id` (e.g., `codex-1`)
     - `surface` (`irc`, `http`, `whistle`, etc.)
     - `origin` (channel/user tuple for IRC)
     - `prompt`
     - `mode` (`work` | `brief` | `planning`)
     - `state` (`queued` | `running` | `done` | `failed` | `timeout` | `cancelled`)
     - `created-at`, `started-at`, `finished-at`
     - `trace-id` (invoke trace id)
     - `session-id`
     - `result-summary` (short projection-safe text)
     - `result-artifact` (path or URI)
     - `execution` map (`executed?`, `tool-events`, `command-events`)
     - `delivery` map (`status`, `surface`, `destination`, `recorded-at`, `error`)

2. `invoke-job-event`
   - Append-only event log with strict sequence:
     - `job-id`, `event-seq`, `event-type`, `at`, `payload`.
   - Event types:
     - `accepted`, `running`, `progress`, `done`, `failed`, `timeout`, `cancelled`, `delivery-recorded`.

3. `invoke-job-promise` (runtime-only)
   - In-memory `promise` keyed by `job-id`.
   - Delivered exactly once with terminal event payload.

### 3.2 Relation Types

1. `invoke-job` has-many `invoke-job-event`.
2. `invoke-job` has-one terminal event (`done|failed|timeout|cancelled`).
3. `invoke-job` has-one delivery record (last delivery attempt/result).
4. `invoke-job` references one artifact (`result-artifact`) and one trace (`trace-id`).

### 3.3 Invariant Rules

1. **I-J1 accepted-to-terminal:** every accepted job must eventually reach exactly one terminal state.
2. **I-J2 terminal immutability:** terminal jobs cannot transition to any other state.
3. **I-J3 done requires evidence (work-mode):** if `mode=work`, then `state=done` implies `execution.executed?` or `tool-events+command-events > 0`.
4. **I-J4 explicit no-evidence failure:** if work-mode result text exists but evidence is zero, terminal state is `failed` with code `no-execution-evidence`.
5. **I-J5 auditable delivery:** every terminal job must have a delivery record marked `delivered` or `delivery-failed`.
6. **I-J6 monotonic events:** `event-seq` is strictly increasing per job, no duplicates.

### 3.4 Data Flow

**Ownership principle:** The job state machine is a futon3c invoke-layer
concern, not a bridge concern. Per I-2 (Transport Routes, It Does Not
Create), the bridge is a surface that calls the API and projects results
to IRC. It does not create job entities, assign job IDs, manage state
transitions, or decide whether work was "real." All of that is
engine/peripheral responsibility.

The bridge's current `_next_job_id()` is a local workaround that gets
replaced by server-assigned IDs. The bridge gets simpler, not more complex.

1. IRC mention arrives (`@codex ...`) at bridge.
2. Bridge calls `POST /api/alpha/invoke` with prompt and surface metadata.
3. futon3c invoke layer creates `invoke-job(state=queued)`, assigns `job-id`, persists `accepted` event, returns `{job-id, state}` immediately.
4. Bridge projects `[accepted <job-id>]` to IRC using the server-assigned ID.
5. Invoke layer transitions job to `running`, records `started-at`, emits `running` event.
6. Codex invoke executes; structured runtime events update `progress` and execution counters.
7. Completion path:
   - If no invoke error and evidence gate passes: `done`.
   - Else: `failed|timeout|cancelled` (including `no-execution-evidence`).
8. Terminal event writes artifact/summary, updates job snapshot, delivers runtime promise.
9. Delivery subsystem attempts IRC post and records `delivery-recorded`.
10. Bridge (or any surface) can query canonical status via `!job <id>` / API.

**Implementation path:** Extend the invoke endpoint to return a durable
job-id, manage the state machine in the invoke layer (new `invoke_jobs.clj`
or extension of `dev.clj`), and simplify the bridge to consume rather than
construct job state.

### 3.5 Promise/Future Model

1. Job creation allocates a `promise` keyed by `job-id`.
2. Worker `future` owns execution and event emission for that `job-id`.
3. Promise deliver value is terminal envelope:
   - `{job-id state code message execution result-artifact trace-id delivery}`.
4. A guard prevents multiple deliveries (`compare-and-set` on state atom before terminal write).

### 3.6 Evidence Gate Semantics

1. Gate applies when prompt intent is `work` (task assignment, mission work, FM-* operational prompts).
2. `done` decision predicate:
   - pass if `executed? = true` OR `tool-events + command-events > 0`.
   - fail otherwise.
3. Planning-only text is legal only if state is `failed` or explicit non-work mode; it is not legal `done` for work-mode.
4. Gate is enforced in futon3c invoke layer (authoritative), never delegated to bridge heuristics.

### 3.7 View/UI Specification

1. IRC projection format:
   - `[accepted <job-id>] queued (...pending...)`
   - `[running <job-id>] started (session <sid>)`
   - `[progress <job-id>] <short event>`
   - `[done <job-id>] <summary> refs: ...`
   - `[failed <job-id>] <code>: <message>`

2. `!job <job-id>` output (single compact line):
   - `state`, `mode`, `executed?`, `tool-events`, `command-events`, `artifact`, `delivery`, `age`.

3. Optional `!jobs` output:
   - queue depth + running job ids + most recent terminal ids.

### 3.8 Persistence and Recovery

1. Authoritative job snapshot + append-only events are persisted durably (file-backed EDN/JSONL or store backend).
2. On restart:
   - reload snapshots and events,
   - mark stale `running` jobs as `failed: worker-lost-on-restart` unless explicit resume is implemented,
   - keep terminal jobs intact and queryable.
3. Recovery does not synthesize `done`; only explicit terminal evidence from prior run is replayed.

### 3.9 IF/HOWEVER/THEN/BECAUSE Decisions

1. **IF** bridge and runtime disagree about completion semantics, **HOWEVER** bridge currently emits user-visible `done`, **THEN** enforcement lives in futon3c invoke layer, **BECAUSE** transport must not define truth.
2. **IF** users need immediate acknowledgement, **HOWEVER** real execution may take 5-20 minutes, **THEN** model invoke as `accepted + future + terminal`, **BECAUSE** this matches chat-agent expectations and avoids fake immediacy.
3. **IF** delivery to IRC can fail after computation succeeds, **HOWEVER** terminal result must remain auditable, **THEN** persist terminal state before delivery attempt and record delivery outcome separately, **BECAUSE** computation truth and transport success are different concerns.
4. **IF** restart happens mid-job, **HOWEVER** in-memory queue state vanishes, **THEN** jobs are durable entities with explicit recovery transitions, **BECAUSE** reliability cannot depend on process uptime.
5. **IF** mission/work prompts vary in surface phrasing (`task`, `state of play`, `FM-*`), **HOWEVER** guarantee must be semantic not lexical, **THEN** mode classification is part of job creation and evidence gate uses mode, **BECAUSE** regex-only gating is brittle.

## 4. ARGUE

- [x] Pattern cross-reference against realtime and transport patterns.
- [x] Coherence check against IDENTIFY theory and constraints.
- [x] Trade-off summary (where enforcement lives, persistence backend, operational cost).
- [x] Plain-language 3-5 sentence argument.

### 4.1 Pattern Cross-Reference

| Pattern | Where it applies in this design | Why it matters |
|---|---|---|
| `realtime/connection-state-machine` | Job lifecycle states (`queued -> running -> terminal`) | Prevents ad-hoc callback flow from pretending to be state; makes illegal transitions impossible by construction. |
| `realtime/single-authority-registration` | Enforcement authority is futon3c invoke layer, not IRC bridge | Ensures one source of truth for completion semantics; avoids bridge/runtime split-brain. |
| `realtime/request-param-resilience` | Surface/mode intent extraction for work vs brief classification | Guarantees are semantic (mode/intent), not brittle on one textual marker. |
| `realtime/surface-map` | Surface projections (`IRC line`) separated from authoritative job model | Keeps transport presentation thin and prevents surface-specific confabulation from becoming truth. |
| `realtime/verify-after-start` | Restart recovery and rehydration of durable job state | Startup/restart cannot silently lose in-flight semantics; system must verify recovered state before serving queries. |
| `realtime/reconnect-with-backoff` | Delivery retries and delivery-failed terminal recording | Delivery is retried with bounded behavior; failure is explicit and auditable. |

### 4.2 Theoretical Coherence

1. A coding agent is defined here as: **intent -> executed state change (with evidence) OR explicit terminal failure**. This is surface-independent.
2. Coherent with `I-1` because codex-1 semantics are surface-invariant: same done/fail contract regardless of IRC vs other entry point.
3. Coherent with `I-2` because bridge only projects state; it no longer determines whether work was “real.”
4. Coherent with structured-events principle because completion truth is derived from runtime evidence fields and job events, not text claims.
5. Coherent with authoritative-transcript principle because terminal state + delivery record + artifact pointer become canonical and queryable.

### 4.3 Trade-Off Summary

1. **Enforcement location**
   - Choice: futon3c invoke layer.
   - Trade-off: bridge remains simple, but invoke layer carries more policy logic.
   - Rationale: policy belongs where evidence is authoritative.

2. **Durability backend**
   - Choice (proposed): local durable log/snapshot first (EDN/JSONL), with optional evidence-store integration later.
   - Trade-off: faster implementation and low dependency now, weaker multi-node query semantics initially.
   - Rationale: mission priority is guarantee correctness before distributed analytics.

3. **Strict no-evidence failure**
   - Choice: fail mission/work completions lacking evidence.
   - Trade-off: more explicit failures during transition period.
   - Rationale: honest failure is preferable to false success.

4. **Progress projections**
   - Choice: optional `[progress job-id]` projections backed by structured events.
   - Trade-off: more surface messages, but significantly better user trust during long runs.
   - Rationale: aligns with expected 5–20 minute execution UX.

### 4.4 Plain-Language Argument

A coding agent is not a chatbot that sounds plausible; it is a system that turns requested work into verifiable work results. That contract must hold on every surface, not just in one UI. The model here makes each request a real job with explicit states and evidence-gated completion, so “done” means work actually ran. If execution did not happen, the system says so as a failure instead of pretending success. IRC then becomes a faithful projection of the same core agent contract, not a weaker mode.

## 5. VERIFY (skeleton)

- [ ] Implement job state machine and persistence.
- [ ] Implement canonical `!job <id>` query.
- [ ] Add integration tests for accepted->terminal invariants.
- [ ] Add tests for no-evidence mission/work failure path.
- [ ] Add restart/recovery tests.
- [ ] Validate completion criteria with concrete evidence.

## 6. INSTANTIATE (skeleton)

- [ ] Run end-to-end IRC demo with real long-running Codex task.
- [ ] Demonstrate progress + terminal delivery with evidence.
- [ ] Demonstrate no-evidence mission/work rejection.
- [ ] Demonstrate restart and `!job` recovery path.
- [ ] Append checkpoint with commits and artifacts.

## 7. DOCUMENT (skeleton)

- [ ] Update README/docs with final execution contract and operator runbook.
- [ ] Add cross-references from mission-control and IRC docs.
- [ ] Document deferred follow-ons as candidate missions.
