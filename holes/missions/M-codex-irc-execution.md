# M-codex-irc-execution — Guaranteed IRC Execution Contract for Codex

**Status:** DONE (2026-03-08)

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

## 5. VERIFY (2026-03-07, pass 2)

- [x] Implement job state machine and persistence.
- [x] Implement canonical `!job <id>` query.
- [x] Add integration tests for accepted->terminal invariants.
- [x] Add tests for no-evidence mission/work failure path.
- [x] Add restart/recovery tests.
- [x] Validate completion criteria with concrete evidence.

### 5.1 Implementation slice completed

1. `src/futon3c/transport/http.clj`
   - Refactored invoke handling into shared `build-invoke-response`.
   - Added sync fallback for `POST /api/alpha/invoke` when `:async-channel` is absent (Ring direct-call path), while preserving async server behavior.
   - Implemented invoke-layer durable job ledger with file-backed persistence (`FUTON3C_INVOKE_JOBS_FILE`, default `/tmp/futon3c-invoke-jobs.edn`).
   - Added canonical endpoints:
     - `GET /api/alpha/invoke/jobs`
     - `GET /api/alpha/invoke/jobs/:id`
   - Added startup recovery rule: stale `queued|running` jobs are marked `failed` with `worker-lost-on-restart`.
   - Wired delivery receipt updates (`/api/alpha/invoke-delivery`) into invoke-job delivery state via `trace-id`.

2. `scripts/ngircd_bridge.py`
   - Passes canonical `job-id` through `/api/alpha/invoke`.
   - Added `!job <id>` command that queries invoke-layer canonical job state (`/api/alpha/invoke/jobs/:id`).
   - Updated help surface to include `!job`.

3. Existing execution-evidence enforcement retained as authoritative:
   - Engine-level guard: `dev/futon3c/dev.clj` (`make-codex-invoke-fn` retry + hard fail on repeated no-evidence work claims).
   - HTTP-level guard: `src/futon3c/transport/http.clj` (`invoke-no-execution-evidence` rejection).

### 5.2 Verification evidence (commands + outcomes)

1. `clojure -M:test -n futon3c.dev-irc-summary-test`
   - Result: pass (`11 tests`, `43 assertions`, `0 failures`, `0 errors`).
   - Confirms no-evidence mission/work replies are rejected after enforcement retry.

2. `clojure -M:test -n futon3c.transport.http-test`
   - Result: pass (`52 tests`, `218 assertions`, `0 failures`, `0 errors`).
   - Confirms `/api/alpha/invoke` behavior, invoke job round-trip query, terminal-on-failure invariant, and restart recovery behavior.

3. `clojure -M:test -n futon3c.social.whistles-test`
   - Result: pass (`12 tests`, `38 assertions`, `0 failures`, `0 errors`).
   - Confirms whistle-surface delivery receipt recording path.

### 5.3 Completion criteria check (IDENTIFY -> VERIFY)

1. **No mission/work invoke may end as `done` without execution evidence.**
   - Status: `met`.
   - Evidence: `dev_irc_summary_test` + `http_test` no-evidence rejection cases.

2. **Every accepted invoke reaches explicit terminal state (`done|failed|timeout|cancelled`).**
   - Status: `met`.
   - Evidence: invoke-job terminal snapshots + `invoke-job-failure-is-terminal` test; stale in-flight recovery also forces explicit terminal state.

3. **Terminal state records where result was delivered (or delivery failure).**
   - Status: `met`.
   - Evidence: `invoke-job-delivery-records-on-job` + delivery recorder tests (`http_test` + `whistles_test`) and trace receipt plumbing.

4. **Canonical job status query with execution fields and artifact pointer.**
   - Status: `met`.
   - Evidence: `GET /api/alpha/invoke/jobs/:id` + bridge `!job <id>` command; response includes execution map and artifact ref field.

5. **Restart/reload has no silent loss of in-flight terminal outcomes.**
   - Status: `met` (current non-resumable policy).
   - Evidence: durable ledger + recovery transition to explicit terminal failure (`worker-lost-on-restart`) validated by test.

### 5.4 Decision log (implementation-time)

1. **D1: Add sync fallback for `/api/alpha/invoke` in handler direct-call path.**
   - Reason: tests call handler as plain Ring fn (no async channel), which previously returned `nil` and masked invoke behavior.
   - Impact: improved determinism and parity between test and runtime logic; no change to async server semantics.

2. **D2: Keep enforcement authority in invoke layer, not bridge.**
   - Reason: aligns with DERIVE ownership principle (`I-2 transport does not create truth`).
   - Impact: bridge remains projection-only for completion semantics; canonical truth lives in invoke-layer ledger/API.

3. **D3: Use non-resumable restart policy for stale running jobs (mark failed).**
   - Reason: avoids fake `done` synthesis and preserves explicit terminal outcome invariants without speculative replay.
   - Impact: correctness first; resumable workers can be a future enhancement.

## 6. INSTANTIATE (2026-03-08, complete)

- [x] Run local end-to-end demo (API surface, assistant-driven).
- [x] Demonstrate terminal delivery recording with evidence.
- [x] Demonstrate no-evidence mission/work rejection.
- [x] Demonstrate restart and `!job` recovery path.
- [x] Run operator-driven IRC demo on live channel.
- [x] Append checkpoint with commands and artifacts.

### 6.1 Demo A — Local, assistant-driven (completed)

Runtime:
1. Started isolated dev runtime on `:48070` with dedicated ledger file:
   - `FUTON3C_INVOKE_JOBS_FILE=/tmp/futon3c-invoke-jobs-demo.edn`
   - `FUTON3C_PORT=48070`, `FUTON1A_PORT=48071`, `FUTON5_PORT=0`, `FUTON3C_IRC_PORT=0`.

Flow:
1. Registered local deterministic codex agent (`codex-demo`) via `POST /api/alpha/agents`.
2. Sent brief invoke (`prompt: "hello"`):
   - response `ok=true`, returned `job-id=invoke-1772921379378-3-bcd2c528`.
   - `GET /api/alpha/invoke/jobs/:id` shows state `done`, mode `brief`, events `[accepted,running,done]`.
3. Sent work-mode invoke (`state of play on FM-001`):
   - response `ok=false`, `error=invoke-no-execution-evidence`, `job-id=invoke-1772921379442-4-60bb4fd7`.
   - canonical job view shows state `failed`, terminal-code `no-execution-evidence`, events `[accepted,running,failed]`.
4. Sent real codex invoke (`codex-1`, short prompt) to get trace-id:
   - returned `job-id=invoke-1772921390764-6-5a783052`, `invoke-trace-id=invoke-00284731-05a0-4e25-9738-9f7f54f558f7`.
5. Recorded delivery via `POST /api/alpha/invoke-delivery`.
6. Queried job by id and verified delivery map updated:
   - `delivery.status=delivered`, `surface=instantiate`, `destination=demo-local`, with `recorded-at`.

Artifacts:
1. `/tmp/instantiate-register.json`
2. `/tmp/instantiate-brief.json`
3. `/tmp/instantiate-work.json`
4. `/tmp/instantiate-codex1.json`
5. `/tmp/instantiate-delivery.json`

### 6.2 Demo B — Restart recovery (completed)

1. Seeded stale running job in `/tmp/futon3c-invoke-jobs-demo.edn`:
   - `job-id=job-stale-demo`, state `running`.
2. Restarted runtime.
3. Queried `GET /api/alpha/invoke/jobs/job-stale-demo`.
4. Observed explicit recovery terminal:
   - state `failed`,
   - terminal-code `worker-lost-on-restart`,
   - appended failed event with message `recovered on startup`.

### 6.3 Demo C — Operator-driven IRC (completed 2026-03-08)

Live channel evidence:
1. `@codex testing IRC invocation` produced accepted + done terminal framing with session continuity.
2. Real execution task (`LOC per futon* repo`) returned computed results from command execution.
3. Behavior parity gap surfaced (`separate IRC messages` request): Codex produced a format-limit refusal while Claude executed multi-line postings.
4. Fixed at invoke-engine contract layer (behavior enforcement) and bridge/repl transport guidance.
5. Re-test confirmed corrected behavior: Codex posted multiple separate IRC lines successfully and no longer relied on fabricated transport constraints.

### 6.4 Checkpoint

Implemented and validated:
1. Canonical invoke-job model (queued/running/terminal) with file-backed durability.
2. Canonical query surfaces (`/api/alpha/invoke/jobs`, `/api/alpha/invoke/jobs/:id`, bridge `!job`).
3. Delivery-receipt integration into job state.
4. Startup recovery policy for stale in-flight jobs.

### 6.5 Bell + Whistle Surface Demo (2026-03-07)

1. **Whistle (blocking modem-like)**
   - Request: `POST /api/alpha/whistle` to `codex-1` with prompt `Reply exactly: whistle-ok-...`.
   - Result: immediate blocking response returned:
     - `ok=true`
     - `response=whistle-ok-1772923467`
     - `invoke-trace-id=invoke-07c64388-fcb0-46d9-b5b8-2c150ee678a6`.

2. **Bell (async SMS-like) — success branch**
   - Request: `POST /api/alpha/bell` with `job-id=bell-demo2-done-1772923593`.
   - Immediate ack: `202`, `{accepted:true, state:\"queued\", status-url:\"/api/alpha/invoke/jobs/...\"}`.
   - Terminal query (`GET /api/alpha/invoke/jobs/:id`):
     - `state=done`
     - events: `[accepted, running, done, delivery-recorded]`
     - delivery recorded: `surface=bell`, destination points to canonical job URL.

3. **Bell (async SMS-like) — evidence-gated failure branch**
   - Request: `POST /api/alpha/bell` with explicit task-mode command prompt (`job-id=bell-demo2-1772923467`).
   - Immediate ack: `202 accepted`.
   - Terminal query:
     - `state=failed`
     - `terminal-code=invoke-error`
     - `terminal-message=work-claim without execution evidence after enforcement retry`
     - delivery still recorded on bell surface.

This confirms bell/whistle semantic split and canonical terminal observability independent of caller process attachment.

### 6.6 Whistle Stream (Long-Running Partial Progress) Demo (2026-03-07)

1. **Endpoint**
   - Added `POST /api/alpha/whistle-stream` (NDJSON).
   - `POST /api/alpha/whistle` now supports `{\"stream\": true}` to use the same streaming path.

2. **Observed stream contract (live curl smoke)**
   - Initial line:
     - `{\"type\":\"started\", ... \"job-id\":\"invoke-...\"}`
   - Progress while running:
     - `{\"type\":\"job-event\", ... \"event\":{\"type\":\"accepted\"|\"running\"}}`
     - periodic `{\"type\":\"heartbeat\", ... \"elapsed-ms\":...}`
   - Terminal:
     - `{\"type\":\"done\",\"ok\":true|false,\"job\":{...}}`

3. **Delivery recording**
   - Terminal job state records `delivery-recorded` with:
     - `surface=whistle-stream`
     - destination `caller <id> (stream)`
     - note `whistle-stream-response` (or failure note on disconnect/send failure).

## 7. DOCUMENT (2026-03-08, complete)

- [x] Update README/docs with final execution contract and operator runbook.
- [x] Add cross-references from mission-control and IRC docs.
- [x] Document deferred follow-ons as candidate missions.

Documented outputs:
1. `README-codex-code.md` captures the core coding-agent contract (`work proposed -> work done` with evidence or explicit failure).
2. `README-bells-and-whistles.md` defines bell/whistle/whistle-stream transport contracts and delivery semantics.
3. `README.md` includes Codex WS bridge and codex-repl routing expectations for IRC delivery.

Deferred follow-ons (new mission candidates):
1. Add first-class progress-event projection (`[running]/[progress]`) for Codex on IRC parity with long-running CLI UX.
2. Extend canonical `!job` UX with richer queue views (`!jobs`) and compact per-job progress snapshots.
