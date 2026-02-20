# Futon3c System Snapshot: What Exists / What's Next

Date: 2026-02-20

This document is a working orientation for coding agents. It is about
current reality, not intent.

## What Exists (Verified)

### 1) Build and test baseline

- Test command: `make test`
- Current result (2026-02-20): `723 tests`, `2469 assertions`, `0 failures`, `0 errors`

### 2) Runtime entrypoints

- `make dev` boots futon1a + futon3c HTTP/WS + IRC transport + Drawbridge
- `make claude` and `make codex` launch session pickers
- `make tickle` launches liveness watchdog
- `make codex-autowake` runs unattended Codex issue loop
- `make status` reports health, agents, and recent evidence

### 3) Agency + social pipeline

- Registry and lifecycle are wired:
  - register: `POST /api/alpha/agents`
  - list: `GET /api/alpha/agents`
  - get one: `GET /api/alpha/agents/:id`
  - deregister: `DELETE /api/alpha/agents/:id`
- Social stages are implemented and tested:
  - `S-presence`, `S-authenticate`, `S-mode`, `S-dispatch`, `S-persist`, `S-validate`

### 4) Transport adapters

- HTTP adapter is wired (`src/futon3c/transport/http.clj`)
- WebSocket adapter is wired (`src/futon3c/transport/ws.clj`)
- IRC adapter is wired and stability-tested (`src/futon3c/transport/irc.clj`)
- `/health` includes:
  - agent/session counts
  - evidence count
  - `started-at`
  - `uptime-seconds`
  - per-agent summary

### 5) Evidence landscape

- Core store + protocol are wired:
  - protocol: `src/futon3c/evidence/backend.clj`
  - API: `src/futon3c/evidence/store.clj`
- Backends:
  - in-memory `AtomBackend`
  - durable `XtdbBackend`
  - remote `HttpBackend` adapter
- HTTP evidence routes:
  - append: `POST /api/alpha/evidence`
  - query: `GET /api/alpha/evidence`
  - count: `GET /api/alpha/evidence/count`
  - single: `GET /api/alpha/evidence/:id`
  - chain: `GET /api/alpha/evidence/:id/chain`
- Query filters available at HTTP edge:
  - `type`, `claim-type`, `author`, `pattern-id`, `subject-type`, `subject-id`, `since`, `limit`, `tag`, `include-ephemeral`

### 6) Peripheral runtime

- Implemented peripherals:
  - `:explore`, `:edit`, `:test`, `:deploy`, `:reflect`, `:proof`, `:discipline`, `:chat`, `:mission`, `:alfworld`, `:mission-control`
- Hop protocol is wired with explicit exit-condition semantics

### 7) Mission control

- Persistent session service is implemented (`src/futon3c/mission_control/service.clj`)
- Supports start/list/get/resume/step/stop and snapshot reload from `storage/mission-control/sessions.edn`
- Usable from Drawbridge/nREPL in the dev JVM

### 8) Operational automation

- `scripts/codex-autowake` now:
  - consumes PSR/PUR context before each cycle
  - emits mission traceability evidence (event, mission, pattern, commit)
  - emits PUR and checkpoint evidence during execution
- Recent commits are concentrated on operational readiness, evidence filters/counting, and health observability

## What's Next (Ranked)

### Next 1: Single source of truth for status

Problem:
- Multiple docs disagree on current numbers/status (for example old test counts and pre-completion "pending" traceability entries).

Target:
- Add one generated status artifact (or scripted refresh) that updates:
  - test counts
  - implemented endpoints
  - wired vs shaped peripheral status
  - latest mission checkpoint state

Why first:
- Reduces agent rework loops caused by stale narrative docs.

### Next 2: Close remaining PSR/PUR mesh gates

Problem:
- Mission notes show Gate C (transport-native interleaving) partial and Gate D (ALFWorld-triggered validation) pending.

Target:
- Land executable live validations for C and D with saved artifacts and repeatable commands.

Why second:
- The system is already strong structurally; this finishes real-world coordination validation paths.

### Next 3: Keep runtime/docs in lockstep

Problem:
- Runtime behavior and docs drift quickly (for example infra defaults and peripheral status), causing rework loops.

Target:
- Add one refresh path that updates `README.md`, `docs/system-now-next.md`, and evidence-ledger counts from current runtime/tests.

Why third:
- Prevents agents from rebuilding decisions from stale prose.

### Next 4: Improve Evidence backend parity for remote agents

Problem:
- `HttpBackend` query support is narrower than HTTP API capabilities (not all filters/facets are forwarded).

Target:
- Extend `src/futon3c/evidence/http_backend.clj` query mapping to cover API filter parity.

Why fourth:
- Reduces behavior skew between in-process and remote-agent evidence queries.

### Next 5: Mission-control API surface (optional but high leverage)

Problem:
- Mission control is powerful but mostly exposed through in-JVM REPL/Drawbridge workflows.

Target:
- Add thin HTTP endpoints for mission-control session lifecycle to support external automation clients.

Why fifth:
- Makes mission-control workflows easier for non-REPL agents/tools.

## Current Decision Rule

When prioritizing new work, prefer tasks that reduce coordination ambiguity
for agents over adding new capability classes. In practice:

1. Keep "what exists" accurate.
2. Close partially completed gates.
3. Only then add new surfaces.
