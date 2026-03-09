# Technote: Codex State Invariants — IRC Bridge / Agent Registry Integrity Gap

Date: 2026-03-09

## Problem

The following IRC exchange reveals a state inconsistency:

```
IRC:    [accepted codex-1773091585-5] queued (1 pending, timeout 1800s)
Agents: codex-1 [codex] idle (0s ago)
```

The IRC bridge claims codex has a queued job. The agent registry claims codex
is idle. Both are locally correct but globally contradictory to any observer
watching both surfaces.

This same class of gap caused the "6h silent on batch 4" false alarm — tickle
paged codex because no evidence was being emitted, even though codex was
working on a long job.

## Three Independent State Stores

The system has three places that track "what codex is doing," and they don't
synchronize during the critical acceptance window:

| Layer | What it tracks | Data store | Knows about jobs? |
|-------|---------------|------------|-------------------|
| **IRC bridge** (ngircd_bridge.py) | Local Python `queue.Queue` of pending invokes | In-memory per-bot queue | Yes — it created the job ID |
| **Invoke job ledger** (http.clj) | Canonical job state machine: queued->running->done/failed | File-backed EDN (`/tmp/futon3c-invoke-jobs.edn`) | Yes — this IS the job store |
| **Agent registry** (registry.clj) | Agent status: `:idle` or `:invoking` | Single atom `!registry` | Only during active `invoke-agent!` execution |

## The Timeline of the Gap

```
T0: IRC @codex mention received by bridge
T1: Bridge creates job-id codex-1773091585-5, enqueues locally   <-- "queued (1 pending)"
T2: Bridge posts [accepted ...] to IRC                           <-- PUBLIC COMMITMENT
T3: (Bridge worker thread hasn't dequeued yet)
T4: Agent registry reports codex-1 as :idle                      <-- CONTRADICTS T2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    GAP: job exists in bridge queue but not in server job ledger

T5: Bridge worker dequeues, POSTs /api/alpha/invoke
T6: Server creates invoke-job state="queued", calls invoke-agent!
T7: Registry transitions codex-1 to :invoking                    <-- NOW they agree
```

The contradiction exists between T1 and T5. The bridge has committed publicly
(on IRC) to a job that the server doesn't know about yet.

## What's Already Built (M-codex-irc-execution, 2026-03-08)

The invoke job ledger closes the "did codex actually execute anything?" question:

- Durable job state machine: `queued -> running -> succeeded|failed|timeout`
- `GET /api/alpha/invoke/jobs/:id` canonical query endpoint
- `!job <id>` IRC operator command
- Evidence gates: work-mode invokes require execution evidence or fail with
  `invoke-no-execution-evidence`
- Delivery receipt tracking (bridge confirms IRC delivery back to server)
- Restart recovery: stale in-flight jobs marked `worker-lost-on-restart`

This is real progress. But the job ledger starts tracking at server acceptance
(T6), not at bridge enqueue (T1). The integrity violation lives in the T1-T5
window, before the server knows.

## What Tickle Sees (and Doesn't)

Tickle monitors agents via evidence timestamps only — not the registry, not
the job ledger. From `tickle-spec.md`:

> Tickle stays in umwelt darkness. It deliberately infers liveness from shared
> evidence timestamps, not by reading private agent internals or job state.

Tickle cannot distinguish:
- "codex is idle" (nothing to do)
- "codex has a queued job it hasn't started yet" (bridge queue, pre-server)
- "codex is thinking deeply" (invoke active, no evidence yet)
- "codex is stuck" (invoke stalled)

This is why false-alarm paging happens on long jobs.

## Agent Registry State Model

The registry tracks two orthogonal concepts:

**Agent status** (`:idle` | `:invoking`):
- Transitions via `invoke-agent!` call (idle->invoking) and invoke completion
  (invoking->idle)
- Includes in-flight metadata: `:invoke-started-at`, `:invoke-prompt-preview`,
  `:invoke-activity`
- External codex detection hack: scans `ProcessHandle` for `codex exec`
  processes (registry.clj lines 491-539) to catch externally-running sessions

**WS connection state** (`:connecting` | `:connected` | `:disconnected`):
- Per-channel, managed by the WS transport layer
- R7 readiness handshake: connected only after `presence/verify` succeeds
- Independent of agent status — an agent can be `:idle` with an active WS
  connection, or `:invoking` with no WS connection (local invoke)

These are correctly separated. The problem is that neither reflects the
bridge's local queue.

## IRC Bridge Job Lifecycle

The bridge (ngircd_bridge.py) maintains its own state per bot:

1. IRC mention received -> `_next_job_id()` creates `{nick}-{timestamp}-{seq}`
2. Enqueue to local `queue.Queue(maxsize=20)` with job metadata
3. Post `[accepted {job_id}] queued ({N} pending, timeout {T}s)` to IRC
4. Worker thread dequeues -> checks agent status via `GET /api/alpha/agents/{id}`
5. If agent busy, waits; if idle, POSTs `POST /api/alpha/invoke` with job-id
6. Server creates canonical job entry, executes, returns result
7. Bridge posts result to IRC
8. Bridge POSTs `POST /api/alpha/invoke-delivery` with delivery receipt

Step 3 is the public commitment. Step 5 is when the server learns about it.
The gap is steps 3-5.

## Three Gaps to Close

### Gap 1: Bridge Queue Is Invisible to the Server

The bridge's Python `queue.Queue` is local state. Between "bridge enqueues"
(T1) and "bridge POSTs /invoke" (T5), no server-side entity knows the job
exists.

**Potential fixes:**
- (a) Bridge POSTs a `job-announced` event to the server immediately at T1,
  before the worker thread picks it up. Server creates a "pre-queued" or
  "announced" job entry.
- (b) Bridge defers the IRC `[accepted]` message until after the server
  acknowledges the POST /invoke. This eliminates the window but delays the
  user-visible ack.
- (c) Server exposes an `/api/alpha/invoke/announce` endpoint that the bridge
  calls synchronously before posting to IRC. The ack message includes the
  server-assigned job state.

### Gap 2: Agent Registry Doesn't Reflect Pending Work

The registry only knows `:idle` vs `:invoking`. There's no `:pending` or
`:has-queued-work` state. The external codex detection hack (ProcessHandle
scanning) catches externally-running sessions but not bridge-queued jobs.

**Potential fixes:**
- (a) Add a `:pending-jobs` count to the agent record, incremented when the
  job ledger accepts a job for that agent, decremented on terminal state.
- (b) The `*agents*` blackboard projection queries the job ledger for
  non-terminal jobs per agent and includes the count in the display.
- (c) Registry status report merges job ledger data: if an agent is `:idle`
  but has queued jobs in the ledger, report as `:idle (1 queued)`.

### Gap 3: No Single Status Query Reconciles All Three Stores

An operator asking "is codex busy?" must check IRC (bridge queue), the job
ledger (server-side job state), and the registry (invoke status) separately.

**Potential fix:**
- Unified status endpoint: `GET /api/alpha/agents/:id/status` returning:
  ```
  {:registry-state :idle|:invoking
   :queued-jobs [{:job-id "..." :queued-at "..." :source "irc"}]
   :in-flight-job {:job-id "..." :state "running" :started-at "..."}
   :last-evidence {:at "..." :type "..."}}
  ```

## Recommended Fix Priority

**Gap 1 is the integrity violation.** The public IRC commitment before server
awareness is the root cause. Fix (c) — synchronous announce before IRC ack —
is the cleanest because it:
- Eliminates the contradiction window entirely
- Keeps the bridge stateless for recovery (server is source of truth)
- Requires minimal bridge change (one HTTP call before the IRC post)
- Makes `!job <id>` work immediately after the `[accepted]` message

**Gap 2 is a display improvement.** The registry showing `:idle` when jobs are
queued is confusing but not a data integrity issue once Gap 1 is closed.

**Gap 3 is operational convenience.** Useful but not blocking.

## Related Missions and Documents

| Document | Location | Relevance |
|----------|----------|-----------|
| M-codex-irc-execution | `futon3c/holes/missions/M-codex-irc-execution.md` | Built the job ledger (completed 2026-03-08) |
| M-IRC-stability | `futon3c/holes/missions/M-IRC-stability.md` | Fixed 6 IRC failure modes (completed 2026-02-23) |
| M-dispatch-peripheral-bridge | `futon3c/holes/missions/M-dispatch-peripheral-bridge.md` | Evidence threading across dispatch boundary |
| M-agency-refactor | `futon3c/holes/missions/M-agency-refactor.md` | Agency invariants R1-R11, A0-A5 |
| M-agency-rebuild | `futon3/holes/missions/M-agency-rebuild.md` | Original agency invariants (superseded) |
| TN-tickle-ct-review-retrospective | `futon3c/holes/technotes/TN-tickle-ct-review-retrospective.md` | Postmortem: silent codex cascading failures |
| agency-topology | `futon3c/docs/agency-topology.md` | WS invoke flow showing divergence point |
| tickle-spec | `futon3c/holes/missions/tickle-spec.md` | Tickle's evidence-only visibility model |
| technote-codex-code-peripheral | `futon3c/docs/technote-codex-code-peripheral.md` | Why codex needs execution evidence |

## Key Implementation Files

| File | What it contains |
|------|-----------------|
| `futon3c/src/futon3c/agency/registry.clj` | Agent state atom, `:idle`/`:invoking` transitions, external codex detection |
| `futon3c/src/futon3c/transport/http.clj` | Invoke job ledger, state machine, API endpoints (lines 184-2472) |
| `futon3c/scripts/ngircd_bridge.py` | Local job queue, `[accepted]` message, invoke POST |
| `futon3c/src/futon3c/agents/tickle.clj` | Evidence-only liveness monitoring |
| `futon3c/src/futon3c/transport/ws.clj` | WS connection state, readiness handshake |
| `futon3c/src/futon3c/social/dispatch.clj` | Message routing (peripheral vs direct invoke) |
| `futon3c/src/futon3c/blackboard.clj` | Agent status projection to Emacs `*agents*` buffer |

## Invariant Proposal

**I-6: Public Commitment Requires Server Awareness**

No bridge or transport layer may announce job acceptance to an external surface
(IRC, WS, HTTP response) before the canonical job ledger has recorded the job.
The server is the source of truth; public-facing state must be derived from it,
not from local queues.

**Test:** Search for IRC/WS messages containing "accepted" or "queued". Verify
that each is preceded by a successful POST to `/api/alpha/invoke/announce` or
`/api/alpha/invoke`. If the announce call fails, the bridge must not post the
acceptance message.
