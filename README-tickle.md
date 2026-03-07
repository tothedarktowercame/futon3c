# Anatomy of Tickle

Tickle is the coordination agent in futon3c. It has several independent
layers that can be activated separately.

## Layers

### 1. Agent (always-on)

Tickle as a registered agent on the agency, backed by Haiku. It joins IRC
channels, responds to @mentions, and can participate in conversation.

```clojure
;; Register tickle-1 with Haiku (no timer, just an agent that talks)
(let [invoke-fn (dev/make-claude-invoke-fn
                  {:agent-id "tickle-1"
                   :model "claude-haiku-4-5-20251001"
                   :session-file (java.io.File. "/tmp/futon-session-id-tickle-1")
                   ...})]
  (rt/register-tickle! {:agent-id "tickle-1" :invoke-fn invoke-fn}))
```

This is the base layer. Everything else builds on top.

### 2. Watchdog (opt-in timer)

Periodic scan → page → escalate loop. Checks evidence timestamps to detect
stalled agents, pages them via IRC, and escalates to Joe if paging fails.

```clojure
(dev/start-tickle!)                              ;; default: 60s scan, 300s stale threshold
(dev/start-tickle! {:interval-ms 30000})         ;; scan every 30s
(dev/start-tickle! {:auto-restart? false})        ;; no auto-restart on escalation
(dev/stop-tickle!)                               ;; stop the timer
```

When started, registers with CYDER as `tickle-watchdog` — visible in
`*processes*` panel, inspectable via `/api/alpha/processes/tickle-watchdog`,
and manually steppable with `(cyder/step! "tickle-watchdog")`.

Scan results project to the `*Tickle*` Emacs buffer after each cycle.

**Key design**: Tickle stays in umwelt darkness — it infers liveness from
evidence timestamps, not by reading agent internals.

Source: `src/futon3c/agents/tickle.clj`

### 3. Conductor (opt-in timer)

LLM-backed decision loop. Reads IRC history + GitHub issue state, decides
what action to take (page an agent, assign work, pass). Uses a dedicated
Claude instance (`tickle-llm`) for decisions.

```clojure
(dev/start-tickle-llm!)         ;; register the decision-making Claude instance
(dev/start-tickle-conductor!)   ;; start the 2-minute decision loop
(dev/stop-tickle-conductor!)
```

Source: `dev/futon3c/dev.clj` (tickle-llm, tickle-conduct!)

### 4. Orchestrator (REPL-driven)

Issue-level work assignment: fetch GitHub issues, assign to agents, review
results. Each step is standalone — compose manually or via `run-issue-workflow!`.

```clojure
(dev/fetch-ti-issues!)           ;; list open tickle-integration issues
(dev/kick-all-ti-issues!)        ;; assign all to codex
(dev/tickle-smoke!)              ;; end-to-end smoke test
(dev/tickle-dashboard)           ;; print task lifecycle table
```

Source: `src/futon3c/agents/tickle_orchestrate.clj`

### 5. Work Queue (batch processing)

Feeds items through the orchestrator in batch. Currently wired for
CT entity extraction (313 PlanetMath entries). Resumable — skips entries
with existing evidence.

Source: `src/futon3c/agents/tickle_work_queue.clj`

### 6. Dispatch Queue (bell-driven)

Per-agent work queues. Tickle watches for completion signals on IRC and
dispatches the next task to the same agent. No timer — purely reactive.

```
Agent completes task → signals on IRC → Tickle dequeues → @mentions agent with next task
```

The queue is a map of `{agent-id -> [task1, task2, ...]}`. Tasks are
fed in from the REPL, the orchestrator, or IRC commands. When an agent
signals completion (DONE, BELL, or any recognized pattern), Tickle:

1. Validates the signal (correct task? correct agent?)
2. Records the completion
3. Pops the next task from that agent's queue
4. @mentions the agent with the new assignment

This is the core coordination primitive for multi-agent work: agents
work at their own pace, Tickle keeps them fed.

**Existing implementation** (tickle-lite): `dev/futon3c/dev.clj`
(`process-done-signals!`) — hardcoded for CT work queue, recognizes
`DONE #N :: <artifact>` format.

**Generalization needed**: extract per-agent queue as a first-class
data structure, support arbitrary task shapes (not just GH issue numbers),
wire completion detection into the Haiku agent layer so Tickle can
recognize natural-language completion signals (not just `DONE #N`).

## IRC integration

- `irc-recent` — last N messages from the IRC log ring buffer (200 max)
- `irc-catchup!` — formatted summary of recent IRC for REPL use
- `send-irc!` — send as any nick via persistent ngircd connection

## Files

| File | What |
|------|------|
| `src/futon3c/agents/tickle.clj` | Watchdog: scan → page → escalate |
| `src/futon3c/agents/tickle_orchestrate.clj` | Conductor: assign → review → report |
| `src/futon3c/agents/tickle_work_queue.clj` | CT batch work queue |
| `src/futon3c/social/bells.clj` | Bell dispatcher (standup + test bells) |
| `src/futon3c/blackboard.clj` | `:tickle` render method for scan visibility |
| `dev/futon3c/dev.clj` | All REPL helpers, tickle-llm, tickle-lite |
| `scripts/tickle-start` | Standalone bash launcher (no REPL needed) |

## Standalone mode

Tickle can run outside the dev REPL via `scripts/tickle-start`:

```bash
./scripts/tickle-start                          # default settings
./scripts/tickle-start --interval 30            # 30s scan interval
FUTON3C_EVIDENCE_BASE=http://linode:7070 ./scripts/tickle-start
```

This registers `tickle-1` via HTTP API and runs the watchdog loop as a
standalone Clojure process. Useful for running Tickle on a different
machine than the main server.
