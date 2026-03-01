# Tickle Agent Spec — Stall Detection Watchdog

Phase 2, Task 2.1 of M-peripheral-gauntlet.

## What Tickle Is

Tickle is the default-mode component: the pre-deliberative path that keeps
the system alive when the deliberative chain (Gates 1-7) stalls. When no
agent has emitted evidence for a configurable threshold, Tickle pages the
stalled agent. If paging fails, it escalates to Joe.

Tickle is the first placenta transfer (Gate 6): a function that Joe
currently performs manually ("hey, are you still working?") transferred to
infrastructure. If Tickle works, Joe can sleep while agents work — and
wake up to evidence of what happened, not silence.

## Wiring Diagram Position

From `gauntlet-wiring.edn`:

```edn
{:id :C-tickle
 :name "Tickle (watchdog)"
 :accepts #{:irc-stream :musn-state :aif-state}
 :produces #{:irc-stream :action-stream :par-record}
 :timescale :social}
```

Edges:
- Reads: `:I-irc` (who's talking), `:I-musn` (activity timestamps),
  `:C-aif-env` (AIF state, when available)
- Writes: `:O-irc-out` (poke messages), `:O-artifacts` (emergency actions),
  `:O-pars` (watchdog cycle records)

Tickle runs in parallel with the Gate 1-7 chain. It is NOT in the
deliberative pipeline — it monitors it from outside.

## Umwelt (What Tickle Perceives)

### Observable Surface

| Input | Source | What Tickle learns |
|-------|--------|--------------------|
| Evidence timestamps | futon1a via `estore/query*` | Last activity per agent (author field) |
| Registry snapshot | `reg/registered-agents` | Which agents exist and should be active |
| IRC stream | Relay bridge (when wired) | Who's talking right now |
| AIF state | Gate 4 output (future) | Agent confidence/fatigue signals |

### Active Surface (What Tickle Can Do)

| Action | Mechanism | When |
|--------|-----------|------|
| Ring test bell | `bells/ring-test-bell!` | First attempt — lightweight liveness probe |
| Send IRC message | `send-to-channel!` | Bell fails — nudge agent on #futon |
| Escalate to Joe | `notify-fn` (blackboard) | IRC page fails — human intervention needed |
| Emit evidence | `estore/append*` | Every scan cycle — watchdog audit trail |

### Darkness (What Tickle Cannot See)

- Agent internal state (context window, working memory, current task)
- Agent code or configuration
- Whether an agent is thinking vs. stuck vs. deliberately idle
- Why an agent stopped — only that it stopped
- Human preferences about which stalls matter

Tickle infers liveness from the evidence trail, not from agent internals.
This is by design (P-1 umwelt constraint): the watchdog sees what any
peer would see — public evidence of activity, not private state.

## Heartbeat Cadence

| Parameter | Default | Env Override | Notes |
|-----------|---------|--------------|-------|
| Scan interval | 60s | `:interval-ms` | How often Tickle checks for stalls |
| Stale threshold | 300s (5 min) | `:threshold-seconds` | Silence longer than this = stall |
| Bell timeout | immediate | (hardcoded) | Test bell is synchronous |
| IRC page | fire-and-forget | — | Delivery assumed if send-fn succeeds |
| Evidence heartbeat | every cycle | — | One evidence entry per scan cycle |

The 5-minute default threshold reflects the social timescale: agents
engaged in task work (editing, testing) emit evidence every few seconds.
A 5-minute gap means the agent has stopped producing observable output.

## Escalation Path

```
  Scan cycle detects stall (agent silent > threshold)
      │
      ▼
  ring-test-bell!(agent-id)  ─── success ──→ {:paged? true :method :bell}
      │                                        (agent proved alive)
      │ failure / :skip-to-irc
      ▼
  send-to-channel!(room, "tickle-1", "paging <agent-id>")
      │                    ─── success ──→ {:paged? true :method :irc}
      │                                    (message delivered to channel)
      │ failure
      ▼
  escalate!(agent-id, :page-failed)
      │
      ▼
  notify-fn(agent-id, reason)  →  blackboard update for Joe
                                   (human must intervene)
```

In the current `dev.clj` wiring, `ring-test-bell!` is configured to always
return `{:ok false :error :skip-to-irc}` — skipping the bell and going
straight to IRC. This is intentional: the dispatch relay on IRC is the real
wake-up mechanism (it invokes the agent via `reg/invoke-agent!`).

## Current Implementation Status

**Code**: `src/futon3c/agents/tickle.clj` (228 lines)
**Tests**: `test/futon3c/agents/tickle_test.clj` (213 lines, 11 tests)

Implemented and tested:
- `scan-activity` — evidence-based liveness detection
- `detect-stalls` — filter for stale agents
- `page-agent!` — bell → IRC escalation
- `escalate!` — human notification
- `run-scan-cycle!` — orchestrate scan → page → escalate
- `start-watchdog!` — background loop with configurable interval
- Self-exclusion (Tickle doesn't page itself)
- Evidence emission per cycle

**Dev integration** (`dev.clj`):
- `start-tickle!` / `stop-tickle!` REPL helpers exist
- Wired to IRC `send-to-channel!` and blackboard escalation
- Configurable via REPL: `(dev/start-tickle! {:interval-ms 30000})`

## What's Missing for Phase 2 Completion

### 1. Dispatch Router Integration

Tickle runs as a background loop but is not wired into the dispatch
relay. When Tickle sends an IRC page, the dispatch relay should
recognize the message and invoke the target agent. Currently:

- Tickle sends: `"tickle paging claude-1: no recent activity observed"`
- Dispatch relay sees this on IRC but it's from `tickle-1`, not a human
- The mention-gating in `start-dispatch-relay!` checks for `@claude`
  format — Tickle's message doesn't use at-mention syntax

**Fix**: Either Tickle should format its pages as `@claude-1 ...` so the
dispatch relay picks them up, or the dispatch relay should recognize
Tickle pages as a special case.

### 2. IRC Server Running (Phase 1 Dependency)

Tickle's IRC paging requires the IRC server. Phase 1 (Gate 0) delivers
the IRC adapter. Without IRC, Tickle can still:
- Scan for stalls (evidence store only)
- Escalate to blackboard
- Emit its own evidence

But the primary wake-up mechanism (IRC page → dispatch relay → invoke)
needs Phase 1 complete.

### 3. AIF State Integration (Future — Gate 4)

The wiring diagram says Tickle accepts `:aif-state`. Currently it
doesn't read AIF signals. When Gate 4 (AIF as Environment) is
implemented, Tickle could use agent fatigue/confidence signals to
adjust its threshold dynamically:
- High fatigue + silence → lower threshold (page sooner)
- High confidence + silence → raise threshold (agent may be thinking)

This is a future enhancement, not a Phase 2 blocker.

### 4. PAR Emission on Escalation

The wiring diagram says Tickle produces `:par-record`. Currently it
emits scan evidence (`:tickle :scan` tags) but not structured PARs.
For Gate 6 completion, Tickle should emit a PAR when:
- An escalation occurs (what happened, what was tried, what failed)
- A long stall is resolved (what worked to wake the agent)

This provides the audit trail for placenta transfer evaluation:
did Tickle successfully replace the human function?

## Design Rationale

### Why evidence timestamps, not heartbeat pings?

Tickle could send PING-style probes and wait for PONG. Instead it reads
the evidence store. This is better because:

1. **No protocol coupling**: Tickle doesn't need a special heartbeat
   protocol with each agent. Any evidence emission counts as activity.
2. **Umwelt-consistent**: Tickle sees what any peer sees — public
   evidence. It doesn't have a privileged monitoring channel.
3. **Retroactive**: If Tickle restarts, it can immediately assess the
   state by reading existing evidence. No handshake needed.

### Why IRC as the primary wake-up mechanism?

The dispatch relay already handles IRC→invoke routing. Tickle pages via
IRC, which triggers the dispatch relay, which invokes the stalled agent.
The agent then emits evidence (from the invoke), which Tickle sees on
the next scan cycle. The loop closes naturally through existing
infrastructure.

### Why bell-first, then IRC?

The test bell is a lightweight liveness check (does the agent respond
at all?). If it works, no IRC message is needed — the agent is alive,
just quiet. If it fails, the IRC page is heavier but more visible: it
appears in the channel transcript, other agents see it, and the
dispatch relay can act on it.

In practice, `dev.clj` skips the bell and goes straight to IRC because
the bell doesn't reach agents reliably in the current architecture.
The bell path exists for future optimization.
