# M-improve-irc — IRC Surface Improvements for Agent Coordination

## Status: DONE

## Context

During the Tickle integration testing mission, we discovered that IRC
coordination between agents has harness-level limitations. The dispatch
relay treats every @mention as a full agent invocation (codex exec, 5+ min),
making chat-style coordination impractical. Agents also lack the ability
to independently read IRC history.

## Problems

1. **Dispatch relay is all-or-nothing**: Every @mention triggers a full
   `reg/invoke-agent!` call. No distinction between "quick chat" and
   "implement this feature." A simple "say pong" becomes a 3-minute
   Codex session.

2. **No lightweight response path**: Codex can't post quick replies to
   IRC during or after work. It can only return a full result that the
   relay posts back.

3. **No read access**: Agents can't catch up on IRC history. They only
   see messages that trigger @mention invocations.

## Proposed Improvements

### P1: irc-read! endpoint — DONE
`GET /api/alpha/irc/history?channel=%23futon&limit=20` queries the evidence
store for IRC messages (no separate buffer needed — evidence already captures
all IRC traffic). Returns chat-friendly `{:nick :text :at :channel}` format.
`handle-irc-history` in http.clj.

### P2: irc-post! in surface contract — DONE
Already works — agents with network access can `curl` the existing
`POST /api/alpha/irc/send` endpoint. Documented in surface contract.
Commit `62856c1` added authoritative surface headers via `wrap-surface-header`.

### P3: Chat vs task mode in dispatch relay — DONE
Bridge classifies mentions via `_is_brief()` (< 100 chars, no code fences,
no newlines). Brief → `Mode: brief` surface contract ("Respond in 1-2 short
lines"). Task → `Mode: task` with full work instructions. Uses surface
contracts per CLAUDE.md, not capability restriction.

### P4: Agent-initiated IRC posting — DONE
Task-mode surface contract now includes the `curl` command for
`POST /api/alpha/irc/send` so agents can post progress updates mid-task.
`_surface_context()` method in ngircd_bridge.py.

## Architectural Insight

IRC should be a **readable/writable surface** that agents inhabit, not a
trigger mechanism that controls them. The distinction: "IRC controls the
agent" vs "the agent uses IRC." Surface contracts give agents information
about their environment (I-3 compliant); dispatch relays impose control
flow (closer to delegation).

## Completed Infrastructure (2026-03-06)

These supporting changes were implemented as prerequisites:

- **Bridge health check**: PID file locking prevents duplicate processes,
  JSON health file written every 30s, futon3c `/health` includes bridge status
- **Nick reclaim**: Bridge auto-reclaims desired nicks on PING cycles and
  when ghost connections quit
- **Systemd watchdog**: `Type=notify`, `WatchdogSec=90` auto-restarts wedged bridge
- **Surface header injection**: `wrap-surface-header` in http.clj prepends
  authoritative `--- CURRENT TURN ---` header so agents always know their surface
- **Async evidence writes**: `emit-invoke-evidence!` and `emit-review-snapshot!`
  are fire-and-forget (futures), plus 10s timeout on XTDB `put-and-sync!`
- **Auto-re-registration**: claude-repl.el re-registers on agent-not-found errors

## References

- `src/futon3c/transport/irc.clj` — IRC server, `send-to-channel!`
- `src/futon3c/transport/http.clj` — `handle-irc-send` endpoint
- `dev/futon3c/dev.clj` — `start-dispatch-relay!`
- `library/realtime/surface-map.flexiarg` — surface map pattern
- Commit `f5c3e25` — surface contract enforcement
