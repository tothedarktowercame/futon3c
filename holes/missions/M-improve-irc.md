# M-improve-irc — IRC Surface Improvements for Agent Coordination

## Status: OPEN

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

### P1: irc-read! endpoint
Add `GET /api/alpha/irc/history?channel=#futon&limit=20` that returns
recent messages from a channel. Requires adding a message buffer to the
IRC server (it currently doesn't store history).

### P2: irc-post! in surface contract
Already works — agents with network access can `curl` the existing
`POST /api/alpha/irc/send` endpoint. Just needs documenting in the
surface contract template (as already done for Claude).

### P3: Chat vs task mode in dispatch relay
If the @mention message is short (< 100 chars, no code blocks), use a
short timeout (30s) and prepend "Respond briefly, this is IRC chat" to
the prompt. Long/structured messages use full task mode.

### P4: Agent-initiated IRC posting
Include the IRC send curl command in `make-assign-prompt` so agents can
post progress updates mid-task. Codex with full sandbox can use this.

## Architectural Insight

IRC should be a **readable/writable surface** that agents inhabit, not a
trigger mechanism that controls them. The distinction: "IRC controls the
agent" vs "the agent uses IRC." Surface contracts give agents information
about their environment (I-3 compliant); dispatch relays impose control
flow (closer to delegation).

## References

- `src/futon3c/transport/irc.clj` — IRC server, `send-to-channel!`
- `src/futon3c/transport/http.clj` — `handle-irc-send` endpoint
- `dev/futon3c/dev.clj` — `start-dispatch-relay!`
- `library/realtime/surface-map.flexiarg` — surface map pattern
- Commit `f5c3e25` — surface contract enforcement
