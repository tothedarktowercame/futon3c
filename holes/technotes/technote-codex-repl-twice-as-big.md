# Technote: codex-repl is twice as big as claude-repl

## Size & Complexity

| | claude-repl | codex-repl |
|---|---|---|
| **Lines** | ~945 | ~1937 (2x) |
| **Functions** | ~35 | ~70 |
| **Config vars** | 7 | 15 |
| **Internal state vars** | 4 | 13 |

Both share a common base in `agent-chat.el` (~688 lines) for faces, buffer
management, markdown rendering, streaming filter scaffolding, and the
`agent-chat-send-input` abstraction.

## Architecture

**claude-repl** connects to `/api/alpha/invoke-stream` at a single hardcoded
base URL (`localhost:7070`). It streams NDJSON via `curl -N` and **displays
text incrementally** as it arrives — you see Claude typing in real time.

**codex-repl** also uses `/api/alpha/invoke-stream`, but with **multi-base API
discovery**: it probes the primary URL, then `agent-chat-agency-base-url`, then
a Drawbridge-discovered URL, caching the first reachable one. Streaming goes to
an **invoke dashboard** side-window rather than directly to chat — the full
response appears in chat only at completion.

## Features unique to claude-repl

1. **Auto-registration & agent reuse** — dynamically finds idle `claude-*`
   agents from the registry, reuses them to preserve identity/history
   (invariant I-1)
2. **Workspace isolation** — each Emacs daemon gets its own agent-id
   (`claude-1`, `claude-2`...) and session file, enabling simultaneous
   conversations
3. **Real-time inline streaming** — text and tool-use annotations appear
   character-by-character in the chat buffer with overlay faces
4. **Drawbridge session reset** — can clear sessions via Drawbridge REPL eval

## Features unique to codex-repl

1. **Invoke dashboard** (`*invoke: codex-repl*`) — live side-window with
   spinner, elapsed time, timestamped trace entries, and tool-call details
   (`C-c C-v` to show, `C-c C-l` to clear)
2. **Multi-base API fallback** — probes multiple candidate Agency URLs, falls
   back to remote if local is down
3. **IRC send routing** — extracts IRC send directives from Codex output,
   routes them through local or remote Agency with health-checking
4. **Registry heartbeat** — reports external invoke state to the JVM registry
   via Drawbridge every 5 seconds
5. **Routing diagnostics** — `C-c C-d` probes all candidate bases and displays
   transport health
6. **Rich modeline** — header-line showing agency/IRC availability, session ID,
   transport state
7. **Advanced event parsing** — handles OpenAI-style events (`thread.started`,
   `item.started`, `reasoning`, `turn.failed`) with humanized labels
8. **Model/sandbox/approval config** — exposes `codex-repl-model`
   ("gpt-5-codex"), `codex-repl-sandbox`, `codex-repl-reasoning-effort`,
   `codex-repl-approval-policy`
9. **Session persistence & buffer restoration** — robust recovery of stale
   buffers via marker repair

## Key Implementation Differences

| Aspect | claude-repl | codex-repl |
|---|---|---|
| **Streaming display** | Directly to chat buffer (real-time) | To invoke dashboard; chat at end |
| **Event types** | `text`, `tool_use`, `done` | `thread.started`, `item.*`, `reasoning`, `error`, `turn.failed` |
| **Agent registration** | Auto-registers, finds idle agents | Assumes pre-configured agent |
| **Error recovery** | Agent-not-found → re-register + retry | Stale session detection, multi-base fallback |
| **Evidence tags** | `["claude", "chat", "turn", ...]` | `["codex", "chat", "turn", ...]` |
| **Extra keybindings** | — | `C-c C-d` (diagnose), `C-c C-v` (trace), `C-c C-l` (clear trace) |

## Summary

**claude-repl** is lean and identity-focused — it auto-discovers agents,
streams inline, and isolates per-workspace. It's the simpler, more interactive
of the two.

**codex-repl** is ~2x the code and transport-aware — multi-base discovery, IRC
routing fallback, invoke dashboard, registry heartbeats, and routing
diagnostics. It's built for operational visibility in a more complex deployment.

Both share the same evidence-logging pattern and `agent-chat.el` foundation.
