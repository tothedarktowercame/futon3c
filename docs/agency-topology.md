# Agency Topology — How Agents Connect

> See `agency-topology.mm` for the Mermaid diagram.

## The Two Machines

| Machine | Role | Agents | Network |
|---------|------|--------|---------|
| **Linode** | Server | claude-1 (local), codex-1 (remote slot) | Public IP, runs IRC |
| **Laptop** | Client | codex-1 (local) | NAT, no inbound ports |

Both run the same `futon3c` dev server (`dev/futon3c/dev.clj`) with
role-specific configuration via `FUTON3C_ROLE`.

## Agent Invocation Modes

There are two ways an agent gets invoked:

### Inline invoke-fn (same JVM)

The agent's CLI runs directly in the dev JVM via `ProcessBuilder` (Claude)
or `clojure.java.shell/sh` (Codex). The invoke-fn is registered with the
agent registry and called by `invoke-agent!`.

Used when: the agent's CLI is available on the same machine as the Agency.

Evidence lifecycle:
- `invoke-start` — emitted before process launch
- `invoke-heartbeat` — emitted every 30s by the invoke ticker
- `invoke-complete` — emitted when the process exits

Written directly to the XTDB evidence store (no HTTP round-trip).

### WS bridge invoke (cross-machine)

The agent runs on a different machine. A WS bridge connects outbound to
the remote Agency's `/agency/ws` endpoint, sends a `ready` handshake,
receives `invoke` frames, runs the CLI locally, and sends `invoke_result`
frames back.

Used when: the agent runs on the laptop but needs to be reachable from
the linode (where IRC and humans are).

## The Laptop→Linode Problem

The laptop is behind NAT — the linode can't dial in. Two solutions exist:

### 1. Outbound WS bridge (current approach)

The laptop's dev JVM opens a WS connection *to* the linode's `/agency/ws`.
It also HTTP-registers `codex-1` on the linode as a `ws-bridge` agent.
When someone on IRC says `@codex`, the linode's dispatch relay calls
`invoke-agent!("codex-1", ...)`, which finds the WS bridge connection
and sends an `invoke` frame. The laptop receives it, runs `codex exec`,
and sends the result back.

```
IRC → linode dispatch relay → invoke-agent! → WS invoke frame
  → [outbound WS from laptop] → laptop invoke-fn → codex exec
  → invoke_result frame → [back over same WS] → IRC response
```

Env vars on laptop:
- `FUTON3C_ROLE=laptop`
- `FUTON3C_PEERS=http://<linode-ip>:7070` (or `FUTON3C_LINODE_URL`)
- `FUTON3C_CODEX_WS_BRIDGE=true` (default for laptop role)

The bridge auto-detects the remote target and registers via HTTP.

### 2. Federation proxy (fallback)

When peers are configured, agent registrations are announced to peer
Agencies. The peer creates a proxy invoke-fn that forwards prompts
via `POST /api/alpha/invoke`. This works but adds HTTP latency and
doesn't maintain a persistent WS connection.

The outbound WS bridge sets `:skip-federation-proxy? true` in metadata
to avoid double-registration when both mechanisms are active.

## Invoke Flow Summary

### `@claude` on IRC (linode)

```
1. IRC PRIVMSG "#futon" ":joe!joe@localhost PRIVMSG #futon :@claude hello"
2. Dispatch relay matches mention, strips prefix → "hello"
3. reg/invoke-agent! "claude-1" "hello" 45000
4. Registry finds claude-1 with inline invoke-fn
5. invoke-fn acquires lock, emits invoke-start evidence
6. ProcessBuilder: claude -p "hello" --resume <sid> --output-format json
7. Ticker updates blackboard every 5s, emits evidence heartbeat every 30s
8. Process exits, parse JSON output
9. Emit invoke-complete evidence, persist session ID
10. Return {:result "..." :session-id "..."}
11. Dispatch relay sends result back to IRC
```

### `@codex` on IRC (linode → laptop)

```
1. IRC PRIVMSG → dispatch relay → reg/invoke-agent! "codex-1" "hello"
2. Registry finds codex-1 with no local invoke-fn, but ws-bridge available
3. ws-invoke/invoke! sends {"type":"invoke","invoke_id":"...","prompt":"hello"}
4. Frame travels over WS to laptop's bridge
5. Laptop's bridge calls inline invoke-fn (codex exec --json)
6. Codex CLI runs, result parsed
7. Bridge sends {"type":"invoke_result","invoke_id":"...","result":"..."}
8. ws-invoke/resolve! delivers promise, invoke-agent! returns
9. Dispatch relay sends result back to IRC
```

### Drawbridge /eval (agent-to-agent)

```
POST http://localhost:6768/eval
  {"code": "(futon3c.agency.registry/invoke-agent! \"codex-1\" \"prove lemma 3\")"}
```

Agents use Drawbridge to call each other through the same `invoke-agent!`
path. This gives full LISP power — proof tools, evidence queries, registry
introspection — all in the same JVM.

## Evidence During Long Invocations

Every invoke-fn (both Claude and Codex) emits evidence at three points:

| Event | When | Tags |
|-------|------|------|
| `invoke-start` | Before process launch | `invoke`, `invoke-start`, `<agent-id>` |
| `invoke-heartbeat` | Every 30s while running | `invoke`, `heartbeat`, `<agent-id>` |
| `invoke-complete` | After process exits | `invoke`, `invoke-complete`, `<agent-id>` |

Query with: `GET /api/alpha/evidence?tag=invoke&tag=claude-1`

The invoke ticker also updates the Emacs blackboard buffers (`*invoke: claude-1*`,
`*agents*`) every 5 seconds with elapsed time and file change detection.

## Configuration Reference

### Linode

```bash
FUTON3C_ROLE=linode
FUTON3C_REGISTER_CLAUDE=true   # default for linode
FUTON3C_REGISTER_CODEX=false   # default for linode (codex arrives via WS)
FUTON3C_RELAY_CLAUDE=true      # IRC dispatch relay
FUTON3C_RELAY_CODEX=true       # IRC dispatch relay (routes to WS bridge)
```

### Laptop

```bash
FUTON3C_ROLE=laptop
FUTON3C_REGISTER_CLAUDE=false  # default for laptop
FUTON3C_REGISTER_CODEX=true    # default for laptop
FUTON3C_CODEX_WS_BRIDGE=true   # default for laptop
FUTON3C_PEERS=http://<linode-ip>:7070
# or: FUTON3C_LINODE_URL=http://<linode-ip>:7070
```

### Shared

```bash
CLAUDE_BIN=claude              # path to claude CLI
CODEX_BIN=codex                # path to codex CLI
CLAUDE_SESSION_FILE=/tmp/futon-session-id
CODEX_SESSION_FILE=/tmp/futon-codex-session-id
FUTON3C_RELAY_INVOKE_TIMEOUT_MS=45000  # IRC dispatch timeout
```
