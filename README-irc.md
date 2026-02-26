# IRC Architecture

## Current Setup: ngircd + Bridge

IRC runs as two independent services, completely decoupled from the
futon3c JVM:

1. **ngircd** — lightweight C IRC daemon (systemd service, ~500KB)
2. **ngircd-bridge** — Python script connecting bots to ngircd as IRC
   clients, relaying @mentions to the futon3c invoke API

Rob and Joe can chat on IRC uninterrupted, regardless of whether
futon3c, agents, or the bridge are running.

### Architecture

```
  Human clients              ngircd (port 6667/6697)        futon3c (port 7070)
  ┌─────────┐                ┌───────────────────┐          ┌──────────────────┐
  │ irssi   │───TCP/TLS─────▶│                   │          │                  │
  │ ERC     │                │   #futon channel   │          │  invoke API      │
  │ weechat │                │                   │          │  /api/alpha/     │
  └─────────┘                │   joe, rob,       │          │  invoke          │
                             │   claude, codex   │          │                  │
                             └─────────▲─────────┘          └────────▲─────────┘
                                       │                             │
                             ┌─────────┴─────────────────────────────┴──┐
                             │         ngircd-bridge.py                  │
                             │  connects as claude + codex nicks        │
                             │  @mention → POST /api/alpha/invoke       │
                             │  response → PRIVMSG #futon               │
                             └──────────────────────────────────────────┘
```

### Ports

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| ngircd | 6667 | IRC | Human clients (plaintext, localhost or LAN) |
| ngircd | 6697 | IRC+TLS | Human clients (encrypted, remote) |
| ngircd-bridge | — | IRC client | Bot presence in #futon |
| WS transport | 7070 | HTTP+WS | Agent connections + invoke API |
| nginx WSS | 5051 | WSS | Remote agent connections (TLS termination) |
| Drawbridge | 6768 | HTTP | REPL access for lifecycle management |

### Services

```bash
# IRC server (C daemon, independent of everything)
systemctl status ngircd
systemctl restart ngircd        # humans stay connected after reconnect

# Agent bridge (Python, connects to both ngircd and futon3c)
systemctl --user status ngircd-bridge
systemctl --user restart ngircd-bridge   # bots rejoin #futon

# futon3c (JVM, agents + WS transport)
systemctl status futon3c
# restart futon3c without affecting IRC:
#   humans: unaffected (ngircd is separate)
#   bots: ngircd-bridge reconnects automatically when invoke API returns
```

### Connection Details

- **Server password**: set in `/etc/ngircd/ngircd.conf`
- **TLS**: via Let's Encrypt certs on port 6697
- **Channel**: `#futon` (persistent, modes `+Pn`)
- **Bot nicks**: `claude`, `codex` (via ngircd-bridge)

### Agent Restart (Drawbridge REPL)

The agent layer can still be restarted independently via Drawbridge:

```clojure
(require '[futon3c.dev :as dev])

(dev/restart-agents!)   ;; restart WS + agents
(dev/stop-agents!)      ;; stop agents only
(dev/start-agents!)     ;; start agents only
(dev/status)            ;; check what's running
```

When agents restart, the bridge's invoke calls may fail briefly, then
succeed once the agents are back. IRC connections are unaffected.

## Configuration

### ngircd

Config: `/etc/ngircd/ngircd.conf`

### ngircd-bridge

Environment variables (set in systemd unit or shell):

| Variable | Default | Purpose |
|----------|---------|---------|
| `IRC_HOST` | 127.0.0.1 | ngircd host |
| `IRC_PORT` | 6667 | ngircd port |
| `IRC_PASSWORD` | MonsterMountain | Server password |
| `IRC_CHANNEL` | #futon | Channel to join |
| `INVOKE_BASE` | http://127.0.0.1:7070 | futon3c HTTP base |
| `BRIDGE_BOTS` | claude,codex | Comma-separated bot nicks |

### futon3c

Start with `FUTON3C_IRC_PORT=0` to disable the built-in IRC server
(since ngircd handles IRC now):

```bash
FUTON3C_IRC_PORT=0 FUTON3C_ROLE=linode make dev
```

## Bang Commands

The bridge provides bang commands in `#futon` for mission control,
mission focus, and task tracking. Any channel member can use them.

### `!help`

Lists all available commands.

### `!mc` — Mission Control

| Command | Description |
|---------|-------------|
| `!mc status` | Session count and active count from mission-control peripheral |
| `!mc review` | Run a portfolio review — summarizes all missions, emits a review snapshot as evidence, lists actionable items |
| `!mc missions` | List missions grouped by status (in-progress, ready, blocked, complete) |
| `!mc sessions` | List active mission-control peripheral sessions |
| `!mc diff` | Compare the two most recent portfolio review snapshots — shows added/removed/changed missions (味 sigil) |

`!mc review` must be run at least twice before `!mc diff` has data to compare.

### `!mission` — Mission Focus

| Command | Description |
|---------|-------------|
| `!mission focus <id>` | Set the focused mission for subsequent @mentions. Evidence entries from focused invocations are tagged with `{:ref/type :mission :ref/id <id>}` |
| `!mission show` | Show the currently focused mission (if any) |
| `!mission clear` | Clear the mission focus |

Mission focus is per-bridge-session (resets on bridge restart).

### `!todo` — Task Tracking

| Command | Description |
|---------|-------------|
| `!todo add <text>` | Add a todo item (attributed to the sender) |
| `!todo list` | List pending todos |
| `!todo done <id>` | Mark a todo as complete |

## How @mentions Work

1. Human types `@claude what time is it?` in #futon
2. ngircd delivers PRIVMSG to all channel members including the bridge
3. Bridge detects the @claude mention, strips the prefix
4. Bridge POSTs to `http://localhost:7070/api/alpha/invoke` with
   `agent-id: claude-1` and the prompt (with surface contract metadata)
5. futon3c invokes the registered Claude agent (via `claude -p` CLI)
6. Bridge receives the response and posts it back as `claude` in #futon
