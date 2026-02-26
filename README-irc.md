# IRC Architecture

## Current Setup

The IRC server runs inside the futon3c JVM alongside the WS transport
and agent layer. This means restarting futon3c drops IRC connections.

### Independent Agent Restart (Drawbridge)

The agent layer (WS transport, Claude, Codex, dispatch relays) can be
restarted without touching the IRC server, via Drawbridge REPL:

```clojure
(require '[futon3c.dev :as dev])

(dev/restart-agents!)   ;; restart WS + agents; IRC stays up
(dev/stop-agents!)      ;; stop agents only
(dev/start-agents!)     ;; start agents only
(dev/status)            ;; check what's running
```

This works because:
- IRC server (`start-irc!`) and WS transport (`start-futon3c!`) are
  separate TCP/HTTP servers on different ports
- The relay bridge wires IRC<->WS and survives agent restarts
- `install-irc-auto-join!` re-wires after restart

### Ports

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| IRC server | 6667 | TCP/IRC | Human clients (irssi, ERC, etc.) |
| WS transport | 7070 | HTTP+WS | Agent connections |
| nginx WSS | 5051 | WSS | Remote agent connections (TLS termination) |
| Drawbridge | 6768 | HTTP | REPL access for lifecycle management |

### Limitation

The IRC server still runs inside the JVM. If the JVM crashes or is
restarted (`systemctl restart futon3c`), IRC connections drop too.

## Planned: Standalone IRC Server

The goal is a lightweight standalone IRC daemon running as its own
systemd service, completely independent of futon3c. The futon3c relay
bridge would then connect as an IRC *client* rather than hosting the
server.

Benefits:
- IRC stays up across futon3c restarts, JVM crashes, deployments
- Memory-light (no JVM overhead for basic chat)
- Rob and Joe can chat uninterrupted regardless of agent state
