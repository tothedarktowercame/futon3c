# Setting up #zabuton — Rob's agent channel

## How it works

Same pattern as Joe's codex-1: strictly outbound WebSocket from Rob's
machine to the Linode. No ports to open, no tunnels.

```
Rob's machine                         Linode (London)
┌─────────────┐                      ┌──────────────────┐
│ futon3c     │──── WS outbound ────▶│ futon3c :7070    │
│  zclaude-1  │     /agency/ws       │  registry:       │
│  zcodex-1   │◀── invoke frames ────│   zclaude-1 (ws) │
│  (CLI runs  │──── result frames ──▶│   zcodex-1  (ws) │
│   locally)  │                      │                  │
└─────────────┘                      │ ngircd-bridge    │
                                     │  @zabuton:       │
                                     │   zclaude ──▶ invoke zclaude-1
                                     │   zcodex  ──▶ invoke zcodex-1
                                     └──────────────────┘
```

The bridge on the Linode calls `localhost:7070/api/alpha/invoke` — same
futon3c instance. Rob's agents are registered there via WS, so the invoke
routes over the WebSocket to Rob's machine, which runs the CLI and sends
back the result.

## What Rob needs to do

### 1. Get futon3c running

```bash
git clone <repo-url> ~/code/futon3c
cd ~/code/futon3c
```

Startup env (create `scripts/dev-rob-env` or just export):

```bash
export FUTON3C_ROLE=laptop
export FUTON3C_PORT=7070
export FUTON3C_IRC_PORT=0

# Both agents run locally, connect to Linode via WS bridge
export FUTON3C_REGISTER_CLAUDE=true
export FUTON3C_REGISTER_CODEX=true
export FUTON3C_CODEX_WS_BRIDGE=true

# WS bridge target: Joe's Linode
export FUTON3C_LINODE_URL=http://172.236.28.208:7070

# Agent IDs — must match what the bridge expects
# The bridge appends -1 to nick names, so zclaude → zclaude-1
# Rob needs to register with these IDs:
#   zclaude-1, zcodex-1

make dev
```

**Important**: Rob's agents need to register as `zclaude-1` and `zcodex-1`
(not `claude-1` / `codex-1`) so they don't collide with Joe's agents on the
shared registry. This requires either:

- Setting env vars that control agent-id (e.g. `CLAUDE_AGENT_ID=zclaude-1`)
- Or a small dev.clj tweak to read agent-id from env

Current dev.clj hardcodes `"claude-1"` and `"codex-1"`. Rob will need to
change these to `"zclaude-1"` and `"zcodex-1"` in his local `start-agents!`
call, or we add env-var overrides (see "Joe's optional prep" below).

### 2. Connect to IRC

```
irssi -c 172.236.28.208 -p 6667 -w MonsterMountain -n rob
/join #zabuton
```

`@zclaude` and `@zcodex` mentions in #zabuton trigger invokes.

## What Joe needs to do

### 1. Create the bridge env file

```bash
cat > ~/.config/futon3c/bridge-zabuton.env << 'EOF'
INVOKE_BASE=http://127.0.0.1:7070
BRIDGE_BOTS=zclaude,zcodex
EOF
```

Note: `INVOKE_BASE` is localhost - the Linode's own futon3c. Rob's agents
register there via WS bridge, so the invoke routes over the WS connection.

Optional explicit bare-command ownership:

Bare `!` command ownership can be made room-scoped and keyed by internal agent
id, not IRC nick:

- use `IRC_COMMAND_OWNER_AGENT_MAP=#channel:agent-id,...`
- when this map is set on a bridge, it is authoritative for that bridge
- unmapped rooms get no bare `!` response from that bridge
- for the Zabuton/Linode side, an explicit allowlist example is:
  - `IRC_COMMAND_OWNER_AGENT_MAP=#zabuton:claude-1`
- if this bridge also joins `#math` via `IRC_CHANNELS=#math`, leaving `#math`
  unmapped means bare `!` commands there remain disabled on this bridge until a
  deliberate owner is chosen

### 2. Start the bridge

```bash
systemctl --user enable --now ngircd-bridge@zabuton
```

The health check in `dev-linode-env` will automatically monitor it.

### 3. Optional: add agent-id env vars to dev.clj

To let Rob (or anyone) set custom agent-ids without editing code:

```clojure
;; In start-agents!, where claude-1 is registered:
(let [claude-id (env "CLAUDE_AGENT_ID" "claude-1") ...]
  (make-claude-invoke-fn {:agent-id claude-id ...}))
```

This isn't strictly needed — Rob can just edit his local copy. But it's
cleaner if we ever add a third person.

## Verification

1. Rob starts futon3c — should see WS bridge connecting to Linode
2. On Linode: `curl -s localhost:7070/api/alpha/agents` shows `zclaude-1`
   and `zcodex-1` with `invoke-route: ws`
3. Rob says `@zclaude hello` in #zabuton on IRC
4. Bridge picks up mention → invokes zclaude-1 on Linode → WS to Rob →
   Claude CLI runs → result back → bridge posts response as `<zclaude>`

## Notes

- The `z` prefix avoids nick collisions on the shared ngircd. Rob could use
  any names (`rob-claude`, etc.) — just match `BRIDGE_BOTS` and agent-ids.
- If Rob's machine sleeps, the WS disconnects. The Linode shows the agents
  as unavailable. IRC mentions will get an error response from the bridge.
  When Rob reconnects, the WS bridge auto-reconnects.
- ngircd password: `MonsterMountain`
- Rob can also join #futon to chat with Joe's agents (and vice versa —
  Joe can `/join #zabuton` to see Rob's agents working).
