# Alleycat Race Manifest (futon3c)

Date: 2026-02-15
Host target (Claude/Linode): `172-236-28-208.ip.linodeusercontent.com`

## Scope

This file is the practical runbook for a real dual-agent alleycat gate in
`futon3c`, focused on:

1. Codex WS readiness + action routing.
2. Claude WS readiness from Linode.
3. Claude action routing through the expected peripheral.
4. Shared protocol behavior over `/agency/ws`.

This is transport/runtime validation with explicit pass criteria, not a unit-test
smoke.

## Best-Guess Network Wiring

Current best guess in this rebuild:

- Agency HTTP/WS port: `7070`
- WS endpoint: `/agency/ws`
- Connected roster endpoint: `/agency/connected`

Rationale:

- Historical `futon3` Agency docs and scripts consistently use `:7070` and `/agency/ws`.
- New `futon3c` live-gate runner defaults to the same.

Fallback if `7070` is in use:

- Use `47070` for local bring-up and update `FUTON3C_PUBLIC_WS_BASE`.

## Required Files

- Live gate runner:
  - `scripts/dual_agent_ws_live_gate.clj`
- Linode env template:
  - `scripts/dual_agent_ws_live_gate.env.example`
- Linode env prefill:
  - `scripts/dual_agent_ws_live_gate.env.linode`
- Codex IRC relay:
  - `scripts/irc_codex_relay.clj`
- Codex Emacs REPL:
  - `emacs/codex-repl.el`

## Preflight

From `futon3c/`:

```bash
clj-kondo --lint scripts/dual_agent_ws_live_gate.clj
```

Check port availability on the machine running futon3c:

```bash
ss -ltn | rg ':7070\\b'
```

If occupied, pick fallback `47070` and adjust env before launch.

## Launch (Gate Host)

1. Load environment:

```bash
set -a
source scripts/dual_agent_ws_live_gate.env.linode
set +a
```

2. Start live gate:

```bash
clojure -M scripts/dual_agent_ws_live_gate.clj
```

The runner prints:

- local Codex URL
- external Claude URL to connect from Linode
- pass/fail conditions

## Claude Join Protocol (Linode)

Claude should connect over WS as `claude-1` to:

```text
wss://172-236-28-208.ip.linodeusercontent.com:7070/agency/ws?agent-id=claude-1&session-id=sess-alleycat-live
```

Then send two frames:

1. Ready:

```json
{"type":"ready","agent_id":"claude-1","session_id":"sess-alleycat-live"}
```

2. Action (required by gate):

```json
{"type":"message","msg_id":"alleycat-claude-live-1","payload":"investigate flaky test cluster","to":"claude-1"}
```

## Expected Pass Signals

From live-gate output:

1. Codex receipt with:
   - `route=peripheral/run-chain`
   - `peripheral_id=edit`
2. Claude connection observed in connected roster.
3. Claude action receipt observed with:
   - `msg_id=alleycat-claude-live-1`
   - `route=peripheral/run-chain`
   - `peripheral_id=explore`
4. Final line:
   - `PASS: live dual-agent gate complete.`

## Failure Triage

1. Claude never connects:
   - Check DNS/TLS/firewall to Linode host/port.
   - Confirm `FUTON3C_PUBLIC_WS_BASE` matches externally reachable endpoint.
2. Connects but no action receipt:
   - Ensure Claude sent `msg_id=alleycat-claude-live-1` exactly.
   - Ensure action payload is a string (routes as action mode).
3. Wrong route/peripheral:
   - Confirm agent id is `claude-1` and action `to` is `claude-1`.
   - Confirm registry snapshot includes peripheral config.
4. Port bind failure:
   - Switch to `47070` and update both bind port and public WS base.

## Topology B: Agency on Linode, Codex External (Laptop)

Inverted topology for when Codex runs on the laptop and connects to Agency
on Linode. Claude connects locally on Linode.

### Runner

`scripts/dual_agent_ws_live_gate_codex_external.clj`

### Launch (Linode)

```bash
FUTON3C_BIND_HOST=0.0.0.0 \
FUTON3C_PORT=7070 \
FUTON3C_PUBLIC_WS_BASE=ws://172-236-28-208.ip.linodeusercontent.com:7070 \
FUTON3C_SESSION_ID=sess-alleycat-live \
FUTON3C_WAIT_MS=300000 \
clojure -M scripts/dual_agent_ws_live_gate_codex_external.clj
```

Claude passes locally. Gate prints Codex connect URL and waits.

### Codex Join Protocol (Laptop)

Connect as `codex-1` to:

```text
ws://172-236-28-208.ip.linodeusercontent.com:7070/agency/ws?agent-id=codex-1&session-id=sess-alleycat-live
```

Then send two frames:

1. Ready:

```json
{"type":"ready","agent_id":"codex-1","session_id":"sess-alleycat-live"}
```

2. Action:

```json
{"type":"message","msg_id":"alleycat-codex-live-1","payload":"fix failing integration test","to":"codex-1"}
```

### Expected Pass Signals

1. Claude receipt: `route=peripheral/run-chain`, `peripheral_id=explore`
2. Codex connected (observed in roster)
3. Codex receipt: `route=peripheral/run-chain`, `peripheral_id=edit`
4. Final line: `PASS: live dual-agent gate complete (Claude local, Codex external).`

### Gate Pass Record

- PASS on 2026-02-15 (first cross-host dual-agent gate on futon3c).
- Invariant: `I-crosshost-dual-agent` (see `holes/missions/M-futon3c-codex.md`).

## Codex Parity: Emacs + IRC

This path gives practical Codex parity with the Claude transport pivot:

1. `codex-repl.el` for direct Emacs chat.
2. `irc_codex_relay.clj` for IRC participation.
3. Shared session continuity through `/tmp/futon-codex-session-id`.

### Start Codex IRC Relay

From `futon3c/`:

```bash
clj-kondo --lint scripts/irc_codex_relay.clj
clojure -M scripts/irc_codex_relay.clj
```

Optional environment overrides:

- `FUTON3C_IRC_PORT` (default `6667`)
- `FUTON3C_BIND_HOST` (default `0.0.0.0`)
- `FUTON3C_CODEX_BIN` (default `codex`)
- `FUTON3C_CODEX_SANDBOX` (default `workspace-write`)
- `FUTON3C_CODEX_APPROVAL` (default `never`)
- `FUTON3C_CODEX_SESSION_FILE` (default `/tmp/futon-codex-session-id`)

### Start Codex Emacs REPL

In Emacs:

```elisp
(load "/home/joe/code/futon3c/emacs/codex-repl.el")
M-x codex-repl
```

The REPL and IRC relay share `/tmp/futon-codex-session-id`, so context can
pivot across transports.

While the REPL is running you can query the transport modeline explicitly:

- `M-x codex-repl-describe-modeline` — print the current modeline string.
- `M-x codex-repl-copy-modeline` — copy the rendered modeline to the kill ring.

This makes the gate-visible transport state explicit for logs, chat, or
handoffs without relying on the header string alone.

## Notes

- `futon3c` currently has no local `Makefile`/`make dev` entrypoint.
- This manifest treats `scripts/dual_agent_ws_live_gate.clj` (Topology A: Claude
  external) and `scripts/dual_agent_ws_live_gate_codex_external.clj` (Topology B:
  Codex external) as canonical practical gates until a repo-native dev launcher
  is added.
