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

## Notes

- `futon3c` currently has no local `Makefile`/`make dev` entrypoint.
- This manifest treats `scripts/dual_agent_ws_live_gate.clj` as the canonical
  practical gate until a repo-native dev launcher is added.
