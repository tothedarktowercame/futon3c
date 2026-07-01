# Excursion: E-agency-ws-cutover — finish the `/agency/ws` observer cutover so server→Emacs broadcasts (incl. `park-ready`) land live

**Date:** 2026-06-30
**Status:** IDENTIFY (diagnosed live; not built). Split out of [[E-repl-continuations]] as the
tracked WS follow-on — the park-continuation ships on **polling**, which works; WS is the latent
instant-delivery fast-path.
**Repo:** futon3c — WS transport: `transport/ws.clj` (`make-ws-handler`, the ready-handshake +
`ws-invoke/register!`), `transport/ws/invoke.clj` (`!agents` broadcast registry, `broadcast-frame!`),
`runtime/agents.clj` (`make-ws-handler` wrapper), `emacs/futon-agency-ws.el` (the shared observer
connector). Provenance: M-agency-hardening "Solve Agents Flickers" (2026-06-10) — the connector was
built as the single ordered reader but the server-side cutover was never completed.

## HEAD (one line)
`/agency/ws` upgrades to a WebSocket (server returns 101) but **not via the agents `make-ws-handler`**,
so the Emacs observer never runs the ready-handshake, never registers in the broadcast set
(`ws-invoke/!agents`), and reconnect-loops — meaning `ws-invoke/broadcast-frame!` (agents-status,
and E-repl-continuations' `park-ready`) reaches **zero** clients. Finish the cutover so the observer
registers and stays connected.

## Diagnosis (live, 2026-06-30)
- **Server upgrades the path:** a raw WS handshake to `ws://localhost:7070/agency/ws?agent-id=probe`
  returns `HTTP/1.1 101 Switching Protocols`.
- **Connector is reconnect-looping:** `futon-agency-ws--ws` is non-nil, `reconnect-timer` active, but
  `(websocket-openp …)` = nil and `--frame-count` is **static** over 3s (1647). It has received frames
  in the past (so the pipe has worked at some point) but is not currently connected.
- **Not registered:** server-side `futon3c.transport.ws.invoke/connected-agent-ids` = `[]`. The
  observer is NOT in `!agents`, so `broadcast-frame!` (which iterates `!agents`) reaches nobody.
- **Not in source:** the string `/agency/ws` appears **nowhere in `src/`** — nothing routes it to the
  agents `make-ws-handler` (which is the handler that does the ready-handshake + `ws-invoke/register!`
  at `ws.clj:220`). The 101 comes from some other upgrade path that neither registers nor handshakes,
  so the connection drops → reconnect loop.

## The fix (DERIVE sketch — not built)
Route `/agency/ws` WebSocket upgrades to the **agents `make-ws-handler`** (`runtime/agents.clj` →
`transport/ws.clj/make-ws-handler`), so a connecting observer:
1. runs the ready-handshake (gets `ready_ack` → connector sets `--ready`, stops reconnecting);
2. calls `ws-invoke/register!` (joins `!agents`);
3. thereby becomes a target of `broadcast-frame!`.

Then E-repl-continuations' already-wired `park-ready` broadcast (in `parked-resume!`, buffer-surface
branch) reaches the `claude-repl-park` subscriber **instantly**, and the 3s poll becomes a fallback.

**Care points:**
- The WS upgrade routing is delicate; the `make-handler` cond is boot-captured (like the `/park`
  route was — new HTTP routes went in `extra-routes`), so the WS mount must land where the live
  server actually dispatches upgrades (confirm whether it flows through the invoke handler's
  `hk/as-channel` at `http.clj:2982` or a separate mount).
- The observer connects as a synthetic id (`emacs-hud`), never a real agent (I-1) — registration must
  preserve that (it's an observer, not an invocable agent), or use a broadcast set that includes
  observers without making them invoke targets.
- Verify `broadcast-frame!` is in fact the channel the HUD/agents-status frames ride (it is, per
  `registry.clj:197` / `multi.clj:203`) so `park-ready` on the same channel is delivered.

## VERIFY (acceptance)
- Connector: `(websocket-openp …)` stays t, `--ready` t, `--frame-count` increments live.
- Server: `connected-agent-ids` (or an observer set) includes the emacs observer.
- End-to-end: with the poll interval raised high, a `park-ready` broadcast still resumes the buffer
  (proving the WS path, not polling, delivered it).

## Cross-links
- [[E-repl-continuations]] — the consumer; `park-ready` is broadcast but currently undelivered, so the
  buffer runs on polling. Doc: `README-park.md`.
- **M-agency-hardening** — owns the connector + the "Solve Agents Flickers" cutover this completes.
