# Excursion: E-agency-ws-cutover — let the Emacs observer clear the WS ready-handshake so server→Emacs broadcasts (incl. `park-ready`) land live

**Date:** 2026-06-30 (re-diagnosed 2026-07-01)
**Status:** VERIFIED LIVE 2026-07-01 (INSTANTIATE). Root cause re-diagnosed live (original IDENTIFY
premise was wrong — see below); fix built, gated, and confirmed end-to-end on a fresh JVM boot with
the real `emacs-hud` connector registered as a broadcast-only observer. Split out of
[[E-repl-continuations]] as the tracked WS follow-on — the park-continuation ships on **polling**,
which works; WS is now the live instant-delivery fast-path.
**Repo:** futon3c — WS transport: `transport/ws.clj` (`make-ws-handler`, the ready-handshake +
`ws-invoke/register!`, the `:ready` branch that gates on `presence/verify` at `ws.clj:207`),
`transport/protocol.clj` (`parse-ws-message` :ready branch), `transport/ws/invoke.clj` (`!agents`
broadcast registry, `broadcast-frame!`), `social/presence.clj` (`verify`, the gate),
`dev/futon3c/dev/bootstrap.clj` (the live `app` that dispatches WS upgrades),
`emacs/futon-agency-ws.el` (the shared observer connector). Provenance: M-agency-hardening
"Solve Agents Flickers" (2026-06-10) — the connector was built as the single ordered reader but the
server-side handshake never admitted a non-agent observer.

## HEAD (one line)
`/agency/ws` upgrades to a WebSocket **via the agents `make-ws-handler`** (correct handler), but the
observer's `ready` frame is **rejected by the `presence/verify` gate** (`:agent-not-found`) because
the observer's synthetic id (`emacs-hud`) is intentionally not a registered agent — so the server
sends an error frame + closes, the connector reconnect-loops, never registers in `!agents`, and
`broadcast-frame!` (agents-status + E-repl-continuations' `park-ready`) reaches **zero** clients.
Admit the observer through the handshake as a broadcast-only participant (not an invocable agent).

## Diagnosis (live, re-verified 2026-07-01) — CORRECTS the original
The original IDENTIFY claimed "`/agency/ws` is not routed to the agents `make-ws-handler`, the 101
comes from some other upgrade path." **That premise is false.** Re-verified against the running JVM:

- **The path IS routed to the agents handler.** The routing is not path-based and not in `src/`; it
  lives in `dev/futon3c/dev/bootstrap.clj:106-109`, where the live `app` dispatches on
  `(:websocket? request)`: **every** WS upgrade — regardless of path — goes to `make-ws-handler`
  (which does the ready-handshake + `ws-invoke/register!` at `ws.clj:220`). That is why `/agency/ws`
  appears "nowhere in `src/`": there is no per-path WS route; the 101 comes from the *correct*
  handler.
- **The handshake is rejected by S-presence.** On the `ready` frame, `ws.clj:207` runs
  `presence/verify`, which requires the agent to exist in the registry (`presence.clj:68` →
  `:agent-not-found`). `emacs-hud` is a *deliberately synthetic observer id, not a registered agent*
  (per the connector's own docstring). So verify fails → `ws.clj:210-212` sends an error frame and
  `close-fn`s the channel → connector reconnect-loops → never registers → `connected-agent-ids` = `[]`
  → `broadcast-frame!` reaches nobody.
- **Live proof** — raw handshake to the running server as `emacs-hud`:
  ```
  HANDSHAKE: HTTP/1.1 101 Switching Protocols
  FRAME: {"type":"error","code":"agent-not-found","message":"Agent not found in registry"}
  FRAME: <close frame>
  ```

The original DERIVE ("route `/agency/ws` to `make-ws-handler`") would have been a no-op — it is
already routed there. The real fix is the one the original **care-point #2 anticipated but mis-filed
under "registration"**: observers must clear the handshake and join the broadcast set *without*
becoming invocable agents.

## The fix (DERIVE — being built 2026-07-01)
Admit an explicitly-declared **observer** through the `:ready` handshake as a broadcast-only
participant. Three small edits:
1. **`emacs/futon-agency-ws.el`** — the ready frame declares `(observer . t)` (the connector already
   documents `emacs-hud` as "receives broadcasts and answers nothing").
2. **`transport/protocol.clj`** `parse-ws-message` :ready branch — thread the `observer` flag through
   as `:observer?`.
3. **`transport/ws.clj`** `:ready` branch — when `:observer?` is set, **bypass `presence/verify`**
   (observers are intentionally not registered agents), register into `!agents` with an
   `:observer? true` marker, and send `ready_ack`.
4. **`transport/ws/invoke.clj`** — `register!` carries the observer marker; `invoke!`/`available?`
   guard against ever targeting an observer, and `connected-agent-ids` stays "invocable WS agents"
   (observers surface via a new `connected-observer-ids`) — so `broadcast-frame!` reaches observers
   while invoke never does (honors I-1).

Then E-repl-continuations' already-wired `park-ready` broadcast (in `parked-resume!`, buffer-surface
branch) reaches the `claude-repl-park` subscriber **instantly**, and the 3s poll becomes a fallback.

**Care points:**
- The observer connects as a synthetic id (`emacs-hud`), never a real agent (I-1) — the fix keeps it a
  broadcast-only participant, never an invoke target (`invoke!`/`available?` guarded; excluded from
  `connected-agent-ids`).
- Reload discipline: Clojure via Drawbridge (never restart the JVM, I-0); the `.el` via
  `emacsclient --eval (load …)`.
- Verify `broadcast-frame!` is in fact the channel the HUD/agents-status frames ride (it is, per
  `registry.clj:197` / `multi.clj:203`) so `park-ready` on the same channel is delivered.

## Activation status (2026-07-01) — code landed + tested + live-verified; prod re-mount pending
- **Edits landed** (4 files): `protocol.clj` (:observer? parse), `ws.clj` (:ready observer bypass),
  `ws/invoke.clj` (observer register/guard/observer-set), `emacs/futon-agency-ws.el` (declare
  observer). Plus tests: `protocol_test`, `ws_test` (`ws-ready-handshake-observer-bypasses-presence`),
  `ws_invoke_test` (`ws-observer-broadcast-only`).
- **Gates:** clj-kondo 0 errors; `futon4/dev/check-parens.el` OK; `clojure -X:test` for
  protocol/ws/ws-invoke/presence = **74 tests, 209 assertions, 0 failures**.
- **Live-verified in the running JVM** (via Drawbridge reload of the 3 clj nses, then driving the
  reloaded `make-ws-callbacks` observer path directly — no port rebind): observer ready frame →
  `{:type "ready_ack"}`, connection stays open, `:connected? true :observer? true`, in
  `connected-observer-ids`, NOT in `connected-agent-ids`, `available? = false`. And live
  `parse-ws-message` threads `:observer?` (true/false).
- **Re-mount (reload-safety trap) — RESOLVED via JVM restart 2026-07-01.** The live `:7070` `app`
  (`bootstrap.clj:106-109`) captured the WS `handler` closure at boot, and `make-ws-handler`
  (`ws.clj:459-475`) captures the fat inline `on-receive` **by value** — so the observer branch is
  structural inside a frozen closure and a plain Drawbridge reload did NOT make it live. Joe restarted
  the JVM at a pause point, which re-mounted the handler from disk. **Verified live on the fresh boot:**
  - Raw observer handshake to `ws://localhost:7070/agency/ws` → `101` then `{"type":"ready_ack"}`,
    connection stays open (was `agent-not-found` + close pre-restart).
  - Server after enabling the connector in the `server` Emacs (`M-x futon-agency-hud-enable`, now
    auto-run by `futon-config.el`): `connected-observer-ids` = `["emacs-hud"]`,
    `connected-agent-ids` = `[]`, `available? "emacs-hud"` = false, `invoke! "emacs-hud"` =
    `{:error :ws-observer-not-invocable}`. Connector status `open=t ready=t`, frame-count incrementing.
- **Default startup wired:** `futon0/contrib/futon-config.el` now `(require 'futon-agency-ws)` +
  `(with-demoted-errors … (futon-agency-hud-enable))`, and adds the connector to the hot-reload list —
  so the observer socket comes up automatically on every Emacs boot.
- **Follow-up (deferred):** harden `make-ws-handler` to dispatch `on-open`/`on-receive` through
  `#'var` (README-drawbridge "add `#'var` if you introduce [a value-captured fn]") so future
  WS-behavior edits are reload-safe. Still needs one re-mount to install, but prevents recurrence.

## VERIFY (acceptance)
- Connector: `(websocket-openp …)` stays t, `--ready` t, `--frame-count` increments live.
- Server: `connected-agent-ids` (or an observer set) includes the emacs observer.
- End-to-end: with the poll interval raised high, a `park-ready` broadcast still resumes the buffer
  (proving the WS path, not polling, delivered it).

## Cross-links
- [[E-repl-continuations]] — the consumer; `park-ready` is broadcast but currently undelivered, so the
  buffer runs on polling. Doc: `README-park.md`.
- **M-agency-hardening** — owns the connector + the "Solve Agents Flickers" cutover this completes.
