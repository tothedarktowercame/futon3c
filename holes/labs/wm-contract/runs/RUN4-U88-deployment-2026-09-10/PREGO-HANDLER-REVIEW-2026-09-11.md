# U88 production preparation and handler installation review — 2026-09-11

Production `run4-boot/materialize true` and `run4-trusted-entry/prepare` passed against the activated canonical mission and frozen packet. Preparation returned `{:prepared? true :error nil}`. The credential stayed inside server construction; no value was reported.

Installation is BLOCKED, not complete. Passive live inspection returned:

```clojure
{:installed-target? true :rebuild-source? false :captured-config? false}
```

The production bootstrap composes an HTTP/WebSocket `app` closure and passes that closure to `http/start-server!` (dev/futon3c/dev/bootstrap.clj). The closure does not carry `make-handler` rebuild/config metadata. Thus `rebuild-handler!` cannot recover the original complete configuration. Replacing this closure with a plain HTTP handler would lose WebSocket routing and is invalid. The failed installation attempt left the existing installed handler intact.

The existing reconfiguration tests pass independently: 3 tests / 13 assertions / 0 failures / 0 errors. They install `make-handler` directly and do not exercise the actual bootstrap HTTP/WebSocket composition. Required repair: preserve the complete composition and captured configuration through the real bootstrap boundary, with regression tests for HTTP, WebSocket routing, reconfiguration failure atomicity, and subsequent rebuilds. Recovery of the currently installed composition must be demonstrated without guessed configuration or a restart.

All six `/home/joe/run4/U88` roots contain no files after preparation. No click, series transition, reservation, acceptance, or restart was performed. Canonical namespace reloads were performed under Joe's authorization. `/health` and the exact Codex-17 Agency endpoint both returned HTTP 200 after these checks.

Mission/pin activation basis: futon2 b5e0df41 and futon3c a0e49926. Reviewed handler API: 23dd048d plus 79fa259e. This note records a live installation gap; it does not retract the disposable U88 roundtrip evidence or claim launch readiness.
