# RUN4 configuration installed without restart — 2026-09-11

Supersedes the installation gap in PREGO-HANDLER-REVIEW-2026-09-11.md.

Implementation: 63e706f2 preserves combined HTTP/WebSocket handling and forwards RUN4 through the runtime adapter. Migration: 7a2a8e4e qualifies the historical bootstrap closure, recovers its retained HTTP rebuild function, and retains the original HTTP and WebSocket handlers. No runtime configuration is guessed. Construction and validation precede the swap under the existing installation lock; mismatched layouts and missing reconstruction evidence refuse.

Independent execution in this session:

- Migration tests: 2 tests, 12 assertions, zero failures/errors. Tests compile the old bootstrap closure in its original namespace, verify retained WebSocket behavior, updated HTTP behavior, and refusal without replacement.
- clj-kondo: zero errors/warnings. Parentheses and scoped diff checks pass.
- Canonical live migration returned `{:ok true :status :bootstrap-composition-migrated :http-handler-retained? true :websocket-handler-retained? true}`.
- Real boot materialization and trusted preparation passed for series `run4-inner-loop-u88-2026-09-10`, trial `M-u88-contextual-preferences`, using the private credential inside server construction.
- Installation returned `{:ok true :status :handler-reconfigured :run4-configured? true}`.
- Passive installed-state read: RUN4 enabled, series enabled, composed builder present. Startup environment switch remains `false`: this is an in-process installation and does not change next-boot configuration.
- Recording environment is `1`; mission-C environment remains unset. Materialization and trusted preparation apply the actual effective-consumer attestation.
- `/health` and `/api/alpha/agents/codex-17` returned HTTP 200 after installation.
- Authenticated read-only report returned `{:decision :incomplete-durable-evidence :accepted? false :acceptance-authority :operator-reserved}`. This is expected before execution and is not run acceptance.
- All six U88 recording/controller roots remain empty after preparation, installation, and reporting.

No click, series step, worker dispatch, reservation, acceptance, restart, or startup profile change occurred. No credentials were logged. Existing WebSocket closures were retained; the isolated tests exercise their routing, while the live checks establish HTTP/Agency availability, not a new worker execution.

The installed service is ready for the separately authorized trial-step action; no automatic series scheduler was added. Run outcomes and compliance remain unestablished until actual execution and evidence review.
