# U88 provisioning — 2026-09-11

Joe authorized working through the remaining setup in this session. Created
six empty durable roots at /home/joe/run4/U88/{controller,bindings,projections,
run-records,recordings,visibility}, each mode 0700. Created a private 256-bit
bearer in /home/joe/.config/futon3c/run4/u88.bearer (mode 0600; private directory
0700), using exclusive creation and fsync. No credential value was printed,
committed or installed in a server. No attempt/store evidence was fabricated.

Passive Drawbridge inspection, with no require/reload/binding mutation:

| Consumer | Current environment | Loaded | Effective |
|---|---|---|---|
| FUTON_WM_FPI_DARK | absent | no | unknown |
| FUTON_WM_BETA_DARK | absent | no | unknown |
| FUTON_WM_TRACE_POLICY_DETAILS | absent | yes | false |

Thus the serving JVM cannot satisfy the pinned required/current/effective
comparison. No dynamic-var rebinding or runtime environment workaround is
permitted. A separately approved restart from a separate shell/session with
the required startup environment is needed before enabled production serving.
It must preserve/recover Agency; this seat must not restart its own transport.

Read-only preflight now reports roots present, sources current, OPEN activated mission,
disabled template and launch false. Its credential result is template metadata
(unprovisioned), not a filesystem credential probe; the private file is created
but not installed. Its consumer unknown result is not the live inspection above.

Bootstrap start-futon3c! currently passes patterns/IRC/evidence options to the
HTTP handler, not the new run4 service configuration. Installing that actual
boot/configuration path is remaining implementation work. Keep it disabled
until explicit activation. Mission activation and refreshed pins are now complete; RUN4 remains disabled.
No live namespace load, restart, handler change, launch or acceptance occurred.
