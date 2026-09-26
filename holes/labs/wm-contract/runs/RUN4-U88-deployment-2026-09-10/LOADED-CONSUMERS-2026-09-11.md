# Loaded RUN4 consumer attestation — 2026-09-11

Joe explicitly authorized loading the required namespaces and completing
pre-go-live work. From the canonical serving checkout, Codex17 invoked
require for futon2.report.war-machine, futon2.aif.trace, and
futon3c.wm.run4-effective-environment through Drawbridge. No reload, dynamic
rebinding, environment modification or restart was used.

The actual production attest function returned all three rows with required
"1", observed "1", effective true: FPI_DARK, BETA_DARK and TRACE_POLICY_DETAILS.
Hierarchy remains declared single-level/RUN4. Recording remains explicitly
not-attested-by-this-component. FUTON3C_RUN4_U88_ENABLED remains "false".

This closes the unloaded-consumer gap in 1e882bb3. It does not claim a trial,
recording deposit, installed RUN4 HTTP configuration or operator acceptance.
Mission activation and exact source/task/series/template re-pin were completed
by Codex12 under Joe's current setup authorization. Independent review remains
required before enablement; RUN4 startup remains false.
