# Restart handoff — 2026-09-11

Joe is pausing topology and APM loops and will restart the service himself.
The actual launcher on this host is the user unit futon3c-zone.service,
ExecStart /home/joe/code/futon3c/scripts/dev-zone-env. Do not use the repository
fdev --restart advice: this checkout's fdev does not implement that option.

Staged /home/joe/.config/systemd/user/futon3c-zone.service.d/70-run4-preparation.conf
and ran systemctl --user daemon-reload (not restart). Required FPI_DARK,
BETA_DARK, TRACE_POLICY_DETAILS and RECORDING_CONTRACT are all 1 for the NEXT
process. MISSION_C is explicitly unset. RUN4_U88_ENABLED is false. Tickle and
FM conductor autostart are false. This does not claim every external loop's
pause is persisted; Joe owns the topology/APM pauses. Existing queued Agency
jobs may resume on recovery; no queue was cleared.

The currently running MainPID remained 562921 after daemon-reload. Its old
values are unchanged. Six private roots and the private bearer already exist;
no secret is in this drop-in. Boot integration is a839d55c, default disabled.

From Joe's separate terminal, once his pauses are complete:

```sh
systemctl --user restart futon3c-zone.service
systemctl --user is-active futon3c-zone.service
systemctl --user show futon3c-zone.service --property=MainPID
```

Then verify exact Agency seat/session reconnection via
GET http://localhost:7070/api/alpha/agents/codex-17 (not just HTTP health).
Use scripts/proof-eval.sh to passively inspect the three named consumer vars
and their required environment. Unloaded FPI/BETA vars remain unknown until
loaded by the real consumer path; do not claim effective values from env alone.
TRACE should be loaded true if its namespace has loaded under this profile.
Confirm FUTON3C_RUN4_U88_ENABLED=false and FUTON_WM_MISSION_C absent.

This restart prepares the environment and keeps the machine disabled. U88 mission activation, refreshed pins, and the effective-consumer check are complete; explicit enablement still precedes any series request. No restart, launch or acceptance was
performed by Codex17.
