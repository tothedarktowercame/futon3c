# Persistent campaign babysitter unit

`apm-campaign-babysit@.service` is shipped inert: it is not installed in the
user systemd directory, enabled, or started by this repository. The instance
name supplies the suffix of the coordinator id. For example,
`apm-campaign-babysit@jit-all-open-v2.service` watches
`jit-queue:jit-all-open-v2`.

## Bindings copied from the live transient unit

The following values were read from
`apm-campaign-babysit-jit-all-open-v2.service` on 2026-09-05:

```text
APM_BABYSIT_FROM_ID=apm-watcher-codex
APM_BABYSIT_TO_ID=codex-17
APM_BABYSIT_PARK_DECISION_TO_ID=codex-17
APM_BABYSIT_BELL_COOLDOWN_S=1200
APM_BABYSIT_COORDINATOR_ID=jit-queue:jit-all-open-v2
```

The template expresses the final binding as
`APM_BABYSIT_COORDINATOR_ID=jit-queue:%i`. The live transient unit does not
set `APM_BABYSIT_OPERATOR_SAY_URL`, `APM_BABYSIT_POLL_S`,
`APM_BABYSIT_DISCOVERY_LOG_S`, `APM_BABYSIT_COORD_STALE_S`,
`APM_BABYSIT_ESCALATION_CEILING_S`, `APM_BABYSIT_PRIMARY_ESCALATIONS`,
`APM_BABYSIT_STATE_HEARTBEAT_S`, or `APM_BABYSIT_BELLS_PAUSED`; the script's
declared values therefore remain in effect. In particular, the declared
operator endpoint is `http://127.0.0.1:8081/say`. This absence is recorded
instead of inventing explicit service-level values that are not on the live
unit.

For a future campaign whose recipient or timing differs, add an operator-
reviewed systemd drop-in containing the required `Environment=` bindings.
Every `APM_BABYSIT_*` binding remains an input to the watcher; the template
does not create a second configuration vocabulary.

## Install, then cut over without overlap

Install the file and load its definition. These commands do not start it:

```bash
cd /home/joe/code/futon3c
install -Dm0644 scripts/systemd/apm-campaign-babysit@.service \
  ~/.config/systemd/user/apm-campaign-babysit@.service
systemctl --user daemon-reload
systemctl --user enable apm-campaign-babysit@jit-all-open-v2.service
```

The transient and persistent instances must never run together: they watch
the same coordinator and would duplicate bells and operator speech. During
the coordinated cutover, stop and verify the old instance before starting the
new one:

```bash
systemctl --user stop apm-campaign-babysit-jit-all-open-v2.service
test "$(systemctl --user is-active apm-campaign-babysit-jit-all-open-v2.service)" = inactive
systemctl --user start apm-campaign-babysit@jit-all-open-v2.service
systemctl --user is-active apm-campaign-babysit@jit-all-open-v2.service
```

Do not replace this sequence with `enable --now` while the transient unit is
active. `Restart=on-failure`, `RestartSec=30s`, and an unlimited start window
make an unexpected watcher exit restart instead of becoming a silent final
state. The Python watcher remains responsible only for observation and
notification; the unit grants no coordinator resume, enable, or repair
operation.
