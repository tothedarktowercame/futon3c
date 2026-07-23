# Futon lifecycle boundaries (user systemd)

This bundle moves the one serving futon3c JVM out of the tmux/comint process
tree and into `futon3c-server.service`. The service has its own cgroup in
`futon-services.slice`, `ManagedOOMPreference=avoid`, full accounting, explicit
memory/task limits, and `KillMode=control-group`. Agent harnesses, click JVMs,
and test JVMs go through `run-agent-scope` into `futon-agents.slice`; they are
accounted and bounded independently and remain eligible for pressure killing.

Nothing in this directory enables, starts, stops, or reloads the live server on
installation. Do the migration only in a quiet operator window.

Before migrating, route every `cr`, `cx`, `cz`, click, and test launch through
`run-agent-scope` as shown below. This is a prerequisite: an unwrapped harness
spawned by Agency would otherwise inherit the protected server cgroup instead
of the bounded agent slice.

## Install inert files

```bash
cd /home/joe/code/futon3c
install -Dm644 scripts/systemd/units/futon-services.slice ~/.config/systemd/user/futon-services.slice
install -Dm644 scripts/systemd/units/futon-agents.slice ~/.config/systemd/user/futon-agents.slice
install -Dm644 scripts/systemd/units/futon1b-server.service ~/.config/systemd/user/futon1b-server.service
install -Dm644 scripts/systemd/units/futon1b-vitality.service ~/.config/systemd/user/futon1b-vitality.service
install -Dm644 scripts/systemd/units/futon1b-vitality.timer ~/.config/systemd/user/futon1b-vitality.timer
install -Dm644 scripts/systemd/units/futon3c-server.service ~/.config/systemd/user/futon3c-server.service
systemd-analyze --user verify ~/.config/systemd/user/futon-services.slice \
  ~/.config/systemd/user/futon-agents.slice \
  ~/.config/systemd/user/futon1b-server.service \
  ~/.config/systemd/user/futon1b-vitality.service \
  ~/.config/systemd/user/futon1b-vitality.timer \
  ~/.config/systemd/user/futon3c-server.service
systemctl --user daemon-reload
```

The vitality timer is independent of the serving JVM. It samples cgroup
`memory.current/high/max`, anon/file breakdown, `memory.events`, PSI, the main
`:7073/health` path, and a separate one-worker `:7072/health` acceptor once per
minute into the bounded private log `~/.local/state/futon1b/vitality.jsonl`.
It also counts completed `/api/alpha/evidence` errors since the preceding
sample, so a live store with rejected boundary appends is reported as degraded
rather than healthy. The split distinguishes a live JVM from a dead or
saturated main dispatcher.
Enable it after installing:

```bash
systemctl --user enable --now futon1b-vitality.timer
```

For a concise, read-only five-minute check:

```bash
python3 scripts/systemd/futon1b-vitality.py --check
```

It exits zero for `OK` and nonzero for `DEGRADED`, while reporting unit state,
both listener latencies, memory-high ratio, and recent evidence rejections.
Manual checks do not advance the timer's journal cursor or monitoring state.

`daemon-reload` only loads definitions; do **not** enable or start the futon3c
service yet. `futon1b-server.service` is the laptop's authoritative evidence
and substrate boundary and may be started independently while futon3c remains
in tmux:

```bash
systemctl --user enable --now futon1b-server.service
curl -fsS http://127.0.0.1:7073/health
```

Keep using the current tmux futon3c server until the quiet-window migration
below.

## Quiet-window migration (exact commands)

```bash
cd /home/joe/code/futon3c
scripts/systemd/server-guard assert-one
curl -fsS http://127.0.0.1:7070/agency/connected

# Gracefully stop the legacy tmux-owned JVM, then wait for it to disappear.
tmux send-keys -t futon-dev:server C-c
while pgrep -f '[c]lojure\.main -m futon3c\.dev($| )' >/dev/null; do sleep 1; done
scripts/systemd/server-guard assert-zero

# Remove the dead legacy pane so it cannot be respawned accidentally.
tmux kill-window -t futon-dev:server

# First managed start. Do not run the legacy ~/bin/fdev after this migration.
scripts/systemd/futon3c-serverctl start
scripts/systemd/futon3c-serverctl status
curl -fsS http://127.0.0.1:7070/agency/connected
scripts/systemd/scope-diagnostic

# Enable boot start only after the above checks pass.
systemctl --user enable futon3c-server.service
```

Thereafter use `scripts/systemd/futon3c-serverctl restart`. It stops the old
unit, inspects the old cgroup recursively, refuses to start if any descendant
remains, runs the I-0 zero-JVM preflight, and verifies exactly one new serving
JVM. `Restart=on-failure` uses systemd's same control-group teardown boundary.
Use `futon3c-serverctl logs` instead of the old server pane.

## Agent/click/test integration

The helper is directly executable or sourceable:

```bash
# futon0/scripts/cr integration point
exec /home/joe/code/futon3c/scripts/systemd/run-agent-scope cr "$agent_id" -- claude ...

# futon0/scripts/cx
exec /home/joe/code/futon3c/scripts/systemd/run-agent-scope cx "$agent_id" -- codex ...

# futon0/scripts/cz
exec /home/joe/code/futon3c/scripts/systemd/run-agent-scope cz "$agent_id" -- zai ...

# War Machine click and repository tests
/home/joe/code/futon3c/scripts/systemd/run-agent-scope click wm-full-loop -- \
  clojure -M:wm-full-loop once ...
/home/joe/code/futon3c/scripts/systemd/run-agent-scope test futon3c-http -- \
  clojure -M:test -n futon3c.transport.http-test
```

Every scope description records its workload label and originating tmux pane.
`scope-diagnostic` joins that metadata to cgroup memory, pressure, task counts,
and processes, and marks active scopes whose recorded pane has disappeared.

## Non-destructive lifecycle tests

With the current live server still running:

```bash
cd /home/joe/code/futon3c
scripts/systemd/lifecycle-tests.sh
```

The test creates only `futon-lifecycle-test-*` scopes. It proves that a
setsid-detached child is killed at a scope respawn boundary, a 32 MiB workload
is killed without changing the Agency PID, and exactly one serving futon3c JVM
remains. A trap stops test units and removes temporary files on every exit.
