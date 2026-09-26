# E2 runner boundary prototype — 2026-08-01

## Design

`scripts/e2_ablation_dispatch.sh` has an outer controller and an inner
boundary. The outer controller can read the source repository only to resolve
the Git tree id of the explicitly named historical revision. It then
pipes its own script through `sudo -n -u apmablate bash -s`; the inner half is
therefore parsed and executed as `apmablate` even though that account cannot
traverse `/home/joe`.

The inner half refuses before starting the runner unless all of these hold:

- effective UID is the UID registered for `apmablate`;
- the staged directory has no `.git`, `git log --all` fails, and a tree object
  built in a temporary Git directory equals the source revision's tree id
  (including paths, executable modes, symlinks, and file contents);
- `/home/joe`, the retrieval analysis directory, Joe's Codex store, and the
  source `.git` all fail with an actual permission-denied result;
- the account's credential directory contains only `auth.json` and the legacy
  `config.toml` copied by setup;
- the per-run `CODEX_HOME` is freshly created with `auth.json` only.

Each probe command, exit status, and boolean result is canonicalized inside
that process. The SHA-256 of the canonical probe array is included in the JSON
receipt. The same shell then runs the command, records its exit status, emits
the receipt, and exits with the runner's status. The outer half only extracts
and validates the already-created receipt; an outside assertion cannot turn a
failed inner probe green.

A Codex command is additionally refused unless it includes:

```text
exec --ephemeral --ignore-user-config --ignore-rules
```

This prevents a nominally fresh session from silently loading or persisting
user state.

## H3g: Codex state and the minimal credential

Local Codex CLI 0.145.0 documents two decisive controls:

- `--ignore-user-config`: do not load `$CODEX_HOME/config.toml`; auth still
  comes from `CODEX_HOME`.
- `--ephemeral`: run without persisting session files.

Therefore the minimum state needed for a ChatGPT-authenticated run is the
Codex binary plus `auth.json`. `config.toml` is not needed: model, sandbox, and
approval settings can be explicit command-line arguments. The setup script's
copy of Joe's complete `config.toml` is unnecessary and should be replaced at
the next privileged setup by either no config or a synthesized empty/minimal
one.

Inventory of Joe's current `~/.codex`, by role:

| category | examples | needed by E2? | leak-bearing? |
|---|---|---:|---:|
| authentication | `auth.json` | yes | credential-secret, but not prior task context |
| user configuration | `config.toml`, rules, skills, plugins | no | yes: Joe project trust paths, tools and policy |
| conversations | `sessions/` (3.3G), `history.jsonl` (20M) | no | **yes** |
| durable agent state | `state_5.sqlite`, `goals_1.sqlite`, `memories_1.sqlite` and WAL/SHM files | no | **yes** |
| logs/snapshots | `logs_2.sqlite`, `log/`, `shell_snapshots/` | no | **yes** |
| caches/catalogues | `cache/`, `models_cache.json`, plugin cache | no | potentially; unnecessary |
| installation metadata | `installation_id`, migrations, `version.json` | no | low-value but unnecessary |

A “fresh session” under the same `CODEX_HOME` does not imply a fresh state
boundary: all of the above remain readable. E2 instead uses a new temporary
`CODEX_HOME` containing only a mode-0600 copy of `auth.json`, ignores config
and rules, runs ephemerally, and deletes the temporary directory afterward.

## Current blocker and attempted demonstration

The wrapper cannot currently enter the execution boundary non-interactively:

```text
$ sudo -n -u apmablate id
sudo: a password is required

$ scripts/e2_ablation_dispatch.sh --problem a95J08 \
    --base-revision 61ddc05 --receipt /tmp/e2-demo.json -- \
    bash -lc 'id && git log -1'
E2 ISOLATION REFUSAL: cannot enter apmablate boundary non-interactively; sudo authorization required
exit 78; receipt absent
```

This is a hard and correctly loud pre-run refusal. It prevents both the real
demo and the requested readable-sentinel sabotage from reaching the inner
boundary. Running either as Joe would be the exact false-isolation result H5
defect 4 forbids, so no substitute demo was recorded.

The smallest privilege change is not broad sudo: authorize Joe to invoke this
single root-owned/reviewed boundary launcher as `apmablate` without a password,
or provide an equivalent root-owned systemd unit with fixed UID, environment,
and executable. The present script cannot safely be named directly in sudoers
while it is Joe-writable; deployment must first place a reviewed copy in a
root-owned path. After that, run the normal demo and then set
`E2_SABOTAGE_READABLE_PATH` to a known readable file in the staged tree. The
extra probe only adds a failure condition and cannot disable any real probe;
the expected sabotage result is exit 78 before the runner begins.
