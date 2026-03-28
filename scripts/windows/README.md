# Windows Script Surface (futon3c)

Purpose:
- define the Windows companion command surface
- avoid duplicating target/script mappings across docs

Entry point:
- `scripts/windows/futon-windows.bat <target> [ARGS=...]`

Shell note:
- `cd /d ...` is `cmd.exe` syntax.
- in PowerShell use `Set-Location <path>` (or `cd <path>`).
- cross-shell safe form:
  - `cmd /c "cd /d <path> && scripts\\windows\\futon-windows.bat <target> ..."`

Target authority:
- `Makefile.windows` is the canonical target list.
- this file maps each target to the implementing script for quick operator reference.
- `scripts/windows/futon-windows.bat` may directly dispatch selected targets
  (`dev`, `dev-arxana`, `dev-core`, `test`, `status`, `repl`, `codex`,
  `codex-repl`, `tickle`, `ngircd-bridge`) to avoid make/shell interop drift
  on Windows.

## Target Mapping

| Target | Script |
|---|---|
| `tools` | `scripts/windows/bootstrap-tools.bat` |
| `preflight` | `scripts/windows/preflight-windows.bat` |
| `stop-futon1a` / `kill-futon1a` | `scripts/windows/stop-futon1a-windows.bat` |
| `dev` | `scripts/windows/dev-stack-windows.bat` |
| `dev-arxana` | `scripts/windows/dev-stack-arxana-windows.bat` |
| `dev-core` | `scripts/windows/dev-windows.bat` |
| `test` | `scripts/windows/test-windows.bat` |
| `status` | `scripts/windows/status-windows.bat` |
| `repl` | `scripts/windows/repl-windows.bat` |
| `codex` | `scripts/windows/codex-picker-windows.bat` |
| `codex-repl` | `scripts/windows/codex-repl-windows.bat` |
| `tickle` | `scripts/windows/tickle-windows.bat` |
| `ngircd-bridge` | `scripts/windows/ngircd-bridge-windows.bat` |

Internal lifecycle helper authority:
- `scripts/windows/dev-stack-windows.bat`
  - operator-facing full-stack entrypoint
- `scripts/windows/stop-dev-stack-windows.bat`
  - canonical full-stack cleanup authority for Windows lane bring-up/teardown
- `scripts/windows/dev-stack-supervisor.ps1`
  - internal lifecycle wrapper that preserves cleanup on launcher exit and `Ctrl-C`
  - do not treat this as a separate operator target; invoke `dev` instead

Codex target arg forms accepted by `futon-windows.bat`:
- `scripts/windows/futon-windows.bat codex --help`
- `scripts/windows/futon-windows.bat codex ARGS=--help`
- `scripts/windows/futon-windows.bat codex-repl --new`
- `scripts/windows/futon-windows.bat codex-repl ARGS=--new`

Tickle launcher examples:
- `scripts/windows/futon-windows.bat tickle`
- `scripts/windows/futon-windows.bat tickle --interval 30 --threshold 120`
- `set FUTON3C_EVIDENCE_BASE=http://127.0.0.1:7070 && scripts/windows/futon-windows.bat tickle`
- implementation note:
  - Windows companion uses `conda run -n codex python scripts/windows/tickle-runner.py`
  - Unix launcher semantics remain authoritative in `scripts/tickle-start`

ngircd bridge launch examples:
- `scripts/windows/futon-windows.bat ngircd-bridge`
- default is codex-only (`BRIDGE_BOTS=codex`)
- `set BRIDGE_BOTS=codex && scripts/windows/futon-windows.bat ngircd-bridge`
- `set BRIDGE_BOTS=claude,codex && scripts/windows/futon-windows.bat ngircd-bridge`
- when `BRIDGE_BOTS=codex`, the bridge wrapper defaults
  `FUTON3C_REGISTER_CLAUDE=false` and `FUTON3C_RELAY_CLAUDE=false`
  (unless already set) so codex is the only active bot lane.
- the Windows bridge wrapper defaults `CODEX_BRIDGE_SUMMARY_MODE=raw` so codex
  replies can preserve multi-line IRC bodies; set it to `summary` before launch
  to restore the older one-line summary behavior.

Full-stack launch (`dev`) behavior:
- runs `stop-futon1a-windows.bat`
- stops existing listeners on `7070`, `6667`, and (when configured) `6768`
- defaults `FUTON1A_STATIC_DIR` to `..\futon4\dev\web` when unset and assets exist
- defaults `CODEX_SESSION_FILE` to `<repo>/.state/codex-irc/session-id`
  for a dedicated IRC codex continuity lane
- defaults `CODEX_SANDBOX=read-only` when unset (most constrained sandbox)
- defaults `CODEX_APPROVAL_POLICY=untrusted` when unset; if `CODEX_APPROVAL`
  is set and `CODEX_APPROVAL_POLICY` is unset, launcher maps `CODEX_APPROVAL`
  into `CODEX_APPROVAL_POLICY`
- defaults `FUTON3C_CODEX_AGENT_ID=codex-1` (Unix-compatible behavior)
- defaults `FUTON3C_REGISTER_VSCODE_CODEX=true`
- defaults `FUTON3C_VSCODE_AGENT_ID=codex-vscode`
- defaults `FUTON3C_VSCODE_CODEX_SESSION_FILE=<repo>/.state/codex-vscode/session-id`
  so the separate VS Code codex lane stays live in every Windows dev-stack mode
- defaults `FUTON3C_CODEX_WS_BRIDGE=false` so codex invokes run local/inline
  for this stack (prevents WS routing from collapsing into the active VS Code
  codex chat lane)
- defaults `FUTON3C_REGISTER_CORPUS=false`; `corpus-1` is reserved for the
  FrontierMath-local lane instead of appearing in every generic dev bring-up
- keeps `FUTON3C_RELAY_CLAUDE=false` in the Windows-owned overlay so the
  external `ngircd-bridge` remains the IRC invoke authority for this launcher
- clears inherited `CODEX_THREAD_ID` and `CODEX_INTERNAL_ORIGINATOR_OVERRIDE`
  by default so IRC invokes do not bind to the active VS Code chat thread
  (set `FUTON3C_IRC_USE_VSCODE_THREAD=1` to opt back in)
- defaults `BRIDGE_BOTS` by IRC lane when unset:
  - `local` -> `codex`
  - `linode` -> `zcodex`
- if `BRIDGE_BOTS` includes `zcodex`, launcher auto-defaults:
  - `FUTON3C_CODEX_AGENT_ID=codex-1` (unless already set)
  - `NICK_AGENT_MAP=zcodex:codex-1` (unless already set)
- bridge bare `!` ownership can be overridden per room with:
  - `IRC_COMMAND_OWNER_AGENT_MAP=#channel:agent-id,...`
  - this is keyed by internal agent id, not IRC nick
  - when this map is set, it is authoritative for that bridge:
    unmapped rooms get no bare `!` response from that bridge
- if `BRIDGE_BOTS=codex`, applies `FUTON3C_REGISTER_CLAUDE=false` and
  leaves `FUTON3C_RELAY_CLAUDE=false` before runtime startup
- if `FUTON3C_REPOS` is unset and a local installation root exists at
  `..\..\gh\mfuton`, `dev` temporarily injects:
  `FUTON3C_REPOS=installation=<that-root>`
- otherwise, runtime uses futon3c native default repo-root discovery
- for deterministic repo scope, set `FUTON3C_REPOS` explicitly before startup
- full-stack launch now runs under a supervisor that always calls
  `stop-dev-stack-windows.bat` on exit or `Ctrl-C`, so the bridge and local
  runtime ports are torn down instead of being left behind as orphaned
  processes
- defaults `CODEX_BRIDGE_SUMMARY_MODE=raw` unless already set so multiline
  codex replies survive the bridge on Windows by default
- starts `dev-core` in the background with output streamed to the same console
- waits for runtime ports, then starts `ngircd-bridge`

IRC lane switch for `dev`:
- unsupported positional args or unknown switches now fail fast with usage
  guidance instead of being silently forwarded
- explicit environment selector:
  - `--env windows-local`
    - the restored default when omitted
    - keeps the Windows-local runtime posture
  - `--env dev-laptop-env`
    - selects the typed reference copy of Joe's Unix `dev-laptop-env`
  - `--env dev-linode-env`
    - selects the typed reference copy of Joe's Unix `dev-linode-env`
  - when omitted, `dev` behaves as though `--env windows-local` was supplied
- default (no flag) -> Windows local base profile
  - keeps the Windows-local runtime posture:
    - `FUTON3C_PORT=7070`
    - `FUTON3C_IRC_PORT=6667`
    - `BRIDGE_BOTS=codex`
    - `IRC_HOST=127.0.0.1`
    - `IRC_PORT=6667`
    - `IRC_CHANNEL=#futon`
    - `CODEX_SESSION_FILE=<repo>/.state/codex-irc/session-id`
    - `USE_LOCAL_IRC=1`
  - the local runtime remains the base behavior; no Joe Unix authority profile
    is selected here
- `--frontiermath-local` -> compatibility redirect to the `futon6` FrontierMath wrapper
  - `dev-stack-windows.bat` no longer owns FrontierMath-local bring-up policy
  - it now delegates immediately to:
    - `futon6/scripts/frontiermath/local-futon3c-windows.bat`
  - default delegated wrapper path:
    - `<repo>/../futon6/scripts/frontiermath/local-futon3c-windows.bat`
  - optional override:
    - `FUTON3C_FRONTIERMATH_LOCAL_WRAPPER=<path to wrapper>`
  - use the `futon6` wrapper directly when you want the canonical owner path
- `--remote-irc` -> remote IRC overlay over that same Windows local base
  - changes only the IRC/bridge lane:
    - `BRIDGE_BOTS=zcodex`
    - `FUTON3C_IRC_LANE=linode`
    - `FUTON3C_IRC_LANE_NORMALIZED=linode`
    - `FUTON3C_IRC_PORT=0`
    - `IRC_HOST=172.236.28.208`
    - `IRC_PORT=6667`
    - `IRC_CHANNEL=#zabuton`
    - `CODEX_SESSION_FILE=<repo>/.state/codex-zabuton/session-id`
    - `FUTON3C_CODEX_AGENT_ID=codex-1`
    - `NICK_AGENT_MAP=zcodex:codex-1`
    - `IRC_COMMAND_OWNER_AGENT_MAP=#zabuton:codex-1`
    - `USE_LOCAL_IRC=0`
  - it does not, by itself, select Joe's full `dev-linode-env` runtime profile
  - the Windows launcher still runs on the local runtime base unless a later
    broader profile selector is added explicitly
  - currently only supported with `--env windows-local`
- modeled Unix reference envs:
  - the Windows config also preserves typed reference copies of Joe's
    `dev-laptop-env` and `dev-linode-env`
  - those are now direct operator-facing selectors through `--env`, while
    remaining separate from the narrower `--remote-irc` overlay
- `--math-irc`
  - ensures `IRC_CHANNELS` includes `#math`
  - preserves the primary channel selected by the active parity profile
    (currently `#zabuton` for both Windows dev-shell parity profiles)
  - intended for `README-math.md` bring-up where `zcodex` should join both
    `#zabuton` and `#math`
  - joining both channels does not imply automatic cross-channel replies;
    bridge replies should return to the channel that originated the mention
    unless a separate relay mode is explicitly introduced
  - when combined with `--remote-irc`, the current Windows trial model keeps:
    - `FUTON3C_CODEX_AGENT_ID=codex-1`
    - `NICK_AGENT_MAP=zcodex:codex-1`
    - `IRC_COMMAND_OWNER_AGENT_MAP=#zabuton:codex-1`
    - `CODEX_SESSION_FILE=<repo>/.state/codex-zabuton/session-id`
  - this means `zcodex` is an IRC nick alias for the shared remote IRC codex
    lane, not a separate local math-agent identity
  - bare `!` command ownership is room-scoped and keyed by internal agent id;
    because `#math` is intentionally left unmapped on this bridge,
    `@zcodex ...` still works there but bare `!` commands do not
  - `codex-vscode` still remains the separate VS Code lane; this trial only
    asks whether one shared `codex-1` worker is already sufficient for both
    `#zabuton` and `#math`
- start-mode flags:
  - these are orthogonal overlays on top of the selected `--env` profile
  - `--start-tickle`
    - sets `FUTON3C_TICKLE_AUTOSTART=true`
  - `--resume-tickle`
    - currently the same as `--start-tickle` on Windows, matching the current
      Unix shell behavior
  - `--start-fm`
    - sets `FUTON3C_FM_CONDUCTOR_AUTOSTART=true`
  - `--start-all`
    - sets both:
      - `FUTON3C_TICKLE_AUTOSTART=true`
      - `FUTON3C_FM_CONDUCTOR_AUTOSTART=true`
  - these flags are supported on the generic `dev` launcher surface
  - they are not supported with `--frontiermath-local`, because that lane is
    owned by the separate futon6 wrapper
- you can still override IRC target defaults by setting `IRC_HOST`, `IRC_PORT`,
  and/or `IRC_CHANNEL` before launch.

Examples:
- Windows local base profile:
  - `scripts/windows/futon-windows.bat dev`
- explicit Windows local base profile:
  - `scripts/windows/futon-windows.bat dev --env windows-local`
- Unix laptop authority profile:
  - `scripts/windows/futon-windows.bat dev --env dev-laptop-env`
- Unix linode authority profile:
  - `scripts/windows/futon-windows.bat dev --env dev-linode-env`
- Windows local base with both tickle and FM conductor autostarted:
  - `scripts/windows/futon-windows.bat dev --start-all`
- Unix laptop authority profile with both tickle and FM conductor autostarted:
  - `scripts/windows/futon-windows.bat dev --env dev-laptop-env --start-all`
- local FrontierMath onboarding lane:
  - `scripts/windows/futon-windows.bat dev --frontiermath-local`
    - now delegates to the `futon6` wrapper rather than owning the lane locally
- launcher help / validation:
  - `scripts/windows/dev-stack-windows.bat --help`
  - `scripts/windows/dev-stack-windows.bat --frontier-math`
    - now fails fast and suggests `--frontiermath-local`
- Windows local base plus remote IRC overlay:
  - `scripts/windows/futon-windows.bat dev --remote-irc`
- Windows local base explicitly named plus remote IRC overlay:
  - `scripts/windows/futon-windows.bat dev --env windows-local --remote-irc`
- Windows local base plus remote IRC overlay and `#math`:
  - `scripts/windows/futon-windows.bat dev --remote-irc --math-irc`
- Windows local base with a different channel override:
  - `set IRC_CHANNEL=#math && scripts/windows/futon-windows.bat dev`

Arxana full-stack launch (`dev-arxana`) behavior:
- forces/validates `FUTON1A_STATIC_DIR` and fails fast if viewer assets are missing
- then runs the standard `dev-stack-windows.bat` flow

Bridge-only behavior:
- `ngircd-bridge` no longer auto-starts runtime services.
- if no local IRC listener is present on `IRC_PORT`, it exits with guidance to
  run `dev-core` (runtime only) or `dev` (full stack).

## Codex Launcher Boundary

- `codex-picker-windows.bat` and `codex-repl-windows.bat` are Windows companions.
- Unix-authoritative behavior remains in:
  - `scripts/codex-picker`
- `run-bash-script-windows.bat` is the bridge that executes Unix shell scripts
  under Windows through `bash.exe`.

`codex-repl` precondition:
- `emacsclient.exe` requires a running Emacs server.
- if no server is running, start one before `codex-repl`:
  - inside Emacs: `M-x server-start`
  - or from shell: `emacs --daemon` (or `runemacs --daemon`, depending on install)
- wrapper default:
  - `run-bash-script-windows.bat` now defaults `HOME=%USERPROFILE%` and
    `TMPDIR=%LOCALAPPDATA%\Temp` when unset, to avoid MSYS Emacs `/tmp`
    permission failures.
- GUI behavior default:
  - `codex-repl-windows.bat` sets:
    - prepends detected native Emacs `bin` directory to `PATH` when found
    - bootstraps one GUI frame via:
      - `emacsclientw -a "" -c -n -e "(progn (switch-to-buffer \"*scratch*\") t)"`
    - then launches `codex-picker-windows.bat --repl`
  - this requests a GUI frame for the REPL by default on Windows.
