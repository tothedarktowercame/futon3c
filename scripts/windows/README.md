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
- defaults `FUTON3C_CODEX_AGENT_ID=codex-1` (Unix-compatible behavior)
- defaults `FUTON3C_CODEX_WS_BRIDGE=false` so codex invokes run local/inline
  for this stack (prevents WS routing from collapsing into the active VS Code
  codex chat lane)
- clears inherited `CODEX_THREAD_ID` and `CODEX_INTERNAL_ORIGINATOR_OVERRIDE`
  by default so IRC invokes do not bind to the active VS Code chat thread
  (set `FUTON3C_IRC_USE_VSCODE_THREAD=1` to opt back in)
- defaults `BRIDGE_BOTS` by IRC lane when unset:
  - `local` -> `codex`
  - `linode` -> `zcodex`
- if `BRIDGE_BOTS` includes `zcodex`, launcher auto-defaults:
  - `FUTON3C_CODEX_AGENT_ID=codex-1` (unless already set)
  - `NICK_AGENT_MAP=zcodex:codex-1` (unless already set)
- if `BRIDGE_BOTS=codex`, applies `FUTON3C_REGISTER_CLAUDE=false` and
  `FUTON3C_RELAY_CLAUDE=false` before runtime startup
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
- default (no flag) -> local lane
  - starts/waits for local futon3c IRC listener on `FUTON3C_IRC_PORT` (default `6667`)
  - bridge defaults to `IRC_HOST=127.0.0.1`, `IRC_PORT=<FUTON3C_IRC_PORT>`,
    `IRC_CHANNEL=#futon`
- `--remote-irc` -> linode lane (alias: joe lane)
  - skips local IRC port kill/wait in `dev-stack-windows.bat`
  - forces `BRIDGE_BOTS=zcodex`
  - forces `FUTON3C_CODEX_AGENT_ID=codex-1`
  - forces `NICK_AGENT_MAP=zcodex:codex-1`
  - defaults `FUTON3C_REGISTER_CLAUDE=false` and `FUTON3C_RELAY_CLAUDE=false`
    when unset (no `zclaude` lane)
  - defaults `CODEX_SESSION_FILE=<repo>/.state/codex-zabuton/session-id`
    when unset, to keep Zabuton invokes out of the active developer chat session
  - defaults `FUTON3C_IRC_PORT=0` when unset (disable local built-in IRC)
  - bridge defaults to `IRC_HOST=172.236.28.208`, `IRC_PORT=6667`,
    `IRC_CHANNEL=#zabuton`
- `--math-irc`
  - ensures `IRC_CHANNELS` includes `#math`
  - preserves the primary channel selected by the active lane
    (`#futon` local, `#zabuton` linode)
  - intended for `README-math.md` bring-up where `zcodex` should join both
    `#zabuton` and `#math`
- you can still override IRC target defaults by setting `IRC_HOST`, `IRC_PORT`,
  and/or `IRC_CHANNEL` before launch.

Examples:
- local IRC (current behavior):
  - `scripts/windows/futon-windows.bat dev`
- Joe/Linode IRC lane:
  - `scripts/windows/futon-windows.bat dev --remote-irc`
- Joe/Linode IRC lane plus `#math`:
  - `scripts/windows/futon-windows.bat dev --remote-irc --math-irc`
- Joe/Linode IRC lane on `#futon` instead:
  - `set IRC_CHANNEL=#futon && scripts/windows/futon-windows.bat dev --remote-irc`

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
