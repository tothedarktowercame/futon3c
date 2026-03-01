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
  (`dev`, `test`, `status`, `repl`, `codex`, `codex-repl`) to avoid make/shell
  interop drift on Windows.

## Target Mapping

| Target | Script |
|---|---|
| `tools` | `scripts/windows/bootstrap-tools.bat` |
| `preflight` | `scripts/windows/preflight-windows.bat` |
| `stop-futon1a` / `kill-futon1a` | `scripts/windows/stop-futon1a-windows.bat` |
| `dev` | `scripts/windows/dev-windows.bat` |
| `test` | `scripts/windows/test-windows.bat` |
| `status` | `scripts/windows/status-windows.bat` |
| `repl` | `scripts/windows/repl-windows.bat` |
| `codex` | `scripts/windows/codex-picker-windows.bat` |
| `codex-repl` | `scripts/windows/codex-repl-windows.bat` |

Codex target arg forms accepted by `futon-windows.bat`:
- `scripts/windows/futon-windows.bat codex --help`
- `scripts/windows/futon-windows.bat codex ARGS=--help`
- `scripts/windows/futon-windows.bat codex-repl --new`
- `scripts/windows/futon-windows.bat codex-repl ARGS=--new`

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
