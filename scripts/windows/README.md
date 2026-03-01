# Windows Script Surface (futon3c)

Purpose:
- define the Windows companion command surface
- avoid duplicating target/script mappings across docs

Entry point:
- `scripts/windows/futon-windows.bat <target> [ARGS=...]`

Target authority:
- `Makefile.windows` is the canonical target list.
- this file maps each target to the implementing script for quick operator reference.

## Target Mapping

| Target | Script |
|---|---|
| `tools` | `scripts/windows/bootstrap-tools.bat` |
| `preflight` | `scripts/windows/preflight-windows.bat` |
| `dev` | `scripts/windows/dev-windows.bat` |
| `test` | `scripts/windows/test-windows.bat` |
| `status` | `scripts/windows/status-windows.bat` |
| `repl` | `scripts/windows/repl-windows.bat` |
| `codex` | `scripts/windows/codex-picker-windows.bat` |
| `codex-repl` | `scripts/windows/codex-repl-windows.bat` |

## Codex Launcher Boundary

- `codex-picker-windows.bat` and `codex-repl-windows.bat` are Windows companions.
- Unix-authoritative behavior remains in:
  - `scripts/codex-picker`
- `run-bash-script-windows.bat` is the bridge that executes Unix shell scripts
  under Windows through `bash.exe`.

