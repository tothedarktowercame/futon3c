@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

set "RUNNER=%SCRIPT_DIR%\tickle-runner.py"
if not exist "%RUNNER%" (
  1>&2 echo [tickle-windows] ERROR: missing %RUNNER%
  exit /b 1
)

where conda >nul 2>nul
if errorlevel 1 (
  1>&2 echo [tickle-windows] ERROR: conda not found on PATH.
  1>&2 echo Install Miniforge/Conda and ensure `conda` is available.
  exit /b 1
)

echo [tickle-windows] Launching tickle watchdog on Windows lane
call conda run --no-capture-output -n codex python "%RUNNER%" %*
exit /b %ERRORLEVEL%
