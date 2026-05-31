@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

rem MFUTON_HOME contract:
rem - DO NOT SET THIS VARIABLE IN THIS SCRIPT OR AS AN INLINE WORKAROUND.
rem - It is an external environment contract that should already be set before
rem   this wrapper is launched.
rem - If it is missing, report the issue to the operator before continuing.
if not defined MFUTON_HOME (
  1>&2 echo [dev-stack-windows] ERROR: MFUTON_HOME is required.
  1>&2 echo [dev-stack-windows] DO NOT set this variable in this script.
  1>&2 echo [dev-stack-windows] MFUTON_HOME should already be present in the launch environment.
  1>&2 echo [dev-stack-windows] Report this issue to the operator before continuing.
  exit /b 1
)

for %%I in ("%MFUTON_HOME%") do set "MFUTON_HOME=%%~fI"
set "MFUTON_CODEX_PY=%MFUTON_HOME%\agent_skills\development\codex-python.bat"
set "MFUTON_LAUNCHER=%MFUTON_HOME%\src\mfuton\development\futon3c_dev_stack_windows.py"
set "DEV_STACK_CONFIG=%MFUTON_HOME%\config\futon3c-dev-stack-windows.json"

if not exist "%MFUTON_CODEX_PY%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %MFUTON_CODEX_PY%.
  exit /b 1
)

if not exist "%MFUTON_LAUNCHER%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %MFUTON_LAUNCHER%.
  exit /b 1
)

if not exist "%DEV_STACK_CONFIG%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %DEV_STACK_CONFIG%.
  exit /b 1
)

call "%MFUTON_CODEX_PY%" "%MFUTON_LAUNCHER%" --futon3c-root "%REPO_ROOT%" --config "%DEV_STACK_CONFIG%" %*
exit /b %ERRORLEVEL%
