@echo off
setlocal EnableExtensions EnableDelayedExpansion

rem MFUTON_HOME contract:
rem - DO NOT SET THIS VARIABLE IN THIS SCRIPT OR AS AN INLINE WORKAROUND.
rem - It is an external environment contract that should already be set before
rem   this wrapper is launched.
rem - If it is missing, report the issue to the operator before continuing.
if not defined MFUTON_HOME (
  1>&2 echo [stop-dev-stack] ERROR: MFUTON_HOME is required.
  1>&2 echo [stop-dev-stack] DO NOT set this variable in this script.
  1>&2 echo [stop-dev-stack] MFUTON_HOME should already be present in the launch environment.
  1>&2 echo [stop-dev-stack] Report this issue to the operator before continuing.
  exit /b 1
)

for %%I in ("%MFUTON_HOME%") do set "MFUTON_HOME=%%~fI"
set "MFUTON_CODEX_PY=%MFUTON_HOME%\agent_skills\development\codex-python.bat"
set "MFUTON_STOP_OWNER=%MFUTON_HOME%\src\mfuton\development\futon3c_stop_dev_stack_windows.py"

if not exist "%MFUTON_CODEX_PY%" (
  1>&2 echo [stop-dev-stack] ERROR: missing %MFUTON_CODEX_PY%.
  exit /b 1
)

if not exist "%MFUTON_STOP_OWNER%" (
  1>&2 echo [stop-dev-stack] ERROR: missing %MFUTON_STOP_OWNER%.
  exit /b 1
)

call "%MFUTON_CODEX_PY%" "%MFUTON_STOP_OWNER%" %*
exit /b %ERRORLEVEL%
