@echo off
setlocal EnableExtensions EnableDelayedExpansion

if not defined MFUTON_HOME (
  1>&2 echo [stop-dev-stack] ERROR: MFUTON_HOME is required.
  1>&2 echo [stop-dev-stack] Set MFUTON_HOME to the mfuton checkout that owns the Windows stop control logic for this wrapper.
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
