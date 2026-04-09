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
set "STOP_LAUNCHER_ARGS="

if defined FUTON3C_STOP_FUTON3C_ROOT set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% --futon3c-root %FUTON3C_STOP_FUTON3C_ROOT%"
if defined FUTON3C_STOP_CONFIG set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% --config %FUTON3C_STOP_CONFIG%"
if defined FUTON3C_STOP_ENV set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% --env %FUTON3C_STOP_ENV%"
if "%FUTON3C_STOP_DEV%"=="1" set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% dev"
if "%FUTON3C_STOP_REMOTE_IRC%"=="1" set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% --remote-irc"
if "%FUTON3C_STOP_NO_LOCAL_IRC%"=="1" set "STOP_LAUNCHER_ARGS=%STOP_LAUNCHER_ARGS% --no-local-irc"

if not exist "%MFUTON_CODEX_PY%" (
  1>&2 echo [stop-dev-stack] ERROR: missing %MFUTON_CODEX_PY%.
  exit /b 1
)

if not exist "%MFUTON_STOP_OWNER%" (
  1>&2 echo [stop-dev-stack] ERROR: missing %MFUTON_STOP_OWNER%.
  exit /b 1
)

call "%MFUTON_CODEX_PY%" "%MFUTON_STOP_OWNER%" %STOP_LAUNCHER_ARGS% %*
exit /b %ERRORLEVEL%
