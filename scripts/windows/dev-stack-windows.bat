@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon4\dev\web") do set "DEFAULT_FUTON1A_STATIC_DIR=%%~fI"
for %%I in ("%REPO_ROOT%\..\..\gh\mfuton") do set "LOCAL_INSTALLATION_ROOT=%%~fI"
set "STOP_DEV_STACK=%SCRIPT_DIR%\stop-dev-stack-windows.bat"
set "DEV_STACK_SUPERVISOR=%SCRIPT_DIR%\dev-stack-supervisor.ps1"
set "DEV_STACK_BODY=%SCRIPT_DIR%\dev-stack-body-windows.bat"

set "REMOTE_IRC=0"
set "MATH_IRC=0"
set "FORWARD_ARGS="
:parse_args
if "%~1"=="" goto args_parsed
if /i "%~1"=="--remote-irc" (
  set "REMOTE_IRC=1"
) else if /i "%~1"=="--math-irc" (
  set "MATH_IRC=1"
) else (
  if defined FORWARD_ARGS (
    set "FORWARD_ARGS=!FORWARD_ARGS! %1"
  ) else (
    set "FORWARD_ARGS=%1"
  )
)
shift
goto parse_args
:args_parsed

set "FUTON3C_IRC_LANE=local"
if "%REMOTE_IRC%"=="1" (
  set "FUTON3C_IRC_LANE=linode"
  set "BRIDGE_BOTS=zcodex"
  set "FUTON3C_CODEX_AGENT_ID=codex-1"
  set "NICK_AGENT_MAP=zcodex:codex-1"
  if not defined FUTON3C_REGISTER_CLAUDE set "FUTON3C_REGISTER_CLAUDE=false"
  if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
  if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex-zabuton\session-id"
)
set "FUTON3C_IRC_LANE_NORMALIZED=%FUTON3C_IRC_LANE: =%"
if /i "%FUTON3C_IRC_LANE_NORMALIZED%"=="joe" set "FUTON3C_IRC_LANE_NORMALIZED=linode"

if not defined FUTON1A_PORT set "FUTON1A_PORT=7071"
if not defined FUTON3C_PORT set "FUTON3C_PORT=7070"
if not defined FUTON3C_DRAWBRIDGE_PORT set "FUTON3C_DRAWBRIDGE_PORT=6768"
if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex-irc\session-id"
if not defined FUTON3C_CODEX_WS_BRIDGE set "FUTON3C_CODEX_WS_BRIDGE=false"
if not defined CODEX_BRIDGE_SUMMARY_MODE set "CODEX_BRIDGE_SUMMARY_MODE=raw"

if not defined FUTON3C_IRC_USE_VSCODE_THREAD set "FUTON3C_IRC_USE_VSCODE_THREAD=0"
set "FUTON3C_IRC_USE_VSCODE_THREAD_NORMALIZED=%FUTON3C_IRC_USE_VSCODE_THREAD: =%"
if /i not "%FUTON3C_IRC_USE_VSCODE_THREAD_NORMALIZED%"=="1" (
  if defined CODEX_THREAD_ID (
    echo [dev-stack-windows] Clearing inherited CODEX_THREAD_ID for IRC lane isolation.
    set "CODEX_THREAD_ID="
  )
  if defined CODEX_INTERNAL_ORIGINATOR_OVERRIDE (
    echo [dev-stack-windows] Clearing inherited CODEX_INTERNAL_ORIGINATOR_OVERRIDE for IRC lane isolation.
    set "CODEX_INTERNAL_ORIGINATOR_OVERRIDE="
  )
)

set "USE_LOCAL_IRC=0"
if /i "%FUTON3C_IRC_LANE_NORMALIZED%"=="local" (
  set "USE_LOCAL_IRC=1"
  if not defined BRIDGE_BOTS set "BRIDGE_BOTS=codex"
  if not defined FUTON3C_IRC_PORT set "FUTON3C_IRC_PORT=6667"
  if not defined IRC_HOST set "IRC_HOST=127.0.0.1"
  if not defined IRC_PORT set "IRC_PORT=%FUTON3C_IRC_PORT%"
  if not defined IRC_CHANNEL set "IRC_CHANNEL=#futon"
) else if /i "%FUTON3C_IRC_LANE_NORMALIZED%"=="linode" (
  if not defined BRIDGE_BOTS set "BRIDGE_BOTS=zcodex"
  if not defined FUTON3C_IRC_PORT set "FUTON3C_IRC_PORT=0"
  if not defined IRC_HOST set "IRC_HOST=172.236.28.208"
  if not defined IRC_PORT set "IRC_PORT=6667"
  if not defined IRC_CHANNEL set "IRC_CHANNEL=#zabuton"
) else (
  1>&2 echo [dev-stack-windows] ERROR: unsupported FUTON3C_IRC_LANE=%FUTON3C_IRC_LANE%.
  1>&2 echo [dev-stack-windows] Expected: local ^| linode ^(alias: joe^)
  exit /b 1
)

if "%MATH_IRC%"=="1" (
  set "IRC_CHANNELS_NORMALIZED=%IRC_CHANNELS: =%"
  if not defined IRC_CHANNELS (
    set "IRC_CHANNELS=#math"
  ) else (
    set "IRC_CHANNELS_SCAN=%IRC_CHANNELS_NORMALIZED%"
    set "IRC_CHANNELS_SCAN=!IRC_CHANNELS_SCAN:#math=!"
    if "!IRC_CHANNELS_SCAN!"=="%IRC_CHANNELS_NORMALIZED%" (
      set "IRC_CHANNELS=%IRC_CHANNELS_NORMALIZED%,#math"
    ) else (
      set "IRC_CHANNELS=%IRC_CHANNELS_NORMALIZED%"
    )
  )
  echo [dev-stack-windows] Additional IRC channels: !IRC_CHANNELS!
  if "%REMOTE_IRC%"=="1" (
    echo [dev-stack-windows] Math IRC lane: using shared codex agent codex-1 with zcodex IRC nick
  )
)

if not defined FUTON3C_REPOS (
  if exist "%LOCAL_INSTALLATION_ROOT%\holes\missions" (
    set "FUTON3C_REPOS=installation=%LOCAL_INSTALLATION_ROOT%"
    echo [dev-stack-windows] FUTON3C_REPOS injected from local installation root: %LOCAL_INSTALLATION_ROOT%
  )
)

if not defined FUTON1A_STATIC_DIR (
  if exist "%DEFAULT_FUTON1A_STATIC_DIR%\evidence-viewer\index.html" (
    set "FUTON1A_STATIC_DIR=%DEFAULT_FUTON1A_STATIC_DIR%"
    echo [dev-stack-windows] FUTON1A_STATIC_DIR defaulted to !FUTON1A_STATIC_DIR!
  ) else (
    echo [dev-stack-windows] WARN: FUTON1A_STATIC_DIR unset and default missing: %DEFAULT_FUTON1A_STATIC_DIR%
  )
) else (
  echo [dev-stack-windows] FUTON1A_STATIC_DIR preset: %FUTON1A_STATIC_DIR%
)

set "BRIDGE_BOTS_NORMALIZED=%BRIDGE_BOTS: =%"
set "USES_ZCODEX=0"
set "BRIDGE_BOTS_SCAN=%BRIDGE_BOTS_NORMALIZED%"
set "BRIDGE_BOTS_SCAN=!BRIDGE_BOTS_SCAN:zcodex=!"
if not "!BRIDGE_BOTS_SCAN!"=="%BRIDGE_BOTS_NORMALIZED%" set "USES_ZCODEX=1"
if "%USES_ZCODEX%"=="1" (
  if not defined FUTON3C_CODEX_AGENT_ID set "FUTON3C_CODEX_AGENT_ID=codex-1"
  if not defined NICK_AGENT_MAP set "NICK_AGENT_MAP=zcodex:codex-1"
) else (
  if not defined FUTON3C_CODEX_AGENT_ID set "FUTON3C_CODEX_AGENT_ID=codex-1"
)
if /i "%BRIDGE_BOTS_NORMALIZED%"=="codex" (
  if not defined FUTON3C_REGISTER_CLAUDE set "FUTON3C_REGISTER_CLAUDE=false"
  if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
  echo [dev-stack-windows] codex-only mode: FUTON3C_REGISTER_CLAUDE=!FUTON3C_REGISTER_CLAUDE! FUTON3C_RELAY_CLAUDE=!FUTON3C_RELAY_CLAUDE!
)
echo [dev-stack-windows] Codex invoke lane: FUTON3C_CODEX_WS_BRIDGE=%FUTON3C_CODEX_WS_BRIDGE%
echo [dev-stack-windows] Codex agent id: %FUTON3C_CODEX_AGENT_ID%
if defined NICK_AGENT_MAP echo [dev-stack-windows] NICK_AGENT_MAP=!NICK_AGENT_MAP!

if not exist "%STOP_DEV_STACK%" (
  1>&2 echo [dev-stack-windows] ERROR: missing cleanup script %STOP_DEV_STACK%.
  exit /b 1
)
if not exist "%DEV_STACK_SUPERVISOR%" (
  1>&2 echo [dev-stack-windows] ERROR: missing supervisor script %DEV_STACK_SUPERVISOR%.
  exit /b 1
)
if not exist "%DEV_STACK_BODY%" (
  1>&2 echo [dev-stack-windows] ERROR: missing body script %DEV_STACK_BODY%.
  exit /b 1
)

set "FUTON_DEV_STACK_BRIDGE_ARGS=%FORWARD_ARGS%"
powershell -NoProfile -ExecutionPolicy Bypass -File "%DEV_STACK_SUPERVISOR%" -ChildScript "%DEV_STACK_BODY%" -CleanupScript "%STOP_DEV_STACK%"
exit /b %ERRORLEVEL%
