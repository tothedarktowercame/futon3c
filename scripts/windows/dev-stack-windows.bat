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
set "FRONTIERMATH_LOCAL=0"
:parse_args
if "%~1"=="" goto args_parsed
if /i "%~1"=="--help" goto usage
if /i "%~1"=="-h" goto usage
if /i "%~1"=="--remote-irc" (
  set "REMOTE_IRC=1"
) else if /i "%~1"=="--frontiermath-local" (
  set "FRONTIERMATH_LOCAL=1"
) else if /i "%~1"=="--math-irc" (
  set "MATH_IRC=1"
) else (
  call :unknown_arg "%~1"
  exit /b 1
)
shift
goto parse_args
:args_parsed

if "%REMOTE_IRC%"=="1" if "%FRONTIERMATH_LOCAL%"=="1" (
  1>&2 echo [dev-stack-windows] ERROR: --remote-irc and --frontiermath-local cannot be combined.
  exit /b 1
)

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
if "%FRONTIERMATH_LOCAL%"=="1" (
  set "FUTON3C_IRC_LANE=frontiermath-local"
  if not defined BRIDGE_BOTS set "BRIDGE_BOTS=codex"
  if not defined FUTON3C_REGISTER_CLAUDE set "FUTON3C_REGISTER_CLAUDE=false"
  if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
  if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex-frontiermath-local\session-id"
  if not defined IRC_COMMAND_OWNER_AGENT_MAP set "IRC_COMMAND_OWNER_AGENT_MAP=#futon:codex-1,#math:codex-1"
)
set "FUTON3C_IRC_LANE_NORMALIZED=%FUTON3C_IRC_LANE: =%"
if /i "%FUTON3C_IRC_LANE_NORMALIZED%"=="joe" set "FUTON3C_IRC_LANE_NORMALIZED=linode"

if not defined FUTON1A_PORT set "FUTON1A_PORT=7071"
if not defined FUTON3C_PORT set "FUTON3C_PORT=7070"
if not defined FUTON3C_DRAWBRIDGE_PORT set "FUTON3C_DRAWBRIDGE_PORT=6768"
if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex-irc\session-id"
if not defined FUTON3C_CODEX_WS_BRIDGE set "FUTON3C_CODEX_WS_BRIDGE=false"
if not defined FUTON3C_RELAY_CODEX set "FUTON3C_RELAY_CODEX=false"
if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
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
) else if /i "%FUTON3C_IRC_LANE_NORMALIZED%"=="frontiermath-local" (
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
  1>&2 echo [dev-stack-windows] Expected: local ^| frontiermath-local ^| linode ^(alias: joe^)
  exit /b 1
)

if "%FRONTIERMATH_LOCAL%"=="1" (
  call :ensure_channel_in_list "#math"
  if defined IRC_CHANNELS echo [dev-stack-windows] FrontierMath local extra IRC channels: !IRC_CHANNELS!
)

if "%MATH_IRC%"=="1" (
  if "%FRONTIERMATH_LOCAL%"=="1" (
    echo [dev-stack-windows] WARN: --math-irc is redundant in --frontiermath-local mode; #math is already joined as an extra room.
  )
  call :ensure_channel_in_list "#math"
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

if "%FRONTIERMATH_LOCAL%"=="1" (
  echo [dev-stack-windows] FrontierMath local mode: preserve #futon baseline and add local #math room with isolated codex continuity.
  echo [dev-stack-windows] This mode is distinct from peer-IRC --remote-irc and shared-room --math-irc bring-up.
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
  echo [dev-stack-windows] codex-only mode: FUTON3C_REGISTER_CLAUDE=!FUTON3C_REGISTER_CLAUDE! FUTON3C_RELAY_CLAUDE=!FUTON3C_RELAY_CLAUDE!
)
echo [dev-stack-windows] Codex invoke lane: FUTON3C_CODEX_WS_BRIDGE=%FUTON3C_CODEX_WS_BRIDGE%
echo [dev-stack-windows] Codex agent id: %FUTON3C_CODEX_AGENT_ID%
echo [dev-stack-windows] IRC dispatch relays: FUTON3C_RELAY_CODEX=!FUTON3C_RELAY_CODEX! FUTON3C_RELAY_CLAUDE=!FUTON3C_RELAY_CLAUDE!
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

powershell.exe -NoProfile -ExecutionPolicy Bypass -Command ^
  "& '%DEV_STACK_SUPERVISOR%' -ChildScript '%DEV_STACK_BODY%' -CleanupScript '%STOP_DEV_STACK%'"
exit /b %ERRORLEVEL%

:unknown_arg
setlocal
set "BAD_ARG=%~1"
1>&2 echo [dev-stack-windows] ERROR: unsupported argument %BAD_ARG%.
if /i "%BAD_ARG%"=="--frontier-math" (
  1>&2 echo [dev-stack-windows] Did you mean --frontiermath-local?
)
1>&2 echo [dev-stack-windows] Supported flags:
1>&2 echo [dev-stack-windows]   --frontiermath-local
1>&2 echo [dev-stack-windows]   --remote-irc
1>&2 echo [dev-stack-windows]   --math-irc
1>&2 echo [dev-stack-windows]   --help
endlocal & exit /b 0

:usage
echo Usage: dev-stack-windows.bat [--frontiermath-local] [--remote-irc] [--math-irc]
echo.
echo Supported flags:
echo   --frontiermath-local  local FrontierMath onboarding lane ^(#futon + #math^)
echo   --remote-irc          Joe/Linode IRC lane
echo   --math-irc            add #math as an extra joined room
echo   --help                show this usage
exit /b 0

:ensure_channel_in_list
setlocal EnableDelayedExpansion
set "TARGET_CHANNEL=%~1"
set "UPDATED_CHANNELS="
if not defined IRC_CHANNELS (
  set "UPDATED_CHANNELS=!TARGET_CHANNEL!"
) else (
  set "IRC_CHANNELS_NORMALIZED=!IRC_CHANNELS: =!"
  set "IRC_CHANNELS_SCAN=!IRC_CHANNELS_NORMALIZED:%~1=!"
  if "!IRC_CHANNELS_SCAN!"=="!IRC_CHANNELS_NORMALIZED!" (
    set "UPDATED_CHANNELS=!IRC_CHANNELS_NORMALIZED!,!TARGET_CHANNEL!"
  ) else (
    set "UPDATED_CHANNELS=!IRC_CHANNELS_NORMALIZED!"
  )
)
endlocal & set "IRC_CHANNELS=%UPDATED_CHANNELS%"
exit /b 0
