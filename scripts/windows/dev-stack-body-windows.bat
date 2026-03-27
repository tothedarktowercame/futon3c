@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
set "STOP_DEV_STACK=%SCRIPT_DIR%\stop-dev-stack-windows.bat"

echo [dev-stack-windows] Preparing clean runtime start...
call "%STOP_DEV_STACK%"
if errorlevel 1 exit /b 1
if "%USE_LOCAL_IRC%"=="0" (
  echo [dev-stack-windows] IRC lane=%FUTON3C_IRC_LANE_NORMALIZED% ^(remote IRC: %IRC_HOST%:%IRC_PORT% channel=%IRC_CHANNEL%^)
  if "%FUTON3C_IRC_PORT%"=="0" (
    echo [dev-stack-windows] Local FUTON3C IRC server disabled ^(FUTON3C_IRC_PORT=0^).
  ) else (
    echo [dev-stack-windows] Local FUTON3C IRC port left as configured: %FUTON3C_IRC_PORT%
  )
)

set "BRIDGE_LOG_IRC_HOST=%IRC_HOST%"
if not defined BRIDGE_LOG_IRC_HOST set "BRIDGE_LOG_IRC_HOST=127.0.0.1"
set "BRIDGE_LOG_IRC_PORT=%IRC_PORT%"
if not defined BRIDGE_LOG_IRC_PORT set "BRIDGE_LOG_IRC_PORT=%FUTON3C_IRC_PORT%"
if not defined BRIDGE_LOG_IRC_PORT set "BRIDGE_LOG_IRC_PORT=6667"
set "BRIDGE_LOG_IRC_CHANNEL=%IRC_CHANNEL%"
if not defined BRIDGE_LOG_IRC_CHANNEL set "BRIDGE_LOG_IRC_CHANNEL=#futon"

echo [dev-stack-windows] Starting futon runtime lane...
start "futon-dev-core" /b cmd.exe /c call "%SCRIPT_DIR%\dev-windows.bat"

call :wait_for_port %FUTON3C_PORT% 120 futon3c-http
if errorlevel 1 exit /b 1
if "%USE_LOCAL_IRC%"=="1" (
  call :wait_for_port %FUTON3C_IRC_PORT% 120 irc
  if errorlevel 1 exit /b 1
)

echo [dev-stack-windows] Starting ngircd bridge...
echo [dev-stack-windows] Bridge target IRC=!BRIDGE_LOG_IRC_HOST!:!BRIDGE_LOG_IRC_PORT! channel=!BRIDGE_LOG_IRC_CHANNEL! lane=!FUTON3C_IRC_LANE_NORMALIZED!
if defined IRC_CHANNELS echo [dev-stack-windows] Bridge extra IRC channels=!IRC_CHANNELS!
set "AUTO_START_DEV=0"
set "AUTO_STOP_FUTON1A=0"
call "%SCRIPT_DIR%\ngircd-bridge-windows.bat" %FUTON_DEV_STACK_BRIDGE_ARGS%
set "DS_EXIT=%ERRORLEVEL%"
exit /b %DS_EXIT%

:wait_for_port
setlocal EnableDelayedExpansion
set "WAIT_PORT=%~1"
set "WAIT_TIMEOUT=%~2"
set "WAIT_NAME=%~3"
set /a WAIT_SECS=0
:wait_for_port_loop
call :port_accepts_local !WAIT_PORT!
if not errorlevel 1 (
  endlocal & exit /b 0
)
if !WAIT_SECS! GEQ !WAIT_TIMEOUT! (
  1>&2 echo [dev-stack-windows] ERROR: timed out waiting for !WAIT_NAME! on port !WAIT_PORT!.
  endlocal & exit /b 1
)
if !WAIT_SECS! EQU 0 echo [dev-stack-windows] Waiting for !WAIT_NAME! on port !WAIT_PORT!...
call :sleep_1s
set /a WAIT_SECS+=1
goto wait_for_port_loop

:port_accepts_local
powershell -NoProfile -Command ^
  "$client = New-Object System.Net.Sockets.TcpClient; try { $iar = $client.BeginConnect('127.0.0.1', %~1, $null, $null); if (-not $iar.AsyncWaitHandle.WaitOne(1000)) { exit 1 }; $client.EndConnect($iar) | Out-Null; exit 0 } catch { exit 1 } finally { $client.Close() }" >nul 2>nul
exit /b %ERRORLEVEL%

:sleep_1s
ping -n 2 127.0.0.1 >nul 2>nul
exit /b 0
