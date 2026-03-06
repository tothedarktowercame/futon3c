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

echo [dev-stack-windows] Starting futon runtime lane...
start "futon-dev-core" /b cmd.exe /c call "%SCRIPT_DIR%\dev-windows.bat"

call :wait_for_port %FUTON3C_PORT% 120 futon3c-http
if errorlevel 1 exit /b 1
if "%USE_LOCAL_IRC%"=="1" (
  call :wait_for_port %FUTON3C_IRC_PORT% 120 irc
  if errorlevel 1 exit /b 1
)

echo [dev-stack-windows] Starting ngircd bridge...
echo [dev-stack-windows] Bridge target IRC=%IRC_HOST%:%IRC_PORT% channel=%IRC_CHANNEL% lane=%FUTON3C_IRC_LANE_NORMALIZED%
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
set "WAIT_FOUND="
for /f "delims=" %%L in ('netstat -ano ^| findstr /R /C:":!WAIT_PORT! .*LISTENING"') do (
  if not defined WAIT_FOUND set "WAIT_FOUND=1"
)
if defined WAIT_FOUND (
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

:sleep_1s
ping -n 2 127.0.0.1 >nul 2>nul
exit /b 0
