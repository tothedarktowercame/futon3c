@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

if not defined FUTON1A_PORT set "FUTON1A_PORT=7071"
if not defined FUTON3C_PORT set "FUTON3C_PORT=7070"
if not defined FUTON3C_IRC_PORT set "FUTON3C_IRC_PORT=6667"
if not defined FUTON3C_DRAWBRIDGE_PORT set "FUTON3C_DRAWBRIDGE_PORT=6768"

echo [dev-stack-windows] Preparing clean runtime start...
call "%SCRIPT_DIR%\stop-futon1a-windows.bat" %FUTON1A_PORT%
call :kill_port %FUTON3C_PORT% futon3c-http
call :kill_port %FUTON3C_IRC_PORT% irc
if not "%FUTON3C_DRAWBRIDGE_PORT%"=="0" (
  call :kill_port %FUTON3C_DRAWBRIDGE_PORT% drawbridge
)

echo [dev-stack-windows] Starting futon runtime lane...
start "futon-dev-core" /b cmd.exe /c call "%SCRIPT_DIR%\dev-windows.bat"

call :wait_for_port %FUTON3C_PORT% 120 futon3c-http
if errorlevel 1 exit /b 1
call :wait_for_port %FUTON3C_IRC_PORT% 120 irc
if errorlevel 1 exit /b 1

echo [dev-stack-windows] Starting ngircd bridge...
set "AUTO_START_DEV=0"
set "AUTO_STOP_FUTON1A=0"
call "%SCRIPT_DIR%\ngircd-bridge-windows.bat" %*
set "DS_EXIT=%ERRORLEVEL%"
exit /b %DS_EXIT%

:is_port_listening
set "PORT_LISTENING="
for /f "delims=" %%L in ('netstat -ano ^| findstr /R /C:":%~1 .*LISTENING"') do (
  if not defined PORT_LISTENING set "PORT_LISTENING=1"
)
if defined PORT_LISTENING (
  exit /b 0
) else (
  exit /b 1
)

:wait_for_port
setlocal EnableDelayedExpansion
set "WAIT_PORT=%~1"
set "WAIT_TIMEOUT=%~2"
set "WAIT_NAME=%~3"
set /a WAIT_SECS=0
:wait_for_port_loop
call :is_port_listening !WAIT_PORT!
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

:kill_port
setlocal EnableDelayedExpansion
set "KILL_PORT=%~1"
set "KILL_NAME=%~2"
set "FOUND=0"
set "LAST_PID="
for /f "tokens=5" %%P in ('netstat -ano ^| findstr /R /C:":!KILL_PORT! .*LISTENING"') do (
  if not "%%P"=="0" if not "%%P"=="!LAST_PID!" (
    set "FOUND=1"
    echo [dev-stack-windows] Stopping !KILL_NAME! PID %%P on port !KILL_PORT!...
    taskkill /PID %%P /T /F >nul 2>nul
    if errorlevel 1 (
      1>&2 echo [dev-stack-windows] WARN: failed to stop PID %%P.
    ) else (
      echo [dev-stack-windows] Stopped PID %%P.
    )
    set "LAST_PID=%%P"
  )
)
if "!FOUND!"=="0" (
  echo [dev-stack-windows] No listener on port !KILL_PORT!.
)
endlocal & exit /b 0

:sleep_1s
ping -n 2 127.0.0.1 >nul 2>nul
exit /b 0
