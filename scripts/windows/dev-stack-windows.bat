@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon4\dev\web") do set "DEFAULT_FUTON1A_STATIC_DIR=%%~fI"
for %%I in ("%REPO_ROOT%\..\..\gh\mfuton") do set "LOCAL_INSTALLATION_ROOT=%%~fI"

if not defined FUTON1A_PORT set "FUTON1A_PORT=7071"
if not defined FUTON3C_PORT set "FUTON3C_PORT=7070"
if not defined FUTON3C_IRC_PORT set "FUTON3C_IRC_PORT=6667"
if not defined FUTON3C_DRAWBRIDGE_PORT set "FUTON3C_DRAWBRIDGE_PORT=6768"
if not defined BRIDGE_BOTS set "BRIDGE_BOTS=codex"

if not defined FUTON3C_REPOS (
  if exist "%LOCAL_INSTALLATION_ROOT%\holes\missions" (
    set "FUTON3C_REPOS=installation=%LOCAL_INSTALLATION_ROOT%"
    echo [dev-stack-windows] FUTON3C_REPOS injected from local installation root: %LOCAL_INSTALLATION_ROOT%
  )
)

if not defined FUTON1A_STATIC_DIR (
  if exist "%DEFAULT_FUTON1A_STATIC_DIR%\evidence-viewer\index.html" (
    set "FUTON1A_STATIC_DIR=%DEFAULT_FUTON1A_STATIC_DIR%"
    echo [dev-stack-windows] FUTON1A_STATIC_DIR defaulted to %FUTON1A_STATIC_DIR%
  ) else (
    echo [dev-stack-windows] WARN: FUTON1A_STATIC_DIR unset and default missing: %DEFAULT_FUTON1A_STATIC_DIR%
  )
) else (
  echo [dev-stack-windows] FUTON1A_STATIC_DIR preset: %FUTON1A_STATIC_DIR%
)

set "BRIDGE_BOTS_NORMALIZED=%BRIDGE_BOTS: =%"
if /i "%BRIDGE_BOTS_NORMALIZED%"=="codex" (
  if not defined FUTON3C_REGISTER_CLAUDE set "FUTON3C_REGISTER_CLAUDE=false"
  if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
  echo [dev-stack-windows] codex-only mode: FUTON3C_REGISTER_CLAUDE=%FUTON3C_REGISTER_CLAUDE% FUTON3C_RELAY_CLAUDE=%FUTON3C_RELAY_CLAUDE%
)

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
