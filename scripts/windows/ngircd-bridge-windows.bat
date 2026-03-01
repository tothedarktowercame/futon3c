@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

set "BRIDGE_SCRIPT=%REPO_ROOT%\scripts\ngircd_bridge.py"
if not exist "%BRIDGE_SCRIPT%" (
  1>&2 echo [ngircd-bridge-windows] ERROR: missing script %BRIDGE_SCRIPT%
  exit /b 1
)

set "PYTHON_EXE="
for /f "delims=" %%P in ('where python.exe 2^>nul') do (
  if not defined PYTHON_EXE set "PYTHON_EXE=%%P"
)
if not defined PYTHON_EXE (
  1>&2 echo [ngircd-bridge-windows] ERROR: python.exe not found on PATH.
  1>&2 echo Install Python or add it to PATH, then retry.
  exit /b 1
)

if not defined IRC_HOST set "IRC_HOST=127.0.0.1"
if not defined IRC_PORT set "IRC_PORT=6667"
if not defined IRC_CHANNEL set "IRC_CHANNEL=#futon"
if not defined INVOKE_BASE set "INVOKE_BASE=http://127.0.0.1:7070"
if not defined BRIDGE_BOTS set "BRIDGE_BOTS=claude,codex"
if not defined AUTO_START_DEV set "AUTO_START_DEV=1"

set "BRIDGE_BOTS_NORMALIZED=%BRIDGE_BOTS: =%"
if /i "%BRIDGE_BOTS_NORMALIZED%"=="codex" (
  if not defined FUTON3C_REGISTER_CLAUDE set "FUTON3C_REGISTER_CLAUDE=false"
  if not defined FUTON3C_RELAY_CLAUDE set "FUTON3C_RELAY_CLAUDE=false"
)

set "IRC_HOST_LOWER=%IRC_HOST%"
for %%L in ("A=a" "B=b" "C=c" "D=d" "E=e" "F=f" "G=g" "H=h" "I=i" "J=j" "K=k" "L=l" "M=m" "N=n" "O=o" "P=p" "Q=q" "R=r" "S=s" "T=t" "U=u" "V=v" "W=w" "X=x" "Y=y" "Z=z") do set "IRC_HOST_LOWER=!IRC_HOST_LOWER:%%~L!"
if "%IRC_HOST_LOWER%"=="127.0.0.1" goto check_local_irc
if "%IRC_HOST_LOWER%"=="localhost" goto check_local_irc
goto after_local_irc_check

:check_local_irc
call :is_port_listening %IRC_PORT%
if not errorlevel 1 goto after_local_irc_check

if /i not "%AUTO_START_DEV%"=="1" (
  1>&2 echo [ngircd-bridge-windows] ERROR: no IRC server listening on %IRC_HOST%:%IRC_PORT%.
  1>&2 echo [ngircd-bridge-windows] Set AUTO_START_DEV=1 to auto-start futon dev.
  exit /b 1
)

echo [ngircd-bridge-windows] No IRC listener on %IRC_HOST%:%IRC_PORT%; auto-starting futon dev in this console...
start "futon-dev" /b cmd /c ""%SCRIPT_DIR%\futon-windows.bat" dev 1>CON 2>&1"
set /a WAIT_SECS=0
:wait_for_irc
call :is_port_listening %IRC_PORT%
if not errorlevel 1 goto after_local_irc_check
if !WAIT_SECS! GEQ 120 (
  1>&2 echo [ngircd-bridge-windows] ERROR: timed out waiting for IRC listener on %IRC_HOST%:%IRC_PORT%.
  1>&2 echo [ngircd-bridge-windows] Check the dev logs above for startup failures.
  exit /b 1
)
if !WAIT_SECS! EQU 0 echo [ngircd-bridge-windows] Waiting for futon dev to open IRC port %IRC_PORT%...
call :sleep_1s
set /a WAIT_SECS+=1
goto wait_for_irc

:after_local_irc_check

echo [ngircd-bridge-windows] Starting ngircd bridge
echo [ngircd-bridge-windows] IRC=%IRC_HOST%:%IRC_PORT% channel=%IRC_CHANNEL%
echo [ngircd-bridge-windows] invoke=%INVOKE_BASE% bots=%BRIDGE_BOTS%
if defined FUTON3C_REGISTER_CLAUDE echo [ngircd-bridge-windows] FUTON3C_REGISTER_CLAUDE=%FUTON3C_REGISTER_CLAUDE%
if defined FUTON3C_RELAY_CLAUDE echo [ngircd-bridge-windows] FUTON3C_RELAY_CLAUDE=%FUTON3C_RELAY_CLAUDE%
echo [ngircd-bridge-windows] python=%PYTHON_EXE%

pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [ngircd-bridge-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 1
)

"%PYTHON_EXE%" "%BRIDGE_SCRIPT%" %*
set "NB_EXIT=%ERRORLEVEL%"
popd
exit /b %NB_EXIT%

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

:sleep_1s
ping -n 2 127.0.0.1 >nul 2>nul
exit /b 0
