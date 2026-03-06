@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

if not defined FUTON1A_PORT set "FUTON1A_PORT=7071"
if not defined FUTON3C_PORT set "FUTON3C_PORT=7070"
if not defined FUTON3C_IRC_PORT set "FUTON3C_IRC_PORT=6667"
if not defined FUTON3C_DRAWBRIDGE_PORT set "FUTON3C_DRAWBRIDGE_PORT=6768"
if not defined IRC_CHANNEL set "IRC_CHANNEL=#futon"
set "STACK_STOP_FAILED=0"

call :stop_bridge "%IRC_CHANNEL%"
if errorlevel 1 set "STACK_STOP_FAILED=1"
if not "%FUTON1A_PORT%"=="0" (
  call "%SCRIPT_DIR%\stop-futon1a-windows.bat" %FUTON1A_PORT%
  if errorlevel 1 set "STACK_STOP_FAILED=1"
) else (
  echo [stop-dev-stack] Skipping futon1a stop for disabled port 0.
)
call :kill_port %FUTON3C_PORT% futon3c-http
if errorlevel 1 set "STACK_STOP_FAILED=1"
if not "%FUTON3C_IRC_PORT%"=="0" (
  call :kill_port %FUTON3C_IRC_PORT% irc
  if errorlevel 1 set "STACK_STOP_FAILED=1"
)
if not "%FUTON3C_DRAWBRIDGE_PORT%"=="0" (
  call :kill_port %FUTON3C_DRAWBRIDGE_PORT% drawbridge
  if errorlevel 1 set "STACK_STOP_FAILED=1"
)
exit /b %STACK_STOP_FAILED%

:stop_bridge
setlocal EnableDelayedExpansion
set "BRIDGE_CHANNEL=%~1"
if not defined BRIDGE_CHANNEL set "BRIDGE_CHANNEL=#futon"
set "RUNTIME_DIR=%XDG_RUNTIME_DIR%"
if not defined RUNTIME_DIR set "RUNTIME_DIR=%TEMP%"
set "CHANNEL_SLUG=!BRIDGE_CHANNEL:#=!"
set "CHANNEL_SLUG=!CHANNEL_SLUG:/=_!"
set "PID_FILE=!RUNTIME_DIR!\ngircd-bridge-!CHANNEL_SLUG!.pid"
set "HEALTH_FILE=!RUNTIME_DIR!\ngircd-bridge-!CHANNEL_SLUG!-health.json"
set "BRIDGE_FAILED=0"

if exist "!PID_FILE!" (
  set "BRIDGE_PID="
  set /p BRIDGE_PID=<"!PID_FILE!"
  for /f "tokens=1" %%P in ("!BRIDGE_PID!") do set "BRIDGE_PID=%%P"
  if defined BRIDGE_PID (
    call :is_numeric_pid !BRIDGE_PID!
    if errorlevel 1 (
      echo [stop-dev-stack] WARN: bridge pid file is stale for !BRIDGE_CHANNEL!: invalid PID "!BRIDGE_PID!".
    ) else (
      call :classify_bridge_pid !BRIDGE_PID!
      set "PID_CHECK_RC=!ERRORLEVEL!"
      if "!PID_CHECK_RC!"=="0" (
        echo [stop-dev-stack] Stopping bridge PID !BRIDGE_PID! for !BRIDGE_CHANNEL!...
        taskkill /PID !BRIDGE_PID! /T /F >nul 2>nul
        if errorlevel 1 (
          call :is_pid_running !BRIDGE_PID!
          if not errorlevel 1 (
            1>&2 echo [stop-dev-stack] ERROR: failed to stop bridge PID !BRIDGE_PID!.
            set "BRIDGE_FAILED=1"
          ) else (
            echo [stop-dev-stack] Bridge PID !BRIDGE_PID! already exited.
          )
        ) else (
          echo [stop-dev-stack] Stopped bridge PID !BRIDGE_PID!.
        )
      ) else if "!PID_CHECK_RC!"=="2" (
        echo [stop-dev-stack] Bridge pid file is stale for !BRIDGE_CHANNEL!: !BRIDGE_PID! not running.
      ) else if "!PID_CHECK_RC!"=="3" (
        1>&2 echo [stop-dev-stack] ERROR: refusing to kill PID !BRIDGE_PID! because it is not an ngircd bridge process.
        set "BRIDGE_FAILED=1"
      ) else (
        1>&2 echo [stop-dev-stack] ERROR: unable to verify bridge PID !BRIDGE_PID! for !BRIDGE_CHANNEL!.
        set "BRIDGE_FAILED=1"
      )
    )
  ) else (
    echo [stop-dev-stack] WARN: bridge pid file is stale for !BRIDGE_CHANNEL!: !PID_FILE! is empty.
  )
) else (
  echo [stop-dev-stack] No bridge pid file for !BRIDGE_CHANNEL!.
)
if "!BRIDGE_FAILED!"=="0" (
  if exist "!PID_FILE!" del /q "!PID_FILE!" >nul 2>nul
  if exist "!HEALTH_FILE!" del /q "!HEALTH_FILE!" >nul 2>nul
)
endlocal & exit /b %BRIDGE_FAILED%

:kill_port
setlocal EnableDelayedExpansion
set "KILL_PORT=%~1"
set "KILL_NAME=%~2"
if "!KILL_PORT!"=="0" (
  echo [stop-dev-stack] Skipping !KILL_NAME! cleanup for disabled port 0.
  endlocal & exit /b 0
)
set "FOUND=0"
set "LAST_PID="
set "FAILED=0"
for /f "tokens=5" %%P in ('netstat -ano ^| findstr /R /C:":!KILL_PORT! .*LISTENING"') do (
  if not "%%P"=="0" if not "%%P"=="!LAST_PID!" (
    set "FOUND=1"
    echo [stop-dev-stack] Stopping !KILL_NAME! PID %%P on port !KILL_PORT!...
    taskkill /PID %%P /T /F >nul 2>nul
    if errorlevel 1 (
      call :is_pid_running %%P
      if not errorlevel 1 (
        1>&2 echo [stop-dev-stack] ERROR: failed to stop PID %%P.
        set "FAILED=1"
      ) else (
        echo [stop-dev-stack] PID %%P already exited.
      )
    ) else (
      echo [stop-dev-stack] Stopped PID %%P.
    )
    set "LAST_PID=%%P"
  )
)
if "!FOUND!"=="0" (
  echo [stop-dev-stack] No listener on port !KILL_PORT!.
)
call :is_port_listening !KILL_PORT!
if not errorlevel 1 (
  1>&2 echo [stop-dev-stack] ERROR: !KILL_NAME! is still listening on port !KILL_PORT!.
  set "FAILED=1"
)
endlocal & exit /b %FAILED%

:is_pid_running
tasklist /FI "PID eq %~1" 2>nul | find "%~1" >nul
if errorlevel 1 (
  exit /b 1
) else (
  exit /b 0
)

:is_numeric_pid
setlocal EnableDelayedExpansion
set "PID_VALUE=%~1"
if not defined PID_VALUE endlocal & exit /b 1
for /f "delims=0123456789" %%D in ("!PID_VALUE!") do set "PID_NON_DIGITS=%%D"
if defined PID_NON_DIGITS (
  endlocal & exit /b 1
) else (
  endlocal & exit /b 0
)

:is_port_listening
setlocal EnableDelayedExpansion
set "PORT_TO_CHECK=%~1"
set "PORT_LISTENING="
for /f "delims=" %%L in ('netstat -ano ^| findstr /R /C:":!PORT_TO_CHECK! .*LISTENING"') do (
  if not defined PORT_LISTENING set "PORT_LISTENING=1"
)
if defined PORT_LISTENING (
  endlocal & exit /b 0
) else (
  endlocal & exit /b 1
)

:classify_bridge_pid
setlocal EnableDelayedExpansion
set "BRIDGE_PID_TO_CHECK=%~1"
set "BRIDGE_PID_STATE="
for /f "usebackq delims=" %%L in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$p = Get-CimInstance Win32_Process -Filter \"ProcessId=$env:BRIDGE_PID_TO_CHECK\" -ErrorAction SilentlyContinue; if (-not $p) { 'NOTRUNNING' } elseif ((([string]$p.Name) -match '^pythonw?\\.exe$') -and ((([string]$p.CommandLine) -match 'ngircd_bridge\\.py'))) { 'MATCH' } else { 'MISMATCH' }" 2^>nul`) do (
  if not defined BRIDGE_PID_STATE set "BRIDGE_PID_STATE=%%L"
)
if not defined BRIDGE_PID_STATE set "BRIDGE_PID_STATE=VERIFYERROR"
if /i "!BRIDGE_PID_STATE!"=="MATCH" endlocal & exit /b 0
if /i "!BRIDGE_PID_STATE!"=="NOTRUNNING" endlocal & exit /b 2
if /i "!BRIDGE_PID_STATE!"=="MISMATCH" endlocal & exit /b 3
endlocal & exit /b 4
