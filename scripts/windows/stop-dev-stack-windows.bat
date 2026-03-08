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
call :stop_discovered_bridges "%IRC_CHANNEL%"
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
set "BRIDGE_PID="
set "BRIDGE_PID_SOURCE="
set "BRIDGE_MARKERS_UNCERTAIN=0"
set "BRIDGE_IRC_PORT=%IRC_PORT%"
if not defined BRIDGE_IRC_PORT set "BRIDGE_IRC_PORT=%FUTON3C_IRC_PORT%"
if not defined BRIDGE_IRC_PORT set "BRIDGE_IRC_PORT=6667"

if exist "!PID_FILE!" (
  set /p BRIDGE_PID=<"!PID_FILE!"
  for /f "tokens=1" %%P in ("!BRIDGE_PID!") do set "BRIDGE_PID=%%P"
  if defined BRIDGE_PID (
    call :is_numeric_pid !BRIDGE_PID!
    if errorlevel 1 (
      echo [stop-dev-stack] WARN: bridge pid file is stale for !BRIDGE_CHANNEL!: invalid PID "!BRIDGE_PID!".
      set "BRIDGE_PID="
    ) else (
      call :classify_bridge_pid !BRIDGE_PID!
      set "PID_CHECK_RC=!ERRORLEVEL!"
      if "!PID_CHECK_RC!"=="0" (
        set "BRIDGE_PID_SOURCE=pid-file"
      ) else if "!PID_CHECK_RC!"=="2" (
        echo [stop-dev-stack] Bridge pid file is stale for !BRIDGE_CHANNEL!: !BRIDGE_PID! not running.
        set "BRIDGE_PID="
      ) else if "!PID_CHECK_RC!"=="3" (
        1>&2 echo [stop-dev-stack] ERROR: refusing to kill PID !BRIDGE_PID! because it is not an ngircd bridge process.
        set "BRIDGE_FAILED=1"
        set "BRIDGE_PID="
      ) else (
        1>&2 echo [stop-dev-stack] ERROR: unable to verify bridge PID !BRIDGE_PID! for !BRIDGE_CHANNEL!.
        set "BRIDGE_FAILED=1"
        set "BRIDGE_PID="
      )
    )
  ) else (
    echo [stop-dev-stack] WARN: bridge pid file is stale for !BRIDGE_CHANNEL!: !PID_FILE! is empty.
  )
) else (
  echo [stop-dev-stack] No bridge pid file for !BRIDGE_CHANNEL!.
)
if not defined BRIDGE_PID if "!BRIDGE_FAILED!"=="0" (
  call :bridge_pid_from_health "!HEALTH_FILE!"
  if defined BRIDGE_HEALTH_PID (
    echo [stop-dev-stack] Falling back to health pid !BRIDGE_HEALTH_PID! for !BRIDGE_CHANNEL!.
    set "BRIDGE_PID=!BRIDGE_HEALTH_PID!"
    set "BRIDGE_PID_SOURCE=health-file"
  )
)
if not defined BRIDGE_PID if "!BRIDGE_FAILED!"=="0" (
  call :bridge_pid_from_socket "!BRIDGE_IRC_PORT!"
  if defined BRIDGE_SOCKET_PID (
    echo [stop-dev-stack] Falling back to IRC socket pid !BRIDGE_SOCKET_PID! for !BRIDGE_CHANNEL!.
    set "BRIDGE_PID=!BRIDGE_SOCKET_PID!"
    set "BRIDGE_PID_SOURCE=irc-socket"
  ) else if exist "!PID_FILE!" (
    echo [stop-dev-stack] WARN: bridge pid unresolved for !BRIDGE_CHANNEL!; leaving markers in place.
    set "BRIDGE_MARKERS_UNCERTAIN=1"
  )
)
if defined BRIDGE_PID if "!BRIDGE_FAILED!"=="0" (
  call :classify_bridge_pid !BRIDGE_PID!
  set "PID_CHECK_RC=!ERRORLEVEL!"
  if "!PID_CHECK_RC!"=="0" (
    echo [stop-dev-stack] Stopping bridge PID !BRIDGE_PID! for !BRIDGE_CHANNEL! ^(!BRIDGE_PID_SOURCE!^)...
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
    echo [stop-dev-stack] Bridge PID !BRIDGE_PID! from !BRIDGE_PID_SOURCE! is not running.
  ) else if "!PID_CHECK_RC!"=="3" (
    1>&2 echo [stop-dev-stack] ERROR: refusing to kill PID !BRIDGE_PID! from !BRIDGE_PID_SOURCE! because it is not an ngircd bridge process.
    set "BRIDGE_FAILED=1"
  ) else (
    1>&2 echo [stop-dev-stack] ERROR: unable to verify bridge PID !BRIDGE_PID! from !BRIDGE_PID_SOURCE!.
    set "BRIDGE_FAILED=1"
  )
)
if "!BRIDGE_FAILED!"=="0" if "!BRIDGE_MARKERS_UNCERTAIN!"=="0" (
  if exist "!PID_FILE!" del /q "!PID_FILE!" >nul 2>nul
  if exist "!HEALTH_FILE!" del /q "!HEALTH_FILE!" >nul 2>nul
)
endlocal & exit /b %BRIDGE_FAILED%

:stop_discovered_bridges
setlocal EnableDelayedExpansion
set "PRIMARY_CHANNEL=%~1"
if not defined PRIMARY_CHANNEL set "PRIMARY_CHANNEL=#futon"
set "PRIMARY_SLUG=!PRIMARY_CHANNEL:#=!"
set "PRIMARY_SLUG=!PRIMARY_SLUG:/=_!"
set "DISCOVERY_FAILED=0"
set "RUNTIME_DIR=%XDG_RUNTIME_DIR%"
if not defined RUNTIME_DIR set "RUNTIME_DIR=%TEMP%"
for /f "usebackq delims=" %%L in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$dir = $env:XDG_RUNTIME_DIR; if (-not $dir) { $dir = $env:TEMP }; if (Test-Path -LiteralPath $dir) { Get-ChildItem -LiteralPath $dir -Filter 'ngircd-bridge-*' -ErrorAction SilentlyContinue | ForEach-Object { if ($_.Name -match '^ngircd-bridge-(.+?)(?:-health\.json|\.pid)$') { $matches[1] } } | Sort-Object -Unique }" 2^>nul`) do (
  set "DISCOVERED_SLUG=%%L"
  if defined DISCOVERED_SLUG if /i not "!DISCOVERED_SLUG!"=="!PRIMARY_SLUG!" (
    echo [stop-dev-stack] Discovered additional bridge channel #!DISCOVERED_SLUG! for cleanup.
    call :stop_bridge "#!DISCOVERED_SLUG!"
    if errorlevel 1 set "DISCOVERY_FAILED=1"
  )
)
endlocal & exit /b %DISCOVERY_FAILED%

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
set "BRIDGE_IMAGE="
for /f "usebackq delims=" %%L in (`powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$p = Get-Process -Id %~1 -ErrorAction SilentlyContinue; if (-not $p) { 'NOTRUNNING' } else { $p.ProcessName }" 2^>nul`) do (
  if not defined BRIDGE_IMAGE set "BRIDGE_IMAGE=%%~L"
)
if not defined BRIDGE_IMAGE endlocal & exit /b 4
if /i "!BRIDGE_IMAGE!"=="NOTRUNNING" endlocal & exit /b 2
if /i "!BRIDGE_IMAGE!"=="python" endlocal & exit /b 0
if /i "!BRIDGE_IMAGE!"=="pythonw" endlocal & exit /b 0
if /i "!BRIDGE_IMAGE!"=="python.exe" endlocal & exit /b 0
if /i "!BRIDGE_IMAGE!"=="pythonw.exe" endlocal & exit /b 0
endlocal & exit /b 3

:bridge_pid_from_socket
setlocal EnableDelayedExpansion
set "SOCKET_PORT=%~1"
set "BRIDGE_SOCKET_PID="
set "BRIDGE_SOCKET_AMBIGUOUS=0"
if not defined SOCKET_PORT endlocal & set "BRIDGE_SOCKET_PID=" & exit /b 0
if "!SOCKET_PORT!"=="0" endlocal & set "BRIDGE_SOCKET_PID=" & exit /b 0
for /f "tokens=2,3,4,5" %%A in ('netstat -ano ^| findstr /R /C:":!SOCKET_PORT! .*ESTABLISHED" 2^>nul') do (
  set "SOCK_LOCAL=%%A"
  set "SOCK_FOREIGN=%%B"
  set "SOCK_STATE=%%C"
  set "SOCK_PID=%%D"
  if /i "!SOCK_STATE!"=="ESTABLISHED" (
    echo(!SOCK_FOREIGN!| findstr /R /C:":!SOCKET_PORT!$" >nul
    if not errorlevel 1 (
      echo(!SOCK_LOCAL!| findstr /R /C:":!SOCKET_PORT!$" >nul
      if errorlevel 1 (
        call :classify_bridge_pid !SOCK_PID!
        if not errorlevel 1 (
          if not defined BRIDGE_SOCKET_PID (
            set "BRIDGE_SOCKET_PID=!SOCK_PID!"
          ) else if not "!BRIDGE_SOCKET_PID!"=="!SOCK_PID!" (
            set "BRIDGE_SOCKET_AMBIGUOUS=1"
          )
        )
      )
    )
  )
)
if "!BRIDGE_SOCKET_AMBIGUOUS!"=="1" set "BRIDGE_SOCKET_PID="
endlocal & set "BRIDGE_SOCKET_PID=%BRIDGE_SOCKET_PID%" & exit /b 0

:bridge_pid_from_health
setlocal EnableDelayedExpansion
set "HEALTH_PATH=%~1"
set "BRIDGE_HEALTH_PID="
if exist "!HEALTH_PATH!" (
  for /f "usebackq delims=" %%L in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$path = '%~1'; if (Test-Path -LiteralPath $path) { try { $raw = Get-Content -LiteralPath $path -Raw -ErrorAction Stop; if ($raw) { $obj = $raw | ConvertFrom-Json -ErrorAction Stop; if ($obj.pid) { [string]$obj.pid } } } catch { } }" 2^>nul`) do (
    if not defined BRIDGE_HEALTH_PID set "BRIDGE_HEALTH_PID=%%L"
  )
)
endlocal & set "BRIDGE_HEALTH_PID=%BRIDGE_HEALTH_PID%" & exit /b 0
