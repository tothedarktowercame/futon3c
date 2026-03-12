@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "PORT=%~1"
if not defined PORT if defined FUTON1A_PORT set "PORT=%FUTON1A_PORT%"
if not defined PORT set "PORT=7071"
if "%PORT%"=="0" (
  echo [stop-futon1a] Skipping disabled port 0.
  exit /b 0
)

set "FOUND=0"
set "LAST_PID="
set "FAILED=0"

for /f "tokens=5" %%P in ('netstat -ano ^| findstr /R /C:":%PORT% .*LISTENING"') do (
  if not "%%P"=="0" if not "%%P"=="!LAST_PID!" (
    set "FOUND=1"
    echo [stop-futon1a] Stopping PID %%P on port %PORT%...
    taskkill /PID %%P /T /F >nul 2>nul
    if errorlevel 1 (
      call :is_pid_running %%P
      if not errorlevel 1 (
        1>&2 echo [stop-futon1a] ERROR: failed to stop PID %%P.
        set "FAILED=1"
      ) else (
        echo [stop-futon1a] PID %%P already exited.
      )
    ) else (
      echo [stop-futon1a] Stopped PID %%P.
    )
    set "LAST_PID=%%P"
  )
)

if "!FOUND!"=="0" (
  echo [stop-futon1a] No process listening on port %PORT%.
)

call :wait_for_port_closed %PORT% 10
if not errorlevel 1 (
  1>&2 echo [stop-futon1a] ERROR: port %PORT% is still listening after stop attempt.
  set "FAILED=1"
)

exit /b %FAILED%

:is_pid_running
tasklist /FI "PID eq %~1" 2>nul | find "%~1" >nul
if errorlevel 1 (
  exit /b 1
) else (
  exit /b 0
)

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

:wait_for_port_closed
setlocal EnableDelayedExpansion
set "WAIT_PORT=%~1"
set "WAIT_TIMEOUT=%~2"
if not defined WAIT_TIMEOUT set "WAIT_TIMEOUT=10"
set /a WAIT_SECS=0
:wait_for_port_closed_loop
call :is_port_listening !WAIT_PORT!
if errorlevel 1 (
  endlocal & exit /b 1
)
if !WAIT_SECS! GEQ !WAIT_TIMEOUT! (
  endlocal & exit /b 0
)
call :sleep_1s
set /a WAIT_SECS+=1
goto wait_for_port_closed_loop

:sleep_1s
ping -n 2 127.0.0.1 >nul 2>nul
exit /b 0
