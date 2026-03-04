@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "PORT=%~1"
if not defined PORT if defined FUTON1A_PORT set "PORT=%FUTON1A_PORT%"
if not defined PORT set "PORT=7071"

set "FOUND=0"
set "LAST_PID="

for /f "tokens=5" %%P in ('netstat -ano ^| findstr /R /C:":%PORT% .*LISTENING"') do (
  if not "%%P"=="0" if not "%%P"=="!LAST_PID!" (
    set "FOUND=1"
    echo [stop-futon1a] Stopping PID %%P on port %PORT%...
    taskkill /PID %%P /T /F >nul 2>nul
    if errorlevel 1 (
      1>&2 echo [stop-futon1a] WARN: failed to stop PID %%P.
    ) else (
      echo [stop-futon1a] Stopped PID %%P.
    )
    set "LAST_PID=%%P"
  )
)

if "%FOUND%"=="0" (
  echo [stop-futon1a] No process listening on port %PORT%.
)

exit /b 0
