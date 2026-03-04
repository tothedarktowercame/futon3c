@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon4\dev\web") do set "ARXANA_STATIC_DIR=%%~fI"

if not defined FUTON1A_STATIC_DIR (
  set "FUTON1A_STATIC_DIR=%ARXANA_STATIC_DIR%"
)

if exist "%FUTON1A_STATIC_DIR%\evidence-viewer\index.html" (
  echo [dev-stack-arxana-windows] FUTON1A_STATIC_DIR=%FUTON1A_STATIC_DIR%
) else (
  1>&2 echo [dev-stack-arxana-windows] ERROR: missing evidence-viewer assets under %FUTON1A_STATIC_DIR%
  1>&2 echo [dev-stack-arxana-windows] Expected: ^<static-dir^>\evidence-viewer\index.html
  exit /b 1
)

call "%SCRIPT_DIR%\dev-stack-windows.bat" %*
exit /b %ERRORLEVEL%
