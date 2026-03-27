@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
set "USE_DEV_PROFILE=0"
set "FORWARD_ARGS="

:parse_args
if "%~1"=="" goto args_done
if /i "%~1"=="-dev" (
  set "USE_DEV_PROFILE=1"
) else if /i "%~1"=="--dev" (
  set "USE_DEV_PROFILE=1"
) else (
  if defined FORWARD_ARGS (
    set "FORWARD_ARGS=!FORWARD_ARGS! "%~1""
  ) else (
    set "FORWARD_ARGS="%~1""
  )
)
shift
goto parse_args

:args_done
if not defined MFUTON_HOME (
  1>&2 echo [dev-stack-windows] ERROR: MFUTON_HOME is required.
  1>&2 echo [dev-stack-windows] Set MFUTON_HOME to the mfuton checkout that owns the Windows launch control logic for this wrapper.
  exit /b 1
)

for %%I in ("%MFUTON_HOME%") do set "MFUTON_HOME=%%~fI"
set "MFUTON_CODEX_PY=%MFUTON_HOME%\agent_skills\development\codex-python.bat"
set "MFUTON_LAUNCHER=%MFUTON_HOME%\src\mfuton\development\mfuton_dev_windows.py"
set "DEV_STACK_CONFIG=%MFUTON_HOME%\config\mfuton-dev-stack-windows.json"
if "%USE_DEV_PROFILE%"=="1" (
  set "MFUTON_HTTP_HOST_CONFIG=%MFUTON_HOME%\config\mfuton-http-host-dev.json"
) else (
  set "MFUTON_HTTP_HOST_CONFIG=%MFUTON_HOME%\config\mfuton-http-host.json"
)

if not exist "%MFUTON_CODEX_PY%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %MFUTON_CODEX_PY%.
  exit /b 1
)

if not exist "%MFUTON_LAUNCHER%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %MFUTON_LAUNCHER%.
  exit /b 1
)

if not exist "%DEV_STACK_CONFIG%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %DEV_STACK_CONFIG%.
  exit /b 1
)

if not exist "%MFUTON_HTTP_HOST_CONFIG%" (
  1>&2 echo [dev-stack-windows] ERROR: missing %MFUTON_HTTP_HOST_CONFIG%.
  exit /b 1
)

if "%USE_DEV_PROFILE%"=="1" (
  echo [dev-stack-windows] profile: dev
) else (
  echo [dev-stack-windows] profile: prod
)
echo [dev-stack-windows] http-host-config: %MFUTON_HTTP_HOST_CONFIG%

call "%MFUTON_CODEX_PY%" "%MFUTON_LAUNCHER%" --config "%DEV_STACK_CONFIG%" !FORWARD_ARGS!
exit /b %ERRORLEVEL%
