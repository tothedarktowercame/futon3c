@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

set "TARGET=%~1"
if not defined TARGET (
  set "TARGET=dev"
) else (
  shift
)

set "TARGET_ARGS="
:collect_target_args
if "%~1"=="" goto target_args_done
set "TARGET_ARGS=!TARGET_ARGS! "%~1""
shift
goto collect_target_args
:target_args_done

if /i "%TARGET%"=="tools" (
  call "%SCRIPT_DIR%\bootstrap-tools.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="preflight" (
  call "%SCRIPT_DIR%\preflight-windows.bat"
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="stop-futon1a" (
  call "%SCRIPT_DIR%\stop-futon1a-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="kill-futon1a" (
  call "%SCRIPT_DIR%\stop-futon1a-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="dev" (
  call :run_windows_script "%SCRIPT_DIR%\dev-stack-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="dev-core" (
  call :run_windows_script "%SCRIPT_DIR%\dev-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="test" (
  call :run_windows_script "%SCRIPT_DIR%\test-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="status" (
  call :run_windows_script "%SCRIPT_DIR%\status-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="repl" (
  call :run_windows_script "%SCRIPT_DIR%\repl-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="codex" (
  call :run_windows_script "%SCRIPT_DIR%\codex-picker-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="codex-repl" (
  call :run_windows_script "%SCRIPT_DIR%\codex-repl-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="ngircd-bridge" (
  call :run_windows_script "%SCRIPT_DIR%\ngircd-bridge-windows.bat"!TARGET_ARGS!
  exit /b %ERRORLEVEL%
)

call "%SCRIPT_DIR%\preflight-windows.bat"
if errorlevel 1 exit /b 1

set "MAKE_CMD="
call :find_make MAKE_CMD
if not defined MAKE_CMD (
  1>&2 echo [futon-windows] ERROR: no make command found.
  1>&2 echo Install make/mingw32-make or run scripts\windows\bootstrap-tools.bat first.
  exit /b 1
)

echo [futon-windows] Running target '%TARGET%' via %MAKE_CMD%
pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [futon-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 1
)
if /i "%MAKE_CMD%"=="%REPO_ROOT%\.tools\bin\make.bat" (
  call "%MAKE_CMD%" -f "%REPO_ROOT%\Makefile.windows" %TARGET%!TARGET_ARGS!
) else (
  "%MAKE_CMD%" -f "%REPO_ROOT%\Makefile.windows" %TARGET%!TARGET_ARGS!
)
set "FW_EXIT=%ERRORLEVEL%"
popd
exit /b %FW_EXIT%

:find_make
set "FM_OUTVAR=%~1"
set "FM_PATH="
for /f "delims=" %%P in ('where make.exe 2^>nul') do (
  if not defined FM_PATH set "FM_PATH=%%P"
)
if not defined FM_PATH (
  for /f "delims=" %%P in ('where mingw32-make.exe 2^>nul') do (
    if not defined FM_PATH set "FM_PATH=%%P"
  )
)
if not defined FM_PATH (
  if exist "%REPO_ROOT%\.tools\bin\make.bat" set "FM_PATH=%REPO_ROOT%\.tools\bin\make.bat"
)
set "%FM_OUTVAR%=%FM_PATH%"
exit /b 0

:run_windows_script
setlocal EnableExtensions EnableDelayedExpansion
set "RWS_SCRIPT=%~1"
shift

call "%SCRIPT_DIR%\preflight-windows.bat"
if errorlevel 1 (
  endlocal & exit /b 1
)

set "RWS_ARGS="
:rws_collect
if "%~1"=="" goto rws_exec
set "RWS_CUR=%~1"
if /i "!RWS_CUR:~0,5!"=="ARGS=" (
  set "RWS_CUR=!RWS_CUR:~5!"
)
if defined RWS_CUR set "RWS_ARGS=!RWS_ARGS! !RWS_CUR!"
shift
goto rws_collect

:rws_exec
echo [futon-windows] Running target '%TARGET%' via !RWS_SCRIPT!
call "%RWS_SCRIPT%"!RWS_ARGS!
set "RWS_EXIT=!ERRORLEVEL!"
endlocal & exit /b %RWS_EXIT%
