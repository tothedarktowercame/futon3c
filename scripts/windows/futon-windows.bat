@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

set "TARGET=%~1"
if not defined TARGET set "TARGET=dev"
if defined TARGET shift

if /i "%TARGET%"=="tools" (
  call "%SCRIPT_DIR%\bootstrap-tools.bat" %*
  exit /b %ERRORLEVEL%
)

if /i "%TARGET%"=="preflight" (
  call "%SCRIPT_DIR%\preflight-windows.bat"
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
  call "%MAKE_CMD%" -f "%REPO_ROOT%\Makefile.windows" %TARGET% %*
) else (
  "%MAKE_CMD%" -f "%REPO_ROOT%\Makefile.windows" %TARGET% %*
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
