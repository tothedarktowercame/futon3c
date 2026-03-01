@echo off
setlocal EnableExtensions DisableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

set "REL_SCRIPT=%~1"
if not defined REL_SCRIPT (
  1>&2 echo [run-bash-script-windows] ERROR: missing relative script path argument.
  exit /b 1
)
shift
set "FORWARD_ARGS="
:collect_forward_args
if "%~1"=="" goto forward_args_done
set "FORWARD_ARGS=%FORWARD_ARGS% "%~1""
shift
goto collect_forward_args
:forward_args_done

set "SCRIPT_PATH=%REL_SCRIPT:/=\%"
set "TARGET_SCRIPT=%REPO_ROOT%\%SCRIPT_PATH%"
if not exist "%TARGET_SCRIPT%" (
  1>&2 echo [run-bash-script-windows] ERROR: script not found: %TARGET_SCRIPT%
  exit /b 1
)

set "BASH_EXE="
call :find_msys_bash BASH_EXE
if not defined BASH_EXE (
  1>&2 echo [run-bash-script-windows] ERROR: no bash.exe found.
  1>&2 echo Install MSYS2 bash or add bash.exe to PATH.
  exit /b 1
)

set "BASH_DIR="
for %%I in ("%BASH_EXE%") do set "BASH_DIR=%%~dpI"
if defined BASH_DIR if "%BASH_DIR:~-1%"=="\" set "BASH_DIR=%BASH_DIR:~0,-1%"

set "MSYS_ROOT="
if defined BASH_DIR (
  for %%I in ("%BASH_DIR%\..\..") do set "MSYS_ROOT=%%~fI"
)

set "MSYS_UCRT64_BIN="
if defined MSYS_ROOT if exist "%MSYS_ROOT%\ucrt64\bin" set "MSYS_UCRT64_BIN=%MSYS_ROOT%\ucrt64\bin"
set "MSYS_BIN_FALLBACK="
if defined MSYS_ROOT if exist "%MSYS_ROOT%\bin" set "MSYS_BIN_FALLBACK=%MSYS_ROOT%\bin"

set "MSYS2_PATH_TYPE=inherit"
set "CHERE_INVOKING=1"
if not defined HOME if defined USERPROFILE set "HOME=%USERPROFILE%"
if not defined TMPDIR if defined LOCALAPPDATA set "TMPDIR=%LOCALAPPDATA%\Temp"
if defined TMPDIR if not exist "%TMPDIR%" mkdir "%TMPDIR%" >nul 2>nul
set "PATH=%REPO_ROOT%\.tools\bin;%BASH_DIR%;%MSYS_UCRT64_BIN%;%MSYS_BIN_FALLBACK%;%PATH%"
pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-bash-script-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 1
)

"%BASH_EXE%" "./%REL_SCRIPT%" %FORWARD_ARGS%
set "RB_EXIT=%ERRORLEVEL%"
popd
exit /b %RB_EXIT%

:find_msys_bash
set "BASH_OUTVAR=%~1"
set "BASH_FOUND="
for %%P in (
  "C:\msys64\usr\bin\bash.exe"
  "%ProgramFiles%\msys64\usr\bin\bash.exe"
  "%ProgramFiles(x86)%\msys64\usr\bin\bash.exe"
) do (
  if exist "%%~fP" if not defined BASH_FOUND set "BASH_FOUND=%%~fP"
)
if not defined BASH_FOUND (
  for /f "delims=" %%P in ('where bash.exe 2^>nul') do (
    if not defined BASH_FOUND set "BASH_FOUND=%%P"
  )
)
set "%BASH_OUTVAR%=%BASH_FOUND%"
exit /b 0
