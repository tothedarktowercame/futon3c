@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

set "FAIL_COUNT=0"

echo [preflight] futon3c Windows-native hybrid preflight
echo [preflight] repo: %REPO_ROOT%

call :check_file "%REPO_ROOT%\deps.edn" "futon3c deps.edn"
call :check_dir "%REPO_ROOT%\..\futon1a" "futon1a local root ..\\futon1a"
call :check_dir "%REPO_ROOT%\..\futon3b" "futon3b local root ..\\futon3b"

call :check_file "%REPO_ROOT%\.tools\bin\bb.exe" "repo-local bb.exe"
call :check_file "%REPO_ROOT%\.tools\clojure\bin\clojure.bat" "repo-local clojure.bat"
call :check_file "%REPO_ROOT%\.tools\bin\gh.exe" "repo-local gh.exe"
call :check_file "%REPO_ROOT%\.tools\bin\jq.exe" "repo-local jq.exe"
call :check_file "%REPO_ROOT%\.tools\bin\emacsclient.exe" "repo-local emacsclient.exe"

call :check_any_make
call :check_msys_bash
call :check_cmd curl.exe "curl"
call :check_cmd git.exe "git"
call :check_cmd java.exe "java"
call :check_cmd python.exe "python"

if "%FAIL_COUNT%"=="0" (
  echo [preflight] PASS
  exit /b 0
)

echo [preflight] FAIL ^(%FAIL_COUNT% missing requirement^)
exit /b 1

:check_any_make
set "MAKE_CANDIDATE="
call :find_cmd make.exe MAKE_CANDIDATE
if defined MAKE_CANDIDATE (
  echo [preflight] OK   make ^(!MAKE_CANDIDATE!^)
  exit /b 0
)
call :find_cmd mingw32-make.exe MAKE_CANDIDATE
if defined MAKE_CANDIDATE (
  echo [preflight] OK   mingw32-make ^(!MAKE_CANDIDATE!^)
  exit /b 0
)
if exist "%REPO_ROOT%\.tools\bin\make.bat" (
  echo [preflight] OK   make shim ^(%REPO_ROOT%\.tools\bin\make.bat^)
  exit /b 0
)
echo [preflight] MISS make ^(expected make.exe, mingw32-make.exe, or .tools\bin\make.bat^)
set /a FAIL_COUNT+=1
exit /b 0

:check_msys_bash
set "MSYS_BASH="
for %%P in (
  "C:\msys64\usr\bin\bash.exe"
  "%ProgramFiles%\msys64\usr\bin\bash.exe"
  "%ProgramFiles(x86)%\msys64\usr\bin\bash.exe"
) do (
  if exist "%%~fP" if not defined MSYS_BASH set "MSYS_BASH=%%~fP"
)
if defined MSYS_BASH (
  echo [preflight] OK   msys bash ^(!MSYS_BASH!^)
  exit /b 0
)
echo [preflight] MISS msys bash ^(expected msys2 bash.exe^)
set /a FAIL_COUNT+=1
exit /b 0

:check_cmd
set "CHECK_EXE=%~1"
set "CHECK_LABEL=%~2"
set "CHECK_PATH="
call :find_cmd "%CHECK_EXE%" CHECK_PATH
if defined CHECK_PATH (
  echo [preflight] OK   %CHECK_LABEL% ^(!CHECK_PATH!^)
  exit /b 0
)
echo [preflight] MISS %CHECK_LABEL%
set /a FAIL_COUNT+=1
exit /b 0

:find_cmd
set "FIND_EXE=%~1"
set "FIND_OUTVAR=%~2"
set "FIND_PATH="
for /f "delims=" %%P in ('where %FIND_EXE% 2^>nul') do (
  if not defined FIND_PATH set "FIND_PATH=%%P"
)
set "%FIND_OUTVAR%=%FIND_PATH%"
exit /b 0

:check_file
if exist "%~1" (
  echo [preflight] OK   %~2
) else (
  echo [preflight] MISS %~2
  set /a FAIL_COUNT+=1
)
exit /b 0

:check_dir
set "DIR_TEST=%~1"
for %%I in ("%DIR_TEST%") do set "DIR_TEST=%%~fI"
if exist "%DIR_TEST%" (
  echo [preflight] OK   %~2
) else (
  echo [preflight] MISS %~2
  set /a FAIL_COUNT+=1
)
exit /b 0
