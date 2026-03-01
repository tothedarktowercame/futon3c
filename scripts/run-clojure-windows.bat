@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "REPO_ROOT=%%~fI"

set "CLOJURE_BAT=%REPO_ROOT%\.tools\clojure\bin\clojure.bat"
if not exist "%CLOJURE_BAT%" (
  1>&2 echo [run-clojure-windows] ERROR: missing %CLOJURE_BAT%
  1>&2 echo Run scripts\bootstrap-tools.bat first.
  exit /b 1
)

set "PATH=%REPO_ROOT%\.tools\bin;%REPO_ROOT%\.tools\clojure\bin;%PATH%"
set "LOCAL_M2=%REPO_ROOT%\.m2\repository"
if not exist "%LOCAL_M2%" mkdir "%LOCAL_M2%" >nul 2>nul

if defined CLJ_JVM_OPTS (
  set "CLJ_JVM_OPTS=-Dmaven.repo.local=%LOCAL_M2% %CLJ_JVM_OPTS%"
) else (
  set "CLJ_JVM_OPTS=-Dmaven.repo.local=%LOCAL_M2%"
)

call "%CLOJURE_BAT%" %*
exit /b %ERRORLEVEL%
