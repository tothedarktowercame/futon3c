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
if not defined FUTON1A_DATA_DIR set "FUTON1A_DATA_DIR=%REPO_ROOT%\.state\futon1a\default"
if not exist "%FUTON1A_DATA_DIR%" mkdir "%FUTON1A_DATA_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create FUTON1A_DATA_DIR=%FUTON1A_DATA_DIR%
  exit /b 1
)
set "CLJ_CONFIG=%REPO_ROOT%\.clj-config"
if not exist "%CLJ_CONFIG%" mkdir "%CLJ_CONFIG%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create %CLJ_CONFIG%
  exit /b 1
)

set "LOCAL_M2=%REPO_ROOT%\.m2\repository"
if not exist "%LOCAL_M2%" mkdir "%LOCAL_M2%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create %LOCAL_M2%
  exit /b 1
)
set "LOCAL_TMP=%REPO_ROOT%\.tmp\java"
if not exist "%LOCAL_TMP%" mkdir "%LOCAL_TMP%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create %LOCAL_TMP%
  exit /b 1
)
set "TMP=%LOCAL_TMP%"
set "TEMP=%LOCAL_TMP%"
if defined JAVA_OPTS (
  set "JAVA_OPTS=-Djava.io.tmpdir=%LOCAL_TMP% %JAVA_OPTS%"
) else (
  set "JAVA_OPTS=-Djava.io.tmpdir=%LOCAL_TMP%"
)
if defined CLJ_JVM_OPTS (
  set "CLJ_JVM_OPTS=-Djava.io.tmpdir=%LOCAL_TMP% %CLJ_JVM_OPTS%"
) else (
  set "CLJ_JVM_OPTS=-Djava.io.tmpdir=%LOCAL_TMP%"
)

set "LOCAL_M2_EDN=%LOCAL_M2:\=/%"
> "%CLJ_CONFIG%\deps.edn" (
  echo {
  echo   :mvn/local-repo "%LOCAL_M2_EDN%"
  echo   :aliases {}
  echo }
)
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to write %CLJ_CONFIG%\deps.edn
  exit /b 1
)

call "%CLOJURE_BAT%" %*
exit /b %ERRORLEVEL%
