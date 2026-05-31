@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
set "RUN_CLOJURE_TARGET_ROOT=%REPO_ROOT%"
if defined RUN_CLOJURE_WORKDIR (
  for %%I in ("%RUN_CLOJURE_WORKDIR%") do set "RUN_CLOJURE_TARGET_ROOT=%%~fI"
)

set "CLOJURE_BAT=%REPO_ROOT%\.tools\clojure\bin\clojure.bat"
if not exist "%CLOJURE_BAT%" (
  1>&2 echo [run-clojure-windows] ERROR: missing %CLOJURE_BAT%
  1>&2 echo Run scripts\windows\bootstrap-tools.bat first.
  exit /b 1
)

set "PATH=%REPO_ROOT%\.tools\bin;%REPO_ROOT%\.tools\clojure\bin;%PATH%"
if not defined FUTON1A_DATA_DIR set "FUTON1A_DATA_DIR=%REPO_ROOT%\.state\futon1a\default"
if not exist "%FUTON1A_DATA_DIR%" mkdir "%FUTON1A_DATA_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create FUTON1A_DATA_DIR=%FUTON1A_DATA_DIR%
  exit /b 1
)
if not defined FUTON_STORAGE_ROOT set "FUTON_STORAGE_ROOT=%REPO_ROOT%\.state\storage"
if not exist "%FUTON_STORAGE_ROOT%" mkdir "%FUTON_STORAGE_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create FUTON_STORAGE_ROOT=%FUTON_STORAGE_ROOT%
  exit /b 1
)
if not defined FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH set "FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH=%REPO_ROOT%\.state\mission-control\sessions.edn"
for %%I in ("%FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH%") do set "MC_SNAPSHOT_DIR=%%~dpI"
if not exist "%MC_SNAPSHOT_DIR%" mkdir "%MC_SNAPSHOT_DIR%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create mission-control snapshot directory=%MC_SNAPSHOT_DIR%
  exit /b 1
)
if not defined GITLIBS set "GITLIBS=%REPO_ROOT%\.gitlibs"
if not exist "%GITLIBS%" mkdir "%GITLIBS%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create GITLIBS=%GITLIBS%
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
for %%I in ("%REPO_ROOT%\..\..\gh\mfuton\data\tmp\run-clojure-windows\java") do set "LOCAL_TMP_ROOT=%%~fI"
if not exist "%LOCAL_TMP_ROOT%" mkdir "%LOCAL_TMP_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to create %LOCAL_TMP_ROOT%
  exit /b 1
)
set "RUN_CLOJURE_TMP_TAG=%RANDOM%-%RANDOM%-%RANDOM%-%TIME: =0%"
set "RUN_CLOJURE_TMP_TAG=%RUN_CLOJURE_TMP_TAG::=%"
set "RUN_CLOJURE_TMP_TAG=%RUN_CLOJURE_TMP_TAG:.=%"
set "LOCAL_TMP=%LOCAL_TMP_ROOT%\run-%RUN_CLOJURE_TMP_TAG%"
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
if not defined RUN_CLOJURE_WINDOWS_LWJGL_NATIVE_PATCH set "RUN_CLOJURE_WINDOWS_LWJGL_NATIVE_PATCH=0"
if defined RUN_CLOJURE_EXTRA_ALIAS_BODY (
  if /I "%RUN_CLOJURE_WINDOWS_LWJGL_NATIVE_PATCH%"=="1" (
    > "%CLJ_CONFIG%\deps.edn" (
      echo {
      echo   :mvn/local-repo "%LOCAL_M2_EDN%"
      echo   :aliases
      echo   {:windows-lmdb
      echo    {:extra-deps
      echo     {org.lwjgl/lwjgl$natives-windows {:mvn/version "3.3.1"}
      echo      org.lwjgl/lwjgl-lmdb$natives-windows {:mvn/version "3.3.1"}}}
      echo    :run-clojure-extra
      echo     %RUN_CLOJURE_EXTRA_ALIAS_BODY%
      echo   }
      echo }
    )
  ) else (
    > "%CLJ_CONFIG%\deps.edn" (
      echo {
      echo   :mvn/local-repo "%LOCAL_M2_EDN%"
      echo   :aliases
      echo   {:run-clojure-extra
      echo     %RUN_CLOJURE_EXTRA_ALIAS_BODY%
      echo   }
      echo }
    )
  )
) else (
  if /I "%RUN_CLOJURE_WINDOWS_LWJGL_NATIVE_PATCH%"=="1" (
    > "%CLJ_CONFIG%\deps.edn" (
      echo {
      echo   :mvn/local-repo "%LOCAL_M2_EDN%"
      echo   :aliases
      echo   {:windows-lmdb
      echo    {:extra-deps
      echo     {org.lwjgl/lwjgl$natives-windows {:mvn/version "3.3.1"}
      echo      org.lwjgl/lwjgl-lmdb$natives-windows {:mvn/version "3.3.1"}}}}
      echo }
    )
  ) else (
    > "%CLJ_CONFIG%\deps.edn" (
      echo {
      echo   :mvn/local-repo "%LOCAL_M2_EDN%"
      echo   :aliases {}
      echo }
    )
  )
)
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to write %CLJ_CONFIG%\deps.edn
  exit /b 1
)

pushd "%RUN_CLOJURE_TARGET_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [run-clojure-windows] ERROR: unable to enter %RUN_CLOJURE_TARGET_ROOT%
  exit /b 1
)

call "%CLOJURE_BAT%" %*
set "RC=%ERRORLEVEL%"
popd >nul 2>nul
exit /b %RC%
