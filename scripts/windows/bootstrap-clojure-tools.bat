@echo off
setlocal EnableExtensions EnableDelayedExpansion

rem Helper for bootstrap-tools.bat: install Clojure CLI tools on Windows.
rem Args:
rem   %1 = PREFIX directory (repo .tools root)
rem   %2 = CLJTOOLS_VERSION (e.g. latest or 1.12.4.1597)
rem   %3 = FORCE (0|1)

if "%~1"=="" (
  1>&2 echo ERROR: bootstrap-clojure-tools.bat requires PREFIX argument.
  exit /b 2
)

set "PREFIX=%~1"
set "CLJTOOLS_VERSION=%~2"
set "FORCE=%~3"
if not defined CLJTOOLS_VERSION set "CLJTOOLS_VERSION=latest"
if not defined FORCE set "FORCE=0"

where powershell.exe >nul 2>nul
if errorlevel 1 (
  1>&2 echo Missing required command: powershell.exe
  exit /b 1
)

set "CLJ_PREFIX=%PREFIX%\clojure"
set "CLJ_BIN=%CLJ_PREFIX%\bin\clojure.bat"
set "CLJ_VER="
set "CLJ_TAG="

if /i "%CLJTOOLS_VERSION%"=="latest" (
  call :github_latest_tag "casselc/clj-msi" CLJ_TAG
  if errorlevel 1 (
    1>&2 echo WARN: failed to resolve latest from casselc/clj-msi; falling back to clojure/brew-install
    call :github_latest_tag "clojure/brew-install" CLJ_TAG
    if errorlevel 1 exit /b 1
  )
  set "CLJ_VER=!CLJ_TAG!"
) else (
  set "CLJ_VER=%CLJTOOLS_VERSION%"
)
if /i "!CLJ_VER:~0,1!"=="v" set "CLJ_VER=!CLJ_VER:~1!"

if exist "%CLJ_BIN%" if "%FORCE%"=="0" (
  echo [bootstrap] clojure already installed: %CLJ_BIN%
  exit /b 0
)

set "CLJ_TMP=%TEMP%\futon3c-bootstrap-clj-%RANDOM%%RANDOM%"
set "CLJ_ZIP=%CLJ_TMP%\clojure-tools.zip"
set "CLJ_EXTRACT=%CLJ_TMP%\extract"
mkdir "%CLJ_EXTRACT%" >nul 2>nul

set "CLJ_URL=https://download.clojure.org/install/clojure-tools-%CLJ_VER%.zip"
echo [bootstrap] Downloading Clojure CLI tools %CLJ_VER%...
call :download_file "%CLJ_URL%" "%CLJ_ZIP%"
if errorlevel 1 (
  set "CLJ_URL=https://github.com/casselc/clj-msi/releases/download/v%CLJ_VER%/clojure-tools-%CLJ_VER%.zip"
  call :download_file "!CLJ_URL!" "%CLJ_ZIP%"
)
if errorlevel 1 (
  1>&2 echo Failed to download Clojure CLI tools archive for version %CLJ_VER%.
  1>&2 echo Try an explicit known Windows version, for example:
  1>&2 echo   scripts\windows\bootstrap-tools.bat --cljtools-version 1.12.4.1597
  rmdir /s /q "%CLJ_TMP%" >nul 2>nul
  exit /b 1
)

powershell -NoProfile -ExecutionPolicy Bypass -Command "$ErrorActionPreference='Stop'; Expand-Archive -Path '%CLJ_ZIP%' -DestinationPath '%CLJ_EXTRACT%' -Force"
if errorlevel 1 (
  1>&2 echo Failed to extract Clojure CLI tools archive.
  rmdir /s /q "%CLJ_TMP%" >nul 2>nul
  exit /b 1
)

set "CLJ_JAR="
for /f "delims=" %%F in ('dir /b /s "%CLJ_EXTRACT%\clojure-tools-*.jar" 2^>nul') do (
  if not defined CLJ_JAR set "CLJ_JAR=%%F"
)

if not defined CLJ_JAR (
  1>&2 echo Could not find clojure-tools-*.jar in archive.
  rmdir /s /q "%CLJ_TMP%" >nul 2>nul
  exit /b 1
)

for %%D in ("%CLJ_JAR%") do set "CLJ_SOURCE_DIR=%%~dpD"
if "%FORCE%"=="1" (
  rmdir /s /q "%CLJ_PREFIX%" >nul 2>nul
)
mkdir "%CLJ_PREFIX%\bin" "%CLJ_PREFIX%\libexec" >nul 2>nul

xcopy /e /i /y "%CLJ_SOURCE_DIR%*" "%CLJ_PREFIX%\libexec\" >nul
if errorlevel 1 (
  1>&2 echo Failed copying Clojure tools files to %CLJ_PREFIX%\libexec
  rmdir /s /q "%CLJ_TMP%" >nul 2>nul
  exit /b 1
)

(
  echo @echo off
  echo setlocal EnableExtensions
  echo set "SCRIPT_DIR=%%~dp0"
  echo if "%%SCRIPT_DIR:~-1%%"=="\" set "SCRIPT_DIR=%%SCRIPT_DIR:~0,-1%%"
  echo set "INSTALL_DIR=%%SCRIPT_DIR%%\.."
  echo set "LIBEXEC=%%INSTALL_DIR%%\libexec"
  echo set "TOOLS_MODULE=%%LIBEXEC%%\ClojureTools.psd1"
  echo if not exist "%%TOOLS_MODULE%%" ^(
  echo   echo ERROR: ClojureTools.psd1 not found under %%LIBEXEC%%
  echo   exit /b 1
  echo ^)
  echo if not defined CLJ_CONFIG set "CLJ_CONFIG=%%INSTALL_DIR%%\.clj-config"
  echo if not exist "%%CLJ_CONFIG%%" mkdir "%%CLJ_CONFIG%%" ^>nul 2^>nul
  echo if errorlevel 1 ^(
  echo   echo ERROR: could not create CLJ_CONFIG at %%CLJ_CONFIG%%
  echo   exit /b 1
  echo ^)
  echo where powershell.exe ^>nul 2^>nul
  echo if errorlevel 1 ^(
  echo   echo ERROR: powershell.exe is required to run clojure tools on Windows.
  echo   exit /b 1
  echo ^)
  echo powershell -NoProfile -ExecutionPolicy Bypass -Command "Import-Module '%%TOOLS_MODULE%%' -Force -ErrorAction Stop; Invoke-Clojure @args" -- %%*
  echo exit /b %%ERRORLEVEL%%
) > "%CLJ_PREFIX%\bin\clojure.bat"

(
  echo @echo off
  echo setlocal EnableExtensions
  echo call "%%~dp0clojure.bat" %%*
  echo exit /b %%ERRORLEVEL%%
) > "%CLJ_PREFIX%\bin\clj.bat"

echo [bootstrap] Installed clojure: %CLJ_PREFIX%\bin\clojure.bat
call "%CLJ_PREFIX%\bin\clojure.bat" -Sdescribe >nul 2>nul

rmdir /s /q "%CLJ_TMP%" >nul 2>nul
exit /b 0

:github_latest_tag
set "GH_REPO=%~1"
set "GH_OUTVAR=%~2"
set "GH_TAG="
for /f "usebackq delims=" %%T in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$ErrorActionPreference='Stop'; (Invoke-RestMethod -Uri 'https://api.github.com/repos/%GH_REPO%/releases/latest').tag_name"`) do (
  if not defined GH_TAG set "GH_TAG=%%T"
)
if not defined GH_TAG (
  1>&2 echo Failed to resolve latest tag for %GH_REPO%
  exit /b 1
)
set "%GH_OUTVAR%=%GH_TAG%"
exit /b 0

:download_file
set "DL_URL=%~1"
set "DL_OUT=%~2"
powershell -NoProfile -ExecutionPolicy Bypass -Command "$ErrorActionPreference='Stop'; $ProgressPreference='SilentlyContinue'; [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; Invoke-WebRequest -UseBasicParsing -Uri '%DL_URL%' -OutFile '%DL_OUT%'"
exit /b %ERRORLEVEL%
