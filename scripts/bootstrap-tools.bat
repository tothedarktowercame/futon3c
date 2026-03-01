@echo off
setlocal EnableExtensions EnableDelayedExpansion

rem Futon3c Windows tool bootstrap.
rem Installs:
rem - Babashka into:      <prefix>\bin\bb.exe
rem - Clojure CLI tools:  <prefix>\clojure\ (bin\clojure.bat, bin\clj.bat, libexec\...)
rem - GitHub CLI into:    <prefix>\bin\gh.exe
rem - jq into:            <prefix>\bin\jq.exe
rem - emacsclient into:   <prefix>\bin\emacsclient.exe (copied from existing install or provisioned)
rem - make shim into:     <prefix>\bin\make.bat when only mingw32-make is present

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "REPO_ROOT=%%~fI"

set "PREFIX=%REPO_ROOT%\.tools"
set "BB_VERSION=latest"
set "CLJTOOLS_VERSION=latest"
set "GH_VERSION=latest"
set "JQ_VERSION=latest"
set "INSTALL_BB=1"
set "INSTALL_CLOJURE=1"
set "INSTALL_GH=1"
set "INSTALL_JQ=1"
set "INSTALL_EMACSCLIENT=1"
set "INSTALL_MSYS_PACKAGES=1"
set "FORCE=0"

:parse_args
if "%~1"=="" goto args_done

if /i "%~1"=="--prefix" (
  if "%~2"=="" (
    1>&2 echo ERROR: --prefix requires DIR
    exit /b 2
  )
  set "PREFIX=%~2"
  shift
  shift
  goto parse_args
)

if /i "%~1"=="--bb-version" (
  if "%~2"=="" (
    1>&2 echo ERROR: --bb-version requires VER
    exit /b 2
  )
  set "BB_VERSION=%~2"
  shift
  shift
  goto parse_args
)

if /i "%~1"=="--cljtools-version" (
  if "%~2"=="" (
    1>&2 echo ERROR: --cljtools-version requires VER
    exit /b 2
  )
  set "CLJTOOLS_VERSION=%~2"
  shift
  shift
  goto parse_args
)

if /i "%~1"=="--gh-version" (
  if "%~2"=="" (
    1>&2 echo ERROR: --gh-version requires VER
    exit /b 2
  )
  set "GH_VERSION=%~2"
  shift
  shift
  goto parse_args
)

if /i "%~1"=="--jq-version" (
  if "%~2"=="" (
    1>&2 echo ERROR: --jq-version requires VER
    exit /b 2
  )
  set "JQ_VERSION=%~2"
  shift
  shift
  goto parse_args
)

if /i "%~1"=="--no-bb" (
  set "INSTALL_BB=0"
  shift
  goto parse_args
)

if /i "%~1"=="--no-clojure" (
  set "INSTALL_CLOJURE=0"
  shift
  goto parse_args
)

if /i "%~1"=="--no-gh" (
  set "INSTALL_GH=0"
  shift
  goto parse_args
)

if /i "%~1"=="--no-jq" (
  set "INSTALL_JQ=0"
  shift
  goto parse_args
)

if /i "%~1"=="--no-emacsclient" (
  set "INSTALL_EMACSCLIENT=0"
  shift
  goto parse_args
)

if /i "%~1"=="--no-msys-packages" (
  set "INSTALL_MSYS_PACKAGES=0"
  shift
  goto parse_args
)

if /i "%~1"=="--force" (
  set "FORCE=1"
  shift
  goto parse_args
)

if /i "%~1"=="-h" goto show_help
if /i "%~1"=="--help" goto show_help

1>&2 echo Unknown argument: %~1
1>&2 echo.
call :usage 1>&2
exit /b 2

:show_help
call :usage
exit /b 0

:args_done
where powershell.exe >nul 2>nul
if errorlevel 1 (
  1>&2 echo Missing required command: powershell.exe
  exit /b 1
)

mkdir "%PREFIX%\bin" "%PREFIX%\clojure" >nul 2>nul

if "%INSTALL_BB%"=="1" (
  call :install_babashka
  if errorlevel 1 exit /b 1
)

if "%INSTALL_CLOJURE%"=="1" (
  call "%SCRIPT_DIR%\bootstrap-clojure-tools.bat" "%PREFIX%" "%CLJTOOLS_VERSION%" "%FORCE%"
  if errorlevel 1 exit /b 1
)

if "%INSTALL_GH%"=="1" (
  call :install_github_cli
  if errorlevel 1 exit /b 1
)

if "%INSTALL_JQ%"=="1" (
  call :install_jq
  if errorlevel 1 exit /b 1
)

if "%INSTALL_EMACSCLIENT%"=="1" (
  call :install_emacsclient
  if errorlevel 1 exit /b 1
)

call :ensure_make_shim
if errorlevel 1 exit /b 1

if "%INSTALL_MSYS_PACKAGES%"=="1" (
  call :ensure_msys_packages
  if errorlevel 1 exit /b 1
)

echo(
echo [bootstrap] Done.
echo(
echo Quick usage:
echo   - babashka: %PREFIX%\bin\bb.exe
echo   - clojure:  %PREFIX%\clojure\bin\clojure.bat
echo   - gh:       %PREFIX%\bin\gh.exe
echo   - jq:       %PREFIX%\bin\jq.exe
echo   - emacs:    %PREFIX%\bin\emacsclient.exe
echo   - make shim (when needed): %PREFIX%\bin\make.bat
echo(
echo PATH handling intentionally omitted.
echo Governed PATH artifacts:
echo   - social/path-changes-must-use-governed-skill
echo   - M-windows-path-governed-skill
exit /b 0

:usage
echo Usage: scripts\bootstrap-tools.bat [options]
echo(
echo Options:
echo   --prefix DIR             Install under DIR ^(default: ^<repo^>\.tools^)
echo   --bb-version VER         Babashka version ^(default: latest^)
echo   --cljtools-version VER   Clojure CLI tools version tag ^(default: latest^)
echo   --gh-version VER         GitHub CLI version ^(default: latest^)
echo   --jq-version VER         jq version ^(default: latest^)
echo   --no-bb                  Skip babashka install
echo   --no-clojure             Skip clojure tools install
echo   --no-gh                  Skip GitHub CLI install
echo   --no-jq                  Skip jq install
echo   --no-emacsclient         Skip emacsclient installation check/install
echo   --no-msys-packages       Skip MSYS package best-effort check/install
echo   --force                  Reinstall even if already present
echo   -h, --help               Show this help
echo(
echo Notes:
echo   - Run where outbound network access is available.
echo   - Uses GitHub release metadata for latest version resolution.
echo   - On Windows, Clojure latest resolves via casselc/clj-msi.
echo   - PATH is not mutated by this script by policy.
exit /b 0

:detect_arch
set "ARCH=%PROCESSOR_ARCHITECTURE%"
if /i "%PROCESSOR_ARCHITEW6432%"=="AMD64" set "ARCH=AMD64"
if /i "%PROCESSOR_ARCHITEW6432%"=="ARM64" set "ARCH=ARM64"

if /i "%ARCH%"=="AMD64" (
  set "TOOL_ARCH=amd64"
  exit /b 0
)

if /i "%ARCH%"=="ARM64" (
  set "TOOL_ARCH=aarch64"
  exit /b 0
)

1>&2 echo Unsupported architecture: %ARCH%
exit /b 1

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

:run_pacman
set "RP_PACMAN=%~1"
if "%~2"=="" (
  1>&2 echo run_pacman requires arguments.
  exit /b 2
)
shift
set "RP_DIR="
for %%P in ("%RP_PACMAN%") do set "RP_DIR=%%~dpP"
if "!RP_DIR:~-1!"=="\" set "RP_DIR=!RP_DIR:~0,-1!"
cmd /d /c "set PATH=%RP_DIR%;%PATH%&\"%RP_PACMAN%\" %*"
exit /b %ERRORLEVEL%

:find_command_path
set "FC_CMD=%~1"
set "FC_OUTVAR=%~2"
set "FC_PATH="
for /f "delims=" %%P in ('where "%FC_CMD%" 2^>nul') do (
  if not defined FC_PATH set "FC_PATH=%%P"
)
if defined FC_PATH (
  set "%FC_OUTVAR%=%FC_PATH%"
  exit /b 0
)
set "%FC_OUTVAR%="
exit /b 1

:find_pacman
set "PAC_OUTVAR=%~1"
set "PACMAN_PATH="
call :find_command_path pacman.exe PACMAN_PATH
if not errorlevel 1 goto pacman_done
for %%P in (
  "C:\msys64\usr\bin\pacman.exe"
  "%ProgramFiles%\msys64\usr\bin\pacman.exe"
  "%ProgramFiles(x86)%\msys64\usr\bin\pacman.exe"
) do (
  if exist "%%~fP" if not defined PACMAN_PATH set "PACMAN_PATH=%%~fP"
)
:pacman_done
if defined PACMAN_PATH (
  set "%PAC_OUTVAR%=%PACMAN_PATH%"
  exit /b 0
)
set "%PAC_OUTVAR%="
exit /b 1

:ensure_make_shim
set "MAKE_PATH="
call :find_command_path make.exe MAKE_PATH
if not errorlevel 1 (
  echo [bootstrap] make available: !MAKE_PATH!
  exit /b 0
)

set "MINGW_MAKE_PATH="
call :find_command_path mingw32-make.exe MINGW_MAKE_PATH
if errorlevel 1 (
  echo [bootstrap] WARN: make/mingw32-make not found. Install MSYS packages or add your toolchain.
  exit /b 0
)

if exist "%PREFIX%\bin\make.bat" if "%FORCE%"=="0" (
  echo [bootstrap] make shim already present: %PREFIX%\bin\make.bat
  exit /b 0
)

(
  echo @echo off
  echo setlocal EnableExtensions
  echo mingw32-make %%*
  echo exit /b %%ERRORLEVEL%%
) > "%PREFIX%\bin\make.bat"

if errorlevel 1 (
  1>&2 echo Failed to write make shim at %PREFIX%\bin\make.bat
  exit /b 1
)
echo [bootstrap] Installed make shim: %PREFIX%\bin\make.bat
exit /b 0

:ensure_msys_packages
set "NEED_MSYS_PKGS="

set "HAS_BASH="
call :find_command_path bash.exe HAS_BASH
if errorlevel 1 set "NEED_MSYS_PKGS=!NEED_MSYS_PKGS! bash"

set "HAS_CURL="
call :find_command_path curl.exe HAS_CURL
if errorlevel 1 set "NEED_MSYS_PKGS=!NEED_MSYS_PKGS! curl"

set "HAS_MAKE="
call :find_command_path make.exe HAS_MAKE
if errorlevel 1 (
  set "HAS_MINGWMAKE="
  call :find_command_path mingw32-make.exe HAS_MINGWMAKE
  if errorlevel 1 set "NEED_MSYS_PKGS=!NEED_MSYS_PKGS! make"
)

if not defined NEED_MSYS_PKGS (
  echo [bootstrap] MSYS package preflight satisfied.
  exit /b 0
)

set "PACMAN_BIN="
call :find_pacman PACMAN_BIN
if errorlevel 1 (
  echo [bootstrap] WARN: missing shell tools:!NEED_MSYS_PKGS!
  echo [bootstrap] WARN: pacman not found; install MSYS2 packages manually.
  exit /b 0
)

echo [bootstrap] Installing MSYS packages via pacman:!NEED_MSYS_PKGS!
call :run_pacman "%PACMAN_BIN%" -S --noconfirm --needed !NEED_MSYS_PKGS!
if errorlevel 1 (
  echo [bootstrap] WARN: pacman reported errors; continuing.
)
exit /b 0

:install_github_cli
call :detect_arch
if errorlevel 1 exit /b 1

set "GH_BIN=%PREFIX%\bin\gh.exe"
set "GH_TAG="
set "GH_VER="
set "GH_ARCH="

if /i "%TOOL_ARCH%"=="amd64" set "GH_ARCH=amd64"
if /i "%TOOL_ARCH%"=="aarch64" set "GH_ARCH=arm64"

if /i "%GH_VERSION%"=="latest" (
  call :github_latest_tag "cli/cli" GH_TAG
  if errorlevel 1 exit /b 1
  set "GH_VER=!GH_TAG:v=!"
) else (
  set "GH_VER=%GH_VERSION%"
  if /i "!GH_VER:~0,1!"=="v" set "GH_VER=!GH_VER:~1!"
  set "GH_TAG=v!GH_VER!"
)

if exist "%GH_BIN%" if "%FORCE%"=="0" (
  set "GH_CURRENT="
  for /f "tokens=3 delims= " %%V in ('"%GH_BIN%" --version 2^>nul') do (
    if not defined GH_CURRENT set "GH_CURRENT=%%V"
  )
  if defined GH_CURRENT if /i "!GH_CURRENT!"=="!GH_VER!" (
    echo [bootstrap] gh already installed: %GH_BIN% ^(v!GH_CURRENT!^)
    exit /b 0
  )
)

set "GH_TMP=%TEMP%\futon3c-bootstrap-gh-%RANDOM%%RANDOM%"
set "GH_ZIP=%GH_TMP%\gh.zip"
set "GH_EXTRACT=%GH_TMP%\extract"
mkdir "%GH_EXTRACT%" >nul 2>nul

set "GH_URL=https://github.com/cli/cli/releases/download/%GH_TAG%/gh_%GH_VER%_windows_%GH_ARCH%.zip"
echo [bootstrap] Downloading GitHub CLI %GH_TAG%...
call :download_file "%GH_URL%" "%GH_ZIP%"
if errorlevel 1 (
  1>&2 echo Failed to download GitHub CLI archive.
  rmdir /s /q "%GH_TMP%" >nul 2>nul
  exit /b 1
)

powershell -NoProfile -ExecutionPolicy Bypass -Command "$ErrorActionPreference='Stop'; Expand-Archive -Path '%GH_ZIP%' -DestinationPath '%GH_EXTRACT%' -Force"
if errorlevel 1 (
  1>&2 echo Failed to extract GitHub CLI archive.
  rmdir /s /q "%GH_TMP%" >nul 2>nul
  exit /b 1
)

set "GH_SRC="
for /f "delims=" %%F in ('dir /b /s "%GH_EXTRACT%\gh.exe" 2^>nul') do (
  if not defined GH_SRC set "GH_SRC=%%F"
)

if not defined GH_SRC (
  1>&2 echo gh.exe not found in downloaded archive.
  rmdir /s /q "%GH_TMP%" >nul 2>nul
  exit /b 1
)

copy /y "%GH_SRC%" "%GH_BIN%" >nul
if errorlevel 1 (
  1>&2 echo Failed to install gh at %GH_BIN%
  rmdir /s /q "%GH_TMP%" >nul 2>nul
  exit /b 1
)

echo [bootstrap] Installed gh: %GH_BIN%
"%GH_BIN%" --version
rmdir /s /q "%GH_TMP%" >nul 2>nul
exit /b 0

:install_jq
call :detect_arch
if errorlevel 1 exit /b 1

set "JQ_BIN=%PREFIX%\bin\jq.exe"
set "JQ_VER="
set "JQ_TAG="
set "JQ_URL="

if /i "%JQ_VERSION%"=="latest" (
  call :github_latest_tag "jqlang/jq" JQ_TAG
  if errorlevel 1 exit /b 1
  set "JQ_VER=!JQ_TAG!"
) else (
  set "JQ_VER=%JQ_VERSION%"
)

if /i "!JQ_VER:~0,3!"=="jq-" set "JQ_VER=!JQ_VER:~3!"
if /i "!JQ_VER:~0,1!"=="v" set "JQ_VER=!JQ_VER:~1!"
set "JQ_TAG=jq-!JQ_VER!"

if exist "%JQ_BIN%" if "%FORCE%"=="0" (
  set "JQ_CURRENT="
  for /f "tokens=1 delims= " %%V in ('"%JQ_BIN%" --version 2^>nul') do (
    if not defined JQ_CURRENT set "JQ_CURRENT=%%V"
  )
  if defined JQ_CURRENT (
    set "JQ_CURRENT=!JQ_CURRENT:jq-=!"
    if /i "!JQ_CURRENT!"=="!JQ_VER!" (
      echo [bootstrap] jq already installed: %JQ_BIN% ^(jq-!JQ_CURRENT!^)
      exit /b 0
    )
  )
)

set "JQ_TMP=%TEMP%\futon3c-bootstrap-jq-%RANDOM%%RANDOM%"
set "JQ_EXE=%JQ_TMP%\jq.exe"
mkdir "%JQ_TMP%" >nul 2>nul

if /i "%TOOL_ARCH%"=="amd64" (
  set "JQ_URL=https://github.com/jqlang/jq/releases/download/%JQ_TAG%/jq-windows-amd64.exe"
  echo [bootstrap] Downloading jq %JQ_TAG%...
  call :download_file "!JQ_URL!" "%JQ_EXE%"
  if errorlevel 1 (
    set "JQ_URL=https://github.com/jqlang/jq/releases/download/%JQ_TAG%/jq-win64.exe"
    call :download_file "!JQ_URL!" "%JQ_EXE%"
  )
) else (
  set "JQ_URL=https://github.com/jqlang/jq/releases/download/%JQ_TAG%/jq-windows-arm64.exe"
  echo [bootstrap] Downloading jq %JQ_TAG%...
  call :download_file "!JQ_URL!" "%JQ_EXE%"
)

if errorlevel 1 (
  1>&2 echo Failed to download jq binary for %JQ_TAG%.
  rmdir /s /q "%JQ_TMP%" >nul 2>nul
  exit /b 1
)

copy /y "%JQ_EXE%" "%JQ_BIN%" >nul
if errorlevel 1 (
  1>&2 echo Failed to install jq at %JQ_BIN%
  rmdir /s /q "%JQ_TMP%" >nul 2>nul
  exit /b 1
)

echo [bootstrap] Installed jq: %JQ_BIN%
"%JQ_BIN%" --version
rmdir /s /q "%JQ_TMP%" >nul 2>nul
exit /b 0

:find_emacsclient
set "EMC_OUTVAR=%~1"
set "EMC_PATH="
for %%C in (emacsclient.exe emacsclientw.exe) do (
  for /f "delims=" %%P in ('where %%C 2^>nul') do (
    if not defined EMC_PATH set "EMC_PATH=%%P"
  )
)
if not defined EMC_PATH (
  for %%P in (
    "%ProgramFiles%\Emacs\bin\emacsclient.exe"
    "%ProgramFiles%\Emacs\bin\emacsclientw.exe"
    "%ProgramFiles(x86)%\Emacs\bin\emacsclient.exe"
    "%ProgramFiles(x86)%\Emacs\bin\emacsclientw.exe"
    "%LOCALAPPDATA%\Programs\Emacs\bin\emacsclient.exe"
    "%LOCALAPPDATA%\Programs\Emacs\bin\emacsclientw.exe"
    "C:\msys64\usr\bin\emacsclient.exe"
  ) do (
    if exist "%%~fP" if not defined EMC_PATH set "EMC_PATH=%%~fP"
  )
)
if defined EMC_PATH (
  set "%EMC_OUTVAR%=%EMC_PATH%"
  exit /b 0
)
set "%EMC_OUTVAR%="
exit /b 1

:install_emacsclient_via_winget
where winget.exe >nul 2>nul
if errorlevel 1 exit /b 1
echo [bootstrap] Attempting Emacs install via winget...
winget install --exact --id GNU.Emacs --scope user --silent --accept-package-agreements --accept-source-agreements >nul
if errorlevel 1 (
  echo [bootstrap] WARN: winget install GNU.Emacs failed.
  exit /b 1
)
exit /b 0

:install_emacsclient_via_pacman
set "PACMAN_BIN="
call :find_pacman PACMAN_BIN
if errorlevel 1 exit /b 1
echo [bootstrap] Attempting Emacs install via pacman...
call :run_pacman "%PACMAN_BIN%" -S --noconfirm --needed emacs
if errorlevel 1 (
  echo [bootstrap] WARN: pacman install emacs failed.
  exit /b 1
)
exit /b 0

:install_emacsclient
set "EMACSCLIENT_BIN=%PREFIX%\bin\emacsclient.exe"

if exist "%EMACSCLIENT_BIN%" if "%FORCE%"=="0" (
  echo [bootstrap] emacsclient already installed: %EMACSCLIENT_BIN%
  exit /b 0
)

set "EMACSCLIENT_SRC="
call :find_emacsclient EMACSCLIENT_SRC
if errorlevel 1 (
  call :install_emacsclient_via_winget
  set "EMACSCLIENT_SRC="
  call :find_emacsclient EMACSCLIENT_SRC
)

if not defined EMACSCLIENT_SRC (
  call :install_emacsclient_via_pacman
  set "EMACSCLIENT_SRC="
  call :find_emacsclient EMACSCLIENT_SRC
)

if not defined EMACSCLIENT_SRC (
  1>&2 echo Could not locate emacsclient after attempted install.
  1>&2 echo Install Emacs manually and rerun, or use --no-emacsclient.
  exit /b 1
)

for %%I in ("%EMACSCLIENT_SRC%") do set "EMACSCLIENT_SRC_F=%%~fI"
for %%I in ("%EMACSCLIENT_BIN%") do set "EMACSCLIENT_BIN_F=%%~fI"
if /i "!EMACSCLIENT_SRC_F!"=="!EMACSCLIENT_BIN_F!" (
  echo [bootstrap] emacsclient already installed: %EMACSCLIENT_BIN%
  exit /b 0
)

copy /y "%EMACSCLIENT_SRC%" "%EMACSCLIENT_BIN%" >nul
if errorlevel 1 (
  1>&2 echo Failed to install emacsclient at %EMACSCLIENT_BIN% from %EMACSCLIENT_SRC%
  exit /b 1
)
echo [bootstrap] Installed emacsclient: %EMACSCLIENT_BIN%
exit /b 0

:install_babashka
call :detect_arch
if errorlevel 1 exit /b 1

set "BB_BIN=%PREFIX%\bin\bb.exe"
set "BB_TAG="
set "BB_VER="

if /i "%BB_VERSION%"=="latest" (
  call :github_latest_tag "babashka/babashka" BB_TAG
  if errorlevel 1 exit /b 1
  set "BB_VER=!BB_TAG:v=!"
) else (
  set "BB_VER=%BB_VERSION%"
  if /i "!BB_VER:~0,1!"=="v" set "BB_VER=!BB_VER:~1!"
  set "BB_TAG=v!BB_VER!"
)

if exist "%BB_BIN%" if "%FORCE%"=="0" (
  set "BB_CURRENT="
  for /f "tokens=2 delims= " %%V in ('"%BB_BIN%" --version 2^>nul') do (
    if not defined BB_CURRENT set "BB_CURRENT=%%V"
  )
  if defined BB_CURRENT (
    set "BB_CURRENT=!BB_CURRENT:v=!"
    if /i "!BB_CURRENT!"=="!BB_VER!" (
      echo [bootstrap] bb already installed: %BB_BIN% ^(v!BB_CURRENT!^)
      exit /b 0
    )
  )
)

set "BB_TMP=%TEMP%\futon3c-bootstrap-bb-%RANDOM%%RANDOM%"
set "BB_ZIP=%BB_TMP%\bb.zip"
set "BB_EXTRACT=%BB_TMP%\extract"
mkdir "%BB_EXTRACT%" >nul 2>nul

set "BB_URL=https://github.com/babashka/babashka/releases/download/%BB_TAG%/babashka-%BB_VER%-windows-%TOOL_ARCH%.zip"
echo [bootstrap] Downloading babashka %BB_TAG%...
call :download_file "%BB_URL%" "%BB_ZIP%"
if errorlevel 1 (
  if /i "%TOOL_ARCH%"=="aarch64" (
    set "BB_URL=https://github.com/babashka/babashka/releases/download/%BB_TAG%/babashka-%BB_VER%-windows-arm64.zip"
    call :download_file "!BB_URL!" "%BB_ZIP%"
  )
)
if errorlevel 1 (
  1>&2 echo Failed to download babashka archive.
  rmdir /s /q "%BB_TMP%" >nul 2>nul
  exit /b 1
)

powershell -NoProfile -ExecutionPolicy Bypass -Command "$ErrorActionPreference='Stop'; Expand-Archive -Path '%BB_ZIP%' -DestinationPath '%BB_EXTRACT%' -Force"
if errorlevel 1 (
  1>&2 echo Failed to extract babashka archive.
  rmdir /s /q "%BB_TMP%" >nul 2>nul
  exit /b 1
)

set "BB_SRC="
for /f "delims=" %%F in ('dir /b /s "%BB_EXTRACT%\bb.exe" 2^>nul') do (
  if not defined BB_SRC set "BB_SRC=%%F"
)

if not defined BB_SRC (
  1>&2 echo bb.exe not found in downloaded archive.
  rmdir /s /q "%BB_TMP%" >nul 2>nul
  exit /b 1
)

copy /y "%BB_SRC%" "%BB_BIN%" >nul
if errorlevel 1 (
  1>&2 echo Failed to install bb at %BB_BIN%
  rmdir /s /q "%BB_TMP%" >nul 2>nul
  exit /b 1
)

echo [bootstrap] Installed bb: %BB_BIN%
"%BB_BIN%" --version
rmdir /s /q "%BB_TMP%" >nul 2>nul
exit /b 0

:install_clojure_tools
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
  1>&2 echo   scripts\bootstrap-tools.bat --cljtools-version 1.12.4.1597
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
