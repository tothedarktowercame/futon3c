@echo off
setlocal EnableExtensions DisableDelayedExpansion
set "EMACSCLIENT_GUI_BIN="
set "EMACSCLIENT_GUI_DIR="
for %%P in (
  "%ProgramFiles%\Emacs\emacs-30.2\bin\emacsclientw.exe"
  "%ProgramFiles%\Emacs\bin\emacsclientw.exe"
  "%ProgramFiles(x86)%\Emacs\bin\emacsclientw.exe"
  "%LOCALAPPDATA%\Programs\Emacs\bin\emacsclientw.exe"
) do (
  if exist "%%~fP" (
    set "EMACSCLIENT_GUI_BIN=%%~fP"
    for %%I in ("%%~fP") do set "EMACSCLIENT_GUI_DIR=%%~dpI"
    goto found_emacsclientw
  )
)
:found_emacsclientw
if defined EMACSCLIENT_GUI_DIR (
  if "%EMACSCLIENT_GUI_DIR:~-1%"=="\" set "EMACSCLIENT_GUI_DIR=%EMACSCLIENT_GUI_DIR:~0,-1%"
  set "PATH=%EMACSCLIENT_GUI_DIR%;%PATH%"
)

if not defined EMACSCLIENT_GUI_BIN set "EMACSCLIENT_GUI_BIN=emacsclientw.exe"
echo [codex-repl-windows] Bootstrapping GUI frame via %EMACSCLIENT_GUI_BIN%
"%EMACSCLIENT_GUI_BIN%" -a "" -c -n -e "(progn (switch-to-buffer \"*scratch*\") t)"
if errorlevel 1 (
  1>&2 echo [codex-repl-windows] ERROR: failed to create GUI frame with emacsclientw.
  1>&2 echo Ensure runemacs daemon is running and emacsclientw can attach.
  exit /b 1
)

call "%~dp0codex-picker-windows.bat" --repl %*
exit /b %ERRORLEVEL%
