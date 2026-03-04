@echo off
setlocal EnableExtensions DisableDelayedExpansion
for %%I in ("%~dp0..\..") do set "REPO_ROOT=%%~fI"
rem Force repo-local Emacs Lisp path for codex-picker on Windows.
set "FUTON3C_EMACS_DIR=%REPO_ROOT:\=/%/emacs"
if not defined CODEX_REASONING_EFFORT set "CODEX_REASONING_EFFORT=high"
if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex\session-id"
for %%I in ("%CODEX_SESSION_FILE%") do set "CODEX_SESSION_DIR=%%~dpI"
if defined CODEX_SESSION_DIR if not exist "%CODEX_SESSION_DIR%" mkdir "%CODEX_SESSION_DIR%" >nul 2>nul
call "%~dp0run-bash-script-windows.bat" "scripts/codex-picker" %*
exit /b %ERRORLEVEL%
