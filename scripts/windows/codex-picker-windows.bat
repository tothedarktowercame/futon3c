@echo off
setlocal EnableExtensions DisableDelayedExpansion
for %%I in ("%~dp0..\..") do set "REPO_ROOT=%%~fI"
rem Force repo-local Emacs Lisp path for codex-picker on Windows.
set "FUTON3C_EMACS_DIR=%REPO_ROOT:\=/%/emacs"
call "%~dp0run-bash-script-windows.bat" "scripts/codex-picker" %*
exit /b %ERRORLEVEL%
