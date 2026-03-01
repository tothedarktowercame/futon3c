@echo off
setlocal EnableExtensions DisableDelayedExpansion
call "%~dp0run-bash-script-windows.bat" "scripts/codex-picker" %*
exit /b %ERRORLEVEL%
