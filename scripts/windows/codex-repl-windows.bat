@echo off
setlocal EnableExtensions DisableDelayedExpansion
call "%~dp0codex-picker-windows.bat" --repl %*
exit /b %ERRORLEVEL%
