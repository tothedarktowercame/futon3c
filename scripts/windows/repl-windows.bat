@echo off
setlocal EnableExtensions EnableDelayedExpansion
call "%~dp0run-clojure-windows.bat" -M:dev-serve:repl %*
exit /b %ERRORLEVEL%
