@echo off
setlocal EnableExtensions EnableDelayedExpansion
call "%~dp0run-clojure-windows.bat" -M:dev:repl %*
exit /b %ERRORLEVEL%
