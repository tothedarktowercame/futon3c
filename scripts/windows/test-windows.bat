@echo off
setlocal EnableExtensions EnableDelayedExpansion
call "%~dp0run-clojure-windows.bat" -X:test %*
exit /b %ERRORLEVEL%
