@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

if not defined CODEX_SANDBOX set "CODEX_SANDBOX=danger-full-access"
if not defined CODEX_APPROVAL_POLICY (
  if defined CODEX_APPROVAL (
    set "CODEX_APPROVAL_POLICY=%CODEX_APPROVAL%"
  ) else (
    set "CODEX_APPROVAL_POLICY=never"
  )
)
if not defined CODEX_REASONING_EFFORT set "CODEX_REASONING_EFFORT=high"

if not defined CODEX_SESSION_FILE set "CODEX_SESSION_FILE=%REPO_ROOT%\.state\codex\session-id"
for %%I in ("%CODEX_SESSION_FILE%") do set "CODEX_SESSION_DIR=%%~dpI"
if defined CODEX_SESSION_DIR (
  if not exist "%CODEX_SESSION_DIR%" mkdir "%CODEX_SESSION_DIR%" >nul 2>nul
  if errorlevel 1 (
    1>&2 echo [dev-windows] ERROR: unable to create CODEX_SESSION_FILE directory: %CODEX_SESSION_DIR%
    exit /b 1
  )
)

if not defined CLAUDE_SESSION_FILE set "CLAUDE_SESSION_FILE=%REPO_ROOT%\.state\claude\session-id"
for %%I in ("%CLAUDE_SESSION_FILE%") do set "CLAUDE_SESSION_DIR=%%~dpI"
if defined CLAUDE_SESSION_DIR (
  if not exist "%CLAUDE_SESSION_DIR%" mkdir "%CLAUDE_SESSION_DIR%" >nul 2>nul
  if errorlevel 1 (
    1>&2 echo [dev-windows] ERROR: unable to create CLAUDE_SESSION_FILE directory: %CLAUDE_SESSION_DIR%
    exit /b 1
  )
)

echo [dev-windows] Codex defaults: sandbox=%CODEX_SANDBOX% approval=%CODEX_APPROVAL_POLICY% reasoning=%CODEX_REASONING_EFFORT%
echo [dev-windows] Codex session file: %CODEX_SESSION_FILE%
call "%~dp0run-clojure-windows.bat" -M:dev %*
exit /b %ERRORLEVEL%
