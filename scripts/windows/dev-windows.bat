@echo off
setlocal EnableExtensions EnableDelayedExpansion

if not defined CODEX_SANDBOX set "CODEX_SANDBOX=danger-full-access"
if not defined CODEX_APPROVAL_POLICY (
  if defined CODEX_APPROVAL (
    set "CODEX_APPROVAL_POLICY=%CODEX_APPROVAL%"
  ) else (
    set "CODEX_APPROVAL_POLICY=never"
  )
)

echo [dev-windows] Codex defaults: sandbox=%CODEX_SANDBOX% approval=%CODEX_APPROVAL_POLICY%
call "%~dp0run-clojure-windows.bat" -M:dev %*
exit /b %ERRORLEVEL%
