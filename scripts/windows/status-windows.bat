@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"

if not defined EVIDENCE_BASE set "EVIDENCE_BASE=http://localhost:7070"

pushd "%REPO_ROOT%" >nul

echo === Agency Health ===
curl -sS --max-time 3 %EVIDENCE_BASE%/health || echo (Agency not reachable)
echo.
echo === Registered Agents ===
curl -sS --max-time 3 %EVIDENCE_BASE%/api/alpha/agents || echo (Agency not reachable)
echo.
echo === Recent Evidence (last 5) ===
curl -sS --max-time 3 "%EVIDENCE_BASE%/api/alpha/evidence?limit=5" || echo (no evidence or Agency not reachable)
echo.
echo === Git Log (last 5) ===
git log --oneline -5 || echo (not a git repo)

popd >nul
exit /b 0
