param(
    [Parameter(Mandatory = $true)]
    [string]$ChildScript,

    [Parameter(Mandatory = $true)]
    [string]$PidFile,

    [string]$ExitFile = "",

    [string]$CombinedLog = ""
)

$ErrorActionPreference = "Stop"

function Quote-CmdArg {
    param([string]$Value)

    if ([string]::IsNullOrEmpty($Value)) {
        return '""'
    }

    if ($Value -notmatch '[\s"&|<>^()]') {
        return $Value
    }

    return '"' + ($Value -replace '"', '""') + '"'
}

function Ensure-ParentDirectory {
    param([string]$PathValue)

    if ([string]::IsNullOrWhiteSpace($PathValue)) {
        return
    }

    $parent = Split-Path -Parent $PathValue
    if ([string]::IsNullOrWhiteSpace($parent)) {
        return
    }

    New-Item -ItemType Directory -Force -Path $parent | Out-Null
}

function Write-JsonFile {
    param(
        [string]$PathValue,
        [hashtable]$Payload
    )

    if ([string]::IsNullOrWhiteSpace($PathValue)) {
        return
    }

    Ensure-ParentDirectory -PathValue $PathValue
    $json = $Payload | ConvertTo-Json -Depth 5
    $utf8NoBom = New-Object System.Text.UTF8Encoding($false)
    [System.IO.File]::WriteAllText($PathValue, $json, $utf8NoBom)
}

$resolvedChildScript = (Resolve-Path -LiteralPath $ChildScript).Path
$resolvedPidFile = [System.IO.Path]::GetFullPath($PidFile)
$resolvedExitFile = if ([string]::IsNullOrWhiteSpace($ExitFile)) { "" } else { [System.IO.Path]::GetFullPath($ExitFile) }
$resolvedCombinedLog = if ([string]::IsNullOrWhiteSpace($CombinedLog)) { "" } else { [System.IO.Path]::GetFullPath($CombinedLog) }
$startedAt = (Get-Date).ToString("o")

$marker = @{
    pid = $PID
    supervisor_script = $PSCommandPath
    child_script = $resolvedChildScript
    marker_file = $resolvedPidFile
    combined_log = $resolvedCombinedLog
    started_at = $startedAt
}
Write-JsonFile -PathValue $resolvedPidFile -Payload $marker

$child = $null
$exitCode = 1

try {
    $cmdLine = "call $(Quote-CmdArg $resolvedChildScript)"
    $childWorkingDirectory = Split-Path -Parent $resolvedChildScript

    if (-not [string]::IsNullOrWhiteSpace($resolvedCombinedLog)) {
        Ensure-ParentDirectory -PathValue $resolvedCombinedLog
        if (Test-Path -LiteralPath $resolvedCombinedLog) {
            Remove-Item -LiteralPath $resolvedCombinedLog -Force -ErrorAction SilentlyContinue
        }
        $cmdWithLogRedirect = "$cmdLine 1>>$(Quote-CmdArg $resolvedCombinedLog) 2>&1"
        & cmd.exe /d /c $cmdWithLogRedirect
        $exitCode = $LASTEXITCODE
    }
    else {
        $child = Start-Process `
            -FilePath "cmd.exe" `
            -ArgumentList @("/d", "/c", $cmdLine) `
            -WorkingDirectory $childWorkingDirectory `
            -NoNewWindow `
            -PassThru

        while (-not $child.HasExited) {
            Start-Sleep -Milliseconds 500
            $child.Refresh()
        }

        $exitCode = $child.ExitCode
    }
}
finally {
    if ($child -and -not $child.HasExited) {
        & taskkill /PID $child.Id /T /F *> $null
        Start-Sleep -Milliseconds 250
    }

    $exitPayload = @{
        pid = $PID
        supervisor_script = $PSCommandPath
        child_script = $resolvedChildScript
        marker_file = $resolvedPidFile
        combined_log = $resolvedCombinedLog
        exit_code = $exitCode
        started_at = $startedAt
        finished_at = (Get-Date).ToString("o")
    }
    if ($child) {
        $exitPayload["child_pid"] = $child.Id
    }
    Write-JsonFile -PathValue $resolvedExitFile -Payload $exitPayload
    Remove-Item -LiteralPath $resolvedPidFile -Force -ErrorAction SilentlyContinue
}

exit $exitCode
