param(
    [Parameter(Mandatory = $true)]
    [string]$ChildScript,

    [Parameter(Mandatory = $true)]
    [string]$CleanupScript
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

$child = $null
$exitCode = 1
$cleanupExit = 0

try {
    $cmdLine = "call $(Quote-CmdArg $ChildScript)"
    $child = Start-Process `
        -FilePath "cmd.exe" `
        -ArgumentList @("/d", "/c", $cmdLine) `
        -WorkingDirectory (Split-Path -Parent $ChildScript) `
        -NoNewWindow `
        -PassThru

    while (-not $child.HasExited) {
        Start-Sleep -Milliseconds 500
        $child.Refresh()
    }

    $exitCode = $child.ExitCode
}
finally {
    if ($child -and -not $child.HasExited) {
        & taskkill /PID $child.Id /T /F *> $null
        Start-Sleep -Milliseconds 250
    }

    & cmd.exe /d /c "call $(Quote-CmdArg $CleanupScript)"
    $cleanupExit = $LASTEXITCODE
    if ($cleanupExit -ne 0) {
        [Console]::Error.WriteLine("[dev-stack-supervisor] cleanup failed rc={0}" -f $cleanupExit)
        if ($exitCode -eq 0) {
            $exitCode = $cleanupExit
        }
    }
}

exit $exitCode
