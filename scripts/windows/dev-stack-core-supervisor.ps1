param(
    [Parameter(Mandatory = $true)]
    [string]$ChildScript,

    [Parameter(Mandatory = $true)]
    [string]$PidFile
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

$resolvedChildScript = (Resolve-Path -LiteralPath $ChildScript).Path
$resolvedPidFile = [System.IO.Path]::GetFullPath($PidFile)
$pidDir = Split-Path -Parent $resolvedPidFile
if (-not [string]::IsNullOrWhiteSpace($pidDir)) {
    New-Item -ItemType Directory -Force -Path $pidDir | Out-Null
}

$marker = @{
    pid = $PID
    supervisor_script = $PSCommandPath
    child_script = $resolvedChildScript
}
$marker | ConvertTo-Json -Compress | Set-Content -LiteralPath $resolvedPidFile -Encoding ascii

$child = $null
$exitCode = 1

try {
    $cmdLine = "call $(Quote-CmdArg $resolvedChildScript)"
    $child = Start-Process `
        -FilePath "cmd.exe" `
        -ArgumentList @("/d", "/c", $cmdLine) `
        -WorkingDirectory (Split-Path -Parent $resolvedChildScript) `
        -NoNewWindow `
        -PassThru

    while (-not $child.HasExited) {
        Start-Sleep -Milliseconds 500
        $child.Refresh()
    }

    $exitCode = $child.ExitCode
}
finally {
    Remove-Item -LiteralPath $resolvedPidFile -Force -ErrorAction SilentlyContinue
}

exit $exitCode
