function Add-ScoopRealPathsToPath {
    [CmdletBinding()]
    param(
        [string]$AppsPath = (Resolve-ScoopPath -Name 'Apps')
    )

    if (-not $env:SSH_CONNECTION) {
        return
    }

    if (-not $AppsPath) {
        return
    }

    if (-not (Test-Path -LiteralPath $AppsPath)) {
        return
    }

    $realPaths = New-Object System.Collections.Generic.List[string]

    Get-ChildItem -LiteralPath $AppsPath -ErrorAction SilentlyContinue | ForEach-Object {
        $current = Join-Path $_.FullName 'current'
        if (Test-Path -LiteralPath $current) {
            $target = (Get-Item -LiteralPath $current).Target
            if ($target -and -not $realPaths.Contains($target)) {
                $realPaths.Add($target)
            }
        }
    }

    if ($realPaths.Count -gt 0) {
        $env:PATH = ($realPaths -join ';') + ';' + $env:PATH
    }
}
