$publicDir = Join-Path $PSScriptRoot 'Public'
$privateDir = Join-Path $PSScriptRoot 'Private'

Get-ChildItem -Path $privateDir -Filter '*.ps1' -ErrorAction SilentlyContinue |
    ForEach-Object { . $_.FullName }

Get-ChildItem -Path $publicDir -Filter '*.ps1' -ErrorAction SilentlyContinue |
    ForEach-Object { . $_.FullName }

$publicFunctions = Get-ChildItem -Path $publicDir -Filter '*.ps1' -ErrorAction SilentlyContinue |
    ForEach-Object { $_.BaseName }

Export-ModuleMember -Function $publicFunctions

try {
    Add-ScoopRealPathsToPath
} catch {
    # Ignore initialization errors to avoid breaking module import.
}
