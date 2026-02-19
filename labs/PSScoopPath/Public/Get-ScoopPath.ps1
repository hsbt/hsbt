function Get-ScoopPath {
    [CmdletBinding()]
    param(
        [ValidateSet('Root', 'Apps', 'Buckets', 'Shims', 'Persist')]
        [string]$Name = 'Root',

        [string]$ScoopHome,

        [string]$ScoopDir
    )

    Resolve-ScoopPath -Name $Name -ScoopHome $ScoopHome -ScoopDir $ScoopDir
}
