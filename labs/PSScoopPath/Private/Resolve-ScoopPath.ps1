function Resolve-ScoopPath {
    [CmdletBinding()]
    param(
        [ValidateSet('Root', 'Apps', 'Buckets', 'Shims', 'Persist')]
        [string]$Name = 'Root',

        [string]$ScoopHome,

        [string]$ScoopDir
    )

    $root = if ($ScoopDir) {
        $ScoopDir
    } elseif ($env:SCOOP) {
        $env:SCOOP
    } else {
        Join-Path $env:USERPROFILE 'scoop'
    }

    $apps = if ($ScoopHome) {
        $ScoopHome
    } elseif ($env:SCOOP_HOME) {
        $env:SCOOP_HOME
    } else {
        Join-Path $root 'apps'
    }

    switch ($Name) {
        'Root' { return $root }
        'Apps' { return $apps }
        'Buckets' { return (Join-Path $root 'buckets') }
        'Shims' { return (Join-Path $root 'shims') }
        'Persist' { return (Join-Path $root 'persist') }
    }
}
