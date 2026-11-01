Import-Module -Name Terminal-Icons

Import-Module -Name PSReadLine

Invoke-Expression (& { (zoxide init --cmd j powershell | Out-String) })

Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

Set-PSReadLineKeyHandler -Key Tab -ScriptBlock { Invoke-FzfTabCompletion }
