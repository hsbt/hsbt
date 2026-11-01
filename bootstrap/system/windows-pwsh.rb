# winget installs PowerShell 7 as an MSIX, which fails to start from an ssh
# logon, and `winget import` cannot ask for another installer type. Claude
# Code looks for the MSI one in Program Files, so a Claude Desktop session
# over SSH needs it.

exit if File.exist?('C:\Program Files\PowerShell\7\pwsh.exe')

system("winget", "install", "--id", "Microsoft.PowerShell", "--exact", "--source", "winget",
  "--installer-type", "wix", "--accept-package-agreements", "--accept-source-agreements",
  exception: true)
