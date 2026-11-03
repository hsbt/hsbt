# winget's Google.Chrome downloads an MSI from a URL without a version, so its
# hash check fails whenever Google ships a release before the manifest catches
# up, and Google.Chrome.EXE installs a per-user Chrome without the
# GoogleUpdater that would keep it current. So the enterprise MSI is fetched
# here with its signature checked in place of the hash, and GoogleUpdater
# keeps Chrome current from then on. See windows-vcredist.rb for why the
# architecture comes from the registry.

require "win32/registry"
require "tmpdir"

chrome = 'C:\Program Files\Google\Chrome\Application\chrome.exe'
exit if File.exist?(chrome)

key = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
arch = Win32::Registry::HKEY_LOCAL_MACHINE.open(key) { |reg| reg["PROCESSOR_ARCHITECTURE"] }

name = arch == "ARM64" ? "googlechromestandaloneenterprise_arm64.msi" : "googlechromestandaloneenterprise64.msi"
msi = File.join(Dir.tmpdir, name)

system("curl.exe", "-fsSL", "-o", msi, "https://dl.google.com/dl/chrome/install/#{name}", exception: true)

# PSModulePath inherited from PowerShell 7 names its modules, which Windows
# PowerShell fails to load in place of its own Get-AuthenticodeSignature.
script = '$s = Get-AuthenticodeSignature -LiteralPath $env:CHROME_MSI; "$($s.Status) $($s.SignerCertificate.Subject)"'
signature = IO.popen({"CHROME_MSI" => msi, "PSModulePath" => nil}, ["powershell", "-NoProfile", "-NonInteractive", "-Command", script], &:read)
abort "#{name}: unexpected signature #{signature}" unless signature.start_with?("Valid CN=Google LLC,")

# msiexec exits 3010 when it wants a reboot and mswin Ruby sees only the low
# byte of that, so the result is judged by the installed file instead.
system("msiexec", "/i", msi.tr("/", "\\"), "/passive", "/norestart")
File.delete(msi)
abort "Chrome did not install" unless File.exist?(chrome)
