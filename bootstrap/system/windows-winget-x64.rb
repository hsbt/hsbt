# Deskflow and Dropbox belong to the x64 desktop only. The ARM64 machine is a
# build server. See windows-vcredist.rb for why the architecture comes from
# the registry.

require "win32/registry"

key = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
arch = Win32::Registry::HKEY_LOCAL_MACHINE.open(key) { |reg| reg["PROCESSOR_ARCHITECTURE"] }

exit unless arch == "AMD64"

system("winget", "import", "-i", "winget-x64.json", "--no-upgrade", exception: true)
