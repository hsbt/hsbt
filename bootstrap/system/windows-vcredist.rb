# Deskflow's installer refuses an older VC++ runtime, and the
# `winget import --no-upgrade` after this leaves an installed one as it is.
# winget install upgrades it. Its exit status goes unchecked because winget
# exits non-zero when the runtime is already current and mswin Ruby sees only
# the low byte of that code. A runtime still too old fails Deskflow's install.
#
# x64 builds run emulated on ARM64 (the rbmanager Ruby too) and need the x64
# runtime, and emulation reports PROCESSOR_ARCHITECTURE as AMD64, so the
# machine's own architecture is read from the registry.

require "win32/registry"

key = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
arch = Win32::Registry::HKEY_LOCAL_MACHINE.open(key) { |reg| reg["PROCESSOR_ARCHITECTURE"] }

ids = ["Microsoft.VCRedist.2015+.x64"]
ids << "Microsoft.VCRedist.2015+.arm64" if arch == "ARM64"

ids.each do |id|
  system("winget", "install", "--id", id, "--exact", "--source", "winget",
    "--accept-package-agreements", "--accept-source-agreements")
end
