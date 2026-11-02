# The Windows counterpart of the exports in .zshenv. User environment
# variables reach shells and GUI apps alike once they restart. The V: paths
# are the data drive of this machine.
#
# XDG_CONFIG_HOME stays unset: every linked file sits at the default
# ~/.config already, and setting it would move gh's login out of %APPDATA%.
# XDG_CACHE_HOME is not %LOCALAPPDATA% because Claude Desktop redirects
# writes under AppData for the processes it starts. Go and uv default to
# AppData as well, and mise points an unset GOBIN at the directory of one Go
# version, which the next upgrade leaves behind.

require "win32/registry"
require "fiddle/import"

module User32
  extend Fiddle::Importer
  dlload "user32.dll"
  extern "long SendMessageTimeoutW(void*, unsigned int, void*, void*, unsigned int, unsigned int, void*)"

  HWND_BROADCAST = 0xffff
  WM_SETTINGCHANGE = 0x1a
  SMTO_ABORTIFHUNG = 0x2
end

home = ENV.fetch("USERPROFILE")
config = File.join(home, ".config")
data = File.join(home, ".local", "share")
cache = File.join(home, ".cache")

vars = {
  "XDG_DATA_HOME" => data,
  "XDG_CACHE_HOME" => cache,
  "GEMRC" => File.join(config, "gem", "gemrc"),
  "IRBRC" => File.join(config, "irb", "irbrc"),
  "BUNDLE_USER_CONFIG" => File.join(config, "bundle"),
  "BUNDLE_USER_CACHE" => File.join(cache, "bundle"),
  "BUNDLE_USER_PLUGIN" => File.join(data, "bundle"),
  "RUBOCOP_OPTS" => "--config #{File.join(config, "rubocop", "config.yml")}",
  "EDITOR" => "hx",
  "RUBY_YJIT_ENABLE" => "1",
  "CLAUDE_CODE_GIT_BASH_PATH" => File.join(home, "scoop", "apps", "msys2", "current", "usr", "bin", "bash.exe"),
  "VCPKG_DEFAULT_BINARY_CACHE" => "V:/scoop.persist/vcpkg",
  "LLAMA_CACHE" => "V:/llm/models",
  "GOPATH" => File.join(data, "go"),
  "GOBIN" => File.join(data, "go", "bin"),
  "GOCACHE" => File.join(cache, "go-build"),
  "GOMODCACHE" => File.join(cache, "go-mod"),
  "NPM_CONFIG_USERCONFIG" => File.join(config, "npm", "npmrc"),
  "UV_CACHE_DIR" => File.join(cache, "uv"),
  "UV_PYTHON_INSTALL_DIR" => File.join(data, "uv", "python"),
  "UV_TOOL_DIR" => File.join(data, "uv", "tools"),
}.transform_values { |v| v.tr("/", "\\") }

# npm on Windows puts global commands in the prefix itself, not in its bin.
paths = [File.join(data, "mise", "shims"), vars["GOBIN"], File.join(data, "npm")].map { |v| v.tr("/", "\\") }

current = Win32::Registry::HKEY_CURRENT_USER.open("Environment") do |reg|
  vars.to_h { |name, _| [name, (reg[name] rescue nil)] }
end

vars.each do |name, value|
  next if current[name] == value
  # setx also broadcasts the change, so new processes see it without a logoff
  system("setx", name, value, out: File::NULL, exception: true)
  puts "#{name}=#{value}"
end

# setx cuts a value at 1024 characters, so Path goes to the registry directly
# and is announced the way setx announces its changes.
Win32::Registry::HKEY_CURRENT_USER.open("Environment", Win32::Registry::KEY_ALL_ACCESS) do |reg|
  type, value = reg.read("Path")
  entries = value.split(";")
  missing = paths.reject { |path| entries.any? { |entry| entry.casecmp?(path) } }
  next if missing.empty?
  reg.write("Path", type, (missing + entries).join(";"))
  User32.SendMessageTimeoutW(User32::HWND_BROADCAST, User32::WM_SETTINGCHANGE, 0,
    "Environment\0".encode("UTF-16LE"), User32::SMTO_ABORTIFHUNG, 5000, nil)
  puts "Path=#{missing.join(";")};..."
end
