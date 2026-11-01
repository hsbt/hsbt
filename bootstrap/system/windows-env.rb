# The Windows counterpart of the exports in .zshenv. User environment
# variables reach shells and GUI apps alike once they restart. The V: paths
# are the data drive of this machine.
#
# XDG_CONFIG_HOME stays unset: every linked file sits at the default
# ~/.config already, and setting it would move gh's login out of %APPDATA%.
# XDG_CACHE_HOME is not %LOCALAPPDATA% because Claude Desktop redirects
# writes under AppData for the processes it starts.

require "win32/registry"

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
}.transform_values { |v| v.tr("/", "\\") }

current = Win32::Registry::HKEY_CURRENT_USER.open("Environment") do |reg|
  vars.to_h { |name, _| [name, (reg[name] rescue nil)] }
end

vars.each do |name, value|
  next if current[name] == value
  # setx also broadcasts the change, so new processes see it without a logoff
  system("setx", name, value, out: File::NULL, exception: true)
  puts "#{name}=#{value}"
end
