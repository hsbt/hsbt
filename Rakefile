require "rake"
require "fileutils"

XDG_CONFIG_FILES = Dir.glob(File.join("xdg", "config", "**", "*")).select{|f| !File.directory?(f)}

task :push do
  XDG_CONFIG_FILES.each do |f|
    from = f
    to = f.gsub(/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end
end

task :pull do
  XDG_CONFIG_FILES.each do |f|
    to = f
    from = f.gsub(/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end
end
