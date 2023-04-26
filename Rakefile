require "rake"
require "fileutils"

desc 'generate index.html'
task :generate do
  require 'kramdown'
  header = File.read('hsbt.org/src/header.html')
  body = Kramdown::Document.new(File.read('hsbt.org/src/index.md')).to_html
  footer = File.read('hsbt.org/src/footer.html')
  File.open('hsbt.org/dist/index.html', 'w') do |f|
    [header, body, footer].each {|s| f.puts s}
  end

  require 'sass-embedded'
  File.open('hsbt.org/dist/hsbt.css', 'w') do |f|
    f.puts Sass.compile('hsbt.org/src/hsbt.scss').css
  end
end

desc 'deploy index.html'
task :deploy => [:generate] do
  require 'net/scp'
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/index.html", "/home/ubuntu/www/hsbt.org/index.html")
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/hsbt.css", "/home/ubuntu/www/hsbt.org/stylesheets/hsbt.css")
end

XDG_CONFIG_FILES = Dir.glob(File.join("toolbox", "xdg", "config", "**", "*")).select{|f| !File.directory?(f)}

task :push do
  XDG_CONFIG_FILES.each do |f|
    from = f
    to = f.gsub(/toolbox\/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end
end

task :pull do
  XDG_CONFIG_FILES.each do |f|
    to = f
    from = f.gsub(/toolbox\/xdg\/config/, ENV['XDG_CONFIG_HOME'])
    FileUtils.cp from, to
  end
end

task :push_system_env do
  system "sudo cp toolbox/system/paths /etc/paths.d/paths"
  system "sudo sd 'HOME' #{ENV.fetch('HOME')} /etc/paths.d/paths"

  uid = `id -u`.chomp
  File.open("toolbox/system/env").each do |line|
    k, v = line.split(",").map(&:chomp)
    v.gsub!("HOME", ENV.fetch('HOME'))
    plist = <<~EOS
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
      <dict>
        <key>Label</key>
        <string>#{k}.SetEnv</string>
        <key>ProgramArguments</key>
        <array>
          <string>/bin/launchctl</string>
          <string>setenv</string>
          <string>#{k}</string>
          <string>#{v}</string>
        </array>
        <key>RunAtLoad</key>
        <true/>
      </dict>
    </plist>
    EOS
    system "launchctl bootout gui/#{uid} #{File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")}"
    File.open(File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist"), "w") do |f|
      f.puts plist
    end
    system "launchctl bootstrap gui/#{uid} #{File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")}"
  end
end

task :purge_system_env do
  system "sudo rm /etc/paths.d/paths"
  uid = `id -u`.chomp
  File.open("toolbox/system/env").each do |line|
    k, v = line.split(",").map(&:chomp)
    system "launchctl bootout gui/#{uid} #{File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")}"
    FileUtils.rm File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")
  end
end
