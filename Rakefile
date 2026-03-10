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
end

desc 'deploy index.html'
task :deploy => [:generate] do
  require 'net/scp'
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/index.html", "/home/ubuntu/www/hsbt.org/index.html")
  Net::SCP.upload!('www.hsbt.org', 'ubuntu', "hsbt.org/dist/hsbt.css", "/home/ubuntu/www/hsbt.org/stylesheets/hsbt.css")
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
    k, _ = line.split(",").map(&:chomp)
    system "launchctl bootout gui/#{uid} #{File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")}"
    FileUtils.rm File.expand_path("~/Library/LaunchAgents/#{k}.SetEnv.plist")
  end
end

RAILS_VERSIONS = {
  "8.1" => "rails81",
  "8.0" => "rails80",
  "7.2" => "rails72",
  "7.1" => "rails71",
  "7.0" => "rails70",
  "6.1" => "rails61",
}

namespace :boilerplate do
  desc "regenerate all boilerplate Rails apps with latest patch versions"
  task :update do
    RAILS_VERSIONS.each do |version, dir|
      dest = File.expand_path("boilerplate/#{dir}", __dir__)
      puts "==> Recreating #{dir} (Rails ~> #{version})"

      FileUtils.rm_rf(dest)
      system("gem install rails -v '~> #{version}.0' --no-document", exception: true)
      installed = `gem list -e rails`.match(/rails \(([^)]+)\)/)[1]
      latest = installed.split(", ").select { |v| v.start_with?(version) }.first
      system("rails", "_#{latest}_", "new", dest, "--skip-git", "--skip-docker", exception: true)

      puts "    #{dir} created"
    end
  end
end
