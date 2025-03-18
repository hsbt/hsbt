#!/usr/bin/env ruby

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "net-ssh"
  gem "net-scp"
  gem "logger"
  gem "ed25519"
  gem "bcrypt_pbkdf"
end

require "fileutils"

HOST = "git.ruby-lang.org"
USER = "admin"
REMOTE_LOG_DIR = "/var/log/apache2"
LOCAL_LOG_DIR = File.join(__dir__, "logs")

FileUtils.mkdir_p(LOCAL_LOG_DIR)

begin
  Net::SSH.start(HOST, USER) do |ssh|
    puts "Connected to #{HOST}"
    
    # リモートのログファイル一覧を取得
    files = ssh.exec!("ls -1 #{REMOTE_LOG_DIR}").split("\n")
    puts "Found #{files.size} log files"
    
    # ファイルをダウンロード
    files.each do |file|
      remote_path = File.join(REMOTE_LOG_DIR, file)
      local_path = File.join(LOCAL_LOG_DIR, file)
      
      puts "Downloading #{file}..."
      ssh.scp.download!(remote_path, local_path)
      puts "✓ Downloaded #{file}"
    end
  end
  
  puts "\nAll log files have been downloaded to #{LOCAL_LOG_DIR}"
rescue Net::SSH::AuthenticationFailed
  puts "Authentication failed. Please check your SSH configuration."
rescue Errno::ECONNREFUSED
  puts "Connection refused. Please check if the host is reachable."
rescue StandardError => e
  puts "An error occurred: #{e.message}"
end