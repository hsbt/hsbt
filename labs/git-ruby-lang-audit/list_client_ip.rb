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
require "zlib"

HOST = "git.ruby-lang.org"
USER = "admin"
REMOTE_LOG_DIR = "/var/log/apache2"
LOCAL_LOG_DIR = File.join(__dir__, "logs")
OUTPUT_FILE = File.join(__dir__, "list.txt")
ACCESS_THRESHOLD = 10080 # 閾値を定数として定義

# 既存のlogsフォルダを削除
FileUtils.rm_rf(LOCAL_LOG_DIR)
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

  # クライアントのIPアドレスを解析してカウントする
  puts "\nAnalyzing log files to extract client IPs..."
  
  ip_counts = Hash.new(0)
  
  # ダウンロードしたログファイルを処理
  Dir.glob(File.join(LOCAL_LOG_DIR, "*")).each do |log_file|
    next unless File.file?(log_file)
    
    puts "Processing #{File.basename(log_file)}..."
    
    # ファイルが.gzで終わるかチェック
    is_gzipped = log_file.end_with?(".gz")
    
    # gzファイルかどうかによって読み込み方法を変更
    if is_gzipped
      Zlib::GzipReader.open(log_file) do |gz|
        gz.each_line do |line|
          # Apacheログフォーマットを正しく解析 - IPは2番目のフィールド
          if line =~ /^\S+\s+(\d+\.\d+\.\d+\.\d+|[0-9a-f:]+)\s/
            ip = $1
            ip_counts[ip] += 1
          end
        end
      end
    else
      # 通常のログファイルを処理
      File.readlines(log_file).each do |line|
        # Apacheログフォーマットを正しく解析 - IPは2番目のフィールド
        if line =~ /^\S+\s+(\d+\.\d+\.\d+\.\d+|[0-9a-f:]+)\s/
          ip = $1
          ip_counts[ip] += 1
        end
      end
    end
  end
  
  # 特殊な値を除外（必要に応じて追加）
  ip_counts.delete_if { |ip, _| ip == "-" } # "-" はIPアドレスではない
  
  puts "Processed #{ip_counts.size} unique IP addresses"
  
  # アクセス数の多い順に並べる
  sorted_ips = ip_counts.sort_by { |_ip, count| -count }
  
  # 閾値以上のアクセス数を持つIPを抽出
  high_access_ips = sorted_ips.select { |_ip, count| count >= ACCESS_THRESHOLD }
  
  puts "\n# Apache deny configuration for IPs with #{ACCESS_THRESHOLD}+ accesses"
  puts "# Generated on: #{Time.now}"
  puts "# Total IPs exceeding threshold: #{high_access_ips.size}"
  puts ""
  
  # "Deny from XX.XX.XX.XX" 形式で出力
  high_access_ips.each do |ip, count|
    puts "Deny from #{ip}  # #{count} accesses"
  end
  
  puts "\nSummary:"
  puts "- Total unique IPs analyzed: #{ip_counts.size}"
  puts "- IPs with #{ACCESS_THRESHOLD}+ accesses: #{high_access_ips.size}"
  puts "- Top access count: #{sorted_ips.first[1]} (IP: #{sorted_ips.first[0]})"
  
  # 結果をlist.txtに出力
  File.open(OUTPUT_FILE, "w") do |file|
    file.puts "# Remote IP addresses sorted by access count"
    file.puts "# Format: IP_ADDRESS ACCESS_COUNT"
    file.puts "# Generated on: #{Time.now}"
    file.puts "# Total unique IP addresses: #{ip_counts.size}"
    file.puts "#"
    
    sorted_ips.each do |ip, count|
      file.puts "#{ip} #{count}"
    end
  end
  
  puts "\nResults have been written to #{OUTPUT_FILE}"
  puts "Top 10 IP addresses by access count:"
  sorted_ips.first(10).each_with_index do |(ip, count), index|
    puts "#{index + 1}. #{ip} - #{count} accesses"
  end

rescue Net::SSH::AuthenticationFailed
  puts "Authentication failed. Please check your SSH configuration."
rescue Errno::ECONNREFUSED
  puts "Connection refused. Please check if the host is reachable."
rescue StandardError => e
  puts "An error occurred: #{e.message}"
  puts e.backtrace.join("\n") if ENV["DEBUG"]
end