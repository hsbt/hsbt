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
OUTPUT_FILE = File.join(__dir__, "user_agents.txt")

# コマンドライン引数の処理
arg = ARGV[0]&.to_i || 0
# 引数が1の場合は2日間、2の場合は3日間など
days = arg + 1 
# User-Agent の出現回数の閾値
OCCURRENCE_THRESHOLD = 10
LOG_FILE_PATTERN = "other_vhosts_access.log*" # 処理対象のログファイルパターン

# 既存のlogsフォルダを削除
FileUtils.rm_rf(LOCAL_LOG_DIR)
FileUtils.mkdir_p(LOCAL_LOG_DIR)

puts "Running User-Agent analysis for #{days} days (threshold: #{OCCURRENCE_THRESHOLD} occurrences)"

begin
  Net::SSH.start(HOST, USER) do |ssh|
    puts "Connected to #{HOST}"
    
    # リモートのログファイル一覧を取得 (other_vhosts_access.log とそのrotateファイルのみ)
    all_files = ssh.exec!("ls -1 #{REMOTE_LOG_DIR}/#{LOG_FILE_PATTERN}").split("\n")
    puts "Found #{all_files.size} log files in total"
    
    # 日数に基づいてファイルをフィルタリング
    # Apacheのログローテーションでは通常:
    # .log - 現在のログ
    # .log.1 - 昨日のログ
    # .log.2.gz - 2日前のログ (圧縮済み)
    # というパターンになる
    files_to_download = all_files.select do |file|
      filename = File.basename(file)
      
      # 現在のログファイル (other_vhosts_access.log) は常に含める
      if filename == "other_vhosts_access.log"
        true
      # .log.1 は1日前のログファイル（2日以上のときに含める）
      elsif filename == "other_vhosts_access.log.1" && days >= 2
        true
      # 数字+.gzで終わるものは、その数字が「日付-1」を表す
      # 例: .log.2.gz は2日前のログなので3日以上指定されたら含める
      elsif filename =~ /other_vhosts_access\.log\.(\d+)\.gz/
        day_index = $1.to_i
        day_index <= days  # 指定日数以下の日付のログだけを含める
      else
        false
      end
    end
    
    puts "Downloading #{files_to_download.size} log files for #{days} days period..."
    
    # ファイルをダウンロード
    files_to_download.each do |file|
      # ファイル名だけを取得 (パスは含まない)
      filename = File.basename(file)
      remote_path = File.join(REMOTE_LOG_DIR, filename)
      local_path = File.join(LOCAL_LOG_DIR, filename)
      
      puts "Downloading #{filename}..."
      ssh.scp.download!(remote_path, local_path)
      puts "✓ Downloaded #{filename}"
    end
  end
  
  puts "\nSelected log files have been downloaded to #{LOCAL_LOG_DIR}"

  # User-Agent を解析してカウントする
  puts "\nAnalyzing log files to extract User-Agents..."
  
  user_agent_counts = Hash.new(0)
  
  # ダウンロードしたログファイルを処理
  Dir.glob(File.join(LOCAL_LOG_DIR, "*")).each do |log_file|
    next unless File.file?(log_file)
    # other_vhosts_access.log に関連するファイルのみを処理
    next unless File.basename(log_file).start_with?("other_vhosts_access.log")
    
    puts "Processing #{File.basename(log_file)}..."
    
    # ファイルが.gzで終わるかチェック
    is_gzipped = log_file.end_with?(".gz")
    
    # gzファイルかどうかによって読み込み方法を変更
    if is_gzipped
      Zlib::GzipReader.open(log_file) do |gz|
        gz.each_line do |line|
          # Apache ログから User-Agent を抽出
          # User-Agent は通常ダブルクォーテーションで囲まれている
          if line =~ /"([^"]*)" "([^"]*)"$/
            user_agent = $2
            user_agent_counts[user_agent] += 1
          end
        end
      end
    else
      # 通常のログファイルを処理
      File.readlines(log_file).each do |line|
        # Apache ログから User-Agent を抽出
        if line =~ /"([^"]*)" "([^"]*)"$/
          user_agent = $2
          user_agent_counts[user_agent] += 1
        end
      end
    end
  end
  
  # 特殊な値を除外（必要に応じて追加）
  user_agent_counts.delete_if { |user_agent, _| user_agent == "-" || user_agent.empty? }
  
  puts "Processed #{user_agent_counts.size} unique User-Agents"
  
  # 出現回数の多い順に並べる
  sorted_user_agents = user_agent_counts.sort_by { |_user_agent, count| -count }
  
  # 閾値以上の出現回数を持つUser-Agentを抽出
  high_occurrence_user_agents = sorted_user_agents.select { |_user_agent, count| count >= OCCURRENCE_THRESHOLD }
  
  puts "\n# Top User-Agents with #{OCCURRENCE_THRESHOLD}+ occurrences (#{days} days)"
  puts "# Generated on: #{Time.now}"
  puts "# Total User-Agents exceeding threshold: #{high_occurrence_user_agents.size}"
  puts ""
  
  # 上位のUser-Agentと出現回数を表示
  high_occurrence_user_agents.each do |user_agent, count|
    puts "#{count}\t#{user_agent}"
  end
  
  puts "\nSummary:"
  puts "- Total unique User-Agents analyzed: #{user_agent_counts.size}"
  puts "- User-Agents with #{OCCURRENCE_THRESHOLD}+ occurrences (#{days} days): #{high_occurrence_user_agents.size}"
  puts "- Top occurrence count: #{sorted_user_agents.first[1]} (User-Agent: #{sorted_user_agents.first[0]})"
  
  # 結果をuser_agents.txtに出力
  File.open(OUTPUT_FILE, "w") do |file|
    file.puts "# User-Agents sorted by occurrence count"
    file.puts "# Format: OCCURRENCE_COUNT USER_AGENT"
    file.puts "# Generated on: #{Time.now}"
    file.puts "# Analysis period: #{days} days (threshold: #{OCCURRENCE_THRESHOLD})"
    file.puts "# Total unique User-Agents: #{user_agent_counts.size}"
    file.puts "#"
    
    sorted_user_agents.each do |user_agent, count|
      file.puts "#{count}\t#{user_agent}"
    end
  end
  
  puts "\nResults have been written to #{OUTPUT_FILE}"
  puts "Top 10 User-Agents by occurrence count:"
  sorted_user_agents.first(10).each_with_index do |(user_agent, count), index|
    puts "#{index + 1}. '#{user_agent}' - #{count} occurrences"
  end

rescue Net::SSH::AuthenticationFailed
  puts "Authentication failed. Please check your SSH configuration."
rescue Errno::ECONNREFUSED
  puts "Connection refused. Please check if the host is reachable."
rescue StandardError => e
  puts "An error occurred: #{e.message}"
  puts e.backtrace.join("\n") if ENV["DEBUG"]
end