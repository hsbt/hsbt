#!/usr/bin/env ruby

require 'fileutils'

# 引数チェック
if ARGV.length != 3
  puts "Usage: #{$0} <gem-name> <start-version> <end-version>"
  puts "Example: #{$0} rubygems 4.0.4 4.0.6"
  exit 1
end

gem_name = ARGV[0]
start_version = ARGV[1]
end_version = ARGV[2]

# 実行ディレクトリからの相対パス
repo_path = File.expand_path("../#{gem_name}", Dir.pwd)

unless Dir.exist?(repo_path)
  puts "Error: Repository directory not found: #{repo_path}"
  puts "Expected path: #{repo_path}"
  exit 1
end

# 利用可能なタグを取得
def get_tags(repo_path)
  Dir.chdir(repo_path) do
    tags = `git tag`.split("\n")
    # "v" で始まるタグをフィルタリングして、バージョン番号を抽出
    tags.select { |tag| tag.start_with?('v') }
        .map { |tag| tag.sub(/^v/, '') }
        .select { |version| version.match?(/^\d+\.\d+\.\d+$/) }
  end
end

# バージョンを比較
def compare_versions(v1, v2)
  v1_parts = v1.split('.').map(&:to_i)
  v2_parts = v2.split('.').map(&:to_i)
  
  [v1_parts.length, v2_parts.length].max.times do |i|
    part1 = v1_parts[i] || 0
    part2 = v2_parts[i] || 0
    return -1 if part1 < part2
    return 1 if part1 > part2
  end
  0
end

# タグを取得してフィルタリング
puts "Fetching tags from #{repo_path}..."
tags = get_tags(repo_path)
target_versions = tags.select do |version|
  compare_versions(version, start_version) >= 0 && 
  compare_versions(version, end_version) <= 0
end.sort { |a, b| compare_versions(a, b) }

if target_versions.empty?
  puts "Error: No versions found in range #{start_version}..#{end_version}"
  exit 1
end

puts "Processing versions: #{target_versions.join(', ')}"

# 各バージョンを処理
target_versions.each do |version|
  puts "\n=== Processing version #{version} ==="
  
  # 1. git checkout in the gem repository
  puts "Checking out v#{version} in #{repo_path}..."
  Dir.chdir(repo_path) do
    unless system("git checkout v#{version}")
      puts "Error: Failed to checkout v#{version}"
      exit 1
    end
  end
  
  # 2. Run sync_default_gems.rb in the current directory
  puts "Running tool/sync_default_gems.rb #{gem_name}..."
  unless system("tool/sync_default_gems.rb #{gem_name}")
    puts "Error: Failed to run sync_default_gems.rb"
    exit 1
  end
  
  # 3. git commit in the current directory
  commit_message = "Merge RubyGems/Bundler #{version}"
  puts "Committing with message: #{commit_message}..."
  unless system("git", "commit", "-am", commit_message)
    puts "Error: Failed to commit"
    exit 1
  end
  
  puts "✓ Successfully processed version #{version}"
end

puts "\n=== All versions processed successfully ==="
