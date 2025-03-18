#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'tmpdir'
require 'time'
require 'base64'

# Configuration
REPO_URL = 'https://github.com/ruby/git.ruby-lang.org.git'

# 2025年時点での推奨設定
RECOMMENDED_ALGORITHMS = {
  'ssh-ed25519' => true,          # Always secure
  'ecdsa-sha2-nistp384' => true,  # NIST P-384 is secure
  'ecdsa-sha2-nistp521' => true   # NIST P-521 is secure
}

WEAK_ALGORITHMS = [
  'ssh-dss',                # DSA is considered weak
  'ecdsa-sha2-nistp256',   # NIST P-256 is not recommended for future use
]

def setup_repository
  tmp_dir = Dir.mktmpdir('git-ruby-lang-audit')
  puts "Cloning #{REPO_URL} into #{tmp_dir}..."
  
  unless system('git', 'clone', '--depth=1', REPO_URL, tmp_dir)
    puts "Error: Failed to clone repository: #{REPO_URL}"
    FileUtils.remove_entry_secure(tmp_dir) if Dir.exist?(tmp_dir)
    exit 1
  end
  
  authorized_keys_path = File.join(tmp_dir, 'recipes', 'files', 'var', 'git', '.ssh', 'authorized_keys')
  [tmp_dir, authorized_keys_path]
end

def extract_key_type_and_data_from_line(line)
  parts = line.sub(/^environment="[^"]+"/, '').strip.split(/\s+/)
  return [nil, nil] if parts.size < 2
  
  [parts[0], parts[1]]
end

def get_rsa_key_bits(base64_key)
  begin
    decoded = Base64.decode64(base64_key)
    # RSA公開鍵のフォーマットに従って解析
    # 最初の4バイトはサイズ、その後に"ssh-rsa"という文字列が続く
    # その後にexponentとmodulusが続く
    # modulusのサイズ（ビット長）を計算する
    parts = decoded.unpack('N*')
    length_ssh_rsa = parts[0]
    offset = 4 + length_ssh_rsa
    length_e = decoded[offset, 4].unpack('N')[0]
    offset += 4 + length_e
    length_n = decoded[offset, 4].unpack('N')[0]
    
    # モジュラスのビット長を計算
    length_n * 8
  rescue
    0  # デコードに失敗した場合は0を返す
  end
end

def check_key_strength(line)
  key_type, key_data = extract_key_type_and_data_from_line(line)
  return :unknown unless key_type

  if RECOMMENDED_ALGORITHMS[key_type]
    return :strong
  elsif WEAK_ALGORITHMS.include?(key_type)
    return :weak
  elsif key_type == 'ssh-rsa'
    # RSAは3072ビット以上が必要（2025年以降）
    bits = get_rsa_key_bits(key_data)
    return bits >= 3072 ? :strong : :weak
  else
    return :unknown
  end
end

def analyze_keys(authorized_keys_path)
  weak_keys = []
  strong_keys = []
  unknown_keys = []

  File.readlines(authorized_keys_path).each do |line|
    next if line.strip.empty? || line.start_with?('#')
    
    account_match = line.match(/SVN_ACCOUNT_NAME=([\w.-]+)/)
    account_name = account_match ? account_match[1] : 'unknown'
    
    key_type, key_data = extract_key_type_and_data_from_line(line)
    strength = check_key_strength(line)
    key_info = { account: account_name, key_type: key_type, line: line.strip }
    
    case strength
    when :strong
      strong_keys << key_info
    when :weak
      weak_keys << key_info
    else
      unknown_keys << key_info
    end
  end
  
  [weak_keys, strong_keys, unknown_keys]
end

def main
  tmp_dir, authorized_keys_path = setup_repository

  begin
    puts "Analyzing SSH public keys in authorized_keys..."
    weak_keys, strong_keys, unknown_keys = analyze_keys(authorized_keys_path)

    puts "\nKeys with insufficient strength for 2025:"
    weak_keys.each do |key|
      puts "  Account: #{key[:account]}"
      puts "  Type: #{key[:key_type]}"
      puts "  Key: #{key[:line]}"
      puts
    end

    puts "\nKeys with sufficient strength:"
    strong_keys.each do |key|
      puts "  Account: #{key[:account]}"
      puts "  Type: #{key[:key_type]}"
    end

    if unknown_keys.any?
      puts "\nKeys with unknown algorithm:"
      unknown_keys.each do |key|
        puts "  Account: #{key[:account]}"
        puts "  Type: #{key[:key_type]}"
        puts "  Key: #{key[:line]}"
        puts
      end
    end

    puts "\nSummary:"
    puts "  Total keys analyzed: #{weak_keys.size + strong_keys.size + unknown_keys.size}"
    puts "  Strong keys: #{strong_keys.size}"
    puts "  Weak keys: #{weak_keys.size}"
    puts "  Unknown algorithm: #{unknown_keys.size}"

    if weak_keys.any?
      puts "\nUsers with weak keys:"
      weak_users = weak_keys.group_by { |key| key[:account] }
      weak_users.keys.sort.each do |user|
        key_types = weak_users[user].map { |k| k[:key_type] }.uniq.join(', ')
        puts "  #{user} (#{key_types})"
      end
    end
  ensure
    FileUtils.remove_entry_secure(tmp_dir) if tmp_dir && Dir.exist?(tmp_dir)
    puts "\nTemporary directory cleaned up."
  end
end

main if $PROGRAM_NAME == __FILE__