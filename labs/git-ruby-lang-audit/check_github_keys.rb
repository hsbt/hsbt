#!/usr/bin/env ruby
# frozen_string_literal: true

require 'bundler/inline'
require 'yaml'
require 'fileutils'
require 'tmpdir'

gemfile do
  source 'https://rubygems.org'
  gem 'octokit', '~> 4.0'
  gem 'base64'  # Ruby 3.5以降ではbase64は標準ライブラリから外されたので追加
  gem 'faraday-retry' # Faraday v2.0以降でリトライ機能を使うために必要
end

# Configuration
GITHUB_TOKEN = ENV['GITHUB_TOKEN'] # Set your GitHub token as an environment variable
TEAM_SLUG = 'ruby-committers'
ORG_NAME = 'ruby'
REPO_URL = 'https://github.com/ruby/git.ruby-lang.org.git'

# Clone the repository and set paths relative to the cloned repo
def setup_repository
  tmp_dir = Dir.mktmpdir('git-ruby-lang-audit')
  puts "Cloning #{REPO_URL} into #{tmp_dir}..."
  
  unless system('git', 'clone', '--depth=1', REPO_URL, tmp_dir)
    puts "Error: Failed to clone repository: #{REPO_URL}"
    FileUtils.remove_entry_secure(tmp_dir) if Dir.exist?(tmp_dir)
    exit 1
  end
  
  authorized_keys_path = File.join(tmp_dir, 'recipes', 'files', 'var', 'git', '.ssh', 'authorized_keys')
  email_config_path = File.join(tmp_dir, 'config', 'email.yml')
  
  unless File.exist?(authorized_keys_path)
    puts "Error: Could not find authorized_keys file at #{authorized_keys_path}"
    FileUtils.remove_entry_secure(tmp_dir)
    exit 1
  end
  
  unless File.exist?(email_config_path)
    puts "Error: Could not find email.yml file at #{email_config_path}"
    FileUtils.remove_entry_secure(tmp_dir)
    exit 1
  end
  
  [tmp_dir, authorized_keys_path, email_config_path]
end

def fetch_team_id
  client = Octokit::Client.new(access_token: GITHUB_TOKEN)
  client.auto_paginate = true
  
  # Get all teams in the organization
  teams = client.organization_teams(ORG_NAME)
  
  # Find the team with the matching slug
  team = teams.find { |t| t.slug == TEAM_SLUG }
  if team
    team.id
  else
    puts "Error: Could not find team '#{TEAM_SLUG}' in organization '#{ORG_NAME}'"
    exit 1
  end
rescue Octokit::Error => e
  puts "Error fetching teams: #{e.message}"
  exit 1
end

def fetch_team_members
  client = Octokit::Client.new(access_token: GITHUB_TOKEN)
  client.auto_paginate = true
  
  team_id = fetch_team_id
  client.team_members(team_id)
rescue Octokit::Error => e
  puts "Error fetching team members: #{e.message}"
  exit 1
end

def fetch_user_keys(username)
  require 'open-uri'
  
  begin
    URI.open("https://github.com/#{username}.keys").read.lines.map(&:strip)
  rescue OpenURI::HTTPError => e
    puts "Error fetching keys for #{username}: #{e.message}"
    []
  end
end

def read_authorized_keys(path)
  File.read(path).lines.map(&:strip).reject(&:empty?)
rescue Errno::ENOENT
  puts "Error: Could not find authorized_keys file at #{path}"
  exit 1
end

def normalize_key(key)
  # Split the key string by spaces
  parts = key.split(/\s+/)
  
  # Extract key type and key data (the base64 encoded portion)
  # Standard format is: [options] key-type key-data [comment]
  if parts.size >= 2
    # Find the index where the key type appears (ssh-rsa, ssh-ed25519, etc.)
    type_index = parts.find_index { |part| part.match?(/^(ssh-rsa|ssh-dss|ssh-ed25519|ecdsa-sha2-nistp\d+)$/) }
    
    if type_index && parts[type_index + 1]
      # Return the key type and the key data
      "#{parts[type_index]} #{parts[type_index + 1]}"
    else
      # If we can't identify the key type properly, return an empty string
      # which will cause this key to be skipped in comparisons
      ""
    end
  else
    ""
  end
end

def read_email_config(path)
  YAML.load_file(path)
rescue Errno::ENOENT
  puts "Error: Could not find email configuration file at #{path}"
  exit 1
rescue => e
  puts "Error reading email configuration: #{e.message}"
  exit 1
end

def main
  unless GITHUB_TOKEN
    puts "Warning: GITHUB_TOKEN is not set. You might encounter API rate limits."
  end
  
  # Clone repository and get paths
  tmp_dir, authorized_keys_path, email_config_path = setup_repository
  
  begin
    puts "Fetching members of the #{TEAM_SLUG} team..."
    team_members = fetch_team_members
    puts "Found #{team_members.size} team members."
    
    authorized_keys = read_authorized_keys(authorized_keys_path)
    puts "Read #{authorized_keys.size} entries from authorized_keys file."
    
    # Normalize the authorized keys for comparison
    normalized_authorized_keys = authorized_keys.map do |line|
      # Find and extract the key part based on key type identifiers
      key_type_match = line.match(/(ssh-rsa|ssh-dss|ssh-ed25519|ecdsa-sha2-nistp\d+)/)
      if key_type_match
        # Extract from the key type onwards
        key_part = line[key_type_match.begin(0)..-1]
        normalize_key(key_part)
      else
        ""
      end
    end.reject(&:empty?)
  
    puts "Normalized #{normalized_authorized_keys.size} valid keys for comparison"
    
    missing_keys_users = []
    
    team_members.each do |member|
      username = member.login
      puts "Checking keys for #{username}..."
      user_keys = fetch_user_keys(username)
      
      if user_keys.empty?
        puts "  No keys found for #{username}"
        next
      end
      
      normalized_user_keys = user_keys.map { |key| normalize_key(key) }.reject(&:empty?)
      
      has_any_key = normalized_user_keys.any? do |user_key|
        normalized_authorized_keys.include?(user_key)
      end
      
      unless has_any_key
        missing_keys_users << username
        puts "  Missing keys for #{username}"
      end
    end
    
    if missing_keys_users.empty?
      puts "\nAll GitHub team members' keys are present in authorized_keys."
    else
      puts "\nThe following users have missing keys in authorized_keys:"
      missing_keys_users.each do |username|
        puts "  - #{username}"
      end
  
      # Check against email.yml entries
      puts "\nChecking users missing from authorized_keys against email.yml..."
      email_config = read_email_config(email_config_path)
      users_missing_from_email_yml = missing_keys_users.select do |username|
        !email_config.key?(username)
      end
  
      if users_missing_from_email_yml.empty?
        puts "All users with missing keys have entries in email.yml."
      else
        puts "Users missing from both authorized_keys and email.yml:"
        users_missing_from_email_yml.each do |username|
          puts "  - #{username}"
        end
      end
    end
  ensure
    # Clean up the temporary directory
    FileUtils.remove_entry_secure(tmp_dir) if tmp_dir && Dir.exist?(tmp_dir)
    puts "\nTemporary directory cleaned up."
  end
end

main if $PROGRAM_NAME == __FILE__