#!/usr/bin/env ruby
# frozen_string_literal: true

require 'bundler/inline'
require 'yaml'
require 'fileutils'
require 'tmpdir'

gemfile do
  source 'https://rubygems.org'
  gem 'octokit'
end

# Configuration
REPO_URL = 'https://github.com/ruby/git.ruby-lang.org.git'
ACCOUNT_TABLE = {
  unak: "usa",
  ioquatix: "samuel",
  "k-tsj": "ktsj",
  junaruga: "jaruga",
  mmasaki: "glass",
  yuki24: "yuki",
  BurdetteLamar: "burdettelamar",
  jemmaissroff: "jemma",
  peterzhu2118: "peter.zhu",
  KJTsanaktsidis: "kjtsanaktsidis",
  XrXr: "alanwu",
  amatsuda: "a_matsuda",
  jeremyevans: "jeremy",
  kateinoigakukun: "katei",
  nurse: "naruse",
  rhenium: "rhe",
  znz: "kazu"
}

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
  
  [tmp_dir, authorized_keys_path, email_config_path]
end

def extract_svn_account_names(authorized_keys_path)
  account_names = []
  File.readlines(authorized_keys_path).each do |line|
    # Updated pattern to include dots and hyphens in account names
    if match = line.match(/SVN_ACCOUNT_NAME=([\w.-]+)/)
      account_names << match[1]
    end
  end
  account_names.uniq
end

def read_email_config(email_config_path)
  YAML.load_file(email_config_path).keys
rescue => e
  puts "Error reading email configuration: #{e.message}"
  exit 1
end

def get_ruby_committers
  client = Octokit::Client.new(access_token: ENV['GITHUB_TOKEN'])
  client.auto_paginate = true # Enable auto pagination
  
  team = client.organization_teams('ruby').find { |t| t.name == 'ruby-committers' }
  return [] unless team

  members = client.team_members(team.id)
  members.map(&:login).map do |login|
    ACCOUNT_TABLE[login.to_sym] || login
  end
rescue Octokit::Error => e
  puts "Error fetching ruby-committers: #{e.message}"
  []
end

def main
  tmp_dir, authorized_keys_path, email_config_path = setup_repository

  begin
    puts "Reading account names from authorized_keys..."
    svn_accounts = extract_svn_account_names(authorized_keys_path)
    puts "Found #{svn_accounts.size} SVN account names"

    puts "\nReading accounts from email.yml..."
    email_accounts = read_email_config(email_config_path)
    puts "Found #{email_accounts.size} email.yml accounts"

    puts "\nFetching ruby-committers team members..."
    committers = get_ruby_committers
    puts "Found #{committers.size} ruby-committers members"

    both_accounts = (svn_accounts & email_accounts).sort

    puts "\nAccounts in ruby-committers but not in both email.yml and authorized_keys:"
    (committers - both_accounts).sort.each do |account|
      puts "  - #{account}"
    end

    puts "\nAccounts in both email.yml and authorized_keys but not in ruby-committers:"
    (both_accounts - committers).sort.each do |account|
      puts "  - #{account}"
    end

    puts "\nAccounts present in all three (ruby-committers, email.yml, and authorized_keys):"
    (both_accounts & committers).sort.each do |account|
      puts "  - #{account}"
    end
  ensure
    FileUtils.remove_entry_secure(tmp_dir) if tmp_dir && Dir.exist?(tmp_dir)
    puts "\nTemporary directory cleaned up."
  end
end

main if $PROGRAM_NAME == __FILE__
