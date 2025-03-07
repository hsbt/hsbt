#!/usr/bin/env ruby
# frozen_string_literal: true

# Use bundler/inline to ensure we have the required gems
require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "base64"
  gem "octokit", "~> 6.0", require: true
  gem "terminal-table", "~> 3.0"
end

require "optparse"
require "yaml"
require "pathname"

class GitHubActionsUpdater
  DEFAULT_WORKFLOW_DIR = ".github/workflows"

  attr_reader :dry_run, :verbose, :workflow_files, :target_actions

  def initialize(options = {})
    @dry_run = options[:dry_run]
    @verbose = options[:verbose]
    @workflow_files = options[:workflow_files] || []
    @target_actions = options[:target_actions] || []
    @latest_versions_cache = {}

    # Initialize the GitHub client with token if provided
    token = options[:token] || ENV["GITHUB_TOKEN"]
    @client = if token
      Octokit::Client.new(access_token: token)
    else
      Octokit::Client.new
    end

    @client.auto_paginate = true
    # Use a higher per_page to reduce API calls
    @client.per_page = 100

    if @verbose && token.nil?
      puts "Warning: No GitHub token provided. API requests may be rate limited."
    end

    # If no workflow files specified, use all files in the default directory
    if @workflow_files.empty?
      @workflow_files = find_all_workflow_files
    end
  end

  def run
    if @workflow_files.empty?
      puts "No workflow files found in #{DEFAULT_WORKFLOW_DIR} directory."
      exit 1
    end

    # Validate all specified files exist
    validate_workflow_files

    puts "Checking GitHub Actions in #{@workflow_files.size} workflow file(s)..."

    # Find all actions with hash-based versions in specified workflow files
    hash_based_actions = find_hash_based_actions

    if hash_based_actions.empty?
      puts "No GitHub Actions with hash-based versions found in specified workflow files."
      return
    end

    puts "Found #{hash_based_actions.size} GitHub Actions with hash-based versions:"
    hash_based_actions.each do |action, versions|
      plural = (versions.size > 1) ? "versions" : "version"
      puts "  - #{action} (#{versions.size} hash-based #{plural})"
    end

    # Fetch latest versions for all identified actions
    puts "\nFetching latest versions for these actions..."
    action_repos = hash_based_actions.keys

    action_repos.each do |action|
      latest_version = fetch_latest_version(action)
      if latest_version
        @latest_versions_cache[action] = latest_version
        puts "Latest version of #{action}: #{latest_version}"
      else
        puts "Warning: Could not determine the latest version of #{action}"
      end
    end

    # Update the workflow files
    update_workflow_files(hash_based_actions)
  end

  def find_all_workflow_files
    workflow_dir = File.join(Dir.pwd, DEFAULT_WORKFLOW_DIR)
    unless Dir.exist?(workflow_dir)
      puts "Workflow directory not found at #{workflow_dir}"
      return []
    end

    Dir.glob(File.join(workflow_dir, "*.{yml,yaml}")).sort
  end

  def validate_workflow_files
    invalid_files = @workflow_files.reject { |file| File.exist?(file) }
    if invalid_files.any?
      puts "Error: The following workflow files do not exist:"
      invalid_files.each { |file| puts "  - #{file}" }
      exit 1
    end
  end

  def find_hash_based_actions
    hash_based_actions = {}
    sha_pattern = /^[0-9a-f]{40}$/

    @workflow_files.each do |file|
      content = File.read(file)

      # Find all 'uses: action@hash' patterns
      content.scan(/uses:\s+([^@\s]+)@([^\s#]+)(?:\s+#\s+(.+))?/) do |match|
        action = match[0]
        version = match[1]
        comment = match[2]

        # Skip if target actions are specified and this action is not included
        next if @target_actions.any? && !@target_actions.include?(action)

        # Check if it's a SHA hash (40 hex characters)
        if version&.match?(sha_pattern)
          hash_based_actions[action] ||= Set.new
          hash_based_actions[action] << "#{version}#{comment ? " # #{comment}" : ""}"
        end
      end
    rescue => e
      puts "Error processing #{file}: #{e.message}" if verbose
    end

    # Convert the Sets to Arrays
    hash_based_actions.transform_values(&:to_a)
  end

  def fetch_latest_version(action_repo)
    return @latest_versions_cache[action_repo] if @latest_versions_cache.key?(action_repo)

    puts "Fetching the latest version of #{action_repo}..." if verbose

    begin
      # Get latest release
      latest_release = @client.latest_release(action_repo)
      latest_tag = latest_release.tag_name

      # Get the reference for this tag to find its SHA
      tag_ref = @client.ref(action_repo, "tags/#{latest_tag}")
      if tag_ref&.object&.sha
        latest_sha = tag_ref.object.sha
        return "#{latest_sha} # #{latest_tag}"
      end

      # If we couldn't get the SHA, just return the tag
      latest_tag
    rescue Octokit::Error => e
      puts "GitHub API error for #{action_repo}: #{e.message}" if verbose
      nil
    rescue => e
      puts "Error fetching latest version for #{action_repo}: #{e.message}" if verbose
      nil
    end
  end

  def update_workflow_files(hash_based_actions)
    files_changed = 0
    actions_updated = Hash.new(0)

    @workflow_files.each do |file|
      content = File.read(file)
      content.dup
      file_modified = false

      hash_based_actions.each do |action, versions|
        versions.each do |old_version|
          # Skip if we don't have a latest version for this action
          next unless @latest_versions_cache.key?(action)

          # Match the full 'uses: action@oldversion' pattern
          old_pattern = "uses: #{action}@#{old_version}"
          new_version = @latest_versions_cache[action]
          new_pattern = "uses: #{action}@#{new_version}"

          # Replace old version with new version
          if content.include?(old_pattern)
            content = content.gsub(old_pattern, new_pattern)
            actions_updated[action] += 1
            file_modified = true
          end
        end
      end

      if file_modified
        if dry_run
          puts "Would update: #{file}"
        else
          File.write(file, content)
          puts "Updated: #{file}"
          files_changed += 1
        end
      else
        puts "No updates needed for: #{file}"
      end
    end

    if files_changed > 0
      puts "\nSuccessfully updated #{files_changed} workflow file(s)"

      if defined?(Terminal::Table)
        rows = actions_updated.map { |action, count| [action, count] }
        table = Terminal::Table.new(
          title: "Actions Updated",
          headings: ["Action", "Occurrences"],
          rows: rows
        )
        puts table
      else
        actions_updated.each do |action, count|
          puts "  - #{action}: #{count} occurrence(s)"
        end
      end
    else
      puts "\nNo files were updated" + (dry_run ? " (dry run)" : "")
    end
  end

  def list_workflow_files
    workflow_dir = File.join(Dir.pwd, DEFAULT_WORKFLOW_DIR)
    unless Dir.exist?(workflow_dir)
      puts "Workflow directory not found at #{workflow_dir}"
      return []
    end

    files = Dir.glob(File.join(workflow_dir, "*.{yml,yaml}")).sort

    if defined?(Terminal::Table)
      rows = files.map.with_index do |file, index|
        relative_path = Pathname.new(file).relative_path_from(Pathname.new(Dir.pwd))
        [index + 1, relative_path.to_s]
      end

      table = Terminal::Table.new(
        title: "Available Workflow Files",
        headings: ["#", "Path"],
        rows: rows
      )
      puts table
    else
      puts "Available workflow files:"
      files.each_with_index do |file, index|
        relative_path = Pathname.new(file).relative_path_from(Pathname.new(Dir.pwd))
        puts "  #{index + 1}. #{relative_path}"
      end
    end

    files
  end
  
  def list_actions_in_workflows
    actions = {}
    
    @workflow_files.each do |file|
      content = File.read(file)
      
      content.scan(/uses:\s+([^@\s]+)@([^\s#]+)(?:\s+#\s+(.+))?/) do |match|
        action = match[0]
        version = match[1]
        comment = match[2]
        
        actions[action] ||= Set.new
        actions[action] << version
      end
    rescue => e
      puts "Error processing #{file}: #{e.message}" if verbose
    end
    
    if defined?(Terminal::Table)
      rows = actions.map { |action, versions| [action, versions.to_a.join(", ")] }
      table = Terminal::Table.new(
        title: "GitHub Actions in Workflow Files",
        headings: ["Action", "Versions"],
        rows: rows
      )
      puts table
    else
      puts "GitHub Actions in workflow files:"
      actions.each do |action, versions|
        puts "  - #{action}: #{versions.to_a.join(", ")}"
      end
    end
    
    actions.keys
  end
end

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: update-github-actions.rb [options] [-f workflow_file ...] [-a action ...]"

  opts.on("-f", "--file FILE", "Specify workflow file(s) to update (can be used multiple times, relative or absolute path)") do |file|
    options[:workflow_files] ||= []
    # Convert to absolute path if it's relative
    file_path = File.expand_path(file, Dir.pwd)
    options[:workflow_files] << file_path
  end

  opts.on("-a", "--action ACTION", "Target specific action(s) to update (can be used multiple times, e.g. 'actions/checkout')") do |action|
    options[:target_actions] ||= []
    options[:target_actions] << action
  end
  
  opts.on("--list-actions", "List all actions used in workflow files") do
    options[:list_actions] = true
  end

  opts.on("-l", "--list", "List available workflow files") do
    options[:list_files] = true
  end

  opts.on("-n", "--dry-run", "Show what would be done without making changes") do
    options[:dry_run] = true
  end

  opts.on("-v", "--verbose", "Show more detailed output") do
    options[:verbose] = true
  end

  opts.on("-t", "--token TOKEN", "GitHub API token to avoid rate limits") do |token|
    options[:token] = token
  end

  opts.on("-h", "--help", "Show this help message") do
    puts opts
    exit
  end
end.parse!

updater = GitHubActionsUpdater.new(options)

if options[:list_files]
  updater.list_workflow_files
elsif options[:list_actions]
  updater.list_actions_in_workflows
else
  updater.run
end
