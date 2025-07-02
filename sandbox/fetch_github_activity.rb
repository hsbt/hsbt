#!/usr/bin/env ruby

# GitHub Activity Fetcher for hsbt user
# This script fetches GitHub activities and saves them as yyyy-mm-dd.md files

require 'json'
require 'date'
require 'fileutils'

class GitHubActivityFetcher
  def initialize(output_dir = nil, username = nil)
    check_dependencies
    @username = username || get_current_user
    @output_dir = output_dir || File.join(File.dirname(__FILE__), 'github')
  end
  
  def run
    print_status "Starting GitHub activity fetch for user: #{@username}"
    
    # Ensure output directory exists
    FileUtils.mkdir_p(@output_dir)
    
    # Get user creation date
    print_status "Getting user creation date..."
    user_creation_date = get_user_creation_date
    print_status "User created on: #{user_creation_date}"
    
    # Get current date
    current_date = Date.today.strftime("%Y-%m-%d")
    print_status "Current date: #{current_date}"
    
    # Generate date range
    print_status "Generating activities from #{user_creation_date} to #{current_date}"
    
    # Get date range
    dates = generate_date_range(user_creation_date, current_date)
    
    # Process each date
    count = 0
    total = dates.length
    
    dates.each do |date|
      count += 1
      print_status "Processing date #{date} (#{count}/#{total})"
      
      output_file = File.join(@output_dir, "#{date}.md")
      
      # Skip if file already exists and is not empty
      if File.exist?(output_file) && !File.zero?(output_file)
        print_status "File #{date}.md already exists, skipping..."
        next
      end
      
      # Check if there are any activities for this date
      unless has_activities_for_date?(date)
        print_status "No activities found for #{date}, skipping..."
        next
      end
      
      # Get activities for this date
      activities = get_activities_for_date(date)
      
      # Save to file
      File.open(output_file, 'w') do |file|
        file.puts "# GitHub Activity for #{date}"
        file.puts ""
        file.puts activities
      end
      
      print_status "Saved activities to #{date}.md"
      
      # Add small delay to avoid hitting rate limits
      sleep 0.1
    end
    
    print_status "GitHub activity fetch completed!"
    print_status "Files saved in: #{@output_dir}"
  end
  
  private
  
  def print_status(message)
    puts "[INFO] #{message}"
  end
  
  def print_warning(message)
    puts "[WARN] #{message}"
  end
  
  def print_error(message)
    puts "[ERROR] #{message}"
  end
  
  def check_dependencies
    unless system("which gh > /dev/null 2>&1")
      print_error "GitHub CLI (gh) is required but not found."
      print_error "Install it with: brew install gh"
      print_error "Then run: gh auth login"
      exit 1
    end
    
    unless system("gh auth status > /dev/null 2>&1")
      print_error "GitHub CLI not authenticated."
      print_error "Run: gh auth login"
      exit 1
    end
    
    print_status "Using GitHub CLI authentication"
  end
  
  def get_current_user
    result = `gh api user --jq .login 2>/dev/null`.strip
    
    if $?.success? && !result.empty?
      result
    else
      print_error "Failed to get current GitHub user"
      exit 1
    end
  end
  
  def api_request(endpoint)
    result = `gh api "#{endpoint}" 2>/dev/null`
    
    if $?.success?
      JSON.parse(result)
    else
      print_error "API request failed for endpoint: #{endpoint}"
      exit 1
    end
  end
  
  def get_user_creation_date
    user_data = api_request("users/#{@username}")
    created_at = user_data['created_at'] || '2008-01-01T00:00:00Z'
    Date.parse(created_at).strftime("%Y-%m-%d")
  end
  
  def has_activities_for_date?(date)
    events = api_request("users/#{@username}/events?per_page=100")
    
    events.any? do |event|
      next unless event.is_a?(Hash)
      
      created_at = event['created_at']
      next unless created_at
      
      event_date = Date.parse(created_at).strftime("%Y-%m-%d")
      event_date == date
    end
  end
  
  def get_activities_for_date(date)
    events = api_request("users/#{@username}/events?per_page=100")
    
    # Initialize activity arrays
    issues = []
    pull_requests = []
    comments = []
    commits = []
    
    # Process each event
    events.each do |event|
      next unless event.is_a?(Hash)
      
      created_at = event['created_at']
      next unless created_at
      
      event_date = Date.parse(created_at).strftime("%Y-%m-%d")
      next unless event_date == date
      
      event_type = event['type']
      payload = event['payload'] || {}
      repo_name = event.dig('repo', 'name') || 'Unknown'
      
      case event_type
      when 'IssuesEvent'
        action = payload['action'] || ''
        issue = payload['issue'] || {}
        title = issue['title'] || 'No title'
        url = issue['html_url'] || ''
        issues << "  * [#{action}] #{title} - #{repo_name} (#{url})"
        
      when 'PullRequestEvent'
        action = payload['action'] || ''
        pr = payload['pull_request'] || {}
        title = pr['title'] || 'No title'
        url = pr['html_url'] || ''
        pull_requests << "  * [#{action}] #{title} - #{repo_name} (#{url})"
        
      when 'IssueCommentEvent', 'PullRequestReviewCommentEvent', 'CommitCommentEvent'
        comment = payload['comment'] || {}
        body = (comment['body'] || '')[0, 100].gsub(/\n/, ' ')
        url = comment['html_url'] || ''
        comments << "  * #{body}... - #{repo_name} (#{url})" unless body.empty?
        
      when 'PushEvent'
        commits_data = payload['commits'] || []
        commits_data.each do |commit|
          message = (commit['message'] || 'No message')[0, 100].gsub(/\n/, ' ')
          sha = (commit['sha'] || '')[0, 7]
          commits << "  * #{message} (#{sha}) - #{repo_name}"
        end
      end
    end
    
    # Format output
    output = []
    
    output << "## issue"
    if issues.any?
      output.concat(issues)
    else
      output << "  * No issues found"
    end
    
    output << ""
    output << "## pull request"
    if pull_requests.any?
      output.concat(pull_requests)
    else
      output << "  * No pull requests found"
    end
    
    output << ""
    output << "## comment"
    if comments.any?
      output.concat(comments)
    else
      output << "  * No comments found"
    end
    
    output << ""
    output << "## commit"
    if commits.any?
      output.concat(commits)
    else
      output << "  * No commits found"
    end
    
    output.join("\n")
  end
  
  def generate_date_range(start_date, end_date)
    start_date = Date.parse(start_date)
    end_date = Date.parse(end_date)
    
    dates = []
    current_date = start_date
    
    while current_date <= end_date
      dates << current_date.strftime("%Y-%m-%d")
      current_date += 1
    end
    
    dates
  end
end

# Run the fetcher
if __FILE__ == $0
  require 'optparse'
  
  options = {}
  OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options]"
    
    opts.on("-o", "--output-dir DIR", "Output directory for GitHub activity files") do |dir|
      options[:output_dir] = dir
    end
    
    opts.on("-u", "--username USER", "GitHub username (defaults to current authenticated user)") do |user|
      options[:username] = user
    end
    
    opts.on("-h", "--help", "Show this help message") do
      puts opts
      exit
    end
  end.parse!
  
  fetcher = GitHubActivityFetcher.new(options[:output_dir], options[:username])
  fetcher.run
end
