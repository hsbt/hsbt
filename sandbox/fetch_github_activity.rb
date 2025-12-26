#!/usr/bin/env ruby

# GitHub Activity Fetcher for hsbt user
# This script fetches GitHub activities and saves them as yyyy-mm-dd.md files

require "json"
require "date"
require "fileutils"
require "set"

class GitHubActivityFetcher
  def initialize(output_dir = nil, username = nil, year_month = nil, start_year = nil)
    check_dependencies
    @username = username || get_current_user
    @output_dir = output_dir || File.join(File.dirname(__FILE__), "github")
    @year_month = year_month
    @start_year = start_year
    @events_cache = nil
  end

  def run
    print_status "Starting GitHub activity fetch for user: #{@username}"

    # Ensure output directory exists
    FileUtils.mkdir_p(@output_dir)

    # Get date range
    if @year_month
      print_status "Getting activities for #{@year_month}"
      dates = generate_dates_for_month(@year_month)

      # Check if the requested month is within the GitHub Events API limit (90 days)
      oldest_date = Date.parse(dates.first)
      days_ago = (Date.today - oldest_date).to_i
      if days_ago > 90
        print_warning "Requested month (#{@year_month}) is #{days_ago} days ago."
        print_warning "GitHub Events API only provides data for the last 90 days."
        print_warning "No data will be available for this period."
      end
    else
      # Get user creation date
      print_status "Getting user creation date..."
      user_creation_date = get_user_creation_date
      print_status "User created on: #{user_creation_date}"

      # Get current date
      current_date = Date.today.strftime("%Y-%m-%d")
      print_status "Current date: #{current_date}"

      # Generate date range by processing month by month
      print_status "Generating activities from #{user_creation_date} to #{current_date}"
      dates = generate_all_dates_with_monthly_data(user_creation_date, current_date)
    end

    # Process each date
    count = 0
    total = dates.length

    print_status "Total dates to process: #{total}"

    dates.each do |date|
      count += 1
      print_status "Processing date #{date} (#{count}/#{total})"

      output_file = File.join(@output_dir, "#{date}.md")

      # Skip if file already exists and is not empty
      if File.exist?(output_file) && !File.zero?(output_file)
        print_status "File #{date}.md already exists, skipping..."
        next
      end

      # For comprehensive data collection, set month context if not already set
      unless @year_month
        date_obj = Date.parse(date)
        temp_year_month = date_obj.strftime("%Y%m")
        original_year_month = @year_month
        @year_month = temp_year_month
        @events_cache = nil

        # Get activities for this date with comprehensive data
        activities = get_activities_for_date(date)

        # Restore original state
        @year_month = original_year_month
        @events_cache = nil
      else
        # Check if there are any activities for this date
        unless has_activities_for_date?(date)
          print_status "No activities found for #{date}, skipping..."
          next
        end

        # Get activities for this date
        activities = get_activities_for_date(date)
      end

      # Save to file
      File.open(output_file, "w") do |file|
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
    status = $?.exitstatus

    if status == 0
      JSON.parse(result)
    else
      # Gracefully handle pagination past the last page of user events
      return [] if endpoint =~ %r{\Ausers/[^/]+/events}

      # For some endpoints, return empty result instead of exiting
      if endpoint.include?("search/") || endpoint.include?("/repos/")
        return endpoint.include?("search/") ? { "items" => [] } : []
      end

      print_error "API request failed for endpoint: #{endpoint} (status: #{status})"
      exit 1
    end
  end

  def get_user_creation_date
    user_data = api_request("users/#{@username}")
    created_at = user_data["created_at"] || "2008-01-01T00:00:00Z"
    Date.parse(created_at).strftime("%Y-%m-%d")
  end

  def get_all_events
    return @events_cache if @events_cache

    all_events = []

    # Get events from multiple sources for comprehensive data
    if @year_month
      # For specific month requests, use multiple API endpoints
      all_events.concat(get_events_from_api)
      all_events.concat(get_commits_data)
      all_events.concat(get_issues_data)
      all_events.concat(get_pull_requests_data)
    else
      # For full history without month specification, use events API only
      # (This case should not happen with the new flow, but keeping for safety)
      all_events = get_events_from_api
    end

    # Sort by date
    all_events.sort_by! { |event| event["created_at"] || event["committed_date"] || "1970-01-01" }

    @events_cache = all_events
  end

  def get_events_from_api
    events = []
    page = 1
    per_page = 100

    loop do
      api_events = api_request("users/#{@username}/events?per_page=#{per_page}&page=#{page}")
      break if api_events.empty?

      events.concat(api_events)
      break if api_events.length < per_page
      page += 1

      # Limit to avoid too many API calls
      break if page > 10
    end

    events
  end

  def get_commits_data
    return [] unless @year_month

    commits = []

    # Get user's repositories
    repos = api_request("users/#{@username}/repos?per_page=100&sort=updated")

    # Get recent repositories (last 20)
    repos.first(20).each do |repo|
      repo_name = repo["full_name"]

      begin
        # Get commits for the specified month
        since_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, 1).strftime("%Y-%m-%dT00:00:00Z")
        until_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, -1).strftime("%Y-%m-%dT23:59:59Z")

        repo_commits = api_request("repos/#{repo_name}/commits?author=#{@username}&since=#{since_date}&until=#{until_date}&per_page=100")

        repo_commits.each do |commit|
          commits << {
            "type" => "PushEvent",
            "created_at" => commit["commit"]["author"]["date"],
            "repo" => { "name" => repo_name },
            "payload" => {
              "commits" => [{
                "message" => commit["commit"]["message"],
                "sha" => commit["sha"]
              }]
            }
          }
        end
      rescue
        # Skip repositories that can't be accessed
        next
      end
    end

    commits
  end

  def get_issues_data
    return [] unless @year_month

    issues = []

    begin
      # Search for issues created by the user in the specified month
      since_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, 1).strftime("%Y-%m-%d")
      until_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, -1).strftime("%Y-%m-%d")

      search_result = api_request("search/issues?q=author:#{@username}+created:#{since_date}..#{until_date}&per_page=100")

      search_result["items"]&.each do |issue|
        issues << {
          "type" => "IssuesEvent",
          "created_at" => issue["created_at"],
          "repo" => { "name" => issue["repository_url"]&.split("/")&.last(2)&.join("/") || "Unknown" },
          "payload" => {
            "action" => "opened",
            "issue" => {
              "title" => issue["title"],
              "html_url" => issue["html_url"]
            }
          }
        }
      end
    rescue
      # Skip if search fails
    end

    issues
  end

  def get_pull_requests_data
    return [] unless @year_month

    pull_requests = []

    begin
      # Search for pull requests created by the user in the specified month
      since_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, 1).strftime("%Y-%m-%d")
      until_date = Date.new(@year_month[0,4].to_i, @year_month[4,2].to_i, -1).strftime("%Y-%m-%d")

      search_result = api_request("search/issues?q=author:#{@username}+type:pr+created:#{since_date}..#{until_date}&per_page=100")

      search_result["items"]&.each do |pr|
        pull_requests << {
          "type" => "PullRequestEvent",
          "created_at" => pr["created_at"],
          "repo" => { "name" => pr["repository_url"]&.split("/")&.last(2)&.join("/") || "Unknown" },
          "payload" => {
            "action" => "opened",
            "pull_request" => {
              "title" => pr["title"],
              "html_url" => pr["html_url"]
            }
          }
        }
      end
    rescue
      # Skip if search fails
    end

    pull_requests
  end

  def has_activities_for_date?(date)
    events = @events_cache || get_all_events

    # Debug: Print first few events to understand the data structure
    if ENV["DEBUG"] && @events_cache.nil?
      puts "[DEBUG] Total events returned: #{events.length}"
      events.first(3).each_with_index do |event, i|
        puts "[DEBUG] Event #{i+1}: #{event['type']} at #{event['created_at']}"
      end
    end

    events.any? do |event|
      next unless event.is_a?(Hash)

      created_at = event["created_at"]
      next unless created_at

      event_date = Date.parse(created_at).strftime("%Y-%m-%d")
      event_date == date
    end
  end

  def get_activities_for_date(date)
    events = @events_cache || get_all_events

    # Initialize activity arrays
    issues = []
    pull_requests = []
    comments = []
    commits = []

    # Process each event
    events.each do |event|
      next unless event.is_a?(Hash)

      created_at = event["created_at"]
      next unless created_at

      event_date = Date.parse(created_at).strftime("%Y-%m-%d")
      next unless event_date == date

      event_type = event["type"]
      payload = event["payload"] || {}
      repo_name = event.dig("repo", "name") || "Unknown"

      case event_type
      when "IssuesEvent"
        action = payload["action"] || ""
        issue = payload["issue"] || {}
        title = issue["title"] || "No title"
        url = issue["html_url"] || ""
        issues << "  * [#{action}] #{title} - #{repo_name} (#{url})"

      when "PullRequestEvent"
        action = payload["action"] || ""
        pr = payload["pull_request"] || {}
        title = pr["title"] || "No title"
        url = pr["html_url"] || ""
        pull_requests << "  * [#{action}] #{title} - #{repo_name} (#{url})"

      when "IssueCommentEvent", "PullRequestReviewCommentEvent", "CommitCommentEvent"
        comment = payload["comment"] || {}
        body = (comment["body"] || "")[0, 100].gsub(/\n/, " ")
        url = comment["html_url"] || ""
        comments << "  * #{body}... - #{repo_name} (#{url})" unless body.empty?

      when "PushEvent"
        commits_data = payload["commits"] || []
        commits_data.each do |commit|
          message = (commit["message"] || "No message")[0, 100].gsub(/\n/, " ")
          sha = (commit["sha"] || "")[0, 7]
          full_sha = commit["sha"] || ""
          commit_url = "https://github.com/#{repo_name}/commit/#{full_sha}"
          commits << "  * #{message} (#{sha}) - #{repo_name} (#{commit_url})"
        end

      when "CreateEvent"
        ref_type = payload["ref_type"] || "unknown"
        ref = payload["ref"] || ""
        description = payload["description"] || ""
        if ref_type == "repository"
          comments << "  * Created repository #{repo_name} - #{description}"
        else
          comments << "  * Created #{ref_type} #{ref} in #{repo_name}"
        end

      when "DeleteEvent"
        ref_type = payload["ref_type"] || "unknown"
        ref = payload["ref"] || ""
        comments << "  * Deleted #{ref_type} #{ref} in #{repo_name}"

      when "ForkEvent"
        forkee = payload["forkee"] || {}
        fork_name = forkee["full_name"] || "Unknown"
        comments << "  * Forked #{repo_name} to #{fork_name}"

      when "WatchEvent"
        comments << "  * Starred #{repo_name}"

      when "ReleaseEvent"
        action = payload["action"] || ""
        release = payload["release"] || {}
        tag_name = release["tag_name"] || ""
        release_name = release["name"] || tag_name
        url = release["html_url"] || ""
        comments << "  * [#{action}] Release #{release_name} - #{repo_name} (#{url})"
      end
    end

    # Remove issues that are also pull requests to avoid duplicates
    pr_urls = Set.new
    pull_requests.each do |pr_line|
      if url_match = pr_line.match(/https:\/\/github\.com\/[^)]+/)
        pr_urls.add(url_match[0])
      end
    end

    issues.reject! do |issue_line|
      if url_match = issue_line.match(/https:\/\/github\.com\/[^)]+/)
        pr_urls.include?(url_match[0])
      else
        false
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

  def generate_all_dates_with_monthly_data(start_date, end_date)
    start_date = Date.parse(start_date)
    end_date = Date.parse(end_date)

    # Use specified start year or limit to recent years to avoid excessive processing
    if @start_year
      specified_start_date = Date.new(@start_year.to_i, 1, 1)
      effective_start_date = [start_date, specified_start_date].max
      print_status "Processing from #{effective_start_date.strftime('%Y-%m-%d')} (from specified year #{@start_year})"
    else
      # Start from 2 years ago or user creation date, whichever is later
      two_years_ago = Date.today - (365 * 2)
      effective_start_date = [start_date, two_years_ago].max
      print_status "Processing from #{effective_start_date.strftime('%Y-%m-%d')} (limited to recent 2 years for performance)"
    end

    all_dates = []
    current_month = Date.new(effective_start_date.year, effective_start_date.month, 1)
    end_month = Date.new(end_date.year, end_date.month, 1)

    while current_month <= end_month
      # Set @year_month for this month to enable comprehensive data collection
      month_string = current_month.strftime("%Y%m")
      print_status "Processing month: #{current_month.strftime("%Y-%m")}"

      # Temporarily set @year_month to get comprehensive data for this month
      original_year_month = @year_month
      @year_month = month_string
      @events_cache = nil  # Clear cache for each month

      # Get events for this month
      month_events = get_all_events

      # Generate dates for this month and filter based on actual data
      month_dates = generate_dates_for_month(month_string)
      month_dates.each do |date|
        # Check if there are activities for this date
        if month_events.any? { |event|
          event_date = Date.parse(event["created_at"] || "1970-01-01").strftime("%Y-%m-%d")
          event_date == date
        }
          all_dates << date
        end
      end

      # Restore original @year_month
      @year_month = original_year_month

      current_month = current_month >> 1  # Move to next month
    end

    all_dates
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

  def generate_dates_for_month(year_month)
    begin
      # Parse year-month (e.g., "2024-01" or "202401")
      if year_month.include?("-")
        year, month = year_month.split("-").map(&:to_i)
      elsif year_month.length == 6
        year = year_month[0, 4].to_i
        month = year_month[4, 2].to_i
      else
        raise ArgumentError, "Invalid year-month format"
      end

      # Validate month
      raise ArgumentError, "Invalid month: #{month}" unless (1..12).include?(month)

      # Generate dates for the specified month
      start_date = Date.new(year, month, 1)
      end_date = Date.new(year, month, -1) # Last day of the month

      dates = []
      current_date = start_date

      while current_date <= end_date
        dates << current_date.strftime("%Y-%m-%d")
        current_date += 1
      end

      dates
    rescue ArgumentError => e
      print_error "Invalid year-month format '#{year_month}': #{e.message}"
      print_error "Use format: YYYY-MM (e.g., 2024-01) or YYYYMM (e.g., 202401)"
      exit 1
    end
  end
end

# Run the fetcher
if __FILE__ == $0
  require "optparse"

  options = {}
  OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options]"

    opts.on("-o", "--output-dir DIR", "Output directory for GitHub activity files") do |dir|
      options[:output_dir] = dir
    end

    opts.on("-u", "--username USER", "GitHub username (defaults to current authenticated user)") do |user|
      options[:username] = user
    end

    opts.on("-m", "--month YYYY-MM", "Year and month to fetch (e.g., 2024-01 or 202401)") do |month|
      options[:year_month] = month
    end

    opts.on("-s", "--start-year YYYY", "Start year for full history fetch (e.g., 2020)") do |year|
      options[:start_year] = year
    end

    opts.on("-h", "--help", "Show this help message") do
      puts opts
      exit
    end
  end.parse!

  fetcher = GitHubActivityFetcher.new(options[:output_dir], options[:username], options[:year_month],
options[:start_year])
  fetcher.run
end
