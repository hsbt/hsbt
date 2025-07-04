#!/usr/bin/env ruby
# frozen_string_literal: true

# Bluesky Posts Backup Script
# This script backs up all posts from your Bluesky account using AT Protocol APIs

require "net/http"
require "json"
require "uri"
require "fileutils"
require "time"

class BlueskyBackup
  API_BASE = "https://bsky.social"
  BATCH_SIZE = 100

  def initialize(identifier, password)
    @identifier = identifier
    @password = password
    @access_token = nil
    @refresh_token = nil
    @did = nil
    @handle = nil
  end

  def backup_all_posts(output_dir = "bluesky_backup")
    puts "Starting Bluesky backup for #{@identifier}..."

    # Create session
    authenticate

    # Create output directory
    FileUtils.mkdir_p(output_dir)

    # Check for existing backup and get last post time
    last_post_time = get_last_post_time(output_dir)
    if last_post_time
      puts "Found existing backup. Latest post: #{last_post_time}"
      puts "Only fetching posts newer than #{last_post_time}..."
    else
      puts "No existing backup found. Fetching all posts..."
    end

    # Get posts (all or only new ones)
    posts = fetch_posts_since(last_post_time)

    if posts.empty?
      puts "No new posts to backup."
      return
    end

    # Save posts
    save_posts(posts, output_dir, last_post_time.nil?)

    puts "Backup completed! #{posts.size} posts saved to #{output_dir}/"
  end

  private

  def authenticate
    puts "Authenticating with Bluesky..."

    uri = URI("#{API_BASE}/xrpc/com.atproto.server.createSession")

    request_body = {
      identifier: @identifier,
      password: @password
    }

    response = make_request(uri, "POST", request_body)

    if response.code == "200"
      data = JSON.parse(response.body)
      @access_token = data["accessJwt"]
      @refresh_token = data["refreshJwt"]
      @did = data["did"]
      @handle = data["handle"]
      puts "Successfully authenticated as #{@handle} (#{@did})"
    else
      error_data = JSON.parse(response.body) rescue {}
      raise "Authentication failed: #{response.code} - #{error_data['message'] || response.body}"
    end
  end

  def get_last_post_time(output_dir)
    json_files = Dir.glob(File.join(output_dir, "*.json")).sort.reverse
    return nil if json_files.empty?

    latest_file = json_files.first
    begin
      posts = JSON.parse(File.read(latest_file))
      return nil if posts.empty?

      latest_post = posts.max_by { |post| Time.parse(post["created_at"]) }
      return Time.parse(latest_post["created_at"])
    rescue StandardError => e
      puts "Warning: Failed to read existing posts from #{latest_file}: #{e.message}"
      return nil
    end
  end

  def fetch_posts_since(since_time = nil)
    puts since_time ? "Fetching posts since #{since_time}..." : "Fetching all posts..."

    all_posts = []
    cursor = nil
    page = 1

    loop do
      puts "Fetching page #{page}..."

      # Build query parameters
      params = {
        actor: @did,
        limit: BATCH_SIZE
      }
      params[:cursor] = cursor if cursor

      query_string = params.map { |k, v| "#{k}=#{URI.encode_www_form_component(v)}" }.join("&")
      uri = URI("#{API_BASE}/xrpc/app.bsky.feed.getAuthorFeed?#{query_string}")

      response = make_authenticated_request(uri, "GET")

      if response.code == "200"
        data = JSON.parse(response.body)
        feed = data["feed"] || []

        # Extract posts from feed items
        posts = feed.map { |item| item["post"] }.compact

        # Filter posts by time if since_time is specified
        if since_time
          new_posts = posts.select do |post|
            begin
              post_time = Time.parse(post["record"]["createdAt"])
              post_time > since_time
            rescue StandardError
              true # Include posts with invalid timestamps to be safe
            end
          end

          all_posts.concat(new_posts)
          puts "  Found #{new_posts.size} new posts on this page (#{posts.size} total posts)"

          # If we found posts older than since_time, we can stop
          if new_posts.size < posts.size
            puts "Reached posts older than last backup. Stopping here."
            break
          end
        else
          all_posts.concat(posts)
          puts "  Found #{posts.size} posts on this page"
        end

        # Check if there are more pages
        cursor = data["cursor"]
        break if cursor.nil? || cursor.empty? || posts.empty?

        page += 1

        # Rate limiting
        sleep(0.5)
      else
        error_data = JSON.parse(response.body) rescue {}
        puts "Warning: Failed to fetch page #{page}: #{response.code} - #{error_data['message'] || response.body}"
        break
      end
    end

    puts "Total posts fetched: #{all_posts.size}"
    all_posts
  end

  # Deprecated: use fetch_posts_since instead
  def fetch_all_posts
    fetch_posts_since(nil)
  end

  def save_posts(posts, output_dir, is_full_backup = true)
    puts "Saving posts to files..."

    # Create individual post files organized by date
    posts_by_date = {}

    posts.each do |post|
      begin
        created_at = Time.parse(post["record"]["createdAt"])
        date_key = created_at.strftime("%Y-%m-%d")

        posts_by_date[date_key] ||= []
        posts_by_date[date_key] << {
          uri: post["uri"],
          cid: post["cid"],
          created_at: post["record"]["createdAt"],
          text: post["record"]["text"],
          reply_count: post["replyCount"] || 0,
          repost_count: post["repostCount"] || 0,
          like_count: post["likeCount"] || 0,
          embed: post["record"]["embed"],
          facets: post["record"]["facets"],
          langs: post["record"]["langs"],
          labels: post["labels"]
        }
      rescue StandardError => e
        puts "Warning: Failed to process post #{post['uri']}: #{e.message}"
      end
    end

    # Save posts by date (merge with existing if not full backup)
    posts_by_date.each do |date, day_posts|
      date_file = File.join(output_dir, "#{date}.json")

      if !is_full_backup && File.exist?(date_file)
        # Merge with existing posts for this date
        begin
          existing_day_posts = JSON.parse(File.read(date_file))
          existing_uris = existing_day_posts.map { |p| p["uri"] }.to_set
          new_day_posts = day_posts.reject { |p| existing_uris.include?(p["uri"]) }

          if new_day_posts.any?
            merged_day_posts = (new_day_posts + existing_day_posts).sort_by { |p| p["created_at"] }.reverse
            File.write(date_file, JSON.pretty_generate(merged_day_posts))
            puts "Added #{new_day_posts.size} new posts to #{date_file}"
          end
        rescue StandardError => e
          puts "Warning: Failed to merge posts for #{date}: #{e.message}"
          File.write(date_file, JSON.pretty_generate(day_posts))
        end
      else
        File.write(date_file, JSON.pretty_generate(day_posts))
      end
    end

    if posts_by_date.any?
      puts "Posts organized by date saved to #{output_dir}/"
    end
  end

  def make_request(uri, method, body = nil)
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    case method
    when "POST"
      request = Net::HTTP::Post.new(uri)
      request["Content-Type"] = "application/json"
      request.body = body.to_json if body
    when "GET"
      request = Net::HTTP::Get.new(uri)
    end

    request["User-Agent"] = "BlueskyBackupScript/1.0"

    http.request(request)
  end

  def make_authenticated_request(uri, method, body = nil)
    request = case method
              when "GET"
                Net::HTTP::Get.new(uri)
              when "POST"
                Net::HTTP::Post.new(uri)
    end

    request["Authorization"] = "Bearer #{@access_token}"
    request["Content-Type"] = "application/json" if method == "POST"
    request["User-Agent"] = "BlueskyBackupScript/1.0"
    request.body = body.to_json if body && method == "POST"

    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    http.request(request)
  end
end

# CLI interface
if __FILE__ == $0
  if ARGV.length < 2
    puts "Usage: #{$0} <identifier> <password> [output_directory]"
    puts ""
    puts "Arguments:"
    puts "  identifier: Your Bluesky handle (e.g., username.bsky.social) or email"
    puts "  password: Your Bluesky password (use App Password if you have 2FA enabled)"
    puts "  output_directory: Directory to save backup files (default: bluesky_backup)"
    puts ""
    puts "Examples:"
    puts "  #{$0} alice.bsky.social my-app-password"
    puts "  #{$0} alice@example.com my-app-password my_backup_folder"
    puts ""
    puts "Note: It's recommended to use App Passwords instead of your main password."
    puts "You can create an App Password in your Bluesky settings under 'Privacy and Security'."
    exit 1
  end

  identifier = ARGV[0]
  password = ARGV[1]
  output_dir = ARGV[2] || "bluesky_backup"

  begin
    backup = BlueskyBackup.new(identifier, password)
    backup.backup_all_posts(output_dir)
  rescue StandardError => e
    puts "Error: #{e.message}"
    exit 1
  end
end
