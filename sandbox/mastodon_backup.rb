#!/usr/bin/env ruby
# frozen_string_literal: true

# Mastodon Posts Backup Script
# This script backs up all posts from your Mastodon account using Mastodon REST APIs

require "net/http"
require "json"
require "uri"
require "fileutils"
require "time"

class MastodonBackup
  BATCH_SIZE = 40 # Mastodon's default limit for statuses endpoint

  def initialize(instance_url, access_token)
    @instance_url = instance_url.sub(/\/$/, "") # Remove trailing slash
    @access_token = access_token
    @account_id = nil
    @username = nil
  end

  def backup_all_posts(output_dir = "mastodon_backup")
    puts "Starting Mastodon backup for #{@instance_url}..."

    # Verify credentials and get account info
    verify_credentials

    # Create output directory
    FileUtils.mkdir_p(output_dir)

    # Check for existing backup and get last post ID
    last_post_id = get_last_post_id(output_dir)
    if last_post_id
      puts "Found existing backup. Last post ID: #{last_post_id}"
      puts "Only fetching posts newer than this ID..."
    else
      puts "No existing backup found. Fetching all posts..."
    end

    # Get posts (all or only new ones)
    posts = fetch_posts(last_post_id)

    if posts.empty?
      puts "No new posts to backup."
      return
    end

    # Save posts
    save_posts(posts, output_dir, last_post_id.nil?)

    puts "Backup completed! #{posts.size} new posts saved to #{output_dir}/"
  end

  private

  def verify_credentials
    puts "Verifying credentials..."

    uri = URI("#{@instance_url}/api/v1/accounts/verify_credentials")
    response = make_authenticated_request(uri, "GET")

    if response.code == "200"
      data = JSON.parse(response.body)
      @account_id = data["id"]
      @username = data["username"]
      puts "Successfully authenticated as @#{@username}@#{URI.parse(@instance_url).host} (ID: #{@account_id})"
    else
      error_data = JSON.parse(response.body) rescue {}
      raise "Authentication failed: #{response.code} - #{error_data['error'] || response.body}"
    end
  end

  def get_last_post_id(output_dir)
    json_files = Dir.glob(File.join(output_dir, "*.json")).sort.reverse
    return nil if json_files.empty?

    max_id = nil

    # Find the highest post ID from all existing JSON files
    json_files.each do |file|
      begin
        posts = JSON.parse(File.read(file))
        next if posts.empty?

        current_max_id = posts.map { |p| p["id"].to_i }.max
        if max_id.nil? || current_max_id > max_id
          max_id = current_max_id
        end
      rescue StandardError => e
        puts "Warning: Failed to read existing posts from #{file}: #{e.message}"
      end
    end

    max_id.to_s if max_id
  end

  def fetch_posts(since_id = nil)
    puts since_id ? "Fetching posts since ID #{since_id}..." : "Fetching all posts..."

    all_posts = []
    max_id = nil
    page = 1

    loop do
      puts "Fetching page #{page} (since_id: #{since_id}, max_id: #{max_id})"

      # Build query parameters
      params = {
        limit: BATCH_SIZE,
        exclude_replies: "false",
        exclude_reblogs: "false"
      }
      params[:since_id] = since_id if since_id
      params[:max_id] = max_id if max_id && !since_id

      query_string = params.map { |k, v| "#{k}=#{URI.encode_www_form_component(v)}" }.join("&")
      uri = URI("#{@instance_url}/api/v1/accounts/#{@account_id}/statuses?#{query_string}")

      response = make_authenticated_request(uri, "GET")

      if response.code == "200"
        posts = JSON.parse(response.body)

        if posts.empty?
          puts "  No more posts found"
          break
        end

        all_posts.concat(posts)
        puts "  Found #{posts.size} posts on this page"

        # For full backups, we paginate backwards using max_id.
        # For incremental backups, the API gives us all newer posts.
        # The loop will naturally stop when an empty page is returned.
        if !since_id
          max_id = posts.last["id"]
        end

        page += 1

        # Rate limiting
        sleep(1)
      else
        error_data = JSON.parse(response.body) rescue {}
        puts "Warning: Failed to fetch page #{page}: #{response.code} - #{error_data['error'] || response.body}"
        break
      end
    end

    puts "Total posts fetched: #{all_posts.size}"
    all_posts
  end

  def save_posts(posts, output_dir, is_full_backup = true)
    puts "Saving posts to files..."

    # Create individual post files organized by date
    posts_by_date = {}

    posts.each do |post|
      begin
        created_at = Time.parse(post["created_at"])
        date_key = created_at.strftime("%Y-%m-%d")

        posts_by_date[date_key] ||= []
        posts_by_date[date_key] << {
          id: post["id"],
          created_at: post["created_at"],
          content: post["content"],
          text: strip_html(post["content"]),
          url: post["url"],
          uri: post["uri"],
          visibility: post["visibility"],
          sensitive: post["sensitive"],
          spoiler_text: post["spoiler_text"],
          replies_count: post["replies_count"] || 0,
          reblogs_count: post["reblogs_count"] || 0,
          favourites_count: post["favourites_count"] || 0,
          language: post["language"],
          in_reply_to_id: post["in_reply_to_id"],
          in_reply_to_account_id: post["in_reply_to_account_id"],
          reblog: post["reblog"] ? {
            id: post["reblog"]["id"],
            created_at: post["reblog"]["created_at"],
            content: post["reblog"]["content"],
            account: {
              username: post["reblog"]["account"]["username"],
              display_name: post["reblog"]["account"]["display_name"]
            }
          } : nil,
          media_attachments: post["media_attachments"]&.map do |media|
            {
              id: media["id"],
              type: media["type"],
              url: media["url"],
              preview_url: media["preview_url"],
              description: media["description"]
            }
          end,
          mentions: post["mentions"]&.map do |mention|
            {
              id: mention["id"],
              username: mention["username"],
              acct: mention["acct"]
            }
          end,
          tags: post["tags"]&.map { |tag| tag["name"] },
          emojis: post["emojis"]
        }
      rescue StandardError => e
        puts "Warning: Failed to process post #{post['id']}: #{e.message}"
      end
    end

    # Save posts by date
    posts_by_date.each do |date, day_posts|
      date_file = File.join(output_dir, "#{date}.json")

      if !is_full_backup && File.exist?(date_file)
        # Merge with existing posts for this date
        begin
          existing_day_posts = JSON.parse(File.read(date_file))
          existing_ids = existing_day_posts.map { |p| p["id"] }.to_set
          new_day_posts = day_posts.reject { |p| existing_ids.include?(p["id"]) }

          if new_day_posts.any?
            merged_day_posts = (new_day_posts + existing_day_posts).sort_by { |p| p["created_at"] }.reverse
            File.write(date_file, JSON.pretty_generate(merged_day_posts))
            puts "Added #{new_day_posts.size} new posts to #{date_file}"
          end
        rescue StandardError => e
          puts "Warning: Failed to merge posts for #{date}: #{e.message}"
          # Overwrite if merging fails to avoid data loss
          File.write(date_file, JSON.pretty_generate(day_posts))
        end
      else
        # Create a new file for the date
        File.write(date_file, JSON.pretty_generate(day_posts))
      end
    end

    if posts_by_date.any?
      puts "Posts organized by date saved to #{output_dir}/"
    end
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
    request["User-Agent"] = "MastodonBackupScript/1.0"
    request.body = body.to_json if body && method == "POST"

    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = uri.scheme == "https"
    http.request(request)
  end

  def strip_html(html_content)
    return "" if html_content.nil? || html_content.empty?

    # Simple HTML tag removal
    html_content.gsub(/<[^>]+>/, "").strip
  end
end

# CLI interface
if __FILE__ == $0
  if ARGV.length < 2
    puts "Usage: #{$0} <instance_url> <access_token> [output_directory]"
    puts ""
    puts "Arguments:"
    puts "  instance_url: Your Mastodon instance URL (e.g., https://mastodon.social)"
    puts "  access_token: Your Mastodon access token"
    puts "  output_directory: Directory to save backup files (default: mastodon_backup)"
    puts ""
    puts "Examples:"
    puts "  #{$0} https://mastodon.social your-access-token"
    puts "  #{$0} https://ruby.social your-access-token my_backup_folder"
    puts ""
    puts "How to get an access token:"
    puts "  1. Go to your Mastodon instance settings"
    puts "  2. Navigate to Development → Your applications"
    puts "  3. Create a new application"
    puts "  4. Set the required scopes: read:accounts, read:statuses"
    puts "  5. Copy the access token from the application details"
    exit 1
  end

  instance_url = ARGV[0]
  access_token = ARGV[1]
  output_dir = ARGV[2] || "mastodon_backup"

  begin
    backup = MastodonBackup.new(instance_url, access_token)
    backup.backup_all_posts(output_dir)
  rescue StandardError => e
    puts "Error: #{e.message}"
    exit 1
  end
end
