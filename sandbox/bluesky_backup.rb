#!/usr/bin/env ruby
# frozen_string_literal: true

# Bluesky Posts Backup Script
# This script backs up all posts from your Bluesky account using AT Protocol APIs

require 'net/http'
require 'json'
require 'uri'
require 'fileutils'
require 'time'

class BlueskyBackup
  API_BASE = 'https://bsky.social'
  BATCH_SIZE = 100

  def initialize(identifier, password)
    @identifier = identifier
    @password = password
    @access_token = nil
    @refresh_token = nil
    @did = nil
    @handle = nil
  end

  def backup_all_posts(output_dir = 'bluesky_backup')
    puts "Starting Bluesky backup for #{@identifier}..."
    
    # Create session
    authenticate
    
    # Create output directory
    FileUtils.mkdir_p(output_dir)
    
    # Get all posts
    posts = fetch_all_posts
    
    # Save posts
    save_posts(posts, output_dir)
    
    # Save profile
    save_profile(output_dir)
    
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
    
    response = make_request(uri, 'POST', request_body)
    
    if response.code == '200'
      data = JSON.parse(response.body)
      @access_token = data['accessJwt']
      @refresh_token = data['refreshJwt']
      @did = data['did']
      @handle = data['handle']
      puts "Successfully authenticated as #{@handle} (#{@did})"
    else
      error_data = JSON.parse(response.body) rescue {}
      raise "Authentication failed: #{response.code} - #{error_data['message'] || response.body}"
    end
  end

  def fetch_all_posts
    puts "Fetching all posts..."
    
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
      
      query_string = params.map { |k, v| "#{k}=#{URI.encode_www_form_component(v)}" }.join('&')
      uri = URI("#{API_BASE}/xrpc/app.bsky.feed.getAuthorFeed?#{query_string}")
      
      response = make_authenticated_request(uri, 'GET')
      
      if response.code == '200'
        data = JSON.parse(response.body)
        feed = data['feed'] || []
        
        # Extract posts from feed items
        posts = feed.map { |item| item['post'] }.compact
        all_posts.concat(posts)
        
        puts "  Found #{posts.size} posts on this page"
        
        # Check if there are more pages
        cursor = data['cursor']
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

  def save_posts(posts, output_dir)
    puts "Saving posts to files..."
    
    # Save all posts as JSON
    posts_file = File.join(output_dir, 'all_posts.json')
    File.write(posts_file, JSON.pretty_generate(posts))
    puts "All posts saved to #{posts_file}"
    
    # Create individual post files organized by date
    posts_by_date = {}
    
    posts.each do |post|
      begin
        created_at = Time.parse(post['record']['createdAt'])
        date_key = created_at.strftime('%Y-%m-%d')
        
        posts_by_date[date_key] ||= []
        posts_by_date[date_key] << {
          uri: post['uri'],
          cid: post['cid'],
          created_at: post['record']['createdAt'],
          text: post['record']['text'],
          reply_count: post['replyCount'] || 0,
          repost_count: post['repostCount'] || 0,
          like_count: post['likeCount'] || 0,
          embed: post['record']['embed'],
          facets: post['record']['facets'],
          langs: post['record']['langs'],
          labels: post['labels']
        }
      rescue StandardError => e
        puts "Warning: Failed to process post #{post['uri']}: #{e.message}"
      end
    end
    
    # Save posts by date
    posts_by_date.each do |date, day_posts|
      date_dir = File.join(output_dir, 'posts_by_date')
      FileUtils.mkdir_p(date_dir)
      
      date_file = File.join(date_dir, "#{date}.json")
      File.write(date_file, JSON.pretty_generate(day_posts))
    end
    
    puts "Posts organized by date saved to #{File.join(output_dir, 'posts_by_date')}/"
    
    # Create a CSV summary
    create_csv_summary(posts, output_dir)
  end

  def create_csv_summary(posts, output_dir)
    require 'csv'
    
    csv_file = File.join(output_dir, 'posts_summary.csv')
    
    CSV.open(csv_file, 'w') do |csv|
      csv << ['Date', 'Time', 'Text', 'Reply Count', 'Repost Count', 'Like Count', 'URI']
      
      posts.each do |post|
        begin
          created_at = Time.parse(post['record']['createdAt'])
          csv << [
            created_at.strftime('%Y-%m-%d'),
            created_at.strftime('%H:%M:%S'),
            post['record']['text'] || '',
            post['replyCount'] || 0,
            post['repostCount'] || 0,
            post['likeCount'] || 0,
            post['uri']
          ]
        rescue StandardError => e
          puts "Warning: Failed to process post for CSV: #{e.message}"
        end
      end
    end
    
    puts "CSV summary saved to #{csv_file}"
  end

  def save_profile(output_dir)
    puts "Saving profile information..."
    
    uri = URI("#{API_BASE}/xrpc/app.bsky.actor.getProfile?actor=#{@did}")
    response = make_authenticated_request(uri, 'GET')
    
    if response.code == '200'
      profile_data = JSON.parse(response.body)
      profile_file = File.join(output_dir, 'profile.json')
      File.write(profile_file, JSON.pretty_generate(profile_data))
      puts "Profile saved to #{profile_file}"
    else
      puts "Warning: Failed to fetch profile: #{response.code}"
    end
  end

  def make_request(uri, method, body = nil)
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    
    case method
    when 'POST'
      request = Net::HTTP::Post.new(uri)
      request['Content-Type'] = 'application/json'
      request.body = body.to_json if body
    when 'GET'
      request = Net::HTTP::Get.new(uri)
    end
    
    request['User-Agent'] = 'BlueskyBackupScript/1.0'
    
    http.request(request)
  end

  def make_authenticated_request(uri, method, body = nil)
    request = case method
              when 'GET'
                Net::HTTP::Get.new(uri)
              when 'POST'
                Net::HTTP::Post.new(uri)
              end
    
    request['Authorization'] = "Bearer #{@access_token}"
    request['Content-Type'] = 'application/json' if method == 'POST'
    request['User-Agent'] = 'BlueskyBackupScript/1.0'
    request.body = body.to_json if body && method == 'POST'
    
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
  output_dir = ARGV[2] || 'bluesky_backup'

  begin
    backup = BlueskyBackup.new(identifier, password)
    backup.backup_all_posts(output_dir)
  rescue StandardError => e
    puts "Error: #{e.message}"
    exit 1
  end
end
