#!/usr/bin/env ruby
# frozen_string_literal: true

# Mastodon Posts Backup Script
# This script backs up all posts from your Mastodon account using Mastodon REST APIs

require 'net/http'
require 'json'
require 'uri'
require 'fileutils'
require 'time'

class MastodonBackup
  BATCH_SIZE = 40 # Mastodon's default limit for statuses endpoint

  def initialize(instance_url, access_token)
    @instance_url = instance_url.sub(/\/$/, '') # Remove trailing slash
    @access_token = access_token
    @account_id = nil
    @username = nil
  end

  def backup_all_posts(output_dir = 'mastodon_backup')
    puts "Starting Mastodon backup for #{@instance_url}..."
    
    # Verify credentials and get account info
    verify_credentials
    
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

  def verify_credentials
    puts "Verifying credentials..."
    
    uri = URI("#{@instance_url}/api/v1/accounts/verify_credentials")
    response = make_authenticated_request(uri, 'GET')
    
    if response.code == '200'
      data = JSON.parse(response.body)
      @account_id = data['id']
      @username = data['username']
      puts "Successfully authenticated as @#{@username}@#{URI.parse(@instance_url).host} (ID: #{@account_id})"
    else
      error_data = JSON.parse(response.body) rescue {}
      raise "Authentication failed: #{response.code} - #{error_data['error'] || response.body}"
    end
  end

  def fetch_all_posts
    puts "Fetching all posts..."
    
    all_posts = []
    max_id = nil
    page = 1
    
    loop do
      puts "Fetching page #{page}..."
      
      # Build query parameters
      params = {
        limit: BATCH_SIZE,
        exclude_replies: 'false',
        exclude_reblogs: 'false'
      }
      params[:max_id] = max_id if max_id
      
      query_string = params.map { |k, v| "#{k}=#{URI.encode_www_form_component(v)}" }.join('&')
      uri = URI("#{@instance_url}/api/v1/accounts/#{@account_id}/statuses?#{query_string}")
      
      response = make_authenticated_request(uri, 'GET')
      
      if response.code == '200'
        posts = JSON.parse(response.body)
        
        if posts.empty?
          puts "  No more posts found"
          break
        end
        
        all_posts.concat(posts)
        puts "  Found #{posts.size} posts on this page"
        
        # Get the ID of the last post for pagination
        max_id = posts.last['id']
        page += 1
        
        # Rate limiting - Mastodon allows 300 requests per 5 minutes
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
        created_at = Time.parse(post['created_at'])
        date_key = created_at.strftime('%Y-%m-%d')
        
        posts_by_date[date_key] ||= []
        posts_by_date[date_key] << {
          id: post['id'],
          created_at: post['created_at'],
          content: post['content'],
          text: strip_html(post['content']),
          url: post['url'],
          uri: post['uri'],
          visibility: post['visibility'],
          sensitive: post['sensitive'],
          spoiler_text: post['spoiler_text'],
          replies_count: post['replies_count'] || 0,
          reblogs_count: post['reblogs_count'] || 0,
          favourites_count: post['favourites_count'] || 0,
          language: post['language'],
          in_reply_to_id: post['in_reply_to_id'],
          in_reply_to_account_id: post['in_reply_to_account_id'],
          reblog: post['reblog'] ? {
            id: post['reblog']['id'],
            created_at: post['reblog']['created_at'],
            content: post['reblog']['content'],
            account: {
              username: post['reblog']['account']['username'],
              display_name: post['reblog']['account']['display_name']
            }
          } : nil,
          media_attachments: post['media_attachments']&.map do |media|
            {
              id: media['id'],
              type: media['type'],
              url: media['url'],
              preview_url: media['preview_url'],
              description: media['description']
            }
          end,
          mentions: post['mentions']&.map do |mention|
            {
              id: mention['id'],
              username: mention['username'],
              acct: mention['acct']
            }
          end,
          tags: post['tags']&.map { |tag| tag['name'] },
          emojis: post['emojis']
        }
      rescue StandardError => e
        puts "Warning: Failed to process post #{post['id']}: #{e.message}"
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
      csv << ['Date', 'Time', 'Text', 'Visibility', 'Replies Count', 'Reblogs Count', 'Favourites Count', 'URL', 'Is Reblog']
      
      posts.each do |post|
        begin
          created_at = Time.parse(post['created_at'])
          text_content = strip_html(post['content'])
          is_reblog = !post['reblog'].nil?
          
          csv << [
            created_at.strftime('%Y-%m-%d'),
            created_at.strftime('%H:%M:%S'),
            text_content,
            post['visibility'],
            post['replies_count'] || 0,
            post['reblogs_count'] || 0,
            post['favourites_count'] || 0,
            post['url'],
            is_reblog
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
    
    uri = URI("#{@instance_url}/api/v1/accounts/#{@account_id}")
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

  def make_authenticated_request(uri, method, body = nil)
    request = case method
              when 'GET'
                Net::HTTP::Get.new(uri)
              when 'POST'
                Net::HTTP::Post.new(uri)
              end
    
    request['Authorization'] = "Bearer #{@access_token}"
    request['Content-Type'] = 'application/json' if method == 'POST'
    request['User-Agent'] = 'MastodonBackupScript/1.0'
    request.body = body.to_json if body && method == 'POST'
    
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = uri.scheme == 'https'
    http.request(request)
  end

  def strip_html(html_content)
    return '' if html_content.nil? || html_content.empty?
    
    # Simple HTML tag removal
    html_content.gsub(/<[^>]+>/, '').strip
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
  output_dir = ARGV[2] || 'mastodon_backup'

  begin
    backup = MastodonBackup.new(instance_url, access_token)
    backup.backup_all_posts(output_dir)
  rescue StandardError => e
    puts "Error: #{e.message}"
    exit 1
  end
end
