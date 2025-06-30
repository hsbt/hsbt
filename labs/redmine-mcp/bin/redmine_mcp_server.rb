#!/usr/bin/env ruby
# frozen_string_literal: true

require "bundler/setup"
require_relative "../lib/redmine_mcp/server"
require "mcp/server/transports/streamable_http_transport"
require "rack"

# Check required environment variables
redmine_url = ENV["REDMINE_URL"]
redmine_api_key = ENV["REDMINE_API_KEY"]
port = ENV["PORT"] || 9292

unless redmine_url && redmine_api_key
  STDERR.puts "Error: Required environment variables are missing."
  STDERR.puts "Please set REDMINE_URL and REDMINE_API_KEY."
  STDERR.puts ""
  STDERR.puts "Example:"
  STDERR.puts "  export REDMINE_URL=https://your-redmine-instance.com"
  STDERR.puts "  export REDMINE_API_KEY=your_api_key_here"
  exit 1
end

begin
  # Create the Redmine MCP server
  server = RedmineMcp::Server.create(
    redmine_url: redmine_url,
    redmine_api_key: redmine_api_key
  )

  # Create the Streamable HTTP transport
  transport = MCP::Server::Transports::StreamableHTTPTransport.new(server)
  server.transport = transport

  # Create a Rack application
  app = proc do |env|
    request = Rack::Request.new(env)
    response = transport.handle_request(env)
    response
  end

  # Wrap the app with Rack middleware
  rack_app = Rack::Builder.new do
    use(Rack::CommonLogger, Logger.new($stdout))
    use(Rack::ShowExceptions)
    run(app)
  end

  puts "Starting Redmine MCP HTTP server on http://localhost:#{port}"
  puts "Redmine URL: #{redmine_url}"
  puts ""
  puts "Available Tools:"
  puts "1. list_issues - List issues with optional filtering"
  puts "2. get_issue - Get detailed information about a specific issue"
  puts ""
  puts "Use POST requests to send JSON-RPC commands."
  puts "Press Ctrl+C to stop the server"

  # Run the server
  Rack::Handler::WEBrick.run(rack_app, Port: port.to_i, Host: '0.0.0.0')
rescue RedmineClient::AuthenticationError => e
  STDERR.puts "Authentication Error: #{e.message}"
  STDERR.puts "Please check your REDMINE_API_KEY."
  exit 1
rescue RedmineClient::RedmineError => e
  STDERR.puts "Redmine Error: #{e.message}"
  exit 1
rescue => e
  STDERR.puts "Error: #{e.message}"
  STDERR.puts e.backtrace.join("\n") if ENV["DEBUG"]
  exit 1
end
