#!/usr/bin/env ruby
# frozen_string_literal: true

require "bundler/setup"
require_relative "../lib/redmine_mcp/server"
require "mcp/server/transports/stdio_transport"

# Check required environment variables
redmine_url = ENV["REDMINE_URL"]
redmine_api_key = ENV["REDMINE_API_KEY"]

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

  STDERR.puts "Starting Redmine MCP server..."
  STDERR.puts "Redmine URL: #{redmine_url}"
  STDERR.puts "Server ready for JSON-RPC requests on stdin/stdout"

  # Create and start the stdio transport
  transport = MCP::Server::Transports::StdioTransport.new(server)
  transport.open
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
