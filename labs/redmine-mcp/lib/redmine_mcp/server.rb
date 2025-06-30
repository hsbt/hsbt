# frozen_string_literal: true

require "mcp"
require_relative "../redmine_client"
require_relative "list_issues_tool"
require_relative "get_issue_tool"

module RedmineMcp
  class Server
    def self.create(redmine_url:, redmine_api_key:)
      # Initialize Redmine client
      redmine_client = RedmineClient.new(
        base_url: redmine_url,
        api_key: redmine_api_key
      )

      # Validate connection
      begin
        redmine_client.list_issues(limit: 1)
      rescue RedmineClient::AuthenticationError => e
        raise e
      rescue => e
        raise RedmineClient::RedmineError, "Failed to connect to Redmine: #{e.message}"
      end

      # Create MCP server
      server = MCP::Server.new(
        name: "redmine-mcp-server",
        version: "1.0.0",
        tools: [
          ListIssuesTool,
          GetIssueTool
        ],
        server_context: {
          redmine_client: redmine_client
        }
      )

      server
    end
  end
end
