# frozen_string_literal: true

require "mcp"

module RedmineMcp
  class GetIssueTool < MCP::Tool
    tool_name "get_issue"
    description "Get detailed information about a specific issue"
    input_schema({
      type: "object",
      properties: {
        issue_id: { type: "integer", description: "The ID of the issue to retrieve" }
      },
      required: ["issue_id"]
    })

    class << self
      def call(issue_id:, server_context: nil)
        client = server_context[:redmine_client]
        
        begin
          result = client.get_issue(issue_id)
          issue = result["issue"]
          
          unless issue
            return MCP::Tool::Response.new([{
              type: "text",
              text: "Issue ##{issue_id} not found."
            }], is_error: true)
          end

          assigned_to = issue["assigned_to"] ? issue["assigned_to"]["name"] : "Unassigned"
          author = issue["author"] ? issue["author"]["name"] : "Unknown"
          
          # Format attachments
          attachments = ""
          if issue["attachments"] && !issue["attachments"].empty?
            attachments = "\nAttachments:\n" + issue["attachments"].map do |att|
              "  • #{att["filename"]} (#{att["filesize"]} bytes)"
            end.join("\n")
          end

          # Format journals (comments/history)
          history = ""
          if issue["journals"] && !issue["journals"].empty?
            history = "\n\nHistory:\n" + issue["journals"].map do |journal|
              next if journal["notes"].nil? || journal["notes"].strip.empty?
              "#{journal["created_on"]} - #{journal["user"]["name"]}:\n#{journal["notes"]}"
            end.compact.join("\n\n")
          end

          content = <<~TEXT
            Issue ##{issue["id"]}: #{issue["subject"]}

            Project: #{issue["project"]["name"]}
            Tracker: #{issue["tracker"]["name"]}
            Status: #{issue["status"]["name"]}
            Priority: #{issue["priority"]["name"]}
            Author: #{author}
            Assigned to: #{assigned_to}
            Created: #{issue["created_on"]}
            Updated: #{issue["updated_on"]}

            Description:
            #{issue["description"] || "No description provided"}#{attachments}#{history}
          TEXT

          MCP::Tool::Response.new([{
            type: "text",
            text: content
          }])
        rescue RedmineClient::RedmineError => e
          MCP::Tool::Response.new([{
            type: "text", 
            text: "Error: #{e.message}"
          }], is_error: true)
        end
      end
    end
  end
end
