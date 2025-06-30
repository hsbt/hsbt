# frozen_string_literal: true

require "mcp"

module RedmineMcp
  class ListIssuesTool < MCP::Tool
    tool_name "list_issues"
    description "List issues with optional filtering"
    input_schema({
      type: "object",
      properties: {
        project_id: { type: "string", description: "Filter by project ID or identifier" },
        status_id: { type: "string", description: "Filter by status ID (use 'open' for all open statuses)" },
        assigned_to_id: { type: "integer", description: "Filter by assigned user ID" },
        limit: { type: "integer", description: "Maximum number of issues to return", default: 25 },
        offset: { type: "integer", description: "Number of issues to skip", default: 0 }
      },
      required: []
    })

    class << self
      def call(project_id: nil, status_id: nil, assigned_to_id: nil, limit: 25, offset: 0, server_context: nil)
        client = server_context[:redmine_client]
        
        begin
          result = client.list_issues(
            project_id: project_id,
            status_id: status_id,
            assigned_to_id: assigned_to_id,
            limit: limit,
            offset: offset
          )
          issues = result["issues"] || []
          
          if issues.empty?
            return MCP::Tool::Response.new([{
              type: "text",
              text: "No issues found with the specified criteria."
            }])
          end

          content = issues.map do |issue|
            assigned_to = issue["assigned_to"] ? issue["assigned_to"]["name"] : "Unassigned"
            "• ##{issue["id"]}: #{issue["subject"]}\n" \
            "  Project: #{issue["project"]["name"]}\n" \
            "  Status: #{issue["status"]["name"]}\n" \
            "  Priority: #{issue["priority"]["name"]}\n" \
            "  Assigned to: #{assigned_to}\n" \
            "  Created: #{issue["created_on"]}\n" \
            "  Updated: #{issue["updated_on"]}\n"
          end.join("\n")

          MCP::Tool::Response.new([{
            type: "text",
            text: "Found #{issues.length} issues:\n\n#{content}"
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
