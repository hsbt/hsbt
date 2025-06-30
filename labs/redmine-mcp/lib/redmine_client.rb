# frozen_string_literal: true

require "faraday"
require "json"

class RedmineClient
  class RedmineError < StandardError; end
  class AuthenticationError < RedmineError; end
  class NotFoundError < RedmineError; end

  attr_reader :base_url, :api_key

  def initialize(base_url:, api_key:)
    @base_url = base_url.chomp("/")
    @api_key = api_key
    @connection = Faraday.new(url: @base_url) do |faraday|
      faraday.request :json
      faraday.response :json
      faraday.adapter Faraday.default_adapter
    end
  end

  def list_issues(project_id: nil, status_id: nil, assigned_to_id: nil, limit: 25, offset: 0)
    params = { limit: limit, offset: offset }
    params[:project_id] = project_id if project_id
    params[:status_id] = status_id if status_id
    params[:assigned_to_id] = assigned_to_id if assigned_to_id

    get("/issues.json", params)
  end

  def get_issue(id)
    get("/issues/#{id}.json", { include: "attachments,journals,relations" })
  end

  private

  def get(path, params = {})
    request(:get, path, params)
  end

  def request(method, path, params = {})
    response = @connection.send(method, path) do |req|
      req.headers["X-Redmine-API-Key"] = @api_key
      req.headers["Content-Type"] = "application/json"
      req.params = params
    end

    handle_response(response)
  end

  def handle_response(response)
    case response.status
    when 200, 201, 204
      response.body
    when 401, 403
      raise AuthenticationError, "Authentication failed. Check your API key."
    when 404
      raise NotFoundError, "Resource not found."
    when 422
      error_msg = response.body.dig("errors")&.join(", ") || "Validation error"
      raise RedmineError, error_msg
    else
      raise RedmineError, "HTTP #{response.status}: #{response.body}"
    end
  end
end
