require "bundler/inline"

gemfile do
  source 'https://rubygems.org'
  gem 'graphql-client'
end

require "graphql/client"
require "graphql/client/http"

module GitHub
  class << self
    def http_adapter
      @_http_adapter ||= GraphQL::Client::HTTP.new(ENV["GITHUB_GRAPHQL_ENDOPOINT"] || "https://api.github.com/graphql") do
        def headers(context)
          {
            "Authorization" => "Bearer #{ENV['GITHUB_ACCESS_TOKEN']}",
            "User-Agent" => 'Ruby'
          }
        end
      end
    end

    def schema
      @_schema ||= GraphQL::Client.load_schema(http_adapter)
    end

    def client
      @_client ||= GraphQL::Client.new(schema: schema, execute: http_adapter)
    end
  end
end

users = begin
  File.read(ARGV[0]).split
rescue Errno::ENOENT
  [ARGV[0]]
end

users.each_slice(20) do |sliced_users|
  q = sliced_users.map do |u|
    "#{u.tr("-", "").downcase}: user(login: \"#{u}\") { ...UserFragment }\n"
  end.join

  year= ARGV.last

  UserQuery = GitHub.client.parse <<~GRAPHQL
    query{
      #{q}
    }
    fragment UserFragment on User {
      login
      contributionsCollection(from: "#{year}-01-01T00:00:00" to:"#{year}-12-31T23:59:59") {
        totalCommitContributions
        totalIssueContributions
        totalPullRequestContributions
        totalPullRequestReviewContributions
        contributionCalendar {
          totalContributions
        }
      }
    }
  GRAPHQL

  response = GitHub.client.query(UserQuery)

  sliced_users.map do |u|
    user = response.data.__send__(u.tr("-", "").downcase)
    cc = user.contributions_collection
    puts "#{user.login},#{cc.total_commit_contributions},#{cc.total_pull_request_contributions},#{cc.total_issue_contributions},#{cc.total_pull_request_review_contributions},#{cc.contribution_calendar.total_contributions}"
  end
end
