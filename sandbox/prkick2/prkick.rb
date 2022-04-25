#!/usr/bin/env ruby

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
end

require "octokit"

Octokit.configure do |c|
  c.web_endpoint = ENV["GITHUB_HOST"]
  c.api_endpoint = "#{ENV["GITHUB_HOST"]}/api/v3"
  c.access_token = ENV['GITHUB_TOKEN']
  c.auto_paginate = true
  c.per_page = 100
end

REPOSITORIES = [
  "hsbt/hsbt",
]

client = Octokit::Client.new
branch_name = "dependabot/init"
content = File.read(File.expand_path("../dependabot.yml", __FILE__))

REPOSITORIES.each do |repo|
  unless !!client.tree(repo, "HEAD")[:tree].find{|o| o[:path] == "Gemfile" }
    next
  end

  default_branch = client.repo(repo).default_branch
  client.create_ref(repo, "refs/heads/#{branch_name}", client.ref(repo, "heads/#{default_branch}").object.sha)
  client.create_contents(repo, ".github/dependabot.yml", "init dependabot", content, :branch => branch_name)
  client.create_pull_request(repo, default_branch, branch_name, "Create a dependabot.yml")
end
