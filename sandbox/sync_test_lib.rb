#!/usr/bin/env ruby

test_lib_files = %w[ core_assertions.rb find_executable.rb envutil.rb]

repos = %w[
  bigdecimal cgi cmath date delegate did_you_mean digest drb erb etc
  fileutils find forwardable io-console io-nonblock io-wait ipaddr
  irb logger net-http net-protocol open-uri open3 openssl optparse
  ostruct pathname pstore psych racc resolv stringio strscan tempfile
  time timeout tmpdir uri weakref win32ole yaml zlib
]

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
end

require "octokit"

Octokit.configure do |c|
  c.access_token = ENV['GITHUB_TOKEN']
  c.auto_paginate = true
  c.per_page = 100
end

client = Octokit::Client.new

branch_name = "update-test-lib-#{Time.now.strftime("%Y%m%d")}"
title = "Update test libraries from ruby/ruby #{Time.now.strftime("%Y-%m-%d")}"

opts = ARGV.shift

puts "Use --no-dry-run option when you want to actually create the PRs"

repos.each do |repo|
  puts repo

  if opts == "--no-dry-run"
    default_branch = client.repo(repo).default_branch
    client.create_ref(repo, "refs/heads/#{branch_name}", client.ref(repo, "heads/#{default_branch}").object.sha)
    test_lib_files.each do |file|
      content = File.read(File.expand_path("tool/lib/#{file}", __FILE__))
      client.update_contents(repo, "test/lib/#{file}", "update #{file}", content, :branch => branch_name)
    end
    client.create_pull_request(repo, default_branch, branch_name, title)
  end

  puts "done"
rescue Octokit::Conflict, Octokit::NotFound
  puts "#{repo} is failed"
end
