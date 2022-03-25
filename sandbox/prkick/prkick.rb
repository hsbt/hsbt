#!/usr/bin/env ruby

require "bundler/inline"
require "fileutils"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
end

class GitHub
  class << self
    def com
      Octokit.reset!

      Octokit.configure do |c|
        c.access_token = ENV['GITHUB_ACCESS_TOKEN']
        c.auto_paginate = true
        c.per_page = 100
      end

      @_com ||= Octokit::Client.new
    end
  end
end

REPOSITORIES = [
  'hsbt/hsbt',
]

REPOSITORIES.each do |repo|
  Dir.chdir(File.expand_path("~/Documents/github.com/#{repo}")) do
    puts "Updating #{repo}..."
    system "git switch master"
    system "git reset origin/master --hard"
    system "git pull --rebase"
    system "git checkout -b add-dependabot"
    FileUtils.mkdir_p(".github")
    FileUtils.cp(File.expand_path("../dependabot.yml", __FILE__), ".github")
    system "git add .github"
    system "git commit -m 'Added dependabot'"
    system "git push"
  end
end
