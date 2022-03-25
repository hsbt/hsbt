#!/usr/bin/env ruby

require "fileutils"

REPOSITORIES = [
  "ruby/ruby",
]

template = File.expand_path("../dependabot.yml", __FILE__)

REPOSITORIES.each do |repo|
  system "ghq get https://github.com/#{repo}"
  Dir.chdir(File.expand_path("~/Documents/github.com/#{repo}")) do
    system "git checkout -b add-dependabot"
    FileUtils.mkdir_p(".github")
    FileUtils.cp(template, ".github")
    system "git add .github"
    system "git commit -m 'Added dependabot'"
    system "git push"
    system "gh pr create --base master --head ruby:add-dependabot --title 'Added dependabot.yml for actions' --body ''"
  end
end
