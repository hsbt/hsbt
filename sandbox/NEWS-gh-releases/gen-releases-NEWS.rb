#!/usr/bin/env ruby

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
  gem "faraday-retry"
end

Octokit.configure do |c|
  c.access_token = ENV["GITHUB_TOKEN"]
  c.auto_paginate = true
  c.per_page = 100
end

versions_from = File.readlines(ARGV[0]).map(&:split).to_h
versions_to = File.readlines(ARGV[1]).map(&:split).to_h
footnote_link = []

versions_to.each do |name, version|
  releases = []

  case name
  when "RubyGems"
    repo = name.downcase
    org = "rubygems"
  when "bundler"
    repo = "rubygems"
    org = "rubygems"
  when "minitest"
    repo = name
    org = "minitest"
  when "test-unit"
    repo = name
    org = "test-unit"
  else
    repo = name
    org = "ruby"
  end

  Octokit.releases("#{org}/#{repo}").each do |release|
    releases << release.tag_name
  end
  releases.select { |v| v =~ /^v/ || v =~ /^¥d/ }.sort { |a, b| Gem::Version.new(a.sub(/^v/, "")) <=> Gem::Version.new(b.sub(/^v/, "")) }
  releases.reverse!

  start_index = releases.index("v#{versions_from[name]}") || releases.index(versions_from[name]) || releases.index("bundler-v#{versions_from[name]}")
  end_index = releases.index("v#{versions_to[name]}") || releases.index(versions_to[name]) || releases.index("bundler-v#{versions_to[name]}")
  release_range = releases[start_index+1..end_index] if start_index && end_index

  if name == "bundler"
    release_range = release_range.select { |v| v =~ /^bundler-/ }
  elsif name == "RubyGems"
    release_range = release_range.select { |v| v =~ /^v/ }
  end

  next unless release_range
  next if release_range.empty?

  puts "* #{name} #{version}"
  puts "  * #{versions_from[name]} to #{release_range.map { |rel|
 "[#{rel.sub(/^bundler-/, '')}][#{name}-#{rel.sub(/^bundler-/, '')}]"}.join(", ")}"
  release_range.each do |rel|
    footnote_link << "[#{name}-#{rel.sub(/^bundler-/, '')}]: https://github.com/#{org}/#{repo}/releases/tag/#{rel}"
  end
end

puts footnote_link.join("\n")
