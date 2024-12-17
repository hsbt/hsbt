#!/usr/bin/env ruby

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "octokit"
  gem "faraday-retry"
end

Octokit.configure do |c|
  c.access_token = ENV['GITHUB_TOKEN']
  c.auto_paginate = true
  c.per_page = 100
end

versions_from = File.readlines(ARGV[0]).map(&:split).to_h
versions_to = File.readlines(ARGV[1]).map(&:split).to_h
footnote_link = []

versions_to.each do |name, version|
  releases = []
  org = "ruby"

  case name
  when "RubyGems"
    name = name.downcase
    org = "rubygems"
  when "bundler"
    name = "rubygems"
    org = "rubygems"
  when "minitest"
    org = "minitest"
  when "test-unit"
    org = "test-unit"
  end

  Octokit.releases("#{org}/#{name}").each do |release|
    releases << release.tag_name
  end
  releases.select{|v| v =~ /^v/ || v =~ /^¥d/ }.sort{|a, b| Gem::Version.new(a.sub(/^v/, "")) <=> Gem::Version.new(b.sub(/^v/, ""))}
  releases.reverse!

  start_index = releases.index("v#{versions_from[name]}") || releases.index(versions_from[name])
  end_index = releases.index("v#{versions_to[name]}") || releases.index(versions_to[name])
  release_range = releases[start_index+1..end_index] if start_index && end_index

  next unless release_range
  next if release_range.empty?

  puts "* #{name} #{version}"
  puts "  * #{release_range.map{|rel| "[#{rel}][#{name}-#{rel}]"}.join(", ")}"
  release_range.each do |rel|
    footnote_link << "[#{name}-#{rel}]: https://github.com/#{org}/#{name}/releases/tag/#{rel}"
  end
end

puts footnote_link.join("\n")
