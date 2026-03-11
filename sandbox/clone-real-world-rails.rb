#!/usr/bin/env ruby

require "open-uri"
require "parallel"

GITMODULES_URL = "https://raw.githubusercontent.com/steveclarke/real-world-rails/main/.gitmodules"

content = URI.open(GITMODULES_URL).read

urls = content.scan(/url\s*=\s*(.+)/).flatten.map do |url|
  url.strip.sub(/\Agit@github\.com:/, "https://github.com/").sub(/\.git\z/, "")
end

puts "Cloning #{urls.size} repositories..."

Parallel.each(urls, in_threads: 8) do |url|
  puts url
  system("git", "goget", url)
end

puts "Running bundle install sequentially..."

urls.each do |url|
  repo_path = url.sub(%r{\Ahttps://}, "")
  dir = File.join(ENV.fetch("GIT_GOGET_ROOT"), repo_path)

  if File.exist?(File.join(dir, "Gemfile"))
    puts "  bundle install: #{dir}"
    system("bundle", "install", chdir: dir)
  end
end
