#!/usr/bin/env ruby

require "open3"
Dir.glob(File.expand_path("~/.rbenv/versions/*")).each do |v|
  r = Open3.capture3("#{v}/bin/ruby -v #{ARGV[0]}")
  puts [r[0], r[1]]
end
