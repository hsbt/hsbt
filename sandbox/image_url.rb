#!/usr/bin/env ruby
Dir.glob(ARGV) do |file|
  io = File.read(file)
  io.gsub!(/image-url\((.*)\)/) do |match|
    "image-url('#{$1}')"
  end
  io.gsub!(/src="\/images\/(.*\.png)"/) do |match|
    "src=\"<%= asset_path('#{$1}') %>\""
  end
  io.gsub!(/"\/images\/(.*\.gif)"/) do |match|
    "\"<%= asset_path('#{$1}') %>\""
  end
  File.open(file, "w") do |f|
    f.write io
  end
end
