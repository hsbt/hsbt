#!/usr/bin/env ruby
require 'pathname'

Dir.glob(File.expand_path("~/.rbenv/versions/*")).each do |v|
  if !v.include?("1.9.3-dev") && !v.include?("jruby-1.7.11")
    system "rbenv install #{Pathname(v).basename.to_s} --force"
  end

  gem_cmd = "#{v}/bin/gem"
  system "#{gem_cmd} update --system"
  system "yes | #{gem_cmd} update"
  system "yes | #{gem_cmd} cleanup"
end
