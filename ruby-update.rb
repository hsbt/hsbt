#!/usr/bin/env ruby
require 'pathname'

Dir.glob(File.expand_path("~/.rbenv/versions/*")).each do |v|
  if %w(2.1.0-dev 2.2.0-dev).include? v
    system "rbenv install #{Pathname(v).basename.to_s} --force"
  end

  if v.include?('dev')
    gem_cmd = "#{v}/bin/gem"
    system "#{gem_cmd} update --system"
    system "yes | #{gem_cmd} update"
    system "yes | #{gem_cmd} cleanup"
  end
end
