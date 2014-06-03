#!/usr/bin/env ruby
require 'uri'
require 'fileutils'

TARGET = ENV['TARGET'] || "~/Documents"

[Dir.glob('*') + Dir.glob('*/*')].flatten.each do |dir|
  next unless File.directory? dir
  p dir
  remote = Dir.chdir(dir) do
    next unless File.directory?('.git')
    git = `git remote -v`.chomp
    git.scan(/origin\t(.*) \(fetch\)/).first[0]
  end

  uri = URI(remote.to_s.gsub(/:/, '/').gsub(/\/\/\//, '://').gsub(/git@/, 'https://').gsub(/\.git/, ''))
  next unless uri.hostname && uri.path

  h = uri.hostname
  u, _ = *uri.path.scan(/\/?(.+)\/(\w+)\/?/).first

  FileUtils.mkdir_p File.expand_path("#{TARGET}/#{h}/#{u}")
  FileUtils.mv dir, File.expand_path("#{TARGET}/#{h}/#{u}")
end
