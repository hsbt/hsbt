#!/usr/bin/env ruby

require 'open-uri'

sites = File.read(ARGV[0]).split("\n")

sites.each do |site|
  File.open(site, "w") do |scrape|
    body = begin
             open("http://webcache.googleusercontent.com/search?q=cache:http://#{site}").read
           rescue OpenURI::HTTPError
             ""
           end
    scrape.write body
    puts "#{site} #{body.length}"
  end
  sleep 1
end
