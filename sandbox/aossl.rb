#!/usr/bin/env ruby

require "net/https"

sites = %w[
  www.hsbt.org
]

sites.each do |site|
  ua = Net::HTTP.new(site, 443)
  ua.instance_eval do
    ctx = OpenSSL::SSL::SSLContext.new
    ctx.ssl_version = :TLSv1_2
    @ssl_context = ctx
  end
  ua.use_ssl = true
  puts site + ": " + ua.start { res = ua.get("/"); res.code }
end
