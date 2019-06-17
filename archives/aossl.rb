#!/usr/bin/env ruby

require 'net/https'

sites = %w[
  pepabo.com
  lolipop.jp
  heteml.jp
  muumuu-domain.com
  jugem.jp
  www.petit.cc
  goope.jp
  shop-pro.jp
  calamel.jp
  30d.jp
  osaipo.jp
  minne.com
  tetote-market.jp
  suzuri.jp
  mc.lolipop.jp
  colorme-repeat.jp
  people.suzuri.jp
]


sites.each do |site|
  ua = Net::HTTP.new(site, 443)
  ua.instance_eval {
    ctx = OpenSSL::SSL::SSLContext.new
    ctx.ssl_version = :TLSv1_2
    @ssl_context = ctx
  }
  ua.use_ssl = true
  puts site + ': ' + ua.start { res = ua.get('/'); res.code }
end
