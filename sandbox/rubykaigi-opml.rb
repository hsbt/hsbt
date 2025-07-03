require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "opml-parser"
  gem "feedbag"
  gem "rss"
end

require "open-uri"

list = []

1.upto(4) do |page|
  url = "https://b.hatena.ne.jp/q/RubyKaigi?target=text&users=1&safe=off&date_begin=2022-09-01&mode=rss&page=#{page}"
  xml = URI.open(url).read
  rss = RSS::Parser.parse(xml)
  rss.items.each do |item|
    feeds = Feedbag.find(item.about)
    next if feeds.empty?
    next if feeds.first.include?("b.hatena.ne.jp")
    list << feeds.first
  end
end

include OpmlParser

outlines = list.flatten.uniq.map do |item|
  OpmlParser::Outline.new({ xmlUrl: item })
end
opml = OpmlParser.export(outlines, "RubyKaigi blogs")

File.open("output.xml", "w") do |f|
  f.write(opml)
end
