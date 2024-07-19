require "rss"
require "open-uri"

feeds = %w[
]

path = "feed.rss"

rss = RSS::Maker.make("1.0") do |maker|
  maker.channel.link = "https://www.hsbt.org/"
  maker.channel.title = "Aggregated RSS Feed"
  maker.channel.about = ""
  maker.channel.description = "Aggregated RSS Feed"

  feeds.each do |feed|
    URI.open(feed) do |rss|
      feed = RSS::Parser.parse(rss, false)
      feed.items.each do |item|
        maker.items.new_item do |new_item|
          new_item.title = item.title
          new_item.link = item.link
          new_item.description = item.description
          new_item.date = item.date
          new_item.content_encoded = item.content_encoded
        end
      end
    end
  end
end

rss.items.sort_by!(&:date).reverse!

File.open(path, "w") do |file|
  file << rss.to_s
end
