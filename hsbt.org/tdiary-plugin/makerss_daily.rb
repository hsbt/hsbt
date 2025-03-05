# makerss_daily.rb
#
# generate daily RSS file when updating.
#
require "rss"
require "time"

class MakeRssFull
  include ERB::Util
  include TDiary::ViewHelper

  def write(encoder)
    File.write(file, rewrite(encoder.call(xml)))
  rescue
  end

  def rewrite(src)
    rss = {}

    src = RSS::Parser.parse(src, false)
    src.items.each do |item|
      next if item.link.include?('#c')

      item.link =~ /(\d{4})(\d{2})(\d{2})/
      k = "#{$1}-#{$2}-#{$3}"
      content = item.content_encoded.to_s

      if rss[k]
        rss[k] << {content: content, date: item.date}
      else
        rss[k] = [{content: content, date: item.date}]
      end
    end

    dist = RSS::Maker.make("1.0") do |maker|
      maker.channel.about = src.channel.about
      maker.channel.title = src.channel.title
      maker.channel.description = src.channel.description
      maker.channel.link = src.channel.link

      maker.items.do_sort = true

      rss.each do |k, v|
        maker.items.new_item do |item|
          item.link = "#{maker.channel.link}#{k.split("-").join}.html"
          item.title = "#{k} の日記"
          item.date = v.last[:date]
          item.pubDate = v.last[:date]
          item.content_encoded = v.map { |c| c[:content] }.join("\n")
        end
      end
    end

    dist.to_s
  end
end
