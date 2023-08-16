# makerss_daily.rb
#
# generate daily RSS file when updating.
#
require "rss"
require "time"

class MakeRssFull
  include ERB::Util
  include TDiary::ViewHelper

  def write( encoder )
    begin
      File::open( file, 'w' ) do |f|
        f.write( rewrite(encoder.call( xml )) )
      end
    rescue
    end
  end

  def rewrite(src)
    rss = {}

    src = RSS::Parser.parse(src, false)
    src.items.each do |item|
      item.link =~ /(\d{4})(\d{2})(\d{2})/
      k = "#{$1}-#{$2}-#{$3}"
      if rss[k]
        rss[k] << {content: item.content_encoded, date: item.date}
      else
        rss[k] = [{content: item.content_encoded, date: item.date}]
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
          item.link = "#{maker.channel.link}#{k.split('-').join}.html"
          item.title = "#{k} の日記"
          item.date = v.last[:date]
          item.content_encoded = v.map{|c| c[:content] }.join("\n")
        end
      end
    end

    dist.to_s
  end
end
