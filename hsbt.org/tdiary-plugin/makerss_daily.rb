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
    body = {}
    date = {}

    src = RSS::Parser.parse(src, false)
    src.items.each do |item|
      item.link =~ /(\d{4})(\d{2})(\d{2})/
      k = "#{$1}-#{$2}-#{$3}"
      if body[k]
        body[k] << item.content_encoded
      else
        body[k] = [item.content_encoded]
      end
      date[k] = item.date
    end

    dist = RSS::Maker.make("1.0") do |maker|
      maker.channel.about = src.channel.about
      maker.channel.title = src.channel.title
      maker.channel.description = src.channel.description
      maker.channel.link = src.channel.link

      maker.items.do_sort = true

      body.each do |k, v|
        maker.items.new_item do |item|
          item.link = "#{maker.channel.link}#{k.split('-').join}.html"
          item.title = "#{k} の日記"
          item.date = date[k]
          item.content_encoded = v.join("\n")
        end
      end
    end

    dist.to_s
  end
end
