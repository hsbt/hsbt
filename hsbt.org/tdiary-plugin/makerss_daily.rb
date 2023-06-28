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
    data = {}
    src.items.each do |item|
      k = item.date.strftime("%Y-%m-%d")
      if data[k]
        data[k] << item.content_encoded
      else
        data[k] = [item.content_encoded]
      end
    end

    dist = RSS::Maker.make("1.0") do |maker|
      maker.channel.about = src.channel.about
      maker.channel.title = src.channel.title
      maker.channel.description = src.channel.description
      maker.channel.link = src.channel.link

      maker.items.do_sort = true

      data.each do |k, v|
        maker.items.new_item do |item|
          item.link = "#{maker.channel.link}#{k.split('-').join}.html"
          item.title = "#{k} の日記"
          item.date = Time.parse(k)
          item.content_encoded = v.join("\n")
        end
      end
    end

    dist.to_s
  end
end

class MakeRssNoComments < MakeRssFull
  def write( encoder )
    return unless @conf['makerss.no_comments']
    super( encoder )
  end
end
