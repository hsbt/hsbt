require "rss"

data = {}

# ARGV から rss ファイルを読み込んで items を保存する
File.open(ARGV[0]) do |f|
  src = RSS::Parser.parse(f.read, false)
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
        item.link = "https://www.hsbt.org/diary/#{k.split('-').join}.html"
        item.title = "#{k} の日記"
        item.date = Time.parse(k)
        item.content_encoded = v.join("\n")
      end
    end
  end

  File.open(ARGV[1], "w") do |f|
    f.write(dist.to_s)
  end
end
