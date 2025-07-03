#!/usr/bin/env ruby

Dir.glob("#{ARGV[0]}/*.rb") do |f|
  r = ""
  File.open(f) do |io|
    io.each do |l|
      if attr = l.match(/attribute :(.*),[ ]+key: :(.*)/)
        method = <<DEF
attributes :#{attr[2]}
  def #{attr[2]}
    object.#{attr[1]}
  end
DEF
        l.gsub!(/attribute :(.*),[ ]+key: :(.*)/, method)
      end
      r << l
    end
  end
  File.open(f, "w") { |io| io.write r }
end
