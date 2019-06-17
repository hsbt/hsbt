require 'exif'
require 'time'
require 'fileutils'
require 'pathname'

Dir.glob("data/*").each do |f|
  exif = Exif::Data.new(File.open(f))
  time = if exif.date_time_original
           exif.date_time_original
         elsif exif.date_time
           exif.date_time
         else
          p exif
          raise Exif::NotReadable
         end
  t = Time.parse(time[0..9].split(":").join("/"))
  dir = "#{t.year}/#{'%02d' % t.month}"
  FileUtils.mkdir_p(dir)
  FileUtils.mv f, File.join(dir, Pathname(f).basename.to_s)
rescue Exif::NotReadable
  next
end
