require "pathname"
require "fileutils"

Dir.glob("**/*").each do |file|
  path = Pathname.new(file)

  FileUtils.rm_r path if Dir.empty?(path)
end

Dir.glob("**/*").each do |file|
  path = Pathname.new(file)

  if %w(.png).include?(path.extname)
    new_path = path.dirname.to_s.chomp + path.extname
    puts "Renaming #{file} to #{new_path}"
    File.rename(file, new_path)
    puts "Removing #{path.dirname}"
    FileUtils.rm_r path.dirname
  end
end

Dir.glob("**/*").each do |file|
  path = Pathname.new(file)

  if %w(.md).include?(path.extname)
    puts "Processing #{file}"
    text = File.read(file)
    new_contents = text.gsub(/\/skitch.png/, ".png")
    File.open(file, "w") { |file| file.puts new_contents }
  end
end
