require "json"
require "fileutils"

Dir.glob("*.json") do |file|
  json = JSON.parse(File.read(file))
  dir = file.gsub(/.json/, "")

  FileUtils.mkdir_p(dir)
  Dir.chdir(dir) do
    json["entries"].each do |entry|
      open("#{entry["uuid"]}.txt", "w") do |f|
        f.write entry["text"]
      end
    end
  end
end
