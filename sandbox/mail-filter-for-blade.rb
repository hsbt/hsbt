require "bundler/inline"

gemfile do
  gem "mail"
  gem "net-smtp"
end

require "pathname"
require "fileutils"

base_dir = Pathname(ARGV[0])
blade_dir = base_dir + "blade-version"

FileUtils.mkdir(blade_dir) unless blade_dir.exist?

Dir.glob( base_dir.join("*") ).each do |file|
  next if File.directory?(file)
  mail = Mail.read( file )
  puts file
  File.open( Pathname(blade_dir).join( File.basename(file) ), "w" ) do |f|
    from = begin
      mail.header["from"].to_s.gsub(/@[a-zA-Z.\-]+/, "@...")
    rescue
      mail.header['from']
    end
    f.puts "From: #{from}"
    f.puts "Date: #{mail.date}"
    begin
      f.puts "Subject: #{mail.subject}"
    rescue Encoding::CompatibilityError
      f.puts "Subject: "
    end
    f.puts ""
    f.puts mail.body.to_s.encode("UTF-8", "ISO-2022-JP", invalid: :replace, undef: :replace)
  end
end
