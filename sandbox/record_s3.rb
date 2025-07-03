require "aws-sdk-s3"
require "mail"

def record_s3(msg)
  m = Mail.new(msg)
  list_name = m.header["List-Id"].to_s.match(/\<(.*)\.ml\.ruby\-lang\.org\>/)
  list_name = list_name && list_name[1]
  post_id = m.header["Subject"].to_s.match(/\[#{list_name}:(\d+)\].*/)
  post_id = post_id && post_id[1]

  from = begin
    m.header["from"].to_s.gsub(/@[a-zA-Z.\-]+/, "@...")
  rescue
    m.header["from"]
  end
  io = StringIO.new
  io.puts "From: #{from}"
  io.puts "Date: #{m.date}"
  begin
    io.puts "Subject: #{m.subject}"
  rescue Encoding::CompatibilityError
    io.puts "Subject: "
  end
  io.puts ""
  io.puts m.body.to_s.encode("UTF-8", "ISO-2022-JP", invalid: :replace, undef: :replace)

  s3 = Aws::S3::Resource.new(
    region: "ap-northeast-1",
    access_key_id: ENV["AWS_ACCESS_KEY_ID"],
    secret_access_key: ENV["AWS_SECRET_ACCESS_KEY"]
  )
  bucket = s3.bucket("blade.ruby-lang.org")
  bucket.object("#{list_name}/#{post_id}").put(body: io.string)
ensure
  io.close
end

msg = File.read(ARGV[0])
record_s3(msg)
