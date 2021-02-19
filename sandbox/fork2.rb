reader, writer = IO.pipe

fork do
  reader.close
  10.times do
    writer.puts "foo"
  end
end

writer.close

while message = reader.gets
  $stdout.puts message
end
