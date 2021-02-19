1.upto(10_000) do |i|
  Thread.new { sleep }
  puts i
end
