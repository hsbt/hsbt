#/usr/bin/env ruby

Dir.glob('**/*.txt') do |file|
  puts "stripping #{file}"
  begin
    body = File.open(file) do |f|
      tmp = f.each_line.to_a
      tmp.delete_if do |line|
        line =~ /\ \!\ / || line =~ /\ from\ \#/ || line =~ /\ to\ \#/ || line =~ /\ ->\ /
      end
      tmp
    end.join
    File.open(file, "w"){|f| f.write body}
    File.delete(file) if File.size(file) == 0
  rescue ArgumentError => e
    puts e
    next
  end
end
