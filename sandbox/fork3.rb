require "socket"

child, parent = Socket.pair(:UNIX, :DGRAM, 0)
maxlen = 1000

fork do
  parent.close
  4.times do
    ins = child.recv(maxlen)
    child.send("#{ins} foo", 0)
  end
end

child.close

2.times do
  parent.send("lift", 0)
end

2.times do
  parent.send("bar", 0)
end

4.times do
  $stdout.puts parent.recv(maxlen)
end
