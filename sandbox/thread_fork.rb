class A
  attr_accessor :x
end

require "thread"

read, write = IO.pipe

pid = fork do
  read.close
  a = A.new
  retval = (dumped = Marshal.dump(a); Marshal.load(dumped))
  write.puts [Marshal.dump([retval, a])].pack("m")
  exit!
end

write.close
result = read.read
Process.wait2(pid)

serialized = result.unpack("m")[0]
p Marshal.load(serialized)
