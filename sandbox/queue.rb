require "thread"

q = Queue.new
Thread.new do
  while last = q.pop
    sleep(1) # simulate cost
    puts last
  end
end

q << :foo
sleep(2)
$stdout.flush
