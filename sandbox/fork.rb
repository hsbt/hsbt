# -*- coding: utf-8 -*-
child_processes = 3
dead_processes = 0
# We fork 3 child processes.
child_processes.times do
  fork do
    # They sleep for 3 seconds.
    sleep 3
  end
end
# Sync $stdout so the call to #puts in the CHLD handler isn't
# buffered. Can cause a ThreadError if a signal handler is
# interrupted after calling #puts. Always a good idea to do
# this if your handlers will be doing IO.
$stdout.sync = true
# Our parent process will be busy doing some intense mathematics.
# But still wants to know when one of its children exits.
# By trapping the :CHLD signal our process will be notified by the kernel # when one of its children exits.
trap(:CHLD) do
# Since Process.wait queues up any data that it has for us we can ask for it
# here, since we know that one of our child processes has exited.
# We loop over a non-blocking Process.wait to ensure that any dead child
# processes are accounted for.
  # begin
  #   while pid = Process.wait(-1, Process::WNOHANG) do
  #     puts pid
  #     dead_processes += 1
  #     # We exit ourselves once all the child processes are accounted for.
  #     exit if dead_processes == child_processes
  #   end
  # rescue Errno::ECHILD
  # end
  puts Process.wait
  dead_processes += 1
  exit if dead_processes == child_processes
end

# Work it.
loop do
  (Math.sqrt(rand(44)) ** 8).floor
  sleep 1
end
