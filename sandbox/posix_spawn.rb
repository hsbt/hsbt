require "benchmark"
require "posix-spawn"

Benchmark.bm do |x|
  x.report ("normal") {
    spawn("true")
  }

  x.report ("posix-spawn") {
    POSIX::Spawn.pspawn("true")
  }
end
