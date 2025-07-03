require "benchmark"

require "redis"

n = 1_00_000
Benchmark.bm do |x|
  x.report {
    redis = Redis.new
    n.times {
      redis.set("mykey", "hello world")
      redis.get("mykey")
    }
  }
  x.report {
    require "hiredis"
    require "redis/connection/hiredis"
    redis = Redis.new
    n.times {
      redis.set("mykey", "hello world")
      redis.get("mykey")
    }
  }
end
