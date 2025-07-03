require "benchmark/ips"

class Foo
  def initialize
    @attributes = { foo: :x, bar: :y }
  end

  def foo
    @attributes[:foo]
  end

  private

  def method_missing(method, *arguments, &block)
    @attributes[method]
  end
end

Benchmark.ips do |x|
  x.report("method") { Foo.new.foo }
  x.report("method_missing") { Foo.new.bar }
end
