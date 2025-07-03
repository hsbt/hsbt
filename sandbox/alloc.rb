require "allocation_tracer"
require "pp"

ObjectSpace::AllocationTracer.setup(%i{path line type})

pp ObjectSpace::AllocationTracer.trace {
  50_000.times { |i|
    a = i.to_s
    b = { bar: i.to_s }
    c = [i.to_s]
  }
}
