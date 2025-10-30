class ExampleError < StandardError
end

class ExampleTestError < ExampleError
end

begin
  raise ExampleTestError
rescue ExampleError => e
  puts "Caught an ExampleError: #{e.class}"
end
