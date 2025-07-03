class A
  def foo
    puts "A"
  end
end

module B
  def foo
    puts "B"
  end
end

class C < A
  include B
  def foo
    super
    puts "C"
  end
end

C.new.foo
