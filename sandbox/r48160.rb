class Foo
  def bar
  end
end

instance = Foo.new
method_proc = instance.method(:bar).to_proc
b = method_proc.binding

b.local_variable_set(:value, [1, 2, 3])
