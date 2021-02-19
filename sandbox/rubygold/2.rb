XXX = "123"

class Foo
  def foo
    XXX
    XXX = "456"
    XXX += "456"
    XXX << "456"
  end
end

Foo.new.foo
