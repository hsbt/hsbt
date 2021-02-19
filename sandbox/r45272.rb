class Bar
  def bar(foo = foo())
    foo
  end

  def buzz(foo = foo)
    foo
  end

  def foo
    :buzz
  end
end

p Bar.new.bar
p Bar.new.buzz
