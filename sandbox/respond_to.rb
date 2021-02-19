class A
  def initialize
    p respond_to?(:foo)
    p respond_to?(:foo, true)
  end

  protected

  def foo
  end
end

A.new
