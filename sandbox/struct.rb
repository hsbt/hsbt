class Foo < Struct.new(:params, :headers)
  def params=(hash)
    super
  end

  def []=(key, value)
    headers[key] = value
  end
end

Foo.new.params = {}
