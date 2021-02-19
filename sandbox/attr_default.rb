class Attr
  attr_reader :foo => :bar, :bar => :buzz, :buzz => :bar
  attr_writer foo: :bar, buzz: :bar
end
