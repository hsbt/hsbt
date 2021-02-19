class Bar
  def buzz
    @_buzz ||= 1
    p @_buzz.object_id
    @_buzz
  end
end

foo = Bar.new
foo.buzz
foo.buzz
