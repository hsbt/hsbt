ru = %r{foo}

l = -> { l.() }
begin
  l.()
rescue SystemStackError
  l.() # segfault
end
