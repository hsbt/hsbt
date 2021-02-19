def render(one = nil, two = nil)
  "bar"
end

def bar
  return render => 'foo', :layout => false
end

p bar
