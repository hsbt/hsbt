begin
  code = "p :foo"
  lambda {
    $SAFE = 4
    eval( "BEGIN {return true}\n#{code}", nil, "(plugin)", 0 )
  }.call
rescue SyntaxError
  p :bar
end
