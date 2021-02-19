require 'sinatra'

before do
  content_type :txt
end

get %r{/(sp|gr)eedy} do
  pass if request.path =~ /\/speedy/
  "You got caught in the greedy route!"
end

get '/speedy' do
  "You must have passed to me!"
end
