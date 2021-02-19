require 'sinatra'

get '/redirect' do
  redirect 'http://www.google.com'
end

get '/redirect2' do
  redirect 'http://www.google.com', 301
end
