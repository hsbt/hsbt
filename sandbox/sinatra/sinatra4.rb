require 'sinatra'

get '/*' do
  "You passed in #{params[:splat]}"
end

get '/specific' do
  "You'll never, ever see me."
end
