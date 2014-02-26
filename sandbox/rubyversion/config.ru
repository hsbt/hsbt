require 'sinatra/base'

class MyApp < Sinatra::Base
  get '/' do
    "Hello, Namahage: #{RUBY_VERSION}"
  end
end

run MyApp
