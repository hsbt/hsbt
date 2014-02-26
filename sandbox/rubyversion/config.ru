require 'sinatra/base'

class RubyVersionApp < Sinatra::Base
  get '/' do
    "Hello, Ruby!: #{RUBY_VERSION} #{RUBY_PATCHLEVEL}"
  end
end

run RubyVersionApp
