require "sinatra/base"

class MyApp < Sinatra::Base
  self.get "/" do
    "Hello from MyApp!"
  end
end

MyApp.run!
