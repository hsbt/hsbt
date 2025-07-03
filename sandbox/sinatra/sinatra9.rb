require "sinatra"

configure do
  enable :sessions
end

before do
  content_type :txt
end

get "/set" do
  session[:foo] = Time.now
  "set!!"
end

get "/get" do
  "session: #{session[:foo]}"
end
