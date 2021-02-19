require 'sinatra'

get '/' do
  "Triggered via GET"
end

post '/' do
  "Triggered via POST"
end

put '/' do
  "Triggered via PUT"
end

delete '/' do
  "Triggered via DELETE"
end

patch '/' do
  "Triggered via PATCH"
end

options '/' do
  "Triggered via OPTIONS"
end

set :public_folder, File.dirname(__FILE__) + '/your_custom_location'
