# -*- coding: utf-8 -*-
require "sinatra"

get "/:name" do
  "Hello, #{params[:name]}!"
end

post "/login" do
  username = params[:username]
  password = params[:password]
  "#{username} #{password}"
end

put "/users/:id" do
  first_name = params[:first_name]
  last_name = params[:last_name]
  "#{first_name} #{last_name}"
end
