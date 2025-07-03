require "sinatra"

get("/public.html") do
  "This is delivered via the route."
end

get %r(/(sp|gr)eedy) do
  "You got caught in the greedy route!"
end

get "/speedy" do
  "No one calls me :("
end

get "/greedy" do
  "No one calls me either!"
end

get "/halt" do
  "You will not see this output."
  halt 500
end
