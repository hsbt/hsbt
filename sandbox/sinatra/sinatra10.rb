require "sinatra"

outer_self = self

get '/' do
  content_type :txt
  "outer self: #{outer_self}, inner self: #{self}"
end
2
