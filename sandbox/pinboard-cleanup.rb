require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "pinboard", require: "pinboard"
  gem "dotenv"
end

require "dotenv"
Dotenv.load

client = Pinboard::Client.new(token: ENV["PINBOARD_TOKEN"])

# Get all bookmarks
bookmarks = client.posts

binding.irb
