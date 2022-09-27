require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "syntax_suggest"
end

require_relative "bar"
