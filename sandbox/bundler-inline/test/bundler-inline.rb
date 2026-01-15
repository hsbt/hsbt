# frozen_string_literal: true

require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "rack-test"
end

binding.irb
require "rack/test"
