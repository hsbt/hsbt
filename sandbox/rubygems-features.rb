require "bundler/inline"

gemfile do
  source "https://rubygems.org"
end

puts $LOADED_FEATURES.sort
