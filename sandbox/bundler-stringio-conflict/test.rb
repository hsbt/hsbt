# frozen_string_literal: true

require "bundler/inline"

gemfile(true) do
  source "https://rubygems.org"

  gem "psych"
  gem "minitest"
end

require "minitest/autorun"

class BugTest < Minitest::Test
  def test_stuff
    assert Psych::VERSION
    puts Psych::VERSION
  end
end
