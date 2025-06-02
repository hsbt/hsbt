require 'test_helper'

class CloneCommandTest < Minitest::Test
  def setup
    @command = Gem::Commands::CloneCommand.new
  end

  def test_initialize
    assert_equal 'clone', @command.command
    assert_equal 'Clone a gem repository using ghq', @command.summary
  end

  def test_arguments
    assert_includes @command.arguments, 'GEM_NAME'
  end

  def test_description
    assert_includes @command.description, 'clone command fetches gem metadata'
  end
end
