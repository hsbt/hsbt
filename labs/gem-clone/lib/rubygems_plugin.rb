require 'rubygems/command_manager'
require 'rubygems/commands/clone_command'

Gem::CommandManager.instance.register_command :clone
