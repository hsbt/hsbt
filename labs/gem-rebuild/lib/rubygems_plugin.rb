require 'rubygems/command_manager'
require 'rubygems/commands/rebuild_command'

Gem::CommandManager.instance.register_command :rebuild
