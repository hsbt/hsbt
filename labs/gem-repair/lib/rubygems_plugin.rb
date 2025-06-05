require 'rubygems/command_manager'
require 'rubygems/commands/repair_command'

Gem::CommandManager.instance.register_command :repair
