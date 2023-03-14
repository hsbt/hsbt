#!/usr/bin/env ruby

require "rubygems/commands/install_command"

cmd = Gem::Commands::InstallCommand.new
cmd.options[:args] = ["rails"]
cmd.execute
