%w(awesome_print hirb pry-nav pry-remote).each do |file|
  begin
    require file
  rescue LoadError
  end
end

Pry.config.history.file = "~/.irb_history"

if defined? PryNav
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
end

if defined? AwesomePrint
  Pry.print = proc {|output, value| output.puts value.ai }
end
