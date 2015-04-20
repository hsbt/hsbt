%w(awesome_print hirb pry-byebug pry-remote).each do |file|
  begin
    require file
  rescue LoadError
  end
end

if defined? AwesomePrint
  Pry.print = proc {|output, value| output.puts value.ai }
end

if defined?(Pry)
  Pry.config.history.file = File.expand_path("~/.irb_history")
  Pry::Commands.command /^$/, "repeat last command" do
    _pry_.run_command Pry.history.to_a.last
  end
end

if defined?(PryByebug)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
end
