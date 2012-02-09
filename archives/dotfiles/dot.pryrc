Pry.config.history.file = "~/.irb_history"

require "awesome_print" rescue nil
if defined? AwesomPrint
  Pry.print = proc { |output, value| output.puts value.ai }
end
