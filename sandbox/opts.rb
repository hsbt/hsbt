require 'optparse'
options = {}
parser = OptionParser.new do |opts|
  opts.on("-p", "--password <num>", "The password of redis server") do |v|
    options[:password] = v
  end
end
parser.parse!

p options

