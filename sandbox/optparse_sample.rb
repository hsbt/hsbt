require 'optparse'

opts = OptionParser.new
opts.on('-v')
opts.on('-h')
opts.parse!(ARGV)

p opts.getopts
