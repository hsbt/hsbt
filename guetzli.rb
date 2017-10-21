require 'fileutils'
require 'pathname'

require 'parallel'

Parallel.each(Dir.glob('**/*.jp*'), in_threads: 4) do |f|
  p f
  o = f + '.new'
  next if File.exist?(o)
  `guetzli #{f} #{o}`
  if File.exist?(o)
    FileUtils.cp o, f
    FileUtils.rm o
  end
end
