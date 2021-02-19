require 'fileutils'
require 'optparse'

module FileUtils
  @fileutils_output = $stdout
end

def setup(options = '', *long_options)
  caller = caller_locations(1, 1)[0].label
  opt_hash = {}
  argv = []
  OptionParser.new do |o|
    options.scan(/.:?/) do |s|
      opt_name = s.delete(":").intern
      o.on("-" + s.tr(":", " ")) do |val|
        opt_hash[opt_name] = val
      end
    end
    long_options.each do |s|
      opt_name, arg_name = s.split(/(?=[\s=])/, 2)
      opt_name.sub!(/\A--/, '')
      s = "--#{opt_name.gsub(/([A-Z]+|[a-z])([A-Z])/, '\1-\2').downcase}#{arg_name}"
      puts "#{opt_name} => #{s}" if $DEBUG
      opt_name = opt_name.intern
      o.on(s) do |val|
        opt_hash[opt_name] = val
      end
    end
    o.on("-v") {
      opt_hash[:verbose] = true
    }
    o.on("--help") do
      UN.help([caller])
      exit
    end
    o.order!(ARGV) do |x|
      if /[*?\[{]/ =~ x
        argv.concat(Dir[x])
      else
        argv << x
      end
    end
  end
  yield argv, opt_hash
end

def cp
  setup("pr") do |argv, options|
    cmd = "cp"
    cmd += "_r" if options.delete :r
    options[:preserve] = true if options.delete :p
    dest = argv.pop
    argv = argv[0] if argv.size == 1
    FileUtils.send cmd, argv, dest, options
end

def httpd
  setup("", "BindAddress=ADDR", "Port=PORT", "MaxClients=NUM", "Tempdir=DIR",
    "DoNotReverseLookup", "RequestTimeout=SECOND", "HTTPVersion=VERSION") do
    |argv, options|
    require 'WEBrick'
    opt = options[:RequestTimeout] and options[:RequestTimeout] = opt.to_i
    [:Port, :MaxClients].each do |name|
      opt = options[name] and (options[name] = Integer(opt)) rescue nil
    end
    unless argv.size == 1
      raise ArgumentError, "DocumentRoot is mandatory"
    end

    options[:DocumentRoot] = argv.shift

    s = WEBrick::HTTPServer.new(options)
    shut = proc{s.shutdown}
    siglist = %w"TERM QUIT"
    siglist.concat(%w"HUP INT") if STDIN.tty?
    siglist &= Signal.list.keys
    siglist.each do |sig|
      Signal.trap(sig, shut)
    end
    s.start
  end
end
