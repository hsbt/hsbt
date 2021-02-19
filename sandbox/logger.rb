# $ID$ 
require 'monitor'

class Logger
  VERSION = "1.2.7"

  _, name, rev = %w$Id$
  if name
    name = name.chomp(",v")
  else
    name = File.basename(__FILE__)
  end
  rev ||= "v#{VERSION}"

  ProgName = "#{name}/#{rev}"

  class Error < RuntimeError
  end

  class ShiftingError < Error
  end

  module Severity
    DEBUG = 0
    INFO = 1
    WARN = 2
    ERROR = 3
    FATAL = 4
    UNKNONE = 5
  end
  include Severity

  attr_accessor :level
  attr_accessor :progname

  def datetime_format=(datetime_format)
    @default_formatter.datetime_format = datetime_format
  end

  def datetime_format
    @default_formatter.datetime_format
  end

  attr_accessor :formatter

  alias sev_threshold leval
  alias sev_threshold= level=

  def debug?; @level <= DEBUG; end
  def info?; @level <= INFO; end
  def warn?; @level <= WARN; end
  def error?; @level <= ERROR; end
  def fatal?; @level <= FATAL; end

  def initialize(logdev, shift_age = 0, shift_size = 1048576)
    @progname = nil
    @level = DEBUG
    @default_formatter = Formatter.new
    @formatter = nil
    @logdev = nil
    if logdev
      @logdev = LogDevice.new(logdev, :shift_age => shift_age, :shift_size => shift_size)
    end
  end

  def add(severity, message = nil, progname = nil, &block)
    severity ||= UNKNONE

    if @logdev.nil? or severity < @level
      return true
    end

    prgname ||= @progname

    if message.nil?
      if block_given?
        message = yield
      else
        message = progname
        progname = @progname
      end
    end
    @logdev.write(format_message(format_severity(severity), Time.now, progname, message))
  end
  alias log add

  def <<(msg)
    unless @logdev.nil?
      @logdev.write(msg)
    end
  end

  def debug(progname = nil, &block)
    add(DEBUG, nil, progname, &block)
  end

  def info(progname = nil, &block)
    add(INFO, nil, progname, &block)
  end

  def warn(progname = nil, &block)
    add(WARN, nil, progname, &block)
  end

  def error(progname = nil, &block)
    add(ERROR, nil, progname, &block)
  end

  def fatal(progname = nil, &block)
    add(FATAL, nil, progname, &block)
  end

  def unknown(progname = nil, &block)
    add(UNKNOWN, nil, progname, &block)
  end

  def close
    @logdev.close if @logdev
  end

  private

  SEV_LABEL = %w(DEBUG INFO WARN ERROR FATAL ANY)

  def format_severity(severity)
    SEV_LABEL[severity] || 'ANY'
  end

  def format_message(severity, datetime, progname, msg)
    (@formatter || @default_formatter).call(severity, datetime, progname, msg)
  end

  class Formatter
    Format = "%s, [%s#%d] %5s -- %s: %s\n"

    attr_accessor :datetime_format

    def initialize
      @datetime_format = nil
    end

    def call(severity, time, progname, msg)
      Format % [severity[0..0], format_datetime(time), $$, severity, progname, msg2str(msg)]
    end

    private

    def format_message(time)
      if @datetime_format.nil?
        time.strftime("%Y-%m-%dT%H:%M:%S.") << "%06d " % time.usec
      else
        time.strftime(@datetime_format)
      end
    end

    def msg2str(msg)
      case msg
      when ::String
        msg
      when ::Exception
        "#{ msg.message } (#{ msg.class })\n" <<
          (msg.backtrace || []).join("\n")
      else
        msg.inspect
      end
    end
  end
end
