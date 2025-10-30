#!/usr/bin/env ruby
# frozen_string_literal: true

# mbox-to-eml.rb
# Convert an mbox file into individual .eml files (one email per file).
# - Splits on mbox 'From ' delimiter lines
# - Writes raw RFC822 message (excluding the mbox delimiter)
# - Names files using Date/Message-ID when possible, otherwise uses a counter
#
# Usage examples:
#   ./sandbox/mbox-to-eml.rb -i inbox.mbox -o ./out_eml
#   ./sandbox/mbox-to-eml.rb -i inbox.mbox -o ./out_eml --prefix ruby-core-
#   ./sandbox/mbox-to-eml.rb -i inbox.mbox    # outputs to ./mbox_eml/

require "optparse"
require "fileutils"
require "time"
require "securerandom"

class MboxToEml
  DELIM_REGEX = /^From \S+ .*/.freeze # mbox separator (not the 'From:' header)

  def initialize(input_path:, output_dir:, prefix: "", include_from_line: false, zero_pad: 5)
    @input_path = input_path
    @output_dir = output_dir
    @prefix = prefix
    @include_from_line = include_from_line
    @zero_pad = zero_pad

    FileUtils.mkdir_p(@output_dir)
  end

  def convert
    raise "Input mbox does not exist: #{@input_path}" unless File.file?(@input_path)

    count = 0
    buffer = []
    current_delim = nil

    File.open(@input_path, "rb") do |f|
      f.each_line do |line|
        if line.start_with?("From ") && line.match?(DELIM_REGEX)
          # delimiter encountered: flush previous message if present
          if !buffer.empty?
            count += 1
            write_message(buffer, count, current_delim)
            buffer.clear
          end
          current_delim = line
          next unless @include_from_line
          # Optionally include the mbox delimiter at top of message
          buffer << line
        else
          buffer << line
        end
      end
    end

    # flush last message
    if !buffer.empty?
      count += 1
      write_message(buffer, count, current_delim)
    end

    puts "Done. Wrote #{count} message(s) to #{@output_dir}/"
  end

  private

  def write_message(lines, index, delim)
    raw = lines.join

    # Extract headers for naming
    headers_text, _body = split_headers_body(raw)
    date_str = header_value(headers_text, "Date")
    msgid = header_value(headers_text, "Message-ID") || header_value(headers_text, "Message-Id")

    timestamp = nil
    if date_str
      begin
        timestamp = Time.parse(date_str).utc.strftime("%Y%m%d%H%M%S")
      rescue StandardError
        timestamp = nil
      end
    end

    id_part = nil
    if msgid
      id_part = sanitize_msgid(msgid)
      id_part = nil if id_part.nil? || id_part.empty?
    end

    base = if timestamp && id_part
      "#{timestamp}-#{id_part}"
    elsif timestamp
      "#{timestamp}-#{index.to_s.rjust(@zero_pad, '0')}"
    elsif id_part
      id_part
    else
      index.to_s.rjust(@zero_pad, '0')
    end

    filename = "#{@prefix}#{base}.eml"
    path = File.join(@output_dir, filename)

    # Avoid accidental overwrite: add a short suffix if exists
    if File.exist?(path)
      suffix = SecureRandom.hex(3)
      path = File.join(@output_dir, "#{@prefix}#{base}-#{suffix}.eml")
    end

    File.open(path, "wb") { |io| io.write(raw) }
    puts "Wrote #{File.basename(path)}"
  end

  def split_headers_body(raw)
    # Headers end at first blank line (\r?\n\r?\n)
    if (i = raw.index(/\r?\n\r?\n/))
      [raw[0...i], raw[(i + ($&.length))..] || ""]
    else
      [raw, ""]
    end
  end

  def header_value(headers_text, name)
    # Extract header with folding support
    value_lines = []
    current = nil

    headers_text.each_line do |line|
      if line =~ /^([\w-]+):\s*(.*)$/
        hname = Regexp.last_match(1)
        hval  = Regexp.last_match(2)
        if hname.casecmp?(name)
          current = name
          value_lines = [hval.chomp]
        else
          # finalize if we were collecting
          break if current == name
          current = nil
        end
      elsif current == name && line =~ /^\s+/
        value_lines << line.strip
      elsif current == name
        # end of header block
        break
      end
    end

    return nil if value_lines.empty?
    value_lines.join(" ")
  end

  def sanitize_msgid(msgid)
    # Message-ID typically like <foo@bar>. Strip angle brackets and unsafe chars
    s = msgid.strip
    s = s[1..-2] if s.start_with?("<") && s.end_with?(">")
    s.gsub!(/[^A-Za-z0-9._-]+/, "-")
    s
  end
end

if __FILE__ == $0
  options = {
    input: nil,
    output: nil,
    prefix: "",
    include_from_line: false
  }

  parser = OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} -i INPUT.mbox [-o OUTPUT_DIR] [--prefix PREFIX] [--include-from-line]"

    opts.on("-i", "--input FILE", "Input mbox file path") { |v| options[:input] = v }
    opts.on("-o", "--output DIR", "Output directory (default: ./mbox_eml)") { |v| options[:output] = v }
    opts.on("--prefix PREFIX", "Filename prefix for .eml files") { |v| options[:prefix] = v }
    opts.on("--include-from-line", "Include the leading mbox 'From ' delimiter line in each .eml") { options[:include_from_line] = true }
    opts.on("-h", "--help", "Show help") do
      puts opts
      exit
    end
  end

  parser.parse!

  if options[:input].nil?
    warn "Error: input mbox is required."
    puts parser
    exit 1
  end

  unless File.file?(options[:input])
    warn "Error: input file not found: #{options[:input]}"
    exit 1
  end

  options[:output] ||= File.join(Dir.pwd, "mbox_eml")

  converter = MboxToEml.new(
    input_path: options[:input],
    output_dir: options[:output],
    prefix: options[:prefix],
    include_from_line: options[:include_from_line]
  )
  converter.convert
end
