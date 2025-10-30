#!/usr/bin/env ruby
# frozen_string_literal: true

# EML File Renamer based on ruby-core/ruby-talk/ruby-dev identifiers
# This script reads EML format email files and renames them based on
# the identifier found in the Subject header (e.g., [ruby-core:123456])

require "fileutils"

class EmlRenamer
  # Pattern to match [ruby-core:123456], [ruby-talk:12345], [ruby-dev:54321], etc.
  IDENTIFIER_PATTERN = /\[(ruby-(?:core|talk|dev|list)):(\d+)\]/i

  def initialize(directory = ".")
    @directory = directory
    @dry_run = false
    @renamed_count = 0
    @skipped_count = 0
    @error_count = 0
  end

  def rename_files(pattern: "*.eml", dry_run: false)
    @dry_run = dry_run
    puts "Scanning directory: #{@directory}"
    puts "Pattern: #{pattern}"
    puts "Mode: #{dry_run ? 'DRY RUN (no actual changes)' : 'LIVE'}"
    puts "-" * 60

    files = Dir.glob(File.join(@directory, pattern))

    if files.empty?
      puts "No files found matching pattern: #{pattern}"
      return
    end

    puts "Found #{files.size} file(s) to process"
    puts

    files.each do |file_path|
      process_file(file_path)
    end

    puts
    puts "-" * 60
    puts "Summary:"
    puts "  Renamed: #{@renamed_count}"
    puts "  Skipped: #{@skipped_count}"
    puts "  Errors:  #{@error_count}"
  end

  private

  def process_file(file_path)
    identifier = extract_identifier(file_path)

    if identifier.nil?
      puts "SKIP: #{File.basename(file_path)} (no identifier found)"
      @skipped_count += 1
      return
    end

    list_name, number = identifier
    subdir = list_name.to_s.downcase
    target_dir = File.join(File.dirname(file_path), subdir)
    new_filename = number.to_s
    new_path = File.join(target_dir, new_filename)

    # Ensure target directory exists (unless dry-run)
    FileUtils.mkdir_p(target_dir) unless @dry_run

    # Resolve name conflicts by appending .1, .2, ...
    chosen_path = new_path
    if File.exist?(chosen_path) && File.expand_path(file_path) != File.expand_path(chosen_path)
      i = 1
      loop do
        alt = File.join(target_dir, "#{new_filename}.#{i}")
        unless File.exist?(alt)
          chosen_path = alt
          break
        end
        i += 1
      end
    end

    # Skip if already correctly named
    if File.expand_path(file_path) == File.expand_path(chosen_path)
      puts "SKIP: #{File.basename(file_path)} (already correctly placed)"
      @skipped_count += 1
      return
    end

    # Rename the file
    if @dry_run
      puts "DRY RUN: #{File.basename(file_path)} -> #{File.join(subdir, File.basename(chosen_path))} [#{list_name}:#{number}]"
    else
      begin
        File.rename(file_path, chosen_path)
        puts "RENAMED: #{File.basename(file_path)} -> #{File.join(subdir, File.basename(chosen_path))} [#{list_name}:#{number}]"
      rescue StandardError => e
        puts "ERROR: Failed to rename #{File.basename(file_path)}: #{e.message}"
        @error_count += 1
        return
      end
    end

    @renamed_count += 1
  end

  def extract_identifier(file_path)
    # Read the file and extract the Subject header
    subject = extract_subject_header(file_path)
    return nil unless subject

    # Look for the identifier pattern in the subject
    match = subject.match(IDENTIFIER_PATTERN)
    return nil unless match

    [match[1], match[2]]
  end

  def extract_subject_header(file_path)
    subject_lines = []
    in_subject = false
    encoding = nil

    File.open(file_path, "r") do |file|
      file.each_line do |line|
        # Stop reading headers when we hit an empty line (body starts)
        break if line.strip.empty? && !in_subject

        # Detect if this line starts with "Subject:"
        if line =~ /^Subject:\s*/i
          in_subject = true
          subject_lines << line.sub(/^Subject:\s*/i, "")
          next
        end

        # Continuation lines start with whitespace (space or tab)
        if in_subject && line =~ /^\s+/
          subject_lines << line
        elsif in_subject
          # If we hit a line that doesn't start with whitespace, subject is complete
          break
        end
      end
    end

    return nil if subject_lines.empty?

    # Join all subject lines and clean up
    subject = subject_lines.join(" ").strip

    # Decode MIME encoded words if present (=?UTF-8?B?...?=)
    subject = decode_mime_header(subject)

    subject
  end

  def decode_mime_header(header)
    # Simple MIME header decoding for common cases
    # This handles patterns like =?UTF-8?B?base64text?= and =?ISO-2022-JP?B?...?=
    header.gsub(/=\?([^\?]+)\?([BQ])\?([^\?]+)\?=/i) do
      charset = $1
      encoding = $2.upcase
      encoded_text = $3

      begin
        case encoding
        when "B"
          # Base64 encoding
          decoded = encoded_text.unpack1("m")
          decoded.force_encoding(charset).encode("UTF-8", invalid: :replace, undef: :replace)
        when "Q"
          # Quoted-printable encoding
          decoded = encoded_text.gsub("_", " ").unpack1("M")
          decoded.force_encoding(charset).encode("UTF-8", invalid: :replace, undef: :replace)
        else
          encoded_text
        end
      rescue StandardError
        # If decoding fails, return the original text
        "=?#{charset}?#{encoding}?#{encoded_text}?="
      end
    end
  end
end

# CLI interface
if __FILE__ == $0
  require "optparse"

  options = {
    directory: ".",
    pattern: "*.eml",
    dry_run: false
  }

  OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options]"
    opts.separator ""
    opts.separator "Options:"

    opts.on("-d", "--directory DIR", "Directory containing EML files (default: current directory)") do |dir|
      options[:directory] = dir
    end

    opts.on("-p", "--pattern PATTERN", "File pattern to match (default: *.eml)") do |pattern|
      options[:pattern] = pattern
    end

    opts.on("-n", "--dry-run", "Dry run mode (show what would be renamed without making changes)") do
      options[:dry_run] = true
    end

    opts.on("-h", "--help", "Show this help message") do
      puts opts
      exit
    end
  end.parse!

  unless Dir.exist?(options[:directory])
    puts "Error: Directory does not exist: #{options[:directory]}"
    exit 1
  end

  renamer = EmlRenamer.new(options[:directory])
  renamer.rename_files(pattern: options[:pattern], dry_run: options[:dry_run])
end
