#!/usr/bin/env ruby

Dir.glob('**/*').each do |file|
  next if File.directory?(file)
  body = File.read(file)

  body.gsub!(/\{\{\{([^\n]+?)\}\}\}/, '`\1`')
  body.gsub!(/\{\{\{(.+?)\}\}\}/m){|m| m.each_line.map{|x| "\t#{x}".gsub(/[\{\}]{3}/,'')}.join}
  body.gsub!(/\=\=\=\=\s(.+?)\s\=\=\=\=/, '### \1')
  body.gsub!(/\=\=\=\s(.+?)\s\=\=\=/, '## \1')
  body.gsub!(/\=\=\s(.+?)\s\=\=/, '# \1')
  body.gsub!(/\=\s(.+?)\s\=[\s\n]*/, '')
  body.gsub!(/\[(http[^\s\[\]]+)\s([^\[\]]+)\]/, '[\2](\1)')
  body.gsub!(/\!(([A-Z][a-z0-9]+){2,})/, '\1')
  body.gsub!(/'''(.+)'''/, '*\1*')
  body.gsub!(/''(.+)''/, '_\1_')
  body.gsub!(/^\s\*/, '*')
  body.gsub!(/^\s\d\./, '1.')

  File.open(file.gsub(/\//, '').gsub(/README/, ''), 'w').write(body)
end
