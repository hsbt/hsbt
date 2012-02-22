#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'hikidoc'

class Rubima2Mobi
  def initialize
    @filename = ARGV.shift
  end

  def to_html
    open(@filename + '.html', 'w') do |f|
      f.puts html_header ARGV.shift
      f.puts html_body
      f.puts html_footer
    end
  end

  def to_mobi
    require 'kindlegen'
    Kindlegen.run(@filename + '.html', "-o", @filename + '.mobi')
  end

  def html_header( title )
    <<-HTML
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></meta>
  <title>#{title}</title>
  <link rel="stylesheet" href="style.css" type="text/css" media="all"></link>
</head>
<body>
  <h1>#{title}</h1>
HTML
  end

  def html_footer
    <<-HTML
</body>
</html>
HTML
  end

  def html_body
    html = HikiDoc.to_html(File.open(@filename).read)
    html.gsub!(/{{.*}}/, '') # TODO remove hiki plugin syntax
    html
  end
end

Rubima2Mobi.new.to_mobi
