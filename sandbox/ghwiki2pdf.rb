#!/usr/bin/env ruby
# -*- coding: utf-8; -*-

require "rubygems"
require "redcarpet"
require "wicked_pdf"
require "pathname"

WickedPdf.config = { exe_path: "/Applications/wkhtmltopdf.app/Contents/MacOS/wkhtmltopdf" }

Dir["*.md"].each do |md|
  target = Pathname.new(md)
  header = <<-HEADER
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html lang="ja-JP">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>#{target.basename('.md')}</title>
</head>
<body>
HEADER
  footer = <<-FOOTER
</body>
</html>
FOOTER
  html = Redcarpet::Markdown.new(Redcarpet::Render::HTML, fenced_code_blocks: true).render(File.open(md).read)
  open(target.sub(/\.md\z/, ".pdf"), "wb") { |f| f.write WickedPdf.new.pdf_from_string(header + html + footer) }
end
