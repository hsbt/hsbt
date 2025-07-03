Dir.glob("**/*.rb") do |f|
  next if f.include?("-ext-")
  logger/test_logger.rb
  ruby/enc/test_euc_jp.rb
  ruby/test_keyword.rb
  ruby/test_proc.rb
  ruby/test_syntax.rb
  test_tempfile.rb
  `rubocop --only Lint/UselessAssignment #{f}`
end
