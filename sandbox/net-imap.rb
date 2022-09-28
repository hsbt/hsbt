require "bundler/inline"

gemfile do
  source "https://rubygems.org"
  gem "net-imap"
end

require "net/imap"

imap = Net::IMAP.new('imap.gmail.com', 993, true)
imap.login(ENV['GMAIL_USERNAME'], ENV['GMAIL_PASSWORD'])

binding.irb
