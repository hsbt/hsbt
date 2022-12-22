require "bundler/inline"

gemfile do
  gem "webrick"
  gem "rack", "< 3"
end

require 'rack'

counter = 0

Thread.new do
  loop do
    puts "hello, #{counter}"
    counter += 1
    sleep 1
  end
end

app = Proc.new do |env|
  [200, {'Content-Type' => 'text/plain; charset=utf-8'}, [counter]]
end

Rack::Handler::WEBrick.run [app, {
  :Host => '127.0.0.1',
  :Port => 8080,
}];
