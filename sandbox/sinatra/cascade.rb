require 'sinatra/base'

class Foo1 < Sinatra::Base
  get('/foo') { not_found }
end

class Foo2 < Sinatra::Base
  get('/foo') { 'foo #2' }
end

run Rack::Cascade, [Foo1, Foo2]
