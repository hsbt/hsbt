module Foo
  require "fiddle/import"
  require "fiddle/types"

  extend Fiddle::Importer
  dlload "/usr/local/lib/libmigemo.1.1.0.dylib"
  include Fiddle::Win32Types

  parse
end
