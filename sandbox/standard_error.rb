require 'timeout'
begin
  p :foo
  raise Timeout::Error
rescue => e
  p e
end
