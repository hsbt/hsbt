require "timeout"

a = ""
begin
  timeout(1) do
    a = :bar
  end
rescue
end

p a
