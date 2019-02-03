#!/usr/bin/env ruby

File.readlines("authorized_keys").each do |key|
  puts key.match(/\-\-tunnel-user=(.*)" (.*) ?/)[1..2].reverse.join(" ")
end
