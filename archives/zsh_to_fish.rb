require "yaml"

r = {}
File.open("dot.zsh-history") do |f|
  r = f.each_line.inject([]) { |a, l|
    a << {"cmd" => l[15..-1].to_s.scrub.strip, "when" => l[1..11].to_i}
  }
end

File.open("fish_history", "w") { |f| f.write r.to_yaml(options = {line_width: -1}) }
