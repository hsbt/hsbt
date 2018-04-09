#!/usr/bin/env ruby

%x{git checkout ruby-core}
commits = %x{git log --oneline #{ARGV[0]}...HEAD #{ARGV[1..-1].join(" ")}| awk -F" " '\{print $1\}'}
%x{git checkout master}
%x{git checkout -b #{ARGV[0]}}

commits.split.reverse.each do |commit|
  %x{git cherry-pick #{commit}}
end
