#!/usr/bin/env ruby

`git checkout ruby-core`
commits = `git log --oneline #{ARGV[0]}...HEAD #{ARGV[1..-1].join(" ")}| awk -F" " '\{print $1\}'`
`git checkout master`
`git checkout -b #{ARGV[0]}`

commits.split.reverse_each do |commit|
  `git cherry-pick #{commit}`
end
