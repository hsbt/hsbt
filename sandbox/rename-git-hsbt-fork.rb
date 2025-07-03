#!/usr/bin/env ruby
require "parallel"

IGNORE_REPOS = %w[
]

Dir.chdir(`ghq root`.chomp) do
  Parallel.map(`ghq list`.split.shuffle) do |dir|
    next if IGNORE_REPOS.any? { |repo| dir.include?(repo) }
    next unless File.directory?(dir + "/.git")

    Dir.chdir(dir) do
      if `git remote -v | awk '{print $1}'`.match?(/hsbt/)
        puts dir
        system("git remote rename hsbt fork")
      end
    end
  end
end
