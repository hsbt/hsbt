#!/usr/bin/env ruby

require "fileutils"
require "tmpdir"

version = ARGV[0]
abort "Usage: ruby ruby-draft-check.rb <version> (e.g. 3.3.10)" unless version

url = "https://cache.ruby-lang.org/pub/tmp/ruby-#{version}-draft.tar.gz"
tarball = "ruby-#{version}-draft.tar.gz"
srcdir = "ruby-#{version}-draft"

Dir.mktmpdir("ruby-draft-") do |tmpdir|
  Dir.chdir(tmpdir) do
    system("curl", "-fSL", "-o", tarball, url, exception: true)
    system("tar", "xf", tarball, exception: true)
    Dir.chdir(srcdir) do
      system("./configure", "--disable-install-doc", exception: true)
      system("make", "-j#{`nproc 2>/dev/null || sysctl -n hw.ncpu`.strip}", exception: true)
      system("make", "check", exception: true)
    end
  end
end
