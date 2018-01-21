#!/usr/bin/env ruby

gems = {
  rubygems: 'rubygems/rubygems',
  rdoc: 'ruby/rdoc',
  json: 'flori/json',
  psych: 'ruby/psych',
  fileutils: 'ruby/fileutils',
  fiddle: 'ruby/fiddle',
  stringio: 'ruby/stringio',
  ioconsole: 'ruby/io-console',
  csv: 'ruby/csv',
  webrick: 'ruby/webrick',
  dbm: 'ruby/dbm',
  gdbm: 'ruby/gdbm',
  sdbm: 'ruby/sdbm',
  etc: 'ruby/etc',
  date: 'ruby/date',
  zlib: 'ruby/zlib',
  fcntl: 'ruby/fcntl',
  scanf: 'ruby/scanf',
  cmath: 'ruby/cmath',
  strscan: 'ruby/strscan',
  ipaddr: 'ruby/ipaddr',
}

gems.each do |k, v|
  mkdir puts k
  dir = File.expand_path("~/Documents/github.com/#{v}")

  Dir.chdir(dir) do
    r = `git branch`

    if r.match(/ruby-core/)
      `git checkout ruby-core`
      `git fetch ruby-core trunk`
      `git rebase ruby-core/trunk`
      `git checkout master`
    else
      `git remote add ruby-core git@github.com:ruby/ruby.git`
      `git fetch ruby-core`
      `git tag -d v1_8_7`
      `git tag -d v2_0_0_rc1`
      `git tag -d v2_1_0_rc1`
      `git tag -d v2_2_0_rc1`
      `git checkout ruby-core/trunk`
      `git branch ruby-core`
      `git checkout master`
    end
  end
end
