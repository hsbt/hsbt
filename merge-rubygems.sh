#/bin/bash
rm -rf lib/rubygems test/rubygtems lib/rubygems.rb
cp -rf ../../rubygems/rubygems/lib/rubygems ./lib
cp -rf ../../rubygems/rubygems/lib/rubygems.rb ./lib
cp -rf ../../rubygems/rubygems/test/rubygems ./test
git checkout lib/rubygems/LICENSE.txt
