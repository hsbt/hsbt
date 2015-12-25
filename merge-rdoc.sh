#/bin/bash
rm -rf lib/rdoc test/rdoc lib/rdoc.rb
cp -rf ../../rdoc/rdoc/lib/rdoc ./lib
cp -rf ../../rdoc/rdoc/lib/rdoc.rb ./lib
cp -rf ../../rdoc/rdoc/test ./test/rdoc
