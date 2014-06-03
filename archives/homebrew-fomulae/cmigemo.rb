require 'formula'

class Cmigemo < Formula
  homepage 'http://www.kaoriya.net/software/cmigemo'
  url 'http://cmigemo.googlecode.com/files/cmigemo-default-src-20110227.zip'
  md5 '6e9b6f6ec96d4eb8bdd18e52b91e1b85'

  depends_on 'nkf' => :build

  # Patch per discussion at: https://github.com/mxcl/homebrew/pull/7005
  def patches
    DATA
  end

  def install
    system "chmod 755 ./configure"
    system "./configure"
    system "make osx"
    system "make osx-dict"
    cd 'dict' do
      system "make utf-8"
    end
    ENV.j1 # Install can fail on multi-core machines unless serialized
    system "make osx-install"
  end

  def caveats; <<-EOS.undent
    See also https://gist.github.com/457761 to use cmigemo with Emacs.
    You will have to save as migemo.el and put it in your load-path.
    EOS
  end
end

__END__
--- a/src/wordbuf.c    2011-08-15 02:57:05.000000000 +0900
+++ b/src/wordbuf.c    2011-08-15 02:57:17.000000000 +0900
@@ -9,6 +9,7 @@
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
+#include <limits.h>
 #include "wordbuf.h"
 
 #define WORDLEN_DEF 64
