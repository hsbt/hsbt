require 'formula'

class Nghttp2 < Formula
  homepage 'http://nghttp2.org'

  url 'https://codeload.github.com/tatsuhiro-t/nghttp2/zip/v0.7.1'
  sha1 '4a53b37c61eb16f4122fd909f215f00d85a44c0f'
  version '0.7.1'

  depends_on 'autoconf'   => :build
  depends_on 'automake'   => :build
  depends_on 'libtool'    => :build
  depends_on 'pkg-config' => :build
  depends_on 'libxml2'
  depends_on 'openssl'
  depends_on 'libev'
  depends_on 'jansson'

  def install
    system 'autoreconf -i'
    system 'automake'
    system 'autoconf'

    ENV['ZLIB_CFLAGS'] = '-I/usr/include'
    ENV['ZLIB_LIBS'] = '-L/usr/lib -lz -lpthread'

    ENV['CXX'] = 'clang++'
    ENV['CXXCPP'] = 'clang++ -E'
    ENV['CXXFLAGS'] = '-g -O2 -std=c++11 -stdlib=libc++'

    system "./configure", "--disable-dependency-tracking",
                          "--disable-threads",
                          "--prefix=#{prefix}"

    system "make install"
  end

  def test
    system "#{bin}/nghttp", "--version"
  end
end