require "openssl"
require "pathname"

ext_names = %w[.gz .bz2 .zip .xz]
dirs = %w[1.0 1.1a 1.1b 1.1c 1.1d 1.2 1.3 1.4 1.6 1.8 1.9 2.0 2.1 2.2 2.3]

File.open("pub/ruby/index.txt", "w") do |f|
  dirs.each do |dir|
    Dir["pub/ruby/#{dir}/*"].each do |pkg|
      next unless ext_names.include?(Pathname(pkg).extname)
      next unless pkg =~ /ruby-/

      %w[SHA1 SHA256 SHA512].each do |algm|
        digest = Object.const_get("OpenSSL::Digest::#{algm}").hexdigest(File.read(pkg))
        f.puts "#{Pathname(pkg).basename}\thttps://cache.ruby-lang.org/pub/ruby/#{dir}/#{Pathname(pkg).basename}\t#{algm}\t#{digest}"
      end
    end
  end
end
