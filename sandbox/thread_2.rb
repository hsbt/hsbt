require 'digest/md5'

3.times.map do
  Thread.new do
    puts Digest::MD5.hexdigest(rand.to_s)
  end
end.each(&:value)
