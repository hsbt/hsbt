require 'net/http'

header = {'accept-encoding' => "gzip;q=1.0,deflate;q=0.6,identity;q=0.3"}
r = Net::HTTP::Post.new('/', header)

Net::HTTP.start('www.hsbt.org') {|http|
  res = http.request(r)
  puts res.body
}

Net::HTTP.start('www.hsbt.org') {|http|
  res = http.request(r)
  puts res.body
}
